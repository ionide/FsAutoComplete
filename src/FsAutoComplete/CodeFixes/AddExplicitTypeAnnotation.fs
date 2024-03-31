module FsAutoComplete.CodeFix.AddExplicitTypeAnnotation

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text.Range
open FsAutoComplete.Core.InlayHints
open FsAutoComplete.Core

let toLspEdit ({ Pos = insertAt; Text = text }: HintInsertion) =
  { Range = fcsPosToProtocolRange insertAt
    NewText = text }

let toLspEdits (edits: HintInsertion[]) = edits |> Array.map toLspEdit

let title = "Add explicit type annotation"

let rec nonTypedParameterName p =
  match p with
  | SynPat.Named(ident = SynIdent(ident, _)) -> Some ident
  | SynPat.Paren(pat = p) -> nonTypedParameterName p
  | _ -> None

/// Captures a SynBinding that either has no return type or has parameters that are not typed.
[<return: Struct>]
let (|FunctionBindingWithMissingTypes|_|) =
  function
  | SynBinding(
      headPat = SynPat.LongIdent(longDotId = lid; argPats = SynArgPats.Pats parameters) as headPat
      returnInfo = None
      trivia = { LeadingKeyword = lk }) ->
    let bindingStartRange = unionRanges lk.Range lid.Range
    ValueSome(bindingStartRange, Some headPat.Range, parameters.Length, List.choose nonTypedParameterName parameters)
  | SynBinding(
      headPat = SynPat.LongIdent(longDotId = lid; argPats = SynArgPats.Pats parameters)
      returnInfo = Some _
      trivia = { LeadingKeyword = lk }) ->
    let bindingStartRange = unionRanges lk.Range lid.Range
    let nonTypedParameters = List.choose nonTypedParameterName parameters

    if List.isEmpty nonTypedParameters then
      ValueNone
    else
      ValueSome(bindingStartRange, None, parameters.Length, nonTypedParameters)
  | _ -> ValueNone

/// <summary>
/// Try and find a SynBinding function where either the return type or any parameter is missing a type definition.
/// </summary>
/// <param name="parseAndCheck"></param>
/// <param name="textDocument"></param>
/// <param name="sourceText"></param>
/// <param name="lineStr"></param>
/// <param name="cursorPos">Expected to be between the start of the leading keyword and the end of the function name.</param>
let tryFunctionIdentifier (parseAndCheck: ParseAndCheckResults) textDocument sourceText lineStr cursorPos =
  let bindingInfo =
    SyntaxTraversal.Traverse(
      cursorPos,
      parseAndCheck.GetAST,
      { new SyntaxVisitorBase<_>() with
          member _.VisitExpr(path, traverseSynExpr, defaultTraverse, expr) = defaultTraverse expr

          member _.VisitBinding(path, defaultTraverse, binding) =
            match binding with
            | FunctionBindingWithMissingTypes(bindingStartRange,
                                              headPatRangeOpt,
                                              totalParameterCount,
                                              nonTypedParameters) when rangeContainsPos bindingStartRange cursorPos ->
              Some(bindingStartRange, headPatRangeOpt, totalParameterCount, nonTypedParameters)
            | _ -> defaultTraverse binding }
    )

  match bindingInfo with
  | None -> []
  | Some(bindingStartRange, headPatRangeOpt, untypedParameterCount, parameters) ->
    match parseAndCheck.TryGetSymbolUse bindingStartRange.End lineStr with
    | Some symbolUse ->
      match symbolUse.Symbol with
      | :? FSharpMemberOrFunctionOrValue as mfv when isPotentialTargetForTypeAnnotation true (symbolUse, mfv) ->
        let returnTypeEdits =
          match headPatRangeOpt with
          | None -> [] // The return type is already present
          | Some headPatRange ->
            let returnTypeText =
              if not mfv.FullType.IsFunctionType then
                mfv.ReturnParameter.Type.Format(symbolUse.DisplayContext)
              else
                // We can't really be trust mfv.ReturnParameter, it will only contain the last type in a function type.
                // Instead we collect all types and skip the amount of parameters we have in the function definition.
                let allTypesFromFunctionType: FSharpType list =
                  let rec visit (t: FSharpType) (continuation: FSharpType list -> FSharpType list) =
                    if not t.IsFunctionType then
                      continuation [ t ]
                    else
                      let funcType = t.GenericArguments.[0]
                      let argType = t.GenericArguments.[1]

                      if not argType.IsFunctionType then
                        continuation [ funcType; argType ]
                      else
                        visit argType (fun types -> funcType :: types |> continuation)

                  visit mfv.FullType id

                if allTypesFromFunctionType.Length <= untypedParameterCount then
                  mfv.ReturnParameter.Type.Format(symbolUse.DisplayContext)
                else
                  allTypesFromFunctionType
                  |> List.skip untypedParameterCount
                  |> List.map (fun t ->
                    let formattedType = t.Format(symbolUse.DisplayContext)

                    if t.IsFunctionType then
                      $"({formattedType})"
                    else
                      formattedType)
                  |> String.concat " -> "

            // Put the return type after the current headPat.
            [ { Range = fcsPosToProtocolRange headPatRange.End
                NewText = $" : {returnTypeText}" } ]

        let parameterEdits =
          parameters
          |> List.choose (fun ident ->
            InlayHints.tryGetDetailedExplicitTypeInfo
              (InlayHints.isPotentialTargetForTypeAnnotation true)
              (sourceText, parseAndCheck)
              ident.idRange.Start
            |> Option.bind (fun (symbolUse, mfv, explTy) ->
              explTy.TryGetTypeAndEdits(mfv.FullType, symbolUse.DisplayContext)
              |> Option.map (fun (_, edits) -> toLspEdits edits)))
          |> Seq.collect id
          |> Seq.toArray

        [ { File = textDocument
            Title = title
            Edits = [| yield! parameterEdits; yield! returnTypeEdits |]
            Kind = FixKind.Refactor
            SourceDiagnostic = None } ]
      | _ -> []
    | _ -> []

let fix (getParseResultsForFile: GetParseResultsForFile) : CodeFix =
  fun codeActionParams ->
    asyncResult {
      let filePath = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath

      let fcsStartPos = protocolPosToPos codeActionParams.Range.Start
      let! parseAndCheck, lineStr, sourceText = getParseResultsForFile filePath fcsStartPos

      let res =
        InlayHints.tryGetDetailedExplicitTypeInfo
          (InlayHints.isPotentialTargetForTypeAnnotation true)
          (sourceText, parseAndCheck)
          fcsStartPos

      match res with
      | None ->
        return
          tryFunctionIdentifier
            parseAndCheck
            codeActionParams.TextDocument
            sourceText
            lineStr
            (protocolPosToPos codeActionParams.Range.End)
      | Some(symbolUse, mfv, explTy) ->
        match explTy.TryGetTypeAndEdits(mfv.FullType, symbolUse.DisplayContext) with
        | None -> return []
        | Some(_, edits) ->
          return
            [ { File = codeActionParams.TextDocument
                Title = title
                Edits = edits |> toLspEdits
                Kind = FixKind.Refactor
                SourceDiagnostic = None } ]
    }
