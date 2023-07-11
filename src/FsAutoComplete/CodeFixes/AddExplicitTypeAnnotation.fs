module FsAutoComplete.CodeFix.AddExplicitTypeAnnotation

open System
open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text.Range
open FsAutoComplete.Core.InlayHints
open FsAutoComplete.Core


let toLspEdit ({ Pos = insertAt; Text = text }: HintInsertion) =
  { Range = fcsPosToProtocolRange insertAt
    NewText = text }

let toLspEdits (edits: HintInsertion[]) = edits |> Array.map toLspEdit

[<Obsolete>] //TODO: correct?
let private isPositionContainedInUntypedImplicitCtorParameter input pos =
  let result =
    SyntaxTraversal.Traverse(
      pos,
      input,
      { new SyntaxVisitorBase<_>() with
          member _.VisitModuleDecl(_, defaultTraverse, decl) =
            match decl with
            | SynModuleDecl.Types(typeDefns = typeDefns) ->
              option {
                let! ctorArgs =
                  typeDefns
                  |> List.tryPick (function
                    | SynTypeDefn(implicitConstructor = Some(SynMemberDefn.ImplicitCtor(ctorArgs = args))) when
                      rangeContainsPos args.Range pos
                      ->
                      Some args
                    | _ -> None)

                match ctorArgs with
                | SynSimplePats.SimplePats(pats = pats) ->
                  let! pat = pats |> List.tryFind (fun pat -> rangeContainsPos pat.Range pos)

                  let rec tryGetUntypedIdent =
                    function
                    | SynSimplePat.Id(ident = ident) when rangeContainsPos ident.idRange pos -> Some ident
                    | SynSimplePat.Attrib(pat = pat) when rangeContainsPos pat.Range pos -> tryGetUntypedIdent pat
                    | SynSimplePat.Typed _
                    | _ -> None

                  return! tryGetUntypedIdent pat
                | _ -> return! None
              }
              |> Option.orElseWith (fun _ -> defaultTraverse decl)
            | _ -> defaultTraverse decl }
    )

  result.IsSome

[<Obsolete>] //TODO: correct
let private isSymbolToTriggerTypeAnnotation
  (funcOrValue: FSharpMemberOrFunctionOrValue)
  (symbolUse: FSharpSymbolUse)
  (parseFileResults: FSharpParseFileResults)
  =
  (funcOrValue.IsValue
   || (funcOrValue.IsFunction
       && parseFileResults.IsBindingALambdaAtPosition symbolUse.Range.Start))
  //TODO: check here for curried parameter? necessary? Or handled by `tryGetExplicitTypeInfo`?
  && not funcOrValue.IsMember
  && not funcOrValue.IsMemberThisValue
  && not funcOrValue.IsConstructorThisValue
  && not (PrettyNaming.IsOperatorDisplayName funcOrValue.DisplayName)


let title = "Add explicit type annotation"

let rec nonTypedParameterName p =
  match p with
  | SynPat.Named(ident = SynIdent(ident, _)) -> Some ident
  | SynPat.Paren(pat = p) -> nonTypedParameterName p
  | _ -> None

let tryFunctionIdentifier (parseAndCheck: ParseAndCheckResults) textDocument sourceText lineStr endPos =
  match parseAndCheck.TryGetSymbolUse endPos lineStr with
  | Some symbolUse ->
    match symbolUse.Symbol with
    | :? FSharpMemberOrFunctionOrValue as mfv when isPotentialTargetForTypeAnnotation true (symbolUse, mfv) ->
      let bindingInfo =
        SyntaxTraversal.Traverse(
          endPos,
          parseAndCheck.GetAST,
          { new SyntaxVisitorBase<_>() with
              member _.VisitPat(path, defaultTraverse, pat) =
                match path, pat with
                | SyntaxNode.SynBinding(SynBinding(headPat = headPat; returnInfo = None)) :: SyntaxNode.SynModule _ :: _,
                  SynPat.LongIdent(longDotId = lid; argPats = SynArgPats.Pats parameters) when
                  rangeContainsPos lid.Range endPos
                  ->
                  Some(headPat.Range, parameters.Length, List.choose nonTypedParameterName parameters)
                | _ -> None }
        )

      match bindingInfo with
      | None -> []
      | Some(headPatRange, untypedParameterCount, parameters) ->
        // Good starting point to start constructing the text edits.
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
            Edits =
              [| yield! parameterEdits
                 yield
                   { Range = fcsPosToProtocolRange headPatRange.End
                     NewText = $" : {returnTypeText}" } |]
            Kind = FixKind.Refactor
            SourceDiagnostic = None } ]
    | _ -> []
  | _ -> []

let fix (getParseResultsForFile: GetParseResultsForFile) : CodeFix =
  fun codeActionParams ->
    asyncResult {
      let filePath = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath

      let fcsStartPos = protocolPosToPos codeActionParams.Range.Start
      let! (parseAndCheck, lineStr, sourceText) = getParseResultsForFile filePath fcsStartPos

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
