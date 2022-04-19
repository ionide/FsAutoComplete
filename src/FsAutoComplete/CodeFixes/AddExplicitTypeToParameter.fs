module FsAutoComplete.CodeFix.AddExplicitTypeToParameter

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text.Range

let private isPositionContainedInUntypedImplicitCtorParameter input pos =
    let result =
        SyntaxTraversal.Traverse(pos, input, { new SyntaxVisitorBase<_>() with 
          member _.VisitModuleDecl(_, defaultTraverse, decl) =
            match decl with
            | SynModuleDecl.Types(typeDefns = typeDefns) ->
                maybe {
                  let! ctorArgs =
                    typeDefns
                    |> List.tryPick (
                      function
                      | SynTypeDefn(implicitConstructor=Some(SynMemberDefn.ImplicitCtor(ctorArgs = args))) when rangeContainsPos args.Range pos ->
                          Some args
                      | _ -> None
                    )
                  
                  match ctorArgs with
                  | SynSimplePats.SimplePats (pats=pats) ->
                      let! pat =
                        pats
                        |> List.tryFind (fun pat -> rangeContainsPos pat.Range pos)
                      let rec tryGetUntypedIdent =
                        function
                        | SynSimplePat.Id (ident=ident) when rangeContainsPos ident.idRange pos ->
                            Some ident
                        | SynSimplePat.Attrib (pat=pat) when rangeContainsPos pat.Range pos ->
                            tryGetUntypedIdent pat
                        | SynSimplePat.Typed _ 
                        | _ ->
                            None
                      return! tryGetUntypedIdent pat
                  | _ -> return! None
                }
                |> Option.orElseWith (fun _ -> defaultTraverse decl)
            | _ -> defaultTraverse decl
        })
    result.IsSome

let title = "Add explicit type annotation"
let fix (getParseResultsForFile: GetParseResultsForFile): CodeFix =
  fun codeActionParams ->
    asyncResult {
      let filePath = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath
      let fcsStartPos = protocolPosToPos codeActionParams.Range.Start
      let! (parseAndCheck, lineStr, sourceText) = getParseResultsForFile filePath fcsStartPos
      let parseFileResults = parseAndCheck.GetParseResults
      let! (rightCol, idents) =
        Lexer.findLongIdents(fcsStartPos.Column, lineStr)
        |> Result.ofOption (fun _ -> $"Couldn't find long ident at %A{fcsStartPos} in file %s{codeActionParams.TextDocument.GetFilePath()}")
      let! symbolUse =
        parseAndCheck.GetCheckResults.GetSymbolUseAtLocation(fcsStartPos.Line, rightCol, lineStr, List.ofArray idents)
        |> Result.ofOption (fun _ -> $"Couldn't find symbolUse at %A{(fcsStartPos.Line, rightCol)} in file %s{codeActionParams.TextDocument.GetFilePath()}")

      let isValidParameterWithoutTypeAnnotation (funcOrValue: FSharpMemberOrFunctionOrValue) (symbolUse: FSharpSymbolUse) =
        let isLambdaIfFunction =
            funcOrValue.IsFunction &&
             parseFileResults.IsBindingALambdaAtPosition symbolUse.Range.Start

        (funcOrValue.IsValue || isLambdaIfFunction) &&
        (
          (
            parseFileResults.IsPositionContainedInACurriedParameter symbolUse.Range.Start &&
            not (parseFileResults.IsTypeAnnotationGivenAtPosition symbolUse.Range.Start)
          ) ||
          (isPositionContainedInUntypedImplicitCtorParameter parseFileResults.ParseTree symbolUse.Range.Start)
        ) &&
        not funcOrValue.IsMember &&
        not funcOrValue.IsMemberThisValue &&
        not funcOrValue.IsConstructorThisValue &&
        not (PrettyNaming.IsOperatorDisplayName funcOrValue.DisplayName)

      match symbolUse.Symbol with
      | :? FSharpMemberOrFunctionOrValue as v when isValidParameterWithoutTypeAnnotation v symbolUse ->
        let typeString = v.FullType.Format symbolUse.DisplayContext
        let title = title
        let fcsSymbolRange = symbolUse.Range
        let protocolSymbolRange = fcsRangeToLsp fcsSymbolRange
        let! symbolText = sourceText.GetText fcsSymbolRange

        let requiresParens =
          if isPositionContainedInUntypedImplicitCtorParameter parseFileResults.ParseTree symbolUse.Range.Start then
            // no patterns in primary ctor allowed -> `type A((a))` is invalid
            false
          else
            // `(a, b, c)`
            // -> between `,` and parens (there might be spaces between)
            let left =
              sourceText.WalkBackwards(fcsSymbolRange.Start, (fun _ -> false), ((<>) ' '))
              |> Option.bind (sourceText.TryGetChar)
            let right =
              sourceText.NextPos fcsSymbolRange.End // end is on last char of identifier
              |> Option.bind (fun pos -> sourceText.WalkForward(pos, (fun _ -> false), ((<>) ' ')))
              |> Option.bind (sourceText.TryGetChar)
            
            match left, right with
            | Some left, Some right ->
                let isContained =
                  (left = '(' || left = ',')
                  &&
                  (right = ',' || right = ')')
                not isContained
            | _, _ -> true

        let changedText, changedRange =
          if requiresParens
          then "(" + symbolText + ": " + typeString + ")", protocolSymbolRange
          else ": " + typeString, { Start = protocolSymbolRange.End; End = protocolSymbolRange.End }
        return [ {
          Edits = [| { Range = changedRange; NewText = changedText } |]
          File = codeActionParams.TextDocument
          Title = title
          SourceDiagnostic = None
          Kind = FixKind.Refactor
          } ]
      | _ ->
        return []
    }
