module FsAutoComplete.CodeFix.AddExplicitTypeToParameter

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Navigation
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FsAutoComplete.FCSPatches
open FSharp.Compiler.Syntax

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

        let IsPositionContainedInACurriedParameter = parseFileResults.IsPositionContainedInACurriedParameter symbolUse.Range.End
        let IsTypeAnnotationGivenAtPosition = parseFileResults.IsTypeAnnotationGivenAtPosition symbolUse.Range.End

        (funcOrValue.IsValue || isLambdaIfFunction) &&
        IsPositionContainedInACurriedParameter &&
        not IsTypeAnnotationGivenAtPosition &&
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

        let alreadyWrappedInParens =
          let hasLeftParen = Navigation.walkBackUntilConditionWithTerminal sourceText protocolSymbolRange.Start (fun c -> c = '(') System.Char.IsWhiteSpace
          let hasRightParen = Navigation.walkForwardUntilConditionWithTerminal sourceText protocolSymbolRange.End (fun c -> c = ')') System.Char.IsWhiteSpace
          hasLeftParen.IsSome && hasRightParen.IsSome

        let changedText, changedRange =
          if alreadyWrappedInParens
          then ": " + typeString, { Start = protocolSymbolRange.End; End = protocolSymbolRange.End }
          else "(" + symbolText + ": " + typeString + ")", protocolSymbolRange
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
