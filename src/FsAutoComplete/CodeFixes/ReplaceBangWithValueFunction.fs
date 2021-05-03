/// replace use of ! operator on ref cells with calls to .Value
module FsAutoComplete.CodeFix.ReplaceBangWithValueFunction

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FsAutoComplete.FCSPatches

let fix (getParseResultsForFile: GetParseResultsForFile) (getLineText: GetLineText): CodeFix =
  fun codeActionParams ->
    asyncResult {
      let fileName = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath
      let selectionRange = protocolRangeToRange (codeActionParams.TextDocument.GetFilePath()) codeActionParams.Range
      let! parseResults, line, lines = getParseResultsForFile fileName selectionRange.Start
      let! derefRange = parseResults.GetParseResults.TryRangeOfRefCellDereferenceContainingPos selectionRange.Start |> Result.ofOption (fun _ -> "No deref found at that pos")
      let! exprRange = parseResults.GetParseResults.TryRangeOfExpressionBeingDereferencedContainingPos selectionRange.Start |> Result.ofOption (fun _ -> "No expr found at that pos")
      let combinedRange = FSharp.Compiler.Text.Range.unionRanges derefRange exprRange
      let protocolRange = fcsRangeToLsp combinedRange
      let! badString = getLineText lines protocolRange
      let replacementString = badString.[1..] + ".Value"
      return [
        { Title = "Use `.Value` instead of dereference operator"
          File = codeActionParams.TextDocument
          SourceDiagnostic = None
          Kind = Refactor
          Edits = [| { Range = protocolRange
                       NewText = replacementString } |] }
      ]
    }
    |> AsyncResult.foldResult id (fun _ -> [])
