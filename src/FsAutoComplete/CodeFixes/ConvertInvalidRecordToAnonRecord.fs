module FsAutoComplete.CodeFix.ConvertInvalidRecordToAnonRecord

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.CodeFix.Navigation
open FsAutoComplete.LspHelpers

/// a codefix that converts unknown/partial record expressions to anonymous records
let fix (getParseResultsForFile: GetParseResultsForFile) : CodeFix =
  Run.ifDiagnosticByCode
    (Set.ofList [ "39" ])
    (fun diagnostic codeActionParams ->
      asyncResult {
        let fileName =
          codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath

        let fcsPos = protocolPosToPos diagnostic.Range.Start
        let! (tyRes, line, lines) = getParseResultsForFile fileName fcsPos

        match tyRes.GetParseResults.TryRangeOfRecordExpressionContainingPos fcsPos with
        | Some recordExpressionRange ->
            let recordExpressionRange = fcsRangeToLsp recordExpressionRange

            let startInsertRange =
              let next = inc lines recordExpressionRange.Start
              { Start = next; End = next }

            let endInsertRange =
              let prev = dec lines recordExpressionRange.End
              { Start = prev; End = prev }

            return
              [ { Title = "Convert to anonymous record"
                  File = codeActionParams.TextDocument
                  SourceDiagnostic = Some diagnostic
                  Edits =
                    [| { Range = startInsertRange
                         NewText = "|" }
                       { Range = endInsertRange
                         NewText = "|" } |]
                  Kind = FixKind.Refactor } ]
        | None -> return []
      }
      )
