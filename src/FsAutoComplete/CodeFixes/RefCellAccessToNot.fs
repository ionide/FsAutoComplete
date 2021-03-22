module FsAutoComplete.CodeFix.RefCellAccesToNot

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

/// a codefix that changes a ref cell deref (!) to a call to 'not'
let fix (getParseResultsForFile: GetParseResultsForFile): CodeFix =
  Run.ifDiagnosticByCode
    (Set.ofList [ "1" ])
    (fun diagnostic codeActionParams ->
      asyncResult {
        let fileName =
          codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath

        let fcsPos = protocolPosToPos diagnostic.Range.Start
        let! (tyRes, line, lines) = getParseResultsForFile fileName fcsPos

        match tyRes.GetParseResults.TryRangeOfRefCellDereferenceContainingPos fcsPos with
        | Some derefRange ->
            return
              [ { SourceDiagnostic = Some diagnostic
                  Title = "Use 'not' to negate expression"
                  File = codeActionParams.TextDocument
                  Edits =
                    [| { Range = fcsRangeToLsp derefRange
                         NewText = "not " } |]
                  Kind = Fix } ]
        | None -> return []
      }
      )
