module FsAutoComplete.CodeFix.ChangeRefCellDerefToNot

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

let title = "Use 'not' to negate expression" 
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
                  Title = title
                  File = codeActionParams.TextDocument
                  Edits =
                    [| { Range = fcsRangeToLsp derefRange
                         NewText = "not " } |]
                  Kind = FixKind.Fix } ]
        | None -> return []
      }
      )
