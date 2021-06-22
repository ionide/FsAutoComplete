module FsAutoComplete.CodeFix.UnusedValue

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix
open FsAutoComplete.CodeFix.Types
open LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

/// a codefix that suggests prepending a _ to unused values
let fix (getRangeText: GetRangeText) =
  Run.ifDiagnosticByMessage
    "is unused"
    (fun diagnostic codeActionParams ->
      asyncResult {
        match getRangeText (codeActionParams.TextDocument.GetFilePath() |> normalizePath) diagnostic.Range with
        | Ok unusedExpression ->
            match diagnostic.Code with
            | Some _ ->
                return
                  [ { SourceDiagnostic = Some diagnostic
                      File = codeActionParams.TextDocument
                      Title = "Replace with _"
                      Edits =
                        [| { Range = diagnostic.Range
                             NewText = "_" } |]
                      Kind = Refactor } ]
            | None ->
                let replaceSuggestion = "_"
                let prefixSuggestion = $"_%s{unusedExpression}"

                return
                  [ { SourceDiagnostic = Some diagnostic
                      File = codeActionParams.TextDocument
                      Title = "Replace with _"
                      Edits =
                        [| { Range = diagnostic.Range
                             NewText = replaceSuggestion } |]
                      Kind = Refactor }
                    { SourceDiagnostic = Some diagnostic
                      File = codeActionParams.TextDocument
                      Title = "Prefix with _"
                      Edits =
                        [| { Range = diagnostic.Range
                             NewText = prefixSuggestion } |]
                      Kind = Refactor } ]
        | Error _ -> return []
      })
