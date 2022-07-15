module FsAutoComplete.CodeFix.RenameUnusedValue

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix
open FsAutoComplete.CodeFix.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

let titleReplace = "Replace with _"
let titlePrefix = "Prefix with _"

/// a codefix that suggests prepending a _ to unused values
let fix (getRangeText: GetRangeText) =
  Run.ifDiagnosticByMessage "is unused" (fun diagnostic codeActionParams ->
    asyncResult {
      match getRangeText (codeActionParams.TextDocument.GetFilePath() |> normalizePath) diagnostic.Range with
      | Ok unusedExpression ->
        match diagnostic.Code with
        | Some _ ->
          return
            [ { SourceDiagnostic = Some diagnostic
                File = codeActionParams.TextDocument
                Title = titleReplace
                Edits =
                  [| { Range = diagnostic.Range
                       NewText = "_" } |]
                Kind = FixKind.Refactor } ]
        | None ->
          let replaceSuggestion = "_"
          let prefixSuggestion = $"_%s{unusedExpression}"

          return
            [ { SourceDiagnostic = Some diagnostic
                File = codeActionParams.TextDocument
                Title = titleReplace
                Edits =
                  [| { Range = diagnostic.Range
                       NewText = replaceSuggestion } |]
                Kind = FixKind.Refactor }
              { SourceDiagnostic = Some diagnostic
                File = codeActionParams.TextDocument
                Title = titlePrefix
                Edits =
                  [| { Range = diagnostic.Range
                       NewText = prefixSuggestion } |]
                Kind = FixKind.Refactor } ]
      | Error _ -> return []
    })
