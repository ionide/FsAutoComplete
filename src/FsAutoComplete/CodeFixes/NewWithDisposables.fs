module FsAutoComplete.CodeFix.NewWithDisposables

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix
open FsAutoComplete.CodeFix.Types
open LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

/// a codefix that suggests using the 'new' keyword on IDisposables
let fix (getRangeText: GetRangeText) =
  Run.ifDiagnosticByMessage
    "It is recommended that objects supporting the IDisposable interface are created using the syntax"
    (fun diagnostic codeActionParams ->
      match getRangeText (codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath) diagnostic.Range with
      | Ok errorText ->
          AsyncResult.retn [ { SourceDiagnostic = Some diagnostic
                               File = codeActionParams.TextDocument
                               Title = "Add new"
                               Edits =
                                 [| { Range = diagnostic.Range
                                      NewText = $"new %s{errorText}" } |]
                               Kind = Refactor } ]
      | Error _ -> AsyncResult.retn [])
