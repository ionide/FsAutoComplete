module FsAutoComplete.CodeFix.AddNewKeywordToDisposableConstructorInvocation

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

let title = "Add 'new'"
/// a codefix that suggests using the 'new' keyword on IDisposables
let fix (getRangeText: GetRangeText) =
  Run.ifDiagnosticByCode
    (Set.ofList [ "760" ])
    (fun diagnostic codeActionParams ->
      AsyncResult.retn [ { SourceDiagnostic = Some diagnostic
                           File = codeActionParams.TextDocument
                           Title = title
                           Edits =
                             [| { Range = { Start = diagnostic.Range.Start; End = diagnostic.Range.Start }
                                  NewText = $"new " } |]
                           Kind = FixKind.Refactor } ]
    )
