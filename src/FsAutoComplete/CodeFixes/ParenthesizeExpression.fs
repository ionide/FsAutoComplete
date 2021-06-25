module FsAutoComplete.CodeFix.ParenthesizeExpression

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

/// a codefix that parenthesizes a member expression that needs it
let fix (getRangeText: GetRangeText): CodeFix =
  Run.ifDiagnosticByCode
    (Set.ofList [ "597" ])
    (fun diagnostic codeActionParams ->
      match getRangeText (codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath) diagnostic.Range with
      | Ok erroringExpression ->
          AsyncResult.retn [ { Title = "Wrap expression in parentheses"
                               File = codeActionParams.TextDocument
                               SourceDiagnostic = Some diagnostic
                               Edits =
                                 [| { Range = diagnostic.Range
                                      NewText = $"(%s{erroringExpression})" } |]
                               Kind = FixKind.Fix } ]
      | Error _ -> AsyncResult.retn [])
