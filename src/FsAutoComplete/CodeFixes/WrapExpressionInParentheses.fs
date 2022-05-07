module FsAutoComplete.CodeFix.WrapExpressionInParentheses

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete

let title = "Wrap expression in parentheses"

/// a codefix that parenthesizes a member expression that needs it
let fix (getRangeText: GetRangeText) : CodeFix =
  Run.ifDiagnosticByCode (Set.ofList [ "597" ]) (fun diagnostic codeActionParams ->
    AsyncResult.retn [ { Title = title
                         File = codeActionParams.TextDocument
                         SourceDiagnostic = Some diagnostic
                         Edits =
                           [| { Range =
                                  { Start = diagnostic.Range.Start
                                    End = diagnostic.Range.Start }
                                NewText = "(" }
                              { Range =
                                  { Start = diagnostic.Range.End
                                    End = diagnostic.Range.End }
                                NewText = ")" } |]
                         Kind = FixKind.Fix } ])
