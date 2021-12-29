module FsAutoComplete.CodeFix.RedundantQualifier

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types

/// a codefix that removes unnecessary qualifiers from an identifier
let fix =
  Run.ifDiagnosticByMessage
    "This qualifier is redundant"
    (fun diagnostic codeActionParams ->
      AsyncResult.retn [ { Edits =
                             [| { Range = diagnostic.Range
                                  NewText = "" } |]
                           File = codeActionParams.TextDocument
                           Title = "Remove redundant qualifier"
                           SourceDiagnostic = Some diagnostic
                           Kind = FixKind.Refactor } ])
