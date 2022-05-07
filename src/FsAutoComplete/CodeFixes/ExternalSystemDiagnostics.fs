module FsAutoComplete.CodeFix.ExternalSystemDiagnostics

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete

let private mapExternalDiagnostic diagnosticType =
  Run.ifDiagnosticByType diagnosticType (fun diagnostic codeActionParams ->
    match diagnostic.Data with
    | None -> AsyncResult.retn []
    | Some fixes ->
      match fixes with
      | :? list<TextEdit> as fixes ->
        AsyncResult.retn [ { SourceDiagnostic = Some diagnostic
                             File = codeActionParams.TextDocument
                             Title = $"Fix issue"
                             Edits = fixes |> List.toArray
                             Kind = FixKind.Fix } ]

      | _ -> AsyncResult.retn [])

/// a codefix that generates fixes reported by FSharpLint
let linter = mapExternalDiagnostic "F# Linter"

/// a codefix that generates fixes reported by F# Analyzers
let analyzers = mapExternalDiagnostic "F# Analyzers"
