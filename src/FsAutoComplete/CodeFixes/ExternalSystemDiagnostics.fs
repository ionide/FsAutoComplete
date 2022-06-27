module FsAutoComplete.CodeFix.ExternalSystemDiagnostics

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open Newtonsoft.Json.Linq

let private (|Payload|_|) (tok: JToken): 't option =
  try
    Some(Ionide.LanguageServerProtocol.Server.deserialize tok : 't)
  with
  | e -> None

let private mapExternalDiagnostic diagnosticType =
  Run.ifDiagnosticByType diagnosticType (fun diagnostic codeActionParams ->
    match diagnostic.Data with
    | None -> AsyncResult.retn []
    | Some fixes ->
      match fixes with
      | Payload(fixes: list<TextEdit>) ->
        AsyncResult.retn
          [ { SourceDiagnostic = Some diagnostic
              File = codeActionParams.TextDocument
              Title = $"Fix issue"
              Edits = fixes |> List.toArray
              Kind = FixKind.Fix } ]

      | _ -> AsyncResult.retn [])

/// a codefix that generates fixes reported by FSharpLint
let linter = mapExternalDiagnostic "F# Linter"

/// a codefix that generates fixes reported by F# Analyzers
let analyzers = mapExternalDiagnostic "F# Analyzers"
