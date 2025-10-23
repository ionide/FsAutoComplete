module FsAutoComplete.CodeFix.ExternalSystemDiagnostics

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open Newtonsoft.Json.Linq

[<return: Struct>]
let private (|Payload|_|) (tok: JToken) : 't voption =
  try
    ValueSome(Ionide.LanguageServerProtocol.Server.deserialize tok: 't)
  with e ->
    ValueNone

let private mapExternalDiagnostic diagnosticType =
  Run.ifDiagnosticByType diagnosticType (fun diagnostic codeActionParams ->
    match diagnostic.Data with
    | None -> AsyncResult.retn []
    | Some fixes ->
      match fixes with
      | Payload(fixes: list<TextEdit>) ->
        let title =
          diagnostic.Code
          |> Option.map (function
            | U2.C1 n -> n.ToString()
            | U2.C2 s -> s)
          |> Option.map (sprintf "Fix %s")
          |> Option.defaultValue "Fix Issue"

        AsyncResult.retn
          [ { SourceDiagnostic = Some diagnostic
              File = codeActionParams.TextDocument
              Title = title
              Edits = fixes |> List.toArray
              Kind = FixKind.Fix } ]

      | _ -> AsyncResult.retn [])

/// a codefix that generates fixes reported by FSharpLint
let linter = mapExternalDiagnostic "F# Linter"

/// a codefix that generates fixes reported by F# Analyzers
let analyzers = mapExternalDiagnostic "F# Analyzers"
