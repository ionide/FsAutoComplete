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
          let code =
            diagnostic.Code
            |> Option.map (function
              | U2.C1 n -> n.ToString()
              | U2.C2 s -> s)
            |> Option.map (sprintf "Fix %s")
            |> Option.defaultValue "Fix Issue"

          // an analyzer can offer SEVERAL actions for one span (a primary
          // fix and alternatives); the code alone renders them as
          // identical menu entries. The replacement text is the clearest
          // possible label — show what the code will BECOME
          let firstLineOf (text: string) =
            let i = text.IndexOfAny([| '\n'; '\r' |])
            let line = if i >= 0 then text.Substring(0, i) else text

            if line.Length > 50 then
              line.Substring(0, 47) + "..."
            else
              line

          let editPreview =
            fixes
            |> List.tryPick (fun edit ->
              match edit.NewText.Trim() with
              | "" -> None
              | text -> Some(firstLineOf text))

          match editPreview with
          | Some preview -> $"{code} → {preview}"
          | None ->
            // a pure deletion has nothing to preview; fall back to the
            // diagnostic message so distinct actions still read distinct
            match diagnostic.Message with
            | null
            | "" -> code
            | message -> $"{code}: {firstLineOf message}"

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
