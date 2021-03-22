module FsAutoComplete.CodeFix.ExternalSystemDiagnostics

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix
open FsAutoComplete.CodeFix.Types
open LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open System.IO

let private mapExternalDiagnostic diagnosticType getFixesForFile =
    Run.ifDiagnosticByType
      diagnosticType
      (fun diagnostic codeActionParams ->
        let fileName =
          codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath

        let normalizedUri = Path.LocalPathToUri fileName

        match getFixesForFile normalizedUri
              |> Option.bind
                   (fun fixes ->
                     fixes
                     |> List.tryFind (fun (range, textEdit) -> range = diagnostic.Range)) with
        | Some (range, textEdit) ->
            AsyncResult.retn [ { SourceDiagnostic = Some diagnostic
                                 File = codeActionParams.TextDocument
                                 Title = $"Replace with %s{textEdit.NewText}"
                                 Edits = [| textEdit |]
                                 Kind = Fix } ]
        | None -> AsyncResult.retn [])


/// a codefix that generates fixes reported by FSharpLint
let linter = mapExternalDiagnostic "F# Linter"

/// a codefix that generates fixes reported by F# Analyzers
let analyzers = mapExternalDiagnostic "F# Analyzers"
