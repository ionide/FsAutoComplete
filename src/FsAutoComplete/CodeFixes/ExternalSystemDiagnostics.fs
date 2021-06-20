module FsAutoComplete.CodeFix.ExternalSystemDiagnostics

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix
open FsAutoComplete.CodeFix.Types
open LanguageServerProtocol.Types
open FsAutoComplete

let private mapExternalDiagnostic diagnosticType =
    Run.ifDiagnosticByType
      diagnosticType
      (fun diagnostic codeActionParams ->

        match diagnostic.Data with
        | None -> AsyncResult.retn []
        | Some fixes ->
          match fixes with
          | :? list<Range * TextEdit> as fixes ->
            match fixes |> List.tryFind (fun (range, textEdit) -> range = diagnostic.Range) with
            | Some (_range, textEdit) ->
                AsyncResult.retn [ { SourceDiagnostic = Some diagnostic
                                     File = codeActionParams.TextDocument
                                     Title = $"Replace with %s{textEdit.NewText}"
                                     Edits = [| textEdit |]
                                     Kind = Fix } ]
            | None -> AsyncResult.retn []
          | _ -> AsyncResult.retn []
        )

/// a codefix that generates fixes reported by FSharpLint
let linter = mapExternalDiagnostic "F# Linter"

/// a codefix that generates fixes reported by F# Analyzers
let analyzers = mapExternalDiagnostic "F# Analyzers"
