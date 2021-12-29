module FsAutoComplete.CodeFix.AddMissingInstanceMember

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types

let fix =
    Run.ifDiagnosticByCode (Set.ofList [ "673" ]) (fun diagnostic codeActionParams -> asyncResult {
      return [{
        Title = "Add missing instance member parameter"
        File = codeActionParams.TextDocument
        Kind = FixKind.Fix
        SourceDiagnostic = Some diagnostic
        Edits = [| { Range = { Start = diagnostic.Range.Start; End = diagnostic.Range.Start }; NewText = "x." } |]
      }]
    })
