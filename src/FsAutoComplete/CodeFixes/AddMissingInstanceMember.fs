module FsAutoComplete.CodeFix.AddMissingInstanceMember

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types

let title = "Add missing instance member parameter"

let fix =
  Run.ifDiagnosticByCode (Set.ofList [ "673" ]) (fun diagnostic codeActionParams ->
    asyncResult {
      return
        [ { Title = title
            File = codeActionParams.TextDocument
            Kind = FixKind.Fix
            SourceDiagnostic = Some diagnostic
            Edits =
              [| { Range =
                     { Start = diagnostic.Range.Start
                       End = diagnostic.Range.Start }
                   NewText = "x." } |] } ]
    })
