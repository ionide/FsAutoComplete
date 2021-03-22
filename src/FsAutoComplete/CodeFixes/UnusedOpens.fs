module FsAutoComplete.CodeFix.UnusedOpens

open LanguageServerProtocol.Types
open FsAutoComplete.CodeFix
open FsAutoComplete.CodeFix.Types
open FsToolkit.ErrorHandling

/// a codefix that removes unused open statements from the source
let fix : CodeFix =
  Run.ifDiagnosticByMessage
    "Unused open statement"
    (fun d codeActionParams ->
      let range =
        { Start =
            { Line = d.Range.Start.Line - 1
              Character = 1000 }
          End =
            { Line = d.Range.End.Line
              Character = d.Range.End.Character } }

      let fix =
        { Edits = [| { Range = range; NewText = "" } |]
          File = codeActionParams.TextDocument
          Title = "Remove unused open"
          SourceDiagnostic = Some d
          Kind = Refactor }

      AsyncResult.retn [ fix ])
