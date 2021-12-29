module FsAutoComplete.CodeFix.ColonInFieldType

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open FsAutoComplete

/// a codefix that fixes a malformed record type annotation to use colon instead of equals
let fix: CodeFix =
  Run.ifDiagnosticByCode
    (Set.ofList [ "10" ])
    (fun diagnostic codeActionParams ->
      if diagnostic.Message = "Unexpected symbol '=' in field declaration. Expected ':' or other token." then
        AsyncResult.retn [ { File = codeActionParams.TextDocument
                             Title = "Use ':' for type in field declaration"
                             SourceDiagnostic = Some diagnostic
                             Edits =
                               [| { Range = diagnostic.Range
                                    NewText = ":" } |]
                             Kind = FixKind.Fix } ]
      else
        AsyncResult.retn [])
