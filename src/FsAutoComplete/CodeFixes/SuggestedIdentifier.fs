module FsAutoComplete.CodeFix.SuggestedIdentifier

open LanguageServerProtocol.Types
open FsAutoComplete.CodeFix
open FsAutoComplete.CodeFix.Types
open FsToolkit.ErrorHandling
open FsAutoComplete

/// a codefix that replaces the use of an unknown identifier with a suggested identifier
let fix =
  Run.ifDiagnosticByMessage
    "Maybe you want one of the following:"
    (fun diagnostic codeActionParams ->
      diagnostic.Message.Split('\n').[1..]
      |> Array.map
           (fun suggestion ->
             let suggestion = suggestion.Trim()

             let suggestion =
               if System.Text.RegularExpressions.Regex.IsMatch(suggestion, """^[a-zA-Z][a-zA-Z0-9']+$""")
               then suggestion
               else $"``%s{suggestion}``"

             { Edits =
                 [| { Range = diagnostic.Range
                      NewText = suggestion } |]
               Title = $"Replace with %s{suggestion}"
               File = codeActionParams.TextDocument
               SourceDiagnostic = Some diagnostic
               Kind = FixKind.Fix })
      |> Array.toList
      |> AsyncResult.retn)
