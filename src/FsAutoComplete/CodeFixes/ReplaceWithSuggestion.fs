module FsAutoComplete.CodeFix.ReplaceWithSuggestion

open FsAutoComplete.CodeFix
open FsAutoComplete.CodeFix.Types
open FsToolkit.ErrorHandling
open FsAutoComplete
open FSharp.Compiler.Syntax

let title suggestion = $"Replace with '%s{suggestion}'"
/// a codefix that replaces the use of an unknown identifier with a suggested identifier
let fix =
  Run.ifDiagnosticByMessage
    "Maybe you want one of the following:"
    (fun diagnostic codeActionParams ->
      diagnostic.Message.Split('\n').[1..]
      |> Array.map
           (fun suggestion ->
              let suggestion = 
                suggestion.Trim()
                |> PrettyNaming.AddBackticksToIdentifierIfNeeded

              { Edits =
                  [| { Range = diagnostic.Range
                       NewText = suggestion } |]
                Title = title suggestion
                File = codeActionParams.TextDocument
                SourceDiagnostic = Some diagnostic
                Kind = FixKind.Fix })
      |> Array.toList
      |> AsyncResult.retn)
