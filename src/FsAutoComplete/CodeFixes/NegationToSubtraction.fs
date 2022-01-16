module FsAutoComplete.CodeFix.NegationToSubtraction

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Navigation
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

/// a codefix that corrects -<something> to - <something> when negation is not intended
let fix (getFileLines: GetFileLines): CodeFix =
  Run.ifDiagnosticByCode
    (Set.ofList [ "3" ])
    (fun diagnostic codeActionParams ->
      asyncResult {
        let fileName =
          codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath

        let! lines = getFileLines fileName

        match walkForwardUntilCondition lines (inc lines diagnostic.Range.End) (fun ch -> ch = '-') with
        | Some dash ->
            return
              [ { SourceDiagnostic = Some diagnostic
                  Title = "Use subtraction instead of negation"
                  File = codeActionParams.TextDocument
                  Edits =
                    [| { Range = { Start = dash; End = inc lines dash }
                         NewText = "- " } |]
                  Kind = FixKind.Fix } ]
        | None -> return []
      }
      )
