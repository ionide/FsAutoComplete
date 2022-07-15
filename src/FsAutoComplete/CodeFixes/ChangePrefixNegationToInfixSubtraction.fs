module FsAutoComplete.CodeFix.ChangePrefixNegationToInfixSubtraction

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Navigation
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

let title = "Use subtraction instead of negation"

/// a codefix that corrects -<something> to - <something> when negation is not intended
let fix (getFileLines: GetFileLines) : CodeFix =
  Run.ifDiagnosticByCode (Set.ofList [ "3" ]) (fun diagnostic codeActionParams ->
    asyncResult {
      let fileName = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath

      let! lines = getFileLines fileName

      let! walkPos = inc lines diagnostic.Range.End |> Result.ofOption (fun _ -> "No walk pos")

      match walkForwardUntilCondition lines walkPos (fun ch -> ch = '-') with
      | Some dash ->
        let! oneBack = dec lines dash |> Result.ofOption (fun _ -> "No one back")

        return
          [ { SourceDiagnostic = Some diagnostic
              Title = title
              File = codeActionParams.TextDocument
              Edits =
                [| { Range = { Start = oneBack; End = dash }
                     NewText = "- " } |]
              Kind = FixKind.Fix } ]
      | None -> return []
    })
