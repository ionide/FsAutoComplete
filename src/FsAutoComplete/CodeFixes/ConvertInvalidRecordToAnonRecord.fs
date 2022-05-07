module FsAutoComplete.CodeFix.ConvertInvalidRecordToAnonRecord

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.CodeFix.Navigation
open FsAutoComplete.LspHelpers

let title = "Convert to anonymous record"

/// a codefix that converts unknown/partial record expressions to anonymous records
let fix (getParseResultsForFile: GetParseResultsForFile) : CodeFix =
  Run.ifDiagnosticByCode (Set.ofList [ "39" ]) (fun diagnostic codeActionParams ->
    asyncResult {
      let fileName =
        codeActionParams.TextDocument.GetFilePath()
        |> Utils.normalizePath

      let fcsPos = protocolPosToPos diagnostic.Range.Start
      let! (tyRes, line, lines) = getParseResultsForFile fileName fcsPos

      match tyRes.GetParseResults.TryRangeOfRecordExpressionContainingPos fcsPos with
      | Some recordExpressionRange ->
        let recordExpressionRange = fcsRangeToLsp recordExpressionRange

        let! startInsertRange =
          inc lines recordExpressionRange.Start
          |> Option.map (fun next -> { Start = next; End = next })
          |> Result.ofOption (fun _ -> "No start insert range")

        let! endInsertRange =
          dec lines recordExpressionRange.End
          |> Option.map (fun prev -> { Start = prev; End = prev })
          |> Result.ofOption (fun _ -> "No end insert range")

        return
          [ { Title = title
              File = codeActionParams.TextDocument
              SourceDiagnostic = Some diagnostic
              Edits =
                [| { Range = startInsertRange
                     NewText = "|" }
                   { Range = endInsertRange
                     NewText = "|" } |]
              Kind = FixKind.Refactor } ]
      | None -> return []
    })
