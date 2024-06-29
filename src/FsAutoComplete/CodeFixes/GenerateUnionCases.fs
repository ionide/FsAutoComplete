module FsAutoComplete.CodeFix.GenerateUnionCases

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FsAutoComplete.CodeFix.Navigation

let title = "Generate union pattern match cases"

/// a codefix that generates union cases for an incomplete match expression
let fix
  (getFileLines: GetFileLines)
  (getParseResultsForFile: GetParseResultsForFile)
  (generateCases: _ -> _ -> _ -> Async<CoreResponse<_>>)
  (getTextReplacements: unit -> Map<string, string>)
  =
  Run.ifDiagnosticByCode (Set.ofList [ "25" ]) (fun diagnostic codeActionParams ->
    let getCasePosFromCaseLine (lines: IFSACSourceText) (fcsRange: FcsRange) =
      result {
        let! nextLine = lines.NextLine fcsRange.Start |> Result.ofOption (fun _ -> "no next line")

        let! caseLine = lines.GetLine(nextLine) |> Result.ofOption (fun _ -> "No case line")

        let! caseCol = match caseLine.IndexOf('|') with
                        | -1 -> Error "Invalid case line"
                        | idx  -> Ok (uint32 idx + 3u) // Find column of first case in pattern matching

        let casePos =
          { Line = uint32 nextLine.Line - 1u;
                  Character = caseCol }
        return casePos
      }

    let getCasePosFromMatch (lines: IFSACSourceText) (fcsRange: FcsRange) =
      result {
        let! matchLine = lines.GetLine fcsRange.Start |> Result.ofOption (fun _ -> "no current line")
        let caseCol = matchLine.IndexOf("match")
        let casePos = { Line = uint32 fcsRange.Start.Line - 1u;
                Character = uint32 caseCol + 7u }
        return casePos
      }

    asyncResult {
      let fileName = codeActionParams.TextDocument.GetFilePath() |> normalizePath

      let! lines = getFileLines fileName
      // try to find the first case already written
      let fcsRange = protocolRangeToRange (FSharp.UMX.UMX.untag fileName) diagnostic.Range


      let! casePos = (getCasePosFromCaseLine lines fcsRange) |> Result.orElseWith (fun _ -> getCasePosFromMatch lines fcsRange)
      let casePosFCS = protocolPosToPos casePos

      let! tyRes, _line, lines = getParseResultsForFile fileName casePosFCS

      match! generateCases tyRes casePosFCS lines |> Async.map Ok with
      | CoreResponse.Res(insertString: string, insertPosition) ->
        let range =
          { Start = fcsPosToLsp insertPosition
            End = fcsPosToLsp insertPosition }

        let replacements = getTextReplacements ()

        let replaced =
          (insertString, replacements)
          ||> Seq.fold (fun text (KeyValue(key, replacement)) -> text.Replace(key, replacement))

        return
          [ { SourceDiagnostic = Some diagnostic
              File = codeActionParams.TextDocument
              Title = title
              Edits = [| { Range = range; NewText = replaced } |]
              Kind = FixKind.Fix } ]

      | _ -> return []
    })
