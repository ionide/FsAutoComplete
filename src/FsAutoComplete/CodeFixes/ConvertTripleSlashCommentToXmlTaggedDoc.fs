module FsAutoComplete.CodeFix.ConvertTripleSlashCommentToXmlTaggedDoc

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Navigation
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open System.Text.RegularExpressions

let title = "Convert '///' comment to XML-tagged doc comment"

let rec private goToTop (currentPos: FSharp.Compiler.Text.Position) (sourceText: NamedText) =
  let prev = currentPos.DecLine()

  match sourceText.GetLine prev with
  | None -> currentPos
  | Some prevLine ->
    if prevLine.TrimStart().StartsWith("///") then
      goToTop prev sourceText
    else
      currentPos

let private collectTrippleSlashComments (currentPos: FSharp.Compiler.Text.Position) (sourceText: NamedText) =
  let rec loop p acc =
    let currentLine = sourceText.GetLine p

    match currentLine with
    | None -> acc
    | Some line ->
      let idx = line.IndexOf("///")

      if idx >= 0 then
        let existingComment = line.TrimStart().Substring(3).TrimStart()
        let acc' = acc @ [ existingComment ]

        match sourceText.NextLine p with
        | None -> acc'
        | Some nextLinePos -> loop nextLinePos acc'
      else
        acc

  loop currentPos List.empty

let private wrapInSummary indent comments =
  let indentation = String.replicate indent " "

  match comments with
  | [] -> $"{indentation}/// <summary></summary>"
  | [ c ] -> $"{indentation}/// <summary>{c}</summary>"
  | cs ->
    seq {
      yield $"{indentation}/// <summary>{System.Environment.NewLine}"
      yield! cs |> List.map (fun s -> $"{indentation}/// {s}{System.Environment.NewLine}")
      yield $"{indentation}/// </summary>"
    }
    |> String.concat ""

let private isFixApplicable (topPos: FSharp.Compiler.Text.Position) (sourceText: NamedText) =
  let line = sourceText.GetLine topPos
  let regex = Regex(@"\s*///\s*<")

  match line with
  | Some s when regex.IsMatch(s) -> false
  | Some s when s.TrimStart().StartsWith("///") -> true
  | _ -> false

let fix (getParseResultsForFile: GetParseResultsForFile) (getRangeText: GetRangeText) : CodeFix =
  fun codeActionParams ->
    asyncResult {
      let filePath = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath
      let fcsPos = protocolPosToPos codeActionParams.Range.Start
      let! (_, lineStr, sourceText) = getParseResultsForFile filePath fcsPos
      let topPos = goToTop fcsPos sourceText
      let showFix = isFixApplicable topPos sourceText

      match showFix with
      | true ->
        let origCommentContents = collectTrippleSlashComments topPos sourceText
        let indent = lineStr.IndexOf("///")
        let summaryXmlDoc = origCommentContents |> wrapInSummary indent

        let startRange =
          { Line = topPos.Line - 1
            Character = 0 }

        let lastLineIdx = topPos.Line + origCommentContents.Length - 1
        let endRange = rangeToDeleteFullLine lastLineIdx sourceText

        let range =
          { Start = startRange
            End = endRange.Start }

        let e =
          { Range = range
            NewText = summaryXmlDoc }

        return
          [ { Edits = [| e |]
              File = codeActionParams.TextDocument
              Title = title
              SourceDiagnostic = None
              Kind = FixKind.Refactor } ]
      | false -> return []
    }
