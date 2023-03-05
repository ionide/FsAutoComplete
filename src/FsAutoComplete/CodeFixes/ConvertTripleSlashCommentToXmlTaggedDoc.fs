module FsAutoComplete.CodeFix.ConvertTripleSlashCommentToXmlTaggedDoc

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Navigation
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open System.Text.RegularExpressions
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text.Range
open System

let title = "Convert '///' comment to XML-tagged doc comment"

// ToDo: make this work for inner bindings
let private tryGetBindingRange input derefPos =
  SyntaxTraversal.Traverse(
    derefPos,
    input,
    { new SyntaxVisitorBase<FSharp.Compiler.Text.Range>() with
        member _.VisitBinding(path, defaultTraverse: (SynBinding -> FSharp.Compiler.Text.Range option), synBinding) =
          match synBinding with
          | SynBinding(accessibility,
                       kind,
                       isInline,
                       isMutable,
                       attributes,
                       xmlDoc,
                       valData,
                       headPat,
                       returnInfo,
                       expr,
                       range,
                       debugPoint,
                       trivia) as b when rangeContainsPos range derefPos -> Some b.RangeOfHeadPattern
          | _ -> defaultTraverse synBinding }
  )

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

let private wrapInSummary indentation comments =
  match comments with
  | [] -> $"{indentation}/// <summary></summary>"
  | [ c ] -> $"{indentation}/// <summary>%s{c}</summary>"
  | cs ->
    seq {
      yield $"{indentation}/// <summary>{Environment.NewLine}"
      yield! cs |> List.map (fun s -> $"%s{indentation}/// %s{s}{Environment.NewLine}")
      yield $"%s{indentation}/// </summary>"
    }
    |> String.concat ""

let private generateParamsXmlDoc indentation (parms: (string * string) list list) =
  let paramXml (name, _type) =
    $"%s{indentation}/// <param name=\"%s{name}\"></param>"

  match parms with
  | [] -> ""
  | parameters ->
    parameters
    |> List.concat
    |> List.map (fun parameter -> paramXml parameter)
    |> String.concat Environment.NewLine

let private generateGenericParamsXmlDoc indentation parms =
  let genericArgXml name =
    $"%s{indentation}/// <typeparam name=\"'%s{name}\"></typeparam>"

  match parms with
  | [] -> ""
  | parameters ->
    parameters
    |> List.map (fun parameter -> genericArgXml parameter)
    |> String.concat Environment.NewLine

let private generateReturnsXmlDoc indentation =
  $"%s{indentation}/// <returns></returns>"

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
      let! (parseAndCheck, lineStr, sourceText) = getParseResultsForFile filePath fcsPos
      let topPos = goToTop fcsPos sourceText
      let showFix = isFixApplicable topPos sourceText

      match showFix with
      | true ->
        let origCommentContents = collectTrippleSlashComments topPos sourceText
        let indent = lineStr.IndexOf("///")
        let indentation = String.replicate indent " "
        let summaryXmlDoc = origCommentContents |> wrapInSummary indentation

        let startRange =
          { Line = topPos.Line - 1
            Character = 0 }

        let lastLineIdx = topPos.Line + origCommentContents.Length - 1
        let endRange = rangeToDeleteFullLine lastLineIdx sourceText

        let parms, genericParms =
          match tryGetBindingRange parseAndCheck.GetAST (endRange.Start |> protocolPosToPos) with
          | Some bindingRange ->
            let lStr = sourceText.GetLine bindingRange.Start |> Option.map LineStr

            match lStr with
            | Some s ->
              let signatureData = parseAndCheck.TryGetSignatureData bindingRange.Start s

              match signatureData with
              | Ok(_, parms, genericParms) -> parms, genericParms
              | Error _ -> List.empty, List.empty
            | _ -> List.empty, List.empty
          | None -> List.empty, List.empty

        let xmlDoc =
          seq {
            yield summaryXmlDoc
            yield generateParamsXmlDoc indentation parms
            yield generateGenericParamsXmlDoc indentation genericParms

            if not (List.isEmpty parms) then
              yield generateReturnsXmlDoc indentation
          }
          |> Seq.filter (fun s -> not (String.IsNullOrWhiteSpace(s)))
          |> String.concat Environment.NewLine

        let range =
          { Start = startRange
            End = endRange.Start }

        let e = { Range = range; NewText = xmlDoc }

        return
          [ { Edits = [| e |]
              File = codeActionParams.TextDocument
              Title = title
              SourceDiagnostic = None
              Kind = FixKind.Refactor } ]
      | false -> return []
    }
