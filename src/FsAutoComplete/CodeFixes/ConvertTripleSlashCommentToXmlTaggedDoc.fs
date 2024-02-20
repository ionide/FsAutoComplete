module FsAutoComplete.CodeFix.ConvertTripleSlashCommentToXmlTaggedDoc

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text.Range
open FSharp.Compiler.Xml
open System

let title = "Convert '///' comment to XML-tagged doc comment"

let private containsPosAndNotEmptyAndNotElaborated (pos: FSharp.Compiler.Text.Position) (xmlDoc: PreXmlDoc) =
  let containsPosAndNoSummaryPresent (xd: PreXmlDoc) =
    if rangeContainsPos xd.Range pos then
      let d = xd.ToXmlDoc(false, None)

      let summaryPresent =
        d.UnprocessedLines |> Array.exists (fun s -> s.Contains("<summary>"))

      not summaryPresent
    else
      false

  not xmlDoc.IsEmpty && containsPosAndNoSummaryPresent xmlDoc

let private isAstElemWithPreXmlDoc input pos =
  let (|Unelaborated|_|) xmlDoc = if containsPosAndNotEmptyAndNotElaborated pos xmlDoc then Some xmlDoc else None
  let (|AnyUnelaborated|_|) getXmlDoc = List.tryPick (getXmlDoc >> (|Unelaborated|_|))
  let field (SynField(xmlDoc = xmlDoc)) = xmlDoc
  let unionCase (SynUnionCase(xmlDoc = xmlDoc)) = xmlDoc
  let enumCase (SynEnumCase(xmlDoc = xmlDoc)) = xmlDoc

  (pos, input)
  ||> ParsedInput.tryPick (fun _path node ->
    match node with
    | SyntaxNode.SynModuleOrNamespace(SynModuleOrNamespace(xmlDoc = Unelaborated xmlDoc))
    | SyntaxNode.SynModule(SynModuleDecl.NestedModule(moduleInfo = SynComponentInfo(xmlDoc = Unelaborated xmlDoc)))
    | SyntaxNode.SynMemberDefn(SynMemberDefn.AutoProperty(xmlDoc = Unelaborated xmlDoc))
    | SyntaxNode.SynBinding(SynBinding(xmlDoc = Unelaborated xmlDoc))
    | SyntaxNode.SynTypeDefn(SynTypeDefn(typeInfo = SynComponentInfo(xmlDoc = Unelaborated xmlDoc)))
    | SyntaxNode.SynTypeDefn(SynTypeDefn(typeRepr = SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Record(recordFields = AnyUnelaborated field xmlDoc), _)))
    | SyntaxNode.SynTypeDefn(SynTypeDefn(typeRepr = SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Union(unionCases = AnyUnelaborated unionCase xmlDoc), _)))
    | SyntaxNode.SynTypeDefn(SynTypeDefn(typeRepr = SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Enum(cases = AnyUnelaborated enumCase xmlDoc), _))) ->
      Some xmlDoc
    | _ -> None)

let private collectCommentContents
  (startPos: FSharp.Compiler.Text.Position)
  (endPos: FSharp.Compiler.Text.Position)
  (sourceText: IFSACSourceText)
  =
  let rec loop (p: FSharp.Compiler.Text.Position) acc =
    if p.Line > endPos.Line then
      acc
    else
      let currentLine = sourceText.GetLine p

      match currentLine with
      | None -> acc
      | Some line ->
        let idx = line.IndexOf("///", StringComparison.Ordinal)

        if idx >= 0 then
          let existingComment = line.TrimStart().Substring(3).TrimStart()
          let acc = acc @ [ existingComment ]

          match sourceText.NextLine p with
          | None -> acc
          | Some nextLinePos -> loop nextLinePos acc
        else
          acc

  loop startPos List.empty

let private wrapInSummary indent comments =
  let indentation = String.replicate indent " "

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

let fix (getParseResultsForFile: GetParseResultsForFile) : CodeFix =
  fun codeActionParams ->
    asyncResult {
      let filePath = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath
      let fcsPos = protocolPosToPos codeActionParams.Range.Start
      let! (parseAndCheck, lineStr, sourceText) = getParseResultsForFile filePath fcsPos
      let showFix = isAstElemWithPreXmlDoc parseAndCheck.GetAST fcsPos

      match showFix with
      | Some xmlDoc ->
        let d = xmlDoc.ToXmlDoc(false, None)

        let origCommentContents =
          collectCommentContents d.Range.Start d.Range.End sourceText

        let indent = lineStr.IndexOf("///", StringComparison.Ordinal)
        let summaryXmlDoc = wrapInSummary indent origCommentContents

        let range =
          { Start = fcsPosToLsp (d.Range.Start.WithColumn 0)
            End = fcsPosToLsp (d.Range.End) }

        let e =
          { Range = range
            NewText = summaryXmlDoc }

        return
          [ { Edits = [| e |]
              File = codeActionParams.TextDocument
              Title = title
              SourceDiagnostic = None
              Kind = FixKind.Refactor } ]
      | None -> return []
    }
