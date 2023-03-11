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
  let d = xmlDoc.ToXmlDoc(false, None)

  if rangeContainsPos d.Range pos then
    let summaryPresent =
      d.UnprocessedLines |> Array.exists (fun s -> s.Contains("<summary>"))

    not xmlDoc.IsEmpty && not summaryPresent
  else
    false

let private isLowerAstElemWithPreXmlDoc input pos =
  SyntaxTraversal.Traverse(
    pos,
    input,
    { new SyntaxVisitorBase<_>() with
        member _.VisitBinding(_, defaultTraverse, synBinding) =
          match synBinding with
          | SynBinding(xmlDoc = xmlDoc) when containsPosAndNotEmptyAndNotElaborated pos xmlDoc -> Some xmlDoc
          | _ -> defaultTraverse synBinding

        member _.VisitComponentInfo(_, synComponentInfo) =
          match synComponentInfo with
          | SynComponentInfo(xmlDoc = xmlDoc) when containsPosAndNotEmptyAndNotElaborated pos xmlDoc -> Some xmlDoc
          | _ -> None

        member _.VisitInheritSynMemberDefn(_, _, _, _, members, _) =
          let isInLine c =
            match c with
            // ToDo not working
            | SynMemberDefn.AutoProperty(xmlDoc = xmlDoc) when containsPosAndNotEmptyAndNotElaborated pos xmlDoc ->
              Some xmlDoc
            | SynMemberDefn.ImplicitCtor(xmlDoc = xmlDoc) when containsPosAndNotEmptyAndNotElaborated pos xmlDoc ->
              Some xmlDoc
            | _ -> None

          members |> List.tryPick isInLine

        member _.VisitRecordDefn(_, fields, _) =
          let isInLine c =
            match c with
            | SynField(xmlDoc = xmlDoc) when containsPosAndNotEmptyAndNotElaborated pos xmlDoc -> Some xmlDoc
            | _ -> None

          fields |> List.tryPick isInLine

        member _.VisitUnionDefn(_, cases, _) =
          let isInLine c =
            match c with
            | SynUnionCase(xmlDoc = xmlDoc) when containsPosAndNotEmptyAndNotElaborated pos xmlDoc -> Some xmlDoc
            | _ -> None

          cases |> List.tryPick isInLine

        member _.VisitEnumDefn(_, cases, _) =
          let isInLine b =
            match b with
            | SynEnumCase(xmlDoc = xmlDoc) when containsPosAndNotEmptyAndNotElaborated pos xmlDoc -> Some xmlDoc
            | _ -> None

          cases |> List.tryPick isInLine

        member _.VisitLetOrUse(_, _, defaultTraverse, bindings, _) =
          let isInLine b =
            match b with
            | SynBinding(xmlDoc = xmlDoc) when containsPosAndNotEmptyAndNotElaborated pos xmlDoc -> Some xmlDoc
            | _ -> defaultTraverse b

          bindings |> List.tryPick isInLine

        member _.VisitExpr(_, _, defaultTraverse, expr) = defaultTraverse expr

        member _.VisitImplicitInherit(_, defaultTraverse, _, synArgs, _) = defaultTraverse synArgs

        member _.VisitType(_, defaultTraverse, synType) = defaultTraverse synType

        member _.VisitMatchClause(_, defaultTraverse, matchClause) = defaultTraverse matchClause

        member _.VisitModuleDecl(_, defaultTraverse, synModuleDecl) = defaultTraverse synModuleDecl

        member _.VisitPat(_, defaultTraverse, synPat) = defaultTraverse synPat }
  )

let private isModuleOrNamespaceWithPreXmlDoc input pos =
  SyntaxTraversal.Traverse(
    pos,
    input,
    { new SyntaxVisitorBase<_>() with

        member _.VisitModuleOrNamespace(_, synModuleOrNamespace) =
          match synModuleOrNamespace with
          | SynModuleOrNamespace(xmlDoc = xmlDoc) when containsPosAndNotEmptyAndNotElaborated pos xmlDoc -> Some xmlDoc
          | SynModuleOrNamespace(decls = decls) ->

            let rec findNested decls =
              decls
              |> List.tryPick (fun d ->
                match d with
                | SynModuleDecl.NestedModule(moduleInfo = moduleInfo; decls = decls) ->
                  match moduleInfo with
                  | SynComponentInfo(xmlDoc = xmlDoc) when containsPosAndNotEmptyAndNotElaborated pos xmlDoc ->
                    Some xmlDoc
                  | _ -> findNested decls
                | _ -> None)

            findNested decls }
  )

let private isAstElemWithPreXmlDoc input pos =
  match isLowerAstElemWithPreXmlDoc input pos with
  | Some xml -> Some xml
  | _ -> isModuleOrNamespaceWithPreXmlDoc input pos

let private collectCommentContents
  (startPos: FSharp.Compiler.Text.Position)
  (endPos: FSharp.Compiler.Text.Position)
  (sourceText: NamedText)
  =
  let rec loop (p: FSharp.Compiler.Text.Position) acc =
    if p.Line > endPos.Line then
      acc
    else
      let currentLine = sourceText.GetLine p

      match currentLine with
      | None -> acc
      | Some line ->
        let idx = line.IndexOf("///")

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

let fix (getParseResultsForFile: GetParseResultsForFile) (getRangeText: GetRangeText) : CodeFix =
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

        let indent = lineStr.IndexOf("///")
        let summaryXmlDoc = wrapInSummary indent origCommentContents

        let range =
          { Start =
              { Character = 0
                Line = d.Range.Start.Line - 1 } // LSP is 0-based
            End =
              { Character = d.Range.End.Column
                Line = d.Range.End.Line - 1 } }

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
