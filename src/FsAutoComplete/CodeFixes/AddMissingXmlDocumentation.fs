module FsAutoComplete.CodeFix.AddMissingXmlDocumentation

open FsAutoComplete
open FsAutoComplete.CodeFix.Types
open FsAutoComplete.LspHelpers
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text.Range
open FSharp.Compiler.Xml
open FsToolkit.ErrorHandling
open Ionide.LanguageServerProtocol.Types
open System

let title = "Add missing XML documentation"

let private tryGetExistingXmlDoc (pos: FSharp.Compiler.Text.Position) (xmlDoc: PreXmlDoc) =
  let containsPosAndSummaryPresent (xd: PreXmlDoc) =
    let d = xd.ToXmlDoc(false, None)

    if rangeContainsPos d.Range pos then
      let summaryPresent =
        d.UnprocessedLines |> Array.exists (fun s -> s.Contains("<summary>"))

      if summaryPresent then
        Some(d.UnprocessedLines, d.Range)
      else
        None
    else
      None

  if not xmlDoc.IsEmpty then
    containsPosAndSummaryPresent xmlDoc
  else
    None

let private tryGetCommentsAndSymbolPos input pos =

  let handleSynBinding defaultTraverse (synBinding: SynBinding) =
    let SynBinding(xmlDoc = xmlDoc; headPat = headPat) as s = synBinding
    let docAndDocRange = tryGetExistingXmlDoc pos xmlDoc

    match docAndDocRange with
    | Some(docLines, docRange) ->
      let symbolRange =
        match headPat with
        | SynPat.LongIdent(longDotId = longDotId) -> longDotId.Range.End
        | _ -> s.RangeOfHeadPattern.Start // for use statements

      Some(docLines, docRange, symbolRange)
    | None -> defaultTraverse synBinding

  SyntaxTraversal.Traverse(
    pos,
    input,
    { new SyntaxVisitorBase<_>() with

        member _.VisitBinding(_, defaultTraverse, synBinding) =
          handleSynBinding defaultTraverse synBinding

        member _.VisitLetOrUse(_, _, defaultTraverse, bindings, _) =
          bindings |> List.tryPick (handleSynBinding defaultTraverse)

        member _.VisitExpr(_, _, defaultTraverse, expr) = defaultTraverse expr

        member _.VisitModuleOrNamespace(_, synModuleOrNamespace) =
          match synModuleOrNamespace with
          | SynModuleOrNamespace(decls = decls) ->

            let rec findNested decls =
              decls
              |> List.tryPick (fun d ->
                match d with
                | SynModuleDecl.NestedModule(moduleInfo = moduleInfo; decls = decls) ->
                  match moduleInfo with
                  | _ -> findNested decls
                | SynModuleDecl.Types(typeDefns = typeDefns) ->
                  typeDefns
                  |> List.tryPick (fun td ->
                    match td with
                    | SynTypeDefn(typeRepr = SynTypeDefnRepr.ObjectModel(_, members, _)) ->
                      members
                      |> List.tryPick (fun m ->
                        match m with
                        | SynMemberDefn.AutoProperty(xmlDoc = xmlDoc; ident = ident) ->
                          let docAndDocRange = tryGetExistingXmlDoc pos xmlDoc

                          match docAndDocRange with
                          | Some(docLines, docRange) -> Some(docLines, docRange, ident.idRange.End)
                          | _ -> None
                        | _ -> None)
                    | _ -> None)
                | _ -> None)

            findNested decls }
  )

let fix (getParseResultsForFile: GetParseResultsForFile) : CodeFix =
  fun codeActionParams ->
    asyncResult {
      let filePath = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath
      let fcsPos = protocolPosToPos codeActionParams.Range.Start
      let! (parseAndCheck, lineStr, _sourceText) = getParseResultsForFile filePath fcsPos

      let parameterSection (name, _type) = $" <param name=\"%s{name}\"></param>"

      let genericArg name =
        $" <typeparam name=\"'%s{name}\"></typeparam>"

      let returnsSection = " <returns></returns>"

      let commentsAndPos = tryGetCommentsAndSymbolPos parseAndCheck.GetAST fcsPos

      match commentsAndPos with
      | Some(docLines, docRange, symbolPos) ->
        let lineStrOfSymbol = _sourceText.GetLine symbolPos |> Option.defaultValue ""
        let signatureData = parseAndCheck.TryGetSignatureData symbolPos lineStrOfSymbol

        match signatureData with
        | Ok(_, memberParameters, genericParameters) ->

          let trimmed = lineStr.TrimStart(' ')
          let indentLength = lineStr.Length - trimmed.Length
          let indentString = String.replicate indentLength " "

          let withAdded =
            let indexForParams =
              Array.FindLastIndex(docLines, (fun s -> s.Contains("</param>") || s.Contains("</summary>")))

            let missingParams =
              match memberParameters with
              | [] -> [||]
              | parameters ->
                parameters
                |> List.concat
                |> List.filter (fun (parameter, _) ->
                  docLines
                  |> Array.exists (fun c -> c.Contains($"<param name=\"{parameter}\">"))
                  |> not)
                |> List.mapi (fun _index parameter -> parameterSection parameter)
                |> Array.ofList

            if indexForParams = -1 then
              Array.append docLines missingParams
            else
              let (before, after) = Array.splitAt (indexForParams + 1) docLines
              Array.append before (Array.append missingParams after)

          let withAdded =
            let indexForTypeParams =
              Array.FindLastIndex(
                withAdded,
                (fun s -> s.Contains("</param>") || s.Contains("</typeparam>") || s.Contains("</summary>"))
              )

            let missingTypeParams =
              match genericParameters with
              | [] -> [||]
              | generics ->
                generics
                |> List.filter (fun generic ->
                  docLines
                  |> Array.exists (fun c -> c.Contains($"<typeparam name=\"'{generic}\">"))
                  |> not)
                |> List.mapi (fun _index generic -> genericArg generic)
                |> Array.ofList

            if indexForTypeParams = -1 then
              Array.append withAdded missingTypeParams
            else
              let (before, after) = Array.splitAt (indexForTypeParams + 1) withAdded
              Array.append before (Array.append missingTypeParams after)

          let withAdded =
            if withAdded |> Array.exists (fun s -> s.Contains("<returns>")) then
              withAdded
            else
              let indexForReturns =
                Array.FindLastIndex(
                  withAdded,
                  (fun s -> s.Contains("</param>") || s.Contains("</typeparam>") || s.Contains("</summary>"))
                )

              let (before, after) = Array.splitAt (indexForReturns + 1) withAdded
              Array.append before (Array.append [| returnsSection |] after)

          let formattedXmlDoc =
            withAdded
            |> Seq.mapi (fun i s -> if i = 0 then $"///{s}" else $"{indentString}///{s}")
            |> String.concat Environment.NewLine

          if docLines.Length = withAdded.Length then
            return []
          else
            let editRange = fcsRangeToLsp docRange

            return
              [ { Edits =
                    [| { Range = editRange
                         NewText = formattedXmlDoc } |]
                  File = codeActionParams.TextDocument
                  Title = title
                  SourceDiagnostic = None
                  Kind = FixKind.Refactor } ]
        | _ -> return []
      | _ -> return []
    }
