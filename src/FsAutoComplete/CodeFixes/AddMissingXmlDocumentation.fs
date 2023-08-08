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
  let tryGetSummaryIfContainsPos (xd: PreXmlDoc) =

    if rangeContainsPos xd.Range pos then
      let d = xd.ToXmlDoc(false, None)
      if Array.isEmpty d.UnprocessedLines then
        None
      elif d.UnprocessedLines |> Array.exists (fun s -> s.Contains("<summary>")) then
        Some(d.UnprocessedLines, xd.Range)
      else
        let lines =
          match d.UnprocessedLines with
          | [||] -> [| " <summary></summary>" |]
          | [| c |] -> [| $" <summary>%s{c.Trim()}</summary>" |]
          | cs -> [| yield " <summary>"; yield! cs; yield " </summary>" |]

        Some(lines, xd.Range)
    else
      None

  if not xmlDoc.IsEmpty then
    tryGetSummaryIfContainsPos xmlDoc
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

      Some(docLines, docRange, symbolRange, false)
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
                          | Some(docLines, docRange) -> Some(docLines, docRange, ident.idRange.End, true)
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
      | Some(docLines, docRange, symbolPos, isAutoProperty) ->
        let lineStrOfSymbol = _sourceText.GetLine symbolPos |> Option.defaultValue ""
        let signatureData = parseAndCheck.TryGetSignatureData symbolPos lineStrOfSymbol

        match signatureData with
        | Ok(_, memberParameters, genericParameters) ->

          let trimmed = lineStr.TrimStart(' ')
          let indentLength = lineStr.Length - trimmed.Length
          let indentString = String.replicate indentLength " "
          let docLines = List.ofArray docLines

          let withAdded =
            let indexForParams =
              docLines
              |> List.tryFindIndexBack (fun s -> s.Contains("</param>") || s.Contains("</summary>"))

            let missingParams =
              if isAutoProperty then
                // An auto property has a setter symbol which takes a parameter.
                // As the user didn't write this parameter, a missing parameter should not be returned.
                []
              else
                match memberParameters with
                | [] -> []
                | parameters ->
                  parameters
                  |> List.concat
                  |> List.filter (fun (parameter, _) ->
                    docLines
                    |> List.exists (fun c -> c.Contains($"<param name=\"%s{parameter}\">"))
                    |> not)
                  |> List.mapi (fun _index parameter -> parameterSection parameter)

            match indexForParams with
            | None -> List.append docLines missingParams
            | Some i -> List.insertManyAt (i + 1) missingParams docLines

          let withAdded =
            let indexForTypeParams =
              withAdded
              |> List.tryFindIndexBack (fun s ->
                s.Contains("</param>") || s.Contains("</typeparam>") || s.Contains("</summary>"))

            let missingTypeParams =
              match genericParameters with
              | [] -> []
              | generics ->
                generics
                |> List.filter (fun generic ->
                  docLines
                  |> List.exists (fun c -> c.Contains($"<typeparam name=\"'%s{generic}\">"))
                  |> not)
                |> List.mapi (fun _index generic -> genericArg generic)

            match indexForTypeParams with
            | None -> List.append withAdded missingTypeParams
            | Some i -> List.insertManyAt (i + 1) missingTypeParams withAdded

          let withAdded =
            if withAdded |> List.exists (fun s -> s.Contains("<returns>")) then
              withAdded
            else
              let indexForReturns =
                withAdded
                |> List.tryFindIndexBack (fun s ->
                  s.Contains("</param>") || s.Contains("</typeparam>") || s.Contains("</summary>"))

              match indexForReturns with
              | None -> List.append withAdded [ returnsSection ]
              | Some i -> List.insertManyAt (i + 1) [ returnsSection ] withAdded

          let formattedXmlDoc =
            withAdded
            |> Seq.mapi (fun i s -> if i = 0 then $"///%s{s}" else $"%s{indentString}///%s{s}")
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
