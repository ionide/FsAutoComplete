module FsAutoComplete.CodeFix.AddMissingXmlDocumentation

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text.Range
open FSharp.Compiler.Xml
open System

let title = "Add missing XML documentation"

let private tryGetExistingXmlDoc (pos: FSharp.Compiler.Text.Position) (xmlDoc: PreXmlDoc) =
  let containsPosAndSummaryPresent (xd: PreXmlDoc) =
    let d = xd.ToXmlDoc(false, None)

    if rangeContainsPos d.Range pos then
      let summaryPresent =
        d.UnprocessedLines |> Array.exists (fun s -> s.Contains("<summary>"))

      if summaryPresent then Some d.UnprocessedLines else None
    else
      None

  if not xmlDoc.IsEmpty then
    containsPosAndSummaryPresent xmlDoc
  else
    None

let private isLetWithSummary input pos =
  SyntaxTraversal.Traverse(
    pos,
    input,
    { new SyntaxVisitorBase<_>() with
        member _.VisitBinding(_, defaultTraverse, synBinding) =
          match synBinding with
          | SynBinding(xmlDoc = xmlDoc; headPat = headPat) as s ->
            let doc = tryGetExistingXmlDoc pos xmlDoc

            match doc with
            | Some d ->
              let r =
                match headPat with
                | SynPat.LongIdent(longDotId = longDotId) -> longDotId.Range.End
                | _ -> s.RangeOfHeadPattern.Start

              Some(d, r)
            | None -> defaultTraverse synBinding

        member _.VisitLetOrUse(_, _, defaultTraverse, bindings, _) =
          let isInLine b =
            match b with
            | SynBinding(xmlDoc = xmlDoc; headPat = headPat) as s ->
              let doc = tryGetExistingXmlDoc pos xmlDoc

              match doc with
              | Some d ->
                let r =
                  match headPat with
                  | SynPat.LongIdent(longDotId = longDotId) -> longDotId.Range.End
                  | _ -> s.RangeOfHeadPattern.Start

                Some(d, r)
              | None -> defaultTraverse b

          bindings |> List.tryPick isInLine

        member _.VisitExpr(_, _, defaultTraverse, expr) = defaultTraverse expr } // needed for nested let bindings
  )

let private isAutoPropertyWithSummary input pos =
  SyntaxTraversal.Traverse(
    pos,
    input,
    { new SyntaxVisitorBase<_>() with

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
                          let doc = tryGetExistingXmlDoc pos xmlDoc

                          match doc with
                          | Some d -> Some(d, ident.idRange.End)
                          | _ -> None
                        | _ -> None)
                    | _ -> None)
                | _ -> None)

            findNested decls }
  )

let private tryGetCommentsAndSymbolPos input pos =
  match isLetWithSummary input pos with
  | Some x -> Some x
  | _ -> isAutoPropertyWithSummary input pos

let fix (getParseResultsForFile: GetParseResultsForFile) : CodeFix =
  fun codeActionParams ->
    asyncResult {
      let filePath = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath
      let fcsPos = protocolPosToPos codeActionParams.Range.Start
      let! (parseAndCheck, lineStr, _sourceText) = getParseResultsForFile filePath fcsPos

      let parameterSection (name, _type) =
        $"/// <param name=\"%s{name}\"></param>"

      let genericArg name =
        $"/// <typeparam name=\"'%s{name}\"></typeparam>"

      let returnsSection = "/// <returns></returns>"

      let commentsAndPos = tryGetCommentsAndSymbolPos parseAndCheck.GetAST fcsPos

      match commentsAndPos with
      | Some(comments, symbolPos) ->
        let lineStrOfSymbol = _sourceText.GetLine symbolPos |> Option.defaultValue ""
        let signatureData = parseAndCheck.TryGetSignatureData symbolPos lineStrOfSymbol

        match signatureData with
        | Ok(_, memberParameters, genericParameters) ->

          let trimmed = lineStr.TrimStart(' ')
          let indentLength = lineStr.Length - trimmed.Length
          let indentString = String.replicate indentLength " "

          let formattedXmlDoc =
            seq {
              match memberParameters with
              | [] -> ()
              | parameters ->
                yield!
                  parameters
                  |> List.concat
                  |> List.filter (fun (parameter, _) -> comments |> Array.exists (fun c -> c.Contains($"<param name=\"{parameter}\">")) |> not)
                  |> List.mapi (fun _index parameter -> parameterSection parameter)

              match genericParameters with
              | [] -> ()
              | generics ->
                yield!
                  generics
                  |> List.filter (fun generic -> comments |> Array.exists (fun c -> c.Contains($"<typeparam name=\"'{generic}\">")) |> not)
                  |> List.mapi (fun _index generic -> genericArg generic)

              if comments |> Array.exists (fun s -> s.Contains("<returns>")) then
                ()
              else
                yield returnsSection
            }
            |> fun lines ->
              if Seq.isEmpty lines then
                None
              else
                lines
                |> Seq.map (fun s -> indentString + s)
                |> String.concat Environment.NewLine
                |> fun s -> Some (s + Environment.NewLine) // need a newline at the very end

          match formattedXmlDoc with
          | Some text ->
            // always insert at the start of the line, because we've prepended the indent to the doc strings
            let insertPosition = fcsPosToLsp (symbolPos.WithColumn 0)

            let editRange =
              { Start = insertPosition
                End = insertPosition }

            return
              [ { Edits = [| { Range = editRange; NewText = text } |]
                  File = codeActionParams.TextDocument
                  Title = title
                  SourceDiagnostic = None
                  Kind = FixKind.Refactor } ]
          | _ -> return []
        | _ -> return []
      | _ -> return []
    }
