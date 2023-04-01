module FsAutoComplete.CodeFix.GenerateXmlDocumentation

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text.Range

let title = "Generate placeholder XML documentation"

let private longIdentContainsPos (longIdent: LongIdent) (pos: FSharp.Compiler.Text.pos) =
  longIdent
  |> List.tryFind (fun i -> rangeContainsPos i.idRange pos)
  |> Option.isSome

let private isLowerAstElemWithEmptyPreXmlDoc input pos =
  SyntaxTraversal.Traverse(
    pos,
    input,
    { new SyntaxVisitorBase<_>() with
        member _.VisitBinding(_, defaultTraverse, synBinding) =
          match synBinding with
          | SynBinding(xmlDoc = xmlDoc) as s when rangeContainsPos s.RangeOfBindingWithoutRhs pos && xmlDoc.IsEmpty ->
            Some()
          | _ -> defaultTraverse synBinding

        member _.VisitComponentInfo(_, synComponentInfo) =
          match synComponentInfo with
          | SynComponentInfo(longId = longId; xmlDoc = xmlDoc) when longIdentContainsPos longId pos && xmlDoc.IsEmpty ->
            Some()
          | _ -> None

        member _.VisitRecordDefn(_, fields, _) =
          let isInLine c =
            match c with
            | SynField(xmlDoc = xmlDoc; idOpt = Some ident) when rangeContainsPos ident.idRange pos && xmlDoc.IsEmpty ->
              Some()
            | _ -> None

          fields |> List.tryPick isInLine

        member _.VisitUnionDefn(_, cases, _) =
          let isInLine c =
            match c with
            | SynUnionCase(xmlDoc = xmlDoc; ident = (SynIdent(ident = ident))) when
              rangeContainsPos ident.idRange pos && xmlDoc.IsEmpty
              ->
              Some()
            | _ -> None

          cases |> List.tryPick isInLine

        member _.VisitEnumDefn(_, cases, _) =
          let isInLine b =
            match b with
            | SynEnumCase(xmlDoc = xmlDoc; ident = (SynIdent(ident = ident))) when
              rangeContainsPos ident.idRange pos && xmlDoc.IsEmpty
              ->
              Some()
            | _ -> None

          cases |> List.tryPick isInLine

        member _.VisitLetOrUse(_, _, defaultTraverse, bindings, _) =
          let isInLine b =
            match b with
            | SynBinding(xmlDoc = xmlDoc) as s when rangeContainsPos s.RangeOfBindingWithoutRhs pos && xmlDoc.IsEmpty ->
              Some()
            | _ -> defaultTraverse b

          bindings |> List.tryPick isInLine

        member _.VisitExpr(_, _, defaultTraverse, expr) = defaultTraverse expr } // needed for nested let bindings
  )

let private isModuleOrNamespaceOrAutoPropertyWithEmptyPreXmlDoc input pos =
  SyntaxTraversal.Traverse(
    pos,
    input,
    { new SyntaxVisitorBase<_>() with

        member _.VisitModuleOrNamespace(_, synModuleOrNamespace) =
          match synModuleOrNamespace with
          | SynModuleOrNamespace(longId = longId; xmlDoc = xmlDoc) when
            longIdentContainsPos longId pos && xmlDoc.IsEmpty
            ->
            Some()
          | SynModuleOrNamespace(decls = decls) ->

            let rec findNested decls =
              decls
              |> List.tryPick (fun d ->
                match d with
                | SynModuleDecl.NestedModule(moduleInfo = moduleInfo; decls = decls) ->
                  match moduleInfo with
                  | SynComponentInfo(longId = longId; xmlDoc = xmlDoc) when
                    longIdentContainsPos longId pos && xmlDoc.IsEmpty
                    ->
                    Some()
                  | _ -> findNested decls
                | SynModuleDecl.Types(typeDefns = typeDefns) ->
                  typeDefns
                  |> List.tryPick (fun td ->
                    match td with
                    | SynTypeDefn(typeRepr = SynTypeDefnRepr.ObjectModel(_, members, _)) ->
                      members
                      |> List.tryPick (fun m ->
                        match m with
                        | SynMemberDefn.AutoProperty(ident = ident; xmlDoc = xmlDoc) when
                          rangeContainsPos ident.idRange pos && xmlDoc.IsEmpty
                          ->
                          Some()
                        | _ -> None)
                    | _ -> None)
                | _ -> None)

            findNested decls }
  )

let private isAstElemWithEmptyPreXmlDoc input pos =
  match isLowerAstElemWithEmptyPreXmlDoc input pos with
  | Some xml -> Some xml
  | _ -> isModuleOrNamespaceOrAutoPropertyWithEmptyPreXmlDoc input pos

let fix (getParseResultsForFile: GetParseResultsForFile) : CodeFix =
  fun codeActionParams ->
    asyncResult {
      let filePath = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath
      let fcsPos = protocolPosToPos codeActionParams.Range.Start
      let! (parseAndCheck, lineStr, _sourceText) = getParseResultsForFile filePath fcsPos
      let showFix = isAstElemWithEmptyPreXmlDoc parseAndCheck.GetAST fcsPos

      match showFix with
      | Some _ ->
        let! docEdit = Commands.GenerateXmlDocumentation(parseAndCheck, fcsPos, lineStr)

        match docEdit with
        | Some({ InsertPosition = insertPosition
                 InsertText = formattedXmlDoc }) ->
          let protocolPos = fcsPosToLsp insertPosition

          let editRange =
            { Start = protocolPos
              End = protocolPos }

          let text = formattedXmlDoc

          return
            [ { Edits = [| { Range = editRange; NewText = text } |]
                File = codeActionParams.TextDocument
                Title = title
                SourceDiagnostic = None
                Kind = FixKind.Refactor } ]
        | _ -> return []
      | None -> return []
    }
