module FsAutoComplete.CodeFix.AddPrivateAccessModifier

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text.Range

let title = "Add private access modifier"

type SymbolUseWorkspace =
  bool
    -> bool
    -> bool
    -> FSharp.Compiler.Text.Position
    -> LineStr
    -> IFSACSourceText
    -> ParseAndCheckResults
    -> Async<
      Result<
        System.Collections.Generic.IDictionary<FSharp.UMX.string<LocalPath>, FSharp.Compiler.Text.range array>,
        string
       >
     >

[<Struct>]
type private Placement =
  | Before
  | After

let private getRangesAndPlacement input pos =

  let getEditRangeForModule (attributes: SynAttributes) (moduleKeywordRange: FSharp.Compiler.Text.Range) posLine =
    match List.tryLast attributes with
    | Some a when a.Range.EndLine = posLine -> a.Range.WithStart a.Range.End
    | _ -> moduleKeywordRange.WithStart moduleKeywordRange.End

  let longIdentContainsPos (longIdent: LongIdent) (pos: FSharp.Compiler.Text.pos) =
    longIdent
    |> List.tryFind (fun i -> rangeContainsPos i.idRange pos)
    |> Option.isSome

  let isLetInsideObjectModel (path: SyntaxVisitorPath) pos =
    path
    |> List.exists (function
      | SyntaxNode.SynTypeDefn(SynTypeDefn(typeRepr = SynTypeDefnRepr.ObjectModel(_, members, _))) ->
        members
        |> List.exists (fun m ->
          match m with
          | SynMemberDefn.LetBindings(range = range) when rangeContainsPos range pos -> true
          | _ -> false)
      | _ -> false)

  let tryGetDeclContainingRange (path: SyntaxVisitorPath) pos =
    let skip =
      match path with
      | SyntaxNode.SynTypeDefn(SynTypeDefn(typeRepr = SynTypeDefnRepr.ObjectModel _)) :: _ -> 0 // keep containing range of ctor decl to class range
      | _ -> 1

    path
    |> Seq.skip skip
    |> Seq.tryPick (fun p ->
      match p with
      | SyntaxNode.SynTypeDefn m when rangeContainsPos m.Range pos -> Some m.Range
      | SyntaxNode.SynModule(SynModuleDecl.NestedModule(range = r)) when rangeContainsPos r pos -> Some r
      | SyntaxNode.SynModuleOrNamespace m when rangeContainsPos m.Range pos -> Some m.Range
      | _ -> None)

  let rec findNested path decls =
    decls
    |> List.tryPick (fun d ->
      match d with
      // Nested Module
      | SynModuleDecl.NestedModule(
          moduleInfo = SynComponentInfo(attributes = attributes; longId = longId; accessibility = None)
          trivia = { ModuleKeyword = Some r }) as m when longIdentContainsPos longId pos ->
        let editRange = getEditRangeForModule attributes r pos.Line
        let path = (SyntaxNode.SynModule m) :: path

        match tryGetDeclContainingRange path pos with
        | Some r -> Some(editRange, r, After)
        | _ -> None
      | SynModuleDecl.NestedModule(moduleInfo = moduleInfo; decls = decls) as m ->
        let path = (SyntaxNode.SynModule m) :: path

        match moduleInfo with
        | _ -> findNested path decls
      | SynModuleDecl.Types(typeDefns = typeDefns) as t ->
        let path = (SyntaxNode.SynModule t) :: path

        typeDefns
        |> List.tryPick (fun td ->
          match td with
          // Class Type
          | SynTypeDefn(
              typeInfo = SynComponentInfo(longId = longId; accessibility = None; range = r)
              typeRepr = SynTypeDefnRepr.ObjectModel _) as t when longIdentContainsPos longId pos ->
            let editRange = r.WithEnd r.Start
            let path = SyntaxNode.SynTypeDefn t :: path

            match tryGetDeclContainingRange path pos with
            | Some r -> Some(editRange, r, Before)
            | _ -> None
          // AutoProperty
          | SynTypeDefn(typeRepr = SynTypeDefnRepr.ObjectModel(_, members, _)) as t ->
            let path = SyntaxNode.SynTypeDefn t :: path

            members
            |> List.tryPick (fun m ->
              match m with
              | SynMemberDefn.AutoProperty(accessibility = None; ident = ident; trivia = trivia) as a when
                rangeContainsPos ident.idRange pos
                ->
                let editRange =
                  trivia.LeadingKeyword.Range.WithStart trivia.LeadingKeyword.Range.End

                let path = SyntaxNode.SynMemberDefn a :: path

                match tryGetDeclContainingRange path pos with
                | Some r -> Some(editRange, r, After)
                | _ -> None
              | _ -> None)
          // Type Abbreviation
          | SynTypeDefn(
              typeInfo = SynComponentInfo(accessibility = None; range = r)
              typeRepr = SynTypeDefnRepr.Simple(simpleRepr = SynTypeDefnSimpleRepr.TypeAbbrev _)) as t when
            rangeContainsPos r pos
            ->
            let editRange = r.WithEnd r.Start
            let path = SyntaxNode.SynTypeDefn t :: path

            match tryGetDeclContainingRange path pos with
            | Some r -> Some(editRange, r, Before)
            | _ -> None
          | _ -> None)
      | _ -> None)

  (pos, input)
  ||> ParsedInput.tryPick (fun path node ->
    match node with
    // explicit Ctor
    | SyntaxNode.SynBinding(SynBinding(
        valData = SynValData(memberFlags = Some({ MemberKind = SynMemberKind.Constructor })))) -> None
    | SyntaxNode.SynBinding(SynBinding(headPat = headPat; kind = SynBindingKind.Normal) as s) when
      rangeContainsPos s.RangeOfHeadPattern pos
      ->
      if isLetInsideObjectModel path pos then
        None
      else
        match headPat with
        | SynPat.LongIdent(longDotId = longDotId; accessibility = None) ->
          let posValidInSynLongIdent =
            longDotId.LongIdent
            |> List.skip (if longDotId.LongIdent.Length > 1 then 1 else 0)
            |> List.exists (fun i -> rangeContainsPos i.idRange pos)

          if not posValidInSynLongIdent then
            None
          else
            let editRange = s.RangeOfHeadPattern.WithEnd s.RangeOfHeadPattern.Start

            match tryGetDeclContainingRange path pos with
            | Some r -> Some(editRange, r, Before)
            | _ -> None
        | SynPat.Named(accessibility = None; isThisVal = false) ->
          let editRange = s.RangeOfHeadPattern.WithEnd s.RangeOfHeadPattern.Start

          match tryGetDeclContainingRange path pos with
          | Some r -> Some(editRange, r, Before)
          | _ -> None
        | _ -> None

    | SyntaxNode.SynModuleOrNamespace(SynModuleOrNamespace(
        longId = longId
        attribs = attribs
        accessibility = None
        trivia = { LeadingKeyword = SynModuleOrNamespaceLeadingKeyword.Module r }) as mOrN) when
      longIdentContainsPos longId pos
      ->
      let editRange = getEditRangeForModule attribs r pos.Line

      if path.Length = 0 then // Top level module
        Some(editRange, mOrN.Range, After)
      else
        match tryGetDeclContainingRange path pos with
        | Some r -> Some(editRange, r, After)
        | _ -> None

    | SyntaxNode.SynModuleOrNamespace(SynModuleOrNamespace(decls = decls) as mOrN) ->
      let path = SyntaxNode.SynModuleOrNamespace mOrN :: path
      findNested path decls

    | _ -> None)

let fix (getParseResultsForFile: GetParseResultsForFile) (symbolUseWorkspace: SymbolUseWorkspace) : CodeFix =
  fun codeActionParams ->
    asyncResult {
      let filePath = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath
      let fcsPos = protocolPosToPos codeActionParams.Range.Start
      let! (parseAndCheck, lineStr, sourceText) = getParseResultsForFile filePath fcsPos
      let rangesAndPlacement = getRangesAndPlacement parseAndCheck.GetAST fcsPos

      match rangesAndPlacement with
      | Some(editRange, declRange, placement) ->

        let! uses = symbolUseWorkspace false true true fcsPos lineStr sourceText parseAndCheck
        let useRanges = uses.Values |> Array.concat

        let usedOutsideOfDecl =
          useRanges
          |> Array.exists (fun usingRange ->
            usingRange.FileName <> editRange.FileName
            || not (rangeContainsRange declRange usingRange))

        if usedOutsideOfDecl then
          return []
        else
          let text =
            match placement with
            | Before -> "private "
            | After -> " private"

          let e =
            { Range = fcsRangeToLsp editRange
              NewText = text }

          return
            [ { Edits = [| e |]
                File = codeActionParams.TextDocument
                Title = title
                SourceDiagnostic = None
                Kind = FixKind.Refactor } ]
      | _ -> return []
    }
