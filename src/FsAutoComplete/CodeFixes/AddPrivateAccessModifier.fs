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
    -> NamedText
    -> ParseAndCheckResults
    -> Async<Result<FSharp.Compiler.Symbols.FSharpSymbol *
    System.Collections.Generic.IDictionary<FSharp.UMX.string<LocalPath>, FSharp.Compiler.Text.range array>, string>>

type private Placement =
  | Before
  | After

let private isLetInsideObjectModel input pos =
  SyntaxTraversal.Traverse(
    pos,
    input,
    { new SyntaxVisitorBase<_>() with
        member _.VisitModuleOrNamespace(_, synModuleOrNamespace) =

          let rec tryFind (decls: SynModuleDecl list) =
            decls
            |> List.tryPick (fun d ->
              match d with
              | SynModuleDecl.Let(range = range) when rangeContainsPos range pos -> None
              | SynModuleDecl.Types(typeDefns = typeDefns) ->
                typeDefns
                |> List.tryPick (fun td ->
                  match td with
                  | SynTypeDefn(typeRepr = SynTypeDefnRepr.ObjectModel(_, members, _)) ->
                    members
                    |> List.tryPick (fun m ->
                      match m with
                      | SynMemberDefn.LetBindings(range = range) when rangeContainsPos range pos -> Some()
                      | _ -> None)
                  | _ -> None)
              | SynModuleDecl.NestedModule(decls = nestedDecls) as m -> tryFind nestedDecls
              | _ -> None)

          match synModuleOrNamespace with
          | SynModuleOrNamespace(decls = decls) as s -> tryFind decls }
  )
  |> Option.isSome

let private getRangeToEdit input pos =
  let tryPickContainingRange (path: SyntaxVisitorPath) pos =
    path
    |> Seq.skip 1
    |> Seq.tryPick (fun p ->
      match p with
      | SyntaxNode.SynTypeDefn m when rangeContainsPos m.Range pos -> Some m.Range
      | SyntaxNode.SynModule m when rangeContainsPos m.Range pos -> Some m.Range
      | SyntaxNode.SynModuleOrNamespace m when rangeContainsPos m.Range pos -> Some m.Range
      | _ -> None)

  let rec findNested path decls =
    decls
    |> List.tryPick (fun d ->
      match d with
      | SynModuleDecl.NestedModule(
          moduleInfo = SynComponentInfo(longId = longId; accessibility = None); trivia = { ModuleKeyword = Some r }) as m when
        longId
        |> List.tryFind (fun i -> rangeContainsPos i.idRange pos)
        |> Option.isSome
        ->
        let editRange = r.WithStart r.End
        let path = (SyntaxNode.SynModule m) :: path

        match tryPickContainingRange path pos with
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
          | SynTypeDefn(typeRepr = SynTypeDefnRepr.ObjectModel(_, members, _)) as d ->
            let path = SyntaxNode.SynTypeDefn d :: path

            members
            |> List.tryPick (fun m ->
              match m with
              | SynMemberDefn.AutoProperty(accessibility = None; ident = ident; trivia = trivia) as a when
                rangeContainsPos ident.idRange pos
                ->
                let editRange =
                  trivia.LeadingKeyword.Range.WithStart trivia.LeadingKeyword.Range.End

                let path = SyntaxNode.SynMemberDefn a :: path

                match tryPickContainingRange path pos with
                | Some r -> Some(editRange, r, After)
                | _ -> None
              | _ -> None)
          | _ -> None)
      | _ -> None)

  let visitor =
    { new SyntaxVisitorBase<_>() with
        member _.VisitBinding(path, _, synBinding) =
          match synBinding with
          | SynBinding(headPat = headPat; kind = SynBindingKind.Normal) as s when
            rangeContainsPos s.RangeOfHeadPattern pos
            ->
            match headPat with
            | SynPat.LongIdent(longDotId = longDotId; accessibility = None; argPats = synArgPats) ->
              let posInArgs =
                synArgPats.Patterns |> List.exists (fun p -> rangeContainsPos p.Range pos)

              let posInFirstIdent =
                longDotId.LongIdent.Length > 1
                && rangeContainsPos longDotId.LongIdent[0].idRange pos

              if posInArgs || posInFirstIdent then
                None
              else
                let editRange = s.RangeOfHeadPattern.WithEnd s.RangeOfHeadPattern.Start

                match tryPickContainingRange path pos with
                | Some r -> Some(editRange, r, Before)
                | _ -> None
            | SynPat.Named(accessibility = None; isThisVal = false) ->
              let editRange = s.RangeOfHeadPattern.WithEnd s.RangeOfHeadPattern.Start

              match tryPickContainingRange path pos with
              | Some r -> Some(editRange, r, Before)
              | _ -> None
            | _ -> None
          | _ -> None

        member _.VisitModuleOrNamespace(path, synModuleOrNamespace) =
          match synModuleOrNamespace with
          | SynModuleOrNamespace(
              longId = longId
              accessibility = None
              trivia = { LeadingKeyword = SynModuleOrNamespaceLeadingKeyword.Module r }) when
            longId
            |> List.tryFind (fun i -> rangeContainsPos i.idRange pos)
            |> Option.isSome
            ->
            let editRange = r.WithStart r.End

            match tryPickContainingRange path pos with
            | Some r -> Some(editRange, r, After)
            | _ -> None
          | SynModuleOrNamespace(decls = decls) as mOrN ->
            let path = SyntaxNode.SynModuleOrNamespace mOrN :: path
            findNested path decls }

  if isLetInsideObjectModel input pos then
    None
  else
    SyntaxTraversal.Traverse(pos, input, visitor)

let fix (getParseResultsForFile: GetParseResultsForFile) (symbolUseWorkspace: SymbolUseWorkspace) : CodeFix =
  fun codeActionParams ->
    asyncResult {
      let filePath = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath
      let fcsPos = protocolPosToPos codeActionParams.Range.Start
      let! (parseAndCheck, lineStr, sourceText) = getParseResultsForFile filePath fcsPos
      let editRangeAndDeclRange = getRangeToEdit parseAndCheck.GetAST fcsPos

      match editRangeAndDeclRange with
      | Some(editRange, declRange, placement) ->

        let! (_, uses) = symbolUseWorkspace false true true fcsPos lineStr sourceText parseAndCheck
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
