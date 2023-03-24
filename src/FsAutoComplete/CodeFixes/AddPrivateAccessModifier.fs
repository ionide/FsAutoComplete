module FsAutoComplete.CodeFix.AddPrivateAccessModifier

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text.Range

let title = "add private access modifier"

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

  SyntaxTraversal.Traverse(
    pos,
    input,
    { new SyntaxVisitorBase<_>() with
        member _.VisitBinding(path, _, synBinding) =
          match synBinding with
          | SynBinding(headPat = headPat) as s when rangeContainsPos s.RangeOfHeadPattern pos ->
            match headPat with
            | SynPat.Named(accessibility = None)
            | SynPat.LongIdent(accessibility = None) ->
              let editRange = s.RangeOfHeadPattern.WithEnd s.RangeOfHeadPattern.Start

              match tryPickContainingRange path pos with
              | Some r -> Some(editRange, r)
              | _ -> None
            | _ -> None
          | _ -> None }
  )

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

let fix
  (getParseResultsForFile: GetParseResultsForFile)
  (getRangeText: GetRangeText)
  (symbolUseWorkspace: SymbolUseWorkspace)
  : CodeFix =
  fun codeActionParams ->
    asyncResult {
      let filePath = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath
      let fcsPos = protocolPosToPos codeActionParams.Range.Start
      let! (parseAndCheck, lineStr, sourceText) = getParseResultsForFile filePath fcsPos
      let editRangeAndDeclRange = getRangeToEdit parseAndCheck.GetAST fcsPos

      match editRangeAndDeclRange with
      | Some(r, declRange) ->

        let! (_, uses) = symbolUseWorkspace false true true fcsPos lineStr sourceText parseAndCheck
        let useRanges = uses.Values |> Array.concat

        let usedOutsideOfDecl =
          useRanges
          |> Array.exists (fun usingRange ->
            usingRange.FileName <> r.FileName
            || not (rangeContainsRange declRange usingRange))

        if usedOutsideOfDecl then
          return []
        else
          let e =
            { Range = fcsRangeToLsp r
              NewText = "private " }

          return
            [ { Edits = [| e |]
                File = codeActionParams.TextDocument
                Title = title
                SourceDiagnostic = None
                Kind = FixKind.Refactor } ]
      | _ -> return []
    }
