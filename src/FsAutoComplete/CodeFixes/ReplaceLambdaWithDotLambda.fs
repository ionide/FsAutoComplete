module FsAutoComplete.CodeFix.ReplaceLambdaWithDotLambda

open FSharp.Compiler.Syntax
open FsAutoComplete.FCSPatches
open FsToolkit.ErrorHandling
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete.CodeFix.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.Compiler.Text.Range
open FSharp.Compiler.Text

[<return: Struct>]
let (|LongIdentExprIdentifier|_|) =
  function
  | SynExpr.LongIdent(longDotId = SynLongIdent(id = identifierIdent :: _)) -> ValueSome identifierIdent
  | _ -> ValueNone

[<RequireQualifiedAccess>]
type ReplaceInfo =
  | RawLambda of range
  | ParenLambda of lpr: range * lambdaRange: range * rpr: range

let tryFindLambda (cursor: pos) (tree: ParsedInput) =
  (cursor, tree)
  ||> ParsedInput.tryPick (fun path node ->
    match node with
    | SyntaxNode.SynExpr(SynExpr.Lambda(parsedData = Some([ SynPat.Named(ident = SynIdent(patIdent, _)) ], bodyExpr)) as synExpr) ->
      match bodyExpr with
      // x.Foo
      | LongIdentExprIdentifier identifierIdent
      // x.Foo().Bar
      | SynExpr.DotGet(
        expr = SynExpr.App(ExprAtomicFlag.Atomic, false, LongIdentExprIdentifier identifierIdent, SynExpr.Paren _, _))
      // x.Foo()
      | SynExpr.App(ExprAtomicFlag.Atomic,
                    false,
                    LongIdentExprIdentifier identifierIdent,
                    (SynExpr.Const(constant = SynConst.Unit) | SynExpr.Paren _),
                    _) ->
        if identifierIdent.idText <> patIdent.idText then
          None
        else
          let mLambda =
            mkRange synExpr.Range.FileName synExpr.Range.Start identifierIdent.idRange.End

          match List.tryHead path with
          | Some(SyntaxNode.SynExpr(SynExpr.Paren(leftParenRange = lpr; rightParenRange = Some rpr))) ->
            Some(ReplaceInfo.ParenLambda(lpr, mLambda, rpr))
          | _ -> Some(ReplaceInfo.RawLambda mLambda)
      | _ -> None
    | _ -> None)

let title = "Replace lambda with _."

let languageFeature = lazy LanguageFeatureShim("AccessorFunctionShorthand")

let fix (getLanguageVersion: GetLanguageVersion) (getParseResultsForFile: GetParseResultsForFile) : CodeFix =
  fun (codeActionParams: CodeActionParams) ->
    asyncResult {
      let fileName = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath
      let! languageVersion = getLanguageVersion fileName

      if not (languageVersion.SupportsFeature languageFeature.Value) then
        return []
      else

        let fcsPos = protocolPosToPos codeActionParams.Range.Start

        let! (parseAndCheckResults: ParseAndCheckResults, _line: string, _sourceText: IFSACSourceText) =
          getParseResultsForFile fileName fcsPos

        match tryFindLambda fcsPos parseAndCheckResults.GetParseResults.ParseTree with
        | None -> return []
        | Some replaceInfo ->
          let edits =
            match replaceInfo with
            | ReplaceInfo.RawLambda m ->
              [| { Range = fcsRangeToLsp m
                   NewText = "_" } |]
            | ReplaceInfo.ParenLambda(lpr, mLambda, rpr) ->
              [| { Range = fcsRangeToLsp lpr
                   NewText = "" }
                 { Range = fcsRangeToLsp mLambda
                   NewText = " _" }
                 { Range = fcsRangeToLsp rpr
                   NewText = "" } |]

          return
            [ { SourceDiagnostic = None
                Title = title
                File = codeActionParams.TextDocument
                Edits = edits
                Kind = FixKind.Fix } ]
    }
