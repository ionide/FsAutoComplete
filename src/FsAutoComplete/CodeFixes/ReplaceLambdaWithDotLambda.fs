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

[<RequireQualifiedAccess; NoComparison; NoEquality; Struct>]
type DotLambdaReplaceInfo =
  | NotNeedParen of range
  | NeedParen of underscoreRange: range * endRange: range

[<RequireQualifiedAccess; NoComparison; NoEquality; Struct>]
type FixType =
  | ReplaceToDotLambda of replaceInfo: ReplaceInfo
  | ReplaceToLambda of dotLambdaReplaceInfo: DotLambdaReplaceInfo

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
            ReplaceInfo.ParenLambda(lpr, mLambda, rpr) |> FixType.ReplaceToDotLambda |> Some
          | _ -> ReplaceInfo.RawLambda mLambda |> FixType.ReplaceToDotLambda |> Some
      | _ -> None

    | SyntaxNode.SynExpr(SynExpr.DotLambda(
        range = mLambda; trivia = { FSharp.Compiler.SyntaxTrivia.SynExprDotLambdaTrivia.UnderscoreRange = m })) ->

      match List.tryHead path with
      | Some(SyntaxNode.SynExpr(SynExpr.Paren _)) ->
        DotLambdaReplaceInfo.NotNeedParen m |> FixType.ReplaceToLambda |> Some
      | _ ->
        DotLambdaReplaceInfo.NeedParen(m, mkRange m.FileName mLambda.End mLambda.End)
        |> FixType.ReplaceToLambda
        |> Some
    | _ -> None)

let titleReplaceToDotLambda = "Replace lambda with _."
let titleReplaceToLambda = "Replace _. with lambda"

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
        | Some(FixType.ReplaceToDotLambda replaceInfo) ->
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
                Title = titleReplaceToDotLambda
                File = codeActionParams.TextDocument
                Edits = edits
                Kind = FixKind.Fix } ]
        | Some(FixType.ReplaceToLambda replaceInfo) ->
          let edits =
            match replaceInfo with
            | DotLambdaReplaceInfo.NeedParen(m, mEnd) ->
              [| { Range = fcsRangeToLsp m
                   NewText = "(fun _i -> _i" }
                 { Range = fcsRangeToLsp mEnd
                   NewText = ")" } |]

            | DotLambdaReplaceInfo.NotNeedParen m ->
              [| { Range = fcsRangeToLsp m
                   NewText = "fun _i -> _i" } |]

          return
            [ { SourceDiagnostic = None
                Title = titleReplaceToLambda
                File = codeActionParams.TextDocument
                Edits = edits
                Kind = FixKind.Fix } ]
    }
