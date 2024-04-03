module FsAutoComplete.CodeFix.NegateBooleanExpression

open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text
open FsToolkit.ErrorHandling
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete.CodeFix.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

let title = "Negate boolean expression"

let booleanOperators = set [ "||"; "&&"; "="; "<>" ]

[<return: Struct>]
let (|BooleanOperator|_|) =
  function
  | SynExpr.LongIdent(
      longDotId = SynLongIdent(id = [ operatorIdent ]; trivia = [ Some(IdentTrivia.OriginalNotation operatorText) ])) ->
    if booleanOperators.Contains operatorText then
      ValueSome operatorIdent
    else
      ValueNone
  | _ -> ValueNone

[<return: Struct>]
let (|LastIdentFromSynLongIdent|_|) (SynLongIdent(id = id)) =
  match List.tryLast id with
  | None -> ValueNone
  | Some ident -> ValueSome ident

type FixData =
  { Expr: SynExpr
    Path: SyntaxVisitorPath
    Ident: Ident
    NeedsParensAfterNot: bool }

let mkFix (codeActionParams: CodeActionParams) (sourceText: ISourceText) fixData (returnType: FSharpType) =
  if
    returnType.IsFunctionType
    || returnType.BasicQualifiedName <> "Microsoft.FSharp.Core.bool"
  then
    []
  else

  let mExpr = fixData.Expr.Range
  let expr = fixData.Expr
  let notExpr = SynExpr.Ident(FSharp.Compiler.Syntax.Ident("not", mExpr.StartRange))

  let appExpr =
    SynExpr.App(ExprAtomicFlag.NonAtomic, false, notExpr, expr, expr.Range)

  let negatedPath, negatedExpr = SyntaxNode.SynExpr appExpr :: fixData.Path, expr

  // not (expr)
  let needsParensAfterNot =
    SynExpr.shouldBeParenthesizedInContext sourceText.GetLineString negatedPath negatedExpr

  let lpr, rpr = if needsParensAfterNot then "(", ")" else "", ""

  // (not expr)
  let needsOuterParens =
    let e =
      if not needsParensAfterNot then
        negatedExpr
      else
        SynExpr.App(
          ExprAtomicFlag.NonAtomic,
          false,
          notExpr,
          SynExpr.Paren(expr, expr.Range.StartRange, Some expr.Range.EndRange, expr.Range),
          expr.Range
        )

    SynExpr.shouldBeParenthesizedInContext sourceText.GetLineString fixData.Path e

  let newText =
    $"not %s{lpr}%s{sourceText.GetSubTextFromRange fixData.Expr.Range}%s{rpr}"
    |> (if not needsOuterParens then id else sprintf "(%s)")

  [ { SourceDiagnostic = None
      Title = title
      File = codeActionParams.TextDocument
      Edits =
        [| { Range = fcsRangeToLsp fixData.Expr.Range
             NewText = newText } |]
      Kind = FixKind.Fix } ]

let fix (getParseResultsForFile: GetParseResultsForFile) : CodeFix =
  fun (codeActionParams: CodeActionParams) ->
    asyncResult {
      let fileName = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath
      let fcsPos = protocolPosToPos codeActionParams.Range.Start

      let! (parseAndCheckResults: ParseAndCheckResults, _line: string, sourceText: IFSACSourceText) =
        getParseResultsForFile fileName fcsPos

      let optFixData =
        (fcsPos, parseAndCheckResults.GetParseResults.ParseTree)
        ||> ParsedInput.tryPick (fun path node ->
          match node with
          | SyntaxNode.SynExpr e ->
            match e with
            // a && b
            | SynExpr.App(isInfix = false; funcExpr = SynExpr.App(isInfix = true; funcExpr = BooleanOperator operator)) ->
              Some
                { Expr = e
                  Ident = operator
                  Path = path
                  NeedsParensAfterNot = true }

            // X.Y()
            | SynExpr.App(funcExpr = SynExpr.LongIdent(longDotId = LastIdentFromSynLongIdent ident))

            // X().Y()
            | SynExpr.App(funcExpr = SynExpr.DotGet(longDotId = LastIdentFromSynLongIdent ident))

            // X().Y
            | SynExpr.DotGet(longDotId = LastIdentFromSynLongIdent ident) ->
              Some
                { Expr = e
                  Ident = ident
                  Path = path
                  NeedsParensAfterNot = true }

            // X()
            | SynExpr.App(funcExpr = SynExpr.Ident ident) ->
              Some
                { Expr = e
                  Ident = ident
                  Path = path
                  NeedsParensAfterNot = true }

            // a.Y
            | SynExpr.LongIdent(isOptional = false; longDotId = LastIdentFromSynLongIdent ident) ->
              Some
                { Expr = e
                  Ident = ident
                  Path = path
                  NeedsParensAfterNot = false }
            // a
            | SynExpr.Ident ident ->
              Some
                { Expr = e
                  Ident = ident
                  Path = path
                  NeedsParensAfterNot = false }

            | _ -> None
          | _ -> None)

      match optFixData with
      | Some fixData ->
        let mExpr = fixData.Expr.Range

        if mExpr.StartLine <> mExpr.EndLine then
          // Only process single line expressions for now
          return []
        else

        match parseAndCheckResults.TryGetSymbolUseFromIdent sourceText fixData.Ident with
        | None -> return []
        | Some symbolUse ->

        match symbolUse.Symbol with
        | :? FSharpField as ff -> return mkFix codeActionParams sourceText fixData ff.FieldType
        | :? FSharpMemberOrFunctionOrValue as mfv ->
          return mkFix codeActionParams sourceText fixData mfv.ReturnParameter.Type
        | _ -> return []
      | _ -> return []
    }
