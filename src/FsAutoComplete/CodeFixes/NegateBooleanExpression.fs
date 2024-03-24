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

let booleanOperators = set [ "||"; "&&"; "=" ] // TODO: probably some others

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
    let needsOuterParens =
      SynExpr.shouldBeParenthesizedInContext sourceText.GetLineString fixData.Path fixData.Expr

    let lpr, rpr = if fixData.NeedsParensAfterNot then "(", ")" else "", ""

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
      // Most code fixes have some general setup.
      // We initially want to detect the state of the current code and whether we can propose any text edits to the user.

      let fileName = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath
      // The converted LSP start position to an FCS start position.
      let fcsPos = protocolPosToPos codeActionParams.Range.Start
      // The syntax tree and typed tree, current line and sourceText of the current file.
      let! (parseAndCheckResults: ParseAndCheckResults, _line: string, sourceText: IFSACSourceText) =
        getParseResultsForFile fileName fcsPos

      let optFixData =
        ParsedInput.tryNode fcsPos parseAndCheckResults.GetParseResults.ParseTree
        |> Option.bind (fun (node, path) ->

          match node with
          | SyntaxNode.SynExpr e ->
            match e, path with
            // &&
            | BooleanOperator operator,
              SyntaxNode.SynExpr(SynExpr.App(isInfix = true)) :: SyntaxNode.SynExpr(SynExpr.App(isInfix = false) as e) :: rest ->
              Some
                { Expr = e
                  Ident = operator
                  Path = rest
                  NeedsParensAfterNot = true }

            // $0X().Y
            | SynExpr.Ident _,
              SyntaxNode.SynExpr(SynExpr.App _) :: SyntaxNode.SynExpr(SynExpr.DotGet(
                longDotId = LastIdentFromSynLongIdent ident) as e) :: rest ->
              Some
                { Expr = e
                  Ident = ident
                  Path = rest
                  NeedsParensAfterNot = true }

            // X$0()
            | SynExpr.Ident ident, SyntaxNode.SynExpr(SynExpr.App _ as e) :: rest ->
              Some
                { Expr = e
                  Ident = ident
                  Path = rest
                  NeedsParensAfterNot = true }

            // X()$0
            | (SynExpr.Const(constant = SynConst.Unit) | SynExpr.Paren _),
              SyntaxNode.SynExpr(SynExpr.App(funcExpr = SynExpr.Ident ident) as e) :: rest ->
              Some
                { Expr = e
                  Ident = ident
                  Path = rest
                  NeedsParensAfterNot = true }

            // X().Y$0
            | SynExpr.DotGet(longDotId = LastIdentFromSynLongIdent ident), path ->
              Some
                { Expr = e
                  Ident = ident
                  Path = path
                  NeedsParensAfterNot = true }

            // a.Y
            | SynExpr.LongIdent(isOptional = false; longDotId = LastIdentFromSynLongIdent ident), path ->
              Some
                { Expr = e
                  Ident = ident
                  Path = path
                  NeedsParensAfterNot = false }
            // a
            | SynExpr.Ident ident, path ->
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
