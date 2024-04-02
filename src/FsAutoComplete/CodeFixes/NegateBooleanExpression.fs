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

/// Update the range of an expression.
let updateRangeOfExpr (range: range) (expr: SynExpr) : SynExpr =
  match expr with
  | SynExpr.AddressOf(i, e, opRange, _) -> SynExpr.AddressOf(i, e, opRange, range)
  | SynExpr.Paren(expr, leftParenRange, rightParenRange, _) ->
    SynExpr.Paren(expr, leftParenRange, rightParenRange, range)
  | SynExpr.Quote(operator, isRaw, quotedExpr, isFromQueryExpression, _) ->
    SynExpr.Quote(operator, isRaw, quotedExpr, isFromQueryExpression, range)
  | SynExpr.Const(constant, _) -> SynExpr.Const(constant, range)
  | SynExpr.Typed(expr, targetType, _) -> SynExpr.Typed(expr, targetType, range)
  | SynExpr.Tuple(isStruct, exprs, commaRanges, _) -> SynExpr.Tuple(isStruct, exprs, commaRanges, range)
  | SynExpr.AnonRecd(isStruct, copyInfo, recordFields, _, trivia) ->
    SynExpr.AnonRecd(isStruct, copyInfo, recordFields, range, trivia)
  | SynExpr.ArrayOrList(isArray, exprs, _) -> SynExpr.ArrayOrList(isArray, exprs, range)
  | SynExpr.Record(baseInfo, copyInfo, recordFields, _) -> SynExpr.Record(baseInfo, copyInfo, recordFields, range)
  | SynExpr.New(isProtected, targetType, expr, _) -> SynExpr.New(isProtected, targetType, expr, range)
  | SynExpr.ObjExpr(objType, argOptions, withKeyword, bindings, members, extraImpls, newExprRange, _) ->
    SynExpr.ObjExpr(objType, argOptions, withKeyword, bindings, members, extraImpls, newExprRange, range)
  | SynExpr.While(whileDebugPoint, whileExpr, doExpr, _) -> SynExpr.While(whileDebugPoint, whileExpr, doExpr, range)
  | SynExpr.For(forDebugPoint, toDebugPoint, ident, equalsRange, identBody, direction, toBody, doBody, _) ->
    SynExpr.For(forDebugPoint, toDebugPoint, ident, equalsRange, identBody, direction, toBody, doBody, range)
  | SynExpr.ForEach(forDebugPoint, inDebugPoint, seqExprOnly, isFromSource, pat, enumExpr, bodyExpr, _) ->
    SynExpr.ForEach(forDebugPoint, inDebugPoint, seqExprOnly, isFromSource, pat, enumExpr, bodyExpr, range)
  | SynExpr.ArrayOrListComputed(isArray, expr, _) -> SynExpr.ArrayOrListComputed(isArray, expr, range)
  | SynExpr.IndexRange(expr1, opm, expr2, range1, range2, _) ->
    SynExpr.IndexRange(expr1, opm, expr2, range1, range2, range)
  | SynExpr.IndexFromEnd(expr, _) -> SynExpr.IndexFromEnd(expr, range)
  | SynExpr.ComputationExpr(hasSeqBuilder, expr, _) -> SynExpr.ComputationExpr(hasSeqBuilder, expr, range)
  | SynExpr.Lambda(fromMethod, inLambdaSeq, args, body, parsedData, _, trivia) ->
    SynExpr.Lambda(fromMethod, inLambdaSeq, args, body, parsedData, range, trivia)
  | SynExpr.MatchLambda(isExnMatch, keywordRange, matchClauses, matchDebugPoint, _) ->
    SynExpr.MatchLambda(isExnMatch, keywordRange, matchClauses, matchDebugPoint, range)
  | SynExpr.Match(matchDebugPoint, expr, clauses, _, trivia) ->
    SynExpr.Match(matchDebugPoint, expr, clauses, range, trivia)
  | SynExpr.Do(expr, _) -> SynExpr.Do(expr, range)
  | SynExpr.Assert(expr, _) -> SynExpr.Assert(expr, range)
  | SynExpr.App(flag, isInfix, funcExpr, argExpr, _) -> SynExpr.App(flag, isInfix, funcExpr, argExpr, range)
  | SynExpr.TypeApp(expr, lessRange, typeArgs, commaRanges, greaterRange, typeArgsRange, _) ->
    SynExpr.TypeApp(expr, lessRange, typeArgs, commaRanges, greaterRange, typeArgsRange, range)
  | SynExpr.LetOrUse(isRecursive, isUse, bindings, body, _, trivia) ->
    SynExpr.LetOrUse(isRecursive, isUse, bindings, body, range, trivia)
  | SynExpr.TryWith(tryExpr, withCases, _, tryDebugPoint, withDebugPoint, trivia) ->
    SynExpr.TryWith(tryExpr, withCases, range, tryDebugPoint, withDebugPoint, trivia)
  | SynExpr.TryFinally(tryExpr, finallyExpr, _, tryDebugPoint, finallyDebugPoint, trivia) ->
    SynExpr.TryFinally(tryExpr, finallyExpr, range, tryDebugPoint, finallyDebugPoint, trivia)
  | SynExpr.Lazy(expr, _) -> SynExpr.Lazy(expr, range)
  | SynExpr.Sequential(debugPoint, isTrueSeq, expr1, expr2, _) ->
    SynExpr.Sequential(debugPoint, isTrueSeq, expr1, expr2, range)
  | SynExpr.IfThenElse(ifExpr, thenExpr, elseExpr, spIfToThen, isFromErrorRecovery, _, trivia) ->
    SynExpr.IfThenElse(ifExpr, thenExpr, elseExpr, spIfToThen, isFromErrorRecovery, range, trivia)
  | SynExpr.Typar(typar, _) -> SynExpr.Typar(typar, range)
  | SynExpr.Ident(ident) -> SynExpr.Ident(FSharp.Compiler.Syntax.Ident(ident.idText, range))
  | SynExpr.LongIdent(isOptional, longDotId, altNameRefCell, _) ->
    SynExpr.LongIdent(isOptional, longDotId, altNameRefCell, range)
  | SynExpr.LongIdentSet(longDotId, expr, _) -> SynExpr.LongIdentSet(longDotId, expr, range)
  | SynExpr.DotGet(expr, rangeOfDot, longDotId, _) -> SynExpr.DotGet(expr, rangeOfDot, longDotId, range)
  | SynExpr.DotLambda(expr, _, trivia) -> SynExpr.DotLambda(expr, range, trivia)
  | SynExpr.DotSet(targetExpr, longDotId, rhsExpr, _) -> SynExpr.DotSet(targetExpr, longDotId, rhsExpr, range)
  | SynExpr.Set(targetExpr, rhsExpr, _) -> SynExpr.Set(targetExpr, rhsExpr, range)
  | SynExpr.DotIndexedGet(objectExpr, indexArgs, dotRange, _) ->
    SynExpr.DotIndexedGet(objectExpr, indexArgs, dotRange, range)
  | SynExpr.DotIndexedSet(objectExpr, indexArgs, valueExpr, leftOfSetRange, dotRange, _) ->
    SynExpr.DotIndexedSet(objectExpr, indexArgs, valueExpr, leftOfSetRange, dotRange, range)
  | SynExpr.NamedIndexedPropertySet(longDotId, expr1, expr2, _) ->
    SynExpr.NamedIndexedPropertySet(longDotId, expr1, expr2, range)
  | SynExpr.DotNamedIndexedPropertySet(targetExpr, longDotId, argExpr, rhsExpr, _) ->
    SynExpr.DotNamedIndexedPropertySet(targetExpr, longDotId, argExpr, rhsExpr, range)
  | SynExpr.TypeTest(expr, targetType, _) -> SynExpr.TypeTest(expr, targetType, range)
  | SynExpr.Upcast(expr, targetType, _) -> SynExpr.Upcast(expr, targetType, range)
  | SynExpr.Downcast(expr, targetType, _) -> SynExpr.Downcast(expr, targetType, range)
  | SynExpr.InferredUpcast(expr, _) -> SynExpr.InferredUpcast(expr, range)
  | SynExpr.InferredDowncast(expr, _) -> SynExpr.InferredDowncast(expr, range)
  | SynExpr.Null _ -> SynExpr.Null range
  | SynExpr.TraitCall(supportTys, traitSig, argExpr, _) -> SynExpr.TraitCall(supportTys, traitSig, argExpr, range)
  | SynExpr.JoinIn(lhsExpr, lhsRange, rhsExpr, _) -> SynExpr.JoinIn(lhsExpr, lhsRange, rhsExpr, range)
  | SynExpr.ImplicitZero _ -> SynExpr.ImplicitZero range
  | SynExpr.SequentialOrImplicitYield(debugPoint, expr1, expr2, ifNotStmt, _) ->
    SynExpr.SequentialOrImplicitYield(debugPoint, expr1, expr2, ifNotStmt, range)
  | SynExpr.YieldOrReturn(flags, expr, _) -> SynExpr.YieldOrReturn(flags, expr, range)
  | SynExpr.YieldOrReturnFrom(flags, expr, _) -> SynExpr.YieldOrReturnFrom(flags, expr, range)
  | SynExpr.LetOrUseBang(bindDebugPoint, isUse, isFromSource, pat, rhs, andBangs, body, _, trivia) ->
    SynExpr.LetOrUseBang(bindDebugPoint, isUse, isFromSource, pat, rhs, andBangs, body, range, trivia)
  | SynExpr.MatchBang(matchDebugPoint, expr, clauses, _, trivia) ->
    SynExpr.MatchBang(matchDebugPoint, expr, clauses, range, trivia)
  | SynExpr.DoBang(expr, _) -> SynExpr.DoBang(expr, range)
  | SynExpr.WhileBang(whileDebugPoint, whileExpr, doExpr, _) ->
    SynExpr.WhileBang(whileDebugPoint, whileExpr, doExpr, range)
  | SynExpr.LibraryOnlyILAssembly(ilCode, typeArgs, args, retTy, _) ->
    SynExpr.LibraryOnlyILAssembly(ilCode, typeArgs, args, retTy, range)
  | SynExpr.LibraryOnlyStaticOptimization(constraints, expr, optimizedExpr, _) ->
    SynExpr.LibraryOnlyStaticOptimization(constraints, expr, optimizedExpr, range)
  | SynExpr.LibraryOnlyUnionCaseFieldGet(expr, longId, fieldNum, _) ->
    SynExpr.LibraryOnlyUnionCaseFieldGet(expr, longId, fieldNum, range)
  | SynExpr.LibraryOnlyUnionCaseFieldSet(expr, longId, fieldNum, rhsExpr, _) ->
    SynExpr.LibraryOnlyUnionCaseFieldSet(expr, longId, fieldNum, rhsExpr, range)
  | SynExpr.ArbitraryAfterError(debugStr, _) -> SynExpr.ArbitraryAfterError(debugStr, range)
  | SynExpr.FromParseError(expr, _) -> SynExpr.FromParseError(expr, range)
  | SynExpr.DiscardAfterMissingQualificationAfterDot(expr, dotRange, _) ->
    SynExpr.DiscardAfterMissingQualificationAfterDot(expr, dotRange, range)
  | SynExpr.Fixed(expr, _) -> SynExpr.Fixed(expr, range)
  | SynExpr.InterpolatedString(contents, synStringKind, _) -> SynExpr.InterpolatedString(contents, synStringKind, range)
  | SynExpr.DebugPoint(debugPoint, isControlFlow, innerExpr) -> SynExpr.DebugPoint(debugPoint, isControlFlow, innerExpr)
  | SynExpr.Dynamic(funcExpr, qmark, argExpr, _) -> SynExpr.Dynamic(funcExpr, qmark, argExpr, range)

let mkFix (codeActionParams: CodeActionParams) (sourceText: ISourceText) fixData (returnType: FSharpType) =
  if
    returnType.IsFunctionType
    || returnType.BasicQualifiedName <> "Microsoft.FSharp.Core.bool"
  then
    []
  else

    let mExpr = fixData.Expr.Range

    let needsParensAfterNot =
      let path, exprAfter =
        // This is the rang of the expression moved 4 spaces so the `not` function can be applied.
        let mExprAfter =
          Range.mkRange
            mExpr.FileName
            (Position.mkPos mExpr.StartLine (mExpr.StartColumn + 4))
            (Position.mkPos mExpr.StartLine (mExpr.EndColumn + 4))

        let exprAfter = updateRangeOfExpr mExprAfter fixData.Expr

        let mNot =
          Range.mkRange mExpr.FileName mExpr.Start (Position.mkPos mExpr.StartLine (mExpr.StartColumn + 3))

        let notExpr = SynExpr.Ident(FSharp.Compiler.Syntax.Ident("not", mNot))

        let appExpr =
          SynExpr.App(
            ExprAtomicFlag.NonAtomic,
            false,
            notExpr,
            exprAfter,
            Range.unionRanges notExpr.Range exprAfter.Range
          )

        SyntaxNode.SynExpr appExpr :: fixData.Path, exprAfter

      SynExpr.shouldBeParenthesizedInContext sourceText.GetLineString path exprAfter

    let lpr, rpr = if needsParensAfterNot then "(", ")" else "", ""

    let needsOuterParens = false // TODO

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
