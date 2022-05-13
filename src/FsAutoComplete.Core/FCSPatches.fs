/// this file contains patches to the F# Compiler Service that have not yet made it into
/// published nuget packages.  We source-copy them here to have a consistent location for our to-be-removed extensions

module FsAutoComplete.FCSPatches

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FsAutoComplete.UntypedAstUtils
open FSharp.Compiler.CodeAnalysis

module internal SynExprAppLocationsImpl =
  let rec private searchSynArgExpr traverseSynExpr expr ranges =
    match expr with
    | SynExpr.Const (SynConst.Unit, _) -> None, None

    | SynExpr.Paren (SynExpr.Tuple (_, exprs, _commas, _tupRange), _, _, _parenRange) ->
      let rec loop (exprs: SynExpr list) ranges =
        match exprs with
        | [] -> ranges
        | h :: t -> loop t (h.Range :: ranges)

      let res = loop exprs ranges
      Some(res), None

    | SynExpr.Paren (SynExpr.Paren (_, _, _, _) as synExpr, _, _, _parenRange) ->
      let r, _cacheOpt = searchSynArgExpr traverseSynExpr synExpr ranges
      r, None

    | SynExpr.Paren (SynExpr.App (_, _isInfix, _, _, _range), _, _, parenRange) -> Some(parenRange :: ranges), None

    | e ->
      let inner = traverseSynExpr e

      match inner with
      | None -> Some(e.Range :: ranges), Some inner
      | _ -> None, Some inner

  let getAllCurriedArgsAtPosition pos parseTree =
    SyntaxTraversal.Traverse(
      pos,
      parseTree,
      { new SyntaxVisitorBase<_>() with
          member _.VisitExpr(_path, traverseSynExpr, defaultTraverse, expr) =
            match expr with
            | SynExpr.App (_exprAtomicFlag, _isInfix, funcExpr, argExpr, range) when Position.posEq pos range.Start ->
              let isInfixFuncExpr =
                match funcExpr with
                | SynExpr.App (_, isInfix, _, _, _) -> isInfix
                | _ -> false

              if isInfixFuncExpr then
                traverseSynExpr funcExpr
              else
                let workingRanges =
                  match traverseSynExpr funcExpr with
                  | Some ranges -> ranges
                  | None -> []

                let xResult, cacheOpt = searchSynArgExpr traverseSynExpr argExpr workingRanges

                match xResult, cacheOpt with
                | Some ranges, _ -> Some ranges
                | None, Some cache -> cache
                | _ -> traverseSynExpr argExpr
            | _ -> defaultTraverse expr }
    )
    |> Option.map List.rev

type FSharpParseFileResults with

  member scope.IsPositionContainedInACurriedParameter pos =
    let result =
      SyntaxTraversal.Traverse(
        pos,
        scope.ParseTree,
        { new SyntaxVisitorBase<_>() with
            member __.VisitExpr(_path, traverseSynExpr, defaultTraverse, expr) = defaultTraverse (expr)

            override __.VisitBinding(_, _, binding) =
              match binding with
              | SynBinding (valData = valData; range = ((ContainsPos pos) as range)) ->
                let info = valData.SynValInfo.CurriedArgInfos
                let mutable found = false

                for group in info do
                  for arg in group do
                    match arg.Ident with
                    | Some (IdentContainsPos pos) -> found <- true
                    | _ -> ()

                if found then Some range else None
              | _ -> None }
      )

    result.IsSome

  member scope.TryRangeOfParenEnclosingOpEqualsGreaterUsage opGreaterEqualPos =
    /// reused pattern to find applications of => (a symptom of improper C# style lambdas)
    let (|InfixAppOfOpEqualsGreater|_|) =
      function
      | SynExpr.App (ExprAtomicFlag.NonAtomic,
                     false,
                     SynExpr.App (ExprAtomicFlag.NonAtomic, true, Ident "op_EqualsGreater", actualParamListExpr, _),
                     actualLambdaBodyExpr,
                     _) -> Some(actualParamListExpr, actualLambdaBodyExpr)
      | _ -> None


    let visitor =
      { new SyntaxVisitorBase<_>() with
          member _.VisitExpr(_, _, defaultTraverse, expr) =
            match expr with
            | SynExpr.Paren ((InfixAppOfOpEqualsGreater (lambdaArgs, lambdaBody) as app), _, _, _) ->
              Some(app.Range, lambdaArgs.Range, lambdaBody.Range)
            | _ -> defaultTraverse expr

          member _.VisitBinding(_, defaultTraverse, binding) =
            match binding with
            | SynBinding (kind = SynBindingKind.Normal; expr = InfixAppOfOpEqualsGreater (lambdaArgs, lambdaBody) as app) ->
              Some(app.Range, lambdaArgs.Range, lambdaBody.Range)
            | _ -> defaultTraverse binding }

    SyntaxTraversal.Traverse(opGreaterEqualPos, scope.ParseTree, visitor)

  member scope.TryRangeOfRefCellDereferenceContainingPos expressionPos =
    SyntaxTraversal.Traverse(
      expressionPos,
      scope.ParseTree,
      { new SyntaxVisitorBase<_>() with
          member _.VisitExpr(_, _, defaultTraverse, expr) =
            match expr with
            | SynExpr.App (_, false, SynExpr.Ident funcIdent, expr, _) ->
              if funcIdent.idText = "op_Dereference"
                 && Range.rangeContainsPos expr.Range expressionPos then
                Some funcIdent.idRange
              else
                None
            | _ -> defaultTraverse expr }
    )

  member scope.TryRangeOfRecordExpressionContainingPos pos =
    SyntaxTraversal.Traverse(
      pos,
      scope.ParseTree,
      { new SyntaxVisitorBase<_>() with
          member _.VisitExpr(_, _, defaultTraverse, expr) =
            match expr with
            | SynExpr.Record (_, _, _, range) when Range.rangeContainsPos range pos -> Some range
            | _ -> defaultTraverse expr }
    )

  member scope.TryRangeOfExprInYieldOrReturn pos =
    SyntaxTraversal.Traverse(
      pos,
      scope.ParseTree,
      { new SyntaxVisitorBase<_>() with
          member __.VisitExpr(_path, _, defaultTraverse, expr) =
            match expr with
            | SynExpr.YieldOrReturn (_, expr, range)
            | SynExpr.YieldOrReturnFrom (_, expr, range) when Range.rangeContainsPos range pos -> Some expr.Range
            | _ -> defaultTraverse expr }
    )

  /// Attempts to find an Ident of a pipeline containing the given position, and the number of args already applied in that pipeline.
  /// For example, '[1..10] |> List.map ' would give back the ident of '|>' and 1, because it applied 1 arg (the list) to 'List.map'.
  member scope.TryIdentOfPipelineContainingPosAndNumArgsApplied pos =
    SyntaxTraversal.Traverse(
      pos,
      scope.ParseTree,
      { new SyntaxVisitorBase<_>() with
          member _.VisitExpr(_, _, defaultTraverse, expr) =
            match expr with
            | SynExpr.App (_, _, SynExpr.App (_, true, SynExpr.Ident ident, _, _), argExpr, _) when
              Range.rangeContainsPos argExpr.Range pos
              ->
              if ident.idText = "op_PipeRight" then
                Some(ident, 1)
              elif ident.idText = "op_PipeRight2" then
                Some(ident, 2)
              elif ident.idText = "op_PipeRight3" then
                Some(ident, 3)
              else
                None
            | _ -> defaultTraverse expr }
    )

  /// Determines if the given position is inside a function or method application.
  member scope.IsPosContainedInApplicationPatched pos =
    let result =
      SyntaxTraversal.Traverse(
        pos,
        scope.ParseTree,
        { new SyntaxVisitorBase<_>() with
            member _.VisitExpr(_, traverseSynExpr, defaultTraverse, expr) =
              match expr with
              | SynExpr.TypeApp (_, _, _, _, _, _, range) when Range.rangeContainsPos range pos -> Some range
              | SynExpr.App (_, _, _, SynExpr.ComputationExpr (_, expr, _), range) when Range.rangeContainsPos range pos ->
                traverseSynExpr expr
              | SynExpr.App (_, _, _, _, range) when Range.rangeContainsPos range pos -> Some range
              | _ -> defaultTraverse expr }
      )

    result.IsSome

  /// Attempts to find the range of a function or method that is being applied. Also accounts for functions in pipelines.
  member scope.TryRangeOfFunctionOrMethodBeingAppliedPatched pos =
    let rec getIdentRangeForFuncExprInApp traverseSynExpr expr pos : Range option =
      match expr with
      | SynExpr.Ident ident -> Some ident.idRange

      | SynExpr.LongIdent (_, _, _, range) -> Some range

      | SynExpr.Paren (expr, _, _, range) when Range.rangeContainsPos range pos ->
        getIdentRangeForFuncExprInApp traverseSynExpr expr pos

      | SynExpr.TypeApp (expr, _, _, _, _, _, _) -> getIdentRangeForFuncExprInApp traverseSynExpr expr pos

      | SynExpr.App (_, _, funcExpr, argExpr, _) ->
        match argExpr with
        | SynExpr.App (_, _, _, _, range) when Range.rangeContainsPos range pos ->
          getIdentRangeForFuncExprInApp traverseSynExpr argExpr pos

        // Special case: `async { ... }` is actually a CompExpr inside of the argExpr of a SynExpr.App
        | SynExpr.ComputationExpr (_, expr, range) when Range.rangeContainsPos range pos ->
          getIdentRangeForFuncExprInApp traverseSynExpr expr pos

        | SynExpr.Paren (expr, _, _, range) when Range.rangeContainsPos range pos ->
          getIdentRangeForFuncExprInApp traverseSynExpr expr pos

        | _ ->
          match funcExpr with
          | SynExpr.App (_, true, _, _, _) when Range.rangeContainsPos argExpr.Range pos ->
            // x |> List.map
            // Don't dive into the funcExpr (the operator expr)
            // because we dont want to offer sig help for that!
            getIdentRangeForFuncExprInApp traverseSynExpr argExpr pos
          | _ ->
            // Generally, we want to dive into the func expr to get the range
            // of the identifier of the function we're after
            getIdentRangeForFuncExprInApp traverseSynExpr funcExpr pos

      | SynExpr.LetOrUse (bindings = bindings; body = body; range = range) when Range.rangeContainsPos range pos ->
        let binding =
          bindings
          |> List.tryFind (fun x -> Range.rangeContainsPos x.RangeOfBindingWithRhs pos)

        match binding with
        | Some (SynBinding (expr = expr)) -> getIdentRangeForFuncExprInApp traverseSynExpr expr pos
        | None -> getIdentRangeForFuncExprInApp traverseSynExpr body pos

      | SynExpr.IfThenElse (ifExpr = ifExpr; thenExpr = thenExpr; elseExpr = elseExpr; range = range) when
        Range.rangeContainsPos range pos
        ->
        if Range.rangeContainsPos ifExpr.Range pos then
          getIdentRangeForFuncExprInApp traverseSynExpr ifExpr pos
        elif Range.rangeContainsPos thenExpr.Range pos then
          getIdentRangeForFuncExprInApp traverseSynExpr thenExpr pos
        else
          match elseExpr with
          | None -> None
          | Some expr -> getIdentRangeForFuncExprInApp traverseSynExpr expr pos

      | SynExpr.Match (expr = expr; clauses = clauses; range = range) when Range.rangeContainsPos range pos ->
        if Range.rangeContainsPos expr.Range pos then
          getIdentRangeForFuncExprInApp traverseSynExpr expr pos
        else
          let clause =
            clauses
            |> List.tryFind (fun clause -> Range.rangeContainsPos clause.Range pos)

          match clause with
          | None -> None
          | Some clause ->
            match clause with
            | SynMatchClause (whenExpr = whenExpr; resultExpr = resultExpr) ->
              match whenExpr with
              | None -> getIdentRangeForFuncExprInApp traverseSynExpr resultExpr pos
              | Some whenExpr ->
                if Range.rangeContainsPos whenExpr.Range pos then
                  getIdentRangeForFuncExprInApp traverseSynExpr whenExpr pos
                else
                  getIdentRangeForFuncExprInApp traverseSynExpr resultExpr pos


      // Ex: C.M(x, y, ...) <--- We want to find where in the tupled application the call is being made
      | SynExpr.Tuple (_, exprs, _, tupRange) when Range.rangeContainsPos tupRange pos ->
        let expr =
          exprs
          |> List.tryFind (fun expr -> Range.rangeContainsPos expr.Range pos)

        match expr with
        | None -> None
        | Some expr -> getIdentRangeForFuncExprInApp traverseSynExpr expr pos

      // Capture the body of a lambda, often nested in a call to a collection function
      | SynExpr.Lambda (body = body) when Range.rangeContainsPos body.Range pos ->
        getIdentRangeForFuncExprInApp traverseSynExpr body pos

      | SynExpr.Do (expr, range) when Range.rangeContainsPos range pos ->
        getIdentRangeForFuncExprInApp traverseSynExpr expr pos

      | SynExpr.Assert (expr, range) when Range.rangeContainsPos range pos ->
        getIdentRangeForFuncExprInApp traverseSynExpr expr pos

      | SynExpr.ArbitraryAfterError (_debugStr, range) when Range.rangeContainsPos range pos -> Some range

      | expr ->
        traverseSynExpr expr
        |> Option.map (fun expr -> expr)


    SyntaxTraversal.Traverse(
      pos,
      scope.ParseTree,
      { new SyntaxVisitorBase<_>() with
          member _.VisitExpr(_, traverseSynExpr, defaultTraverse, expr) =
            match expr with
            | SynExpr.TypeApp (expr, _, _, _, _, _, range) when Range.rangeContainsPos range pos ->
              getIdentRangeForFuncExprInApp traverseSynExpr expr pos
            | SynExpr.App (_, _, _funcExpr, _, range) as app when Range.rangeContainsPos range pos ->
              getIdentRangeForFuncExprInApp traverseSynExpr app pos
            | _ -> defaultTraverse expr }
    )

  /// Gets the ranges of all arguments, if they can be found, for a function application at the given position.
  member scope.GetAllArgumentsForFunctionApplicationAtPostion pos =
    SynExprAppLocationsImpl.getAllCurriedArgsAtPosition pos scope.ParseTree


  member scope.TryRangeOfExpressionBeingDereferencedContainingPos expressionPos =
    SyntaxTraversal.Traverse(
      expressionPos,
      scope.ParseTree,
      { new SyntaxVisitorBase<_>() with
          member _.VisitExpr(_, _, defaultTraverse, expr) =
            match expr with
            | SynExpr.App (_, false, SynExpr.Ident funcIdent, expr, _) ->
              if funcIdent.idText = "op_Dereference"
                 && Range.rangeContainsPos expr.Range expressionPos then
                Some expr.Range
              else
                None
            | _ -> defaultTraverse expr }
    )

  /// Attempts to find the range of the string interpolation that contains a given position.
  member scope.TryRangeOfStringInterpolationContainingPos pos =
    SyntaxTraversal.Traverse(
      pos,
      scope.ParseTree,
      { new SyntaxVisitorBase<_>() with
          member _.VisitExpr(_, _, defaultTraverse, expr) =
            match expr with
            | SynExpr.InterpolatedString (range = range) when Range.rangeContainsPos range pos -> Some range
            | _ -> defaultTraverse expr }
    )

module SyntaxTreeOps =
  open FSharp.Compiler.Syntax

  let rec synExprContainsError inpExpr =
    let rec walkBind (SynBinding (expr = expr)) = walkExpr expr

    and walkExprs es = es |> List.exists walkExpr

    and walkBinds es = es |> List.exists walkBind

    and walkMatchClauses cl =
      cl
      |> List.exists (fun (SynMatchClause (whenExpr = whenExpr; resultExpr = resultExpr)) ->
        walkExprOpt whenExpr || walkExpr resultExpr)

    and walkExprOpt eOpt = eOpt |> Option.exists walkExpr

    and walkExpr e =
      match e with
      | SynExpr.FromParseError _
      | SynExpr.DiscardAfterMissingQualificationAfterDot _
      | SynExpr.ArbitraryAfterError _ -> true

      | SynExpr.LongIdent _
      | SynExpr.Quote _
      | SynExpr.LibraryOnlyILAssembly _
      | SynExpr.LibraryOnlyStaticOptimization _
      | SynExpr.Null _
      | SynExpr.Ident _
      | SynExpr.ImplicitZero _
      | SynExpr.Const _ -> false

      | SynExpr.TypeTest (e, _, _)
      | SynExpr.Upcast (e, _, _)
      | SynExpr.AddressOf (_, e, _, _)
      | SynExpr.ComputationExpr (_, e, _)
      | SynExpr.ArrayOrListComputed (_, e, _)
      | SynExpr.Typed (e, _, _)
      | SynExpr.FromParseError (e, _)
      | SynExpr.Do (e, _)
      | SynExpr.Assert (e, _)
      | SynExpr.DotGet (e, _, _, _)
      | SynExpr.LongIdentSet (_, e, _)
      | SynExpr.New (_, _, e, _)
      | SynExpr.TypeApp (e, _, _, _, _, _, _)
      | SynExpr.LibraryOnlyUnionCaseFieldGet (e, _, _, _)
      | SynExpr.Downcast (e, _, _)
      | SynExpr.InferredUpcast (e, _)
      | SynExpr.InferredDowncast (e, _)
      | SynExpr.Lazy (e, _)
      | SynExpr.TraitCall (_, _, e, _)
      | SynExpr.YieldOrReturn (_, e, _)
      | SynExpr.YieldOrReturnFrom (_, e, _)
      | SynExpr.DoBang (e, _)
      | SynExpr.Fixed (e, _)
      | SynExpr.Paren (e, _, _, _) -> walkExpr e

      | SynExpr.NamedIndexedPropertySet (_, e1, e2, _)
      | SynExpr.DotSet (e1, _, e2, _)
      | SynExpr.Set (e1, e2, _)
      | SynExpr.LibraryOnlyUnionCaseFieldSet (e1, _, _, e2, _)
      | SynExpr.JoinIn (e1, _, e2, _)
      | SynExpr.App (_, _, e1, e2, _) -> walkExpr e1 || walkExpr e2

      | SynExpr.ArrayOrList (_, es, _)
      | SynExpr.Tuple (_, es, _, _) -> walkExprs es

      | SynExpr.AnonRecd (copyInfo = copyInfo; recordFields = recordFields) ->
        (match copyInfo with
         | Some (e, _) -> walkExpr e
         | None -> false)
        || walkExprs (
          recordFields
          |> List.map (fun (ident, range, expr) -> expr)
        )

      | SynExpr.Record (copyInfo = copyInfo; recordFields = recordFields) ->
        (match copyInfo with
         | Some (e, _) -> walkExpr e
         | None -> false)
        || let flds =
             recordFields
             |> List.choose (fun (SynExprRecordField (expr = expr)) -> expr) in
           walkExprs flds

      | SynExpr.ObjExpr (bindings = bindings; extraImpls = extraImpls) ->
        walkBinds bindings
        || walkBinds
             [ for (SynInterfaceImpl (bindings = bindings)) in extraImpls do
                 yield! bindings ]

      | SynExpr.ForEach (enumExpr = e1; bodyExpr = e2)
      | SynExpr.While (whileExpr = e1; doExpr = e2) -> walkExpr e1 || walkExpr e2

      | SynExpr.For (identBody = e1; toBody = e2; doBody = e3) -> walkExpr e1 || walkExpr e2 || walkExpr e3

      | SynExpr.MatchLambda (matchClauses = cl) -> walkMatchClauses cl

      | SynExpr.Lambda (body = body) -> walkExpr body

      | SynExpr.Match (expr = e; clauses = cl) -> walkExpr e || walkMatchClauses cl

      | SynExpr.LetOrUse (bindings = bs; body = e) -> walkBinds bs || walkExpr e

      | SynExpr.TryWith (tryExpr = e; withCases = cl) -> walkExpr e || walkMatchClauses cl

      | SynExpr.TryFinally (tryExpr = e1; finallyExpr = e2) -> walkExpr e1 || walkExpr e2

      | SynExpr.Sequential (_, _, e1, e2, _) -> walkExpr e1 || walkExpr e2

      | SynExpr.SequentialOrImplicitYield (_, e1, e2, _, _) -> walkExpr e1 || walkExpr e2

      | SynExpr.IfThenElse (ifExpr = e1; thenExpr = e2; elseExpr = e3opt) ->
        walkExpr e1 || walkExpr e2 || walkExprOpt e3opt

      | SynExpr.DotIndexedGet (e1, es, _, _) -> walkExpr e1 || walkExpr es

      | SynExpr.DotIndexedSet (e1, es, e2, _, _, _) -> walkExpr e1 || walkExpr es || walkExpr e2

      | SynExpr.DotNamedIndexedPropertySet (e1, _, e2, e3, _) -> walkExpr e1 || walkExpr e2 || walkExpr e3

      | SynExpr.MatchBang (expr = e; clauses = cl) -> walkExpr e || walkMatchClauses cl

      | SynExpr.LetOrUseBang (rhs = e1; body = e2; andBangs = es) ->
        walkExpr e1
        || walkExprs
             [ for (SynExprAndBang (body = e)) in es do
                 yield e ]
        || walkExpr e2

      | SynExpr.InterpolatedString (parts, _, _m) ->
        walkExprs (
          parts
          |> List.choose (function
            | SynInterpolatedStringPart.String _ -> None
            | SynInterpolatedStringPart.FillExpr (x, _) -> Some x)
        )
      | SynExpr.IndexRange (expr1, opm, expr2, range1, range2, range3) ->
        Option.map walkExpr expr1
        |> Option.orElseWith (fun _ -> Option.map walkExpr expr2)
        |> Option.defaultValue false
      | SynExpr.IndexFromEnd (expr, range) -> walkExpr expr
      | SynExpr.DebugPoint (innerExpr = expr) -> walkExpr expr

    walkExpr inpExpr
