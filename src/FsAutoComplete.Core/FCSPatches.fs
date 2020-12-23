/// this file contains patches to the F# Compiler Service that have not yet made it into
/// published nuget packages.  We source-copy them here to have a consistent location for our to-be-removed extensions

module FsAutoComplete.FCSPatches

open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.SyntaxTree
open FSharp.Compiler.Range
open FsAutoComplete.UntypedAstUtils

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
        let r, _cacheOpt =
          searchSynArgExpr traverseSynExpr synExpr ranges

        r, None

    | SynExpr.Paren (SynExpr.App (_, _isInfix, _, _, _range), _, _, parenRange) -> Some(parenRange :: ranges), None

    | e ->
        let inner = traverseSynExpr e

        match inner with
        | None -> Some(e.Range :: ranges), Some inner
        | _ -> None, Some inner

  let getAllCurriedArgsAtPosition pos parseTree =
    AstTraversal.Traverse(
      pos,
      parseTree,
      { new AstTraversal.AstVisitorBase<_>() with
          member _.VisitExpr(_path, traverseSynExpr, defaultTraverse, expr) =
            match expr with
            | SynExpr.App (_exprAtomicFlag, _isInfix, funcExpr, argExpr, range) when posEq pos range.Start ->
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

                  let xResult, cacheOpt =
                    searchSynArgExpr traverseSynExpr argExpr workingRanges

                  match xResult, cacheOpt with
                  | Some ranges, _ -> Some ranges
                  | None, Some cache -> cache
                  | _ -> traverseSynExpr argExpr
            | _ -> defaultTraverse expr }
    )
    |> Option.map List.rev

type FSharpParseFileResults with

  member scope.IsPositionContainedInACurriedParameter pos =
    match scope.ParseTree with
    | Some input ->
        let result =
          AstTraversal.Traverse(
            pos,
            input,
            { new AstTraversal.AstVisitorBase<_>() with
                member __.VisitExpr(_path, traverseSynExpr, defaultTraverse, expr) = defaultTraverse (expr)

                member __.VisitBinding(_, binding) =
                  match binding with
                  | SynBinding.Binding (_, _, _, _, _, _, valData, _, _, _, ((ContainsPos pos) as range), _) ->
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
    | _ -> false

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

    match scope.ParseTree with
    | None -> None
    | Some input ->
        let visitor =
          { new AstTraversal.AstVisitorBase<_>() with
              member _.VisitExpr(_, _, defaultTraverse, expr) =
                match expr with
                | SynExpr.Paren ((InfixAppOfOpEqualsGreater (lambdaArgs, lambdaBody) as app), _, _, _) ->
                    Some(app.Range, lambdaArgs.Range, lambdaBody.Range)
                | _ -> defaultTraverse expr

              member _.VisitBinding(defaultTraverse, binding) =
                match binding with
                | SynBinding.Binding (_,
                                      SynBindingKind.NormalBinding,
                                      _,
                                      _,
                                      _,
                                      _,
                                      _,
                                      _,
                                      _,
                                      (InfixAppOfOpEqualsGreater (lambdaArgs, lambdaBody) as app),
                                      _,
                                      _) -> Some(app.Range, lambdaArgs.Range, lambdaBody.Range)
                | _ -> defaultTraverse binding }

        AstTraversal.Traverse(opGreaterEqualPos, input, visitor)

  member scope.TryRangeOfRefCellDereferenceContainingPos expressionPos =
    match scope.ParseTree with
    | Some input ->
        AstTraversal.Traverse(
          expressionPos,
          input,
          { new AstTraversal.AstVisitorBase<_>() with
              member _.VisitExpr(_, _, defaultTraverse, expr) =
                match expr with
                | SynExpr.App (_, false, SynExpr.Ident funcIdent, expr, _) ->
                    if funcIdent.idText = "op_Dereference"
                       && rangeContainsPos expr.Range expressionPos then
                      Some funcIdent.idRange
                    else
                      None
                | _ -> defaultTraverse expr }
        )
    | None -> None

  member scope.TryRangeOfRecordExpressionContainingPos pos =
    match scope.ParseTree with
    | Some input ->
        AstTraversal.Traverse(
          pos,
          input,
          { new AstTraversal.AstVisitorBase<_>() with
              member _.VisitExpr(_, _, defaultTraverse, expr) =
                match expr with
                | SynExpr.Record (_, _, _, range) when rangeContainsPos range pos -> Some range
                | _ -> defaultTraverse expr }
        )
    | None -> None

  member scope.TryRangeOfExprInYieldOrReturn pos =
    match scope.ParseTree with
    | Some parseTree ->
        AstTraversal.Traverse(
          pos,
          parseTree,
          { new AstTraversal.AstVisitorBase<_>() with
              member __.VisitExpr(_path, _, defaultTraverse, expr) =
                match expr with
                | SynExpr.YieldOrReturn (_, expr, range)
                | SynExpr.YieldOrReturnFrom (_, expr, range) when rangeContainsPos range pos -> Some expr.Range
                | _ -> defaultTraverse expr }
        )
    | None -> None

  /// Attempts to find an Ident of a pipeline containing the given position, and the number of args already applied in that pipeline.
  /// For example, '[1..10] |> List.map ' would give back the ident of '|>' and 1, because it applied 1 arg (the list) to 'List.map'.
  member scope.TryIdentOfPipelineContainingPosAndNumArgsApplied pos =
    match scope.ParseTree with
    | Some input ->
        AstTraversal.Traverse(
          pos,
          input,
          { new AstTraversal.AstVisitorBase<_>() with
              member _.VisitExpr(_, _, defaultTraverse, expr) =
                match expr with
                | SynExpr.App (_, _, SynExpr.App (_, true, SynExpr.Ident ident, _, _), argExpr, _) when
                  rangeContainsPos argExpr.Range pos ->
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
    | None -> None

  /// Determines if the given position is inside a function or method application.
  member scope.IsPosContainedInApplication pos =
    match scope.ParseTree with
    | Some input ->
        let result =
          AstTraversal.Traverse(
            pos,
            input,
            { new AstTraversal.AstVisitorBase<_>() with
                member _.VisitExpr(_, _, defaultTraverse, expr) =
                  match expr with
                  | SynExpr.App (_, _, _, _, range) when rangeContainsPos range pos -> Some range
                  | _ -> defaultTraverse expr }
          )

        result.IsSome
    | None -> false

  /// Attempts to find the range of a function or method that is being applied. Also accounts for functions in pipelines.
  member scope.TryRangeOfFunctionOrMethodBeingApplied pos =
    let rec getIdentRangeForFuncExprInApp expr pos =
      match expr with
      | SynExpr.Ident ident -> ident.idRange

      | SynExpr.LongIdent (_, _, _, range) -> range

      | SynExpr.Paren (expr, _, _, range) when rangeContainsPos range pos -> getIdentRangeForFuncExprInApp expr pos

      | SynExpr.App (_, _, funcExpr, argExpr, _) ->
          match argExpr with
          | SynExpr.App (_, _, _, _, range) when rangeContainsPos range pos -> getIdentRangeForFuncExprInApp argExpr pos
          | _ ->
              match funcExpr with
              | SynExpr.App (_, true, _, _, _) when rangeContainsPos argExpr.Range pos ->
                  // x |> List.map
                  // Don't dive into the funcExpr (the operator expr)
                  // because we dont want to offer sig help for that!
                  getIdentRangeForFuncExprInApp argExpr pos
              | _ ->
                  // Generally, we want to dive into the func expr to get the range
                  // of the identifier of the function we're after
                  getIdentRangeForFuncExprInApp funcExpr pos
      | expr -> expr.Range // Exhaustiveness, this shouldn't actually be necessary...right?

    match scope.ParseTree with
    | Some input ->
        AstTraversal.Traverse(
          pos,
          input,
          { new AstTraversal.AstVisitorBase<_>() with
              member _.VisitExpr(_, _, defaultTraverse, expr) =
                match expr with
                | SynExpr.App (_, _, _funcExpr, _, range) as app when rangeContainsPos range pos ->
                    getIdentRangeForFuncExprInApp app pos |> Some
                | _ -> defaultTraverse expr }
        )
    | None -> None

  /// Gets the ranges of all arguments, if they can be found, for a function application at the given position.
  member scope.GetAllArgumentsForFunctionApplicationAtPostion pos =
    match scope.ParseTree with
    | Some input -> SynExprAppLocationsImpl.getAllCurriedArgsAtPosition pos input
    | None -> None

  member scope.TryRangeOfNearestOuterBindingContainingPos pos =
    let tryGetIdentRangeFromBinding binding =
      match binding with
      | SynBinding.Binding (_, _, _, _, _, _, _, headPat, _, _, _, _) ->
          match headPat with
          | SynPat.LongIdent (longIdentWithDots, _, _, _, _, _) -> Some longIdentWithDots.Range
          | SynPat.Named (_, ident, false, _, _) -> Some ident.idRange
          | _ -> None

    let rec walkBinding expr workingRange =
      match expr with
      | SynExpr.Sequential (_, _, expr1, expr2, _) ->
          if rangeContainsPos expr1.Range pos then
            walkBinding expr1 workingRange
          else
            walkBinding expr2 workingRange
      | SynExpr.LetOrUse (_, _, bindings, bodyExpr, _) ->
          let potentialNestedRange =
            bindings
            |> List.tryFind (fun binding -> rangeContainsPos binding.RangeOfBindingAndRhs pos)
            |> Option.bind tryGetIdentRangeFromBinding

          match potentialNestedRange with
          | Some range -> walkBinding bodyExpr range
          | None -> walkBinding bodyExpr workingRange
      | _ -> Some workingRange

    match scope.ParseTree with
    | Some input ->
        AstTraversal.Traverse(
          pos,
          input,
          { new AstTraversal.AstVisitorBase<_>() with
              member _.VisitExpr(_, _, defaultTraverse, expr) = defaultTraverse expr

              member _.VisitBinding(defaultTraverse, binding) =
                match binding with
                | SynBinding.Binding (_, _, _, _, _, _, _, _, _, expr, _range, _) as b when
                  rangeContainsPos b.RangeOfBindingAndRhs pos ->
                    tryGetIdentRangeFromBinding b
                    |> Option.bind (walkBinding expr)
                | _ -> defaultTraverse binding }
        )
    | None -> None
