/// this file contains patches to the F# Compiler Service that have not yet made it into
/// published nuget packages.  We source-copy them here to have a consistent location for our to-be-removed extensions

module FsAutoComplete.FCSPatches

open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.SyntaxTree
open FSharp.Compiler.Text
open FsAutoComplete.UntypedAstUtils

module internal SynExprAppLocationsImpl =
    let rec private searchSynArgExpr traverseSynExpr expr ranges =
        match expr with
        | SynExpr.Const(SynConst.Unit, _) ->
            None, None

        | SynExpr.Paren(SynExpr.Tuple (_, exprs, _commas, _tupRange), _, _, _parenRange) ->
            let rec loop (exprs: SynExpr list) ranges =
                match exprs with
                | [] -> ranges
                | h::t ->
                    loop t (h.Range :: ranges)

            let res = loop exprs ranges
            Some (res), None

        | SynExpr.Paren(SynExpr.Paren(_, _, _, _) as synExpr, _, _, _parenRange) ->
            let r, _cacheOpt = searchSynArgExpr traverseSynExpr synExpr ranges
            r, None

        | SynExpr.Paren(SynExpr.App (_, _isInfix, _, _, _range), _, _, parenRange) ->
            Some (parenRange :: ranges), None

        | e ->
            let inner = traverseSynExpr e
            match inner with
            | None ->
                Some (e.Range :: ranges), Some inner
            | _ -> None, Some inner

    let getAllCurriedArgsAtPosition pos parseTree =
        AstTraversal.Traverse(pos, parseTree, { new AstTraversal.AstVisitorBase<_>() with
            member _.VisitExpr(_path, traverseSynExpr, defaultTraverse, expr) =
                match expr with
                | SynExpr.App (_exprAtomicFlag, _isInfix, funcExpr, argExpr, range) when Pos.posEq pos range.Start ->
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
                | _ -> defaultTraverse expr })
        |> Option.map List.rev

type FSharpParseFileResults with

  member scope.IsPositionContainedInACurriedParameter pos =
    match scope.ParseTree with
    | Some input ->
        let result =
            AstTraversal.Traverse(pos, input, { new AstTraversal.AstVisitorBase<_>() with
                member __.VisitExpr(_path, traverseSynExpr, defaultTraverse, expr) =
                    defaultTraverse(expr)

                override __.VisitBinding (_, binding) =
                    match binding with
                    | SynBinding.Binding(_, _, _, _, _, _, valData, _, _, _, ((ContainsPos pos) as range), _) ->
                        let info = valData.SynValInfo.CurriedArgInfos
                        let mutable found = false
                        for group in info do
                            for arg in group do
                                match arg.Ident with
                                | Some (IdentContainsPos pos) ->
                                    found <- true
                                | _ -> ()
                        if found then Some range else None
                    | _ ->
                        None
            })
        result.IsSome
    | _ -> false

  member scope.TryRangeOfParenEnclosingOpEqualsGreaterUsage opGreaterEqualPos =
    /// reused pattern to find applications of => (a symptom of improper C# style lambdas)
    let (|InfixAppOfOpEqualsGreater|_|) =
      function | SynExpr.App(ExprAtomicFlag.NonAtomic, false, SynExpr.App(ExprAtomicFlag.NonAtomic, true, Ident "op_EqualsGreater", actualParamListExpr, _), actualLambdaBodyExpr, _) -> Some (actualParamListExpr, actualLambdaBodyExpr)
               | _ -> None

    match scope.ParseTree with
    | None -> None
    | Some input ->
      let visitor = {
        new AstTraversal.AstVisitorBase<_>() with
          member _.VisitExpr(_, _, defaultTraverse, expr) =
            match expr with
            | SynExpr.Paren((InfixAppOfOpEqualsGreater(lambdaArgs, lambdaBody) as app), _, _, _) ->
              Some (app.Range, lambdaArgs.Range, lambdaBody.Range)
            | _ -> defaultTraverse expr
          member _.VisitBinding(defaultTraverse, binding) =
            match binding with
            | SynBinding.Binding (_, SynBindingKind.NormalBinding, _, _, _, _, _, _, _, (InfixAppOfOpEqualsGreater(lambdaArgs, lambdaBody) as app), _, _) ->
              Some(app.Range, lambdaArgs.Range, lambdaBody.Range)
            | _ -> defaultTraverse binding
        }
      AstTraversal.Traverse(opGreaterEqualPos, input, visitor)

  member scope.TryRangeOfRefCellDereferenceContainingPos expressionPos =
    match scope.ParseTree with
    | Some input ->
        AstTraversal.Traverse(expressionPos, input, { new AstTraversal.AstVisitorBase<_>() with
            member _.VisitExpr(_, _, defaultTraverse, expr) =
                match expr with
                | SynExpr.App(_, false, SynExpr.Ident funcIdent, expr, _) ->
                    if funcIdent.idText = "op_Dereference" && Range.rangeContainsPos expr.Range expressionPos then
                        Some funcIdent.idRange
                    else
                        None
                | _ -> defaultTraverse expr })
    | None -> None

  member scope.TryRangeOfRecordExpressionContainingPos pos =
    match scope.ParseTree with
    | Some input ->
        AstTraversal.Traverse(pos, input, { new AstTraversal.AstVisitorBase<_>() with
            member _.VisitExpr(_, _, defaultTraverse, expr) =
                match expr with
                | SynExpr.Record(_, _, _, range) when Range.rangeContainsPos range pos ->
                    Some range
                | _ -> defaultTraverse expr })
    | None ->
        None

  member scope.TryRangeOfExprInYieldOrReturn pos =
      match scope.ParseTree with
      | Some parseTree ->
          AstTraversal.Traverse(pos, parseTree, { new AstTraversal.AstVisitorBase<_>() with
              member __.VisitExpr(_path, _, defaultTraverse, expr) =
                  match expr with
                  | SynExpr.YieldOrReturn(_, expr, range)
                  | SynExpr.YieldOrReturnFrom(_, expr, range) when Range.rangeContainsPos range pos ->
                      Some expr.Range
                  | _ -> defaultTraverse expr })
      | None -> None

  /// Attempts to find an Ident of a pipeline containing the given position, and the number of args already applied in that pipeline.
  /// For example, '[1..10] |> List.map ' would give back the ident of '|>' and 1, because it applied 1 arg (the list) to 'List.map'.
  member scope.TryIdentOfPipelineContainingPosAndNumArgsApplied pos =
      match scope.ParseTree with
      | Some input ->
          AstTraversal.Traverse(pos, input, { new AstTraversal.AstVisitorBase<_>() with
              member _.VisitExpr(_, _, defaultTraverse, expr) =
                  match expr with
                  | SynExpr.App (_, _, SynExpr.App(_, true, SynExpr.Ident ident, _, _), argExpr, _) when Range.rangeContainsPos argExpr.Range pos ->
                      if ident.idText = "op_PipeRight" then
                          Some (ident, 1)
                      elif ident.idText = "op_PipeRight2" then
                          Some (ident, 2)
                      elif ident.idText = "op_PipeRight3" then
                          Some (ident, 3)
                      else
                          None
                  | _ -> defaultTraverse expr
          })
      | None -> None

  /// Determines if the given position is inside a function or method application.
  member scope.IsPosContainedInApplicationPatched pos =
      match scope.ParseTree with
      | Some input ->
          let result =
              AstTraversal.Traverse(pos, input, { new AstTraversal.AstVisitorBase<_>() with
                  member _.VisitExpr(_, traverseSynExpr, defaultTraverse, expr) =
                    match expr with
                    | SynExpr.TypeApp (_, _, _, _, _, _, range) when Range.rangeContainsPos range pos ->
                        Some range
                    | SynExpr.App(_, _, _, SynExpr.CompExpr (_, _, expr, _), range) when Range.rangeContainsPos range pos ->
                        traverseSynExpr expr
                    | SynExpr.App (_, _, _, _, range) when Range.rangeContainsPos range pos ->
                        Some range
                    | _ -> defaultTraverse expr
              })
          result.IsSome
      | None -> false

  /// Attempts to find the range of a function or method that is being applied. Also accounts for functions in pipelines.
  member scope.TryRangeOfFunctionOrMethodBeingAppliedPatched pos =
      let rec getIdentRangeForFuncExprInApp traverseSynExpr expr pos: Range option =
          match expr with
          | SynExpr.Ident ident -> Some ident.idRange

          | SynExpr.LongIdent(_, _, _, range) -> Some range

          | SynExpr.Paren(expr, _, _, range) when Range.rangeContainsPos range pos ->
              getIdentRangeForFuncExprInApp traverseSynExpr expr pos

          | SynExpr.TypeApp (expr, _, _, _, _, _, _) ->
              getIdentRangeForFuncExprInApp traverseSynExpr expr pos

          | SynExpr.App (_, _, funcExpr, argExpr, _) ->
              match argExpr with
              | SynExpr.App (_, _, _, _, range) when Range.rangeContainsPos range pos ->
                  getIdentRangeForFuncExprInApp traverseSynExpr argExpr pos

              // Special case: `async { ... }` is actually a CompExpr inside of the argExpr of a SynExpr.App
              | SynExpr.CompExpr (_, _, expr, range) when Range.rangeContainsPos range pos ->
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

          | SynExpr.LetOrUse (_, _, bindings, body, range) when Range.rangeContainsPos range pos  ->
                let binding =
                    bindings
                    |> List.tryFind (fun x -> Range.rangeContainsPos x.RangeOfBindingAndRhs pos)
                match binding with
                | Some(SynBinding.Binding(_, _, _, _, _, _, _, _, _, expr, _, _)) ->
                    getIdentRangeForFuncExprInApp traverseSynExpr expr pos
                | None ->
                    getIdentRangeForFuncExprInApp traverseSynExpr body pos

          | SynExpr.IfThenElse (ifExpr, thenExpr, elseExpr, _, _, _, range) when Range.rangeContainsPos range pos ->
              if Range.rangeContainsPos ifExpr.Range pos then
                  getIdentRangeForFuncExprInApp traverseSynExpr ifExpr pos
              elif Range.rangeContainsPos thenExpr.Range pos then
                  getIdentRangeForFuncExprInApp traverseSynExpr thenExpr pos
              else
                  match elseExpr with
                  | None -> None
                  | Some expr ->
                      getIdentRangeForFuncExprInApp traverseSynExpr expr pos

          | SynExpr.Match (_, expr, clauses, range) when Range.rangeContainsPos range pos ->
              if Range.rangeContainsPos expr.Range pos then
                  getIdentRangeForFuncExprInApp traverseSynExpr expr pos
              else
                  let clause = clauses |> List.tryFind (fun clause -> Range.rangeContainsPos clause.Range pos)
                  match clause with
                  | None -> None
                  | Some clause ->
                      match clause with
                      | SynMatchClause.Clause (_, whenExpr, resultExpr, _, _) ->
                          match whenExpr with
                          | None ->
                              getIdentRangeForFuncExprInApp traverseSynExpr resultExpr pos
                          | Some whenExpr ->
                              if Range.rangeContainsPos whenExpr.Range pos then
                                  getIdentRangeForFuncExprInApp traverseSynExpr whenExpr pos
                              else
                                  getIdentRangeForFuncExprInApp traverseSynExpr resultExpr pos


          // Ex: C.M(x, y, ...) <--- We want to find where in the tupled application the call is being made
          | SynExpr.Tuple(_, exprs, _, tupRange) when Range.rangeContainsPos tupRange pos ->
              let expr = exprs |> List.tryFind (fun expr -> Range.rangeContainsPos expr.Range pos)
              match expr with
              | None -> None
              | Some expr ->
                  getIdentRangeForFuncExprInApp traverseSynExpr expr pos

          // Capture the body of a lambda, often nested in a call to a collection function
          | SynExpr.Lambda(_, _, _args, body, _, _) when Range.rangeContainsPos body.Range pos ->
              getIdentRangeForFuncExprInApp traverseSynExpr body pos

          | SynExpr.Do(expr, range) when Range.rangeContainsPos range pos ->
              getIdentRangeForFuncExprInApp traverseSynExpr expr pos

          | SynExpr.Assert(expr, range) when Range.rangeContainsPos range pos ->
              getIdentRangeForFuncExprInApp traverseSynExpr expr pos

          | SynExpr.ArbitraryAfterError (_debugStr, range) when Range.rangeContainsPos range pos ->
              Some range

          | expr ->
              traverseSynExpr expr
              |> Option.map (fun expr -> expr)

      match scope.ParseTree with
      | Some input ->
          AstTraversal.Traverse(pos, input, { new AstTraversal.AstVisitorBase<_>() with
              member _.VisitExpr(_, traverseSynExpr, defaultTraverse, expr) =
                  match expr with
                  | SynExpr.TypeApp (expr, _, _, _, _, _, range) when Range.rangeContainsPos range pos ->
                    getIdentRangeForFuncExprInApp traverseSynExpr expr pos
                  | SynExpr.App (_, _, _funcExpr, _, range) as app when Range.rangeContainsPos range pos ->
                      getIdentRangeForFuncExprInApp traverseSynExpr app pos
                  | _ -> defaultTraverse expr
          })
      | None -> None

  /// Gets the ranges of all arguments, if they can be found, for a function application at the given position.
  member scope.GetAllArgumentsForFunctionApplicationAtPostion pos =
      match scope.ParseTree with
      | Some input -> SynExprAppLocationsImpl.getAllCurriedArgsAtPosition pos input
      | None -> None


  member scope.TryRangeOfExpressionBeingDereferencedContainingPos expressionPos =
    scope.ParseTree
    |> Option.bind (fun input ->
        AstTraversal.Traverse(expressionPos, input, { new AstTraversal.AstVisitorBase<_>() with
            member _.VisitExpr(_, _, defaultTraverse, expr) =
                match expr with
                | SynExpr.App(_, false, SynExpr.Ident funcIdent, expr, _) ->
                    if funcIdent.idText = "op_Dereference" && Range.rangeContainsPos expr.Range expressionPos then
                        Some expr.Range
                    else
                        None
                | _ -> defaultTraverse expr })
    )
module SyntaxTreeOps =
  open FSharp.Compiler.SyntaxTree
  let rec synExprContainsError inpExpr =
    let rec walkBind (SynBinding.Binding(_, _, _, _, _, _, _, _, _, synExpr, _, _)) = walkExpr synExpr

    and walkExprs es = es |> List.exists walkExpr

    and walkBinds es = es |> List.exists walkBind

    and walkMatchClauses cl =
        cl |> List.exists (fun (SynMatchClause.Clause(_, whenExpr, e, _, _)) -> walkExprOpt whenExpr || walkExpr e)

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
          | SynExpr.CompExpr (_, _, e, _)
          | SynExpr.ArrayOrListOfSeqExpr (_, e, _)
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
          | SynExpr.Paren (e, _, _, _) ->
              walkExpr e

          | SynExpr.NamedIndexedPropertySet (_, e1, e2, _)
          | SynExpr.DotSet (e1, _, e2, _)
          | SynExpr.Set (e1, e2, _)
          | SynExpr.LibraryOnlyUnionCaseFieldSet (e1, _, _, e2, _)
          | SynExpr.JoinIn (e1, _, e2, _)
          | SynExpr.App (_, _, e1, e2, _) ->
              walkExpr e1 || walkExpr e2

          | SynExpr.ArrayOrList (_, es, _)
          | SynExpr.Tuple (_, es, _, _) ->
              walkExprs es

          | SynExpr.AnonRecd (_, origExpr, flds, _) ->
              (match origExpr with Some (e, _) -> walkExpr e | None -> false) ||
              walkExprs (List.map snd flds)

          | SynExpr.Record (_, origExpr, fs, _) ->
              (match origExpr with Some (e, _) -> walkExpr e | None -> false) ||
              let flds = fs |> List.choose (fun (_, v, _) -> v)
              walkExprs flds

          | SynExpr.ObjExpr (_, _, bs, is, _, _) ->
              walkBinds bs || walkBinds [ for (SynInterfaceImpl.InterfaceImpl(_, bs, _)) in is do yield! bs  ]

          | SynExpr.ForEach (_, _, _, _, e1, e2, _)
          | SynExpr.While (_, e1, e2, _) ->
              walkExpr e1 || walkExpr e2

          | SynExpr.For (_, _, e1, _, e2, e3, _) ->
              walkExpr e1 || walkExpr e2 || walkExpr e3

          | SynExpr.MatchLambda (_, _, cl, _, _) ->
              walkMatchClauses cl

          | SynExpr.Lambda (_, _, _, e, _, _) ->
              walkExpr e

          | SynExpr.Match (_, e, cl, _) ->
              walkExpr e || walkMatchClauses cl

          | SynExpr.LetOrUse (_, _, bs, e, _) ->
              walkBinds bs || walkExpr e

          | SynExpr.TryWith (e, _, cl, _, _, _, _) ->
              walkExpr e  || walkMatchClauses cl

          | SynExpr.TryFinally (e1, e2, _, _, _) ->
              walkExpr e1 || walkExpr e2

          | SynExpr.Sequential (_, _, e1, e2, _) ->
              walkExpr e1 || walkExpr e2

          | SynExpr.SequentialOrImplicitYield (_, e1, e2, _, _) ->
              walkExpr e1 || walkExpr e2

          | SynExpr.IfThenElse (e1, e2, e3opt, _, _, _, _) ->
              walkExpr e1 || walkExpr e2 || walkExprOpt e3opt

          | SynExpr.DotIndexedGet (e1, es, _, _) ->
              walkExpr e1 || walkExprs [ for e in es do yield! e.Exprs ]

          | SynExpr.DotIndexedSet (e1, es, e2, _, _, _) ->
              walkExpr e1 || walkExprs [ for e in es do yield! e.Exprs ] || walkExpr e2

          | SynExpr.DotNamedIndexedPropertySet (e1, _, e2, e3, _) ->
              walkExpr e1 || walkExpr e2 || walkExpr e3

          | SynExpr.MatchBang (_, e, cl, _) ->
              walkExpr e || walkMatchClauses cl

          | SynExpr.LetOrUseBang  (rhs = e1; body = e2; andBangs = es) ->
              walkExpr e1 || walkExprs [ for (_,_,_,_,e,_) in es do yield e ] || walkExpr e2

          | SynExpr.InterpolatedString (parts, _m) ->
              walkExprs
                  (parts |> List.choose (function
                      | SynInterpolatedStringPart.String _ -> None
                      | SynInterpolatedStringPart.FillExpr (x, _) -> Some x))

    walkExpr inpExpr
