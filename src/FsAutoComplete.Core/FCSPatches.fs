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
              if
                funcIdent.idText = "op_Dereference"
                && Range.rangeContainsPos expr.Range expressionPos
              then
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
              if ident.idText = "op_PipeRight" then Some(ident, 1)
              elif ident.idText = "op_PipeRight2" then Some(ident, 2)
              elif ident.idText = "op_PipeRight3" then Some(ident, 3)
              else None
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
            clauses |> List.tryFind (fun clause -> Range.rangeContainsPos clause.Range pos)

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
        let expr = exprs |> List.tryFind (fun expr -> Range.rangeContainsPos expr.Range pos)

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

      | expr -> traverseSynExpr expr |> Option.map (fun expr -> expr)


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
              if
                funcIdent.idText = "op_Dereference"
                && Range.rangeContainsPos expr.Range expressionPos
              then
                Some expr.Range
              else
                None
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
        || walkExprs (recordFields |> List.map (fun (ident, range, expr) -> expr))

      | SynExpr.Record (copyInfo = copyInfo; recordFields = recordFields) ->
        (match copyInfo with
         | Some (e, _) -> walkExpr e
         | None -> false)
        || let flds =
             recordFields |> List.choose (fun (SynExprRecordField (expr = expr)) -> expr) in
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

module SyntaxTraversal =
  open FSharp.Compiler.Syntax

  let rec stripParenTypes synType =
    match synType with
    | SynType.Paren (innerType, _) -> stripParenTypes innerType
    | _ -> synType

  let (|StripParenTypes|) synType = stripParenTypes synType

  /// used to track route during traversal AST
  [<RequireQualifiedAccess>]
  type SyntaxNode =
    | SynPat of SynPat
    | SynType of SynType
    | SynExpr of SynExpr
    | SynModule of SynModuleDecl
    | SynModuleOrNamespace of SynModuleOrNamespace
    | SynTypeDefn of SynTypeDefn
    | SynMemberDefn of SynMemberDefn
    | SynMatchClause of SynMatchClause
    | SynBinding of SynBinding

  type SyntaxVisitorPath = SyntaxNode list

  [<AbstractClass>]
  type SyntaxWalkerBase<'T>() =
    abstract VisitExpr:
      path: SyntaxVisitorPath *
      traverseSynExpr: (SynExpr -> 'T[]) *
      defaultTraverse: (SynExpr -> 'T[]) *
      synExpr: SynExpr ->
        'T[]

    default _.VisitExpr
      (
        path: SyntaxVisitorPath,
        traverseSynExpr: SynExpr -> 'T[],
        defaultTraverse: SynExpr -> 'T[],
        synExpr: SynExpr
      ) =
      ignore (path, traverseSynExpr, defaultTraverse, synExpr)
      Array.empty

    /// VisitTypeAbbrev(ty,m), defaults to ignoring this leaf of the AST
    abstract VisitTypeAbbrev: path: SyntaxVisitorPath * synType: SynType * range: range -> 'T[]

    default _.VisitTypeAbbrev(path, synType, range) =
      ignore (path, synType, range)
      Array.empty

    /// VisitImplicitInherit(defaultTraverse,ty,expr,m), defaults to just visiting expr
    abstract VisitImplicitInherit:
      path: SyntaxVisitorPath *
      defaultTraverse: (SynExpr -> 'T[]) *
      inheritedType: SynType *
      synArgs: SynExpr *
      range: range ->
        'T[]

    default _.VisitImplicitInherit(path, defaultTraverse, inheritedType, synArgs, range) =
      ignore (path, inheritedType, range)
      defaultTraverse synArgs

    /// VisitModuleDecl allows overriding module declaration behavior
    abstract VisitModuleDecl:
      path: SyntaxVisitorPath * defaultTraverse: (SynModuleDecl -> 'T[]) * synModuleDecl: SynModuleDecl -> 'T[]

    default _.VisitModuleDecl(path, defaultTraverse, synModuleDecl) =
      ignore path
      defaultTraverse synModuleDecl

    /// VisitBinding allows overriding binding behavior (note: by default it would defaultTraverse expression)
    abstract VisitBinding:
      path: SyntaxVisitorPath * defaultTraverse: (SynBinding -> 'T[]) * synBinding: SynBinding -> 'T[]

    default _.VisitBinding(path, defaultTraverse, synBinding) =
      ignore path
      defaultTraverse synBinding

    /// VisitMatchClause allows overriding clause behavior (note: by default it would defaultTraverse expression)
    abstract VisitMatchClause:
      path: SyntaxVisitorPath * defaultTraverse: (SynMatchClause -> 'T[]) * matchClause: SynMatchClause -> 'T[]

    default _.VisitMatchClause(path, defaultTraverse, matchClause) =
      ignore path
      defaultTraverse matchClause

    /// VisitInheritSynMemberDefn allows overriding inherit behavior (by default do nothing)
    abstract VisitInheritSynMemberDefn:
      path: SyntaxVisitorPath *
      componentInfo: SynComponentInfo *
      typeDefnKind: SynTypeDefnKind *
      SynType *
      SynMemberDefns *
      range ->
        'T[]

    default _.VisitInheritSynMemberDefn(path, componentInfo, typeDefnKind, synType, members, range) =
      ignore (path, componentInfo, typeDefnKind, synType, members, range)
      Array.empty

    /// VisitInterfaceSynMemberDefnType allows overriding behavior for visiting interface member in types (by default - do nothing)
    abstract VisitInterfaceSynMemberDefnType: path: SyntaxVisitorPath * synType: SynType -> 'T[]

    default _.VisitInterfaceSynMemberDefnType(path, synType) =
      ignore (path, synType)
      Array.empty

    /// VisitRecordField allows overriding behavior when visiting l.h.s. of constructed record instances
    abstract VisitRecordField:
      path: SyntaxVisitorPath * copyOpt: SynExpr option * recordField: LongIdentWithDots option -> 'T[]

    default _.VisitRecordField(path, copyOpt, recordField) =
      ignore (path, copyOpt, recordField)
      Array.empty

    /// VisitHashDirective allows overriding behavior when visiting hash directives in FSX scripts, like #r, #load and #I.
    abstract VisitHashDirective: path: SyntaxVisitorPath * hashDirective: ParsedHashDirective * range: range -> 'T[]

    default _.VisitHashDirective(path, hashDirective, range) =
      ignore (path, hashDirective, range)
      Array.empty

    /// VisitModuleOrNamespace allows overriding behavior when visiting module or namespaces
    abstract VisitModuleOrNamespace: path: SyntaxVisitorPath * synModuleOrNamespace: SynModuleOrNamespace -> 'T[]

    default _.VisitModuleOrNamespace(path, synModuleOrNamespace) =
      ignore (path, synModuleOrNamespace)
      Array.empty

    /// VisitComponentInfo allows overriding behavior when visiting type component infos
    abstract VisitComponentInfo: path: SyntaxVisitorPath * synComponentInfo: SynComponentInfo -> 'T[]

    default _.VisitComponentInfo(path, synComponentInfo) =
      ignore (path, synComponentInfo)
      Array.empty

    /// VisitLetOrUse allows overriding behavior when visiting module or local let or use bindings
    abstract VisitLetOrUse:
      path: SyntaxVisitorPath *
      isRecursive: bool *
      defaultTraverse: (SynBinding -> 'T[]) *
      bindings: SynBinding list *
      range: range ->
        'T[]

    default _.VisitLetOrUse(path, isRecursive, defaultTraverse, bindings, range) =
      ignore (path, isRecursive, defaultTraverse, bindings, range)
      Array.empty

    /// VisitType allows overriding behavior when visiting simple pats
    abstract VisitSimplePats: path: SyntaxVisitorPath * synPats: SynSimplePat list -> 'T[]

    default _.VisitSimplePats(path, synPats) =
      ignore (path, synPats)
      Array.empty

    /// VisitPat allows overriding behavior when visiting patterns
    abstract VisitPat: path: SyntaxVisitorPath * defaultTraverse: (SynPat -> 'T[]) * synPat: SynPat -> 'T[]

    default _.VisitPat(path, defaultTraverse, synPat) =
      ignore path
      defaultTraverse synPat

    /// VisitType allows overriding behavior when visiting type hints (x: ..., etc.)
    abstract VisitType: path: SyntaxVisitorPath * defaultTraverse: (SynType -> 'T[]) * synType: SynType -> 'T[]

    default _.VisitType(path, defaultTraverse, synType) =
      ignore path
      defaultTraverse synType


  let Walk (parseTree, walker: SyntaxWalkerBase<'T>) =
    let rec traverseSynModuleDecl origPath (decl: SynModuleDecl) =
      let defaultTraverse m =
        let path = SyntaxNode.SynModule m :: origPath

        match m with
        | SynModuleDecl.ModuleAbbrev (_ident, _longIdent, _range) -> Array.empty
        | SynModuleDecl.NestedModule (decls = synModuleDecls) ->
          synModuleDecls |> List.toArray |> Array.collect (traverseSynModuleDecl path)
        | SynModuleDecl.Let (isRecursive, synBindingList, range) ->
          match walker.VisitLetOrUse(path, isRecursive, traverseSynBinding path, synBindingList, range) with
          | [||] -> synBindingList |> List.toArray |> Array.collect (traverseSynBinding path)
          | nodes -> nodes
        | SynModuleDecl.Expr (synExpr, _range) -> traverseSynExpr path synExpr
        | SynModuleDecl.Types (synTypeDefnList, _range) ->
          synTypeDefnList |> List.toArray |> Array.collect (traverseSynTypeDefn path)
        | SynModuleDecl.Exception (_synExceptionDefn, _range) -> Array.empty
        | SynModuleDecl.Open (_target, _range) -> Array.empty
        | SynModuleDecl.Attributes (_synAttributes, _range) -> Array.empty
        | SynModuleDecl.HashDirective (parsedHashDirective, range) ->
          walker.VisitHashDirective(path, parsedHashDirective, range)
        | SynModuleDecl.NamespaceFragment (synModuleOrNamespace) ->
          traverseSynModuleOrNamespace path synModuleOrNamespace

      walker.VisitModuleDecl(origPath, defaultTraverse, decl)

    and traverseSynModuleOrNamespace
      origPath
      (SynModuleOrNamespace (_longIdent,
                             _isRec,
                             _isModule,
                             synModuleDecls,
                             _preXmlDoc,
                             _synAttributes,
                             _synAccessOpt,
                             _range) as mors)
      =
      match walker.VisitModuleOrNamespace(origPath, mors) with
      | [||] ->
        let path = SyntaxNode.SynModuleOrNamespace mors :: origPath

        synModuleDecls |> List.toArray |> Array.collect (traverseSynModuleDecl path)
      | nodes -> nodes

    and traverseSynExpr origPath (expr: SynExpr) =
      let defaultTraverse e =
        let path = SyntaxNode.SynExpr e :: origPath
        let traverseSynExpr = traverseSynExpr path
        let traverseSynType = traverseSynType path
        let traversePat = traversePat path

        match e with

        | SynExpr.Paren (synExpr, _, _, _parenRange) -> traverseSynExpr synExpr

        | SynExpr.Quote (_, _, quotedExpression, _, _range) -> traverseSynExpr quotedExpression

        | SynExpr.Const (_synConst, _range) -> [||]

        | SynExpr.InterpolatedString (parts, _, _) ->
          [| for part in parts do
               match part with
               | SynInterpolatedStringPart.String _ -> ()
               | SynInterpolatedStringPart.FillExpr (fillExpr, _) -> yield! traverseSynExpr fillExpr |]

        | SynExpr.Typed (synExpr, synType, _range) ->
          [| yield! traverseSynExpr synExpr; yield! traverseSynType synType |]

        | SynExpr.Tuple (_, synExprList, _, _range)
        | SynExpr.ArrayOrList (_, synExprList, _range) -> synExprList |> List.toArray |> Array.collect traverseSynExpr

        | SynExpr.AnonRecd (_isStruct, copyOpt, synExprList, _range) ->
          [| match copyOpt with
             | Some (expr, _) ->
               yield! traverseSynExpr expr
               yield! walker.VisitRecordField(path, Some expr, None)
             | _ -> ()
             for _, _, x in synExprList do
               yield! traverseSynExpr x |]

        | SynExpr.Record (inheritOpt, copyOpt, fields, _range) ->
          [| match inheritOpt with
             | Some (_ty, expr, _range, _sepOpt, _inheritRange) -> yield! traverseSynExpr expr
             | _ -> ()
             match copyOpt with
             | Some (expr, _) ->
               yield! traverseSynExpr expr
               yield! walker.VisitRecordField(path, Some expr, None)
             | _ -> ()
             let copyOpt = Option.map fst copyOpt

             for SynExprRecordField.SynExprRecordField (fieldName = (field, _); expr = e) in fields do
               yield! walker.VisitRecordField(path, copyOpt, Some field)

               match e with
               | Some e -> yield! traverseSynExpr e
               | None -> () |]

        | SynExpr.New (_, _synType, synExpr, _range) -> traverseSynExpr synExpr
        | SynExpr.ObjExpr (ty, baseCallOpt, _, binds, members, ifaces, _range1, _range2) ->
          [| yield!
               ifaces
               |> Seq.collect (fun (SynInterfaceImpl (interfaceTy = ty)) ->
                 walker.VisitInterfaceSynMemberDefnType(path, ty))
             match baseCallOpt with
             | Some (expr, _) ->
               // this is like a call to 'new', so mock up a 'new' so we can recurse and use that existing logic
               let newCall =
                 SynExpr.New(false, ty, expr, FSharp.Compiler.Text.Range.unionRanges ty.Range expr.Range)

               yield! traverseSynExpr newCall
             | _ -> ()
             for b in binds do
               yield! traverseSynBinding path b
             for m in members do
               yield! traverseSynMemberDefn path (fun _ -> [||]) m
             for SynInterfaceImpl (bindings = binds) in ifaces do
               for b in binds do
                 yield! traverseSynBinding path b |]

        | SynExpr.While (_sequencePointInfoForWhileLoop, whileExpr, doExpr, _range) ->
          [| yield! traverseSynExpr whileExpr; yield! traverseSynExpr doExpr |]

        | SynExpr.For (identBody = identBody; toBody = toBody; doBody = doBody) ->
          [| yield! traverseSynExpr identBody
             yield! traverseSynExpr toBody
             yield! traverseSynExpr doBody |]

        | SynExpr.ForEach (pat = pat; enumExpr = enumExpr; bodyExpr = bodyExpr) ->
          [| yield! traversePat pat
             yield! traverseSynExpr enumExpr
             yield! traverseSynExpr bodyExpr |]

        | SynExpr.ArrayOrListComputed (_, synExpr, _range) -> traverseSynExpr synExpr

        | SynExpr.ComputationExpr (_, synExpr, _range) ->
          // now parser treats this syntactic expression as computation expression
          // { identifier }
          // here we detect this situation and treat ComputationExpr  { Identifier } as attempt to create record
          // note: sequence expressions use SynExpr.ComputationExpr too - they need to be filtered out
          let isPartOfArrayOrList =
            match origPath with
            | SyntaxNode.SynExpr (SynExpr.ArrayOrListComputed _) :: _ -> true
            | _ -> false

          [| yield!
               match isPartOfArrayOrList, synExpr with
               | false, SynExpr.Ident ident ->
                 walker.VisitRecordField(path, None, Some(LongIdentWithDots([ ident ], [])))
               | false, SynExpr.LongIdent (false, lidwd, _, _) -> walker.VisitRecordField(path, None, Some lidwd)
               | _ -> [||]
             yield! traverseSynExpr synExpr |]

        | SynExpr.Lambda (args = args; body = body) ->
          [| match args with
             | SynSimplePats.SimplePats (pats, _) -> yield! walker.VisitSimplePats(path, pats)
             | _ -> ()
             yield! traverseSynExpr body |]

        | SynExpr.MatchLambda (_isExnMatch, _argm, synMatchClauseList, _spBind, _wholem) ->
          synMatchClauseList
          |> List.toArray
          |> Array.collect (traverseSynMatchClause path)

        | SynExpr.Match (expr = expr; clauses = clauses) ->
          [| yield! traverseSynExpr expr
             yield! clauses |> List.toArray |> Array.collect (traverseSynMatchClause path) |]

        | SynExpr.Do (synExpr, _range) -> traverseSynExpr synExpr

        | SynExpr.Assert (synExpr, _range) -> traverseSynExpr synExpr

        | SynExpr.Fixed (synExpr, _range) -> traverseSynExpr synExpr

        | SynExpr.App (_exprAtomicFlag, isInfix, synExpr, synExpr2, _range) ->
          if isInfix then
            [| yield! traverseSynExpr synExpr2; yield! traverseSynExpr synExpr |] // reverse the args
          else
            [| yield! traverseSynExpr synExpr; yield! traverseSynExpr synExpr2 |]

        | SynExpr.TypeApp (synExpr, _, _synTypeList, _commas, _, _, _range) -> traverseSynExpr synExpr

        | SynExpr.LetOrUse (isRecursive, _, synBindingList, synExpr, range, _) ->
          match walker.VisitLetOrUse(path, isRecursive, traverseSynBinding path, synBindingList, range) with
          | [||] ->
            [| yield! synBindingList |> List.toArray |> Array.collect (traverseSynBinding path)
               yield! traverseSynExpr synExpr |]
          | nodes -> nodes

        | SynExpr.TryWith (tryExpr = tryExpr; withCases = withCases) ->
          [| yield! traverseSynExpr tryExpr
             yield! withCases |> List.toArray |> Array.collect (traverseSynMatchClause path) |]

        | SynExpr.TryFinally (tryExpr = tryExpr; finallyExpr = finallyExpr) ->
          [| yield! traverseSynExpr tryExpr; yield! traverseSynExpr finallyExpr |]

        | SynExpr.Lazy (synExpr, _range) -> traverseSynExpr synExpr

        | SynExpr.SequentialOrImplicitYield (_sequencePointInfoForSequential, synExpr, synExpr2, _, _range)

        | SynExpr.Sequential (_sequencePointInfoForSequential, _, synExpr, synExpr2, _range) ->
          [| yield! traverseSynExpr synExpr; yield! traverseSynExpr synExpr2 |]

        | SynExpr.IfThenElse (ifExpr, thenExpr, elseExpr, _sequencePointInfoForBinding, _isRecovery, _range, _range2) ->
          [| yield! traverseSynExpr ifExpr
             yield! traverseSynExpr thenExpr
             match elseExpr with
             | None -> ()
             | Some elseExpr -> yield! traverseSynExpr elseExpr |]

        | SynExpr.Ident _ident -> [||]

        | SynExpr.LongIdent (_, _longIdent, _altNameRefCell, _range) -> [||]

        | SynExpr.LongIdentSet (_longIdent, synExpr, _range) -> traverseSynExpr synExpr

        | SynExpr.DotGet (synExpr, _dotm, _longIdent, _range) -> traverseSynExpr synExpr

        | SynExpr.Set (targetExpr, assignmentExpr, _)

        | SynExpr.DotSet (targetExpr, _, assignmentExpr, _) ->
          [| yield! traverseSynExpr targetExpr; yield! traverseSynExpr assignmentExpr |]

        | SynExpr.IndexRange (leftExpr, _, rightExpr, _, _, _) ->
          [| match leftExpr with
             | Some e -> yield! traverseSynExpr e
             | None -> ()
             match rightExpr with
             | Some e -> yield! traverseSynExpr e
             | None -> () |]

        | SynExpr.IndexFromEnd (e, _) -> traverseSynExpr e

        | SynExpr.DotIndexedGet (objExpr, indexArgs, _range, _range2) ->
          [| yield! traverseSynExpr objExpr; yield! traverseSynExpr indexArgs |]

        | SynExpr.DotIndexedSet (objExpr, indexArgs, valueExpr, _, _range, _range2) ->
          [| yield! traverseSynExpr objExpr
             yield! traverseSynExpr indexArgs
             yield! traverseSynExpr valueExpr |]

        | SynExpr.JoinIn (synExpr1, _range, synExpr2, _range2) ->
          [| yield! traverseSynExpr synExpr1; yield! traverseSynExpr synExpr2 |]

        | SynExpr.NamedIndexedPropertySet (_longIdent, synExpr, synExpr2, _range) ->
          [| yield! traverseSynExpr synExpr; yield! traverseSynExpr synExpr2 |]

        | SynExpr.DotNamedIndexedPropertySet (synExpr, _longIdent, synExpr2, synExpr3, _range) ->
          [| yield! traverseSynExpr synExpr
             yield! traverseSynExpr synExpr2
             yield! traverseSynExpr synExpr3 |]

        | SynExpr.TypeTest (synExpr, synType, _range)

        | SynExpr.Upcast (synExpr, synType, _range)

        | SynExpr.Downcast (synExpr, synType, _range) ->
          [| yield! traverseSynExpr synExpr; yield! traverseSynType synType |]

        | SynExpr.InferredUpcast (synExpr, _range) -> traverseSynExpr synExpr

        | SynExpr.InferredDowncast (synExpr, _range) -> traverseSynExpr synExpr

        | SynExpr.Null _range -> [||]

        | SynExpr.AddressOf (_, synExpr, _range, _range2) -> traverseSynExpr synExpr

        | SynExpr.TraitCall (_synTyparList, _synMemberSig, synExpr, _range) -> traverseSynExpr synExpr

        | SynExpr.ImplicitZero _range -> [||]

        | SynExpr.YieldOrReturn (_, synExpr, _range) -> traverseSynExpr synExpr

        | SynExpr.YieldOrReturnFrom (_, synExpr, _range) -> traverseSynExpr synExpr

        | SynExpr.LetOrUseBang (pat = pat; rhs = rhs; andBangs = andBangs; body = body) ->
          [| yield! traversePat pat
             yield! traverseSynExpr rhs
             yield!
               [ for SynExprAndBang (pat = pat; body = body) in andBangs do
                   yield! traversePat pat
                   yield! traverseSynExpr body ]
             yield! traverseSynExpr body |]

        | SynExpr.MatchBang (expr = expr; clauses = clauses) ->
          [| yield! traverseSynExpr expr
             yield! clauses |> List.toArray |> Array.collect (traverseSynMatchClause path) |]

        | SynExpr.DoBang (synExpr, _range) -> traverseSynExpr synExpr

        | SynExpr.LibraryOnlyILAssembly _ -> [||]

        | SynExpr.LibraryOnlyStaticOptimization _ -> [||]

        | SynExpr.LibraryOnlyUnionCaseFieldGet _ -> [||]

        | SynExpr.LibraryOnlyUnionCaseFieldSet _ -> [||]

        | SynExpr.ArbitraryAfterError (_debugStr, _range) -> [||]

        | SynExpr.FromParseError (synExpr, _range) -> traverseSynExpr synExpr

        | SynExpr.DiscardAfterMissingQualificationAfterDot (synExpr, _range) -> traverseSynExpr synExpr

        | SynExpr.DebugPoint (innerExpr = innerExpr) -> traverseSynExpr innerExpr

      walker.VisitExpr(origPath, traverseSynExpr origPath, defaultTraverse, expr)

    and traversePat origPath (pat: SynPat) =
      let defaultTraverse p =
        let path = SyntaxNode.SynPat p :: origPath

        match p with
        | SynPat.Paren (p, _) -> traversePat path p
        | SynPat.Or (lhsPat = lhsPat; rhsPat = rhsPat) -> [| lhsPat; rhsPat |] |> Array.collect (traversePat path)
        | SynPat.Ands (ps, _)
        | SynPat.Tuple (_, ps, _)
        | SynPat.ArrayOrList (_, ps, _) -> ps |> List.toArray |> Array.collect (traversePat path)
        | SynPat.Attrib (p, _, _) -> traversePat path p
        | SynPat.LongIdent (argPats = argPats) ->
          match argPats with
          | SynArgPats.Pats ps -> ps |> List.toArray |> Array.collect (traversePat path)
          | SynArgPats.NamePatPairs (ps, _) ->
            ps
            |> List.map (fun (_, _, pat) -> pat)
            |> List.toArray
            |> Array.collect (traversePat path)
        | SynPat.Typed (p, ty, _) -> [| yield! traversePat path p; yield! traverseSynType path ty |]
        | _ -> [||]

      walker.VisitPat(origPath, defaultTraverse, pat)

    and traverseSynType origPath (StripParenTypes ty) =
      let defaultTraverse ty =
        let path = SyntaxNode.SynType ty :: origPath

        match ty with
        | SynType.App (typeName, _, typeArgs, _, _, _, _)
        | SynType.LongIdentApp (typeName, _, _, typeArgs, _, _, _) ->
          [| yield typeName; yield! typeArgs |] |> Array.collect (traverseSynType path)
        | SynType.Fun (ty1, ty2, _) -> [| ty1; ty2 |] |> Array.collect (traverseSynType path)
        | SynType.MeasurePower (ty, _, _)
        | SynType.HashConstraint (ty, _)
        | SynType.WithGlobalConstraints (ty, _, _)
        | SynType.Array (_, ty, _) -> traverseSynType path ty
        | SynType.StaticConstantNamed (ty1, ty2, _)
        | SynType.MeasureDivide (ty1, ty2, _) -> [| ty1; ty2 |] |> Array.collect (traverseSynType path)
        | SynType.Tuple (_, tys, _) -> tys |> List.map snd |> List.toArray |> Array.collect (traverseSynType path)
        | SynType.StaticConstantExpr (expr, _) -> traverseSynExpr [] expr
        | SynType.Anon _ -> [||]
        | _ -> [||]

      walker.VisitType(origPath, defaultTraverse, ty)

    and normalizeMembersToDealWithPeculiaritiesOfGettersAndSetters
      path
      traverseInherit
      (synMemberDefns: SynMemberDefns)
      =
      synMemberDefns
      // property getters are setters are two members that can have the same range, so do some somersaults to deal with this
      |> Seq.groupBy (fun x -> x.Range)
      |> Seq.collect (fun (_, mems) ->
        match mems |> Seq.toList with
        | [ mem ] -> // the typical case, a single member has this range 'r'
          traverseSynMemberDefn path traverseInherit mem
        | [ SynMemberDefn.Member(memberDefn = SynBinding(headPat = SynPat.LongIdent (longDotId = lid1
                                                                                     propertyKeyword = Some (info1)
                                                                                     extraId = Some getset1))) as mem1
            SynMemberDefn.Member(memberDefn = SynBinding(headPat = SynPat.LongIdent (longDotId = lid2
                                                                                     propertyKeyword = Some (info2)
                                                                                     extraId = Some getset2))) as mem2 ] -> // can happen if one is a getter and one is a setter
          // ensure same long id
          assert ((lid1.Lid, lid2.Lid) ||> List.forall2 (fun x y -> x.idText = y.idText))
          // ensure one is getter, other is setter
          assert
            ((getset1.idText = "set" && getset2.idText = "get")
             || (getset2.idText = "set" && getset1.idText = "get"))

          // both mem1 and mem2 have same range, would violate dive-and-pick assertions, so just try the first one, else try the second one:
          match traverseSynMemberDefn path (fun _ -> [||]) mem1 with
          | [||] -> traverseSynMemberDefn path (fun _ -> [||]) mem2
          | nodes -> nodes
        | [] ->
#if DEBUG
          assert false
          failwith "impossible, Seq.groupBy never returns empty results"
#else
          // swallow AST error and recover silently
          [||]
#endif
        | _ ->
#if DEBUG
          assert false // more than 2 members claim to have the same range, this indicates a bug in the AST
          failwith "bug in AST"
#else
          // swallow AST error and recover silently
          [||]
#endif
      )

    and traverseSynTypeDefn
      origPath
      (SynTypeDefn (synComponentInfo, synTypeDefnRepr, synMemberDefns, implicitCtor, _tRange, _) as tydef)
      =
      let path = SyntaxNode.SynTypeDefn tydef :: origPath

      match walker.VisitComponentInfo(origPath, synComponentInfo) with
      | [||] ->
        [| match synTypeDefnRepr with
           | SynTypeDefnRepr.Exception _ ->
             // This node is generated in CheckExpressions.fs, not in the AST.
             // But note exception declarations are missing from this tree walk.
             ()
           | SynTypeDefnRepr.ObjectModel (synTypeDefnKind, synMemberDefns, _oRange) ->
             // traverse inherit function is used to capture type specific data required for processing Inherit part
             let traverseInherit (synType: SynType, range: range) =
               walker.VisitInheritSynMemberDefn(path, synComponentInfo, synTypeDefnKind, synType, synMemberDefns, range)

             yield!
               synMemberDefns
               |> normalizeMembersToDealWithPeculiaritiesOfGettersAndSetters path traverseInherit
           | SynTypeDefnRepr.Simple (synTypeDefnSimpleRepr, _range) ->
             match synTypeDefnSimpleRepr with
             | SynTypeDefnSimpleRepr.TypeAbbrev (_, synType, m) -> yield! walker.VisitTypeAbbrev(path, synType, m)
             | _ -> () // enums/DUs/record definitions don't have any SynExprs inside them
           yield!
             implicitCtor
             |> Option.toArray
             |> Array.collect (traverseSynMemberDefn path (fun _ -> [||]))
           yield!
             synMemberDefns
             |> normalizeMembersToDealWithPeculiaritiesOfGettersAndSetters path (fun _ -> [||]) |]
      | nodes -> nodes

    and traverseSynMemberDefn path traverseInherit (m: SynMemberDefn) =
      let path = SyntaxNode.SynMemberDefn m :: path

      match m with
      | SynMemberDefn.Open (_longIdent, _range) -> [||]
      | SynMemberDefn.Member (synBinding, _range) -> traverseSynBinding path synBinding
      | SynMemberDefn.ImplicitCtor (_synAccessOption, _synAttributes, simplePats, _identOption, _doc, _range) ->
        match simplePats with
        | SynSimplePats.SimplePats (simplePats, _) -> walker.VisitSimplePats(path, simplePats)
        | _ -> [||]
      | SynMemberDefn.ImplicitInherit (synType, synExpr, _identOption, range) ->
        [| yield!
             match traverseInherit (synType, range) with
             | [||] -> walker.VisitImplicitInherit(path, traverseSynExpr path, synType, synExpr, range)
             | x -> x
           yield! walker.VisitImplicitInherit(path, traverseSynExpr path, synType, synExpr, range) |]
      | SynMemberDefn.AutoProperty (synExpr = synExpr) -> traverseSynExpr path synExpr
      | SynMemberDefn.LetBindings (synBindingList, isRecursive, _, range) ->
        match walker.VisitLetOrUse(path, isRecursive, traverseSynBinding path, synBindingList, range) with
        | [||] -> synBindingList |> List.toArray |> Array.collect (traverseSynBinding path)
        | nodes -> nodes
      | SynMemberDefn.AbstractSlot (_synValSig, _memberFlags, _range) -> [||]
      | SynMemberDefn.Interface (interfaceType = interfaceType; members = members) ->
        match walker.VisitInterfaceSynMemberDefnType(path, interfaceType) with
        | [||] ->
          match members with
          | None -> [||]
          | Some (x) ->
            [| yield!
                 x
                 |> normalizeMembersToDealWithPeculiaritiesOfGettersAndSetters path (fun _ -> [||]) |]
        | ok -> ok
      | SynMemberDefn.Inherit (synType, _identOption, range) -> traverseInherit (synType, range)
      | SynMemberDefn.ValField (_synField, _range) -> [||]
      | SynMemberDefn.NestedType (synTypeDefn, _synAccessOption, _range) -> traverseSynTypeDefn path synTypeDefn

    and traverseSynMatchClause origPath mc =
      let defaultTraverse mc =
        let path = SyntaxNode.SynMatchClause mc :: origPath

        match mc with
        | SynMatchClause (pat = synPat; whenExpr = synExprOption; resultExpr = synExpr) ->
          [| yield! traversePat path synPat
             match synExprOption with
             | None -> ()
             | Some guard -> yield! traverseSynExpr path guard
             yield! traverseSynExpr path synExpr |]

      walker.VisitMatchClause(origPath, defaultTraverse, mc)

    and traverseSynBinding origPath b =
      let defaultTraverse b =
        let path = SyntaxNode.SynBinding b :: origPath

        match b with
        | SynBinding (headPat = headPat; expr = expr) ->
          [| yield! traversePat path headPat; yield! traverseSynExpr path expr |]

      walker.VisitBinding(origPath, defaultTraverse, b)

    match parseTree with
    | ParsedInput.ImplFile (ParsedImplFileInput (modules = l)) ->
      l |> List.toArray |> Array.collect (traverseSynModuleOrNamespace [])
    | ParsedInput.SigFile _sigFile -> [||]

[<AutoOpen>]
module ParsedInputExtensions =
  open SyntaxTraversal
  open FSharp.Compiler.EditorServices
  open FSharp.Compiler.Symbols

  [<return: Struct>]
  let (|LID|_|) (matchTexts: string list) (lidParts: LongIdent) =
    Seq.zip matchTexts lidParts
    |> Seq.forall (fun (matchText, lid) -> matchText = lid.idText)
    |> function
      | true -> ValueSome()
      | false -> ValueNone

  [<return: Struct>]
  let (|StringSyntaxAttribute|_|) =
    (|LID|_|) [ "System"; "Diagnostics"; "CodeAnalysis"; "StringSyntaxAttribute" ]

  [<return: Struct>]
  let rec (|HasStringSyntaxAttributeL|_|) (attrs: SynAttribute list) =
    match attrs with
    | [] -> ValueNone
    | x :: xs ->
      match x with
      | { TypeName = LongIdentWithDots (StringSyntaxAttribute, _) } -> ValueSome()
      | _ -> (|HasStringSyntaxAttributeL|_|) xs

  [<return: Struct>]
  let rec (|HasStringSyntaxAttribute|_|) (attrs: FSharp.Compiler.Syntax.SynAttributes) =
    match attrs with
    | [] -> ValueNone
    | x :: xs -> (|HasStringSyntaxAttribute|_|) xs


  let private semanticClassificationWalker =
    { new SyntaxWalkerBase<SemanticClassificationItem>() with
        member x.VisitExpr(_, _, defaultTraverse, synExpr) =
          match synExpr with
          | SynExpr.Const (_, m) ->
            SemanticClassificationItem((m, SemanticClassificationType.Value))
            |> Array.singleton
          | _ -> defaultTraverse synExpr }

  type FSharp.Compiler.Syntax.ParsedInput with

    member parseTree.GetSemanticClassification(_: range option) : SemanticClassificationItem[] =
      SyntaxTraversal.Walk(parseTree, semanticClassificationWalker)

  let walk (c: FSharpImplementationFileContents) : SemanticClassificationItem[] =
    let rec walkDeclaration (d: FSharpImplementationFileDeclaration) =
      match d with
      | FSharpImplementationFileDeclaration.Entity (e, declarations) ->
        [| yield! walkEntity e
           for decl in declarations do
             yield! walkDeclaration decl |]
      | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (mfv, curriedArgList, body) ->
        [| yield! walkMemberOrFunctionOrValue mfv
           for curriedArgs in curriedArgList do
             for arg in curriedArgs do
               yield! walkMemberOrFunctionOrValue arg
           yield! walkExpr body |]
      | FSharpImplementationFileDeclaration.InitAction action -> walkExpr action

    and walkEntity (e: FSharpEntity) = [||]
    and walkExpr (e: FSharpExpr) = [||]
    and walkMemberOrFunctionOrValue (mfv: FSharpMemberOrFunctionOrValue) = [||]

    [| for decl in c.Declarations do
         yield! walkDeclaration decl

       |]


  type FSharp.Compiler.CodeAnalysis.FSharpCheckFileResults with

    member x.GetSemanticClassificationsForReal(?range) =
      [| yield! x.GetSemanticClassification(range)
         yield! x.ImplementationFile |> Option.toArray |> Array.collect walk |]
