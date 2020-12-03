/// this file contains patches to the F# Compiler Service that have not yet made it into
/// published nuget packages.  We source-copy them here to have a consistent location for our to-be-removed extensions

module FsAutoComplete.FCSPatches

open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.SyntaxTree
open FSharp.Compiler.Range
open FsAutoComplete.UntypedAstUtils

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
                    if funcIdent.idText = "op_Dereference" && rangeContainsPos expr.Range expressionPos then
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
                | SynExpr.Record(_, _, _, range) when rangeContainsPos range pos ->
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
                  | SynExpr.YieldOrReturnFrom(_, expr, range) when rangeContainsPos range pos ->
                      Some expr.Range
                  | _ -> defaultTraverse expr })
      | None -> None
