/// this file contains patches to the F# Compiler Service that have not yet made it into
/// published nuget packages.  We source-copy them here to have a consistent location for our to-be-removed extensions

module FsAutoComplete.FCSPatches

open FSharp.Compiler.EditorServices
open FSharp.Compiler.Syntax
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open FsAutoComplete.UntypedAstUtils

module SyntaxTreeOps =
  open FSharp.Compiler.Syntax
  let rec synExprContainsError inpExpr =
    let rec walkBind (SynBinding(_, _, _, _, _, _, _, _, _, synExpr, _, _)) = walkExpr synExpr

    and walkExprs es = es |> List.exists walkExpr

    and walkBinds es = es |> List.exists walkBind

    and walkMatchClauses cl =
        cl |> List.exists (fun (SynMatchClause(_, whenExpr, e, _, _)) -> walkExprOpt whenExpr || walkExpr e)

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
              walkBinds bs || walkBinds [ for (SynInterfaceImpl(_, bs, _)) in is do yield! bs  ]

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

          | SynExpr.InterpolatedString (parts, _, _m) ->
              walkExprs
                  (parts |> List.choose (function
                      | SynInterpolatedStringPart.String _ -> None
                      | SynInterpolatedStringPart.FillExpr (x, _) -> Some x))

    walkExpr inpExpr
