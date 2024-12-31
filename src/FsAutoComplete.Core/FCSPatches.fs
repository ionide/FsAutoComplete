/// this file contains patches to the F# Compiler Service that have not yet made it into
/// published nuget packages.  We source-copy them here to have a consistent location for our to-be-removed extensions

module FsAutoComplete.FCSPatches

open FSharp.Compiler.Syntax
open FSharp.Compiler.CodeAnalysis

module internal SynExprAppLocationsImpl =
  let rec private searchSynArgExpr traverseSynExpr expr ranges =
    match expr with
    | SynExpr.Const(SynConst.Unit, _) -> None, None

    | SynExpr.Paren(SynExpr.Tuple(_, exprs, _commas, _tupRange), _, _, _parenRange) ->
      let rec loop (exprs: SynExpr list) ranges =
        match exprs with
        | [] -> ranges
        | h :: t -> loop t (h.Range :: ranges)

      let res = loop exprs ranges
      Some(res), None

    | SynExpr.Paren(SynExpr.Paren(_, _, _, _) as synExpr, _, _, _parenRange) ->
      let r, _cacheOpt = searchSynArgExpr traverseSynExpr synExpr ranges
      r, None

    | SynExpr.Paren(SynExpr.App(_, _isInfix, _, _, _range), _, _, parenRange) -> Some(parenRange :: ranges), None

    | e ->
      let inner = traverseSynExpr e

      match inner with
      | None -> Some(e.Range :: ranges), Some inner
      | _ -> None, Some inner

module SyntaxTreeOps =
  open FSharp.Compiler.Syntax

  let rec synExprContainsError inpExpr =
    let rec walkBind (SynBinding(expr = expr)) = walkExpr expr

    and walkExprs es = es |> List.exists walkExpr

    and walkBinds es = es |> List.exists walkBind

    and walkMatchClauses cl =
      cl
      |> List.exists (fun (SynMatchClause(whenExpr = whenExpr; resultExpr = resultExpr)) ->
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
      | SynExpr.Typar _
      | SynExpr.Const _ -> false

      | SynExpr.TypeTest(e, _, _)
      | SynExpr.Upcast(e, _, _)
      | SynExpr.AddressOf(_, e, _, _)
      | SynExpr.ComputationExpr(_, e, _)
      | SynExpr.ArrayOrListComputed(_, e, _)
      | SynExpr.Typed(e, _, _)
      | SynExpr.FromParseError(e, _)
      | SynExpr.Do(e, _)
      | SynExpr.Assert(e, _)
      | SynExpr.DotGet(e, _, _, _)
      | SynExpr.LongIdentSet(_, e, _)
      | SynExpr.New(_, _, e, _)
      | SynExpr.TypeApp(e, _, _, _, _, _, _)
      | SynExpr.LibraryOnlyUnionCaseFieldGet(e, _, _, _)
      | SynExpr.Downcast(e, _, _)
      | SynExpr.InferredUpcast(e, _)
      | SynExpr.InferredDowncast(e, _)
      | SynExpr.Lazy(e, _)
      | SynExpr.TraitCall(_, _, e, _)
      | SynExpr.YieldOrReturn(_, e, _, _)
      | SynExpr.YieldOrReturnFrom(_, e, _, _)
      | SynExpr.DoBang(e, _, _)
      | SynExpr.Fixed(e, _)
      | SynExpr.Paren(e, _, _, _)
      | SynExpr.DotLambda(expr = e) -> walkExpr e

      | SynExpr.NamedIndexedPropertySet(_, e1, e2, _)
      | SynExpr.DotSet(e1, _, e2, _)
      | SynExpr.Set(e1, e2, _)
      | SynExpr.LibraryOnlyUnionCaseFieldSet(e1, _, _, e2, _)
      | SynExpr.JoinIn(e1, _, e2, _)
      | SynExpr.App(_, _, e1, e2, _) -> walkExpr e1 || walkExpr e2

      | SynExpr.ArrayOrList(_, es, _)
      | SynExpr.Tuple(_, es, _, _) -> walkExprs es

      | SynExpr.AnonRecd(copyInfo = copyInfo; recordFields = recordFields) ->
        (match copyInfo with
         | Some(e, _) -> walkExpr e
         | None -> false)
        || walkExprs (recordFields |> List.map (fun (_ident, _range, expr) -> expr))

      | SynExpr.Record(copyInfo = copyInfo; recordFields = recordFields) ->
        (match copyInfo with
         | Some(e, _) -> walkExpr e
         | None -> false)
        || let flds =
             recordFields |> List.choose (fun (SynExprRecordField(expr = expr)) -> expr) in

           walkExprs flds

      | SynExpr.ObjExpr(bindings = bindings; extraImpls = extraImpls) ->
        walkBinds bindings
        || walkBinds
          [ for (SynInterfaceImpl(bindings = bindings)) in extraImpls do
              yield! bindings ]

      | SynExpr.ForEach(enumExpr = e1; bodyExpr = e2)
      | SynExpr.While(whileExpr = e1; doExpr = e2)
      | SynExpr.WhileBang(whileExpr = e1; doExpr = e2) -> walkExpr e1 || walkExpr e2

      | SynExpr.For(identBody = e1; toBody = e2; doBody = e3) -> walkExpr e1 || walkExpr e2 || walkExpr e3

      | SynExpr.MatchLambda(matchClauses = cl) -> walkMatchClauses cl

      | SynExpr.Lambda(body = body) -> walkExpr body

      | SynExpr.Match(expr = e; clauses = cl) -> walkExpr e || walkMatchClauses cl

      | SynExpr.LetOrUse(bindings = bs; body = e) -> walkBinds bs || walkExpr e

      | SynExpr.TryWith(tryExpr = e; withCases = cl) -> walkExpr e || walkMatchClauses cl

      | SynExpr.TryFinally(tryExpr = e1; finallyExpr = e2) -> walkExpr e1 || walkExpr e2

      | SynExpr.Sequential(expr1 = e1; expr2 = e2) -> walkExpr e1 || walkExpr e2

      | SynExpr.SequentialOrImplicitYield(_, e1, e2, _, _) -> walkExpr e1 || walkExpr e2

      | SynExpr.IfThenElse(ifExpr = e1; thenExpr = e2; elseExpr = e3opt) ->
        walkExpr e1 || walkExpr e2 || walkExprOpt e3opt

      | SynExpr.DotIndexedGet(e1, es, _, _) -> walkExpr e1 || walkExpr es

      | SynExpr.DotIndexedSet(e1, es, e2, _, _, _) -> walkExpr e1 || walkExpr es || walkExpr e2

      | SynExpr.DotNamedIndexedPropertySet(e1, _, e2, e3, _) -> walkExpr e1 || walkExpr e2 || walkExpr e3

      | SynExpr.MatchBang(expr = e; clauses = cl) -> walkExpr e || walkMatchClauses cl

      | SynExpr.LetOrUseBang(rhs = e1; body = e2; andBangs = es) ->
        walkExpr e1
        || walkExprs
          [ for (SynExprAndBang(body = e)) in es do
              yield e ]
        || walkExpr e2

      | SynExpr.InterpolatedString(parts, _, _m) ->
        walkExprs (
          parts
          |> List.choose (function
            | SynInterpolatedStringPart.String _ -> None
            | SynInterpolatedStringPart.FillExpr(x, _) -> Some x)
        )
      | SynExpr.IndexRange(expr1 = expr1; expr2 = expr2) ->
        Option.map walkExpr expr1
        |> Option.orElseWith (fun _ -> Option.map walkExpr expr2)
        |> Option.defaultValue false
      | SynExpr.IndexFromEnd(expr, _) -> walkExpr expr
      | SynExpr.DebugPoint(innerExpr = expr) -> walkExpr expr
      | SynExpr.Dynamic(funcExpr = funcExpr; argExpr = argExpr) -> walkExpr funcExpr || walkExpr argExpr


    walkExpr inpExpr

open System
open System.Reflection
open Microsoft.FSharp.Reflection

module ReflectionDelegates =

  let public BindingFlagsToSeeAll: BindingFlags =
    BindingFlags.Static
    ||| BindingFlags.FlattenHierarchy
    ||| BindingFlags.Instance
    ||| BindingFlags.NonPublic
    ||| BindingFlags.Public

  let createFuncArity1<'returnType> (instanceType: Type) (arg1: Type) (getterName: string) =
    let method = instanceType.GetMethod(getterName, BindingFlagsToSeeAll)

    let getFunc =
      typedefof<Func<_, _, _>>
        .MakeGenericType(instanceType, arg1, typeof<'returnType>)

    let delegate2 = method.CreateDelegate(getFunc)
    // TODO: Emit IL for performance
    fun (instance, arg1) -> delegate2.DynamicInvoke [| instance; arg1 |] |> unbox<bool>

  let createGetter<'returnType> (instanceType: System.Type) (getterName: string) =
    let method =
      instanceType.GetProperty(getterName, BindingFlagsToSeeAll).GetGetMethod(true)

    let getFunc =
      typedefof<Func<_, _>>.MakeGenericType(instanceType, typeof<'returnType>)

    let delegate2 = method.CreateDelegate(getFunc)
    // TODO: Emit IL for performance
    fun instance -> delegate2.DynamicInvoke [| instance |] |> unbox<bool>


/// <summary>
/// Reflection Shim around the <see href="https://github.com/dotnet/fsharp/blob/7725ddbd61ab3e5bf7e2fc35d76a0ece3903a5d9/src/Compiler/Facilities/LanguageFeatures.fs#L18">LanguageFeature</see> in FSharp.Compiler.Service
/// </summary>
type LanguageFeatureShim(langFeature: string) =
  static let LanguageFeatureTy =
    lazy (Type.GetType("FSharp.Compiler.Features+LanguageFeature, FSharp.Compiler.Service"))

  static let cases =
    lazy (FSharpType.GetUnionCases(LanguageFeatureTy.Value, ReflectionDelegates.BindingFlagsToSeeAll))

  let case =
    lazy
      (let v = cases.Value |> Array.tryFind (fun c -> c.Name = langFeature)

       v
       |> Option.map (fun x -> FSharpValue.MakeUnion(x, [||], ReflectionDelegates.BindingFlagsToSeeAll)))

  member x.Case = case.Value

  static member Type = LanguageFeatureTy.Value

// Worth keeping commented out as they shouldn't be used until we need to find other properties/methods to support
// member x.Properties = LanguageFeatureShim.Type.GetProperties(ReflectionDelegates.BindingFlagsToSeeAll)
// member x.Methods = LanguageFeatureShim.Type.GetMethods(ReflectionDelegates.BindingFlagsToSeeAll)
// member x.Fields = LanguageFeatureShim.Type.GetFields(ReflectionDelegates.BindingFlagsToSeeAll)
// member x.Cases = cases.Value

/// <summary>
/// Reflection Shim around the <see href="https://github.com/dotnet/fsharp/blob/7725ddbd61ab3e5bf7e2fc35d76a0ece3903a5d9/src/Compiler/Facilities/LanguageFeatures.fs#L76">LanguageVersion</see> in FSharp.Compiler.Service
/// </summary>
type LanguageVersionShim(versionText: string) =
  static let LanguageVersionTy =
    lazy (Type.GetType("FSharp.Compiler.Features+LanguageVersion, FSharp.Compiler.Service"))

  static let ctor = lazy (LanguageVersionTy.Value.GetConstructor([| typeof<string> |]))

  static let isPreviewEnabled =
    lazy (ReflectionDelegates.createGetter<bool> LanguageVersionTy.Value "IsPreviewEnabled")

  static let supportsFeature =
    lazy (ReflectionDelegates.createFuncArity1<bool> LanguageVersionTy.Value LanguageFeatureShim.Type "SupportsFeature")

  let realLanguageVersion = ctor.Value.Invoke([| versionText |])

  member x.IsPreviewEnabled = isPreviewEnabled.Value realLanguageVersion

  member x.SupportsFeature(featureId: LanguageFeatureShim) =
    match featureId.Case with
    | None -> false
    | Some x -> supportsFeature.Value(realLanguageVersion, x)

  member x.Real = realLanguageVersion
  static member Type = LanguageVersionTy.Value

// Worth keeping commented out as they shouldn't be used until we need to find other properties/methods to support
// member x.Properties = LanguageVersionShim.Type.GetProperties(ReflectionDelegates.BindingFlagsToSeeAll)
// member x.Methods = LanguageVersionShim.Type.GetMethods(ReflectionDelegates.BindingFlagsToSeeAll)
// member x.Fields = LanguageVersionShim.Type.GetFields(ReflectionDelegates.BindingFlagsToSeeAll)

module LanguageVersionShim =

  /// <summary>Default is "latest"</summary>
  /// <returns></returns>
  let defaultLanguageVersion = lazy (LanguageVersionShim("latest"))

  /// <summary>Tries to parse out "--langversion:" from OtherOptions if it can't find it, returns defaultLanguageVersion</summary>
  /// <param name="options">The OtherOptions to use</param>
  /// <returns>A LanguageVersionShim from the parsed "--langversion:" or defaultLanguageVersion </returns>
  let fromOtherOptions (options: string seq) =
    options
    |> Seq.tryFind (fun x -> x.StartsWith("--langversion:", StringComparison.Ordinal))
    |> Option.map (fun x -> x.Split(":")[1])
    |> Option.map (fun x -> LanguageVersionShim(x))
    |> Option.defaultWith (fun () -> defaultLanguageVersion.Value)

  /// <summary>Tries to parse out "--langversion:" from OtherOptions if it can't find it, returns defaultLanguageVersion</summary>
  /// <param name="fpo">The FSharpProjectOptions to use</param>
  /// <returns>A LanguageVersionShim from the parsed "--langversion:" or defaultLanguageVersion </returns>
  let fromFSharpProjectOptions (fpo: FSharpProjectOptions) = fpo.OtherOptions |> fromOtherOptions


  /// <summary>Tries to parse out "--langversion:" from OtherOptions if it can't find it, returns defaultLanguageVersion</summary>
  /// <param name="fpo">The FSharpProjectOptions to use</param>
  /// <returns>A LanguageVersionShim from the parsed "--langversion:" or defaultLanguageVersion </returns>
  let fromFSharpProjectSnapshot (fpo: FSharpProjectSnapshot) = fpo.OtherOptions |> fromOtherOptions
