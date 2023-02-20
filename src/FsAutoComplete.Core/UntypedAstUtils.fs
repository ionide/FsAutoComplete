/// Code from VisualFSharpPowerTools project: https://github.com/fsprojects/VisualFSharpPowerTools/blob/master/src/FSharp.Editing/Common/UntypedAstUtils.fs
module FsAutoComplete.UntypedAstUtils

open FSharp.Compiler.Syntax
open System.Collections.Generic
open FSharp.Compiler
open FSharp.Compiler.Text
open FSharp.Control.Reactive.Observable

type Range with

  member inline x.IsEmpty = x.StartColumn = x.EndColumn && x.StartLine = x.EndLine

type internal ShortIdent = string
type internal Idents = ShortIdent[]

let internal longIdentToArray (longIdent: LongIdent) : Idents =
  longIdent |> Seq.map string |> Seq.toArray

/// An recursive pattern that collect all sequential expressions to avoid StackOverflowException
let rec (|Sequentials|_|) =
  function
  | SynExpr.Sequential(_, _, e, Sequentials es, _) -> Some(e :: es)
  | SynExpr.Sequential(_, _, e1, e2, _) -> Some [ e1; e2 ]
  | _ -> None

let (|ConstructorPats|) =
  function
  | SynArgPats.Pats ps -> ps
  | SynArgPats.NamePatPairs(pats = xs) -> xs |> List.map (fun (_, _, pat) -> pat)

/// matches if the range contains the position
let (|ContainsPos|_|) pos range =
  if Range.rangeContainsPos range pos then Some() else None

/// Active pattern that matches an ident on a given name by the ident's `idText`
let (|Ident|_|) ofName =
  function
  | SynExpr.Ident ident when ident.idText = ofName -> Some()
  | _ -> None

/// matches if the range contains the position
let (|IdentContainsPos|_|) pos (ident: Ident) = (|ContainsPos|_|) pos ident.idRange

/// A pattern that collects all attributes from a `SynAttributes` into a single flat list
let (|AllAttrs|) (attrs: SynAttributes) =
  attrs |> List.collect (fun attrList -> attrList.Attributes)

/// A pattern that collects all patterns from a `SynSimplePats` into a single flat list
let (|AllSimplePats|) (pats: SynSimplePats) =
  let rec loop acc pat =
    match pat with
    | SynSimplePats.SimplePats(pats, _) -> acc @ pats
    | SynSimplePats.Typed(pats, _, _) -> loop acc pats

  loop [] pats

/// Gives all ranges for current position
let internal getRangesAtPosition input (r: Position) : Range list =
  let mutable result = []


  let addIfInside (ran: Range) =
    let addToResult r = result <- r :: result

    let isInside (ran: Range) = Range.rangeContainsPos ran r

    if isInside ran then
      addToResult ran



  let rec walkImplFileInput (ParsedImplFileInput(contents = moduleOrNamespaceList)) =
    List.iter walkSynModuleOrNamespace moduleOrNamespaceList

  and walkSynModuleOrNamespace (SynModuleOrNamespace(decls = decls; attribs = AllAttrs attrs; range = r)) =
    addIfInside r
    List.iter walkAttribute attrs
    List.iter walkSynModuleDecl decls

  and walkAttribute (attr: SynAttribute) =
    addIfInside attr.Range
    walkExpr attr.ArgExpr

  and walkTyparDecl (SynTyparDecl(attributes = AllAttrs attrs; Item2 = typar)) =
    List.iter walkAttribute attrs
    walkTypar typar

  and walkTyparDecls (typars: SynTyparDecls) =
    typars.TyparDecls |> List.iter walkTyparDecl
    typars.Constraints |> List.iter walkTypeConstraint

  and walkSynValTyparDecls (SynValTyparDecls(typars, _)) = Option.iter walkTyparDecls typars

  and walkTypeConstraint =
    function
    | SynTypeConstraint.WhereTyparIsValueType(t, r)
    | SynTypeConstraint.WhereTyparIsReferenceType(t, r)
    | SynTypeConstraint.WhereTyparIsUnmanaged(t, r)
    | SynTypeConstraint.WhereTyparSupportsNull(t, r)
    | SynTypeConstraint.WhereTyparIsComparable(t, r)
    | SynTypeConstraint.WhereTyparIsEquatable(t, r) ->
      addIfInside r
      walkTypar t
    | SynTypeConstraint.WhereTyparDefaultsToType(t, ty, r)
    | SynTypeConstraint.WhereTyparSubtypeOfType(t, ty, r) ->
      addIfInside r
      walkTypar t
      walkType ty
    | SynTypeConstraint.WhereTyparIsEnum(t, ts, r)
    | SynTypeConstraint.WhereTyparIsDelegate(t, ts, r) ->
      addIfInside r
      walkTypar t
      List.iter walkType ts
    | SynTypeConstraint.WhereTyparSupportsMember(t, sign, r) ->
      addIfInside r
      walkType t
      walkMemberSig sign
    | SynTypeConstraint.WhereSelfConstrained(t, r) ->
      addIfInside r
      walkType t

  and walkPat =
    function
    | SynPat.Tuple(_, pats, r)
    | SynPat.ArrayOrList(_, pats, r)
    | SynPat.Ands(pats, r) ->
      addIfInside r
      List.iter walkPat pats
    | SynPat.Named(ident, _, _, r) -> addIfInside r
    | SynPat.Typed(pat, t, r) ->
      addIfInside r
      walkPat pat
      walkType t
    | SynPat.Attrib(pat, AllAttrs attrs, r) ->
      addIfInside r
      walkPat pat
      List.iter walkAttribute attrs
    | SynPat.Or(pat1, pat2, r, _) ->
      addIfInside r
      List.iter walkPat [ pat1; pat2 ]
    | SynPat.LongIdent(typarDecls = typars; argPats = ConstructorPats pats; range = r) ->
      addIfInside r
      Option.iter walkSynValTyparDecls typars
      List.iter walkPat pats
    | SynPat.Paren(pat, r) ->
      addIfInside r
      walkPat pat
    | SynPat.IsInst(t, r) ->
      addIfInside r
      walkType t
    | SynPat.QuoteExpr(e, r) ->
      addIfInside r
      walkExpr e
    | SynPat.Const(_, r) -> addIfInside r
    | SynPat.Wild(r) -> addIfInside r
    | SynPat.Record(_, r) -> addIfInside r
    | SynPat.Null(r) -> addIfInside r
    | SynPat.OptionalVal(_, r) -> addIfInside r
    | SynPat.DeprecatedCharRange(_, _, r) -> addIfInside r
    | SynPat.InstanceMember(_, _, _, accessibility, r) -> addIfInside r
    | SynPat.FromParseError(_, r) -> addIfInside r
    | SynPat.As(lpat, rpat, r) ->
      addIfInside r
      walkPat lpat
      walkPat rpat
    | SynPat.ListCons(lpat, rpat, r, _) ->
      addIfInside r
      walkPat lpat
      walkPat rpat

  and walkTypar (SynTypar(_, _, _)) = ()

  and walkBinding
    (SynBinding(attributes = AllAttrs attrs; headPat = pat; returnInfo = returnInfo; expr = e; range = r))
    =
    addIfInside r
    List.iter walkAttribute attrs
    walkPat pat
    walkExpr e

    returnInfo
    |> Option.iter (fun (SynBindingReturnInfo(t, r, attrs, _)) ->
      addIfInside r
      walkType t
      walkAttributes attrs)

  and walkAttributes (attrs: SynAttributes) =
    List.iter
      (fun (attrList: SynAttributeList) ->
        addIfInside attrList.Range
        List.iter walkAttribute attrList.Attributes)
      attrs

  and walkInterfaceImpl (SynInterfaceImpl(bindings = bindings; range = r)) =
    addIfInside r
    List.iter walkBinding bindings

  and walkType =
    function
    | SynType.Array(_, t, r)
    | SynType.HashConstraint(t, r)
    | SynType.MeasurePower(t, _, r) ->
      addIfInside r
      walkType t
    | SynType.Fun(t1, t2, r, _) ->
      // | SynType.MeasureDivide(t1, t2, r) ->
      addIfInside r
      walkType t1
      walkType t2
    | SynType.App(ty, _, types, _, _, _, r) ->
      addIfInside r
      walkType ty
      List.iter walkType types
    | SynType.LongIdentApp(_, _, _, types, _, _, r) ->
      addIfInside r
      List.iter walkType types
    | SynType.Tuple(_, ts, r) ->
      addIfInside r

      ts
      |> List.iter (function
        | SynTupleTypeSegment.Type t -> walkType t
        | _ -> ())
    | SynType.WithGlobalConstraints(t, typeConstraints, r) ->
      addIfInside r
      walkType t
      List.iter walkTypeConstraint typeConstraints
    | SynType.LongIdent(longDotId) -> ()
    | SynType.AnonRecd(isStruct, typeNames, r) -> addIfInside r
    | SynType.Var(genericName, r) -> addIfInside r
    | SynType.Anon(r) -> addIfInside r
    | SynType.StaticConstant(constant, r) -> addIfInside r
    | SynType.StaticConstantExpr(expr, r) -> addIfInside r
    | SynType.StaticConstantNamed(expr, _, r) -> addIfInside r
    | SynType.Paren(innerType, r) ->
      addIfInside r
      walkType innerType
    | SynType.SignatureParameter(usedType = t; range = r) ->
      addIfInside r
      walkType t
    | SynType.Or(lhs, rhs, r, _) ->
      addIfInside r
      walkType lhs
      walkType rhs

  and walkClause (SynMatchClause(pat, e1, e2, r, _, _)) =
    addIfInside r
    walkPat pat
    walkExpr e2
    e1 |> Option.iter walkExpr

  and walkSimplePats =
    function
    | SynSimplePats.SimplePats(pats, r) ->
      addIfInside r
      List.iter walkSimplePat pats
    | SynSimplePats.Typed(pats, ty, r) ->
      addIfInside r
      walkSimplePats pats
      walkType ty

  and walkInterpolatedStringPart =
    function
    | SynInterpolatedStringPart.FillExpr(expr, ident) ->
      ident |> Option.iter (fun ident -> addIfInside ident.idRange)

      walkExpr expr
    | SynInterpolatedStringPart.String(s, r) -> addIfInside r

  and walkExpr =
    function
    | SynExpr.Typed(e, _, r) ->
      addIfInside r
      walkExpr e
    | SynExpr.Paren(e, _, _, r)
    | SynExpr.Quote(_, _, e, _, r)
    | SynExpr.InferredUpcast(e, r)
    | SynExpr.InferredDowncast(e, r)
    | SynExpr.AddressOf(_, e, _, r)
    | SynExpr.DoBang(e, r)
    | SynExpr.YieldOrReturn(_, e, r)
    | SynExpr.ArrayOrListComputed(_, e, r)
    | SynExpr.ComputationExpr(_, e, r)
    | SynExpr.Do(e, r)
    | SynExpr.Assert(e, r)
    | SynExpr.Lazy(e, r)
    | SynExpr.YieldOrReturnFrom(_, e, r) ->
      addIfInside r
      walkExpr e
    | SynExpr.SequentialOrImplicitYield(_, e1, e2, ifNotE, r) ->
      addIfInside r
      walkExpr e1
      walkExpr e2
      walkExpr ifNotE
    | SynExpr.Lambda(args = pats; body = e; range = r) ->
      addIfInside r
      walkSimplePats pats
      walkExpr e
    | SynExpr.New(_, t, e, r)
    | SynExpr.TypeTest(e, t, r)
    | SynExpr.Upcast(e, t, r)
    | SynExpr.Downcast(e, t, r) ->
      addIfInside r
      walkExpr e
      walkType t
    | SynExpr.Tuple(_, es, _, _)
    | Sequentials es -> List.iter walkExpr es //TODO??
    | SynExpr.ArrayOrList(_, es, r) ->
      addIfInside r
      List.iter walkExpr es
    | SynExpr.App(_, _, e1, e2, r)
    | SynExpr.TryFinally(e1, e2, r, _, _, _)
    | SynExpr.While(_, e1, e2, r) ->
      addIfInside r
      List.iter walkExpr [ e1; e2 ]
    | SynExpr.Record(_, _, fields, r) ->
      addIfInside r

      fields
      |> List.iter (fun (SynExprRecordField(fieldName = (ident, _); expr = e)) -> e |> Option.iter walkExpr)
    | SynExpr.ObjExpr(ty, argOpt, _, bindings, _, ifaces, _, r) ->
      addIfInside r

      argOpt |> Option.iter (fun (e, ident) -> walkExpr e)

      walkType ty
      List.iter walkBinding bindings
      List.iter walkInterfaceImpl ifaces
    | SynExpr.For(identBody = e1; toBody = e2; doBody = e3; range = r) ->
      addIfInside r
      List.iter walkExpr [ e1; e2; e3 ]
    | SynExpr.ForEach(_, _, _, _, pat, e1, e2, r) ->
      addIfInside r
      walkPat pat
      List.iter walkExpr [ e1; e2 ]
    | SynExpr.MatchLambda(_, _, synMatchClauseList, _, r) ->
      addIfInside r
      List.iter walkClause synMatchClauseList
    | SynExpr.Match(expr = e; clauses = synMatchClauseList; range = r) ->
      addIfInside r
      walkExpr e
      List.iter walkClause synMatchClauseList
    | SynExpr.TypeApp(e, _, tys, _, _, tr, r) ->
      addIfInside tr
      addIfInside r
      List.iter walkType tys
      walkExpr e
    | SynExpr.LetOrUse(bindings = bindings; body = e; range = r) ->
      addIfInside r
      List.iter walkBinding bindings
      walkExpr e
    | SynExpr.TryWith(tryExpr = e; withCases = clauses; range = r) ->
      addIfInside r
      List.iter walkClause clauses
      walkExpr e
    | SynExpr.IfThenElse(ifExpr = e1; thenExpr = e2; elseExpr = e3; range = r) ->
      addIfInside r
      List.iter walkExpr [ e1; e2 ]
      e3 |> Option.iter walkExpr
    | SynExpr.LongIdentSet(ident, e, r)
    | SynExpr.DotGet(e, _, ident, r) ->
      addIfInside r
      walkExpr e
    | SynExpr.DotSet(e1, idents, e2, r) ->
      addIfInside r
      walkExpr e1
      walkExpr e2
    | SynExpr.DotIndexedGet(e, args, _, r) ->
      addIfInside r
      walkExpr e
      walkExpr args
    | SynExpr.DotIndexedSet(e1, args, e2, _, _, r) ->
      addIfInside r
      walkExpr e1
      walkExpr args
      walkExpr e2
    | SynExpr.NamedIndexedPropertySet(ident, e1, e2, r) ->
      addIfInside r
      List.iter walkExpr [ e1; e2 ]
    | SynExpr.DotNamedIndexedPropertySet(e1, ident, e2, e3, r) ->
      addIfInside r
      List.iter walkExpr [ e1; e2; e3 ]
    | SynExpr.JoinIn(e1, _, e2, r) ->
      addIfInside r
      List.iter walkExpr [ e1; e2 ]
    | SynExpr.LetOrUseBang(pat = pat; rhs = e1; andBangs = ands; body = e2; range = r) ->
      addIfInside r
      walkPat pat
      walkExpr e1

      for (SynExprAndBang(pat = pat; body = body; range = r)) in ands do
        addIfInside r
        walkPat pat
        walkExpr body

      walkExpr e2
    | SynExpr.TraitCall(t, sign, e, r) ->
      addIfInside r
      walkType t
      walkMemberSig sign
      walkExpr e
    | SynExpr.Const(SynConst.Measure(_, _, m), r) ->
      addIfInside r
      walkMeasure m
    | SynExpr.Const(_, r) -> addIfInside r
    | SynExpr.AnonRecd(isStruct, copyInfo, recordFields, r) -> addIfInside r
    | SynExpr.Sequential(seqPoint, isTrueSeq, expr1, expr2, r) -> ()
    | SynExpr.Ident(_) -> ()
    | SynExpr.LongIdent(isOptional, longDotId, altNameRefCell, r) -> addIfInside r
    | SynExpr.Set(_, _, r) -> addIfInside r
    | SynExpr.Null(r) -> addIfInside r
    | SynExpr.ImplicitZero(r) -> addIfInside r
    | SynExpr.MatchBang(range = r) -> addIfInside r
    | SynExpr.LibraryOnlyILAssembly(_, _, _, _, r) -> addIfInside r
    | SynExpr.LibraryOnlyStaticOptimization(_, _, _, r) -> addIfInside r
    | SynExpr.LibraryOnlyUnionCaseFieldGet(expr, longId, _, r) -> addIfInside r
    | SynExpr.LibraryOnlyUnionCaseFieldSet(_, longId, _, _, r) -> addIfInside r
    | SynExpr.ArbitraryAfterError(debugStr, r) -> addIfInside r
    | SynExpr.FromParseError(expr, r) -> addIfInside r
    | SynExpr.DiscardAfterMissingQualificationAfterDot(_, r) -> addIfInside r
    | SynExpr.Fixed(expr, r) -> addIfInside r
    | SynExpr.InterpolatedString(parts, kind, r) ->
      addIfInside r

      for part in parts do
        walkInterpolatedStringPart part
    | SynExpr.IndexFromEnd(itemExpr, r) ->
      addIfInside r
      walkExpr itemExpr
    | SynExpr.IndexRange(e1, _, e2, _, _, r) ->
      addIfInside r
      Option.iter walkExpr e1
      Option.iter walkExpr e2
    | SynExpr.DebugPoint(innerExpr = expr) -> walkExpr expr
    | SynExpr.Dynamic(funcExpr = e1; argExpr = e2; range = range) ->
      addIfInside range
      walkExpr e1
      walkExpr e2
    | SynExpr.Typar(t, r) ->
      addIfInside r
      walkTypar t

  and walkMeasure =
    function
    | SynMeasure.Product(m1, m2, r)
    | SynMeasure.Divide(m1, m2, r) ->
      addIfInside r
      walkMeasure m1
      walkMeasure m2
    | SynMeasure.Named(longIdent, r) -> addIfInside r
    | SynMeasure.Seq(ms, r) ->
      addIfInside r
      List.iter walkMeasure ms
    | SynMeasure.Power(m, _, r) ->
      addIfInside r
      walkMeasure m
    | SynMeasure.Var(ty, r) ->
      addIfInside r
      walkTypar ty
    | SynMeasure.Paren(m, r) ->
      addIfInside r
      walkMeasure m
    | SynMeasure.One
    | SynMeasure.Anon _ -> ()

  and walkSimplePat =
    function
    | SynSimplePat.Attrib(pat, AllAttrs attrs, r) ->
      addIfInside r
      walkSimplePat pat
      List.iter walkAttribute attrs
    | SynSimplePat.Typed(pat, t, r) ->
      addIfInside r
      walkSimplePat pat
      walkType t
    | SynSimplePat.Id(ident, altNameRefCell, isCompilerGenerated, isThisVar, isOptArg, r) -> addIfInside r


  and walkField (SynField(attributes = AllAttrs attrs; fieldType = t; range = r)) =
    addIfInside r
    List.iter walkAttribute attrs
    walkType t

  and walkValSig
    (SynValSig(attributes = AllAttrs attrs; synType = t; arity = SynValInfo(argInfos, argInfo); range = r))
    =
    addIfInside r
    List.iter walkAttribute attrs
    walkType t

    argInfo :: (argInfos |> List.concat)
    |> List.collect (fun (SynArgInfo(attributes = AllAttrs attrs)) -> attrs)
    |> List.iter walkAttribute

  and walkMemberSig =
    function
    | SynMemberSig.Inherit(t, r)
    | SynMemberSig.Interface(t, r) ->
      addIfInside r
      walkType t
    | SynMemberSig.Member(vs, _, r, _) ->
      addIfInside r
      walkValSig vs
    | SynMemberSig.ValField(f, r) ->
      addIfInside r
      walkField f
    | SynMemberSig.NestedType(SynTypeDefnSig(typeInfo = info; typeRepr = repr; members = memberSigs), r) ->
      addIfInside r

      let isTypeExtensionOrAlias =
        match repr with
        | SynTypeDefnSigRepr.Simple(SynTypeDefnSimpleRepr.TypeAbbrev _, _)
        | SynTypeDefnSigRepr.ObjectModel(SynTypeDefnKind.Abbrev, _, _)
        | SynTypeDefnSigRepr.ObjectModel(SynTypeDefnKind.Augmentation _, _, _) -> true
        | _ -> false

      walkComponentInfo isTypeExtensionOrAlias info
      walkTypeDefnSigRepr repr
      List.iter walkMemberSig memberSigs

  and walkMember =
    function
    | SynMemberDefn.AbstractSlot(valSig, _, r, _) ->
      addIfInside r
      walkValSig valSig
    | SynMemberDefn.Member(binding, r) ->
      addIfInside r
      walkBinding binding
    | SynMemberDefn.ImplicitCtor(_, AllAttrs attrs, AllSimplePats pats, _, _, r) ->
      addIfInside r
      List.iter walkAttribute attrs
      List.iter walkSimplePat pats
    | SynMemberDefn.ImplicitInherit(t, e, _, r) ->
      addIfInside r
      walkType t
      walkExpr e
    | SynMemberDefn.LetBindings(bindings, _, _, r) ->
      addIfInside r
      List.iter walkBinding bindings
    | SynMemberDefn.Interface(t, _, members, r) ->
      addIfInside r
      walkType t
      members |> Option.iter (List.iter walkMember)
    | SynMemberDefn.Inherit(t, _, r) ->
      addIfInside r
      walkType t
    | SynMemberDefn.ValField(field, r) ->
      addIfInside r
      walkField field
    | SynMemberDefn.NestedType(tdef, _, r) ->
      addIfInside r
      walkTypeDefn tdef
    | SynMemberDefn.AutoProperty(attributes = AllAttrs attrs; typeOpt = t; synExpr = e; range = r) ->
      addIfInside r
      List.iter walkAttribute attrs
      Option.iter walkType t
      walkExpr e
    | SynMemberDefn.Open(longId, r) -> addIfInside r
    | SynMemberDefn.GetSetMember(memberDefnForGet = getter; memberDefnForSet = setter; range = range) ->
      addIfInside range
      Option.iter walkBinding getter
      Option.iter walkBinding setter

  and walkEnumCase (SynEnumCase(attributes = AllAttrs attrs; range = r)) =
    addIfInside r
    List.iter walkAttribute attrs

  and walkUnionCaseType =
    function
    | SynUnionCaseKind.Fields fields -> List.iter walkField fields
    | SynUnionCaseKind.FullType(t, _) -> walkType t

  and walkUnionCase (SynUnionCase(attributes = AllAttrs attrs; caseType = t; range = r)) =
    addIfInside r
    List.iter walkAttribute attrs
    walkUnionCaseType t

  and walkTypeDefnSimple =
    function
    | SynTypeDefnSimpleRepr.Enum(cases, r) ->
      addIfInside r
      List.iter walkEnumCase cases
    | SynTypeDefnSimpleRepr.Union(_, cases, r) ->
      addIfInside r
      List.iter walkUnionCase cases
    | SynTypeDefnSimpleRepr.Record(_, fields, r) ->
      addIfInside r
      List.iter walkField fields
    | SynTypeDefnSimpleRepr.TypeAbbrev(_, t, r) ->
      addIfInside r
      walkType t
    | SynTypeDefnSimpleRepr.General(_, _, _, _, _, _, _, r) -> addIfInside r
    | SynTypeDefnSimpleRepr.LibraryOnlyILAssembly(_, r) -> addIfInside r
    | SynTypeDefnSimpleRepr.None(r) -> addIfInside r
    | SynTypeDefnSimpleRepr.Exception(_) -> ()

  and walkComponentInfo
    isTypeExtensionOrAlias
    (SynComponentInfo(
      attributes = AllAttrs attrs; typeParams = typars; constraints = constraints; longId = longIdent; range = r))
    =
    addIfInside r
    List.iter walkAttribute attrs
    Option.iter walkTyparDecls typars
    List.iter walkTypeConstraint constraints

  and walkTypeDefnRepr =
    function
    | SynTypeDefnRepr.ObjectModel(_, defns, r) ->
      addIfInside r
      List.iter walkMember defns
    | SynTypeDefnRepr.Simple(defn, r) ->
      addIfInside r
      walkTypeDefnSimple defn
    | SynTypeDefnRepr.Exception _ -> ()

  and walkTypeDefnSigRepr =
    function
    | SynTypeDefnSigRepr.ObjectModel(_, defns, _) -> List.iter walkMemberSig defns
    | SynTypeDefnSigRepr.Simple(defn, _) -> walkTypeDefnSimple defn
    | SynTypeDefnSigRepr.Exception _ -> ()

  and walkTypeDefn (SynTypeDefn(info, repr, members, implicitCtor, r, _)) =
    addIfInside r

    let isTypeExtensionOrAlias =
      match repr with
      | SynTypeDefnRepr.ObjectModel(SynTypeDefnKind.Augmentation _, _, _)
      | SynTypeDefnRepr.ObjectModel(SynTypeDefnKind.Abbrev, _, _)
      | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.TypeAbbrev _, _) -> true
      | _ -> false

    walkComponentInfo isTypeExtensionOrAlias info
    walkTypeDefnRepr repr
    Option.iter walkMember implicitCtor
    List.iter walkMember members

  and walkSynModuleDecl (decl: SynModuleDecl) =
    match decl with
    | SynModuleDecl.NamespaceFragment fragment -> walkSynModuleOrNamespace fragment
    | SynModuleDecl.NestedModule(info, _, modules, _, r, _) ->
      addIfInside r
      walkComponentInfo false info
      List.iter walkSynModuleDecl modules
    | SynModuleDecl.Let(_, bindings, r) ->
      addIfInside r
      List.iter walkBinding bindings
    | SynModuleDecl.Expr(expr, r) ->
      addIfInside r
      walkExpr expr
    | SynModuleDecl.Types(types, r) ->
      addIfInside r
      List.iter walkTypeDefn types
    | SynModuleDecl.Attributes(attributes = AllAttrs attrs; range = r) ->
      addIfInside r
      List.iter walkAttribute attrs
    | SynModuleDecl.ModuleAbbrev(ident, longId, r) -> addIfInside r
    | SynModuleDecl.Exception(_, r) -> addIfInside r
    | SynModuleDecl.Open(longDotId, r) -> addIfInside r
    | SynModuleDecl.HashDirective(_, r) -> addIfInside r

  match input with
  | ParsedInput.ImplFile input -> walkImplFileInput input
  | _ -> ()
  //debug "%A" idents
  result

module Completion =

  [<RequireQualifiedAccess>]
  type Context =
    | StringLiteral
    | Unknown
    | SynType

  let atPos (pos: Position, ast: ParsedInput) : Context =
    let visitor =
      { new SyntaxVisitorBase<Context>() with

          member x.VisitExpr(path, traverseExpr, defaultTraverse, expr) : Context option =
            if Range.rangeContainsPos expr.Range pos then
              match expr with
              | SynExpr.Const(SynConst.String _, _) -> Some Context.StringLiteral
              | SynExpr.InterpolatedString(parts, _, _) ->
                parts
                |> List.tryPick (function
                  | SynInterpolatedStringPart.String(s, m) when Range.rangeContainsPos m pos ->
                    Some Context.StringLiteral
                  | SynInterpolatedStringPart.String _ -> None
                  | SynInterpolatedStringPart.FillExpr(e, _) when Range.rangeContainsPos e.Range pos ->
                    defaultTraverse e // gotta dive into the expr to see if we're in a literal inside the expr
                  | SynInterpolatedStringPart.FillExpr _ -> None)
              | _ -> defaultTraverse expr
            else
              None

          member x.VisitType(path, defaultTraverse, synType) : Context option = Some Context.SynType }

    SyntaxTraversal.Traverse(pos, ast, visitor)
    |> Option.defaultValue Context.Unknown
