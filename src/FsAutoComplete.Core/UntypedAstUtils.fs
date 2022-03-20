/// Code from VisualFSharpPowerTools project: https://github.com/fsprojects/VisualFSharpPowerTools/blob/master/src/FSharp.Editing/Common/UntypedAstUtils.fs
module FsAutoComplete.UntypedAstUtils

open FSharp.Compiler.Syntax
open System.Collections.Generic
open FSharp.Compiler
open FSharp.Compiler.Text

type Range with
    member inline x.IsEmpty = x.StartColumn = x.EndColumn && x.StartLine = x.EndLine

type internal ShortIdent = string
type internal Idents = ShortIdent[]

let internal longIdentToArray (longIdent: LongIdent): Idents =
    longIdent |> Seq.map string |> Seq.toArray

/// An recursive pattern that collect all sequential expressions to avoid StackOverflowException
let rec (|Sequentials|_|) = function
    | SynExpr.Sequential(_, _, e, Sequentials es, _) ->
        Some(e::es)
    | SynExpr.Sequential(_, _, e1, e2, _) ->
        Some [e1; e2]
    | _ -> None

let (|ConstructorPats|) = function
    | SynArgPats.Pats ps -> ps
    | SynArgPats.NamePatPairs(xs, _) -> xs |> List.map (fun (_, _, pat) -> pat)

/// matches if the range contains the position
let (|ContainsPos|_|) pos range =
  if Range.rangeContainsPos range pos then Some () else None

/// Active pattern that matches an ident on a given name by the ident's `idText`
let (|Ident|_|) ofName =
  function | SynExpr.Ident ident when ident.idText = ofName -> Some ()
           | _ -> None

/// matches if the range contains the position
let (|IdentContainsPos|_|) pos (ident: Ident) =
  (|ContainsPos|_|) pos ident.idRange

/// A pattern that collects all attributes from a `SynAttributes` into a single flat list
let (|AllAttrs|) (attrs: SynAttributes) =
    attrs |> List.collect (fun attrList -> attrList.Attributes)

/// A pattern that collects all patterns from a `SynSimplePats` into a single flat list
let (|AllSimplePats|) (pats: SynSimplePats) =
    let rec loop acc pat =
        match pat with
        | SynSimplePats.SimplePats (pats,_) -> acc @ pats
        | SynSimplePats.Typed(pats,_,_) -> loop acc pats

    loop [] pats

/// Returns all Idents and LongIdents found in an untyped AST.
let internal getLongIdents (input: ParsedInput option) : IDictionary<Position, Idents> =
    let identsByEndPos = Dictionary<Position, Idents>()

    let addLongIdent (longIdent: LongIdent) =
        let idents = longIdentToArray longIdent
        for ident in longIdent do
            identsByEndPos.[ident.idRange.End] <- idents

    let addLongIdentWithDots (LongIdentWithDots (longIdent, lids) as value) =
        match longIdentToArray longIdent with
        | [||] -> ()
        | [|_|] as idents -> identsByEndPos.[value.Range.End] <- idents
        | idents ->
            for dotRange in lids do
                identsByEndPos.[Position.mkPos dotRange.EndLine (dotRange.EndColumn - 1)] <- idents
            identsByEndPos.[value.Range.End] <- idents

    let addIdent (ident: Ident) =
        identsByEndPos.[ident.idRange.End] <- [|ident.idText|]

    let rec walkImplFileInput (ParsedImplFileInput(_, _, _, _, _, moduleOrNamespaceList, _)) =
        List.iter walkSynModuleOrNamespace moduleOrNamespaceList

    and walkSynModuleOrNamespace (SynModuleOrNamespace(_, _, _, decls, _, AllAttrs attrs, _, _)) =
        List.iter walkAttribute attrs
        List.iter walkSynModuleDecl decls

    and walkAttribute (attr: SynAttribute) =
        addLongIdentWithDots attr.TypeName
        walkExpr attr.ArgExpr

    and walkTyparDecl (SynTyparDecl (attributes = AllAttrs attrs; Item2 = typar)) =
        List.iter walkAttribute attrs
        walkTypar typar

    and walkTypeConstraint = function
        | SynTypeConstraint.WhereTyparIsValueType (t, _)
        | SynTypeConstraint.WhereTyparIsReferenceType (t, _)
        | SynTypeConstraint.WhereTyparIsUnmanaged (t, _)
        | SynTypeConstraint.WhereTyparSupportsNull (t, _)
        | SynTypeConstraint.WhereTyparIsComparable (t, _)
        | SynTypeConstraint.WhereTyparIsEquatable (t, _) -> walkTypar t
        | SynTypeConstraint.WhereTyparDefaultsToType (t, ty, _)
        | SynTypeConstraint.WhereTyparSubtypeOfType (t, ty, _) -> walkTypar t; walkType ty
        | SynTypeConstraint.WhereTyparIsEnum (t, ts, _)
        | SynTypeConstraint.WhereTyparIsDelegate (t, ts, _) -> walkTypar t; List.iter walkType ts
        | SynTypeConstraint.WhereTyparSupportsMember (ts, sign, _) -> List.iter walkType ts; walkMemberSig sign

    and walkPat = function
        | SynPat.Tuple (_, pats, _)
        | SynPat.ArrayOrList (_, pats, _)
        | SynPat.Ands (pats, _) -> List.iter walkPat pats
        | SynPat.Named (ident, _, _, _) ->
            addIdent ident
        | SynPat.Typed (pat, t, _) ->
            walkPat pat
            walkType t
        | SynPat.Attrib (pat, AllAttrs attrs, _) ->
            walkPat pat
            List.iter walkAttribute attrs
        | SynPat.Or (lhsPat = pat1; rhsPat = pat2) -> List.iter walkPat [pat1; pat2]
        | SynPat.LongIdent (longDotId = ident; typarDecls = typars; argPats = ConstructorPats pats) ->
            addLongIdentWithDots ident
            typars
            |> Option.iter (fun (SynValTyparDecls (typars, _)) ->
                 match typars with
                 | None -> ()
                 | Some typars ->
                    typars.TyparDecls |> List.iter walkTyparDecl
                    typars.Constraints |> List.iter walkTypeConstraint
            )
            List.iter walkPat pats
        | SynPat.Paren (pat, _) -> walkPat pat
        | SynPat.IsInst (t, _) -> walkType t
        | SynPat.QuoteExpr(e, _) -> walkExpr e
        | _ -> ()

    and walkTypar (SynTypar (_, _, _)) = ()

    and walkBinding (SynBinding (attributes = AllAttrs attrs; headPat = pat; returnInfo = returnInfo; expr = e)) =
        List.iter walkAttribute attrs
        walkPat pat
        walkExpr e
        returnInfo |> Option.iter (fun (SynBindingReturnInfo (t, _, _)) -> walkType t)

    and walkInterfaceImpl (SynInterfaceImpl(bindings = bindings)) = List.iter walkBinding bindings

    and walkType = function
        | SynType.Array (_, t, _)
        | SynType.HashConstraint (t, _)
        | SynType.MeasurePower (t, _, _) -> walkType t
        | SynType.Fun (t1, t2, _)
        | SynType.MeasureDivide (t1, t2, _) -> walkType t1; walkType t2
        | SynType.LongIdent ident -> addLongIdentWithDots ident
        | SynType.App (ty, _, types, _, _, _, _) -> walkType ty; List.iter walkType types
        | SynType.LongIdentApp (_, _, _, types, _, _, _) -> List.iter walkType types
        | SynType.Tuple (_, ts, _) -> ts |> List.iter (fun (_, t) -> walkType t)
        | SynType.WithGlobalConstraints (t, typeConstraints, _) ->
            walkType t; List.iter walkTypeConstraint typeConstraints
        | _ -> ()

    and walkClause (SynMatchClause (pat = pat; whenExpr = e1; resultExpr = e2)) =
        walkPat pat
        walkExpr e2
        e1 |> Option.iter walkExpr

    and walkSimplePats = function
        | SynSimplePats.SimplePats (pats, _) -> List.iter walkSimplePat pats
        | SynSimplePats.Typed (pats, ty, _) ->
            walkSimplePats pats
            walkType ty

    and walkExpr = function
        | SynExpr.Paren (e, _, _, _)
        | SynExpr.Quote (_, _, e, _, _)
        | SynExpr.Typed (e, _, _)
        | SynExpr.InferredUpcast (e, _)
        | SynExpr.InferredDowncast (e, _)
        | SynExpr.AddressOf (_, e, _, _)
        | SynExpr.DoBang (e, _)
        | SynExpr.YieldOrReturn (_, e, _)
        | SynExpr.ArrayOrListComputed (_, e, _)
        | SynExpr.ComputationExpr (_, e, _)
        | SynExpr.Do (e, _)
        | SynExpr.Assert (e, _)
        | SynExpr.Lazy (e, _)
        | SynExpr.YieldOrReturnFrom (_, e, _) -> walkExpr e
        | SynExpr.Lambda (args = pats; body = e) ->
            walkSimplePats pats
            walkExpr e
        | SynExpr.New (_, t, e, _)
        | SynExpr.TypeTest (e, t, _)
        | SynExpr.Upcast (e, t, _)
        | SynExpr.Downcast (e, t, _) -> walkExpr e; walkType t
        | SynExpr.Tuple (_, es, _, _)
        | Sequentials es
        | SynExpr.ArrayOrList (_, es, _) -> List.iter walkExpr es
        | SynExpr.App (_, _, e1, e2, _)
        | SynExpr.TryFinally (tryExpr = e1; finallyExpr = e2)
        | SynExpr.While (_, e1, e2, _) -> List.iter walkExpr [e1; e2]
        | SynExpr.Record (_, _, fields, _) ->
            fields |> List.iter (fun (SynExprRecordField(fieldName = (ident, _); expr = e)) ->
                        addLongIdentWithDots ident
                        e |> Option.iter walkExpr)
        | SynExpr.Ident ident -> addIdent ident
        | SynExpr.ObjExpr(objType = ty; argOptions = argOpt; bindings = bindings; extraImpls = ifaces) ->
            argOpt |> Option.iter (fun (e, ident) ->
                walkExpr e
                ident |> Option.iter addIdent)
            walkType ty
            List.iter walkBinding bindings
            List.iter walkInterfaceImpl ifaces
        | SynExpr.LongIdent (_, ident, _, _) -> addLongIdentWithDots ident
        | SynExpr.For (ident = ident; identBody = e1; toBody = e2; doBody = e3) ->
            addIdent ident
            List.iter walkExpr [e1; e2; e3]
        | SynExpr.ForEach (pat = pat; enumExpr = e1; bodyExpr = e2) ->
            walkPat pat
            List.iter walkExpr [e1; e2]
        | SynExpr.MatchLambda (_, _, synMatchClauseList, _, _) ->
            List.iter walkClause synMatchClauseList
        | SynExpr.Match (expr = e; clauses = synMatchClauseList) ->
            walkExpr e
            List.iter walkClause synMatchClauseList
        | SynExpr.TypeApp (e, _, tys, _, _, _, _) ->
            List.iter walkType tys; walkExpr e
        | SynExpr.LetOrUse (bindings = bindings; body = e) ->
            List.iter walkBinding bindings; walkExpr e
        | SynExpr.TryWith (e, clauses, _, _, _, _) ->
            List.iter walkClause clauses;  walkExpr e
        | SynExpr.IfThenElse (ifExpr = e1; thenExpr = e2; elseExpr = e3) ->
            List.iter walkExpr [e1; e2]
            e3 |> Option.iter walkExpr
        | SynExpr.LongIdentSet (ident, e, _)
        | SynExpr.DotGet (e, _, ident, _) ->
            addLongIdentWithDots ident
            walkExpr e
        | SynExpr.DotSet (e1, idents, e2, _) ->
            walkExpr e1
            addLongIdentWithDots idents
            walkExpr e2
        | SynExpr.DotIndexedGet (e, args, _, _) ->
            walkExpr e
            walkExpr args
        | SynExpr.DotIndexedSet (e1, args, e2, _, _, _) ->
            walkExpr e1
            walkExpr args
            walkExpr e2
        | SynExpr.NamedIndexedPropertySet (ident, e1, e2, _) ->
            addLongIdentWithDots ident
            List.iter walkExpr [e1; e2]
        | SynExpr.DotNamedIndexedPropertySet (e1, ident, e2, e3, _) ->
            addLongIdentWithDots ident
            List.iter walkExpr [e1; e2; e3]
        | SynExpr.JoinIn (e1, _, e2, _) -> List.iter walkExpr [e1; e2]
        | SynExpr.LetOrUseBang (pat = pat; rhs = e1; andBangs = ands; body = e2) ->
            walkPat pat
            walkExpr e1
            for (SynExprAndBang(pat = pat; body = body)) in ands do
              walkPat pat
              walkExpr body
            walkExpr e2
        | SynExpr.TraitCall (ts, sign, e, _) ->
            List.iter walkTypar ts
            walkMemberSig sign
            walkExpr e
        | SynExpr.Const (SynConst.Measure(_, _, m), _) -> walkMeasure m
        | _ -> ()

    and walkMeasure = function
        | SynMeasure.Product (m1, m2, _)
        | SynMeasure.Divide (m1, m2, _) -> walkMeasure m1; walkMeasure m2
        | SynMeasure.Named (longIdent, _) -> addLongIdent longIdent
        | SynMeasure.Seq (ms, _) -> List.iter walkMeasure ms
        | SynMeasure.Power (m, _, _) -> walkMeasure m
        | SynMeasure.Var (ty, _) -> walkTypar ty
        | SynMeasure.One
        | SynMeasure.Anon _ -> ()

    and walkSimplePat = function
        | SynSimplePat.Attrib (pat, AllAttrs attrs, _) ->
            walkSimplePat pat
            List.iter walkAttribute attrs
        | SynSimplePat.Typed(pat, t, _) ->
            walkSimplePat pat
            walkType t
        | _ -> ()

    and walkField (SynField(attributes = AllAttrs attrs; fieldType = t)) =
        List.iter walkAttribute attrs
        walkType t

    and walkValSig (SynValSig(attributes = AllAttrs attrs; synType =  t; arity = SynValInfo(argInfos, argInfo))) =
        List.iter walkAttribute attrs
        walkType t
        argInfo :: (argInfos |> List.concat)
        |> List.collect (fun (SynArgInfo(attributes = AllAttrs attrs)) -> attrs)
        |> List.iter walkAttribute

    and walkMemberSig = function
        | SynMemberSig.Inherit (t, _)
        | SynMemberSig.Interface(t, _) -> walkType t
        | SynMemberSig.Member(vs, _, _) -> walkValSig vs
        | SynMemberSig.ValField(f, _) -> walkField f
        | SynMemberSig.NestedType(SynTypeDefnSig (typeInfo = info; typeRepr = repr; members = memberSigs), _) ->
            let isTypeExtensionOrAlias =
                match repr with
                | SynTypeDefnSigRepr.Simple(SynTypeDefnSimpleRepr.TypeAbbrev _, _)
                | SynTypeDefnSigRepr.ObjectModel(SynTypeDefnKind.Abbrev, _, _)
                | SynTypeDefnSigRepr.ObjectModel(SynTypeDefnKind.Augmentation _, _, _) -> true
                | _ -> false
            walkComponentInfo isTypeExtensionOrAlias info
            walkTypeDefnSigRepr repr
            List.iter walkMemberSig memberSigs

    and walkMember = function
        | SynMemberDefn.AbstractSlot (valSig, _, _) -> walkValSig valSig
        | SynMemberDefn.Member (binding, _) -> walkBinding binding
        | SynMemberDefn.ImplicitCtor (_, AllAttrs attrs, AllSimplePats pats, _, _, _) ->
            List.iter walkAttribute attrs
            List.iter walkSimplePat pats
        | SynMemberDefn.ImplicitInherit (t, e, _, _) -> walkType t; walkExpr e
        | SynMemberDefn.LetBindings (bindings, _, _, _) -> List.iter walkBinding bindings
        | SynMemberDefn.Interface (interfaceType = t; members = members) ->
            walkType t
            members |> Option.iter (List.iter walkMember)
        | SynMemberDefn.Inherit (t, _, _) -> walkType t
        | SynMemberDefn.ValField (field, _) -> walkField field
        | SynMemberDefn.NestedType (tdef, _, _) -> walkTypeDefn tdef
        | SynMemberDefn.AutoProperty (attributes = AllAttrs attrs; typeOpt = t; synExpr = e) ->
            List.iter walkAttribute attrs
            Option.iter walkType t
            walkExpr e
        | _ -> ()

    and walkEnumCase (SynEnumCase(attributes = AllAttrs attrs)) = List.iter walkAttribute attrs

    and walkUnionCaseType = function
        | SynUnionCaseKind.Fields fields -> List.iter walkField fields
        | SynUnionCaseKind.FullType (t, _) -> walkType t

    and walkUnionCase (SynUnionCase (attributes = AllAttrs attrs; caseType = t)) =
        List.iter walkAttribute attrs
        walkUnionCaseType t

    and walkTypeDefnSimple = function
        | SynTypeDefnSimpleRepr.Enum (cases, _) -> List.iter walkEnumCase cases
        | SynTypeDefnSimpleRepr.Union (_, cases, _) -> List.iter walkUnionCase cases
        | SynTypeDefnSimpleRepr.Record (_, fields, _) -> List.iter walkField fields
        | SynTypeDefnSimpleRepr.TypeAbbrev (_, t, _) -> walkType t
        | _ -> ()

    and walkTyparDecls (typars: SynTyparDecls) =
        typars.TyparDecls |> List.iter walkTyparDecl
        typars.Constraints |> List.iter walkTypeConstraint

    and walkComponentInfo isTypeExtensionOrAlias (SynComponentInfo(attributes = AllAttrs attrs; typeParams = typars; longId = longIdent)) =
        List.iter walkAttribute attrs
        Option.iter walkTyparDecls typars
        if isTypeExtensionOrAlias then
            addLongIdent longIdent

    and walkTypeDefnRepr = function
        | SynTypeDefnRepr.ObjectModel (_, defns, _) -> List.iter walkMember defns
        | SynTypeDefnRepr.Simple(defn, _) -> walkTypeDefnSimple defn
        | SynTypeDefnRepr.Exception _ -> ()

    and walkTypeDefnSigRepr = function
        | SynTypeDefnSigRepr.ObjectModel (_, defns, _) -> List.iter walkMemberSig defns
        | SynTypeDefnSigRepr.Simple(defn, _) -> walkTypeDefnSimple defn
        | SynTypeDefnSigRepr.Exception _ -> ()

    and walkTypeDefn (SynTypeDefn (info, repr, members, implicitCtor, _, _)) =
        let isTypeExtensionOrAlias =
            match repr with
            | SynTypeDefnRepr.ObjectModel (SynTypeDefnKind.Augmentation _, _, _)
            | SynTypeDefnRepr.ObjectModel (SynTypeDefnKind.Abbrev, _, _)
            | SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.TypeAbbrev _, _) -> true
            | _ -> false
        walkComponentInfo isTypeExtensionOrAlias info
        walkTypeDefnRepr repr
        Option.iter walkMember implicitCtor
        List.iter walkMember members

    and walkSynModuleDecl (decl: SynModuleDecl) =
        match decl with
        | SynModuleDecl.NamespaceFragment fragment -> walkSynModuleOrNamespace fragment
        | SynModuleDecl.NestedModule (moduleInfo = info; decls = modules) ->
            walkComponentInfo false info
            List.iter walkSynModuleDecl modules
        | SynModuleDecl.Let (_, bindings, _) -> List.iter walkBinding bindings
        | SynModuleDecl.DoExpr (_, expr, _) -> walkExpr expr
        | SynModuleDecl.Types (types, _) -> List.iter walkTypeDefn types
        | SynModuleDecl.Attributes (attributes = AllAttrs attrs) -> List.iter walkAttribute attrs
        | _ -> ()

    match input with
    | Some (ParsedInput.ImplFile input) ->
         walkImplFileInput input
    | _ -> ()
    //debug "%A" idents
    identsByEndPos :> _

/// Checks if given position is part of the typed binding
let internal isTypedBindingAtPosition (input: ParsedInput) (r: Range) : bool =
    let mutable result = false

    let isInside (ran : Range) =
        Range.rangeContainsRange ran r

    let rec walkImplFileInput (ParsedImplFileInput(_, _, _, _, _, moduleOrNamespaceList, _)) =
        List.iter walkSynModuleOrNamespace moduleOrNamespaceList

    and walkSynModuleOrNamespace (SynModuleOrNamespace(_, _, _, decls, _, AllAttrs attrs, _, _)) =
        List.iter walkAttribute attrs
        List.iter walkSynModuleDecl decls

    and walkAttribute (attr: SynAttribute) =
        walkExpr attr.ArgExpr

    and walkTyparDecl (SynTyparDecl (attributes = AllAttrs attrs; Item2 = typar)) =
        List.iter walkAttribute attrs
        walkTypar typar

    and walkTyparDecls (typars: SynTyparDecls) =
        typars.TyparDecls |> List.iter walkTyparDecl
        typars.Constraints |> List.iter walkTypeConstraint

    and walkSynValTyparDecls (SynValTyparDecls(typars, _)) =
        Option.iter walkTyparDecls typars

    and walkTypeConstraint = function
        | SynTypeConstraint.WhereTyparIsValueType (t, _)
        | SynTypeConstraint.WhereTyparIsReferenceType (t, _)
        | SynTypeConstraint.WhereTyparIsUnmanaged (t, _)
        | SynTypeConstraint.WhereTyparSupportsNull (t, _)
        | SynTypeConstraint.WhereTyparIsComparable (t, _)
        | SynTypeConstraint.WhereTyparIsEquatable (t, _) -> walkTypar t
        | SynTypeConstraint.WhereTyparDefaultsToType (t, ty, _)
        | SynTypeConstraint.WhereTyparSubtypeOfType (t, ty, _) -> walkTypar t; walkType ty
        | SynTypeConstraint.WhereTyparIsEnum (t, ts, _)
        | SynTypeConstraint.WhereTyparIsDelegate (t, ts, _) -> walkTypar t; List.iter walkType ts
        | SynTypeConstraint.WhereTyparSupportsMember (ts, sign, _) -> List.iter walkType ts; walkMemberSig sign

    and walkPat = function
        | SynPat.Tuple (_, pats, _)
        | SynPat.ArrayOrList (_, pats, _)
        | SynPat.Ands (pats, _) -> List.iter walkPat pats
        | SynPat.Named (ident, _, _, _) -> ()
        | SynPat.Typed (pat, t, ran) ->
            if isInside ran then result <- true
            walkPat pat
            walkType t
        | SynPat.Attrib (pat, AllAttrs attrs, _) ->
            walkPat pat
            List.iter walkAttribute attrs
        | SynPat.Or (lhsPat = pat1; rhsPat = pat2) -> List.iter walkPat [pat1; pat2]
        | SynPat.LongIdent (longDotId = ident; typarDecls = typars; argPats = ConstructorPats pats) ->
            Option.iter walkSynValTyparDecls typars
            List.iter walkPat pats
        | SynPat.Paren (pat, _) -> walkPat pat
        | SynPat.IsInst (t, _) -> walkType t
        | SynPat.QuoteExpr(e, _) -> walkExpr e
        | _ -> ()

    and walkTypar (SynTypar (_, _, _)) = ()

    and walkBinding (SynBinding (attributes = AllAttrs attrs; headPat = pat; returnInfo = returnInfo; expr = e)) =
        List.iter walkAttribute attrs
        walkPat pat
        walkExpr e
        returnInfo |> Option.iter (fun (SynBindingReturnInfo (t, _, _)) -> walkType t)

    and walkInterfaceImpl (SynInterfaceImpl(bindings = bindings)) = List.iter walkBinding bindings

    and walkType = function
        | SynType.Array (_, t, _)
        | SynType.HashConstraint (t, _)
        | SynType.MeasurePower (t, _, _) -> walkType t
        | SynType.Fun (t1, t2, _)
        | SynType.MeasureDivide (t1, t2, _) -> walkType t1; walkType t2
        | SynType.App (ty, _, types, _, _, _, _) -> walkType ty; List.iter walkType types
        | SynType.LongIdentApp (_, _, _, types, _, _, _) -> List.iter walkType types
        | SynType.Tuple (_, ts, _) -> ts |> List.iter (fun (_, t) -> walkType t)
        | SynType.WithGlobalConstraints (t, typeConstraints, _) ->
            walkType t; List.iter walkTypeConstraint typeConstraints
        | _ -> ()

    and walkClause (SynMatchClause (pat = pat; whenExpr = e1; resultExpr = e2)) =
        walkPat pat
        walkExpr e2
        e1 |> Option.iter walkExpr

    and walkSimplePats = function
        | SynSimplePats.SimplePats (pats, _) -> List.iter walkSimplePat pats
        | SynSimplePats.Typed (pats, ty, ran) ->
            if isInside ran then result <- true
            walkSimplePats pats
            walkType ty

    and walkExpr = function
        | SynExpr.Typed (e, _, ran) ->
            if isInside ran then result <- true
            walkExpr e
        | SynExpr.Paren (e, _, _, _)
        | SynExpr.Quote (_, _, e, _, _)
        | SynExpr.InferredUpcast (e, _)
        | SynExpr.InferredDowncast (e, _)
        | SynExpr.AddressOf (_, e, _, _)
        | SynExpr.DoBang (e, _)
        | SynExpr.YieldOrReturn (_, e, _)
        | SynExpr.ArrayOrListComputed (_, e, _)
        | SynExpr.ComputationExpr (_, e, _)
        | SynExpr.Do (e, _)
        | SynExpr.Assert (e, _)
        | SynExpr.Lazy (e, _)
        | SynExpr.YieldOrReturnFrom (_, e, _) -> walkExpr e
        | SynExpr.Lambda (args = pats; body = e) ->
            walkSimplePats pats
            walkExpr e
        | SynExpr.New (_, t, e, _)
        | SynExpr.TypeTest (e, t, _)
        | SynExpr.Upcast (e, t, _)
        | SynExpr.Downcast (e, t, _) -> walkExpr e; walkType t
        | SynExpr.Tuple (_, es, _, _)
        | Sequentials es
        | SynExpr.ArrayOrList (_, es, _) -> List.iter walkExpr es
        | SynExpr.App (funcExpr = e1; argExpr = e2)
        | SynExpr.TryFinally (tryExpr = e1; finallyExpr = e2)
        | SynExpr.While (_, e1, e2, _) -> List.iter walkExpr [e1; e2]
        | SynExpr.Record (_, _, fields, _) ->
            fields |> List.iter (fun (SynExprRecordField((ident, _), _, e, _)) ->
                        e |> Option.iter walkExpr)
        | SynExpr.ObjExpr(ty, argOpt, _, bindings, _, ifaces, _, _) ->
            argOpt |> Option.iter (fun (e, ident) ->
                walkExpr e)
            walkType ty
            List.iter walkBinding bindings
            List.iter walkInterfaceImpl ifaces
        | SynExpr.For (_, _, ident, _, e1, _, e2, e3, _) ->
            List.iter walkExpr [e1; e2; e3]
        | SynExpr.ForEach (_, _, _, _, pat, e1, e2, _) ->
            walkPat pat
            List.iter walkExpr [e1; e2]
        | SynExpr.MatchLambda (_, _, synMatchClauseList, _, _) ->
            List.iter walkClause synMatchClauseList
        | SynExpr.Match (_, _, e, _, synMatchClauseList, _) ->
            walkExpr e
            List.iter walkClause synMatchClauseList
        | SynExpr.TypeApp (e, _, tys, _, _, _, _) ->
            List.iter walkType tys; walkExpr e
        | SynExpr.LetOrUse (_, _, bindings, e, _, _) ->
            List.iter walkBinding bindings; walkExpr e
        | SynExpr.TryWith (e, clauses, _, _, _, _) ->
            List.iter walkClause clauses;  walkExpr e
        | SynExpr.IfThenElse (e1, e2, e3, _, _, _, _) ->
            List.iter walkExpr [e1; e2]
            e3 |> Option.iter walkExpr
        | SynExpr.LongIdentSet (ident, e, _)
        | SynExpr.DotGet (e, _, ident, _) ->
            walkExpr e
        | SynExpr.DotSet (e1, idents, e2, _) ->
            walkExpr e1
            walkExpr e2
        | SynExpr.DotIndexedGet (e, args, _, _) ->
            walkExpr e
            walkExpr args
        | SynExpr.DotIndexedSet (e1, args, e2, _, _, _) ->
            walkExpr e1
            walkExpr args
            walkExpr e2
        | SynExpr.NamedIndexedPropertySet (ident, e1, e2, _) ->
            List.iter walkExpr [e1; e2]
        | SynExpr.DotNamedIndexedPropertySet (e1, ident, e2, e3, _) ->
            List.iter walkExpr [e1; e2; e3]
        | SynExpr.JoinIn (e1, _, e2, _) -> List.iter walkExpr [e1; e2]
        | SynExpr.LetOrUseBang (_, _, _, pat, _, e1, ands, e2, _) ->
            walkPat pat
            walkExpr e1
            for (SynExprAndBang(_,_,_,pat,body,_,_)) in ands do
              walkPat pat
              walkExpr body
            walkExpr e2
        | SynExpr.TraitCall (ts, sign, e, _) ->
            List.iter walkTypar ts
            walkMemberSig sign
            walkExpr e
        | SynExpr.Const (SynConst.Measure(_, _, m), _) -> walkMeasure m
        | _ -> ()

    and walkMeasure = function
        | SynMeasure.Product (m1, m2, _)
        | SynMeasure.Divide (m1, m2, _) -> walkMeasure m1; walkMeasure m2
        | SynMeasure.Named (longIdent, _) -> ()
        | SynMeasure.Seq (ms, _) -> List.iter walkMeasure ms
        | SynMeasure.Power (m, _, _) -> walkMeasure m
        | SynMeasure.Var (ty, _) -> walkTypar ty
        | SynMeasure.One
        | SynMeasure.Anon _ -> ()

    and walkSimplePat = function
        | SynSimplePat.Attrib (pat, AllAttrs attrs, _) ->
            walkSimplePat pat
            List.iter walkAttribute attrs
        | SynSimplePat.Typed(pat, t, ran) ->
            if isInside ran then result <- true
            walkSimplePat pat
            walkType t
        | _ -> ()

    and walkField (SynField(attributes = AllAttrs attrs; fieldType = t)) =
        List.iter walkAttribute attrs
        walkType t

    and walkValSig (SynValSig(attributes = AllAttrs attrs; synType = t; arity = SynValInfo(argInfos, argInfo))) =
        List.iter walkAttribute attrs
        walkType t
        argInfo :: (argInfos |> List.concat)
        |> List.collect (fun (SynArgInfo(attributes = AllAttrs attrs)) -> attrs)
        |> List.iter walkAttribute

    and walkMemberSig = function
        | SynMemberSig.Inherit (t, _)
        | SynMemberSig.Interface(t, _) -> walkType t
        | SynMemberSig.Member(vs, _, _) -> walkValSig vs
        | SynMemberSig.ValField(f, _) -> walkField f
        | SynMemberSig.NestedType(SynTypeDefnSig (typeInfo = info; typeRepr = repr; members = memberSigs), _) ->
            let isTypeExtensionOrAlias =
                match repr with
                | SynTypeDefnSigRepr.Simple(SynTypeDefnSimpleRepr.TypeAbbrev _, _)
                | SynTypeDefnSigRepr.ObjectModel(SynTypeDefnKind.Abbrev, _, _)
                | SynTypeDefnSigRepr.ObjectModel(SynTypeDefnKind.Augmentation _, _, _) -> true
                | _ -> false
            walkComponentInfo isTypeExtensionOrAlias info
            walkTypeDefnSigRepr repr
            List.iter walkMemberSig memberSigs

    and walkMember = function
        | SynMemberDefn.AbstractSlot (valSig, _, _) -> walkValSig valSig
        | SynMemberDefn.Member (binding, _) -> walkBinding binding
        | SynMemberDefn.ImplicitCtor (_, AllAttrs attrs, AllSimplePats pats, _, _, _) ->
            List.iter walkAttribute attrs
            List.iter walkSimplePat pats
        | SynMemberDefn.ImplicitInherit (t, e, _, _) -> walkType t; walkExpr e
        | SynMemberDefn.LetBindings (bindings, _, _, _) -> List.iter walkBinding bindings
        | SynMemberDefn.Interface (t, _, members, _) ->
            walkType t
            members |> Option.iter (List.iter walkMember)
        | SynMemberDefn.Inherit (t, _, _) -> walkType t
        | SynMemberDefn.ValField (field, _) -> walkField field
        | SynMemberDefn.NestedType (tdef, _, _) -> walkTypeDefn tdef
        | SynMemberDefn.AutoProperty (attributes = AllAttrs attrs; typeOpt = t; synExpr = e) ->
            List.iter walkAttribute attrs
            Option.iter walkType t
            walkExpr e
        | _ -> ()

    and walkEnumCase (SynEnumCase(attributes = AllAttrs attrs)) = List.iter walkAttribute attrs

    and walkUnionCaseType = function
        | SynUnionCaseKind.Fields fields -> List.iter walkField fields
        | SynUnionCaseKind.FullType (t, _) -> walkType t

    and walkUnionCase (SynUnionCase (attributes = AllAttrs attrs; caseType = t)) =
        List.iter walkAttribute attrs
        walkUnionCaseType t

    and walkTypeDefnSimple = function
        | SynTypeDefnSimpleRepr.Enum (cases, _) -> List.iter walkEnumCase cases
        | SynTypeDefnSimpleRepr.Union (_, cases, _) -> List.iter walkUnionCase cases
        | SynTypeDefnSimpleRepr.Record (_, fields, _) -> List.iter walkField fields
        | SynTypeDefnSimpleRepr.TypeAbbrev (_, t, _) -> walkType t
        | _ -> ()

    and walkComponentInfo isTypeExtensionOrAlias (SynComponentInfo(attributes = AllAttrs attrs; typeParams = typars; constraints = constraints; longId = longIdent)) =
        List.iter walkAttribute attrs
        Option.iter walkTyparDecls typars
        List.iter walkTypeConstraint constraints

    and walkTypeDefnRepr = function
        | SynTypeDefnRepr.ObjectModel (_, defns, _) -> List.iter walkMember defns
        | SynTypeDefnRepr.Simple(defn, _) -> walkTypeDefnSimple defn
        | SynTypeDefnRepr.Exception _ -> ()

    and walkTypeDefnSigRepr = function
        | SynTypeDefnSigRepr.ObjectModel (_, defns, _) -> List.iter walkMemberSig defns
        | SynTypeDefnSigRepr.Simple(defn, _) -> walkTypeDefnSimple defn
        | SynTypeDefnSigRepr.Exception _ -> ()

    and walkTypeDefn (SynTypeDefn (info, repr, members, implicitCtor, _, _)) =
        let isTypeExtensionOrAlias =
            match repr with
            | SynTypeDefnRepr.ObjectModel (SynTypeDefnKind.Augmentation _, _, _)
            | SynTypeDefnRepr.ObjectModel (SynTypeDefnKind.Abbrev, _, _)
            | SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.TypeAbbrev _, _) -> true
            | _ -> false
        walkComponentInfo isTypeExtensionOrAlias info
        walkTypeDefnRepr repr
        Option.iter walkMember implicitCtor
        List.iter walkMember members

    and walkSynModuleDecl (decl: SynModuleDecl) =
        match decl with
        | SynModuleDecl.NamespaceFragment fragment -> walkSynModuleOrNamespace fragment
        | SynModuleDecl.NestedModule (info, _, modules, _, _, _) ->
            walkComponentInfo false info
            List.iter walkSynModuleDecl modules
        | SynModuleDecl.Let (_, bindings, _) -> List.iter walkBinding bindings
        | SynModuleDecl.DoExpr (_, expr, _) -> walkExpr expr
        | SynModuleDecl.Types (types, _) -> List.iter walkTypeDefn types
        | SynModuleDecl.Attributes (attributes = AllAttrs attrs) -> List.iter walkAttribute attrs
        | _ -> ()

    match input with
    | ParsedInput.ImplFile input ->
         walkImplFileInput input
    | _ -> ()
    //debug "%A" idents
    result

/// Gives all ranges for current position
let internal getRangesAtPosition input (r: Position) : Range list =
    let mutable result = []


    let addIfInside (ran : Range) =
        let addToResult r =
            result <- r::result

        let isInside (ran : Range) =
            Range.rangeContainsPos ran r

        if isInside ran then addToResult ran



    let rec walkImplFileInput (ParsedImplFileInput(_, _, _, _, _, moduleOrNamespaceList, _)) =
        List.iter walkSynModuleOrNamespace moduleOrNamespaceList

    and walkSynModuleOrNamespace (SynModuleOrNamespace(_, _, _, decls, _, AllAttrs attrs, _, r)) =
        addIfInside r
        List.iter walkAttribute attrs
        List.iter walkSynModuleDecl decls

    and walkAttribute (attr: SynAttribute) =
        addIfInside attr.Range
        walkExpr attr.ArgExpr

    and walkTyparDecl (SynTyparDecl (attributes = AllAttrs attrs; Item2 = typar)) =
        List.iter walkAttribute attrs
        walkTypar typar

    and walkTyparDecls (typars: SynTyparDecls) =
        typars.TyparDecls |> List.iter walkTyparDecl
        typars.Constraints |> List.iter walkTypeConstraint

    and walkSynValTyparDecls (SynValTyparDecls(typars, _)) =
        Option.iter walkTyparDecls typars

    and walkTypeConstraint = function
        | SynTypeConstraint.WhereTyparIsValueType (t, r)
        | SynTypeConstraint.WhereTyparIsReferenceType (t, r)
        | SynTypeConstraint.WhereTyparIsUnmanaged (t, r)
        | SynTypeConstraint.WhereTyparSupportsNull (t, r)
        | SynTypeConstraint.WhereTyparIsComparable (t, r)
        | SynTypeConstraint.WhereTyparIsEquatable (t, r) ->
            addIfInside r
            walkTypar t
        | SynTypeConstraint.WhereTyparDefaultsToType (t, ty, r)
        | SynTypeConstraint.WhereTyparSubtypeOfType (t, ty, r) ->
            addIfInside r
            walkTypar t; walkType ty
        | SynTypeConstraint.WhereTyparIsEnum (t, ts, r)
        | SynTypeConstraint.WhereTyparIsDelegate (t, ts, r) ->
            addIfInside r
            walkTypar t; List.iter walkType ts
        | SynTypeConstraint.WhereTyparSupportsMember (ts, sign, r) ->
            addIfInside r
            List.iter walkType ts; walkMemberSig sign

    and walkPat = function
        | SynPat.Tuple (_, pats, r)
        | SynPat.ArrayOrList (_, pats, r)
        | SynPat.Ands (pats, r) ->
            addIfInside r
            List.iter walkPat pats
        | SynPat.Named (ident, _, _, r) ->
            addIfInside r
        | SynPat.Typed (pat, t, r) ->
            addIfInside r
            walkPat pat
            walkType t
        | SynPat.Attrib (pat, AllAttrs attrs, r) ->
            addIfInside r
            walkPat pat
            List.iter walkAttribute attrs
        | SynPat.Or (pat1, pat2, r, _) ->
            addIfInside r
            List.iter walkPat [pat1; pat2]
        | SynPat.LongIdent (ident, _, _, typars, ConstructorPats pats, _, r) ->
            addIfInside r
            Option.iter walkSynValTyparDecls typars
            List.iter walkPat pats
        | SynPat.Paren (pat, r) ->
            addIfInside r
            walkPat pat
        | SynPat.IsInst (t, r) ->
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
        | SynPat.FromParseError(_, r) ->addIfInside r
        | SynPat.As(lpat, rpat, r) ->
            addIfInside r
            walkPat lpat
            walkPat rpat

    and walkTypar (SynTypar (_, _, _)) = ()

    and walkBinding (SynBinding (attributes = AllAttrs attrs; headPat = pat; returnInfo = returnInfo; expr = e; range = r)) =
        addIfInside r
        List.iter walkAttribute attrs
        walkPat pat
        walkExpr e
        returnInfo |> Option.iter (fun (SynBindingReturnInfo (t, r, _)) -> addIfInside r; walkType t)

    and walkInterfaceImpl (SynInterfaceImpl(bindings = bindings; range = r)) =
        addIfInside r
        List.iter walkBinding bindings

    and walkType = function
        | SynType.Array (_, t, r)
        | SynType.HashConstraint (t, r)
        | SynType.MeasurePower (t, _, r) ->
            addIfInside r
            walkType t
        | SynType.Fun (t1, t2, r)
        | SynType.MeasureDivide (t1, t2, r) ->
            addIfInside r
            walkType t1; walkType t2
        | SynType.App (ty, _, types, _, _, _, r) ->
            addIfInside r
            walkType ty; List.iter walkType types
        | SynType.LongIdentApp (_, _, _, types, _, _, r) ->
            addIfInside r
            List.iter walkType types
        | SynType.Tuple (_, ts, r) ->
            addIfInside r
            ts |> List.iter (fun (_, t) -> walkType t)
        | SynType.WithGlobalConstraints (t, typeConstraints, r) ->
            addIfInside r
            walkType t; List.iter walkTypeConstraint typeConstraints
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


    and walkClause (SynMatchClause (pat, e1, e2, r, _, _)) =
        addIfInside r
        walkPat pat
        walkExpr e2
        e1 |> Option.iter walkExpr

    and walkSimplePats = function
        | SynSimplePats.SimplePats (pats, r) ->
            addIfInside r
            List.iter walkSimplePat pats
        | SynSimplePats.Typed (pats, ty, r) ->
            addIfInside r
            walkSimplePats pats
            walkType ty
    and walkInterpolatedStringPart = function
      | SynInterpolatedStringPart.FillExpr(expr, ident) ->
        ident |> Option.iter (fun ident -> addIfInside ident.idRange)
        walkExpr expr
      | SynInterpolatedStringPart.String (s, r) ->
        addIfInside r

    and walkExpr = function
        | SynExpr.Typed (e, _, r) ->
            addIfInside r
            walkExpr e
        | SynExpr.Paren (e, _, _, r)
        | SynExpr.Quote (_, _, e, _, r)
        | SynExpr.InferredUpcast (e, r)
        | SynExpr.InferredDowncast (e, r)
        | SynExpr.AddressOf (_, e, _, r)
        | SynExpr.DoBang (e, r)
        | SynExpr.YieldOrReturn (_, e, r)
        | SynExpr.ArrayOrListComputed (_, e, r)
        | SynExpr.ComputationExpr (_, e, r)
        | SynExpr.Do (e, r)
        | SynExpr.Assert (e, r)
        | SynExpr.Lazy (e, r)
        | SynExpr.YieldOrReturnFrom (_, e, r) ->
            addIfInside r
            walkExpr e
        | SynExpr.SequentialOrImplicitYield(_, e1, e2, ifNotE, r) ->
            addIfInside r
            walkExpr e1
            walkExpr e2
            walkExpr ifNotE
        | SynExpr.Lambda (args = pats; body = e; range = r) ->
            addIfInside r
            walkSimplePats pats
            walkExpr e
        | SynExpr.New (_, t, e, r)
        | SynExpr.TypeTest (e, t, r)
        | SynExpr.Upcast (e, t, r)
        | SynExpr.Downcast (e, t, r) ->
            addIfInside r
            walkExpr e; walkType t
        | SynExpr.Tuple (_, es, _, _)
        | Sequentials es -> List.iter walkExpr es //TODO??
        | SynExpr.ArrayOrList (_, es, r) ->
            addIfInside r
            List.iter walkExpr es
        | SynExpr.App (_, _, e1, e2, r)
        | SynExpr.TryFinally (e1, e2, r, _, _, _)
        | SynExpr.While (_, e1, e2, r) ->
            addIfInside r
            List.iter walkExpr [e1; e2]
        | SynExpr.Record (_, _, fields, r) ->
            addIfInside r
            fields |> List.iter (fun (SynExprRecordField(fieldName = (ident, _); expr = e)) -> e |> Option.iter walkExpr)
        | SynExpr.ObjExpr(ty, argOpt, _, bindings, _, ifaces, _, r) ->
            addIfInside r
            argOpt |> Option.iter (fun (e, ident) ->
                walkExpr e)
            walkType ty
            List.iter walkBinding bindings
            List.iter walkInterfaceImpl ifaces
        | SynExpr.For (identBody = e1; toBody = e2; doBody = e3; range = r) ->
            addIfInside r
            List.iter walkExpr [e1; e2; e3]
        | SynExpr.ForEach (_, _, _, _, pat, e1, e2, r) ->
            addIfInside r
            walkPat pat
            List.iter walkExpr [e1; e2]
        | SynExpr.MatchLambda (_, _, synMatchClauseList, _, r) ->
            addIfInside r
            List.iter walkClause synMatchClauseList
        | SynExpr.Match (_, _, e, _, synMatchClauseList, r) ->
            addIfInside r
            walkExpr e
            List.iter walkClause synMatchClauseList
        | SynExpr.TypeApp (e, _, tys, _, _, tr, r) ->
            addIfInside tr
            addIfInside r
            List.iter walkType tys; walkExpr e
        | SynExpr.LetOrUse (bindings = bindings; body = e; range = r) ->
            addIfInside r
            List.iter walkBinding bindings; walkExpr e
        | SynExpr.TryWith (tryExpr = e; withCases = clauses; range = r) ->
            addIfInside r
            List.iter walkClause clauses;  walkExpr e
        | SynExpr.IfThenElse (ifExpr = e1; thenExpr = e2; elseExpr = e3; range = r) ->
            addIfInside r
            List.iter walkExpr [e1; e2]
            e3 |> Option.iter walkExpr
        | SynExpr.LongIdentSet (ident, e, r)
        | SynExpr.DotGet (e, _, ident, r) ->
            addIfInside r
            walkExpr e
        | SynExpr.DotSet (e1, idents, e2, r) ->
            addIfInside r
            walkExpr e1
            walkExpr e2
        | SynExpr.DotIndexedGet (e, args, _, r) ->
            addIfInside r
            walkExpr e
            walkExpr args
        | SynExpr.DotIndexedSet (e1, args, e2, _, _, r) ->
            addIfInside r
            walkExpr e1
            walkExpr args
            walkExpr e2
        | SynExpr.NamedIndexedPropertySet (ident, e1, e2, r) ->
            addIfInside r
            List.iter walkExpr [e1; e2]
        | SynExpr.DotNamedIndexedPropertySet (e1, ident, e2, e3, r) ->
            addIfInside r
            List.iter walkExpr [e1; e2; e3]
        | SynExpr.JoinIn (e1, _, e2, r) ->
            addIfInside r
            List.iter walkExpr [e1; e2]
        | SynExpr.LetOrUseBang (pat =  pat; rhs = e1; andBangs = ands; body = e2; range = r) ->
            addIfInside r
            walkPat pat
            walkExpr e1
            for (SynExprAndBang(pat = pat; body = body; range = r)) in ands do
              addIfInside r
              walkPat pat
              walkExpr body
            walkExpr e2
        | SynExpr.TraitCall (ts, sign, e, r) ->
            addIfInside r
            List.iter walkTypar ts
            walkMemberSig sign
            walkExpr e
        | SynExpr.Const (SynConst.Measure(_, _, m), r) ->
            addIfInside r
            walkMeasure m
        | SynExpr.Const (_, r) ->
            addIfInside r
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
        | SynExpr.DebugPoint(innerExpr = expr) ->
            walkExpr expr

    and walkMeasure = function
        | SynMeasure.Product (m1, m2, r)
        | SynMeasure.Divide (m1, m2, r) ->
            addIfInside r
            walkMeasure m1; walkMeasure m2
        | SynMeasure.Named (longIdent, r) -> addIfInside r
        | SynMeasure.Seq (ms, r) ->
            addIfInside r
            List.iter walkMeasure ms
        | SynMeasure.Power (m, _, r) ->
            addIfInside r
            walkMeasure m
        | SynMeasure.Var (ty, r) ->
            addIfInside r
            walkTypar ty
        | SynMeasure.One
        | SynMeasure.Anon _ -> ()

    and walkSimplePat = function
        | SynSimplePat.Attrib (pat, AllAttrs attrs, r) ->
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

    and walkValSig (SynValSig(attributes = AllAttrs attrs; synType = t; arity = SynValInfo(argInfos, argInfo); range = r)) =
        addIfInside r
        List.iter walkAttribute attrs
        walkType t
        argInfo :: (argInfos |> List.concat)
        |> List.collect (fun (SynArgInfo(attributes = AllAttrs attrs)) -> attrs)
        |> List.iter walkAttribute

    and walkMemberSig = function
        | SynMemberSig.Inherit (t, r)
        | SynMemberSig.Interface(t, r) ->
            addIfInside r
            walkType t
        | SynMemberSig.Member(vs, _, r) ->
            addIfInside r
            walkValSig vs
        | SynMemberSig.ValField(f, r) ->
            addIfInside r
            walkField f
        | SynMemberSig.NestedType(SynTypeDefnSig (typeInfo = info; typeRepr = repr; members = memberSigs), r) ->
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

    and walkMember = function
        | SynMemberDefn.AbstractSlot (valSig, _, r) ->
            addIfInside r
            walkValSig valSig
        | SynMemberDefn.Member (binding, r) ->
            addIfInside r
            walkBinding binding
        | SynMemberDefn.ImplicitCtor (_, AllAttrs attrs, AllSimplePats pats, _, _, r) ->
            addIfInside r
            List.iter walkAttribute attrs
            List.iter walkSimplePat pats
        | SynMemberDefn.ImplicitInherit (t, e, _, r) ->
            addIfInside r
            walkType t; walkExpr e
        | SynMemberDefn.LetBindings (bindings, _, _, r) ->
            addIfInside r
            List.iter walkBinding bindings
        | SynMemberDefn.Interface (t, _, members, r) ->
            addIfInside r
            walkType t
            members |> Option.iter (List.iter walkMember)
        | SynMemberDefn.Inherit (t, _, r) ->
            addIfInside r
            walkType t
        | SynMemberDefn.ValField (field, r) ->
            addIfInside r
            walkField field
        | SynMemberDefn.NestedType (tdef, _, r) ->
            addIfInside r
            walkTypeDefn tdef
        | SynMemberDefn.AutoProperty (attributes = AllAttrs attrs; typeOpt = t; synExpr = e; range = r) ->
            addIfInside r
            List.iter walkAttribute attrs
            Option.iter walkType t
            walkExpr e
        | SynMemberDefn.Open(longId, r) -> addIfInside r

    and walkEnumCase (SynEnumCase(attributes = AllAttrs attrs;range = r)) =
        addIfInside r
        List.iter walkAttribute attrs

    and walkUnionCaseType = function
        | SynUnionCaseKind.Fields fields -> List.iter walkField fields
        | SynUnionCaseKind.FullType (t, _) -> walkType t

    and walkUnionCase (SynUnionCase (attributes = AllAttrs attrs; caseType = t; range = r)) =
        addIfInside r
        List.iter walkAttribute attrs
        walkUnionCaseType t

    and walkTypeDefnSimple = function
        | SynTypeDefnSimpleRepr.Enum (cases, r) ->
            addIfInside r
            List.iter walkEnumCase cases
        | SynTypeDefnSimpleRepr.Union (_, cases, r) ->
            addIfInside r
            List.iter walkUnionCase cases
        | SynTypeDefnSimpleRepr.Record (_, fields, r) ->
            addIfInside r
            List.iter walkField fields
        | SynTypeDefnSimpleRepr.TypeAbbrev (_, t, r) ->
            addIfInside r
            walkType t
        | SynTypeDefnSimpleRepr.General(_, _, _, _, _, _, _, r) -> addIfInside r
        | SynTypeDefnSimpleRepr.LibraryOnlyILAssembly(_, r) -> addIfInside r
        | SynTypeDefnSimpleRepr.None(r) -> addIfInside r
        | SynTypeDefnSimpleRepr.Exception(_) -> ()

    and walkComponentInfo isTypeExtensionOrAlias (SynComponentInfo(attributes = AllAttrs attrs; typeParams = typars; constraints =  constraints; longId = longIdent; range = r)) =
        addIfInside r
        List.iter walkAttribute attrs
        Option.iter walkTyparDecls typars
        List.iter walkTypeConstraint constraints

    and walkTypeDefnRepr = function
        | SynTypeDefnRepr.ObjectModel (_, defns, r) ->
            addIfInside r
            List.iter walkMember defns
        | SynTypeDefnRepr.Simple(defn, r) ->
            addIfInside r
            walkTypeDefnSimple defn
        | SynTypeDefnRepr.Exception _ -> ()

    and walkTypeDefnSigRepr = function
        | SynTypeDefnSigRepr.ObjectModel (_, defns, _) -> List.iter walkMemberSig defns
        | SynTypeDefnSigRepr.Simple(defn, _) -> walkTypeDefnSimple defn
        | SynTypeDefnSigRepr.Exception _ -> ()

    and walkTypeDefn (SynTypeDefn (info, repr, members, implicitCtor, r, _)) =
        addIfInside r
        let isTypeExtensionOrAlias =
            match repr with
            | SynTypeDefnRepr.ObjectModel (SynTypeDefnKind.Augmentation _, _, _)
            | SynTypeDefnRepr.ObjectModel (SynTypeDefnKind.Abbrev, _, _)
            | SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.TypeAbbrev _, _) -> true
            | _ -> false
        walkComponentInfo isTypeExtensionOrAlias info
        walkTypeDefnRepr repr
        Option.iter walkMember implicitCtor
        List.iter walkMember members

    and walkSynModuleDecl (decl: SynModuleDecl) =
        match decl with
        | SynModuleDecl.NamespaceFragment fragment -> walkSynModuleOrNamespace fragment
        | SynModuleDecl.NestedModule (info, _, modules, _, r, _) ->
            addIfInside r
            walkComponentInfo false info
            List.iter walkSynModuleDecl modules
        | SynModuleDecl.Let (_, bindings, r) ->
            addIfInside r
            List.iter walkBinding bindings
        | SynModuleDecl.DoExpr (_, expr, r) ->
            addIfInside r
            walkExpr expr
        | SynModuleDecl.Types (types, r) ->
            addIfInside r
            List.iter walkTypeDefn types
        | SynModuleDecl.Attributes (attributes = AllAttrs attrs; range = r) ->
            addIfInside r
            List.iter walkAttribute attrs
        | SynModuleDecl.ModuleAbbrev(ident, longId, r) -> addIfInside r
        | SynModuleDecl.Exception(_, r) -> addIfInside r
        | SynModuleDecl.Open(longDotId, r) -> addIfInside r
        | SynModuleDecl.HashDirective(_, r) -> addIfInside r

    match input with
    | ParsedInput.ImplFile input ->
        walkImplFileInput input
    | _ -> ()
    //debug "%A" idents
    result


let getLongIdentAt ast pos =
    let idents = getLongIdents (Some ast)
    match idents.TryGetValue pos with
    | true, idents -> Some idents
    | _ -> None

/// Returns ranges of all quotations found in an untyped AST
let getQuotationRanges ast =
    let quotationRanges = ResizeArray()

    let rec visitExpr = function
        | SynExpr.LongIdentSet (_, expr, _)
        | SynExpr.Typed (expr, _, _)
        | SynExpr.Paren (expr, _, _, _)
        | SynExpr.New (_, _, expr, _)
        | SynExpr.ArrayOrListComputed (_, expr, _)
        | SynExpr.ComputationExpr (_, expr, _)
        | SynExpr.ForEach (bodyExpr = expr)
        | SynExpr.YieldOrReturn (_, expr, _)
        | SynExpr.YieldOrReturnFrom (_, expr, _)
        | SynExpr.Do (expr, _)
        | SynExpr.DoBang (expr, _)
        | SynExpr.Downcast (expr, _, _)
        | SynExpr.For (doBody = expr)
        | SynExpr.Lazy (expr, _)
        | SynExpr.Assert (expr, _)
        | SynExpr.TypeApp (expr, _, _, _, _, _, _)
        | SynExpr.DotSet (_, _, expr, _)
        | SynExpr.DotIndexedSet (_, _, expr, _, _, _)
        | SynExpr.NamedIndexedPropertySet (_, _, expr, _)
        | SynExpr.DotNamedIndexedPropertySet (_, _, _, expr, _)
        | SynExpr.TypeTest (expr, _, _)
        | SynExpr.Upcast (expr, _, _)
        | SynExpr.InferredUpcast (expr, _)
        | SynExpr.InferredDowncast (expr, _)
        | SynExpr.Lambda (body = expr)
        | SynExpr.AddressOf (_, expr, _, _) ->
            visitExpr expr
        | SynExpr.App (funcExpr = expr1; argExpr = expr2)
        | SynExpr.TryFinally (tryExpr = expr1; finallyExpr = expr2)
        | SynExpr.While (_, expr1, expr2, _) ->
            visitExpr expr1; visitExpr expr2
        | SynExpr.LetOrUseBang (rhs = expr1; andBangs = ands; body = expr2) ->
          visitExpr expr1
          for SynExprAndBang(body = body) in ands do
            visitExpr body
          visitExpr expr2
        | SynExpr.Tuple (_, exprs, _, _)
        | SynExpr.ArrayOrList (_, exprs, _)
        | Sequentials  exprs ->
            List.iter visitExpr exprs
        | SynExpr.TryWith (tryExpr = expr; withCases = clauses)
        | SynExpr.Match (expr = expr; clauses = clauses) ->
            visitExpr expr; visitMatches clauses
        | SynExpr.IfThenElse (ifExpr = cond; thenExpr = trueBranch; elseExpr = falseBranchOpt) ->
            visitExpr cond; visitExpr trueBranch
            falseBranchOpt |> Option.iter visitExpr
        | SynExpr.LetOrUse (bindings = bindings; body = body) -> visitBindindgs bindings; visitExpr body
        | SynExpr.Quote (_, _isRaw, _quotedExpr, _, range) -> quotationRanges.Add range
        | SynExpr.MatchLambda (_, _, clauses, _, _) -> visitMatches clauses
        | SynExpr.ObjExpr (bindings =  bindings) -> visitBindindgs bindings
        | SynExpr.Record (_, _, fields, _) ->
            fields |> List.choose (fun (SynExprRecordField(expr = expr)) -> expr) |> List.iter visitExpr
        | _ -> ()

    and visitBinding (SynBinding(expr = body)) = visitExpr body
    and visitBindindgs = List.iter visitBinding

    and visitPattern = function
        | SynPat.QuoteExpr (expr, _) -> visitExpr expr
        | SynPat.Paren (pat, _)
        | SynPat.Typed (pat, _, _) -> visitPattern pat
        | SynPat.Ands (pats, _)
        | SynPat.Tuple (_, pats, _)
        | SynPat.ArrayOrList (_, pats, _) -> List.iter visitPattern pats
        | SynPat.Or (lhsPat = pat1; rhsPat = pat2) -> visitPattern pat1; visitPattern pat2
        | SynPat.LongIdent (argPats = ctorArgs) ->
            match ctorArgs with
            | SynArgPats.Pats pats -> List.iter visitPattern pats
            | SynArgPats.NamePatPairs(xs, _) ->
                xs |> List.map (fun (_, _, pat) -> pat) |> List.iter visitPattern
        | SynPat.Record(xs, _) -> xs |> List.map (fun (_, _, pat) -> pat) |> List.iter visitPattern
        | _ -> ()

    and visitMatch (SynMatchClause (pat = pat; resultExpr = expr)) = visitPattern pat; visitExpr expr

    and visitMatches = List.iter visitMatch

    let visitMember = function
        | SynMemberDefn.LetBindings (bindings, _, _, _) -> visitBindindgs bindings
        | SynMemberDefn.Member (binding, _) -> visitBinding binding
        | SynMemberDefn.AutoProperty (synExpr = expr) -> visitExpr expr
        | _ -> ()

    let visitType ty =
        let (SynTypeDefn (typeRepr = repr; members = defns; implicitConstructor = implicitCtor)) = ty
        match repr with
        | SynTypeDefnRepr.ObjectModel (_, objDefns, _) ->
            for d in objDefns do visitMember d
        | _ -> ()
        Option.iter visitMember implicitCtor
        for d in defns do visitMember d

    let rec visitDeclarations decls =
        decls |> List.iter
           (function
            | SynModuleDecl.Let (_, bindings, _) -> visitBindindgs bindings
            | SynModuleDecl.DoExpr (_, expr, _) -> visitExpr expr
            | SynModuleDecl.Types (types, _) -> List.iter visitType types
            | SynModuleDecl.NestedModule (decls = decls) -> visitDeclarations decls
            | _ -> () )

    let visitModulesAndNamespaces modulesOrNss =
        modulesOrNss
        |> Seq.iter (fun (SynModuleOrNamespace(_, _, _, decls, _, _, _, _)) -> visitDeclarations decls)
    ast
    |> Option.iter (function
        | ParsedInput.ImplFile (ParsedImplFileInput(_, _, _, _, _, modules, _)) -> visitModulesAndNamespaces modules
        | _ -> ())
    quotationRanges

/// Returns all string literal ranges
let internal getStringLiterals ast : Range list =
    let result = ResizeArray()

    let visitType ty =
        match ty with
        | SynType.StaticConstant (SynConst.String(_, _, r), _) -> result.Add r
        | _ -> ()

    let rec visitExpr = function
        | SynExpr.ArrayOrListComputed (_, expr, _)
        | SynExpr.ComputationExpr (_, expr, _)
        | SynExpr.Lambda (body =  expr)
        | SynExpr.YieldOrReturn (_, expr, _)
        | SynExpr.YieldOrReturnFrom (_, expr, _)
        | SynExpr.New (_, _, expr, _)
        | SynExpr.Assert (expr, _)
        | SynExpr.Do (expr, _)
        | SynExpr.Typed (expr, _, _)
        | SynExpr.Paren (expr, _, _, _)
        | SynExpr.DoBang (expr, _)
        | SynExpr.Downcast (expr, _, _)
        | SynExpr.For (doBody = expr)
        | SynExpr.Lazy (expr, _)
        | SynExpr.TypeTest(expr, _, _)
        | SynExpr.Upcast(expr, _, _)
        | SynExpr.InferredUpcast(expr, _)
        | SynExpr.InferredDowncast(expr, _)
        | SynExpr.LongIdentSet (_, expr, _)
        | SynExpr.DotGet (expr, _, _, _)
        | SynExpr.ForEach (bodyExpr = expr) -> visitExpr expr
        | SynExpr.App (funcExpr = expr1; argExpr = expr2)
        | SynExpr.TryFinally (tryExpr = expr1; finallyExpr = expr2)
        | SynExpr.NamedIndexedPropertySet (_, expr1, expr2, _)
        | SynExpr.DotNamedIndexedPropertySet (_, _, expr1, expr2, _)
        | SynExpr.While (_, expr1, expr2, _) ->
            visitExpr expr1; visitExpr expr2
        | SynExpr.LetOrUseBang (rhs = expr1; andBangs = ands; body = expr2) ->
            visitExpr expr1
            for (SynExprAndBang(body = body)) in ands do
              visitExpr body
            visitExpr expr2
        | Sequentials exprs
        | SynExpr.Tuple (_, exprs, _, _)
        | SynExpr.ArrayOrList(_, exprs, _) -> List.iter visitExpr exprs
        | SynExpr.Match (expr = expr; clauses = clauses)
        | SynExpr.TryWith(tryExpr = expr; withCases = clauses) ->
            visitExpr expr; visitMatches clauses
        | SynExpr.IfThenElse(ifExpr = cond; thenExpr = trueBranch; elseExpr = falseBranchOpt;) ->
            visitExpr cond
            visitExpr trueBranch
            falseBranchOpt |> Option.iter visitExpr
        | SynExpr.LetOrUse (bindings = bindings; body = body) ->
            visitBindindgs bindings
            visitExpr body
        | SynExpr.Record (_, _, fields, _) ->
            fields |> List.choose (fun (SynExprRecordField(expr = expr)) -> expr) |> List.iter visitExpr
        | SynExpr.MatchLambda (_, _, clauses, _, _) -> visitMatches clauses
        | SynExpr.ObjExpr (bindings = bindings) -> visitBindindgs bindings
        | SynExpr.Const (SynConst.String (_, _, r), _) -> result.Add r
        | SynExpr.TypeApp(_, _, tys, _, _, _, _) -> List.iter visitType tys
        | _ -> ()

    and visitBinding (SynBinding(expr = body)) = visitExpr body
    and visitBindindgs = List.iter visitBinding
    and visitMatch (SynMatchClause (resultExpr = expr)) = visitExpr expr
    and visitMatches = List.iter visitMatch

    let visitMember = function
        | SynMemberDefn.LetBindings (bindings, _, _, _) -> visitBindindgs bindings
        | SynMemberDefn.Member (binding, _) -> visitBinding binding
        | SynMemberDefn.AutoProperty (synExpr = expr) -> visitExpr expr
        | _ -> ()

    let visitTypeDefn ty =
        let (SynTypeDefn (typeRepr = repr; members = memberDefns; implicitConstructor = implicitCtor)) = ty
        match repr with
        | SynTypeDefnRepr.ObjectModel (_, defns, _) ->
            for d in defns do visitMember d
        | SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.TypeAbbrev(_, SynType.App(_, _, tys, _,_ , _, _), _), _) ->
            List.iter visitType tys
        | _ -> ()
        Option.iter visitMember implicitCtor
        List.iter visitMember memberDefns

    let rec visitDeclarations decls =
        for declaration in decls do
            match declaration with
            | SynModuleDecl.Let (_, bindings, _) -> visitBindindgs bindings
            | SynModuleDecl.DoExpr (_, expr, _) -> visitExpr expr
            | SynModuleDecl.Types (types, _) -> for ty in types do visitTypeDefn ty
            | SynModuleDecl.NestedModule (decls = decls) -> visitDeclarations decls
            | _ -> ()

    let visitModulesAndNamespaces modulesOrNss =
        Seq.iter (fun (SynModuleOrNamespace(_, _, _, decls, _, _, _, _)) -> visitDeclarations decls) modulesOrNss

    ast
    |> Option.iter (function
        | ParsedInput.ImplFile (ParsedImplFileInput(_, _, _, _, _, modules, _)) -> visitModulesAndNamespaces modules
        | _ -> ())

    List.ofSeq result

/// Get path to containing module/namespace of a given position
let getModuleOrNamespacePath (pos: Position) (ast: ParsedInput) =
    let idents =
        match ast with
        | ParsedInput.ImplFile (ParsedImplFileInput(_, _, _, _, _, modules, _)) ->
            let rec walkModuleOrNamespace idents (decls, moduleRange) =
                decls
                |> List.fold (fun acc ->
                    function
                    | SynModuleDecl.NestedModule (moduleInfo = componentInfo; decls = nestedModuleDecls; range = nestedModuleRange) ->
                        if Range.rangeContainsPos moduleRange pos then
                            let (SynComponentInfo(_,_,_,longIdent,_,_,_,_)) = componentInfo
                            walkModuleOrNamespace (longIdent::acc) (nestedModuleDecls, nestedModuleRange)
                        else acc
                    | _ -> acc) idents

            modules
            |> List.fold (fun acc (SynModuleOrNamespace(longIdent, _, _, decls, _, _, _, moduleRange)) ->
                    if Range.rangeContainsPos moduleRange pos then
                        walkModuleOrNamespace (longIdent::acc) (decls, moduleRange) @ acc
                    else acc) []
        | ParsedInput.SigFile(ParsedSigFileInput(_, _, _, _, modules)) ->
            let rec walkModuleOrNamespaceSig idents (decls, moduleRange) =
                decls
                |> List.fold (fun acc ->
                    function
                    | SynModuleSigDecl.NestedModule (moduleInfo = componentInfo; moduleDecls = nestedModuleDecls; range = nestedModuleRange) ->
                        if Range.rangeContainsPos moduleRange pos then
                            let (SynComponentInfo(_,_,_,longIdent,_,_,_,_)) = componentInfo
                            walkModuleOrNamespaceSig (longIdent::acc) (nestedModuleDecls, nestedModuleRange)
                        else acc
                    | _ -> acc) idents

            modules
            |> List.fold (fun acc (SynModuleOrNamespaceSig(longIdent, _, _, decls, _, _, _, moduleRange)) ->
                    if Range.rangeContainsPos moduleRange pos then
                        walkModuleOrNamespaceSig (longIdent::acc) (decls, moduleRange) @ acc
                    else acc) []
    idents
    |> List.rev
    |> Seq.concat
    |> Seq.map (fun ident -> ident.idText)
    |> String.concat "."

let getIdentUsagesByName ast name =
  let idents = getLongIdents (Some ast)
  idents
  |> Seq.choose (fun (KeyValue(pos, ident)) -> if ident = name then Some pos else None)
  |> List.ofSeq

module HashDirectiveInfo =
    open System.IO

    type IncludeDirective =
        | ResolvedDirectory of string

    type LoadDirective =
        | ExistingFile of string
        | UnresolvableFile of string * previousIncludes : string array

    [<NoComparison>]
    type Directive =
        | Include of IncludeDirective * range
        | Load of LoadDirective * range

    /// returns an array of LoadScriptResolutionEntries
    /// based on #I and #load directives
    let getIncludeAndLoadDirectives ast =
        // the Load items are resolved using fallback resolution relying on previously parsed #I directives
        // (this behaviour is undocumented in F# but it seems to be how it works).

        // list of #I directives so far (populated while encountering those in order)
        // TODO: replace by List.fold if possible
        let includesSoFar = new System.Collections.Generic.List<_>()
        let pushInclude = includesSoFar.Add

        // those might need to be abstracted away from real filesystem operations
        let fileExists = File.Exists
        let directoryExists = Directory.Exists
        let isPathRooted (path: string) = Path.IsPathRooted path
        let getDirectoryOfFile = Path.GetFullPathSafe >> Path.GetDirectoryName
        let getRootedDirectory = Path.GetFullPathSafe
        let makeRootedDirectoryIfNecessary baseDirectory directory =
            if not (isPathRooted directory) then
                getRootedDirectory (baseDirectory </> directory)
            else
                directory

        let (|StringDirective|_|) (p: ParsedHashDirectiveArgument) =
            match p with
            | ParsedHashDirectiveArgument.String(v, _, _) -> Some v
            | _ -> None
        // separate function to reduce nesting one level
        let parseDirectives modules file =
            [|
            let baseDirectory = getDirectoryOfFile file
            for (SynModuleOrNamespace (_, _, _, declarations, _, _, _, _)) in modules do
                for decl in declarations do
                    match decl with
                    | SynModuleDecl.HashDirective (ParsedHashDirective("I",[StringDirective directory],range),_) ->
                        let directory = makeRootedDirectoryIfNecessary (getDirectoryOfFile file) directory

                        if directoryExists directory then
                            let includeDirective = ResolvedDirectory(directory)
                            pushInclude includeDirective
                            yield Include (includeDirective, range)

                    | SynModuleDecl.HashDirective (ParsedHashDirective ("load",files,range),_) ->
                        for f in files do
                            match f with
                            | StringDirective f ->
                                if isPathRooted f && fileExists f then

                                    // this is absolute reference to an existing script, easiest case
                                    yield Load (ExistingFile f, range)

                                else
                                    // I'm not sure if the order is correct, first checking relative to file containing the #load directive
                                    // then checking for undocumented resolution using previously parsed #I directives
                                    let fileRelativeToCurrentFile = baseDirectory </> f
                                    if fileExists fileRelativeToCurrentFile then
                                        // this is existing file relative to current file
                                        yield Load (ExistingFile fileRelativeToCurrentFile, range)

                                    else
                                        // match file against first include which seemingly have it found
                                        let maybeFile =
                                            includesSoFar
                                            |> Seq.tryPick (function
                                                | (ResolvedDirectory d) ->
                                                    let filePath = d </> f
                                                    if fileExists filePath then Some filePath else None
                                            )
                                        match maybeFile with
                                        | None -> () // can't load this file even using any of the #I directives...
                                        | Some f ->
                                            yield Load (ExistingFile f,range)
                            | _ -> ()
                    | _ -> ()
            |]

        match ast with
        | ParsedInput.ImplFile (ParsedImplFileInput(fn,_,_,_,_,modules,_)) -> parseDirectives modules fn
        | _ -> [||]

    /// returns the Some (complete file name of a resolved #load directive at position) or None
    let getHashLoadDirectiveResolvedPathAtPosition (pos: Position) (ast: ParsedInput) : string option =
        getIncludeAndLoadDirectives ast
        |> Array.tryPick (
            function
            | Load (ExistingFile f,range)
                // check the line is within the range
                // (doesn't work when there are multiple files given to a single #load directive)
                when Range.rangeContainsPos range pos
                    -> Some f
            | _     -> None
        )

module Printf =
    [<NoComparison>]
    type PrintfFunction =
        { FormatString: Range
          Args: Range[] }

    [<NoComparison>]
    type private AppWithArg =
        { Range: Range
          Arg: Range }

    let internal getAll (input: ParsedInput option) : PrintfFunction[] =
        let result = ResizeArray()
        let appStack: AppWithArg list ref = ref []

        let addAppWithArg appWithArg =
            match appStack.Value with
            | lastApp :: _ when not (Range.rangeContainsRange lastApp.Range appWithArg.Range) ->
                appStack.Value <- [appWithArg]
            | _ ->
                appStack.Value <- appWithArg :: appStack.Value

        let rec walkImplFileInput (ParsedImplFileInput(_, _, _, _, _, moduleOrNamespaceList, _)) =
            List.iter walkSynModuleOrNamespace moduleOrNamespaceList

        and walkSynModuleOrNamespace (SynModuleOrNamespace(_, _, _, decls, _, _, _, _)) =
            List.iter walkSynModuleDecl decls

        and walkTypeConstraint = function
            | SynTypeConstraint.WhereTyparDefaultsToType (_, ty, _)
            | SynTypeConstraint.WhereTyparSubtypeOfType (_, ty, _) -> walkType ty
            | SynTypeConstraint.WhereTyparIsEnum (_, ts, _)
            | SynTypeConstraint.WhereTyparIsDelegate (_, ts, _) -> List.iter walkType ts
            | SynTypeConstraint.WhereTyparSupportsMember (_, sign, _) -> walkMemberSig sign
            | _ -> ()

        and walkBinding (SynBinding (returnInfo = returnInfo; expr = e)) =
            walkExpr e
            returnInfo |> Option.iter (fun (SynBindingReturnInfo (t, _, _)) -> walkType t)

        and walkInterfaceImpl (SynInterfaceImpl(bindings = bindings)) = List.iter walkBinding bindings

        and walkType = function
            | SynType.Array (_, t, _)
            | SynType.HashConstraint (t, _)
            | SynType.MeasurePower (t, _, _) -> walkType t
            | SynType.Fun (t1, t2, _)
            | SynType.MeasureDivide (t1, t2, _) -> walkType t1; walkType t2
            | SynType.App (ty, _, types, _, _, _, _) -> walkType ty; List.iter walkType types
            | SynType.LongIdentApp (_, _, _, types, _, _, _) -> List.iter walkType types
            | SynType.Tuple (_, ts, _) -> ts |> List.iter (fun (_, t) -> walkType t)
            | SynType.WithGlobalConstraints (t, typeConstraints, _) ->
                walkType t; List.iter walkTypeConstraint typeConstraints
            | _ -> ()

        and walkClause (SynMatchClause (whenExpr = e1; resultExpr = e2)) =
            walkExpr e2
            e1 |> Option.iter walkExpr

        and walkSimplePats = function
            | SynSimplePats.SimplePats (pats, _) -> List.iter walkSimplePat pats
            | SynSimplePats.Typed (pats, ty, _) ->
                walkSimplePats pats
                walkType ty

        and walkExpr e =
            match e with
            | SynExpr.App (_, _, SynExpr.Ident _, SynExpr.Const (SynConst.String (_, _, stringRange), _), r) ->
                match appStack.Value with
                | (lastApp :: _) as apps when Range.rangeContainsRange lastApp.Range e.Range ->
                    let intersectsWithFuncOrString (arg: Range) =
                        Range.rangeContainsRange arg stringRange
                        || arg = stringRange
                        || Range.rangeContainsRange arg r
                        || arg = r

                    let rec loop acc (apps: AppWithArg list) =
                        match acc, apps with
                        | _, [] -> acc
                        | [], h :: t ->
                            if not (intersectsWithFuncOrString h.Arg) then
                                loop [h] t
                            else loop [] t
                        | prev :: _, curr :: rest ->
                            if Range.rangeContainsRange curr.Range prev.Range
                               && not (intersectsWithFuncOrString curr.Arg) then
                                loop (curr :: acc) rest
                            else acc

                    let args =
                        apps
                        |> loop []
                        |> List.rev
                        |> List.map (fun x -> x.Arg)
                        |> List.toArray
                    let res = { FormatString = stringRange
                                Args = args }
                    result.Add res
                | _ -> ()
                appStack.Value <- []
            | SynExpr.App (_, _, SynExpr.App(_, true, SynExpr.Ident op, e1, _), e2, _) ->
                let rec deconstruct = function
                    | SynExpr.Paren (exp, _, _, _) -> deconstruct exp
                    | SynExpr.Tuple (_, exps, _, _) ->
                        exps |> List.iter (fun exp -> addAppWithArg { Range = e.Range; Arg = exp.Range})
                        ()
                    | _ -> ()

                addAppWithArg { Range = e.Range; Arg = e2.Range }
                if op.idText = (PrettyNaming.CompileOpName "||>")
                        || op.idText = (PrettyNaming.CompileOpName "|||>") then
                    deconstruct e1
                    walkExpr e2
                else
                    if op.idText = (PrettyNaming.CompileOpName "|>") then
                        addAppWithArg { Range = e.Range; Arg = e1.Range }
                    walkExpr e2
                    walkExpr e1
            | SynExpr.App (_, _, SynExpr.App(_, true, _, e1, _), e2, _) ->
                addAppWithArg { Range = e.Range; Arg = e2.Range }
                addAppWithArg { Range = e.Range; Arg = e1.Range }
                walkExpr e1
                walkExpr e2
            | SynExpr.App (_, _, e1, e2, _) ->
                addAppWithArg { Range = e.Range; Arg = e2.Range }
                walkExpr e1
                walkExpr e2
            | _ ->
                match e with
                | SynExpr.Paren (e, _, _, _)
                | SynExpr.Quote (_, _, e, _, _)
                | SynExpr.Typed (e, _, _)
                | SynExpr.InferredUpcast (e, _)
                | SynExpr.InferredDowncast (e, _)
                | SynExpr.AddressOf (_, e, _, _)
                | SynExpr.DoBang (e, _)
                | SynExpr.YieldOrReturn (_, e, _)
                | SynExpr.ArrayOrListComputed (_, e, _)
                | SynExpr.ComputationExpr (_, e, _)
                | SynExpr.Do (e, _)
                | SynExpr.Assert (e, _)
                | SynExpr.Lazy (e, _)
                | SynExpr.YieldOrReturnFrom (_, e, _) -> walkExpr e
                | SynExpr.Lambda (args = pats; body = e) ->
                    walkSimplePats pats
                    walkExpr e
                | SynExpr.New (_, t, e, _)
                | SynExpr.TypeTest (e, t, _)
                | SynExpr.Upcast (e, t, _)
                | SynExpr.Downcast (e, t, _) -> walkExpr e; walkType t
                | SynExpr.Tuple (_, es, _, _)
                | Sequentials es
                | SynExpr.ArrayOrList (_, es, _) -> List.iter walkExpr es
                | SynExpr.TryFinally (tryExpr = e1; finallyExpr = e2)
                | SynExpr.While (_, e1, e2, _) -> List.iter walkExpr [e1; e2]
                | SynExpr.Record (_, _, fields, _) ->
                    fields |> List.iter (fun (SynExprRecordField(expr = e)) -> e |> Option.iter walkExpr)
                | SynExpr.ObjExpr(ty, argOpt, _, bindings, _, ifaces, _, _) ->
                    argOpt |> Option.iter (fun (e, _) -> walkExpr e)
                    walkType ty
                    List.iter walkBinding bindings
                    List.iter walkInterfaceImpl ifaces
                | SynExpr.For (identBody = e1; toBody = e2; doBody = e3) -> List.iter walkExpr [e1; e2; e3]
                | SynExpr.ForEach (enumExpr = e1; bodyExpr = e2) -> List.iter walkExpr [e1; e2]
                | SynExpr.MatchLambda (_, _, synMatchClauseList, _, _) ->
                    List.iter walkClause synMatchClauseList
                | SynExpr.Match (expr = e; clauses = synMatchClauseList) ->
                    walkExpr e
                    List.iter walkClause synMatchClauseList
                | SynExpr.TypeApp (e, _, tys, _, _, _, _) ->
                    List.iter walkType tys; walkExpr e
                | SynExpr.LetOrUse (bindings = bindings; body = e) ->
                    List.iter walkBinding bindings; walkExpr e
                | SynExpr.TryWith (tryExpr =e; withCases = clauses;) ->
                    List.iter walkClause clauses;  walkExpr e
                | SynExpr.IfThenElse (ifExpr = e1; thenExpr = e2; elseExpr = e3) ->
                    List.iter walkExpr [e1; e2]
                    e3 |> Option.iter walkExpr
                | SynExpr.LongIdentSet (_, e, _)
                | SynExpr.DotGet (e, _, _, _) -> walkExpr e
                | SynExpr.DotSet (e1, _, e2, _) ->
                    walkExpr e1
                    walkExpr e2
                | SynExpr.DotIndexedGet (e, args, _, _) ->
                    walkExpr e
                    walkExpr args
                | SynExpr.DotIndexedSet (e1, args, e2, _, _, _) ->
                    walkExpr e1
                    walkExpr args
                    walkExpr e2
                | SynExpr.NamedIndexedPropertySet (_, e1, e2, _) -> List.iter walkExpr [e1; e2]
                | SynExpr.DotNamedIndexedPropertySet (e1, _, e2, e3, _) -> List.iter walkExpr [e1; e2; e3]
                | SynExpr.JoinIn (e1, _, e2, _) -> List.iter walkExpr [e1; e2]
                | SynExpr.LetOrUseBang (rhs = e1; andBangs = ands; body = e2) ->
                  walkExpr e1
                  for (SynExprAndBang(body = body)) in ands do
                    walkExpr body
                  walkExpr e2
                | SynExpr.TraitCall (_, sign, e, _) ->
                    walkMemberSig sign
                    walkExpr e
                | SynExpr.Const (SynConst.Measure(_, _, m), _) -> walkMeasure m
                | _ -> ()

        and walkMeasure = function
            | SynMeasure.Product (m1, m2, _)
            | SynMeasure.Divide (m1, m2, _) -> walkMeasure m1; walkMeasure m2
            | SynMeasure.Seq (ms, _) -> List.iter walkMeasure ms
            | SynMeasure.Power (m, _, _) -> walkMeasure m
            | SynMeasure.One
            | SynMeasure.Anon _
            | SynMeasure.Named _
            | SynMeasure.Var _ -> ()

        and walkSimplePat = function
            | SynSimplePat.Attrib (pat, _, _) -> walkSimplePat pat
            | SynSimplePat.Typed(_, t, _) -> walkType t
            | _ -> ()

        and walkField (SynField(_, _, _, t, _, _, _, _)) = walkType t

        and walkMemberSig = function
            | SynMemberSig.Inherit (t, _)
            | SynMemberSig.Interface(t, _) -> walkType t
            | SynMemberSig.ValField(f, _) -> walkField f
            | SynMemberSig.NestedType(SynTypeDefnSig (typeRepr = repr; members = memberSigs), _) ->
                walkTypeDefnSigRepr repr
                List.iter walkMemberSig memberSigs
            | SynMemberSig.Member _ -> ()

        and walkMember = function
            | SynMemberDefn.Member (binding, _) -> walkBinding binding
            | SynMemberDefn.ImplicitCtor (_, _, AllSimplePats pats, _, _, _) -> List.iter walkSimplePat pats
            | SynMemberDefn.ImplicitInherit (t, e, _, _) -> walkType t; walkExpr e
            | SynMemberDefn.LetBindings (bindings, _, _, _) -> List.iter walkBinding bindings
            | SynMemberDefn.Interface (interfaceType = t; members = members) ->
                walkType t
                members |> Option.iter (List.iter walkMember)
            | SynMemberDefn.Inherit (t, _, _) -> walkType t
            | SynMemberDefn.ValField (field, _) -> walkField field
            | SynMemberDefn.NestedType (tdef, _, _) -> walkTypeDefn tdef
            | SynMemberDefn.AutoProperty (typeOpt = t; synExpr = e) ->
                Option.iter walkType t
                walkExpr e
            | _ -> ()

        and walkTypeDefnRepr = function
            | SynTypeDefnRepr.ObjectModel (_, defns, _) -> List.iter walkMember defns
            | SynTypeDefnRepr.Simple _ -> ()
            | SynTypeDefnRepr.Exception _ -> ()

        and walkTypeDefnSigRepr = function
            | SynTypeDefnSigRepr.ObjectModel (_, defns, _) -> List.iter walkMemberSig defns
            | SynTypeDefnSigRepr.Simple _ -> ()
            | SynTypeDefnSigRepr.Exception _ -> ()

        and walkTypeDefn (SynTypeDefn (typeRepr = repr; members = members; implicitConstructor = implicitCtor)) =
            walkTypeDefnRepr repr
            Option.iter walkMember implicitCtor
            List.iter walkMember members

        and walkSynModuleDecl (decl: SynModuleDecl) =
            match decl with
            | SynModuleDecl.NamespaceFragment fragment -> walkSynModuleOrNamespace fragment
            | SynModuleDecl.NestedModule (decls = modules) ->
                List.iter walkSynModuleDecl modules
            | SynModuleDecl.Let (_, bindings, _) -> List.iter walkBinding bindings
            | SynModuleDecl.DoExpr (_, expr, _) -> walkExpr expr
            | SynModuleDecl.Types (types, _) -> List.iter walkTypeDefn types
            | _ -> ()

        match input with
        | Some (ParsedInput.ImplFile input) ->
             walkImplFileInput input
        | _ -> ()
        //debug "%A" idents
        result.ToArray()

module Completion =

  [<RequireQualifiedAccess>]
  type Context =
  | StringLiteral
  | Unknown

  let atPos (pos: Position, ast: ParsedInput): Context =
    let visitor =
      { new SyntaxVisitorBase<Context>() with

        member x.VisitExpr(path, traverseExpr, defaultTraverse, expr): Context option =
          if Range.rangeContainsPos expr.Range pos
          then
              match expr with
              | SynExpr.Const(SynConst.String _, _) -> Some Context.StringLiteral
              | SynExpr.InterpolatedString (parts, _, _) ->
                parts
                |> List.tryPick (
                  function | SynInterpolatedStringPart.String(s, m) when Range.rangeContainsPos m pos -> Some Context.StringLiteral
                           | SynInterpolatedStringPart.String _ -> None
                           | SynInterpolatedStringPart.FillExpr(e, _) when Range.rangeContainsPos e.Range pos -> defaultTraverse e // gotta dive into the expr to see if we're in a literal inside the expr
                           | SynInterpolatedStringPart.FillExpr _ -> None
                )
              | _ -> defaultTraverse expr
          else None
      }
    SyntaxTraversal.Traverse(pos, ast, visitor)
    |> Option.defaultValue Context.Unknown
