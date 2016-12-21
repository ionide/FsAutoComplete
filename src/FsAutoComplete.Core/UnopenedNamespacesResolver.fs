/// Original code from VisualFSharpPowerTools project: https://github.com/fsprojects/VisualFSharpPowerTools/blob/master/src/FSharp.Editing/Coloring/UnopenedNamespacesResolver.fs
namespace FsAutoComplete.UnopenedNamespacesResolver

open FsAutoComplete
open FsAutoComplete.UntypedAstUtils

type LongIdent = string


type Entity =
    { FullRelativeName: LongIdent
      Qualifier: LongIdent
      Namespace: LongIdent option
      Name: LongIdent }
    override x.ToString() = sprintf "%A" x

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Entity =
    let getRelativeNamespace (targetNs: Idents) (sourceNs: Idents) =
        let rec loop index =
            if index > targetNs.Length - 1 then sourceNs.[index..]
            // target namespace is not a full parent of source namespace, keep the source ns as is
            elif index > sourceNs.Length - 1 then sourceNs
            elif targetNs.[index] = sourceNs.[index] then loop (index + 1)
            else sourceNs.[index..]
        if sourceNs.Length = 0 || targetNs.Length = 0 then sourceNs
        else loop 0

    let cutAutoOpenModules (autoOpenParent: Idents option) (candidateNs: Idents) =
        let nsCount =
            match autoOpenParent with
            | Some parent when parent.Length > 0 ->
                min (parent.Length - 1) candidateNs.Length
            | _ -> candidateNs.Length
        candidateNs.[0..nsCount - 1]

    let tryCreate (targetNamespace: Idents option, targetScope: Idents, partiallyQualifiedName: Idents,
                   requiresQualifiedAccessParent: Idents option, autoOpenParent: Idents option,
                   candidateNamespace: Idents option, candidate: Idents) =
        match candidate with
        | [||] -> [||]
        | _ ->
            partiallyQualifiedName
            |> Array.heads
            |> Array.choose (fun parts ->
                if not (candidate |> Array.endsWith parts) then None
                else
                  let identCount = parts.Length
                  let fullOpenableNs, restIdents =
                      let openableNsCount =
                          match requiresQualifiedAccessParent with
                          | Some parent -> min parent.Length candidate.Length
                          | None -> candidate.Length
                      candidate.[0..openableNsCount - 2], candidate.[openableNsCount - 1..]

                  let openableNs = cutAutoOpenModules autoOpenParent fullOpenableNs

                  let getRelativeNs ns =
                      match targetNamespace, candidateNamespace with
                      | Some targetNs, Some candidateNs when candidateNs = targetNs ->
                          getRelativeNamespace targetScope ns
                      | None, _ -> getRelativeNamespace targetScope ns
                      | _ -> ns

                  let relativeNs = getRelativeNs openableNs

                  match relativeNs, restIdents with
                  | [||], [||] -> None
                  | [||], [|_|] -> None
                  | _ ->
                      let fullRelativeName = Array.append (getRelativeNs fullOpenableNs) restIdents
                      let ns =
                          match relativeNs with
                          | [||] -> None
                          | _ when identCount > 1 && relativeNs.Length >= identCount ->
                              Some (relativeNs.[0..relativeNs.Length - identCount] |> String.concat ".")
                          | _ -> Some (relativeNs |> String.concat ".")
                      let qualifier =
                          if fullRelativeName.Length > 1 && fullRelativeName.Length >= identCount then
                              fullRelativeName.[0..fullRelativeName.Length - identCount]
                          else fullRelativeName
                      Some
                          { FullRelativeName = String.concat "." fullRelativeName //.[0..fullRelativeName.Length - identCount - 1]
                            Qualifier = String.concat "." qualifier
                            Namespace = ns
                            Name = match restIdents with [|_|] -> "" | _ -> String.concat "." restIdents })

type ScopeKind =
    | Namespace
    | TopModule
    | NestedModule
    | OpenDeclaration
    | HashDirective
    override x.ToString() = sprintf "%A" x

type InsertContext =
    { ScopeKind: ScopeKind
      Pos: Pos }

module ParsedInput =
    open Microsoft.FSharp.Compiler
    open Microsoft.FSharp.Compiler.Ast

    type private EndLine = int

    let getEntityKind (input: ParsedInput) (p: Pos) : EntityKind option =
        let pos = Range.mkPos p.Line p.Col

        let (|ConstructorPats|) = function
            | Pats ps -> ps
            | NamePatPairs(xs, _) -> List.map snd xs

        let isPosInRange range = Range.rangeContainsPos range pos

        let ifPosInRange range f =
            if isPosInRange range then f()
            else None

        let rec walkImplFileInput (ParsedImplFileInput(_, _, _, _, _, moduleOrNamespaceList, _)) =
            List.tryPick (walkSynModuleOrNamespace true) moduleOrNamespaceList

        and walkSynModuleOrNamespace isTopLevel (SynModuleOrNamespace(_, _, isModule, decls, _, attrs, _, r)) =
            if isModule && isTopLevel then None else List.tryPick walkAttribute attrs
            |> Option.orElse (ifPosInRange r (fun _ -> List.tryPick (walkSynModuleDecl isTopLevel) decls))

        and walkAttribute (attr: SynAttribute) =
            if isPosInRange attr.Range then Some EntityKind.Attribute else None
            |> Option.orElse (walkExprWithKind (Some EntityKind.Type) attr.ArgExpr)

        and walkTypar (Typar (ident, _, _)) = ifPosInRange ident.idRange (fun _ -> Some EntityKind.Type)

        and walkTyparDecl (SynTyparDecl.TyparDecl (attrs, typar)) =
            List.tryPick walkAttribute attrs
            |> Option.orElse (walkTypar typar)

        and walkTypeConstraint = function
            | SynTypeConstraint.WhereTyparDefaultsToType (t1, t2, _) -> walkTypar t1 |> Option.orElse (walkType t2)
            | SynTypeConstraint.WhereTyparIsValueType(t, _) -> walkTypar t
            | SynTypeConstraint.WhereTyparIsReferenceType(t, _) -> walkTypar t
            | SynTypeConstraint.WhereTyparIsUnmanaged(t, _) -> walkTypar t
            | SynTypeConstraint.WhereTyparSupportsNull (t, _) -> walkTypar t
            | SynTypeConstraint.WhereTyparIsComparable(t, _) -> walkTypar t
            | SynTypeConstraint.WhereTyparIsEquatable(t, _) -> walkTypar t
            | SynTypeConstraint.WhereTyparSubtypeOfType(t, ty, _) -> walkTypar t |> Option.orElse (walkType ty)
            | SynTypeConstraint.WhereTyparSupportsMember(ts, sign, _) ->
                List.tryPick walkType ts |> Option.orElse (walkMemberSig sign)
            | SynTypeConstraint.WhereTyparIsEnum(t, ts, _) -> walkTypar t |> Option.orElse (List.tryPick walkType ts)
            | SynTypeConstraint.WhereTyparIsDelegate(t, ts, _) -> walkTypar t |> Option.orElse (List.tryPick walkType ts)

        and walkPatWithKind (kind: EntityKind option) = function
            | SynPat.Ands (pats, _) -> List.tryPick walkPat pats
            | SynPat.Named(SynPat.Wild nameRange as pat, _, _, _, _) ->
                if isPosInRange nameRange then None
                else walkPat pat
            | SynPat.Typed(pat, t, _) -> walkPat pat |> Option.orElse (walkType t)
            | SynPat.Attrib(pat, attrs, _) -> walkPat pat |> Option.orElse (List.tryPick walkAttribute attrs)
            | SynPat.Or(pat1, pat2, _) -> List.tryPick walkPat [pat1; pat2]
            | SynPat.LongIdent(_, _, typars, ConstructorPats pats, _, r) ->
                ifPosInRange r (fun _ -> kind)
                |> Option.orElse (
                    typars
                    |> Option.bind (fun (SynValTyparDecls (typars, _, constraints)) ->
                        List.tryPick walkTyparDecl typars
                        |> Option.orElse (List.tryPick walkTypeConstraint constraints)))
                |> Option.orElse (List.tryPick walkPat pats)
            | SynPat.Tuple(pats, _) -> List.tryPick walkPat pats
            | SynPat.Paren(pat, _) -> walkPat pat
            | SynPat.ArrayOrList(_, pats, _) -> List.tryPick walkPat pats
            | SynPat.IsInst(t, _) -> walkType t
            | SynPat.QuoteExpr(e, _) -> walkExpr e
            | _ -> None

        and walkPat = walkPatWithKind None

        and walkBinding (SynBinding.Binding(_, _, _, _, attrs, _, _, pat, returnInfo, e, _, _)) =
            List.tryPick walkAttribute attrs
            |> Option.orElse (walkPat pat)
            |> Option.orElse (walkExpr e)
            |> Option.orElse (
                match returnInfo with
                | Some (SynBindingReturnInfo (t, _, _)) -> walkType t
                | None -> None)

        and walkInterfaceImpl (InterfaceImpl(_, bindings, _)) =
            List.tryPick walkBinding bindings

        and walkIndexerArg = function
            | SynIndexerArg.One e -> walkExpr e
            | SynIndexerArg.Two(e1, e2) -> List.tryPick walkExpr [e1; e2]

        and walkType = function
            | SynType.LongIdent ident -> ifPosInRange ident.Range (fun _ -> Some EntityKind.Type)
            | SynType.App(ty, _, types, _, _, _, _) ->
                walkType ty |> Option.orElse (List.tryPick walkType types)
            | SynType.LongIdentApp(_, _, _, types, _, _, _) -> List.tryPick walkType types
            | SynType.Tuple(ts, _) -> ts |> List.tryPick (fun (_, t) -> walkType t)
            | SynType.Array(_, t, _) -> walkType t
            | SynType.Fun(t1, t2, _) -> walkType t1 |> Option.orElse (walkType t2)
            | SynType.WithGlobalConstraints(t, _, _) -> walkType t
            | SynType.HashConstraint(t, _) -> walkType t
            | SynType.MeasureDivide(t1, t2, _) -> walkType t1 |> Option.orElse (walkType t2)
            | SynType.MeasurePower(t, _, _) -> walkType t
            | _ -> None

        and walkClause (Clause(pat, e1, e2, _, _)) =
            walkPatWithKind (Some EntityKind.Type) pat
            |> Option.orElse (walkExpr e2)
            |> Option.orElse (Option.bind walkExpr e1)

        and walkExprWithKind (parentKind: EntityKind option) = function
            | SynExpr.LongIdent (_, LongIdentWithDots(_, dotRanges), _, r) ->
                match dotRanges with
                | [] when isPosInRange r -> parentKind |> Option.orElse (Some (EntityKind.FunctionOrValue false))
                | firstDotRange :: _  ->
                    let firstPartRange =
                        Range.mkRange "" r.Start (Range.mkPos firstDotRange.StartLine (firstDotRange.StartColumn - 1))
                    if isPosInRange firstPartRange then
                        parentKind |> Option.orElse (Some (EntityKind.FunctionOrValue false))
                    else None
                | _ -> None
            | SynExpr.Paren (e, _, _, _) -> walkExprWithKind parentKind e
            | SynExpr.Quote(_, _, e, _, _) -> walkExprWithKind parentKind e
            | SynExpr.Typed(e, _, _) -> walkExprWithKind parentKind e
            | SynExpr.Tuple(es, _, _) -> List.tryPick (walkExprWithKind parentKind) es
            | SynExpr.ArrayOrList(_, es, _) -> List.tryPick (walkExprWithKind parentKind) es
            | SynExpr.Record(_, _, fields, r) ->
                ifPosInRange r (fun _ ->
                    fields |> List.tryPick (fun (_, e, _) -> e |> Option.bind (walkExprWithKind parentKind)))
            | SynExpr.New(_, t, e, _) -> walkExprWithKind parentKind e |> Option.orElse (walkType t)
            | SynExpr.ObjExpr(ty, _, bindings, ifaces, _, _) ->
                walkType ty
                |> Option.orElse (List.tryPick walkBinding bindings)
                |> Option.orElse (List.tryPick walkInterfaceImpl ifaces)
            | SynExpr.While(_, e1, e2, _) -> List.tryPick (walkExprWithKind parentKind) [e1; e2]
            | SynExpr.For(_, _, e1, _, e2, e3, _) -> List.tryPick (walkExprWithKind parentKind) [e1; e2; e3]
            | SynExpr.ForEach(_, _, _, _, e1, e2, _) -> List.tryPick (walkExprWithKind parentKind) [e1; e2]
            | SynExpr.ArrayOrListOfSeqExpr(_, e, _) -> walkExprWithKind parentKind e
            | SynExpr.CompExpr(_, _, e, _) -> walkExprWithKind parentKind e
            | SynExpr.Lambda(_, _, _, e, _) -> walkExprWithKind parentKind e
            | SynExpr.MatchLambda(_, _, synMatchClauseList, _, _) ->
                List.tryPick walkClause synMatchClauseList
            | SynExpr.Match(_, e, synMatchClauseList, _, _) ->
                walkExprWithKind parentKind e |> Option.orElse (List.tryPick walkClause synMatchClauseList)
            | SynExpr.Do(e, _) -> walkExprWithKind parentKind e
            | SynExpr.Assert(e, _) -> walkExprWithKind parentKind e
            | SynExpr.App(_, _, e1, e2, _) -> List.tryPick (walkExprWithKind parentKind) [e1; e2]
            | SynExpr.TypeApp(e, _, tys, _, _, _, _) ->
                walkExprWithKind (Some EntityKind.Type) e |> Option.orElse (List.tryPick walkType tys)
            | SynExpr.LetOrUse(_, _, bindings, e, _) -> List.tryPick walkBinding bindings |> Option.orElse (walkExprWithKind parentKind e)
            | SynExpr.TryWith(e, _, clauses, _, _, _, _) -> walkExprWithKind parentKind e |> Option.orElse (List.tryPick walkClause clauses)
            | SynExpr.TryFinally(e1, e2, _, _, _) -> List.tryPick (walkExprWithKind parentKind) [e1; e2]
            | SynExpr.Lazy(e, _) -> walkExprWithKind parentKind e
            | Sequentials es -> List.tryPick (walkExprWithKind parentKind) es
            | SynExpr.IfThenElse(e1, e2, e3, _, _, _, _) ->
                List.tryPick (walkExprWithKind parentKind) [e1; e2] |> Option.orElse (match e3 with None -> None | Some e -> walkExprWithKind parentKind e)
            | SynExpr.Ident ident -> ifPosInRange ident.idRange (fun _ -> Some (EntityKind.FunctionOrValue false))
            | SynExpr.LongIdentSet(_, e, _) -> walkExprWithKind parentKind e
            | SynExpr.DotGet(e, _, _, _) -> walkExprWithKind parentKind e
            | SynExpr.DotSet(e, _, _, _) -> walkExprWithKind parentKind e
            | SynExpr.DotIndexedGet(e, args, _, _) -> walkExprWithKind parentKind e |> Option.orElse (List.tryPick walkIndexerArg args)
            | SynExpr.DotIndexedSet(e, args, _, _, _, _) -> walkExprWithKind parentKind e |> Option.orElse (List.tryPick walkIndexerArg args)
            | SynExpr.NamedIndexedPropertySet(_, e1, e2, _) -> List.tryPick (walkExprWithKind parentKind) [e1; e2]
            | SynExpr.DotNamedIndexedPropertySet(e1, _, e2, e3, _) -> List.tryPick (walkExprWithKind parentKind) [e1; e2; e3]
            | SynExpr.TypeTest(e, t, _) -> walkExprWithKind parentKind e |> Option.orElse (walkType t)
            | SynExpr.Upcast(e, t, _) -> walkExprWithKind parentKind e |> Option.orElse (walkType t)
            | SynExpr.Downcast(e, t, _) -> walkExprWithKind parentKind e |> Option.orElse (walkType t)
            | SynExpr.InferredUpcast(e, _) -> walkExprWithKind parentKind e
            | SynExpr.InferredDowncast(e, _) -> walkExprWithKind parentKind e
            | SynExpr.AddressOf(_, e, _, _) -> walkExprWithKind parentKind e
            | SynExpr.JoinIn(e1, _, e2, _) -> List.tryPick (walkExprWithKind parentKind) [e1; e2]
            | SynExpr.YieldOrReturn(_, e, _) -> walkExprWithKind parentKind e
            | SynExpr.YieldOrReturnFrom(_, e, _) -> walkExprWithKind parentKind e
            | SynExpr.LetOrUseBang(_, _, _, _, e1, e2, _) -> List.tryPick (walkExprWithKind parentKind) [e1; e2]
            | SynExpr.DoBang(e, _) -> walkExprWithKind parentKind e
            | SynExpr.TraitCall (ts, sign, e, _) ->
                List.tryPick walkTypar ts
                |> Option.orElse (walkMemberSig sign)
                |> Option.orElse (walkExprWithKind parentKind e)
            | _ -> None

        and walkExpr = walkExprWithKind None

        and walkSimplePat = function
            | SynSimplePat.Attrib (pat, attrs, _) ->
                walkSimplePat pat |> Option.orElse (List.tryPick walkAttribute attrs)
            | SynSimplePat.Typed(pat, t, _) -> walkSimplePat pat |> Option.orElse (walkType t)
            | _ -> None

        and walkField (SynField.Field(attrs, _, _, t, _, _, _, _)) =
            List.tryPick walkAttribute attrs |> Option.orElse (walkType t)

        and walkValSig (SynValSig.ValSpfn(attrs, _, _, t, _, _, _, _, _, _, _)) =
            List.tryPick walkAttribute attrs |> Option.orElse (walkType t)

        and walkMemberSig = function
            | SynMemberSig.Inherit (t, _) -> walkType t
            | SynMemberSig.Member(vs, _, _) -> walkValSig vs
            | SynMemberSig.Interface(t, _) -> walkType t
            | SynMemberSig.ValField(f, _) -> walkField f
            | SynMemberSig.NestedType(SynTypeDefnSig.TypeDefnSig (info, repr, memberSigs, _), _) ->
                walkComponentInfo false info
                |> Option.orElse (walkTypeDefnSigRepr repr)
                |> Option.orElse (List.tryPick walkMemberSig memberSigs)

        and walkMember = function
            | SynMemberDefn.AbstractSlot (valSig, _, _) -> walkValSig valSig
            | SynMemberDefn.Member(binding, _) -> walkBinding binding
            | SynMemberDefn.ImplicitCtor(_, attrs, pats, _, _) ->
                List.tryPick walkAttribute attrs |> Option.orElse (List.tryPick walkSimplePat pats)
            | SynMemberDefn.ImplicitInherit(t, e, _, _) -> walkType t |> Option.orElse (walkExpr e)
            | SynMemberDefn.LetBindings(bindings, _, _, _) -> List.tryPick walkBinding bindings
            | SynMemberDefn.Interface(t, members, _) ->
                walkType t
                |> Option.orElse (members |> Option.bind (List.tryPick walkMember))
            | SynMemberDefn.Inherit(t, _, _) -> walkType t
            | SynMemberDefn.ValField(field, _) -> walkField field
            | SynMemberDefn.NestedType(tdef, _, _) -> walkTypeDefn tdef
            | SynMemberDefn.AutoProperty(attrs, _, _, t, _, _, _, _, e, _, _) ->
                List.tryPick walkAttribute attrs
                |> Option.orElse (Option.bind walkType t)
                |> Option.orElse (walkExpr e)
            | _ -> None

        and walkEnumCase (EnumCase(attrs, _, _, _, _)) = List.tryPick walkAttribute attrs

        and walkUnionCaseType = function
            | SynUnionCaseType.UnionCaseFields fields -> List.tryPick walkField fields
            | SynUnionCaseType.UnionCaseFullType(t, _) -> walkType t

        and walkUnionCase (UnionCase(attrs, _, t, _, _, _)) =
            List.tryPick walkAttribute attrs |> Option.orElse (walkUnionCaseType t)

        and walkTypeDefnSimple = function
            | SynTypeDefnSimpleRepr.Enum (cases, _) -> List.tryPick walkEnumCase cases
            | SynTypeDefnSimpleRepr.Union(_, cases, _) -> List.tryPick walkUnionCase cases
            | SynTypeDefnSimpleRepr.Record(_, fields, _) -> List.tryPick walkField fields
            | SynTypeDefnSimpleRepr.TypeAbbrev(_, t, _) -> walkType t
            | _ -> None

        and walkComponentInfo isModule (ComponentInfo(attrs, typars, constraints, _, _, _, _, r)) =
            if isModule then None else ifPosInRange r (fun _ -> Some EntityKind.Type)
            |> Option.orElse (
                List.tryPick walkAttribute attrs
                |> Option.orElse (List.tryPick walkTyparDecl typars)
                |> Option.orElse (List.tryPick walkTypeConstraint constraints))

        and walkTypeDefnRepr = function
            | SynTypeDefnRepr.ObjectModel (_, defns, _) -> List.tryPick walkMember defns
            | SynTypeDefnRepr.Simple(defn, _) -> walkTypeDefnSimple defn
            | SynTypeDefnRepr.Exception(_) -> None

        and walkTypeDefnSigRepr = function
            | SynTypeDefnSigRepr.ObjectModel (_, defns, _) -> List.tryPick walkMemberSig defns
            | SynTypeDefnSigRepr.Simple(defn, _) -> walkTypeDefnSimple defn
            | SynTypeDefnSigRepr.Exception(_) -> None

        and walkTypeDefn (TypeDefn (info, repr, members, _)) =
            walkComponentInfo false info
            |> Option.orElse (walkTypeDefnRepr repr)
            |> Option.orElse (List.tryPick walkMember members)

        and walkSynModuleDecl isTopLevel (decl: SynModuleDecl) =
            match decl with
            | SynModuleDecl.NamespaceFragment fragment -> walkSynModuleOrNamespace isTopLevel fragment
            | SynModuleDecl.NestedModule(info, _, modules, _, range) ->
                walkComponentInfo true info
                |> Option.orElse (ifPosInRange range (fun _ -> List.tryPick (walkSynModuleDecl false) modules))
            | SynModuleDecl.Open _ -> None
            | SynModuleDecl.Let (_, bindings, _) -> List.tryPick walkBinding bindings
            | SynModuleDecl.DoExpr (_, expr, _) -> walkExpr expr
            | SynModuleDecl.Types (types, _) -> List.tryPick walkTypeDefn types
            | _ -> None

        let res =
            match input with
            | ParsedInput.SigFile _ -> None
            | ParsedInput.ImplFile input -> walkImplFileInput input
        //debug "%A" ast
        res

    type Col = int

    type Scope =
        { Idents: Idents
          Kind: ScopeKind }

    let tryFindInsertionContext (currentLine: int) (ast: ParsedInput) =
        let result: (Scope * Pos) option ref = ref None
        let ns: string[] option ref = ref None
        let modules = ResizeArray<Idents * EndLine * Col>()

        let inline longIdentToIdents ident = ident |> Seq.map (fun x -> string x) |> Seq.toArray

        let addModule (longIdent: LongIdent) endLine col =
            modules.Add(longIdent |> List.map string |> List.toArray, endLine, col)

        let doRange kind (scope: LongIdent) line col =
            if line <= currentLine then
                match !result with
                | None ->
                    result := Some ({ Idents = longIdentToIdents scope; Kind = kind }, Pos.make line col)
                | Some (oldScope, oldPos) ->
                    match kind, oldScope.Kind with
                    | (Namespace | NestedModule | TopModule), OpenDeclaration
                    | _ when oldPos.Line <= line ->
                        result :=
                            Some ({ Idents =
                                        match scope with
                                        | [] -> oldScope.Idents
                                        | _ -> longIdentToIdents scope
                                    Kind = kind },
                                  Pos.make line col)
                    | _ -> ()

        let getMinColumn (decls: SynModuleDecls) =
            match decls with
            | [] -> None
            | firstDecl :: _ ->
                match firstDecl with
                | SynModuleDecl.NestedModule (_, _, _, _, r) -> Some r
                | SynModuleDecl.Let (_, _, r) -> Some r
                | SynModuleDecl.DoExpr (_, _, r) -> Some r
                | SynModuleDecl.Types (_, r) -> Some r
                | SynModuleDecl.Exception (_, r) -> Some r
                | SynModuleDecl.Open (_, r) -> Some r
                | SynModuleDecl.HashDirective (_, r) -> Some r
                | _ -> None
                |> Option.map (fun r -> r.StartColumn)


        let rec walkImplFileInput (ParsedImplFileInput(_, _, _, _, _, moduleOrNamespaceList, _)) =
            List.iter (walkSynModuleOrNamespace []) moduleOrNamespaceList

        and walkSynModuleOrNamespace (parent: LongIdent) (SynModuleOrNamespace(ident, _, isModule, decls, _, _, _, range)) =
            if range.EndLine >= currentLine then
                match isModule, parent, ident with
                | false, _, _ -> ns := Some (longIdentToIdents ident)
                // top level module with "inlined" namespace like Ns1.Ns2.TopModule
                | true, [], _f :: _s :: _ ->
                    let ident = longIdentToIdents ident
                    ns := Some (ident.[0..ident.Length - 2])
                | _ -> ()

                let fullIdent = parent @ ident

                let startLine =
                    if isModule then range.StartLine
                    else range.StartLine - 1

                let scopeKind =
                    match isModule, parent with
                    | true, [] -> TopModule
                    | true, _ -> NestedModule
                    | _ -> Namespace

                doRange scopeKind fullIdent startLine range.StartColumn
                addModule fullIdent range.EndLine range.StartColumn
                List.iter (walkSynModuleDecl fullIdent) decls

        and walkSynModuleDecl (parent: LongIdent) (decl: SynModuleDecl) =
            match decl with
            | SynModuleDecl.NamespaceFragment fragment -> walkSynModuleOrNamespace parent fragment
            | SynModuleDecl.NestedModule(ComponentInfo(_, _, _, ident, _, _, _, _), _, decls, _, range) ->
                let fullIdent = parent @ ident
                addModule fullIdent range.EndLine range.StartColumn
                if range.EndLine >= currentLine then
                    let moduleBodyIdentation = getMinColumn decls |> Option.getOrElse (range.StartColumn + 4)
                    doRange NestedModule fullIdent range.StartLine moduleBodyIdentation
                    List.iter (walkSynModuleDecl fullIdent) decls
            | SynModuleDecl.Open (_, range) -> doRange OpenDeclaration [] range.EndLine (range.StartColumn - 5)
            | SynModuleDecl.HashDirective (_, range) -> doRange HashDirective [] range.EndLine range.StartColumn
            | _ -> ()

        match ast with
        | ParsedInput.SigFile _ -> ()
        | ParsedInput.ImplFile input -> walkImplFileInput input

        let res =
            !result
            |> Option.map (fun (scope, pos) ->
                let ns = !ns |> Option.map longIdentToIdents
                scope, ns, { pos with Line = pos.Line + 1 } )

        let modules =
            modules
            |> Seq.filter (fun (_, endLine, _) -> endLine < currentLine)
            |> Seq.sortBy (fun (m, _, _) -> -m.Length)
            |> Seq.toList

        fun (partiallyQualifiedName: Idents) (requiresQualifiedAccessParent: Idents option, autoOpenParent: Idents option,
                                              entityNamespace: Idents option, entity: Idents) ->
            match res with
            | None -> [||]
            | Some (scope, ns, pos) ->
                Entity.tryCreate(ns, scope.Idents, partiallyQualifiedName, requiresQualifiedAccessParent,
                                 autoOpenParent, entityNamespace, entity)
                |> Array.map (fun e ->
                    e,
                    match modules |> List.filter (fun (m, _, _) -> entity |> Array.startsWith m ) with
                    | [] -> { ScopeKind = scope.Kind; Pos = pos }
                    | (_, endLine, startCol) :: _ ->
                        //printfn "All modules: %A, Win module: %A" modules m
                        let scopeKind =
                            match scope.Kind with
                            | TopModule -> NestedModule
                            | x -> x
                        { ScopeKind = scopeKind; Pos = Pos.make (endLine + 1) startCol })
