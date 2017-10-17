//Based on VF# implementation - https://github.com/Microsoft/visualfsharp/blob/master/vsintegration/src/FSharp.Editor/Diagnostics/UnusedOpensDiagnosticAnalyzer.fs
namespace FsAutoComplete

open FsAutoComplete
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices

module UnusedOpensAnalyzer =
    let rec visitSynModuleOrNamespaceDecls (parent: Ast.LongIdent) decls : (Set<string> * range) list =
        [ for decl in decls do
            match decl with
            | SynModuleDecl.Open(LongIdentWithDots.LongIdentWithDots(id = longId), range) ->
                yield
                    set [ yield (longId |> List.map(fun l -> l.idText) |> String.concat ".")
                          // `open N.M` can open N.M module from parent module as well, if it's non empty
                          if not (List.isEmpty parent) then
                            yield (parent @ longId |> List.map(fun l -> l.idText) |> String.concat ".") ], range
            | SynModuleDecl.NestedModule(SynComponentInfo.ComponentInfo(longId = longId),_, decls,_,_) ->
                yield! visitSynModuleOrNamespaceDecls longId decls
            | _ -> () ]

    let getOpenStatements = function
        | ParsedInput.ImplFile (ParsedImplFileInput(modules = modules)) ->
            [ for md in modules do
                let SynModuleOrNamespace(longId = longId; decls = decls) = md
                yield! visitSynModuleOrNamespaceDecls longId decls ]
        | _ -> []

    let getAutoOpenAccessPath (ent:FSharpEntity) =
        // Some.Namespace+AutoOpenedModule+Entity

        // HACK: I can't see a way to get the EnclosingEntity of an Entity
        // Some.Namespace + Some.Namespace.AutoOpenedModule are both valid
        ent.TryFullName |> Option.bind(fun _ ->
            if (not ent.IsNamespace) && ent.QualifiedName.Contains "+" then
                Some ent.QualifiedName.[0..ent.QualifiedName.IndexOf "+" - 1]
            else
                None)

    let entityNamespace (entOpt: FSharpEntity option) =
        match entOpt with
        | Some ent ->
            if ent.IsFSharpModule then
                [ yield Some ent.QualifiedName
                  yield Some ent.LogicalName
                  yield Some ent.AccessPath
                  yield Some ent.FullName
                  yield Some ent.DisplayName
                  yield ent.TryGetFullDisplayName()
                  if ent.HasFSharpModuleSuffix then
                    yield Some (ent.AccessPath + "." + ent.DisplayName)]
            else
                [ yield ent.Namespace
                  yield Some ent.AccessPath
                  yield getAutoOpenAccessPath ent
                  for path in ent.AllCompilationPaths do
                    yield Some path
                ]
        | None -> []

    let getTextForRange (lines : string[]) (range : range) : string option =
        try
            let line = lines.[max 0 (range.StartLine - 1)]
            line
            |> String.toCharArray
            |> Seq.skip range.StartColumn
            |> Seq.take (range.EndColumn - range.StartColumn)
            |> string
            |> Some
        with _ -> None

    let symbolIsFullyQualified (sourceText: string[]) (sym: FSharpSymbolUse) (fullName: string) =
        match getTextForRange sourceText sym.RangeAlternate with
        | Some span -> span = fullName
        | None -> false

    let getUnusedOpens (sourceText: string[]) (parsedInput: ParsedInput) (symbolUses: FSharpSymbolUse[]) =

        let getPartNamespace (symbolUse: FSharpSymbolUse) (fullName: string) =
            let length = symbolUse.RangeAlternate.EndColumn - symbolUse.RangeAlternate.StartColumn
            let lengthDiff = fullName.Length - length - 2
            if lengthDiff <= 0 || lengthDiff > fullName.Length - 1 then None
            else Some fullName.[0..lengthDiff]

        let getPossibleNamespaces (symbolUse: FSharpSymbolUse) : string list =
            let isQualified = symbolIsFullyQualified sourceText symbolUse


            match symbolUse with
            | SymbolUse.Entity (ent, cleanFullNames) when not (cleanFullNames |> List.exists isQualified) ->
                Some (cleanFullNames, Some ent)
            | SymbolUse.Field f when not (isQualified f.FullName) ->
                Some ([f.FullName], Some f.DeclaringEntity)
            | SymbolUse.MemberFunctionOrValue mfv when not (isQualified mfv.FullName) ->
                Some ([mfv.FullName], mfv.EnclosingEntity)
            | SymbolUse.Operator op when not (isQualified op.FullName) ->
                Some ([op.FullName], op.EnclosingEntity)
            | SymbolUse.ActivePattern ap when not (isQualified ap.FullName) ->
                Some ([ap.FullName], ap.EnclosingEntity)
            | SymbolUse.ActivePatternCase apc when not (isQualified apc.FullName) ->
                Some ([apc.FullName], apc.Group.EnclosingEntity)
            | SymbolUse.UnionCase uc when not (isQualified uc.FullName) ->
                Some ([uc.FullName], Some uc.ReturnType.TypeDefinition)
            | SymbolUse.Parameter p when not (isQualified p.FullName) && p.Type.HasTypeDefinition ->
                Some ([p.FullName], Some p.Type.TypeDefinition)
            | _ -> None

            |> Option.map (fun (fullNames, declaringEntity) ->
                [ for name in fullNames do
                    yield getPartNamespace symbolUse name
                  yield! entityNamespace declaringEntity ]
            ) |> Option.toList |> List.concat |> List.choose id

        let namespacesInUse =
            symbolUses
            |> Seq.filter (fun (s: FSharpSymbolUse) -> not s.IsFromDefinition)
            |> Seq.collect getPossibleNamespaces
            |> Set.ofSeq

        let filter list: (Set<string> * range) list =
            let rec filterInner acc list (seenNamespaces: Set<string>) =
                let notUsed ns = not (namespacesInUse.Contains ns) || seenNamespaces.Contains ns
                match list with
                | (ns, range) :: xs when ns |> Set.forall notUsed ->
                    filterInner ((ns, range) :: acc) xs (seenNamespaces |> Set.union ns)
                | (ns, _) :: xs ->
                    filterInner acc xs (seenNamespaces |> Set.union ns)
                | [] -> List.rev acc
            filterInner [] list Set.empty

        let openStatements = getOpenStatements parsedInput
        openStatements |> filter |> List.map snd