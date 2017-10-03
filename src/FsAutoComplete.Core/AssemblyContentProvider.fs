/// Original code from VisualFSharpPowerTools project: https://github.com/fsprojects/VisualFSharpPowerTools/blob/master/src/FSharp.Editing/ProjectSystem/AssemblyContentProvider.fs
namespace FsAutoComplete

open System
open Microsoft.FSharp.Compiler.SourceCodeServices

type internal ShortIdent = string
type Idents = ShortIdent[]
type IsAutoOpen = bool
type ModuleKind = { IsAutoOpen: bool; HasModuleSuffix: bool }

type EntityKind =
    | Attribute
    | Type
    | FunctionOrValue of isActivePattern:bool
    | Module of ModuleKind
    override x.ToString() = sprintf "%A" x

type RawEntity =
    { /// Full entity name as it's seen in compiled code (raw FSharpEntity.FullName, FSharpValueOrFunction.FullName).
      FullName: string
      /// Entity name parts with removed module suffixes (Ns.M1Module.M2Module.M3.entity -> Ns.M1.M2.M3.entity)
      /// and replaced compiled names with display names (FSharpEntity.DisplayName, FSharpValueOrFucntion.DisplayName).
      /// Note: *all* parts are cleaned, not the last one.
      CleanedIdents: Idents
      Namespace: Idents option
      IsPublic: bool
      TopRequireQualifiedAccessParent: Idents option
      AutoOpenParent: Idents option
      Kind: EntityKind }
    override x.ToString() = sprintf "%A" x

type AssemblyPath = string
type AssemblyContentType = Public | Full

type Parent =
    { Namespace: Idents option
      RequiresQualifiedAccess: Idents option
      AutoOpen: Idents option
      WithModuleSuffix: Idents option }
    static member Empty =
        { Namespace = None
          RequiresQualifiedAccess = None
          AutoOpen = None
          WithModuleSuffix = None }
    static member RewriteParentIdents (parentIdents: Idents option) (idents: Idents) =
        match parentIdents with
        | Some p when p.Length <= idents.Length ->
            for i in 0..p.Length - 1 do
                idents.[i] <- p.[i]
        | _ -> ()
        idents

    member x.FixParentModuleSuffix (idents: Idents) =
        Parent.RewriteParentIdents x.WithModuleSuffix idents

    member __.FormatEntityFullName (entity: FSharpEntity) =
        // remove number of arguments from generic types
        // e.g. System.Collections.Generic.Dictionary`2 -> System.Collections.Generic.Dictionary
        // and System.Data.Listeners`1.Func -> System.Data.Listeners.Func
        let removeGenericParamsCount (idents: Idents) =
            idents
            |> Array.map (fun ident ->
                if ident.Length > 0 && Char.IsDigit ident.[ident.Length - 1] then
                    let lastBacktickIndex = ident.LastIndexOf '`'
                    if lastBacktickIndex <> -1 then
                        ident.Substring(0, lastBacktickIndex)
                    else ident
                else ident)

        let removeModuleSuffix (idents: Idents) =
            if entity.IsFSharpModule && idents.Length > 0 && hasModuleSuffixAttribute entity then
                let lastIdent = idents.[idents.Length - 1]
                if lastIdent.EndsWith "Module" then
                    idents |> Array.replace (idents.Length - 1) (lastIdent.Substring(0, lastIdent.Length - 6))
                else idents
            else idents

        entity.TryGetFullName()
        |> Option.bind (fun fullName ->
            entity.TryGetFullDisplayName()
            |> Option.map (fun fullDisplayName ->
                fullName,
                fullDisplayName.Split '.'
                |> removeGenericParamsCount
                |> removeModuleSuffix))

module AssemblyContentProvider =
    open System.IO

    type AssemblyContentCacheEntry =
        { FileWriteTime: DateTime
          ContentType: AssemblyContentType
          Entities: RawEntity list }

    [<NoComparison; NoEquality>]
    type IAssemblyContentCache =
        abstract TryGet: AssemblyPath -> AssemblyContentCacheEntry option
        abstract Set: AssemblyPath -> AssemblyContentCacheEntry -> unit

    let private createEntity ns (parent: Parent) (entity: FSharpEntity) =
        parent.FormatEntityFullName entity
        |> Option.map (fun (fullName, cleanIdents) ->
            { FullName = fullName
              CleanedIdents = cleanIdents
              Namespace = ns
              IsPublic = entity.Accessibility.IsPublic
              TopRequireQualifiedAccessParent = parent.RequiresQualifiedAccess |> Option.map parent.FixParentModuleSuffix
              AutoOpenParent = parent.AutoOpen |> Option.map parent.FixParentModuleSuffix
              Kind =
                match entity with
                | TypedAstPatterns.Attribute -> EntityKind.Attribute
                | FSharpModule ->
                    EntityKind.Module
                        { IsAutoOpen = hasAttribute<AutoOpenAttribute> entity.Attributes
                          HasModuleSuffix = hasModuleSuffixAttribute entity }
                | _ -> EntityKind.Type })

    let private traverseMemberFunctionAndValues ns (parent: Parent) (membersFunctionsAndValues: seq<FSharpMemberOrFunctionOrValue>) =
        membersFunctionsAndValues
        |> Seq.collect (fun func ->
            let processIdents fullName idents =
                { FullName = fullName
                  CleanedIdents = parent.FixParentModuleSuffix idents
                  Namespace = ns
                  IsPublic = func.Accessibility.IsPublic
                  TopRequireQualifiedAccessParent =
                        parent.RequiresQualifiedAccess |> Option.map parent.FixParentModuleSuffix
                  AutoOpenParent = parent.AutoOpen |> Option.map parent.FixParentModuleSuffix
                  Kind = EntityKind.FunctionOrValue func.IsActivePattern }

            [ yield! func.TryGetFullDisplayName()
                     |> Option.map (fun fullDisplayName -> processIdents func.FullName (fullDisplayName.Split '.'))
                     |> Option.toList
              (* for
                 [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
                 module M =
                     let (++) x y = ()
                 open M
                 let _ = 1 ++ 2

                 we should return additional RawEntity { FullName = MModule.op_PlusPlus; CleanedIdents = [|"M"; "op_PlusPlus"|] ... }
              *)
              yield! func.TryGetFullCompiledOperatorNameIdents()
                     |> Option.map (fun fullCompiledIdents ->
                          processIdents (fullCompiledIdents |> String.concat ".") fullCompiledIdents)
                     |> Option.toList ])

    let rec private traverseEntity contentType (parent: Parent) (entity: FSharpEntity) =

        seq {
              let shouldTraverse =
#if NO_EXTENSIONTYPING
                true
#else
                not entity.IsProvided
#endif
              if shouldTraverse then
                match contentType, entity.Accessibility.IsPublic with
                | Full, _ | Public, true ->
                    let ns = entity.Namespace |> Option.map (fun x -> x.Split '.') |> Option.orElse parent.Namespace
                    let currentEntity = createEntity ns parent entity

                    match currentEntity with
                    | Some x -> yield x
                    | None -> ()

                    let currentParent =
                        { RequiresQualifiedAccess =
                            parent.RequiresQualifiedAccess
                            |> Option.orElse (
                                if hasAttribute<RequireQualifiedAccessAttribute> entity.Attributes then
                                    parent.FormatEntityFullName entity |> Option.map snd
                                else None)
                          AutoOpen =
                            let isAutoOpen = entity.IsFSharpModule && hasAttribute<AutoOpenAttribute> entity.Attributes
                            match isAutoOpen, parent.AutoOpen with
                            // if parent is also AutoOpen, then keep the parent
                            | true, Some parent -> Some parent
                            // if parent is not AutoOpen, but current entity is, peek the latter as a new AutoOpen module
                            | true, None -> parent.FormatEntityFullName entity |> Option.map snd
                            // if current entity is not AutoOpen, we discard whatever parent was
                            | false, _ -> None

                          WithModuleSuffix =
                            if entity.IsFSharpModule && hasModuleSuffixAttribute entity then
                                currentEntity |> Option.map (fun e -> e.CleanedIdents)
                            else parent.WithModuleSuffix
                          Namespace = ns }

                    if entity.IsFSharpModule then
                        match entity.TryGetMembersFunctionsAndValues with
                        | xs when xs.Count > 0 ->
                            yield! traverseMemberFunctionAndValues ns currentParent xs
                        | _ -> ()

                    for e in (try entity.NestedEntities :> _ seq with _ -> Seq.empty) do
                        yield! traverseEntity contentType currentParent e
                | _ -> () }

    let getAssemblySignatureContent contentType (signature: FSharpAssemblySignature) =
            signature.TryGetEntities()
            |> Seq.collect (traverseEntity contentType Parent.Empty)
            |> Seq.distinct

    let private getAssemblySignaturesContent contentType (assemblies: FSharpAssembly list) =
        assemblies
        |> Seq.collect (fun asm -> getAssemblySignatureContent contentType asm.Contents)
        |> Seq.toList

    let getAssemblyContent (withCache: ((IAssemblyContentCache -> _) -> _) option)
                           contentType (fileName: string option) (assemblies: FSharpAssembly list) =
        let nonProviderGeneratedAssemblies =
            assemblies |> List.filter (fun x -> not x.IsProviderGenerated)
        match nonProviderGeneratedAssemblies, fileName with
        | [], _ -> []
        | assemblies, Some fileName ->
            let fileWriteTime = FileInfo(fileName).LastWriteTime
            match withCache with
            | Some withCache ->
                withCache <| fun cache ->
                    match contentType, cache.TryGet fileName with
                    | _, Some entry
                    | Public, Some entry when entry.FileWriteTime = fileWriteTime -> entry.Entities
                    | _ ->
                        let entities = getAssemblySignaturesContent contentType assemblies
                        cache.Set fileName { FileWriteTime = fileWriteTime; ContentType = contentType; Entities = entities }
                        entities
            | None -> getAssemblySignaturesContent contentType assemblies
        | assemblies, None ->
            getAssemblySignaturesContent contentType assemblies
        |> List.filter (fun entity ->
            match contentType, entity.IsPublic with
            | Full, _ | Public, true -> true
            | _ -> false)