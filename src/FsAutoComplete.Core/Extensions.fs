[<AutoOpen>]
module FsAutoComplete.Extensions

open Microsoft.FSharp.Compiler.SourceCodeServices
open System

type FSharpEntity with
    member x.TryGetFullName() =
        Option.attempt (fun _ -> x.TryFullName)
        |> Option.flatten
        |> Option.orTry (fun _ -> Option.attempt (fun _ -> String.Join(".", x.AccessPath, x.DisplayName)))

    member x.TryGetFullNameWithUnderScoreTypes() =
        try
            let name = String.Join(".", x.AccessPath, x.DisplayName)
            if x.GenericParameters.Count > 0 then
              Some (name + "<" + String.concat "," (x.GenericParameters |> Seq.map (fun gp -> gp.DisplayName)) + ">")
            else Some name
        with _ -> None

    member x.UnAnnotate() =
        let rec realEntity (s:FSharpEntity) =
            if s.IsFSharpAbbreviation
            then realEntity s.AbbreviatedType.TypeDefinition
            else s
        realEntity x

    member x.InheritanceDepth() =
        let rec loop (ent:FSharpEntity) l =
            match ent.BaseType with
            | Some bt -> loop (bt.TypeDefinition.UnAnnotate()) l + 1
            | None -> l
        loop x 0

    //TODO: Do we need to unannotate like above?
    member x.AllBaseTypes =
        let rec allBaseTypes (entity:FSharpEntity) =
            [
                match entity.TryFullName with
                | Some _ ->
                    match entity.BaseType with
                    | Some bt ->
                        yield bt
                        if bt.HasTypeDefinition then
                            yield! allBaseTypes bt.TypeDefinition
                    | _ -> ()
                | _ -> ()
            ]
        allBaseTypes x


type FSharpSymbol with
    member this.IsPrivateToFile =
        match this with
        | :? FSharpMemberOrFunctionOrValue as m -> not m.IsModuleValueOrMember
        | :? FSharpEntity as m -> m.Accessibility.IsPrivate
        | :? FSharpGenericParameter -> true
        | :? FSharpUnionCase as m -> m.Accessibility.IsPrivate
        | :? FSharpField as m -> m.Accessibility.IsPrivate
        | _ -> false

    member this.IsInternalToProject =
        match this with
        | :? FSharpParameter -> true
        | :? FSharpMemberOrFunctionOrValue as m -> not m.IsModuleValueOrMember || not m.Accessibility.IsPublic
        | :? FSharpEntity as m -> not m.Accessibility.IsPublic
        | :? FSharpGenericParameter -> true
        | :? FSharpUnionCase as m -> not m.Accessibility.IsPublic
        | :? FSharpField as m -> not m.Accessibility.IsPublic
        | _ -> false

    member x.XmlDocSig =
        match x with
        | :? FSharpMemberOrFunctionOrValue as func -> func.XmlDocSig
        | :? FSharpEntity as fse -> fse.XmlDocSig
        | :? FSharpField as fsf -> fsf.XmlDocSig
        | :? FSharpUnionCase as fsu -> fsu.XmlDocSig
        | :? FSharpActivePatternCase as apc -> apc.XmlDocSig
        | :? FSharpGenericParameter -> ""
        | _ -> ""

type FSharpSymbolUse with
    member this.IsPrivateToFile =
        let isPrivate =
            match this.Symbol with
            | :? FSharpMemberOrFunctionOrValue as m -> not m.IsModuleValueOrMember || m.Accessibility.IsPrivate
            | :? FSharpEntity as m -> m.Accessibility.IsPrivate
            | :? FSharpGenericParameter -> true
            | :? FSharpUnionCase as m -> m.Accessibility.IsPrivate
            | :? FSharpField as m -> m.Accessibility.IsPrivate
            | _ -> false

        let declarationLocation =
            match this.Symbol.SignatureLocation with
            | Some x -> Some x
            | _ ->
                match this.Symbol.DeclarationLocation with
                | Some x -> Some x
                | _ -> this.Symbol.ImplementationLocation

        let declaredInTheFile =
            match declarationLocation with
            | Some declRange -> declRange.FileName = this.RangeAlternate.FileName
            | _ -> false

        isPrivate && declaredInTheFile


type FSharpMemberOrFunctionOrValue with

    member x.IsConstructor = x.CompiledName = ".ctor"

    member x.IsOperatorOrActivePattern =
        let name = x.DisplayName
        if name.StartsWith "( " && name.EndsWith " )" && name.Length > 4
        then name.Substring (2, name.Length - 4) |> String.forall (fun c -> c <> ' ')
        else false

    member x.FullTypeSafe = Option.attempt (fun _ -> x.FullType)

    member x.EnclosingEntitySafe =
        try
            x.DeclaringEntity 
        with :? InvalidOperationException -> None 

type FSharpGenericParameterMemberConstraint with
    member x.IsProperty =
        (x.MemberIsStatic && x.MemberArgumentTypes.Count = 0) ||
        (not x.MemberIsStatic && x.MemberArgumentTypes.Count = 1)



/// Active patterns over `FSharpSymbolUse`.
module SymbolUse =

    let (|ActivePatternCase|_|) (symbol : FSharpSymbolUse) =
        match symbol.Symbol with
        | :? FSharpActivePatternCase as ap-> ActivePatternCase(ap) |> Some
        | _ -> None

    let private attributeSuffixLength = "Attribute".Length

    let (|Entity|_|) (symbol : FSharpSymbolUse) : (FSharpEntity * (* cleanFullNames *) string list) option =
        match symbol.Symbol with
        | :? FSharpEntity as ent ->
            // strip generic parameters count suffix (List`1 => List)
            let cleanFullName =
                // `TryFullName` for type aliases is always `None`, so we have to make one by our own
                if ent.IsFSharpAbbreviation then
                    [ent.AccessPath + "." + ent.DisplayName]
                else
                    ent.TryFullName
                    |> Option.toList
                    |> List.map (fun fullName ->
                        if ent.GenericParameters.Count > 0 && fullName.Length > 2 then
                            fullName.[0..fullName.Length - 3] //Get name without sufix specifing number of generic arguments (for example `'2`)
                        else fullName)

            let cleanFullNames =
                cleanFullName
                |> List.collect (fun cleanFullName ->
                    if ent.IsAttributeType then
                        [cleanFullName; cleanFullName.[0..cleanFullName.Length - attributeSuffixLength - 1]] //Get full name, and name without AttributeSuffix (for example `Literal` instead of `LiteralAttribute`)
                    else [cleanFullName]
                    )
            Some (ent, cleanFullNames)
        | _ -> None


    let (|Field|_|) (symbol : FSharpSymbolUse) =
        match symbol.Symbol with
        | :? FSharpField as field-> Some field
        |  _ -> None

    let (|GenericParameter|_|) (symbol: FSharpSymbolUse) =
        match symbol.Symbol with
        | :? FSharpGenericParameter as gp -> Some gp
        | _ -> None

    let (|MemberFunctionOrValue|_|) (symbol : FSharpSymbolUse) =
        match symbol.Symbol with
        | :? FSharpMemberOrFunctionOrValue as func -> Some func
        | _ -> None

    let (|ActivePattern|_|) = function
        | MemberFunctionOrValue m when m.IsActivePattern -> Some m | _ -> None

    let (|Parameter|_|) (symbol : FSharpSymbolUse) =
        match symbol.Symbol with
        | :? FSharpParameter as param -> Some param
        | _ -> None

    let (|StaticParameter|_|) (symbol : FSharpSymbolUse) =
        #if NO_EXTENSIONTYPING
        Some
        #else
        match symbol.Symbol with
        | :? FSharpStaticParameter as sp -> Some sp
        | _ -> None
        #endif

    let (|UnionCase|_|) (symbol : FSharpSymbolUse) =
        match symbol.Symbol with
        | :? FSharpUnionCase as uc-> Some uc
        | _ -> None

    let (|Constructor|_|) = function
        | MemberFunctionOrValue func when func.IsConstructor || func.IsImplicitConstructor -> Some func
        | _ -> None


    let (|TypeAbbreviation|_|) = function
        | Entity (entity, _) when entity.IsFSharpAbbreviation -> Some entity
        | _ -> None

    let (|Class|_|) = function
        | Entity (entity, _) when entity.IsClass -> Some entity
        | Entity (entity, _) when entity.IsFSharp &&
            entity.IsOpaque &&
            not entity.IsFSharpModule &&
            not entity.IsNamespace &&
            not entity.IsDelegate &&
            not entity.IsFSharpUnion &&
            not entity.IsFSharpRecord &&
            not entity.IsInterface &&
            not entity.IsValueType -> Some entity
        | _ -> None

    let (|Delegate|_|) = function
        | Entity (entity, _) when entity.IsDelegate -> Some entity
        | _ -> None

    let (|Event|_|) = function
        | MemberFunctionOrValue symbol when symbol.IsEvent -> Some symbol
        | _ -> None

    let (|Property|_|) = function
        | MemberFunctionOrValue symbol when symbol.IsProperty || symbol.IsPropertyGetterMethod || symbol.IsPropertySetterMethod -> Some symbol
        | _ -> None

    let inline private notCtorOrProp (symbol:FSharpMemberOrFunctionOrValue) =
        not symbol.IsConstructor && not symbol.IsPropertyGetterMethod && not symbol.IsPropertySetterMethod

    let (|Method|_|) (symbolUse:FSharpSymbolUse) =
        match symbolUse with
        | MemberFunctionOrValue symbol when
            symbol.IsModuleValueOrMember  &&
            not symbolUse.IsFromPattern &&
            not symbol.IsOperatorOrActivePattern &&
            not symbol.IsPropertyGetterMethod &&
            not symbol.IsPropertySetterMethod -> Some symbol
        | _ -> None

    let (|Function|_|) (symbolUse:FSharpSymbolUse) =
        match symbolUse with
        | MemberFunctionOrValue symbol when
            notCtorOrProp symbol  &&
            symbol.IsModuleValueOrMember &&
            not symbol.IsOperatorOrActivePattern &&
            not symbolUse.IsFromPattern ->

            match symbol.FullTypeSafe with
            | Some fullType when fullType.IsFunctionType -> Some symbol
            | _ -> None
        | _ -> None

    let (|Operator|_|) (symbolUse:FSharpSymbolUse) =
        match symbolUse with
        | MemberFunctionOrValue symbol when
            notCtorOrProp symbol &&
            not symbolUse.IsFromPattern &&
            not symbol.IsActivePattern &&
            symbol.IsOperatorOrActivePattern ->

            match symbol.FullTypeSafe with
            | Some fullType when fullType.IsFunctionType -> Some symbol
            | _ -> None
        | _ -> None

    let (|Pattern|_|) (symbolUse:FSharpSymbolUse) =
        match symbolUse with
        | MemberFunctionOrValue symbol when
            notCtorOrProp symbol &&
            not symbol.IsOperatorOrActivePattern &&
            symbolUse.IsFromPattern ->

            match symbol.FullTypeSafe with
            | Some fullType when fullType.IsFunctionType ->Some symbol
            | _ -> None
        | _ -> None


    let (|ClosureOrNestedFunction|_|) = function
        | MemberFunctionOrValue symbol when
            notCtorOrProp symbol &&
            not symbol.IsOperatorOrActivePattern &&
            not symbol.IsModuleValueOrMember ->

            match symbol.FullTypeSafe with
            | Some fullType when fullType.IsFunctionType -> Some symbol
            | _ -> None
        | _ -> None


    let (|Val|_|) = function
        | MemberFunctionOrValue symbol when notCtorOrProp symbol &&
                                            not symbol.IsOperatorOrActivePattern ->
            match symbol.FullTypeSafe with
            | Some _fullType -> Some symbol
            | _ -> None
        | _ -> None

    let (|Enum|_|) = function
        | Entity (entity, _) when entity.IsEnum -> Some entity
        | _ -> None

    let (|Interface|_|) = function
        | Entity (entity, _) when entity.IsInterface -> Some entity
        | _ -> None

    let (|Module|_|) = function
        | Entity (entity, _) when entity.IsFSharpModule -> Some entity
        | _ -> None

    let (|Namespace|_|) = function
        | Entity (entity, _) when entity.IsNamespace -> Some entity
        | _ -> None

    let (|Record|_|) = function
        | Entity (entity, _) when entity.IsFSharpRecord -> Some entity
        | _ -> None

    let (|Union|_|) = function
        | Entity (entity, _) when entity.IsFSharpUnion -> Some entity
        | _ -> None

    let (|ValueType|_|) = function
        | Entity (entity, _) when entity.IsValueType && not entity.IsEnum -> Some entity
        | _ -> None

    let (|ComputationExpression|_|) (symbol:FSharpSymbolUse) =
        if symbol.IsFromComputationExpression then Some symbol
        else None

    let (|Attribute|_|) = function
        | Entity (entity, _) when entity.IsAttributeType -> Some entity
        | _ -> None

module Utils =
    let projectOptionsToParseOptions checkOptions =
        //TODO: Investigate why sometimes SourceFiles are not filled
        let files =
            match checkOptions.SourceFiles with
            | [||] -> checkOptions.OtherOptions |> Array.where (fun n -> n.EndsWith ".fs" || n.EndsWith ".fsx" || n.EndsWith ".fsi")
            | x -> x

        { FSharpParsingOptions.Default with SourceFiles = files}