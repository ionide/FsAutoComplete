[<AutoOpen>]
module FsAutoComplete.Extensions

open Microsoft.FSharp.Compiler.SourceCodeServices


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

module Utils =
    let projectOptionsToParseOptions checkOptions =
        //TODO: Investigate why sometimes SourceFiles are not filled
        let files =
            match checkOptions.SourceFiles with
            | [||] -> checkOptions.OtherOptions |> Array.where (fun n -> n.EndsWith ".fs" || n.EndsWith ".fsx" || n.EndsWith ".fsi")
            | x -> x

        { FSharpParsingOptions.Default with SourceFiles = files}
