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

module Utils =
    let projectOptionsToParseOptions checkOptions =
        //TODO: Investigate why sometimes SourceFiles are not filled
        let files =
            match checkOptions.SourceFiles with
            | [||] -> checkOptions.OtherOptions |> Array.where (fun n -> n.EndsWith ".fs" || n.EndsWith ".fsx" || n.EndsWith ".fsi")
            | x -> x

        { FSharpParsingOptions.Default with SourceFiles = files}
