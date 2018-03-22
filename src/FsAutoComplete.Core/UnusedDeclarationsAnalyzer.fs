//Based on VF# implementation - https://github.com/Microsoft/visualfsharp/blob/master/vsintegration/src/FSharp.Editor/Diagnostics/UnusedDeclarationsAnalyzer.fs
namespace FsAutoComplete

open FsAutoComplete
open Microsoft.FSharp.Compiler.SourceCodeServices

module UnusedDeclarationsAnalyzer =
    open System.Collections.Generic
    let isPotentiallyUnusedDeclaration (symbol: FSharpSymbol) : bool =
        match symbol with
        // Determining that a record, DU or module is used anywhere requires inspecting all their enclosed entities (fields, cases and func / vals)
        // for usages, which is too expensive to do. Hence we never gray them out.
        | :? FSharpEntity as e when e.IsFSharpRecord || e.IsFSharpUnion || e.IsInterface || e.IsFSharpModule || e.IsClass -> false
        // FCS returns inconsistent results for override members; we're skipping these symbols.
        | :? FSharpMemberOrFunctionOrValue as f when
                f.IsOverrideOrExplicitInterfaceImplementation ||
                f.IsBaseValue ||
                f.IsConstructor -> false
        // Usage of DU case parameters does not give any meaningful feedback; we never gray them out.
        | :? FSharpParameter -> false
        // Same as DUs
        | :? FSharpActivePatternCase -> false
        | _ -> true

    let getUnusedDeclarationRanges (symbolsUses: FSharpSymbolUse[]) (isScript: bool) =
        let definitions =
            symbolsUses
            |> Array.filter (fun su ->
                su.IsFromDefinition &&
                su.Symbol.DeclarationLocation.IsSome &&
                (isScript || su.IsPrivateToFile) &&
                not (su.Symbol.DisplayName.StartsWith "_") &&
                isPotentiallyUnusedDeclaration su.Symbol)

        let usages =
            let usages =
                symbolsUses
                |> Array.filter (fun su -> not su.IsFromDefinition)
                |> Array.choose (fun su -> su.Symbol.DeclarationLocation)
            HashSet(usages)

        let unusedRanges =
            definitions
            |> Array.map (fun defSu -> defSu, usages.Contains defSu.Symbol.DeclarationLocation.Value)
            |> Array.groupBy (fun (defSu, _) -> defSu.RangeAlternate)
            |> Array.filter (fun (_, defSus) -> defSus |> Array.forall (fun (_, isUsed) -> not isUsed))
            |> Array.choose (fun (range, defSus) ->

                try
                    let (symbol, _) = defSus.[0]
                    match symbol.Symbol with
                    | :? FSharpMemberOrFunctionOrValue as func when func.IsMemberThisValue -> Some (range, true)
                    | :? FSharpMemberOrFunctionOrValue as func when func.IsValue -> Some (range, false)
                    | _ -> None
                with
                | _ -> None)

        unusedRanges

