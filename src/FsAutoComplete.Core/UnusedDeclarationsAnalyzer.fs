//Based on VF# implementation - https://github.com/Microsoft/visualfsharp/blob/master/vsintegration/src/FSharp.Editor/Diagnostics/UnusedDeclarationsAnalyzer.fs
namespace FsAutoComplete

open FsAutoComplete
open FSharp.Compiler.Symbols
open FSharp.Compiler.CodeAnalysis

module UnusedDeclarationsAnalyzer =
  open System.Collections.Generic

  let isPotentiallyUnusedDeclaration (symbol: FSharpSymbol) : bool =
    match symbol with
    // Determining that a record, DU or module is used anywhere requires inspecting all their enclosed entities (fields, cases and func / vals)
    // for usages, which is too expensive to do. Hence we never gray them out.
    | :? FSharpEntity as e when
      e.IsFSharpRecord
      || e.IsFSharpUnion
      || e.IsInterface
      || e.IsFSharpModule
      || e.IsClass
      ->
      false
    // FCS returns inconsistent results for override members; we're skipping these symbols.
    | :? FSharpMemberOrFunctionOrValue as f when
      f.IsOverrideOrExplicitInterfaceImplementation
      || f.IsBaseValue
      || f.IsConstructor
      ->
      false
    // Usage of DU case parameters does not give any meaningful feedback; we never gray them out.
    | :? FSharpParameter -> false
    // Same as DUs
    | :? FSharpActivePatternCase -> false
    | _ -> true

  let getUnusedDeclarationRanges (symbolsUses: FSharpSymbolUse seq) (isScript: bool) =
    let definitions =
      symbolsUses
      |> Seq.filter (fun su ->
        su.IsFromDefinition
        && su.Symbol.DeclarationLocation.IsSome
        && (isScript || su.IsPrivateToFile)
        && not (su.Symbol.DisplayName.StartsWith "_")
        && isPotentiallyUnusedDeclaration su.Symbol)

    let usages =
      let usages =
        symbolsUses
        |> Seq.choose (fun su ->
          if not su.IsFromDefinition then
            su.Symbol.DeclarationLocation
          else
            None)

      HashSet(usages)

    let unusedRanges =
      definitions
      |> Seq.map (fun defSu -> defSu, usages.Contains defSu.Symbol.DeclarationLocation.Value)
      |> Seq.groupBy (fun (defSu, _) -> defSu.Range)
      |> Seq.filter (fun (_, defSus) ->
        defSus
        |> Seq.forall (fun (_, isUsed) -> not isUsed))
      |> Seq.choose (fun (range, defSus) ->

        try
          let (symbol, _) = Seq.head defSus

          match symbol.Symbol with
          | :? FSharpMemberOrFunctionOrValue as func when func.IsMemberThisValue -> Some(range, true)
          | :? FSharpMemberOrFunctionOrValue as func when func.IsValue -> Some(range, false)
          | _ -> None
        with
        | _ -> None)

    unusedRanges
