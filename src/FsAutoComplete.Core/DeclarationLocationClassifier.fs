namespace FsAutoComplete

open FsAutoComplete
open FSharp.Compiler.Symbols
open FSharp.Compiler.CodeAnalysis
open System.Collections.Generic

module DeclarationLocationClassifier =
    type DeclarationLocation =
        | Unknown
        | Local
        | ProjectReference
        | FSharpCore
        | External

    let getDeclarationLocationClassification (symbolsUses: FSharpSymbolUse seq) =
        symbolsUses
        |> Seq.map (fun n ->
            if n.IsPrivateToFile then
                n.Range, Local
            elif n.Symbol.IsInternalToProject then
                n.Range, Local
            elif n.Symbol.Assembly.SimpleName = "FSharp.Core" then
                n.Range, FSharpCore
            else
                //TODO: Make ProjectReference vs External vs Local [in case of public symbols] decision
                //based on the assembly name or something
                n.Range, External
            )


