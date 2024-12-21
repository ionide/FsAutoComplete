module FsAutoComplete.CodeFix.AddMissingSeq

open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FsToolkit.ErrorHandling
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete.CodeFix.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

let title = "Add missing 'seq'"

let fix
  (getParseResultsForFile: GetParseResultsForFile)
  : CodeFix =
  Run.ifDiagnosticByCode (Set.ofList [ "3873"; "740" ]) (fun _ codeActionParams ->
    asyncResult {
      // Most code fixes have some general setup.
      // We initially want to detect the state of the current code and whether we can propose any text edits to the user.

      let fileName = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath
      // The converted LSP start position to an FCS start position.
      let fcsPos = protocolPosToPos codeActionParams.Range.Start
      // The syntax tree and typed tree, current line and sourceText of the current file.
      let! (parseAndCheckResults:ParseAndCheckResults, _line:string, sourceText:IFSACSourceText) =
        getParseResultsForFile fileName fcsPos

      // The syntax tree can be an intimidating set of types to work with.
      // It is a tree structure but it consists out of many different types.
      // See https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntax.html
      // It can be useful to inspect a syntax tree via a code sample using https://fsprojects.github.io/fantomas-tools/#/ast
      // For example `let a b c = ()` in
      // https://fsprojects.github.io/fantomas-tools/#/ast?data=N4KABGBEAmCmBmBLAdrAzpAXFSAacUiaAYmolmPAIYA2as%2BEkAxgPZwWQ2wAuYVYAEZhmYALxgAFAEo8BSLAAeAByrJoFHgCcArrBABfIA
      // Let's say we want to find the (FCS) range for identifier `a` if the user's cursor is inside the function name.
      // We will query the syntax tree to verify this is the case.
      let maybeCERange =
        (fcsPos, parseAndCheckResults.GetParseResults.ParseTree)
        ||> ParsedInput.tryPick (fun _path node ->
          match node with
          // We know that `a` will be part of a `SynPat.LongIdent`
          // This was visible in the online tool.
          | SyntaxNode.SynExpr(SynExpr.ComputationExpr _) as e when
            // When our code fix operates on the user's code there is no way of knowing what will be inside the syntax tree.
            // So we need to be careful and verify that the pattern is indeed matching the position of the cursor.
            Range.rangeContainsPos e.Range fcsPos
            ->
            Some  e.Range
          | _ -> None)

      match maybeCERange with
      | None ->
        // The cursor is not in a position we are interested in.
        // This code fix should not trigger any suggestions so we return an empty list.
        return []
      | Some ceExprRange ->
        // It turns out we are inside a let binding and we have the range of the function name.
        // Just for fun, we want to detect if there is a matching typed tree symbol present for the current name.
        // We could have passed the function name from the syntax visitor, instead will we grab it from the source text.
        let! sourceText = sourceText.GetText ceExprRange
        // FSharpSymbolUse is reflecting the typed tree.
        // See https://fsharp.github.io/fsharp-compiler-docs/fcs/symbols.html
        // let symbolUse: FSharp.Compiler.CodeAnalysis.FSharpSymbolUse option =
        //   parseAndCheckResults.GetCheckResults.GetSymbolUseAtLocation(ceExprRange.EndLine, ceExprRange.EndColumn, line, [ functionName ])

        // let hasCEExprDefinitionSymbol =
        //   match symbolUse with
        //   | None -> false
        //   | Some symbolUse ->
        //     // We want to verify the found symbol is indeed a definition of a function
        //     match symbolUse.Symbol with
        //     | :? FSharpMemberOrFunctionOrValue -> true
        //     | _ -> false

        // if not hasCEExprDefinitionSymbol then
        //   return []
        // else
          // Return a list of Fix records for when the code fix is applicable.
        return [
            {
                  SourceDiagnostic = None
                  Title = title
                  File = codeActionParams.TextDocument
                  // Based on conditional logic, you typically want to suggest a text edit to the user.
                  Edits = [|
                    {
                      // When dealing with FCS, we typically want to use the FCS flavour of range.
                      // However, to interact correctly with the LSP protocol, we need to return an LSP range.
                      Range = fcsRangeToLsp ceExprRange
                      NewText = $"seq { sourceText }"
                     }
                  |]
                  Kind = FixKind.Fix
            }
          ]
    })
