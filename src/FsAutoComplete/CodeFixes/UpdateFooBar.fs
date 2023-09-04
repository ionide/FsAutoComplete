module FsAutoComplete.CodeFix.UpdateFooBar

open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FsToolkit.ErrorHandling
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete.CodeFix.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

// The syntax tree can be an intimidating set of types to work with.
// It is a tree structure but it consists out of many different types.
// See https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntax.html
// It can be useful to inspect a syntax tree via a code sample using https://fsprojects.github.io/fantomas-tools/#/ast
// For example `let a b c = ()` in
// https://fsprojects.github.io/fantomas-tools/#/ast?data=N4KABGBEAmCmBmBLAdrAzpAXFSAacUiaAYmolmPAIYA2as%2BEkAxgPZwWQ2wAuYVYAEZhmYALxgAFAEo8BSLAAeAByrJoFHgCcArrBABfIA
// Let's say we want to find the (FCS) range for identifier `a`.
let visitSyntaxTree
  (cursor: FSharp.Compiler.Text.pos)
  (tree: ParsedInput)
  =
  // We will use a syntax visitor to traverse the tree from the top to the node of interest.
  // See https://github.com/dotnet/fsharp/blob/main/src/Compiler/Service/ServiceParseTreeWalk.fsi
  // We implement the different members of interest and allow the default traversal to move to the lower levels we care about.
  let visitor =
    // A visitor will report the first item it finds.
    // Think of it as `List.tryPick`
    // It is not an ideal solution to find all nodes inside a tree, be aware of that.
    // For example finding all function names.
    { new SyntaxVisitorBase<FSharp.Compiler.Text.range>() with
        // We know that `a` will be part of a `SynPat.LongIdent`
        // This was visible in the online tool.
        member _.VisitPat(path, defaultTraverse, synPat) =
          match synPat with
          | SynPat.LongIdent(longDotId = SynLongIdent(id = [ functionNameIdent ])) ->
            // When our code fix operates on the user's code there is no way of knowing what will be inside the syntax tree.
            // So we need to be careful and verify that the pattern is indeed matching the position of the cursor.
            if FSharp.Compiler.Text.Range.rangeContainsPos functionNameIdent.idRange cursor then
              Some functionNameIdent.idRange
            else
              None
          | _ -> None }

  // Invoke the visitor and kick off the traversal.
  SyntaxTraversal.Traverse(cursor, tree, visitor)

// TODO: add proper title for code fix
let title = "UpdateFooBar Codefix"

let fix
  (getParseResultsForFile: GetParseResultsForFile)
  : CodeFix =
  fun (codeActionParams: CodeActionParams) ->
    asyncResult {
      // Most code fixes have some general setup.
      // We initially want to detect the state of the current code and whether we can propose any text edits to the user.

      let fileName = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath
      // The converted LSP start position to an FCS start position.
      let fcsPos = protocolPosToPos codeActionParams.Range.Start
      // The syntax tree and typed tree, current line and sourceText of the current file.
      let! (parseAndCheckResults:ParseAndCheckResults, line:string, sourceText:IFSACSourceText) =
        getParseResultsForFile fileName fcsPos

      // As an example, we want to check whether the users cursor is inside a function definition name.
      // We will traverse the syntax tree to verify this is the case.
      match visitSyntaxTree fcsPos parseAndCheckResults.GetParseResults.ParseTree with
      | None ->
        // The cursor is not in a position we are interested in.
        // This code fix should not trigger any suggestions so we return an empty list.
        return []
      | Some mBindingName ->
        // It turns out we are inside a let binding and we have the range of the function name.
        // Just for fun, we want to detect if there is a matching typed tree symbol present for the current name.
        // We could have passed the function name from the syntax visitor, instead will we grab it from the source text.
        let! functionName = sourceText.GetText mBindingName
        // FSharpSymbolUse is reflecting the typed tree.
        // See https://fsharp.github.io/fsharp-compiler-docs/fcs/symbols.html
        let symbolUse: FSharp.Compiler.CodeAnalysis.FSharpSymbolUse option =
          parseAndCheckResults.GetCheckResults.GetSymbolUseAtLocation(mBindingName.EndLine, mBindingName.EndColumn, line, [ functionName ])

        let hasFunctionDefinitionSymbol =
          match symbolUse with
          | None -> false
          | Some symbolUse ->
            // We want to verify the found symbol is indeed a definition of a function
            match symbolUse.Symbol with
            | :? FSharpMemberOrFunctionOrValue -> true
            | _ -> false

        if not hasFunctionDefinitionSymbol then
          return []
        else
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
                      Range = fcsRangeToLsp mBindingName
                      NewText = "Text replaced by UpdateFooBar"
                     }
                  |]
                  Kind = FixKind.Fix
            }
          ]
    }
