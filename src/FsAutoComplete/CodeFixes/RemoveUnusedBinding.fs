module FsAutoComplete.CodeFix.RemoveUnusedBinding


open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Navigation
open FsAutoComplete.CodeFix.Types
open LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.SyntaxTree
open FSharp.Compiler.Text

type FSharpParseFileResults with
  member this.TryRangeOfBindingWithHeadPatternWithPos pos =
      this.ParseTree
      |> Option.bind (fun input ->
        AstTraversal.Traverse(pos, input, { new AstTraversal.AstVisitorBase<_>() with
            member _.VisitExpr(_, _, defaultTraverse, expr) =
                defaultTraverse expr

            override _.VisitBinding(defaultTraverse, binding) =
                match binding with
                | SynBinding.Binding(_, SynBindingKind.NormalBinding, _, _, _, _, _, _, _, _, _, _) as binding ->
                    if Pos.posEq binding.RangeOfHeadPat.Start pos then
                        Some binding.RangeOfBindingAndRhs
                    else
                        defaultTraverse binding
                | _ -> defaultTraverse binding })
      )

  member this.TryRangeOfBindingStartingAtPos pos =
      this.ParseTree
      |> Option.bind (fun input ->
        AstTraversal.Traverse(pos, input, { new AstTraversal.AstVisitorBase<_>() with
            member _.VisitExpr(_, _, defaultTraverse, expr) =
                defaultTraverse expr

            override _.VisitBinding(defaultTraverse, binding) =
                match binding with
                | SynBinding.Binding(_, SynBindingKind.NormalBinding, _, _, _, _, _, _, _, _, range, _) as binding ->
                    if Pos.posEq range.Start pos then
                        Some binding.RangeOfBindingAndRhs
                    else
                        defaultTraverse binding
                | _ -> defaultTraverse binding })
      )

let fix (getRangeText: GetRangeText) (getParseResults: GetParseResultsForFile): CodeFix =
  Run.ifDiagnosticByCode
    (Set.ofList ["1182"])
    (fun diagnostic codeActionParams -> asyncResult {
      let fileName = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath
      let! ident = getRangeText fileName diagnostic.Range
      // Prefixing operators and backticked identifiers does not make sense.
      // We have to use the additional check for backtickes because `IsOperatorOrBacktickedName` operates on display names
      // where backtickes are replaced with parens.
      if not (PrettyNaming.IsOperatorOrBacktickedName ident) && not (ident.StartsWith "``")
      then
        let fcsRange = protocolRangeToRange (codeActionParams.TextDocument.GetFilePath()) diagnostic.Range
        let! tyres, line, lines = getParseResults fileName fcsRange.Start
        // find the symbol use matching the current member
        match tyres.TryGetSymbolUse fcsRange.Start line with
        | None ->
          return []
        | Some symbolUse ->
          match symbolUse.Symbol with
          | :? FSharpMemberOrFunctionOrValue as mfv ->
              let! rangeOfBinding =
                  if mfv.IsValue then
                      tyres.GetParseResults.TryRangeOfBindingWithHeadPatternWithPos(symbolUse.RangeAlternate.Start)
                  else
                      tyres.GetParseResults.TryRangeOfBindingWithHeadPatternWithPos(symbolUse.RangeAlternate.Start)

                  |> Result.ofOption (fun () -> "no binding range found")

              let protocolRange = fcsRangeToLsp rangeOfBinding

              // the pos at the end of the keyword
              let! endOfPrecedingKeyword =
                Navigation.walkBackUntilCondition lines (dec lines protocolRange.Start) (System.Char.IsWhiteSpace)
                |> Result.ofOption (fun _ -> "failed to walk")

              // walk back to the start of the keyword, which is always `let` or `use`
              let keywordStartColumn = decMany lines endOfPrecedingKeyword 3
              let replacementRange = { Start = keywordStartColumn; End = protocolRange.End }
              return [ { Title = "Remove unused binding"
                         Edits = [| { Range = replacementRange; NewText = "" } |]
                         File = codeActionParams.TextDocument
                         SourceDiagnostic = Some diagnostic
                         Kind = Refactor } ]
          | _ -> return []

      else
        return []
    })
