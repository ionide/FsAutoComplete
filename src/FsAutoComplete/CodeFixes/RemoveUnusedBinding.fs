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


let posBetween (range: Range) tester =
  Pos.posGeq tester range.Start // positions on this one are flipped to simulate Pos.posLte, because that doesn't exist
  && Pos.posGeq range.End tester

type private ReplacmentRangeResult =
| FullBinding of bindingRange: Range
| Pattern of patternRange: Range

type FSharpParseFileResults with
  member private this.TryRangeOfBindingWithHeadPatternWithPos (diagnosticRange: range) =
      this.ParseTree
      |> Option.bind (fun input ->
        AstTraversal.Traverse(diagnosticRange.Start, input, { new AstTraversal.AstVisitorBase<_>() with
            member _.VisitExpr(_, _, defaultTraverse, expr) =
                defaultTraverse expr
            override _.VisitPat(defaultTraverse, pat: SynPat) =
              // if the diagnostic was for this specific pattern in its entirety, then we're don
              if Range.equals pat.Range diagnosticRange then Some (Pattern diagnosticRange)
              else
                match pat with
                | SynPat.Paren (inner, m) ->
                  // otherwise if the pattern inside a parens
                  if Range.rangeContainsRange m diagnosticRange
                  then
                    // explicitly matches
                    if Range.equals inner.Range diagnosticRange
                    // then return the range of the parens, so the entire pattern gets removed
                    then Some (Pattern m)
                    else defaultTraverse inner
                  else defaultTraverse inner
                | pat -> defaultTraverse pat

            override _.VisitBinding(defaultTraverse, binding) =
                match binding with
                | SynBinding.Binding(_, SynBindingKind.NormalBinding, _, _, _, _, _, pat, _, _, _, _) as binding ->
                    // walk the patterns in the binding first, to allow the parameter traversal a chance to fire
                    match defaultTraverse binding with
                    | None ->
                      // otherwise if the diagnostic was in this binding's head pattern then do teh replacement
                      if Range.rangeContainsRange binding.RangeOfHeadPat diagnosticRange then
                          Some (FullBinding binding.RangeOfBindingAndRhs)
                      else
                          // Check if it's an operator
                          match pat with
                          | SynPat.LongIdent(LongIdentWithDots([id], _), _, _, _, _, _) when id.idText.StartsWith("op_") ->
                              if Range.rangeContainsRange id.idRange diagnosticRange then
                                  Some (FullBinding binding.RangeOfBindingAndRhs)
                              else
                                  defaultTraverse binding
                          | _ -> defaultTraverse binding
                    | Some range -> Some range
                | _ -> defaultTraverse binding })
      )

let fix (getParseResults: GetParseResultsForFile): CodeFix =
  Run.ifDiagnosticByCode
    (Set.ofList ["1182"])
    (fun diagnostic codeActionParams -> asyncResult {
      let fileName = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath
      let fcsRange = protocolRangeToRange (codeActionParams.TextDocument.GetFilePath()) diagnostic.Range
      let! tyres, line, lines = getParseResults fileName fcsRange.Start
      let! rangeOfBinding = tyres.GetParseResults.TryRangeOfBindingWithHeadPatternWithPos(fcsRange) |> Result.ofOption (fun () -> "no binding range found")

      match rangeOfBinding with
      | Pattern rangeOfPattern ->
        let protocolRange = fcsRangeToLsp rangeOfPattern

        // the pos at the end of the previous token
        let! endOfPrecedingToken =
          Navigation.walkBackUntilCondition lines protocolRange.Start (System.Char.IsWhiteSpace >> not)
          |> Result.ofOption (fun _ -> "failed to walk")
        // replace from there to the end of the pattern's range
        let replacementRange = { Start = endOfPrecedingToken; End = protocolRange.End }
        return [ { Title = "Remove unused parameter"
                   Edits = [| { Range = replacementRange; NewText = "" } |]
                   File = codeActionParams.TextDocument
                   SourceDiagnostic = Some diagnostic
                   Kind = FixKind.Refactor } ]
      | FullBinding bindingRangeWithPats ->
        let protocolRange = fcsRangeToLsp bindingRangeWithPats
        // the pos at the end of the previous keyword
        let! endOfPrecedingKeyword =
          Navigation.walkBackUntilCondition lines (dec lines protocolRange.Start) (System.Char.IsWhiteSpace >> not)
          |> Result.ofOption (fun _ -> "failed to walk")

        // walk back to the start of the keyword, which is always `let` or `use`
        let keywordStartColumn = decMany lines endOfPrecedingKeyword 3
        let replacementRange = { Start = keywordStartColumn; End = protocolRange.End }
        return [ { Title = "Remove unused binding"
                   Edits = [| { Range = replacementRange; NewText = "" } |]
                   File = codeActionParams.TextDocument
                   SourceDiagnostic = Some diagnostic
                   Kind = FixKind.Refactor } ]
    })
