module FsAutoComplete.CodeFix.RemoveUnusedBinding


open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Navigation
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text


let posBetween (range: Range) tester =
  Position.posGeq tester range.Start // positions on this one are flipped to simulate Pos.posLte, because that doesn't exist
  && Position.posGeq range.End tester

type private ReplacmentRangeResult =
| FullBinding of bindingRange: Range
| Pattern of patternRange: Range

type FSharpParseFileResults with
  member private this.TryRangeOfBindingWithHeadPatternWithPos (diagnosticRange: range) =
    SyntaxTraversal.Traverse(diagnosticRange.Start, this.ParseTree, { new SyntaxVisitorBase<_>() with
        member _.VisitExpr(_, _, defaultTraverse, expr) =
            defaultTraverse expr
        override _.VisitPat(_, defaultTraverse, pat: SynPat) =
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

        override _.VisitBinding(_, defaultTraverse, binding) =
            match binding with
            | SynBinding(kind = SynBindingKind.Normal; headPat = pat) as binding ->
                // walk the patterns in the binding first, to allow the parameter traversal a chance to fire
                match defaultTraverse binding with
                | None ->
                  // otherwise if the diagnostic was in this binding's head pattern then do teh replacement
                  if Range.rangeContainsRange binding.RangeOfHeadPattern diagnosticRange then
                      Some (FullBinding binding.RangeOfBindingWithRhs)
                  else
                      // Check if it's an operator
                      match pat with
                      | SynPat.LongIdent(longDotId = LongIdentWithDots([id], _)) when id.idText.StartsWith("op_") ->
                          if Range.rangeContainsRange id.idRange diagnosticRange then
                              Some (FullBinding binding.RangeOfBindingWithRhs)
                          else
                              defaultTraverse binding
                      | _ -> defaultTraverse binding
                | Some range -> Some range
            | _ -> defaultTraverse binding })

let titleParameter = "Remove unused parameter"
let titleBinding = "Remove unused binding"
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
        return [ { Title = titleParameter
                   Edits = [| { Range = replacementRange; NewText = "" } |]
                   File = codeActionParams.TextDocument
                   SourceDiagnostic = Some diagnostic
                   Kind = FixKind.Refactor } ]
      | FullBinding bindingRangeWithPats ->
        let protocolRange = fcsRangeToLsp bindingRangeWithPats
        // the pos at the end of the previous keyword
        let! walkPos = dec lines protocolRange.Start |> Result.ofOption (fun _ -> "failed to walk")
        let! endOfPrecedingKeyword =
          Navigation.walkBackUntilCondition lines walkPos (System.Char.IsWhiteSpace >> not)
          |> Result.ofOption (fun _ -> "failed to walk")

        // walk back to the start of the keyword, which is always `let` or `use`
        let! keywordStartColumn = decMany lines endOfPrecedingKeyword 3 |> Result.ofOption (fun _ -> "failed to walk")
        let replacementRange = { Start = keywordStartColumn; End = protocolRange.End }
        return [ { Title = titleBinding
                   Edits = [| { Range = replacementRange; NewText = "" } |]
                   File = codeActionParams.TextDocument
                   SourceDiagnostic = Some diagnostic
                   Kind = FixKind.Refactor } ]
    })
