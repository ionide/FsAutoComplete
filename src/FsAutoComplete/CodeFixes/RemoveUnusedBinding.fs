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

type FSharpParseFileResults with
  member this.TryRangeOfBindingWithHeadPatternWithPos pos =
      this.ParseTree
      |> Option.bind (fun input ->
        AstTraversal.Traverse(pos, input, { new AstTraversal.AstVisitorBase<_>() with
            member _.VisitExpr(_, _, defaultTraverse, expr) =
                defaultTraverse expr

            override _.VisitBinding(defaultTraverse, binding) =
                match binding with
                | SynBinding.Binding(_, SynBindingKind.NormalBinding, _, _, _, _, _, pat, _, _, _, _) as binding ->
                    if posBetween binding.RangeOfHeadPat pos then
                        Some binding.RangeOfBindingAndRhs
                    else
                        // Check if it's an operator
                        match pat with
                        | SynPat.LongIdent(LongIdentWithDots([id], _), _, _, _, _, _) when id.idText.StartsWith("op_") ->
                            if posBetween id.idRange pos then
                                Some binding.RangeOfBindingAndRhs
                            else
                                defaultTraverse binding
                        | _ -> defaultTraverse binding
                | _ -> defaultTraverse binding })
      )

let fix (getParseResults: GetParseResultsForFile): CodeFix =
  Run.ifDiagnosticByCode
    (Set.ofList ["1182"])
    (fun diagnostic codeActionParams -> asyncResult {
      let fileName = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath
      let fcsRange = protocolRangeToRange (codeActionParams.TextDocument.GetFilePath()) diagnostic.Range
      let! tyres, line, lines = getParseResults fileName fcsRange.Start
      let! rangeOfBinding = tyres.GetParseResults.TryRangeOfBindingWithHeadPatternWithPos(fcsRange.Start) |> Result.ofOption (fun () -> "no binding range found")

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
    })
