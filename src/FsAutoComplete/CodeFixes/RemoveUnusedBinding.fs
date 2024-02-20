module FsAutoComplete.CodeFix.RemoveUnusedBinding

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Navigation
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

type private ReplacementRangeResult =
  | FullBinding of bindingRange: Range
  | Pattern of patternRange: Range

let titleParameter = "Remove unused parameter"
let titleBinding = "Remove unused binding"

let fix (getParseResults: GetParseResultsForFile) : CodeFix =
  Run.ifDiagnosticByCode (Set.ofList [ "1182" ]) (fun diagnostic codeActionParams ->
    asyncResult {
      let fileName = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath

      let fcsRange =
        protocolRangeToRange (codeActionParams.TextDocument.GetFilePath()) diagnostic.Range

      let! tyres, _line, lines = getParseResults fileName fcsRange.Start

      let! rangeOfBinding =
        (fcsRange.Start, tyres.GetParseResults.ParseTree)
        ||> ParsedInput.tryPick (fun path node ->
          let (|LongIdentRange|) (idents: Ident list) = (Range.range0, idents) ||> List.fold (fun acc ident -> Range.unionRanges acc ident.idRange)

          match node, path with
          | SyntaxNode.SynPat pat, SyntaxNode.SynBinding(SynBinding(kind = SynBindingKind.Normal; headPat = SynPat.Named(range = nameRange) as headPat) as binding) :: _
          | SyntaxNode.SynPat pat, SyntaxNode.SynBinding(SynBinding(kind = SynBindingKind.Normal; headPat = SynPat.LongIdent(longDotId = SynLongIdent(id = LongIdentRange nameRange)) as headPat) as binding) :: _ ->
            if obj.ReferenceEquals(pat, headPat)
              && Range.rangeContainsRange nameRange fcsRange then
              Some(FullBinding binding.RangeOfBindingWithRhs)
            else
              None

          | SyntaxNode.SynPat pat, _ when Range.rangeContainsRange pat.Range fcsRange -> Some(Pattern pat.Range)

          | _ -> None)
        |> Result.ofOption (fun () -> "no binding range found")

      match rangeOfBinding with
      | Pattern rangeOfPattern ->
        let protocolRange = fcsRangeToLsp rangeOfPattern

        // the pos at the end of the previous token
        let! endOfPrecedingToken =
          Navigation.walkBackUntilCondition lines protocolRange.Start (System.Char.IsWhiteSpace >> not)
          |> Result.ofOption (fun _ -> "failed to walk")
        // replace from there to the end of the pattern's range
        let replacementRange =
          { Start = endOfPrecedingToken
            End = protocolRange.End }

        return
          [ { Title = titleParameter
              Edits =
                [| { Range = replacementRange
                     NewText = "" } |]
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
        let! keywordStartColumn =
          decMany lines endOfPrecedingKeyword 3
          |> Result.ofOption (fun _ -> "failed to walk")

        let replacementRange =
          { Start = keywordStartColumn
            End = protocolRange.End }

        return
          [ { Title = titleBinding
              Edits =
                [| { Range = replacementRange
                     NewText = "" } |]
              File = codeActionParams.TextDocument
              SourceDiagnostic = Some diagnostic
              Kind = FixKind.Refactor } ]
    })
