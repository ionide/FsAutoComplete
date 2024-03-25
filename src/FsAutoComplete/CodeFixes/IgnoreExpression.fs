module FsAutoComplete.CodeFix.IgnoreExpression

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FsToolkit.ErrorHandling
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete.CodeFix.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

let title = "Ignore expression"

let fix (getParseResultsForFile: GetParseResultsForFile) : CodeFix =
  Run.ifDiagnosticByCode (set [ "20" ]) (fun diagnostic (codeActionParams: CodeActionParams) ->
    asyncResult {
      let fileName = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath
      let fcsPos = protocolPosToPos codeActionParams.Range.Start

      let! (parseAndCheckResults: ParseAndCheckResults, _line: string, sourceText: IFSACSourceText) =
        getParseResultsForFile fileName fcsPos

      let mDiag =
        protocolRangeToRange parseAndCheckResults.GetParseResults.FileName diagnostic.Range

      let mExprOpt =
        (fcsPos, parseAndCheckResults.GetParseResults.ParseTree)
        ||> ParsedInput.tryPick (fun path node ->
          match node with
          | SyntaxNode.SynExpr(e) when Range.equals mDiag e.Range -> Some(path, e)
          | _ -> None)

      match mExprOpt with
      | None -> return []
      | Some(path, expr) ->
        // Only do single line for now
        if expr.Range.StartLine <> expr.Range.EndLine then
          return []
        else
          let needsParentheses =
            SynExpr.shouldBeParenthesizedInContext sourceText.GetLineString path expr

          let newText =
            let currentText = sourceText.GetSubTextFromRange expr.Range

            if not needsParentheses then
              $"%s{currentText} |> ignore"
            else
              $"(%s{currentText}) |> ignore"

          return
            [ { SourceDiagnostic = None
                Title = title
                File = codeActionParams.TextDocument
                Edits =
                  [| { Range = fcsRangeToLsp expr.Range
                       NewText = newText } |]
                Kind = FixKind.Fix } ]
    })
