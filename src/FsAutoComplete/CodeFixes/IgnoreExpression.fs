module FsAutoComplete.CodeFix.IgnoreExpression

open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text
open FSharp.UMX
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
      let mDiag = protocolRangeToRange (UMX.untag fileName) diagnostic.Range

      // Only do single line for now
      if mDiag.StartLine <> mDiag.EndLine then
        return []
      else

      let! (parseAndCheckResults: ParseAndCheckResults, _line: string, sourceText: IFSACSourceText) =
        getParseResultsForFile fileName fcsPos

      let mExprOpt =
        (fcsPos, parseAndCheckResults.GetParseResults.ParseTree)
        ||> ParsedInput.tryPick (fun path node ->
          match node with
          | SyntaxNode.SynExpr(e) when Range.equals mDiag e.Range -> Some(path, e)
          | _ -> None)

      match mExprOpt with
      | None -> return []
      | Some(path, expr) ->

      let needsParentheses =
        let appPipe, appIgnore =
          let lineNumber = expr.Range.StartLine

          let mkRangeInFile (offset: int) (length: int) : range =
            Range.mkRange
              expr.Range.FileName
              (Position.mkPos lineNumber (expr.Range.EndColumn + offset))
              (Position.mkPos lineNumber (expr.Range.EndColumn + offset + length))

          let mPipe = mkRangeInFile 2 2
          let mIgnore = mkRangeInFile 5 6

          let appPipe =
            SynExpr.App(
              ExprAtomicFlag.NonAtomic,
              true,
              SynExpr.LongIdent(
                false,
                SynLongIdent(
                  [ FSharp.Compiler.Syntax.Ident("op_PipeRight", mPipe) ],
                  [],
                  [ Some(IdentTrivia.OriginalNotation "|>") ]
                ),
                None,
                mPipe
              ),
              expr, // The expr that will now be piped into ignore.
              Range.unionRanges expr.Range mPipe
            )

          let appIgnore =
            SynExpr.App(
              ExprAtomicFlag.NonAtomic,
              false,
              appPipe,
              SynExpr.Ident(FSharp.Compiler.Syntax.Ident("ignore", mIgnore)),
              Range.unionRanges expr.Range mIgnore
            )

          appPipe, appIgnore

        let pathWithPipeIgnore =
          SyntaxNode.SynExpr appPipe :: SyntaxNode.SynExpr appIgnore :: path

        SynExpr.shouldBeParenthesizedInContext sourceText.GetLineString pathWithPipeIgnore expr

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
