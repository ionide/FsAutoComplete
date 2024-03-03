/// a codefix that replaces typeof<'t>.Name with nameof('t)
module FsAutoComplete.CodeFix.ChangeTypeOfNameToNameOf

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.Compiler.Syntax

let title = "Use 'nameof'"

let fix (getParseResultsForFile: GetParseResultsForFile) : CodeFix =
  fun codeActionParams ->
    asyncResult {
      let fileName = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath

      let pos = protocolPosToPos codeActionParams.Range.Start

      let! tyRes, _line, sourceText = getParseResultsForFile fileName pos

      let! results =
        (pos, tyRes.GetParseResults.ParseTree)
        ||> ParsedInput.tryPick (fun _path node ->
          let (|Ident|) (ident: Ident) = ident.idText

          match node with
          | SyntaxNode.SynExpr(SynExpr.DotGet(
              expr = SynExpr.TypeApp(expr = SynExpr.Ident(Ident "typeof"); typeArgs = [ typeArg ]); range = range)) ->
            Some
              {| NamedIdentRange = typeArg.Range
                 FullExpressionRange = range |}

          | _ -> None)
        |> Result.ofOption (fun _ -> "no typeof expr found")

      let! typeName = sourceText.GetText results.NamedIdentRange
      let replacement = $"nameof({typeName})"

      return
        [ { Edits =
              [| { Range = fcsRangeToLsp results.FullExpressionRange
                   NewText = replacement } |]
            File = codeActionParams.TextDocument
            Title = title
            SourceDiagnostic = None
            Kind = FixKind.Refactor } ]
    }
