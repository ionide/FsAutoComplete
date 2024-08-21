module FsAutoComplete.CodeFix.UseTripleQuotedInterpolation

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text.Range

let title = "Use triple-quoted string interpolation"

/// a codefix that replaces erroring single-quoted interpolations with triple-quoted interpolations
let fix (getParseResultsForFile: GetParseResultsForFile) : CodeFix =
  Run.ifDiagnosticByCode (Set.ofList [ "3373" ]) (fun diagnostic codeActionParams ->
    asyncResult {
      let pos = protocolPosToPos diagnostic.Range.Start

      let filePath = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath

      let! tyRes, _, sourceText = getParseResultsForFile filePath pos

      let range =
        (pos, tyRes.GetParseResults.ParseTree)
        ||> ParsedInput.tryPick (fun _path node ->
          match node with
          | SyntaxNode.SynExpr(SynExpr.InterpolatedString(range = range)) when rangeContainsPos range pos ->
            Some range
          | _ -> None)

      match range with
      | Some range ->
        let! interpolationText = sourceText.GetText range
        // skip the leading '$' in the existing single-quoted interpolation
        let newText = "$\"\"" + interpolationText.[1..] + "\"\""

        return
          [ { File = codeActionParams.TextDocument
              SourceDiagnostic = Some diagnostic
              Title = title
              Edits =
                [| { Range = fcsRangeToLsp range
                     NewText = newText } |]
              Kind = FixKind.Fix } ]
      | None -> return []
    })
