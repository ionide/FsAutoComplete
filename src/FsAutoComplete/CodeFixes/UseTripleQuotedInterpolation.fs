module FsAutoComplete.CodeFix.UseTripleQuotedInterpolation

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FsAutoComplete.FCSPatches
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

type FSharpParseFileResults with

  /// Attempts to find the range of the string interpolation that contains a given position.
  member scope.TryRangeOfStringInterpolationContainingPos pos =
    SyntaxTraversal.Traverse(
      pos,
      scope.ParseTree,
      { new SyntaxVisitorBase<_>() with
          member _.VisitExpr(_, _, defaultTraverse, expr) =
            match expr with
            | SynExpr.InterpolatedString (range = range) when Range.rangeContainsPos range pos -> Some range
            | _ -> defaultTraverse expr }
    )


let title = "Use triple-quoted string interpolation"

/// a codefix that replaces erroring single-quoted interpolations with triple-quoted interpolations
let fix (getParseResultsForFile: GetParseResultsForFile) (getRangeText: GetRangeText) : CodeFix =
  Run.ifDiagnosticByCode (Set.ofList [ "3373" ]) (fun diagnostic codeActionParams ->
    asyncResult {
      let pos = protocolPosToPos diagnostic.Range.Start

      let filePath = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath

      let! tyRes, _, sourceText = getParseResultsForFile filePath pos

      match tyRes.GetParseResults.TryRangeOfStringInterpolationContainingPos pos with
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
