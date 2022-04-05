module FsAutoComplete.CodeFix.UseTripleQuotedInterpolation

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FsAutoComplete.FCSPatches

/// a codefix that replaces erroring single-quoted interpolations with triple-quoted interpolations
let fix (getParseResultsForFile: GetParseResultsForFile) (getRangeText: GetRangeText) : CodeFix =
  Run.ifDiagnosticByCode (Set.ofList [ "3373" ]) (fun diagnostic codeActionParams ->
    asyncResult {
      let pos = protocolPosToPos diagnostic.Range.Start

      let filePath =
        codeActionParams.TextDocument.GetFilePath()
        |> Utils.normalizePath

      let! tyRes, _, sourceText = getParseResultsForFile filePath pos

      match tyRes.GetParseResults.TryRangeOfStringInterpolationContainingPos pos with
      | Some range ->
        let! interpolationText = sourceText.GetText range
        // skip the leading '$' in the existing single-quoted interpolation
        let newText = "$\"\"" + interpolationText.[1..] + "\"\""

        return
          [ { File = codeActionParams.TextDocument
              SourceDiagnostic = Some diagnostic
              Title = "Use triple-quoted string interpolation"
              Edits =
                [| { Range = fcsRangeToLsp range
                     NewText = newText } |]
              Kind = FixKind.Fix } ]
      | None -> return []
    })
