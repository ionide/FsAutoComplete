/// a codefix that makes a binding 'rec' if something inside the binding requires recursive access
module FsAutoComplete.CodeFix.MakeOuterBindingRecursive

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

let fix (getParseResultsForFile: GetParseResultsForFile) (getLineText: GetLineText) : CodeFix =
  Run.ifDiagnosticByCode
    (Set.ofList [ "39" ])
    (fun diagnostic codeActionParams ->
      asyncResult {
        let fileName =
          codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath

        let errorRangeStart = protocolPosToPos diagnostic.Range.Start
        let! (tyres, _line, lines) = getParseResultsForFile fileName errorRangeStart
        let missingMemberName = getLineText lines diagnostic.Range

        let! outerBindingRange =
          tyres.GetParseResults.TryRangeOfNameOfNearestOuterBindingContainingPos errorRangeStart
          |> Result.ofOption (fun _ -> "No outer binding found at pos")

        let lspOuterBindingRange = fcsRangeToLsp outerBindingRange
        let outerBindingName = getLineText lines lspOuterBindingRange

        do! Result.guard
              (fun _ -> missingMemberName = outerBindingName)
              "member names didn't match, don't suggest fix"

        return
          [ { Title = "Make outer binding recursive"
              File = codeActionParams.TextDocument
              SourceDiagnostic = Some diagnostic
              Kind = FixKind.Fix
              Edits =
                [| { Range =
                       { Start = lspOuterBindingRange.Start
                         End = lspOuterBindingRange.Start }
                     NewText = "rec " } |] } ]
      }
      )
