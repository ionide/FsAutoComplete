/// a codefix that rewrites C#-style '=>' lambdas to F#-style 'fun _ -> _' lambdas
module FsAutoComplete.CodeFix.ChangeCSharpLambdaToFSharp

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

let fix (getParseResultsForFile: GetParseResultsForFile) (getLineText: GetLineText)
                                      : CodeFix =
  Run.ifDiagnosticByCode
    (Set.ofList [ "39" // undefined value
                  "43" ]) // operator not defined
    (fun diagnostic codeActionParams ->
      asyncResult {
        let fileName =
          codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath

        let fcsPos = protocolPosToPos diagnostic.Range.Start
        let! (tyRes, _, lines) = getParseResultsForFile fileName fcsPos

        match tyRes.GetParseResults.TryRangeOfParenEnclosingOpEqualsGreaterUsage fcsPos with
        | Some (fullParenRange, lambdaArgRange, lambdaBodyRange) ->
            let argExprText =
              getLineText lines (fcsRangeToLsp lambdaArgRange)

            let bodyExprText =
              getLineText lines (fcsRangeToLsp lambdaBodyRange)

            let replacementText = $"fun {argExprText} -> {bodyExprText}"
            let replacementRange = fcsRangeToLsp fullParenRange

            return
              [ { Title = "Replace C#-style lambda with F# lambda"
                  File = codeActionParams.TextDocument
                  SourceDiagnostic = Some diagnostic
                  Edits =
                    [| { Range = replacementRange
                         NewText = replacementText } |]
                  Kind = FixKind.Refactor } ]
        | None -> return []
      }
      )
