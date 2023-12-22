module FsAutoComplete.CodeFix.TopLevelTypes

open FSharp.Compiler.Text
open FSharp.UMX
open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.Compiler.CodeAnalysis

let title = "Oh some stuff, y'know"

let fix
  (forceGetFSharpProjectOptions: string<LocalPath> -> Async<Result<FSharpProjectOptions, string>>)
  (getParseResultsForFile: GetParseResultsForFile)
  (forceChecker: unit -> FSharpCompilerServiceChecker)
  : CodeFix =
  fun codeActionParams ->
    asyncResult {
      let fileName = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath
      let! options = forceGetFSharpProjectOptions fileName
      let checker = forceChecker ()
      let! projectCheckResults = checker.ParseAndCheckProject options
      let! (parseAndCheckResult, _, fsacSourceText) = getParseResultsForFile fileName FSharp.Compiler.Text.Position.pos0
      let sourceText: ISourceText = fsacSourceText

      let parsedInput, checkFileResults =
        parseAndCheckResult.GetParseResults.ParseTree, parseAndCheckResult.GetCheckResults

      let missingTypeInformation =
        TopLevelTypeAnalysis.findMissingTypeInformation sourceText parsedInput checkFileResults projectCheckResults

      ignore missingTypeInformation

      return
        [ { Edits = Array.empty
            File = codeActionParams.TextDocument
            Title = title
            SourceDiagnostic = None
            Kind = FixKind.Refactor } ]
    }
