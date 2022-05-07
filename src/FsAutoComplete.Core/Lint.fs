module FsAutoComplete.Lint

open FSharpLint.Framework
open FSharpLint.Application
open FsAutoComplete.Logging
open FSharp.Compiler.Text

let logger = LogProvider.getLoggerByName "FSharpLint"

type EnrichedLintWarning =
  { Warning: Suggestion.LintWarning
    HelpUrl: string
    Code: string }

let pageForLint (identifier: string) =
  sprintf "http://fsprojects.github.io/FSharpLint/how-tos/rules/%s.html" identifier

/// In addition we add the url to the matching help page for fsharplint
let enrichLintWarning (w: Suggestion.LintWarning) : EnrichedLintWarning =
  { Code = w.RuleIdentifier
    HelpUrl = pageForLint w.RuleIdentifier
    Warning = w }

/// Attempts to load the F#Lint configuration from the closest available settings file to the given project file.
let loadConfiguration (workspaceRoot: string option) (lintConfigFileRelativePath: string option) =
  let expectedConfigPath = defaultArg lintConfigFileRelativePath "fsharplint.json"

  match workspaceRoot with
  | Some root ->
    let fullPath = System.IO.Path.Combine(root, expectedConfigPath)

    if System.IO.File.Exists fullPath then
      ConfigurationParam.FromFile fullPath
    else
      ConfigurationParam.Default
  | None -> ConfigurationParam.Default

let lintWithConfiguration (lintConfig: ConfigurationParam) ctok ast (sourceCode: ISourceText) typeCheckResults =
  try
    let res =
      Lint.lintParsedSource
        { Lint.OptionalLintParameters.Default with
            Configuration = lintConfig
            CancellationToken = Some ctok }
        { Ast = ast
          Source = sourceCode.ToString()
          TypeCheckResults = Some typeCheckResults }

    match res with
    | LintResult.Failure f ->
      logger.error (
        Log.setMessage "Linter failure: {message}"
        >> Log.addContextDestructured "message" f.Description
      )

      Error(sprintf "Something went wrong, linter failed: %s" f.Description)
    | LintResult.Success warnings ->
      let splitWarnings = warnings |> List.map enrichLintWarning
      Ok splitWarnings
  with
  | e ->
    logger.error (
      Log.setMessage "Fatal error in linter: {message}"
      >> Log.addContextDestructured "message" e.Message
      >> Log.addExn e
    )

    Error(sprintf "Something went wrong, linter failed: %s" e.Message)
