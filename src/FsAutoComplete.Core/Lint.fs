module FsAutoComplete.Lint

open FSharpLint.Framework
open FSharpLint.Application

type EnrichedLintWarning =
    { Warning: Suggestion.LintWarning
      HelpUrl: string
      Code: string }

let pageForLint (identifier: string) =
  sprintf "http://fsprojects.github.io/FSharpLint/rules/%s.html" identifier

/// In addition we add the url to the matching help page for fsharplint
let enrichLintWarning (w: Suggestion.LintWarning): EnrichedLintWarning =
    { Code = w.RuleIdentifier; HelpUrl = pageForLint w.RuleIdentifier; Warning = w }

/// Attempts to load the F#Lint configuration from the closest available settings file to the given project file.
let loadConfiguration (workspaceRoot: string option) =
  match workspaceRoot with
  | Some root ->
    let configFileName = "fsharplint.json"
    let fullPath = System.IO.Path.Combine(root, configFileName)
    if System.IO.File.Exists fullPath
    then ConfigurationParam.FromFile fullPath
    else ConfigurationParam.Default
  | None -> ConfigurationParam.Default

let lintWithConfiguration (lintConfig: ConfigurationParam) ctok ast sourceCode typeCheckResults =
    let res =
        Lint.lintParsedSource
            { Lint.OptionalLintParameters.Default with
                    Configuration = lintConfig
                    CancellationToken = Some ctok }
            { Ast = ast
              Source = sourceCode
              TypeCheckResults = Some typeCheckResults }

    match res with
    | LintResult.Failure f -> Error (sprintf "Something went wrong, linter failed: %s" f.Description)
    | LintResult.Success warnings ->
        let splitWarnings = warnings |> List.map enrichLintWarning
        Ok splitWarnings
