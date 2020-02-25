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

let tryFindSettingsForProject (projectFilePath: string) =
  None

/// Attempts to load the F#Lint configuration from the closest available settings file to the given project file.
let loadConfiguration (projectFilePath: string) =
  match tryFindSettingsForProject projectFilePath with
  | None -> ConfigurationParam.Default
  | Some f -> ConfigurationParam.FromFile f

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
