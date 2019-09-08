module FsAutoComplete.Lint

open FSharpLint.Application
open FSharpLint.Application.ConfigurationManagement
open FSharpLint.Application.ConfigurationManager

type EnrichedLintWarning = 
    { Warning: LintWarning.Warning 
      HelpUrl: string option
      Code: int option }

let private loadXmlConfig projectFilePath =
    match XmlConfiguration.tryLoadConfigurationForProject projectFilePath with
    | None -> Ok None
    | Some xmlConfig ->
        { conventions = xmlConfig |> XmlConfiguration.convertConventions
          formatting = xmlConfig |> XmlConfiguration.convertFormatting
          hints = xmlConfig |> XmlConfiguration.convertHints
          ignoreFiles = xmlConfig |> XmlConfiguration.convertIgnoreFiles
          typography = xmlConfig |> XmlConfiguration.convertTypography }
        |> Some
        |> Ok
    
let private loadJsonConfig projectFilePath =
    match loadConfigurationForProject projectFilePath with
    | ConfigurationResult.Success config ->
        Ok config
    | ConfigurationResult.Failure errors ->
        Error errors

let pageForLint (code: int) =
    let codeNumber = code |> int
    match codeNumber with
    | 1 | 2 | 3 -> Some "http://fsprojects.github.io/FSharpLint/TupleFormatting.html"
    | 4 | 5 | 6 | 7 -> Some "http://fsprojects.github.io/FSharpLint/PatternMatchFormatting.html"
    | 8 | 9 -> Some "http://fsprojects.github.io/FSharpLint/Spacing.html"
    | 10 (* | 11 (not present?) *) | 12 -> Some "http://fsprojects.github.io/FSharpLint/Formatting.html"
    | 13 | 14 | 15 -> Some "http://fsprojects.github.io/FSharpLint/Conventions.html"
    | 22 | 23 | 24 | 25 | 26 | 27 | 28 | 29 | 30 | 31 | 32 | 33 -> Some "http://fsprojects.github.io/FSharpLint/SourceLength.html"
    | 34 | 35 -> Some "http://fsprojects.github.io/FSharpLint/FunctionReimplementation.html"
    | 36 | 37 | 38 | 39 | 40 | 41 | 42 | 43 | 44 | 45 | 46 | 47 | 48 | 49 | 50 -> Some "http://fsprojects.github.io/FSharpLint/NameConventions.html"
    | 51 | 52 | 53 | 54 -> Some "http://fsprojects.github.io/FSharpLint/NumberOfItems.html"
    | 55 | 56 | 57 | 58 -> Some "http://fsprojects.github.io/FSharpLint/Binding.html"
    | 60 | 61 | 62 | 63 | 64 -> Some "http://fsprojects.github.io/FSharpLint/Typography.html"
    | 65 -> Some "http://fsprojects.github.io/FSharpLint/Hints.html"
    | _ -> None

/// lint warnings come to us with the lint identifier as part of the info message,
/// so we parse and split that out here for processing later.
/// In addition we add the url to the matching help page for fsharplint
let enrichLintWarning (w: LintWarning.Warning): EnrichedLintWarning =
    match String.splitAtChar ':' w.Info with
    | String.SplitResult.NoMatch -> 
        { Code = None; HelpUrl = None; Warning = w }
    | String.SplitResult.Split(left, right) -> 
        let tleft, tright = left.Trim(), right.Trim()
        let code = tleft.Substring(2) |> int
        let message = tright
        let warning' = { w with Info = message }
        { Code = Some code; HelpUrl = pageForLint code; Warning = warning' }
    
/// Attempts to load the F#Lint configuration from the closest available settings file to the given project file.
/// We attempt the old xml config style first, then the newer json format.
let tryLoadConfiguration (projectFilePath: string) = 
    // first we check if exists xml config (backward compatibility)
    // after that if exists json config
    match loadXmlConfig projectFilePath with
    | Ok (Some config) ->
        Ok config
    | Ok None
    | Error _ ->
        //TODO log xml config error
        loadJsonConfig projectFilePath

let lintWithConfiguration (lintConfig: Configuration option) ast sourceCode typeCheckResults = 
    let res =
        Lint.lintParsedSource
            { Lint.OptionalLintParameters.Default with Configuration = lintConfig }
            { Ast = ast
              Source = sourceCode
              TypeCheckResults = Some typeCheckResults }

    match res with
    | LintResult.Failure f -> Error (sprintf "Something went wrong, linter failed: %s" f.Description)
    | LintResult.Success warnings ->
        let splitWarnings = warnings |> List.map enrichLintWarning
        Ok splitWarnings