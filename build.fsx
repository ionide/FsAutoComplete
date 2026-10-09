#r "nuget: Fun.Build, 1.1.2"
#r "nuget: Fake.Tools.Git, 6.0.0"
#r "nuget: Fake.IO.FileSystem, 6.0.0"
#r "nuget: Fantomas.Core, 6.3.1"
#r "nuget: Ionide.KeepAChangelog, 0.2.0"

open Fun.Build
open Fake.Tools

let toolRestore = stage "ToolRestore" { run "dotnet tool restore" }

module ScaffoldCodeFix =
  open System
  open System.IO
  open Fake.Core
  open Fake.IO.FileSystemOperators
  open Fantomas.Core.SyntaxOak

  let repositoryRoot = __SOURCE_DIRECTORY__

  let AdaptiveServerStatePath =
    repositoryRoot
    </> "src"
    </> "FsAutoComplete"
    </> "LspServers"
    </> "AdaptiveServerState.fs"


  let TestsPath =
    repositoryRoot
    </> "test"
    </> "FsAutoComplete.Tests.Lsp"
    </> "CodeFixTests"
    </> "Tests.fs"

  let removeReturnCarriage (v: string) = v.Replace("\r", "")

  let mkCodeFixImplementation codeFixName =
    let path =
      repositoryRoot
      </> "src"
      </> "FsAutoComplete"
      </> "CodeFixes"
      </> $"{codeFixName}.fs"

    let content =
      $"""module FsAutoComplete.CodeFix.%s{codeFixName}

open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FsToolkit.ErrorHandling
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete.CodeFix.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

// TODO: add proper title for code fix
let title = "%s{codeFixName} Codefix"

let fix
  (getParseResultsForFile: GetParseResultsForFile)
  : CodeFix =
  fun (codeActionParams: CodeActionParams) ->
    asyncResult {{
      // Most code fixes have some general setup.
      // We initially want to detect the state of the current code and whether we can propose any text edits to the user.

      let fileName = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath
      // The converted LSP start position to an FCS start position.
      let fcsPos = protocolPosToPos codeActionParams.Range.Start
      // The syntax tree and typed tree, current line and sourceText of the current file.
      let! (parseAndCheckResults:ParseAndCheckResults, line:string, sourceText:IFSACSourceText) =
        getParseResultsForFile fileName fcsPos

      // The syntax tree can be an intimidating set of types to work with.
      // It is a tree structure but it consists out of many different types.
      // See https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntax.html
      // It can be useful to inspect a syntax tree via a code sample using https://fsprojects.github.io/fantomas-tools/#/ast
      // For example `let a b c = ()` in
      // https://fsprojects.github.io/fantomas-tools/#/ast?data=N4KABGBEAmCmBmBLAdrAzpAXFSAacUiaAYmolmPAIYA2as%%2BEkAxgPZwWQ2wAuYVYAEZhmYALxgAFAEo8BSLAAeAByrJoFHgCcArrBABfIA
      // Let's say we want to find the (FCS) range for identifier `a` if the user's cursor is inside the function name.
      // We will query the syntax tree to verify this is the case.
      let maybeFunctionNameRange =
        (fcsPos, parseAndCheckResults.GetParseResults.ParseTree)
        ||> ParsedInput.tryPick (fun _path node ->
          match node with
          // We know that `a` will be part of a `SynPat.LongIdent`
          // This was visible in the online tool.
          | SyntaxNode.SynPat(SynPat.LongIdent(longDotId = SynLongIdent(id = [ functionNameIdent ]))) when
            // When our code fix operates on the user's code there is no way of knowing what will be inside the syntax tree.
            // So we need to be careful and verify that the pattern is indeed matching the position of the cursor.
            Range.rangeContainsPos functionNameIdent.idRange fcsPos
            ->
            Some functionNameIdent.idRange
          | _ -> None)

      match maybeFunctionNameRange with
      | None ->
        // The cursor is not in a position we are interested in.
        // This code fix should not trigger any suggestions so we return an empty list.
        return []
      | Some mBindingName ->
        // It turns out we are inside a let binding and we have the range of the function name.
        // Just for fun, we want to detect if there is a matching typed tree symbol present for the current name.
        // We could have passed the function name from the syntax visitor, instead will we grab it from the source text.
        let! functionName = sourceText.GetText mBindingName
        // FSharpSymbolUse is reflecting the typed tree.
        // See https://fsharp.github.io/fsharp-compiler-docs/fcs/symbols.html
        let symbolUse: FSharp.Compiler.CodeAnalysis.FSharpSymbolUse option =
          parseAndCheckResults.GetCheckResults.GetSymbolUseAtLocation(mBindingName.EndLine, mBindingName.EndColumn, line, [ functionName ])

        let hasFunctionDefinitionSymbol =
          match symbolUse with
          | None -> false
          | Some symbolUse ->
            // We want to verify the found symbol is indeed a definition of a function
            match symbolUse.Symbol with
            | :? FSharpMemberOrFunctionOrValue -> true
            | _ -> false

        if not hasFunctionDefinitionSymbol then
          return []
        else
          // Return a list of Fix records for when the code fix is applicable.
          return [
            {{
                  SourceDiagnostic = None
                  Title = title
                  File = codeActionParams.TextDocument
                  // Based on conditional logic, you typically want to suggest a text edit to the user.
                  Edits = [|
                    {{
                      // When dealing with FCS, we typically want to use the FCS flavour of range.
                      // However, to interact correctly with the LSP protocol, we need to return an LSP range.
                      Range = fcsRangeToLsp mBindingName
                      NewText = "Text replaced by %s{codeFixName}"
                     }}
                  |]
                  Kind = FixKind.Fix
            }}
          ]
    }}
"""

    File.WriteAllText(path, removeReturnCarriage content)
    Trace.tracefn $"Generated %s{Path.GetRelativePath(repositoryRoot, path)}"

  let mkCodeFixSignature codeFixName =
    let path =
      repositoryRoot
      </> "src"
      </> "FsAutoComplete"
      </> "CodeFixes"
      </> $"{codeFixName}.fsi"

    let content =
      $"""module FsAutoComplete.CodeFix.%s{codeFixName}

open FsAutoComplete.CodeFix.Types

val title: string
val fix: getParseResultsForFile: GetParseResultsForFile -> CodeFix
"""

    File.WriteAllText(path, removeReturnCarriage content)
    Trace.tracefn $"Generated %s{Path.GetRelativePath(repositoryRoot, path)}"

  let updateProjectFiles () =
    let fsAutoCompleteProject =
      repositoryRoot </> "src" </> "FsAutoComplete" </> "FsAutoComplete.fsproj"

    File.SetLastWriteTime(fsAutoCompleteProject, DateTime.Now)

    let fsAutoCompleteTestsLsp =
      repositoryRoot
      </> "test"
      </> "FsAutoComplete.Tests.Lsp"
      </> "FsAutoComplete.Tests.Lsp.fsproj"

    File.SetLastWriteTime(fsAutoCompleteTestsLsp, DateTime.Now)

  let (|IdentName|_|) (name: string) (identListNode: IdentListNode) =
    match identListNode.Content with
    | [ IdentifierOrDot.Ident stn ] when stn.Text = name -> Some()
    | _ -> None

  let getOakFor path =
    let content = File.ReadAllText path

    Fantomas.Core.CodeFormatter.ParseOakAsync(false, content)
    |> Async.RunSynchronously
    |> Array.head
    |> fst

  let appendItemToArrayOrList item path (node: ExprArrayOrListNode) =
    let lastElement = node.Elements |> List.last |> Expr.Node
    let startIndent = lastElement.Range.StartColumn
    let lineIdx = lastElement.Range.EndLine - 1
    let arrayEndsOnLastElement = node.Range.EndLine = lastElement.Range.EndLine

    let updatedLines =
      let lines = File.ReadAllLines path
      let currentLastLine = lines.[lineIdx]
      let spaces = String.replicate startIndent " "

      if arrayEndsOnLastElement then
        let endOfLastElement = currentLastLine.Substring(0, lastElement.Range.EndColumn)
        let endOfArray = currentLastLine.Substring(lastElement.Range.EndColumn)

        lines
        |> Array.updateAt lineIdx $"{endOfLastElement}\n%s{spaces}%s{item}%s{endOfArray}"
      else
        lines |> Array.insertAt (lineIdx + 1) $"%s{spaces}%s{item}"

    let content = String.concat "\n" updatedLines
    File.WriteAllText(path, content)
    Trace.tracefn $"Added \"%s{item}\" to %s{Path.GetRelativePath(repositoryRoot, path)}"

  module List =
    let exactlyOneOrFail (message: string) (items: 'T list) : 'T =
      if items.Length = 1 then items.Head else failwith message

    let pickOrFail (message: string) (chooser: 'T -> 'U option) (items: 'T list) : 'U =
      match List.tryPick chooser items with
      | None -> failwith message
      | Some u -> u

  let findArrayOrListOfFail (e: Expr) =
    match e with
    | Expr.ArrayOrList array -> array
    | e -> failwithf $"Expected to find Expr.ArrayOrList, got %A{e}"

  let findTypeWithNameOfFail (typeName: string) (mn: ModuleOrNamespaceNode) : ITypeDefn =
    mn.Declarations
    |> List.pickOrFail $"Expected to find ModuleDecl.TypeDefn for %s{typeName}" (function
      | ModuleDecl.TypeDefn t ->
        let tdn = TypeDefn.TypeDefnNode t

        match tdn.TypeName.Identifier with
        | IdentName typeName -> Some tdn
        | _ -> None
      | _ -> None)

  let findArrayInAdaptiveFSharpLspServer () : ExprArrayOrListNode =
    let oak = getOakFor AdaptiveServerStatePath

    // namespace FsAutoComplete.Lsp
    let ns =
      oak.ModulesOrNamespaces
      |> List.exactlyOneOrFail "Expected a single namespace in Oak."

    // type AdaptiveState
    let t = findTypeWithNameOfFail "AdaptiveState" ns

    // let codefixes =
    let codefixesValue =
      t.Members
      |> List.pickOrFail "Expected to find MemberDefn.LetBinding for codefixes" (function
        | MemberDefn.LetBinding bindingList ->
          match bindingList.Bindings with
          | bindings ->
            bindings
            |> List.tryPick (fun binding ->
              match binding.FunctionName with
              | Choice1Of2(IdentName "codefixes") -> Some binding
              | _ -> None)
        | _ -> None)

    let infixApp =
      match codefixesValue.Expr with
      | Expr.CompExprBody body ->
        match List.last body.Statements with
        | ComputationExpressionStatement.OtherStatement other ->
          match other with
          | Expr.InfixApp infixApp -> infixApp
          | e -> failwithf $"Expected to find Expr.InfixApp, got %A{e}"
        | ces -> failwithf $"Expected to find ComputationExpressionStatement.OtherStatement, got %A{ces}"
      | e -> failwithf $"Expected to find Expr.CompExprBody, got %A{e}"

    let appWithLambda =
      match infixApp.RightHandSide with
      | Expr.AppWithLambda appWithLambda -> appWithLambda
      | e -> failwithf $"Expected to find Expr.AppWithLambda, got %A{e}"

    let lambda =
      match appWithLambda.Lambda with
      | Choice1Of2 lambda -> lambda
      | Choice2Of2 ml -> failwithf $"Expected to find ExprLambdaNode, got %A{ml}"

    findArrayOrListOfFail lambda.Expr

  let wireCodeFixInAdaptiveFSharpLspServer codeFixName =
    try
      let array = findArrayInAdaptiveFSharpLspServer ()

      appendItemToArrayOrList $"%s{codeFixName}.fix tryGetParseAndCheckResultsForFile" AdaptiveServerStatePath array
    with ex ->
      Trace.traceException ex

      Trace.traceError
        $"Unable to find array of codefixes in %s{AdaptiveServerStatePath}.\nDid the code structure change?"


  let mkCodeFixTests codeFixName =
    let path =
      repositoryRoot
      </> "test"
      </> "FsAutoComplete.Tests.Lsp"
      </> "CodeFixTests"
      </> $"%s{codeFixName}Tests.fs"

    let contents =
      $"module private FsAutoComplete.Tests.CodeFixTests.%s{codeFixName}Tests

open Expecto
open Helpers
open Utils.ServerTests
open Utils.CursorbasedTests
open FsAutoComplete.CodeFix

let tests state =
  serverTestList (nameof %s{codeFixName}) state defaultConfigDto None (fun server ->
    [ let selectCodeFix = CodeFix.withTitle %s{codeFixName}.title

      ftestCaseAsync \"first unit test for %s{codeFixName}\"
      <| CodeFix.check
        server
        \"let a$0 b c = ()\"
        Diagnostics.acceptAll
        selectCodeFix
        \"let Text replaced by %s{codeFixName} b c = ()\"
    ])
"

    File.WriteAllText(path, removeReturnCarriage contents)
    Trace.tracefn $"Generated %s{Path.GetRelativePath(repositoryRoot, path)}"

  let findListInTests () =
    let oak = getOakFor TestsPath
    // module FsAutoComplete.Tests.CodeFixTests.Tests
    let testsModule =
      oak.ModulesOrNamespaces
      |> List.exactlyOneOrFail "Expected a single module in Oak."

    // let tests state =
    let testBinding =
      testsModule.Declarations
      |> List.pickOrFail "Expected to find ModuleDecl.TopLevelBinding for tests" (function
        | ModuleDecl.TopLevelBinding binding ->
          match binding.FunctionName with
          | Choice1Of2(IdentName "tests") -> Some binding
          | _ -> None
        | _ -> None)

    let appNode =
      match testBinding.Expr with
      | Expr.InfixApp infixApp ->
        match infixApp.RightHandSide with
        | Expr.App appNode -> appNode
        | e -> failwithf $"Expected Expr.App, got %A{e}"
      | e -> failwithf $"Expected Expr.InfixApp, got %A{e}"

    findArrayOrListOfFail (List.last appNode.Arguments)

  let wireCodeFixTests codeFixName =
    try
      let list = findListInTests ()
      appendItemToArrayOrList $"%s{codeFixName}Tests.tests state" TestsPath list
    with ex ->
      Trace.traceException ex
      Trace.traceError $"Unable to find array of tests in %s{TestsPath}.\nDid the code structure change?"

  let scaffold (codeFixName: string) : unit =
    // generate files in src/CodeFixes/
    mkCodeFixImplementation codeFixName
    mkCodeFixSignature codeFixName

    // Wire up codefix to LSP servers
    wireCodeFixInAdaptiveFSharpLspServer codeFixName

    // Add test file
    mkCodeFixTests codeFixName

    // Wire up tests in test/FsAutoComplete.Tests.Lsp/CodeFixTests/Tests.fs
    wireCodeFixTests codeFixName

    updateProjectFiles ()
    Trace.tracefn $"Scaffolding %s{codeFixName} complete!"

  let ensureScaffoldStillWorks () =
    findArrayInAdaptiveFSharpLspServer () |> ignore
    findListInTests () |> ignore

pipeline "EnsureRepoConfig" {
  description "Configure custom git hooks, currently only used to ensure that code is formatted before pushing"
  workingDir __SOURCE_DIRECTORY__
  stage "Git" { run (fun _ -> Git.CommandHelper.gitCommand "" "config core.hooksPath .githooks") }
  runIfOnlySpecified true
}

pipeline "ScaffoldCodeFix" {
  description "Scaffold a new code fix."
  workingDir __SOURCE_DIRECTORY__

  stage "Scaffold" {
    run (fun ctx ->
      let codeFixName = ctx.GetAllCmdArgs() |> List.tryLast

      match codeFixName with
      | None -> printfn "Usage: dotnet fsi build.fsx -- -p ScaffoldCodeFix <name>"
      | Some codeFixName -> ScaffoldCodeFix.scaffold codeFixName)
  }

  runIfOnlySpecified true
}

pipeline "EnsureCanScaffoldCodeFix" {
  description "Ensure the ScaffoldCodeFix pipeline can still be executed."
  workingDir __SOURCE_DIRECTORY__
  stage "Ensure" { run (fun _ -> ScaffoldCodeFix.ensureScaffoldStillWorks ()) }
  runIfOnlySpecified true
}

pipeline "Build" {
  description "Default build pipeline"
  workingDir __SOURCE_DIRECTORY__

  stage "Build" {
    toolRestore
    run "dotnet build"
  }

// runIfOnlySpecified false
}


let (</>) a b = System.IO.Path.Combine(a, b)


let lspTestsPath = (__SOURCE_DIRECTORY__ </> "test" </> "FsAutoComplete.Tests.Lsp")

/// Ionide.ProjInfo loads the current SDK's MsBuild library so we need to ensure we
/// run on the version associated with the SDK we want to test against.
/// https://learn.microsoft.com/en-us/dotnet/core/deploying/?pivots=cli#framework-dependent-deployment
let createGlobalJson sdkVersion =
  $"dotnet new globaljson --force --sdk-version %s{sdkVersion} --roll-forward LatestMinor"

let coverageAssemblyExcludeFilter =
  @"^(?!FsAutoComplete\.Core$)(?!FsAutoComplete\.Logging$)(?!fsautocomplete$).*"

let testCommand targetFramework =
  $"""dotnet test -c Release -f %s{targetFramework} --no-restore --no-build --logger "console;verbosity=minimal" --logger GitHubActions /p:AltCover=true /p:AltCoverAssemblyExcludeFilter="%s{coverageAssemblyExcludeFilter}" -- Expecto.fail-on-focused-tests=true Expecto.parallel-workers=4 --blame-hang --blame-hang-timeout 1m"""

/// The test project runs its test groups in parallel inside one process, so one `dotnet test` covers the framework.
let ciTests targetFramework sdkVersion =
  stage $"test-ci:%s{targetFramework}" {
    workingDir lspTestsPath
    run (createGlobalJson sdkVersion)

    // The pin would otherwise outlive a failed run and select the wrong SDK for the next one.
    run (fun ctx ->
      async {
        try
          return! ctx.RunCommand(testCommand targetFramework)
        finally
          System.IO.File.Delete(lspTestsPath </> "global.json")
      })
  }

// Every stage restores with the .NET 10 SDK because older SDKs cannot restore Paket 10.
// It then builds and tests with the SDK that matches the test target framework.
let net80Tests =
  stage "test:net8.0" {
    workingDir lspTestsPath
    run (createGlobalJson "10.0.100")
    toolRestore
    run "dotnet restore"
    run (createGlobalJson "8.0.100")
    run "dotnet build -c Release -f net8.0 --no-restore"
    run (testCommand "net8.0")
    run (fun _ -> System.IO.File.Delete(lspTestsPath </> "global.json"))
  }

let net90Tests =
  stage "test:net9.0" {
    workingDir lspTestsPath
    run (createGlobalJson "10.0.100")
    toolRestore
    run "dotnet restore"
    run (createGlobalJson "9.0.100")
    run "dotnet build -c Release -f net9.0 --no-restore"
    run (testCommand "net9.0")
    run (fun _ -> System.IO.File.Delete(lspTestsPath </> "global.json"))
  }

let net100Tests =
  stage "test:net10.0" {
    workingDir lspTestsPath
    run (createGlobalJson "10.0.100")
    toolRestore
    run "dotnet restore"
    run "dotnet build -c Release -f net10.0 --no-restore"
    run (testCommand "net10.0")
    run (fun _ -> System.IO.File.Delete(lspTestsPath </> "global.json"))
  }

let ciNet80Tests = ciTests "net8.0" "8.0.100"
let ciNet90Tests = ciTests "net9.0" "9.0.100"
let ciNet100Tests = ciTests "net10.0" "10.0.100"

pipeline "test:net8.0" {
  description "Run net8.0 tests"
  net80Tests
  runIfOnlySpecified true
}

pipeline "test:net9.0" {
  description "Run net9.0 tests"
  net90Tests
  runIfOnlySpecified true
}

pipeline "test:net10.0" {
  description "Run net10.0 tests"
  net100Tests
  runIfOnlySpecified true
}

pipeline "test-ci:net8.0" {
  description "Run net8.0 tests of a Release build, as CI does"
  ciNet80Tests
  runIfOnlySpecified true
}

pipeline "test-ci:net9.0" {
  description "Run net9.0 tests of a Release build, as CI does"
  ciNet90Tests
  runIfOnlySpecified true
}

pipeline "test-ci:net10.0" {
  description "Run net10.0 tests of a Release build, as CI does"
  ciNet100Tests
  runIfOnlySpecified true
}


pipeline "Tests" {
  description "Run all tests"
  net80Tests
  net90Tests
  net100Tests
  runIfOnlySpecified true
}

module Release =
  open System
  open System.IO
  open Ionide.KeepAChangelog

  let packageDir = __SOURCE_DIRECTORY__ </> "bin"

  /// Whether this run was asked not to publish anything.
  let isDryRun = fsi.CommandLineArgs |> Array.contains "--dry-run"

  let packages () =
    if Directory.Exists packageDir then
      Directory.EnumerateFiles(packageDir, "*.nupkg") |> Seq.toList
    else
      []

  /// Version and GitHub release notes of the newest release in CHANGELOG.md.
  let latestChangelogRelease () =
    match Parser.parseChangeLog (FileInfo(__SOURCE_DIRECTORY__ </> "CHANGELOG.md")) with
    | Error error -> failwithf "Could not parse CHANGELOG.md: %A" error
    | Ok changelogs ->
      // The topmost entry, the same one the Release workflow's detect job reads.
      let version, _, data =
        match changelogs.Releases with
        | [] -> failwith "CHANGELOG.md has no release entry."
        | release :: _ -> release

      let notes =
        match data with
        | None -> ""
        | Some data ->
          [ "Added", data.Added
            "Changed", data.Changed
            "Fixed", data.Fixed
            "Deprecated", data.Deprecated
            "Removed", data.Removed
            "Security", data.Security
            yield! Map.toList data.Custom ]
          |> List.filter (fun (_, body) -> not (String.IsNullOrWhiteSpace body))
          |> List.map (fun (header, body) -> $"### %s{header}\n\n%s{body.Trim()}")
          |> String.concat "\n\n"

      string version, notes

  let release (ctx: Internal.StageContext) =
    async {
      let version, notes = latestChangelogRelease ()
      let tag = $"v%s{version}"

      // The workflow reads the version with sed, guard against it disagreeing with the parser.
      match Environment.GetEnvironmentVariable "RELEASE_VERSION" with
      | expected when not (String.IsNullOrWhiteSpace expected) && expected <> version ->
        failwithf "The workflow detected version %s, but CHANGELOG.md parses as %s." expected version
      | _ -> ()

      let pkgs = packages ()

      match
        pkgs
        |> List.filter (fun pkg -> not (pkg.EndsWith($".%s{version}.nupkg", StringComparison.Ordinal)))
      with
      | [] when not pkgs.IsEmpty -> ()
      | [] -> failwith "No packages found."
      | unexpected -> failwithf "Packages do not match changelog version %s: %A" version unexpected

      // A package file is named <id>.<version>.nupkg, nuget.org has a page for each id and version.
      let nugetLinks =
        pkgs
        |> List.map (fun pkg ->
          let fileName = Path.GetFileName pkg
          let id = fileName.Substring(0, fileName.Length - $".%s{version}.nupkg".Length)
          $"[%s{id} %s{version} on NuGet](https://www.nuget.org/packages/%s{id}/%s{version})")
        |> String.concat "\n\n"

      let notes = $"%s{notes}\n\n%s{nugetLinks}".TrimStart()

      let notesFile = Path.GetTempFileName()
      File.WriteAllText(notesFile, notes)

      let assets = pkgs |> List.map (sprintf "\"%s\"") |> String.concat " "

      let target =
        match Environment.GetEnvironmentVariable "GITHUB_SHA" with
        | sha when not (String.IsNullOrWhiteSpace sha) -> $" --target %s{sha}"
        | _ -> ""

      let ghArgs =
        $"release create %s{tag} %s{assets} --title %s{tag} --notes-file \"%s{notesFile}\"%s{target}"

      try
        if isDryRun then
          printfn $"[dry-run] Release notes for %s{tag}:\n%s{notes}"

          for pkg in pkgs do
            printfn $"[dry-run] dotnet nuget push %s{pkg} --skip-duplicate"

          printfn $"[dry-run] gh %s{ghArgs}"
          return 0
        else
          let key = Environment.GetEnvironmentVariable "NUGET_KEY"

          if String.IsNullOrWhiteSpace key then
            failwith "NUGET_KEY is not set."

          let rec push pkgs =
            async {
              match pkgs with
              | [] -> return Ok()
              | pkg :: rest ->
                match!
                  ctx.RunSensitiveCommand
                    $"dotnet nuget push \"{pkg}\" --api-key {key} --source https://api.nuget.org/v3/index.json --skip-duplicate"
                with
                | Error e -> return Error e
                | Ok() -> return! push rest
            }

          match! push pkgs with
          | Error _ -> return 1
          | Ok() ->
            match! ctx.RunCommand $"gh %s{ghArgs}" with
            | Error _ -> return 1
            | Ok() -> return 0
      finally
        File.Delete notesFile
    }

// Publishes the latest version in CHANGELOG.md to NuGet and as a GitHub release.
// The Release workflow only runs this when that GitHub release does not exist yet.
// `dotnet fsi build.fsx -- -p Release --dry-run` only prints what would happen.
pipeline "Release" {
  description "Publish the newest CHANGELOG.md version to NuGet and as a GitHub release"
  workingDir __SOURCE_DIRECTORY__

  stage "Pack" {
    run (fun _ -> Release.packages () |> List.iter System.IO.File.Delete)
    toolRestore
    run $"dotnet pack -c Release -o \"%s{Release.packageDir}\""
  }

  stage "Release" { run Release.release }
  runIfOnlySpecified true
}

tryPrintPipelineCommandHelp ()
