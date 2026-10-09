module LspTest

open Expecto
open Serilog
open FsAutoComplete.Logging
open System
open Serilog.Core
open Serilog.Events
open FsAutoComplete.Tests
open FsAutoComplete.Tests.CoreTest
open FsAutoComplete.Tests.ScriptTest
open FsAutoComplete.Tests.ExtensionsTests
open FsAutoComplete.Tests.InteractiveDirectivesTests
open FsAutoComplete.Tests.Lsp.CoreUtilsTests
open FsAutoComplete.Tests.Lsp.DecompilerTests
open FsAutoComplete.Tests.CallHierarchy
open Ionide.ProjInfo
open System.Threading
open Serilog.Filters
open System.IO
open FsAutoComplete
open Helpers
open FsToolkit.ErrorHandling

Expect.defaultDiffPrinter <- Diff.colourisedDiff


let testTimeout =
  Environment.GetEnvironmentVariable "TEST_TIMEOUT_MINUTES"
  |> Int32.TryParse
  |> function
    | true, duration -> duration
    | false, _ -> 10
  |> float
  |> TimeSpan.FromMinutes

// delay in ms between workspace start + stop notifications because the system goes too fast :-/
Environment.SetEnvironmentVariable("FSAC_WORKSPACELOAD_DELAY", "250")

// Child `dotnet` processes started by tests must not leave MSBuild nodes, the MSBuild server or the compiler server
// running, and parallel test hosts must not race to start a shared MSBuild server.
for name, value in
  [ "DOTNET_CLI_USE_MSBUILD_SERVER", "0"
    "MSBUILDDISABLENODEREUSE", "1"
    "UseSharedCompilation", "false" ] do
  Environment.SetEnvironmentVariable(name, value)

// Every directory a test runs `dotnet` in must resolve the SDK pinned in the test project directory. Servers without a
// workspace folder ask `dotnet` for its SDK in the current directory, which in-process MSBuild builds move to the
// project they build, so start in the test project directory, as CI does, and keep temporary workspaces inside it.
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

/// Path.GetTempPath reads TMPDIR on Linux and macOS, and TMP or TEMP on Windows. One directory per process, so that
/// test processes running side by side do not delete each other's files.
let testTempDirectory =
  Path.Combine(__SOURCE_DIRECTORY__, "TestResults", "tmp", string Environment.ProcessId)

Directory.CreateDirectory testTempDirectory |> ignore

// Unlike the system temporary directory, this one is inside the repository: without these, projects copied here would
// pick up the repository's Directory.Build.props and .targets (no implicit FSharp.Core, warnings as errors).
for name in [ "Directory.Build.props"; "Directory.Build.targets" ] do
  File.WriteAllText(Path.Combine(testTempDirectory, name), "<Project />")

// Remove the directories of test processes that ended without cleaning up, such as runs through `dotnet test`,
// which does not call `main`.
for leftover in Directory.EnumerateDirectories(Path.GetDirectoryName testTempDirectory) do
  match Int32.TryParse(Path.GetFileName leftover) with
  | true, pid when pid <> Environment.ProcessId ->
    let running =
      try
        Diagnostics.Process.GetProcessById pid |> ignore
        true
      with :? ArgumentException ->
        false

    if not running then
      try
        Directory.Delete(leftover, true)
      with _ ->
        ()
  | _ -> ()

for name in [ "TMPDIR"; "TMP"; "TEMP" ] do
  Environment.SetEnvironmentVariable(name, testTempDirectory)

let getEnvVarAsStr name = Environment.GetEnvironmentVariable(name) |> Option.ofObj

let (|EqIC|_|) (a: string) (b: string) =
  if String.Equals(a, b, StringComparison.OrdinalIgnoreCase) then
    Some()
  else
    None

let loaders =
  match getEnvVarAsStr "USE_WORKSPACE_LOADER" with
  | Some(EqIC "WorkspaceLoader") ->
    [ "Ionide WorkspaceLoader",
      (fun toolpath -> WorkspaceLoader.Create(toolpath, FsAutoComplete.Core.ProjectLoader.globalProperties)) ]
  | Some(EqIC "ProjectGraph") ->
    [ "MSBuild Project Graph WorkspaceLoader",
      (fun toolpath ->
        WorkspaceLoaderViaProjectGraph.Create(toolpath, FsAutoComplete.Core.ProjectLoader.globalProperties)) ]
  | _ ->
    [ "Ionide WorkspaceLoader",
      (fun toolpath -> WorkspaceLoader.Create(toolpath, FsAutoComplete.Core.ProjectLoader.globalProperties))
      // "MSBuild Project Graph WorkspaceLoader", (fun toolpath -> WorkspaceLoaderViaProjectGraph.Create(toolpath, FsAutoComplete.Core.ProjectLoader.globalProperties))
      ]


let adaptiveLspServerFactory toolsPath workspaceLoaderFactory sourceTextFactory =
  Helpers.createAdaptiveServer (fun () -> workspaceLoaderFactory toolsPath) sourceTextFactory

let sourceTextFactory: ISourceTextFactory = RoslynSourceTextFactory()

/// Ionide.ProjInfo loads MSBuild into the test host from the .NET SDK that `dotnet` resolves for the test project
/// directory. The tests need that SDK to have the major version of the test host runtime, as in CI, where build.fsx
/// pins it with a global.json in this directory. The same global.json then also applies to the child `dotnet` processes
/// and the script checks in TestCases.
let msbuild: Result<Types.ToolsPath, string> =
  let runtimeMajor = Environment.Version.Major
  let testProjectDirectory = DirectoryInfo __SOURCE_DIRECTORY__

  match Ionide.ProjInfo.Paths.dotnetRoot.Value with
  | None -> Error "No dotnet binary found. Set DOTNET_ROOT or add dotnet to PATH."
  | Some dotnet ->
    let resolvedSdk =
      try
        SdkDiscovery.versionAt testProjectDirectory dotnet
        |> Result.mapError (fun (_, _, _, output) -> output)
      with ex ->
        Error ex.Message

    match resolvedSdk with
    | Ok version when version.Major = runtimeMajor -> Ok(Init.init testProjectDirectory (Some dotnet))
    | _ ->
      let resolved =
        match resolvedSdk with
        | Ok version -> $"resolves SDK %O{version}"
        | Error output -> $"fails (%s{output})"

      let fix =
        SdkDiscovery.sdks dotnet
        |> Array.filter (fun sdk -> sdk.Version.Major = runtimeMajor)
        |> Array.sortBy _.Version
        |> Array.tryLast
        |> function
          | Some sdk ->
            $"Pin one with:\n  dotnet new globaljson --force --sdk-version %O{sdk.Version} --roll-forward latestFeature --output %s{testProjectDirectory.FullName}"
          | None -> $"Install a .NET %i{runtimeMajor} SDK, or run the target framework of an installed SDK."

      Error
        $"The test host runs on .NET %i{runtimeMajor}, but `dotnet --version` in %s{testProjectDirectory.FullName} %s{resolved}. The tests need a .NET %i{runtimeMajor} SDK. %s{fix}"

let compilers =
  match getEnvVarAsStr "USE_TRANSPARENT_COMPILER" with
  | Some(EqIC "TransparentCompiler") -> [ "TransparentCompiler", true ]
  | Some(EqIC "BackgroundCompiler") -> [ "BackgroundCompiler", false ]
  | _ -> [ "BackgroundCompiler", false; "TransparentCompiler", true ]

let rec private groupName test =
  match test with
  | Test.TestLabel(name, _, _) -> Some name
  | Test.Sequenced(_, test) -> groupName test
  | Test.TestList([ test ], _) -> groupName test
  | _ -> None

let rec private mapTestCode (map: TestCode -> TestCode) test =
  match test with
  | Test.TestCase(code, state) -> Test.TestCase(map code, state)
  | Test.TestList(tests, state) -> Test.TestList(List.map (mapTestCode map) tests, state)
  | Test.TestLabel(label, test, state) -> Test.TestLabel(label, mapTestCode map test, state)
  | Test.Sequenced(sequenced, test) -> Test.Sequenced(sequenced, mapTestCode map test)

/// Wraps every test of `group` so that the last one to finish calls `allDone`, whatever order the tests run in.
/// Pending tests never run, so they are not counted. When a filter or a focused test leaves out tests of the group,
/// `allDone` is not called, and the servers live until the process ends.
let private afterLastTest (allDone: unit -> unit) (group: Test) =
  let rec runnable pending test =
    match test with
    | Test.TestCase(_, state) -> if pending || state = Pending then 0 else 1
    | Test.TestList(tests, state) -> tests |> List.sumBy (runnable (pending || state = Pending))
    | Test.TestLabel(_, test, state) -> runnable (pending || state = Pending) test
    | Test.Sequenced(_, test) -> runnable pending test

  let remaining = ref (runnable false group)

  let finished () =
    if Interlocked.Decrement(&remaining.contents) = 0 then
      allDone ()

  let wrap code =
    match code with
    | TestCode.Sync test ->
      TestCode.Sync(fun () ->
        try
          test ()
        finally
          finished ())
    | TestCode.SyncWithCancel test ->
      TestCode.SyncWithCancel(fun ct ->
        try
          test ct
        finally
          finished ())
    | TestCode.Async test ->
      TestCode.Async(
        async {
          try
            do! test
          finally
            finished ()
        }
      )
    | TestCode.AsyncFsCheck(config, stressConfig, test) ->
      TestCode.AsyncFsCheck(
        config,
        stressConfig,
        fun fsCheckConfig ->
          async {
            try
              do! test fsCheckConfig
            finally
              finished ()
          }
      )

  mapTestCode wrap group

/// Gives a test group its own server factory, and shuts down every server the group started once its last test is done.
let private withServerShutdown (createServer: unit -> FsAutoComplete.Lsp.IFSharpLspServer * ClientEvents) group =
  let started = System.Collections.Concurrent.ConcurrentQueue<unit -> unit>()

  let tests =
    group (fun () ->
      let server, events = createServer ()
      let handle, shutdown = handleFor server
      started.Enqueue shutdown
      handle, events)

  tests
  // Also the tests that were written without a timeout. Tests that have one get a second, equal one.
  |> mapTestCode (Helpers.Expecto.cancelOnTimeout Helpers.Expecto.DEFAULT_TIMEOUT)
  |> afterLastTest (fun () ->
    let mutable shutdown = ignore

    // A server that fails to shut down must not fail the test that happened to finish last, nor keep the
    // remaining servers alive.
    while started.TryDequeue(&shutdown) do
      try
        shutdown ()
      with e ->
        Helpers.logger.Value.Warning(e, "A server of the test group failed to shut down"))

let rec private withoutSequencing test =
  match test with
  | Test.Sequenced(_, test) -> withoutSequencing test
  | Test.TestList(tests, state) -> Test.TestList(List.map withoutSequencing tests, state)
  | Test.TestLabel(label, test, state) -> Test.TestLabel(label, withoutSequencing test, state)
  | Test.TestCase _ -> test

/// Runs `group` in Expecto's sequential phase, after every parallel test. For groups that change process-wide state,
/// such as the current directory or environment variables: an in-process MSBuild build of another group saves both
/// when it starts and restores them when it ends, which would undo or bring back such a change.
let private inSequentialPhase group = Test.Sequenced(SequenceMethod.Synchronous, withoutSequencing group)

/// Runs `group` in Expecto's parallel phase, next to other groups. Its own tests still run one after another, and
/// one after another with the group of the same name for the other compiler, which uses the same TestCases folders.
let private inParallelPhase group =
  let key =
    groupName group
    |> Option.defaultWith (fun () -> failwith "A test group in the parallel phase needs a name")

  Test.Sequenced(SequenceMethod.SynchronousGroup key, withoutSequencing group)

let lspTests toolsPath =
  testList
    "lsp"
    [ for (loaderName, workspaceLoaderFactory) in loaders do

        testList
          $"{loaderName}"
          [ for (compilerName, useTransparentCompiler) in compilers do
              let createServer () =
                adaptiveLspServerFactory toolsPath workspaceLoaderFactory sourceTextFactory useTransparentCompiler

              let servers group = withServerShutdown createServer group |> inParallelPhase

              let serversInSequentialPhase group = withServerShutdown createServer group |> inSequentialPhase

              let compilerTests =
                [ inSequentialPhase (Templates.tests ())
                  servers initTests
                  servers closeTests

                  servers Utils.Tests.Server.tests
                  servers Utils.Tests.CursorbasedTests.tests

                  servers CodeLens.tests
                  servers documentSymbolTest
                  servers workspaceSymbolTest
                  servers Completion.autocompleteTest
                  servers Completion.autoOpenTests
                  servers Completion.fullNameExternalAutocompleteTest
                  servers foldingTests
                  servers tooltipTests
                  servers Highlighting.tests
                  servers scriptPreviewTests
                  servers scriptEvictionTests
                  servers scriptProjectOptionsCacheTests
                  servers dependencyManagerTests
                  inParallelPhase interactiveDirectivesUnitTests

                  // commented out because FSDN is down
                  //fsdnTest createServer

                  //linterTests createServer
                  inParallelPhase uriTests
                  servers formattingTests
                  servers analyzerTests
                  servers signatureTests
                  servers SignatureHelp.tests
                  servers InlineHints.tests
                  servers (CodeFixTests.Tests.tests sourceTextFactory)
                  servers Completion.tests
                  serversInSequentialPhase GoTo.tests

                  servers FindReferences.tests
                  servers Rename.tests

                  servers InfoPanelTests.docFormattingTest
                  servers DetectUnitTests.tests
                  servers XmlDocumentationGeneration.tests
                  servers InlayHintTests.tests
                  servers DependentFileChecking.tests
                  servers UnusedDeclarationsTests.tests
                  servers EmptyFileTests.tests
                  servers CallHierarchy.tests
                  servers diagnosticsTest
                  servers InheritDocTooltipTests.tests
                  servers CrefLinkDocumentationTests.tests

                  serversInSequentialPhase TestExplorer.tests ]

              testList $"{compilerName}" compilerTests ] ]

let expectedRuntimeMajor =
  System.Reflection.CustomAttributeExtensions
    .GetCustomAttribute<System.Runtime.Versioning.TargetFrameworkAttribute>(
      System.Reflection.Assembly.GetExecutingAssembly()
    )
    .FrameworkName
  |> System.Runtime.Versioning.FrameworkName
  |> _.Version.Major

/// Tests that do not require a LSP server
let generalTests =
  testList
    "general"
    [ testCase "test host uses target runtime" (fun _ ->
        Expect.equal Environment.Version.Major expectedRuntimeMajor "Test host runtime must match the target framework")
      testList (nameof (Utils)) [ Utils.Tests.Utils.tests; Utils.Tests.TextEdit.tests ]
      InlayHintTests.explicitTypeInfoTests sourceTextFactory
      FindReferences.tryFixupRangeTests sourceTextFactory
      UtilsTests.allTests
      LspHelpersTests.allTests
      TipFormatterTests.allTests
      FcsInvariantTests.tests
      FsProjEditorTests.allTests
      FsAutoComplete.Tests.Lsp.AdaptiveExtensionsTests.tests
      FsAutoComplete.Tests.Lsp.TimeoutTests.tests
      FsAutoComplete.Tests.Lsp.WorkspaceLoadFailureTests.tests sourceTextFactory
      decompilerTests ]

[<Tests>]
let tests =
  match msbuild with
  | Error message ->
    testList "FSAC" [ testCase "test host and .NET SDK major versions match" (fun _ -> failtest message) ]
  | Ok toolsPath ->
    testList
      "FSAC"
      [ generalTests
        lspTests toolsPath
        SnapshotTests.snapshotTests loaders toolsPath ]

open OpenTelemetry
open OpenTelemetry.Resources
open OpenTelemetry.Trace
open OpenTelemetry.Logs
open OpenTelemetry.Metrics
open System.Diagnostics
open FsAutoComplete.Telemetry

/// Expecto's default printer, plus the names of the tests that did not pass at the end of the run. Expecto's
/// summary printers list every test, including the thousands that passed, which buries the failures.
let private failuresAtTheEnd (inner: Expecto.Impl.TestPrinters) =
  { inner with
      summary =
        fun config summary ->
          async {
            do! inner.summary config summary

            let names label (tests: (FlatTest * Expecto.Impl.TestSummary) list) =
              tests
              |> List.map (fun (test, _) -> $"{label}: {config.joinWith.format test.name}")

            match names "Failed" summary.failed @ names "Errored" summary.errored with
            | [] -> ()
            | notPassed ->
              do!
                Expecto.Logging.Log.create("Expecto").logWithAck
                  Expecto.Logging.Info
                  (Expecto.Logging.Message.eventX "Tests that did not pass:\n{tests}"
                   >> Expecto.Logging.Message.setField "tests" (String.concat "\n" notPassed))
          } }

let runTests (args: string[]) =
  let serviceName = "FsAutoComplete.Tests.Lsp"

  use traceProvider =
    let version = FsAutoComplete.Utils.Version.info().Version

    Sdk
      .CreateTracerProviderBuilder()
      .AddSource(FsAutoComplete.Utils.Tracing.serviceName, Tracing.fscServiceName, serviceName)
      .SetResourceBuilder(
        ResourceBuilder.CreateDefault().AddService(serviceName = serviceName, serviceVersion = version)
      )
      .AddOtlpExporter()
      .Build()

  let outputTemplate =
    "[{Timestamp:HH:mm:ss} {Level:u3}] [{SourceContext}] {Message:lj}{NewLine}{Exception}"

  let parseLogLevel (args: string[]) =
    let logMarker = "--log="

    let logLevel =
      match
        args
        |> Array.tryFind (fun arg -> arg.StartsWith(logMarker, StringComparison.Ordinal))
        |> Option.map (fun log -> log.Substring(logMarker.Length))
      with
      | Some("warn" | "warning") -> Logging.LogLevel.Warn
      | Some "error" -> Logging.LogLevel.Error
      | Some "fatal" -> Logging.LogLevel.Fatal
      | Some "info" -> Logging.LogLevel.Info
      | Some "verbose" -> Logging.LogLevel.Verbose
      | Some "debug" -> Logging.LogLevel.Debug
      | _ -> Logging.LogLevel.Warn

    let args =
      args
      |> Array.filter (fun arg -> not <| arg.StartsWith(logMarker, StringComparison.Ordinal))

    logLevel, args

  let expectoToSerilogLevel =
    function
    | Logging.LogLevel.Debug -> LogEventLevel.Debug
    | Logging.LogLevel.Verbose -> LogEventLevel.Verbose
    | Logging.LogLevel.Info -> LogEventLevel.Information
    | Logging.LogLevel.Warn -> LogEventLevel.Warning
    | Logging.LogLevel.Error -> LogEventLevel.Error
    | Logging.LogLevel.Fatal -> LogEventLevel.Fatal

  let parseLogExcludes (args: string[]) =
    let excludeMarker = "--exclude-from-log="

    let toExclude =
      args
      |> Array.filter (fun arg -> arg.StartsWith(excludeMarker, StringComparison.Ordinal))
      |> Array.collect (fun arg -> arg.Substring(excludeMarker.Length).Split(','))

    let args =
      args
      |> Array.filter (fun arg -> not <| arg.StartsWith(excludeMarker, StringComparison.Ordinal))

    toExclude, args

  let logLevel, args = parseLogLevel args
  let switch = LoggingLevelSwitch(expectoToSerilogLevel logLevel)
  let logSourcesToExclude, args = parseLogExcludes args

  let sourcesToExclude =
    Matching.WithProperty<string>(
      Constants.SourceContextPropertyName,
      fun s -> s <> null && logSourcesToExclude |> Array.contains s
    )

  let argsToRemove, _loaders =
    args
    |> Array.windowed 2
    |> Array.tryPick (function
      | [| "--loader"; "ionide" |] as args -> Some(args, [ "Ionide WorkspaceLoader", WorkspaceLoader.Create ])
      | [| "--loader"; "graph" |] as args ->
        Some(args, [ "MSBuild Project Graph WorkspaceLoader", WorkspaceLoaderViaProjectGraph.Create ])
      | _ -> None)
    |> Option.defaultValue ([||], loaders)

  // Logs go to stderr through a writer of their own. Writing them through System.Console deadlocks on Linux and macOS:
  // Expecto redirects Console.Out and Console.Error, and a log line and a test's printfn then take the console locks
  // in opposite order.
  let logWriter = new StreamWriter(Console.OpenStandardError(), AutoFlush = true)

  let logFormatter =
    Serilog.Formatting.Display.MessageTemplateTextFormatter(outputTemplate)

  // Serilog's async wrapper calls the sink from a single thread.
  let logSink =
    { new ILogEventSink with
        member _.Emit(logEvent) = logFormatter.Format(logEvent, logWriter) }

  let serilogLogger =
    LoggerConfiguration()
      .Enrich.FromLogContext()
      .MinimumLevel.ControlledBy(switch)
      .Filter.ByExcluding(Matching.FromSource("FileSystem"))
      .Filter.ByExcluding(sourcesToExclude)

      .Destructure.FSharpTypes()
      .Destructure.ByTransforming<FSharp.Compiler.Text.Range>(fun r ->
        box
          {| FileName = r.FileName
             Start = r.Start
             End = r.End |})
      .Destructure.ByTransforming<FSharp.Compiler.Text.Position>(fun r -> box {| Line = r.Line; Column = r.Column |})
      .Destructure.ByTransforming<Newtonsoft.Json.Linq.JToken>(fun tok -> tok.ToString() |> box)
      .Destructure.ByTransforming<System.IO.DirectoryInfo>(fun di -> box di.FullName)
      .WriteTo.Async(fun c -> c.Sink(logSink) |> ignore)
      .CreateLogger() // make it so that every console log is logged to stderr

  // uncomment these next two lines if you want verbose output from the LSP server _during_ your tests
  Serilog.Log.Logger <- serilogLogger
  LogProvider.setLoggerProvider (Providers.SerilogProvider.create ())

  let fixedUpArgs = args |> Array.except argsToRemove

  let cts = new CancellationTokenSource(testTimeout)
  use activitySource = new ActivitySource(serviceName)

  let cliArgs =
    [ CLIArguments.Printer(failuresAtTheEnd defaultConfig.printer)
      CLIArguments.Verbosity Expecto.Logging.LogLevel.Info
      CLIArguments.Parallel
      // Every LSP test group starts its own servers, so more workers mostly add memory. `--parallel-workers` overrides it.
      CLIArguments.Parallel_Workers 4 ]
  // let trace = traceProvider.GetTracer("FsAutoComplete.Tests.Lsp")
  // use span =  trace.StartActiveSpan("runTests", SpanKind.Internal)
  use span = activitySource.StartActivity("runTests")
  let exitCode = runTestsWithCLIArgsAndCancel cts.Token cliArgs fixedUpArgs tests
  // Stop the timer, so a run that just finished is not taken for a cancelled one.
  cts.CancelAfter System.Threading.Timeout.Infinite

  // Expecto returns 0 for a cancelled run: the tests that did not start are not failures.
  if cts.IsCancellationRequested then
    eprintfn $"The run was cancelled after {testTimeout} (TEST_TIMEOUT_MINUTES), not every test ran."
    max exitCode 1
  else
    exitCode

[<EntryPoint>]
let main args =
  let exitCode = runTests args
  Serilog.Log.CloseAndFlush()

  try
    Directory.Delete(testTempDirectory, true)
  with _ ->
    ()

  // Tests do not dispose every server they start, and a live server can keep foreground threads running.
  // Returning from main would then wait for those threads forever, so end the process explicitly.
  exit exitCode
