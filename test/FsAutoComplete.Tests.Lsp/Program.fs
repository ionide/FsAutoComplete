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

let mutable toolsPath =
  Ionide.ProjInfo.Init.init (System.IO.DirectoryInfo Environment.CurrentDirectory) None



let compilers =
  match getEnvVarAsStr "USE_TRANSPARENT_COMPILER" with
  | Some(EqIC "TransparentCompiler") -> [ "TransparentCompiler", true ]
  | Some(EqIC "BackgroundCompiler") -> [ "BackgroundCompiler", false ]
  | _ -> [ "BackgroundCompiler", false; "TransparentCompiler", true ]

let testShard =
  match getEnvVarAsStr "FSAC_TEST_SHARD" with
  | None -> None
  | Some("1" | "2" | "3" | "4" as shard) -> Some(int shard)
  | Some shard -> invalidArg "FSAC_TEST_SHARD" $"FSAC_TEST_SHARD must be 1, 2, 3, or 4. Actual value: %s{shard}"

let selectTestGroups groups =
  match testShard with
  | None -> groups |> List.map snd
  | Some selectedShard ->
    groups
    |> List.choose (fun (shard, test) -> if shard = selectedShard then Some test else None)

let lspTests =
  testSequenced
  <| testList
    "lsp"
    [ for (loaderName, workspaceLoaderFactory) in loaders do

        testList
          $"{loaderName}"
          [ for (compilerName, useTransparentCompiler) in compilers do
              let createServer () =
                adaptiveLspServerFactory toolsPath workspaceLoaderFactory sourceTextFactory useTransparentCompiler

              // Shard 1 carries general tests and shard 4 carries snapshots; keep shared fixtures together and add isolated groups to the fastest measured shard.
              let compilerTests =
                [ 4, Templates.tests ()
                  4, initTests createServer
                  4, closeTests createServer

                  1, Utils.Tests.Server.tests createServer
                  4, Utils.Tests.CursorbasedTests.tests createServer

                  4, CodeLens.tests createServer
                  4, documentSymbolTest createServer
                  4, workspaceSymbolTest createServer
                  4, Completion.autocompleteTest createServer
                  2, Completion.autoOpenTests createServer
                  3, Completion.fullNameExternalAutocompleteTest createServer
                  4, foldingTests createServer
                  4, tooltipTests createServer
                  4, Highlighting.tests createServer
                  4, scriptPreviewTests createServer
                  4, scriptEvictionTests createServer
                  4, scriptProjectOptionsCacheTests createServer
                  4, dependencyManagerTests createServer
                  4, interactiveDirectivesUnitTests

                  // commented out because FSDN is down
                  //fsdnTest createServer

                  //linterTests createServer
                  4, uriTests
                  4, formattingTests createServer
                  4, analyzerTests createServer
                  4, signatureTests createServer
                  4, SignatureHelp.tests createServer
                  4, InlineHints.tests createServer
                  2, CodeFixTests.Tests.tests sourceTextFactory createServer
                  4, Completion.tests createServer
                  3, GoTo.tests createServer

                  4, FindReferences.tests createServer
                  3, Rename.tests createServer

                  4, InfoPanelTests.docFormattingTest createServer
                  4, DetectUnitTests.tests createServer
                  4, XmlDocumentationGeneration.tests createServer
                  4, InlayHintTests.tests createServer
                  3, DependentFileChecking.tests createServer
                  2, UnusedDeclarationsTests.tests createServer
                  4, EmptyFileTests.tests createServer
                  3, CallHierarchy.tests createServer
                  4, diagnosticsTest createServer
                  4, InheritDocTooltipTests.tests createServer
                  4, CrefLinkDocumentationTests.tests createServer

                  3, TestExplorer.tests createServer ]

              testList $"{compilerName}" (selectTestGroups compilerTests) ] ]

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
      decompilerTests ]

[<Tests>]
let tests =
  match testShard with
  | None -> testList "FSAC" [ generalTests; lspTests; SnapshotTests.snapshotTests loaders toolsPath ]
  | Some 1 -> testList "FSAC" [ generalTests; lspTests ]
  | Some 4 -> testList "FSAC" [ lspTests; SnapshotTests.snapshotTests loaders toolsPath ]
  | Some _ -> testList "FSAC" [ lspTests ]

open OpenTelemetry
open OpenTelemetry.Resources
open OpenTelemetry.Trace
open OpenTelemetry.Logs
open OpenTelemetry.Metrics
open System.Diagnostics
open FsAutoComplete.Telemetry

[<EntryPoint>]
let main args =
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
      .WriteTo.Async(fun c ->
        c.Console(
          outputTemplate = outputTemplate,
          standardErrorFromLevel = Nullable<_>(LogEventLevel.Verbose),
          theme = Serilog.Sinks.SystemConsole.Themes.AnsiConsoleTheme.Code
        )
        |> ignore)
      .CreateLogger() // make it so that every console log is logged to stderr

  // uncomment these next two lines if you want verbose output from the LSP server _during_ your tests
  Serilog.Log.Logger <- serilogLogger
  LogProvider.setLoggerProvider (Providers.SerilogProvider.create ())

  let fixedUpArgs = args |> Array.except argsToRemove

  let cts = new CancellationTokenSource(testTimeout)
  use activitySource = new ActivitySource(serviceName)

  let cliArgs =
    [ CLIArguments.Printer(Expecto.Impl.TestPrinters.summaryWithLocationPrinter defaultConfig.printer)
      CLIArguments.Verbosity Expecto.Logging.LogLevel.Info
      CLIArguments.Parallel ]
  // let trace = traceProvider.GetTracer("FsAutoComplete.Tests.Lsp")
  // use span =  trace.StartActiveSpan("runTests", SpanKind.Internal)
  use span = activitySource.StartActivity("runTests")
  runTestsWithCLIArgsAndCancel cts.Token cliArgs fixedUpArgs tests
