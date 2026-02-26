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
open FsAutoComplete.Tests.CallHierarchy
open Ionide.ProjInfo
open System.Threading
open Serilog.Filters
open System.IO
open FsAutoComplete
open Helpers
open FsToolkit.ErrorHandling
open System.Diagnostics
open OpenTelemetry.Resources
open OpenTelemetry
open OpenTelemetry.Exporter
open OpenTelemetry.Exporter.OtlpFile
open OpenTelemetry.Trace

Expect.defaultDiffPrinter <- Diff.colourisedDiff

let resourceBuilder version =
  ResourceBuilder.CreateDefault().AddService(serviceName = serviceName, serviceVersion = version)

/// Check if we're running in GitHub Actions CI
let isCI = Environment.GetEnvironmentVariable("CI") = "true"

/// Directory to write failed test traces to
let failedTracesDirectory =
  let dir =
    Environment.GetEnvironmentVariable("FAILED_TRACES_DIR")
    |> Option.ofObj
    |> Option.defaultValue "failed_traces"

  Path.GetFullPath(dir)

type SpanFilter(filter: Activity -> bool) =
  inherit BaseProcessor<Activity>()

  override x.OnEnd(span: Activity) : unit =
    if filter span then
      span.ActivityTraceFlags <- span.ActivityTraceFlags &&& (~~~ActivityTraceFlags.Recorded)
    else
      base.OnEnd(span: Activity)

type TracerProviderBuilder with
  member x.AddSpanFilter(filter: Activity -> bool) = x.AddProcessor(new SpanFilter(filter))

// Module-level OTel state – shared between module init and main() for dotnet run/dotnet test compat
let private currentTraceProvider: TracerProvider option ref = ref None

let private currentFailedTestExporter: FailedTestOtlpFileExportProcessor option ref =
  ref None

let private tracesAlreadyWritten = ref false

/// Write failed test traces to file and print a GitHub Actions error annotation.
/// Safe to call multiple times – only the first call writes.
let private writeFailedTraces () =
  if not tracesAlreadyWritten.Value then
    tracesAlreadyWritten.Value <- true

    match currentFailedTestExporter.Value with
    | Some exporter ->
      let traceResult = exporter.WriteToFile()

      match traceResult with
      | Some(file, count) ->
        if not (Directory.Exists failedTracesDirectory) then
          Directory.CreateDirectory failedTracesDirectory |> ignore

        let summaryFile = Path.Combine(failedTracesDirectory, "summary.txt")
        File.WriteAllText(summaryFile, $"Failed test traces: {file}\nFailed test count: {count}")
        printfn $"::error::Found {count} failed test(s). Traces written to {file}"
      | None -> printfn "No failed tests – no traces written"
    | None -> ()

/// Flush the current trace provider (no-op when not configured).
let private flushTraceProvider () =
  match currentTraceProvider.Value with
  | Some provider -> provider.ForceFlush(3000) |> ignore
  | None -> ()

// Initialize OTel at module level so it works for both `dotnet run` and `dotnet test`.
// For CI: export only failed-test spans to OTLP JSON files.
// For local dev: send all spans to an OTLP endpoint when OTEL_EXPORTER_OTLP_ENDPOINT is set.
do
  let version = FsAutoComplete.Utils.Version.info().Version

  let baseBuilder =
    Sdk
      .CreateTracerProviderBuilder()
      .AddSource(FsAutoComplete.Utils.Tracing.serviceName, Tracing.fscServiceName, serviceName)
      .SetResourceBuilder(resourceBuilder version)
      .AddSpanFilter((fun span -> span.DisplayName.Contains "DiagnosticsLogger")) // DiagnosticsLogger.StackGuard.Guard is too noisy

  if isCI then
    printfn $"Running in CI mode – failed test traces will be written to: {failedTracesDirectory}"

    let builder, exporter =
      baseBuilder.AddFailedTestOtlpFileExporter(fun opts ->
        opts.OutputDirectory <- failedTracesDirectory
        opts.ServiceName <- serviceName
        opts.ServiceVersion <- version)

    currentTraceProvider.Value <- Some(builder.Build())
    currentFailedTestExporter.Value <- Some exporter

    AppDomain.CurrentDomain.ProcessExit.Add(fun _ ->
      writeFailedTraces ()
      flushTraceProvider ())
  else
    // Use the standard OTEL_EXPORTER_OTLP_ENDPOINT env var to enable local-dev tracing.
    // Example: OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4317
    let otlpEndpoint = Environment.GetEnvironmentVariable "OTEL_EXPORTER_OTLP_ENDPOINT"

    if not (String.IsNullOrEmpty otlpEndpoint) then
      currentTraceProvider.Value <- Some(baseBuilder.AddOtlpExporter().Build())

      AppDomain.CurrentDomain.ProcessExit.Add(fun _ -> flushTraceProvider ())


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

let otelTests =
  OpenTelemetry.addOpenTelemetry_SpanPerTest Expecto.Impl.ExpectoConfig.defaultConfig source

let lspTests =

  testSequenced
  <| testList
    "lsp"
    [ for (loaderName, workspaceLoaderFactory) in loaders do

        testList
          $"{loaderName}"
          [ for (compilerName, useTransparentCompiler) in compilers do
              testList
                $"{compilerName}"
                [ Templates.tests ()
                  let createServer () =
                    adaptiveLspServerFactory toolsPath workspaceLoaderFactory sourceTextFactory useTransparentCompiler

                  initTests createServer
                  closeTests createServer

                  Utils.Tests.Server.tests createServer
                  Utils.Tests.CursorbasedTests.tests createServer

                  CodeLens.tests createServer
                  documentSymbolTest createServer
                  workspaceSymbolTest createServer
                  Completion.autocompleteTest createServer
                  Completion.autoOpenTests createServer
                  Completion.fullNameExternalAutocompleteTest createServer
                  foldingTests createServer
                  tooltipTests createServer
                  Highlighting.tests createServer
                  scriptPreviewTests createServer
                  scriptEvictionTests createServer
                  scriptProjectOptionsCacheTests createServer
                  dependencyManagerTests createServer
                  interactiveDirectivesUnitTests

                  // commented out because FSDN is down
                  //fsdnTest createServer

                  //linterTests createServer
                  uriTests
                  formattingTests createServer
                  analyzerTests createServer
                  signatureTests createServer
                  SignatureHelp.tests createServer
                  InlineHints.tests createServer
                  CodeFixTests.Tests.tests sourceTextFactory createServer
                  Completion.tests createServer
                  GoTo.tests createServer

                  FindReferences.tests createServer
                  Rename.tests createServer

                  InfoPanelTests.docFormattingTest createServer
                  DetectUnitTests.tests createServer
                  XmlDocumentationGeneration.tests createServer
                  InlayHintTests.tests createServer
                  DependentFileChecking.tests createServer
                  UnusedDeclarationsTests.tests createServer
                  EmptyFileTests.tests createServer
                  CallHierarchy.tests createServer
                  diagnosticsTest createServer
                  InheritDocTooltipTests.tests createServer

                  TestExplorer.tests createServer ] ] ]

/// Tests that do not require a LSP server
let generalTests =
  testList
    "general"
    [ testList (nameof (Utils)) [ Utils.Tests.Utils.tests; Utils.Tests.TextEdit.tests ]
      InlayHintTests.explicitTypeInfoTests sourceTextFactory
      FindReferences.tryFixupRangeTests sourceTextFactory
      UtilsTests.allTests
      LspHelpersTests.allTests
      TipFormatterTests.allTests
      FcsInvariantTests.tests ]

[<Tests>]
let tests =
  testList "FSAC" [ generalTests; lspTests; SnapshotTests.snapshotTests loaders toolsPath ]
  |> otelTests

open FsAutoComplete.Telemetry

[<EntryPoint>]
let main args =
  // OTel is already initialized at module level (works for both `dotnet run` and `dotnet test`).
  // See the module-level `do` block above for the provider setup.

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

  let cliArgs =
    [ CLIArguments.Printer(Expecto.Impl.TestPrinters.summaryWithLocationPrinter defaultConfig.printer)
      CLIArguments.Verbosity Expecto.Logging.LogLevel.Info
      CLIArguments.Parallel ]
  // let trace = traceProvider.GetTracer("FsAutoComplete.Tests.Lsp")
  // use span =  trace.StartActiveSpan("runTests", SpanKind.Internal)
  use span = source.StartActivity("runTests")
  let result = runTestsWithCLIArgsAndCancel cts.Token cliArgs fixedUpArgs tests

  // Explicitly write traces when running via `dotnet run` so the annotation is printed
  // before the process exits.  ProcessExit will no-op because tracesAlreadyWritten is set.
  writeFailedTraces ()
  flushTraceProvider ()

  result
