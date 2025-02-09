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
open OpenTelemetry.Trace

Expect.defaultDiffPrinter <- Diff.colourisedDiff

let resourceBuilder version =
    ResourceBuilder
        .CreateDefault()
        .AddService(serviceName = serviceName, serviceVersion = version)

type SpanFilter (filter : Activity -> bool) =
  inherit BaseProcessor<Activity>()

  override x.OnEnd (span: Activity): unit =
    if filter span then span.ActivityTraceFlags <- span.ActivityTraceFlags &&& (~~~ActivityTraceFlags.Recorded)
    else base.OnEnd(span: Activity)

type TracerProviderBuilder with
  member x.AddSpanFilter(filter : Activity -> bool) =
    x.AddProcessor(new SpanFilter(filter))

let traceProvider () =
  let version = FsAutoComplete.Utils.Version.info().Version
  Sdk
      .CreateTracerProviderBuilder()
      .AddSource(FsAutoComplete.Utils.Tracing.serviceName, Tracing.fscServiceName, serviceName)
      .SetResourceBuilder(resourceBuilder version)
      // .AddConsoleExporter()
      // .AddOtlpExporter()
      .AddSpanFilter((fun span -> span.DisplayName.Contains "DiagnosticsLogger")) // DiagnosticsLogger.StackGuard.Guard is too noisy
      .AddOtlpExporter(fun opt ->
        opt.Endpoint <- Uri "http://localhost:4317"
      )
      // .AddOtlpExporter(fun opt ->
      //   opt.Endpoint <- Uri "http://localhost:5341/ingest/otlp/v1/traces"
      //   opt.Protocol <- OtlpExportProtocol.HttpProtobuf
      // )
      .Build()
do
    let provider = traceProvider()
    AppDomain.CurrentDomain.ProcessExit.Add(fun _ ->
      provider.ForceFlush(3000) |> ignore
      // provider.Dispose()
      )


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

let getEnvVarAsStr name =
  Environment.GetEnvironmentVariable(name)
  |> Option.ofObj

let (|EqIC|_|) (a: string) (b: string) =
  if String.Equals(a, b, StringComparison.OrdinalIgnoreCase) then Some () else None

let loaders =
  match getEnvVarAsStr "USE_WORKSPACE_LOADER" with
  | Some (EqIC "WorkspaceLoader") -> [ "Ionide WorkspaceLoader", (fun toolpath -> WorkspaceLoader.Create(toolpath, FsAutoComplete.Core.ProjectLoader.globalProperties)) ]
  | Some (EqIC "ProjectGraph") -> [ "MSBuild Project Graph WorkspaceLoader", (fun toolpath -> WorkspaceLoaderViaProjectGraph.Create(toolpath, FsAutoComplete.Core.ProjectLoader.globalProperties)) ]
  | _ ->
    [
      "Ionide WorkspaceLoader", (fun toolpath -> WorkspaceLoader.Create(toolpath, FsAutoComplete.Core.ProjectLoader.globalProperties))
      // "MSBuild Project Graph WorkspaceLoader", (fun toolpath -> WorkspaceLoaderViaProjectGraph.Create(toolpath, FsAutoComplete.Core.ProjectLoader.globalProperties))
    ]


let adaptiveLspServerFactory toolsPath workspaceLoaderFactory sourceTextFactory =
  Helpers.createAdaptiveServer (fun () -> workspaceLoaderFactory toolsPath) sourceTextFactory

let sourceTextFactory: ISourceTextFactory = RoslynSourceTextFactory()

let mutable toolsPath =
  Ionide.ProjInfo.Init.init (System.IO.DirectoryInfo Environment.CurrentDirectory) None



let compilers =
  match getEnvVarAsStr "USE_TRANSPARENT_COMPILER" with
  | Some (EqIC "TransparentCompiler") -> ["TransparentCompiler", true ]
  | Some (EqIC "BackgroundCompiler") -> [ "BackgroundCompiler", false ]
  | _ ->
    [
      "BackgroundCompiler", false
      "TransparentCompiler", true
    ]

let otelTests = OpenTelemetry.addOpenTelemetry_SpanPerTest Expecto.Impl.ExpectoConfig.defaultConfig  source

let lspTests =

  testSequenced <|
  testList
    "lsp"
    [ for (loaderName, workspaceLoaderFactory) in loaders do

        testList
          $"{loaderName}"
          [
            for (compilerName, useTransparentCompiler) in compilers do
              testList
                $"{compilerName}"
                [
                  Templates.tests ()
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
                  ] ] ]

/// Tests that do not require a LSP server
let generalTests = testList "general" [
  testList (nameof (Utils)) [ Utils.Tests.Utils.tests; Utils.Tests.TextEdit.tests ]
  InlayHintTests.explicitTypeInfoTests sourceTextFactory
  FindReferences.tryFixupRangeTests sourceTextFactory
]

[<Tests>]
let tests =
  testList "FSAC" [
    generalTests
    lspTests
    SnapshotTests.snapshotTests loaders toolsPath
  ]
  |> otelTests

[<EntryPoint>]
let main args =

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
    [
      // CLIArguments.Printer(Expecto.Impl.TestPrinters.summaryWithLocationPrinter defaultConfig.printer)
      CLIArguments.Verbosity Expecto.Logging.LogLevel.Info
      CLIArguments.Parallel
    ]

  runTestsWithCLIArgsAndCancel cts.Token cliArgs fixedUpArgs tests
