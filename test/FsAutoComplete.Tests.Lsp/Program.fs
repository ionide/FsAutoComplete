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

let testTimeout =
  Environment.GetEnvironmentVariable "TEST_TIMEOUT_MINUTES"
  |> Int32.TryParse
  |> function true, duration -> duration
            | false, _ -> 10
  |> float
  |> TimeSpan.FromMinutes

// delay in ms between workspace start + stop notifications because the system goes too fast :-/
Environment.SetEnvironmentVariable("FSAC_WORKSPACELOAD_DELAY", "250")

let loaders = [
  "Ionide WorkspaceLoader",  WorkspaceLoader.Create
  // "MSBuild Project Graph WorkspaceLoader", WorkspaceLoaderViaProjectGraph.Create
]

let mutable toolsPath = Ionide.ProjInfo.Init.init (System.IO.DirectoryInfo Environment.CurrentDirectory) None

[<Tests>]
let tests =
  testSequenced <| testList "lsp" [
    for (name, workspaceLoaderFactory) in loaders do
      testSequenced <| testList name [
        Templates.tests()
        let state = FsAutoComplete.State.Initial toolsPath workspaceLoaderFactory
        initTests state
        codeLensTest state
        documentSymbolTest state
        Completion.autocompleteTest state
        Completion.autoOpenTests state
        Rename.tests state
        foldingTests state
        tooltipTests state
        Highlighting.tests state
        scriptPreviewTests state
        scriptEvictionTests state
        scriptProjectOptionsCacheTests state
        dependencyManagerTests  state
        interactiveDirectivesUnitTests

        // commented out because FSDN is down
        //fsdnTest state
        uriTests
        //linterTests state
        formattingTests state
        // fake isn't updated to FCS 39, disabling tests until that's resolved
        //fakeInteropTests toolsPath
        analyzerTests state
        signatureTests state
        SignatureHelp.tests state
        CodeFixTests.tests state
        Completion.tests state
        GoTo.tests state
        FindReferences.tests state
        InfoPanelTests.docFormattingTest state
        DetectNUnitTests.tests state
      ]
  ]


[<EntryPoint>]
let main args =
  let outputTemplate = "[{Timestamp:HH:mm:ss} {Level:u3}] [{SourceContext}] {Message:lj}{NewLine}{Exception}"

  let parseLogLevel args =
    let debugMarker = "--debug"
    let logMarker = "--log="
    let logLevel =
      if args |> Array.contains "--debug" then
        Logging.LogLevel.Verbose
      else
        match args |> Array.tryFind (fun arg -> arg.StartsWith logMarker) |> Option.map (fun log -> log.Substring(logMarker.Length)) with
        | Some ("warn" | "warning") ->
          Logging.LogLevel.Warn
        | Some "error" ->
          Logging.LogLevel.Error
        | Some "fatal" ->
          Logging.LogLevel.Fatal
        | Some "info" ->
          Logging.LogLevel.Info
        | Some "verbose" ->
          Logging.LogLevel.Verbose
        | Some "debug" ->
          Logging.LogLevel.Debug
        | _ -> Logging.LogLevel.Info
    let args =
      args
      |> Array.except [debugMarker]
      |> Array.filter (fun arg -> not <| arg.StartsWith logMarker)
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
      |> Array.filter (fun arg -> arg.StartsWith excludeMarker)
      |> Array.collect (fun arg -> arg.Substring(excludeMarker.Length).Split(','))
    let args =
      args
      |> Array.filter (fun arg -> not <| arg.StartsWith excludeMarker)
    toExclude, args

  let logLevel, args = parseLogLevel args
  let switch = LoggingLevelSwitch(expectoToSerilogLevel logLevel)
  let logSourcesToExclude, args = parseLogExcludes args
  let sourcesToExclude =
    Matching.WithProperty<string>(
      Constants.SourceContextPropertyName,
      fun s ->
        s <> null
        &&
        logSourcesToExclude |> Array.contains s
    )

  let argsToRemove, loaders =
    args
    |> Array.windowed 2
    |> Array.tryPick (function [| "--loader"; "ionide" |] as args -> Some(args, ["Ionide WorkspaceLoader", WorkspaceLoader.Create])
                               | [| "--loader"; "graph" |] as args -> Some(args, ["MSBuild Project Graph WorkspaceLoader", WorkspaceLoaderViaProjectGraph.Create])
                               | _ -> None )
    |> Option.defaultValue ([||], loaders)

  let serilogLogger =
    LoggerConfiguration()
      .Enrich.FromLogContext()
      .MinimumLevel.ControlledBy(switch)
      .Filter.ByExcluding(Matching.FromSource("FileSystem"))
      .Filter.ByExcluding(sourcesToExclude)

      .Destructure.FSharpTypes()
      .Destructure.ByTransforming<FSharp.Compiler.Text.Range>(fun r -> box {| FileName = r.FileName; Start = r.Start; End = r.End |})
      .Destructure.ByTransforming<FSharp.Compiler.Text.Position>(fun r -> box {| Line = r.Line; Column = r.Column |})
      .Destructure.ByTransforming<Newtonsoft.Json.Linq.JToken>(fun tok -> tok.ToString() |> box)
      .Destructure.ByTransforming<System.IO.DirectoryInfo>(fun di -> box di.FullName)
      .WriteTo.Async(
        fun c -> c.Console(outputTemplate = outputTemplate, standardErrorFromLevel = Nullable<_>(LogEventLevel.Verbose), theme = Serilog.Sinks.SystemConsole.Themes.AnsiConsoleTheme.Code) |> ignore
      ).CreateLogger() // make it so that every console log is logged to stderr
  Serilog.Log.Logger <- serilogLogger
  LogProvider.setLoggerProvider (Providers.SerilogProvider.create())

  let fixedUpArgs =
    args
    |> Array.except argsToRemove

  let cts = new CancellationTokenSource(testTimeout)
  let config =
    { defaultConfig
      with runInParallel = false
           failOnFocusedTests = true
           printer = Expecto.Impl.TestPrinters.summaryPrinter defaultConfig.printer
           verbosity = logLevel }
  runTestsWithArgsAndCancel cts.Token config fixedUpArgs tests
