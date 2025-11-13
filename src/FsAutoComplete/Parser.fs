namespace FsAutoComplete

open System
open System.IO
open Serilog
open Serilog.Core
open Serilog.Events
open System.CommandLine
open System.CommandLine.Parsing
open Serilog.Filters
open System.Threading.Tasks
open FsAutoComplete.Lsp
open OpenTelemetry
open OpenTelemetry.Resources
open OpenTelemetry.Trace
open OpenTelemetry.Metrics

module Parser =
  open FsAutoComplete.Core
  open System.Diagnostics

  let mutable tracerProvider = Unchecked.defaultof<_>
  let mutable meterProvider = Unchecked.defaultof<_>

  [<Struct>]
  type Pos = { Line: int; Column: int }

  [<Struct>]
  type Rng =
    { File: string
      Start: FSharp.Compiler.Text.pos
      End: FSharp.Compiler.Text.pos }

  let verboseOption =
    let o = Option<bool>("--verbose", "-v", "--debug")
    o.Description <- "Enable verbose logging. This is equivalent to --log-level debug."
    o

  let logLevelOption =
    let o = Option<LogEventLevel>("--log-level")
    o.Description <- "Set the log verbosity to a specific level."
    o

  let attachOption =
    let o = Option<bool>("--attach-debugger")
    o.Description <- "Launch the system debugger and break immediately"
    o

  let logFileOption =
    let o = Option<string>("--logfile", "-l", "--log-file")
    o.Description <- "Send log output to specified file."
    o

  let logFilterOption =
    let o = Option<string[]>("--filter", "--log-filter")
    o.Description <- "Filter logs by category. The category can be seen in the logs inside []. For example: [Compiler]."
    o.AllowMultipleArgumentsPerToken <- true
    o

  let waitForDebuggerOption =
    let o = Option<bool>("--wait-for-debugger", "--attachdebugger")
    o.Description <- "Stop execution on startup until an external debugger to attach to this process"
    o

  let projectGraphOption =
    let o = Option<bool>("--project-graph-enabled")
    o.Description <- "Enable MSBuild Graph workspace loading. Should be faster than the default, but is experimental."
    o

  let adaptiveLspServerOption =
    let o = Option<bool>("--adaptive-lsp-server-enabled")
    o.Description <- "Enable LSP Server based on FSharp.Data.Adaptive. Should be more stable, but is experimental."
    o

  let otelTracingOption =
    let o = Option<bool>("--otel-exporter-enabled")

    o.Description <-
      "Enabled OpenTelemetry exporter. See https://opentelemetry.io/docs/reference/specification/protocol/exporter/ for environment variables to configure for the exporter."

    o

  let useTransparentCompilerOption =
    let o = Option<bool>("--use-fcs-transparent-compiler")

    o.Description <-
      "Use Transparent Compiler in FSharp.Compiler.Services. Should have better performance characteristics, but is experimental. See https://github.com/dotnet/fsharp/pull/15179 for more details."

    o

  let stateLocationOption =
    let o = Option<DirectoryInfo>("--state-directory")

    o.Description <-
      "Set the directory to store the state of the server. This should be a per-workspace location, not a shared-workspace location."

    o.DefaultValueFactory <- fun _ -> DirectoryInfo System.Environment.CurrentDirectory
    o

  let rootCommand =
    let rootCommand = RootCommand("An F# LSP server implementation")

    rootCommand.Options.Add verboseOption
    rootCommand.Options.Add attachOption
    rootCommand.Options.Add logFileOption
    rootCommand.Options.Add logFilterOption
    rootCommand.Options.Add waitForDebuggerOption
    rootCommand.Options.Add projectGraphOption
    rootCommand.Options.Add adaptiveLspServerOption
    rootCommand.Options.Add logLevelOption
    rootCommand.Options.Add stateLocationOption
    rootCommand.Options.Add otelTracingOption
    rootCommand.Options.Add useTransparentCompilerOption

    // for back-compat - we removed some options and this broke some clients.
    rootCommand.TreatUnmatchedTokensAsErrors <- false

    rootCommand.SetAction(fun (parseResult: ParseResult) ->
      let projectGraphEnabled = parseResult.GetValue(projectGraphOption)
      // let stateDirectory = parseResult.GetValue(stateLocationOption)
      let adaptiveLspEnabled = parseResult.GetValue(adaptiveLspServerOption)
      let useTransparentCompiler = parseResult.GetValue(useTransparentCompilerOption)

      let workspaceLoaderFactory =
        fun toolsPath ->
          if projectGraphEnabled then
            Ionide.ProjInfo.WorkspaceLoaderViaProjectGraph.Create(toolsPath, ProjectLoader.globalProperties)
          else
            Ionide.ProjInfo.WorkspaceLoader.Create(toolsPath, ProjectLoader.globalProperties)

      let sourceTextFactory: ISourceTextFactory = new RoslynSourceTextFactory()

      let dotnetPath =
        if
          Environment.ProcessPath.EndsWith("dotnet", StringComparison.Ordinal)
          || Environment.ProcessPath.EndsWith("dotnet.exe", StringComparison.Ordinal)
        then
          // this is valid when not running as a global tool
          Some(FileInfo(Environment.ProcessPath))
        else
          None

      let toolsPath =
        Ionide.ProjInfo.Init.init (IO.DirectoryInfo Environment.CurrentDirectory) dotnetPath

      let lspFactory =
        if adaptiveLspEnabled then
          fun () ->
            AdaptiveFSharpLspServer.startCore toolsPath workspaceLoaderFactory sourceTextFactory useTransparentCompiler
        else
          fun () ->
            AdaptiveFSharpLspServer.startCore toolsPath workspaceLoaderFactory sourceTextFactory useTransparentCompiler

      let result = AdaptiveFSharpLspServer.start lspFactory

      result)

    rootCommand

  let handleWaitForDebugger (parseResult: ParseResult) =
    if parseResult.GetValue(waitForDebuggerOption) then
      Debug.waitForDebugger ()

  let handleImmediateAttach (parseResult: ParseResult) =
    if parseResult.GetValue(attachOption) then
      Diagnostics.Debugger.Launch() |> ignore<bool>

  let warnOnUnknownOptions (parseResult: ParseResult) =
    if
      not (isNull parseResult.UnmatchedTokens)
      && parseResult.UnmatchedTokens.Count > 0
    then
      eprintfn
        $"""The following options were not recognized - please consider removing them: {String.Join(", ", parseResult.UnmatchedTokens)}"""

  let configureOTel (parseResult: ParseResult) =
    if parseResult.GetValue(otelTracingOption) then

      let serviceName = FsAutoComplete.Utils.Tracing.serviceName
      let version = FsAutoComplete.Utils.Version.info().Version

      let resourceBuilder =
        ResourceBuilder.CreateDefault().AddService(serviceName = serviceName, serviceVersion = version)


      meterProvider <-
        Sdk
          .CreateMeterProviderBuilder()
          .AddMeter()
          .AddRuntimeInstrumentation()
          .SetResourceBuilder(resourceBuilder)
          .AddOtlpExporter()
          .Build()

      tracerProvider <-
        Sdk
          .CreateTracerProviderBuilder()
          .AddSource(serviceName, Tracing.fscServiceName)
          .SetResourceBuilder(resourceBuilder)
          .AddOtlpExporter()
          .Build()

  let configureLogging (parseResult: ParseResult) =
    let isCategory (category: string) (e: LogEvent) =
      match e.Properties.TryGetValue "SourceContext" with
      | true, loggerName ->
        match loggerName with
        | :? ScalarValue as v ->
          match v.Value with
          | :? string as s when s = category -> true
          | _ -> false
        | _ -> false
      | false, _ -> false

    let hasMinLevel (minLevel: LogEventLevel) (e: LogEvent) = e.Level >= minLevel

    // will use later when a mapping-style config of { "category": "minLevel" } is established
    let _excludeByLevelWhenCategory category level event = isCategory category event || not (hasMinLevel level event)

    let logLevel =
      if parseResult.GetValue(verboseOption) then
        LogEventLevel.Debug
      else if parseResult.GetValue(logLevelOption) <> Unchecked.defaultof<_> then
        parseResult.GetValue(logLevelOption)
      else
        LogEventLevel.Warning

    let logSourcesToExclude =
      let filterValue = parseResult.GetValue(logFilterOption)
      if isNull filterValue then [||] else filterValue

    let sourcesToExclude =
      Matching.WithProperty<string>(
        Constants.SourceContextPropertyName,
        fun s -> not (isNull s) && Array.contains s logSourcesToExclude
      )

    let verbositySwitch = LoggingLevelSwitch(logLevel)

    let outputTemplate =
      "[{Timestamp:HH:mm:ss.fff} {Level:u3}] [{SourceContext}] {Message:lj}{NewLine}{Exception}"

    let logConf =
      LoggerConfiguration()
        .MinimumLevel.ControlledBy(verbositySwitch)
        .Filter.ByExcluding(Matching.FromSource("FileSystem"))
        .Filter.ByExcluding(sourcesToExclude)
        .Enrich.FromLogContext()
        .Destructure.FSharpTypes()
        .Destructure.ByTransforming<FSharp.Compiler.Text.Range>(fun r ->
          { File = r.FileName
            Start = r.Start
            End = r.End })
        .Destructure.ByTransforming<FSharp.Compiler.Text.Position>(fun r -> { Line = r.Line; Column = r.Column })
        .Destructure.ByTransforming<Newtonsoft.Json.Linq.JToken>(fun tok -> tok.ToString() |> box)
        .Destructure.ByTransforming<System.IO.DirectoryInfo>(fun di -> box di.FullName)
        .WriteTo.Async(fun c ->
          c.Console(
            outputTemplate = outputTemplate,
            standardErrorFromLevel = Nullable<_>(LogEventLevel.Verbose),
            theme = Serilog.Sinks.SystemConsole.Themes.AnsiConsoleTheme.Code
          )
          |> ignore) // make it so that every console log is logged to stderr so that we don't interfere with LSP stdio

    let logFile = parseResult.GetValue(logFileOption)

    if not (isNull logFile) then
      try
        logConf.WriteTo.Async(fun c -> c.File(path = logFile, levelSwitch = verbositySwitch) |> ignore)
        |> ignore
      with e ->
        eprintfn "Bad log file: %s" e.Message
        exit 1

    let categories = parseResult.GetValue(logFilterOption)

    if not (isNull categories) then
      categories
      |> Array.iter (fun category ->
        // category is encoded in the SourceContext property, so we filter messages based on that property's value
        logConf.Filter.ByExcluding(Func<_, _>(isCategory category)) |> ignore)

    let logger = logConf.CreateLogger()
    Serilog.Log.Logger <- logger
    Logging.LogProvider.setLoggerProvider (Logging.Providers.SerilogProvider.create ())

  let parse (args: string[]) =
    let parseResult = rootCommand.Parse(args)

    // Execute setup logic before invocation
    handleWaitForDebugger parseResult
    handleImmediateAttach parseResult
    configureLogging parseResult
    configureOTel parseResult
    warnOnUnknownOptions parseResult

    parseResult

  let invoke (args: string[]) =
    let parseResult = parse args

    try
      parseResult.Invoke()
    finally
      Serilog.Log.CloseAndFlush()
