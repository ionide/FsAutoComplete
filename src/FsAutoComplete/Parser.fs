namespace FsAutoComplete

open System
open System.IO
open Serilog
open Serilog.Core
open Serilog.Events
open System.CommandLine
open System.CommandLine.Parsing
open System.CommandLine.Builder
open Serilog.Filters
open System.Threading.Tasks
open FsAutoComplete.Lsp
open OpenTelemetry
open OpenTelemetry.Resources
open OpenTelemetry.Trace
open OpenTelemetry.Metrics
open FsAutoComplete.Lsp.BuildServerWorkspaceLoaderFactory

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

  let setArity arity (o: #Option) =
    o.Arity <- arity
    o

  /// set option to expect no arguments (e.g a flag-style argument: `--verbose`)
  let inline zero x = setArity ArgumentArity.Zero x
  /// set option to expect one argument (e.g a single value: `--foo bar)
  let inline one x = setArity ArgumentArity.ExactlyOne x

  /// set option to expect multiple arguments
  /// (e.g a list of values: `--foo bar baz` or `--foo bar --foo baz` depending on the style)
  let inline many x = setArity ArgumentArity.OneOrMore x

  let verboseOption =
    Option<bool>([| "--verbose"; "-v"; "--debug" |], "Enable verbose logging. This is equivalent to --log-level debug.")
    |> setArity ArgumentArity.Zero

  let logLevelOption =
    Option<LogEventLevel>("--log-level", "Set the log verbosity to a specific level.")

  let attachOption =
    Option<bool>("--attach-debugger", "Launch the system debugger and break immediately")
    |> zero

  let logFileOption =
    Option<string>([| "--logfile"; "-l"; "--log-file" |], "Send log output to specified file.")
    |> one

  let logFilterOption =
    Option<string[]>(
      [| "--filter"; "--log-filter" |],
      "Filter logs by category. The category can be seen in the logs inside []. For example: [Compiler]."
    )
    |> many
    |> fun o ->
        o.AllowMultipleArgumentsPerToken <- true
        o

  let waitForDebuggerOption =
    Option<bool>(
      [| "--wait-for-debugger"; "--attachdebugger" |],
      "Stop execution on startup until an external debugger to attach to this process"
    )
    |> zero

  let projectGraphOption =
    Option<bool>(
      "--project-graph-enabled",
      "Enable MSBuild Graph workspace loading. Should be faster than the default, but is experimental."
    )
    |> zero

  let adaptiveLspServerOption =
    Option<bool>(
      "--adaptive-lsp-server-enabled",
      "Enable LSP Server based on FSharp.Data.Adaptive. Should be more stable, but is experimental."
    )

  let otelTracingOption =
    Option<bool>(
      "--otel-exporter-enabled",
      "Enabled OpenTelemetry exporter. See https://opentelemetry.io/docs/reference/specification/protocol/exporter/ for environment variables to configure for the exporter."
    )

  let useTransparentCompilerOption =
    Option<bool>(
      "--use-fcs-transparent-compiler",
      "Use Transparent Compiler in FSharp.Compiler.Services. Should have better performance characteristics, but is experimental. See https://github.com/dotnet/fsharp/pull/15179 for more details."
    )

  let useBuildServerOption =
    Option<bool>(
      "--use-build-server",
      "Enable the separate build server process for MSBuild evaluation (experimental)."
    )

  let stateLocationOption =
    Option<DirectoryInfo>(
      "--state-directory",
      getDefaultValue = Func<_>(fun () -> DirectoryInfo System.Environment.CurrentDirectory),
      description =
        "Set the directory to store the state of the server. This should be a per-workspace location, not a shared-workspace location."
    )

  let rootCommand =
    let rootCommand = RootCommand("An F# LSP server implementation")

    rootCommand.AddOption verboseOption
    rootCommand.AddOption attachOption
    rootCommand.AddOption logFileOption
    rootCommand.AddOption logFilterOption
    rootCommand.AddOption waitForDebuggerOption
    rootCommand.AddOption projectGraphOption
    rootCommand.AddOption adaptiveLspServerOption
    rootCommand.AddOption logLevelOption
    rootCommand.AddOption stateLocationOption
    rootCommand.AddOption otelTracingOption
    rootCommand.AddOption useTransparentCompilerOption
    rootCommand.AddOption useBuildServerOption

    // for back-compat - we removed some options and this broke some clients.
    rootCommand.TreatUnmatchedTokensAsErrors <- false

    rootCommand.SetHandler(
      Func<_, _, _, _, _, Task>(fun projectGraphEnabled stateDirectory adaptiveLspEnabled useTransparentCompiler useBuildServer ->
        let workspaceLoaderFactory =
          fun toolsPath ->
            if useBuildServer then
              BuildServerWorkspaceLoaderFactory.create toolsPath
            elif projectGraphEnabled then
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
              AdaptiveFSharpLspServer.startCore
                toolsPath
                workspaceLoaderFactory
                sourceTextFactory
                useTransparentCompiler
          else
            fun () ->
              AdaptiveFSharpLspServer.startCore
                toolsPath
                workspaceLoaderFactory
                sourceTextFactory
                useTransparentCompiler

        let result = AdaptiveFSharpLspServer.start lspFactory

        Task.FromResult result),
      projectGraphOption,
      stateLocationOption,
      adaptiveLspServerOption,
      useTransparentCompilerOption,
      useBuildServerOption
    )

    rootCommand

  let waitForDebugger =
    Invocation.InvocationMiddleware(fun ctx next ->
      let waitForDebugger = ctx.ParseResult.HasOption waitForDebuggerOption

      if waitForDebugger then
        Debug.waitForDebugger ()

      next.Invoke(ctx))

  let immediateAttach =
    Invocation.InvocationMiddleware(fun ctx next ->
      let attachDebugger = ctx.ParseResult.HasOption attachOption

      if attachDebugger then
        Diagnostics.Debugger.Launch() |> ignore<bool>

      next.Invoke(ctx))

  let warnOnUnknownOptions =
    Invocation.InvocationMiddleware(fun ctx next ->
      if
        isNull ctx.ParseResult.UnmatchedTokens
        || ctx.ParseResult.UnmatchedTokens.Count = 0
      then
        next.Invoke(ctx)
      else
        ctx.Console.Error.Write(
          $"""The following options were not recognized - please consider removing them: {String.Join(", ", ctx.ParseResult.UnmatchedTokens)}"""
        )

        next.Invoke(ctx))

  let configureOTel =
    Invocation.InvocationMiddleware(fun ctx next ->



      if ctx.ParseResult.GetValueForOption otelTracingOption then

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

      next.Invoke(ctx))

  let configureLogging =
    Invocation.InvocationMiddleware(fun ctx next ->
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

      let args = ctx.ParseResult

      let logLevel =
        if args.HasOption verboseOption then
          LogEventLevel.Debug
        else if args.HasOption logLevelOption then
          args.GetValueForOption logLevelOption
        else
          LogEventLevel.Warning

      let logSourcesToExclude =
        if args.HasOption logFilterOption then
          args.GetValueForOption logFilterOption
        else
          [||]

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

      if args.HasOption logFileOption then
        let logFile = args.GetValueForOption logFileOption

        try
          logConf.WriteTo.Async(fun c -> c.File(path = logFile, levelSwitch = verbositySwitch) |> ignore)
          |> ignore
        with e ->
          eprintfn "Bad log file: %s" e.Message
          exit 1

      if args.HasOption logFilterOption then
        let categories = args.GetValueForOption logFilterOption

        categories
        |> Array.iter (fun category ->
          // category is encoded in the SourceContext property, so we filter messages based on that property's value
          logConf.Filter.ByExcluding(Func<_, _>(isCategory category)) |> ignore)

      let logger = logConf.CreateLogger()
      Serilog.Log.Logger <- logger
      Logging.LogProvider.setLoggerProvider (Logging.Providers.SerilogProvider.create ())
      next.Invoke(ctx))

  let serilogFlush =
    Invocation.InvocationMiddleware(fun ctx next ->
      task {
        do! next.Invoke ctx
        Serilog.Log.CloseAndFlush()
      })

  let parser =
    CommandLineBuilder(rootCommand)
      .UseDefaults()
      .AddMiddleware(waitForDebugger)
      .AddMiddleware(immediateAttach)
      .AddMiddleware(serilogFlush)
      .AddMiddleware(configureLogging)
      .AddMiddleware(configureOTel)
      .AddMiddleware(warnOnUnknownOptions)
      .Build()
