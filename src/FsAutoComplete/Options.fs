// --------------------------------------------------------------------------------------
// (c) Robin Neatherway
// --------------------------------------------------------------------------------------
namespace FsAutoComplete

open System
open Serilog
open Serilog.Core
open Serilog.Events

module Options =
  open System.CommandLine
  open System.CommandLine.Parsing
  open System.CommandLine.Builder
  open System.Threading.Tasks

  let private setArity arity (o: #Option) = o.Arity <- arity; o
  let inline private zero x = setArity ArgumentArity.Zero x
  let inline private one x = setArity ArgumentArity.ExactlyOne x
  let inline private many x = setArity ArgumentArity.OneOrMore x

  let verboseOption =
    Option<bool>([|"--verbose"; "-v"|], "enable verbose logging")
    |> setArity ArgumentArity.Zero

  let attachOption =
    Option<bool>("--attach-debugger", "launch the system debugger and break")
    |> zero

  let logFileOption =
    Option<string>([|"--logfile"; "-l"|], "send verbose output to specified log file")
    |> one

  let logFilterOption =
    Option<string[]>("--filter", "filter log messages by category")
    |> many

  let waitForDebuggerOption =
    Option<bool>("--wait-for-debugger", "wait for a debugger to attach to the process")
    |> zero

  let hostPIDOption =
    Option<int>("--hostPID", "the Host process ID")
    |> one

  let backgroundServiceOption =
    Option<bool>("--background-service-enabled", "enable background service")
    |> zero

  let projectGraphOption =
    Option<bool>("--project-graph-enabled", "enable MSBuild ProjectGraph for workspace loading. Experimental.")
    |> zero

  let rootCommand =
    let rootCommand = RootCommand("An F# LSP server implementation")

    rootCommand.AddOption verboseOption
    rootCommand.AddOption attachOption
    rootCommand.AddOption logFileOption
    rootCommand.AddOption logFilterOption
    rootCommand.AddOption waitForDebuggerOption
    rootCommand.AddOption hostPIDOption
    rootCommand.AddOption backgroundServiceOption
    rootCommand.AddOption projectGraphOption
    rootCommand

  let waitForDebugger = Invocation.InvocationMiddleware (fun ctx next ->
    let waitForDebugger = ctx.ParseResult.HasOption waitForDebuggerOption
    if waitForDebugger then Debug.waitForDebugger()
    next.Invoke(ctx)
  )

  let immediateAttach = Invocation.InvocationMiddleware (fun ctx next ->
    let attachDebugger = ctx.ParseResult.HasOption attachOption
    if attachDebugger then System.Diagnostics.Debugger.Launch() |> ignore<bool>
    next.Invoke(ctx)
  )

  let serilogFlush = Invocation.InvocationMiddleware (fun ctx next -> task {
    do! next.Invoke ctx
    Serilog.Log.CloseAndFlush()
  })

  let parser =
    CommandLineBuilder(rootCommand).UseDefaults().AddMiddleware(waitForDebugger).AddMiddleware(immediateAttach).AddMiddleware(serilogFlush).Build()

  let isCategory (category: string) (e: LogEvent) =
    match e.Properties.TryGetValue "SourceContext" with
    | true, loggerName ->
      match loggerName with
      | :? ScalarValue as v ->
        match v.Value with
        | :? string as s when s = category -> true
        | _ -> false
      | _ -> false
    | false,  _ -> false

  let hasMinLevel (minLevel: LogEventLevel) (e: LogEvent) =
    e.Level >= minLevel

  // will use later when a mapping-style config of { "category": "minLevel" } is established
  let excludeByLevelWhenCategory category level event = isCategory category event || not (hasMinLevel level event)

  let apply (levelSwitch: LoggingLevelSwitch) (logConfig: Serilog.LoggerConfiguration) (args: ParseResult) =
    if args.HasOption verboseOption then levelSwitch.MinimumLevel <- LogEventLevel.Verbose
    if args.HasOption logFileOption then
      let logFile = args.GetValueForOption logFileOption

      try
        logConfig.WriteTo.Async(fun c -> c.File(path = logFile, levelSwitch = levelSwitch) |> ignore) |> ignore
      with
      | e ->
        eprintfn "Bad log file: %s" e.Message
        exit 1

    if args.HasOption logFilterOption then
      let categories = args.GetValueForOption logFilterOption
      categories
      |> Array.iter (fun category ->
        // category is encoded in the SourceContext property, so we filter messages based on that property's value
        logConfig.Filter.ByExcluding(Func<_,_>(isCategory category)) |> ignore
      )

