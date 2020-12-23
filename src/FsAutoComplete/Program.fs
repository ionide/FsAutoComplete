module FsAutoComplete.Program

open System
open System.IO
open FSharp.Compiler
open FsAutoComplete.JsonSerializer
open Argu
open Serilog
open Serilog.Core
open Serilog.Events
open FsAutoComplete.Logging

[<EntryPoint>]
let entry args =


  try
    let parser =
      ArgumentParser.Create<Options.CLIArguments>(programName = "fsautocomplete")

    let results = parser.Parse args

    System.Threading.ThreadPool.SetMinThreads(16, 16)
    |> ignore

    // default the verbosity to warning
    let verbositySwitch =
      LoggingLevelSwitch(LogEventLevel.Warning)

    let outputTemplate =
      "[{Timestamp:HH:mm:ss.fff} {Level:u3}] [{SourceContext}] {Message:lj}{NewLine}{Exception}"

    let logConf =
      LoggerConfiguration()
        .MinimumLevel.ControlledBy(verbositySwitch)
        .Enrich.FromLogContext()
        .Destructure.FSharpTypes()
        .WriteTo.Async(fun c ->
          c.Console(
            outputTemplate = outputTemplate,
            standardErrorFromLevel = Nullable<_>(LogEventLevel.Verbose),
            theme = Serilog.Sinks.SystemConsole.Themes.AnsiConsoleTheme.Code
          )
          |> ignore) // make it so that every console log is logged to stderr


    results.TryGetResult(<@ Options.CLIArguments.WaitForDebugger @>)
    |> Option.iter (ignore >> Debug.waitForDebugger)

    results.TryGetResult(<@ Options.CLIArguments.Version @>)
    |> Option.iter
         (fun _ ->
           let version = Version.info ()
           printfn "FsAutoComplete %s (git sha %s)" (version.Version) (version.GitSha)
           exit 0)

    Options.apply verbositySwitch logConf results

    let logger = logConf.CreateLogger()
    Serilog.Log.Logger <- logger
    LogProvider.setLoggerProvider (Providers.SerilogProvider.create ())

    let backgroundServiceEnabled =
      results.Contains <@ Options.CLIArguments.BackgroundServiceEnabled @>

    let toolsPath = Ionide.ProjInfo.Init.init ()

    let commands =
      Commands(writeJson, backgroundServiceEnabled, toolsPath)

    let originalFs =
      AbstractIL.Internal.Library.Shim.FileSystem

    let fs =
      FileSystem(originalFs, commands.Files.TryFind)

    AbstractIL.Internal.Library.Shim.FileSystem <- fs

    use compilerEventListener =
      new Debug.FSharpCompilerEventLogger.Listener()

    let result = FsAutoComplete.Lsp.start commands
    Serilog.Log.CloseAndFlush()
    result
  with
  | :? ArguParseException as ex ->
      printfn "%s" ex.Message

      match ex.ErrorCode with
      | ErrorCode.HelpText -> 0
      | _ -> 1 // Unrecognised arguments
  | e ->
      printfn "Server crashing error - %s \n %s" e.Message e.StackTrace
      3
