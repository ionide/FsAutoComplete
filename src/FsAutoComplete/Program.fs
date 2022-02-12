module FsAutoComplete.Program

open System
open Serilog
open Serilog.Core
open Serilog.Events
open FsAutoComplete.Logging
open System.CommandLine
open System.CommandLine.Parsing
open System.Threading.Tasks

[<EntryPoint>]
let entry args =
    System.Threading.ThreadPool.SetMinThreads(16, 16) |> ignore
    Options.rootCommand.SetHandler(Func<_,Task>(fun (args: ParseResult) ->
      // default the verbosity to warning
      let verbositySwitch = LoggingLevelSwitch(LogEventLevel.Warning)
      let outputTemplate = "[{Timestamp:HH:mm:ss.fff} {Level:u3}] [{SourceContext}] {Message:lj}{NewLine}{Exception}"
      let logConf =
        LoggerConfiguration()
          .MinimumLevel.ControlledBy(verbositySwitch)
          .Enrich.FromLogContext()
          .Destructure.FSharpTypes()
          .Destructure.ByTransforming<FSharp.Compiler.Text.Range>(fun r -> box {| FileName = r.FileName; Start = r.Start; End = r.End |})
          .Destructure.ByTransforming<FSharp.Compiler.Text.Position>(fun r -> box {| Line = r.Line; Column = r.Column |})
          .Destructure.ByTransforming<Newtonsoft.Json.Linq.JToken>(fun tok -> tok.ToString() |> box)
          .Destructure.ByTransforming<System.IO.DirectoryInfo>(fun di -> box di.FullName)
          .WriteTo.Async(
            fun c -> c.Console(outputTemplate = outputTemplate, standardErrorFromLevel = Nullable<_>(LogEventLevel.Verbose), theme = Serilog.Sinks.SystemConsole.Themes.AnsiConsoleTheme.Code) |> ignore
          ) // make it so that every console log is logged to stderr

      Options.apply verbositySwitch logConf args

      let logger = logConf.CreateLogger()
      Serilog.Log.Logger <- logger
      LogProvider.setLoggerProvider (Providers.SerilogProvider.create())

      let backgroundServiceEnabled = args.HasOption Options.backgroundServiceOption
      let projectGraphEnabled = args.HasOption Options.projectGraphOption

      let workspaceLoaderFactory =
        if projectGraphEnabled then Ionide.ProjInfo.WorkspaceLoaderViaProjectGraph.Create
        else Ionide.ProjInfo.WorkspaceLoader.Create

      let toolsPath = Ionide.ProjInfo.Init.init (System.IO.DirectoryInfo Environment.CurrentDirectory) None
      use _compilerEventListener = new Debug.FSharpCompilerEventLogger.Listener()
      let result = FsAutoComplete.Lsp.start backgroundServiceEnabled toolsPath workspaceLoaderFactory

      Task.FromResult result :> Task
    ))

    let results = Options.parser.Invoke args
    results
