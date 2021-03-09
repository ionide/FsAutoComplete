module FsAutoComplete.Program

open System
open FSharp.Compiler.SourceCodeServices
open FsAutoComplete.JsonSerializer
open Argu
open Serilog
open Serilog.Core
open Serilog.Events
open FsAutoComplete.Logging

[<EntryPoint>]
let entry args =


    try
      let parser = ArgumentParser.Create<Options.CLIArguments>(programName = "fsautocomplete")

      let results = parser.Parse args

      System.Threading.ThreadPool.SetMinThreads(16, 16) |> ignore

      // default the verbosity to warning
      let verbositySwitch = LoggingLevelSwitch(LogEventLevel.Warning)
      let outputTemplate = "[{Timestamp:HH:mm:ss.fff} {Level:u3}] [{SourceContext}] {Message:lj}{NewLine}{Exception}"
      let logConf =
        LoggerConfiguration()
          .MinimumLevel.ControlledBy(verbositySwitch)
          .Enrich.FromLogContext()
          .Destructure.FSharpTypes()
          .Destructure.ByTransforming<FSharp.Compiler.Text.Range>(fun r -> box {| FileName = r.FileName; Start = r.Start; End = r.End |})
          .Destructure.ByTransforming<FSharp.Compiler.Text.Pos>(fun r -> box {| Line = r.Line; Column = r.Column |})
          .Destructure.ByTransforming<Newtonsoft.Json.Linq.JToken>(fun tok -> tok.ToString() |> box)
          .Destructure.ByTransforming<System.IO.DirectoryInfo>(fun di -> box di.FullName)
          .WriteTo.Async(
            fun c -> c.Console(outputTemplate = outputTemplate, standardErrorFromLevel = Nullable<_>(LogEventLevel.Verbose), theme = Serilog.Sinks.SystemConsole.Themes.AnsiConsoleTheme.Code) |> ignore
          ) // make it so that every console log is logged to stderr


      results.TryGetResult(<@ Options.CLIArguments.WaitForDebugger @>)
      |> Option.iter (ignore >> Debug.waitForDebugger)

      results.TryGetResult(<@ Options.CLIArguments.Version @>)
      |> Option.iter (fun _ ->
          let version = Version.info ()
          printfn "FsAutoComplete %s (git sha %s)" (version.Version) (version.GitSha)
          exit 0 )

      Options.apply verbositySwitch logConf results

      let logger = logConf.CreateLogger()
      Serilog.Log.Logger <- logger
      LogProvider.setLoggerProvider (Providers.SerilogProvider.create())

      let backgroundServiceEnabled =
        results.Contains <@ Options.CLIArguments.BackgroundServiceEnabled @>

      let projectGraphEnabled =
        results.Contains <@ Options.CLIArguments.ProjectGraphEnabled @>

      let workspaceLoaderFactory =
        if projectGraphEnabled then Ionide.ProjInfo.WorkspaceLoaderViaProjectGraph.Create
        else Ionide.ProjInfo.WorkspaceLoader.Create

      let toolsPath = Ionide.ProjInfo.Init.init ()
      let commands = Commands(writeJson, backgroundServiceEnabled, toolsPath, workspaceLoaderFactory)
      let originalFs = FileSystemAutoOpens.FileSystem
      let fs = FsAutoComplete.FileSystem(originalFs, commands.Files.TryFind)
      FileSystemAutoOpens.FileSystem <- fs

      use compilerEventListener = new Debug.FSharpCompilerEventLogger.Listener()
      let result = FsAutoComplete.Lsp.start commands
      Serilog.Log.CloseAndFlush()
      result
    with
    | :? ArguParseException as ex ->
      printfn "%s" ex.Message
      match ex.ErrorCode with
      | ErrorCode.HelpText -> 0
      | _ -> 1  // Unrecognised arguments
    | e ->
      printfn "Server crashing error - %s \n %s" e.Message e.StackTrace
      3
