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
    Options.rootCommand.SetHandler(Func<_,_,Task>(fun backgroundServiceEnabled projectGraphEnabled ->
      let workspaceLoaderFactory =
        if projectGraphEnabled then Ionide.ProjInfo.WorkspaceLoaderViaProjectGraph.Create
        else Ionide.ProjInfo.WorkspaceLoader.Create

      let toolsPath = Ionide.ProjInfo.Init.init (System.IO.DirectoryInfo Environment.CurrentDirectory) None
      use _compilerEventListener = new Debug.FSharpCompilerEventLogger.Listener()
      let result = FsAutoComplete.Lsp.start backgroundServiceEnabled toolsPath workspaceLoaderFactory

      Task.FromResult result :> Task
    ), Options.backgroundServiceOption, Options.projectGraphOption)

    let results = Options.parser.Invoke args
    results
