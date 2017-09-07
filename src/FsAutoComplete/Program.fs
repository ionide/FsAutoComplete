module FsAutoComplete.Program

open System
open System.IO
open Microsoft.FSharp.Compiler
open FsAutoComplete.JsonSerializer
open Argu

[<EntryPoint>]
let entry args =
    System.Threading.ThreadPool.SetMinThreads(8, 8) |> ignore

    let commands = Commands(writeJson)
    let originalFs = AbstractIL.Internal.Library.Shim.FileSystem
    let fs = FileSystem(originalFs, commands.Files.TryFind)
    AbstractIL.Internal.Library.Shim.FileSystem <- fs

    let parser = ArgumentParser.Create<Options.CLIArguments>(programName = "fsautocomplete.exe")
    try
      let results = parser.Parse args

      Options.apply results

      Debug.checkIfWaitForDebugger()

      match results.GetResult(<@ Options.CLIArguments.Mode @>, defaultValue = Options.TransportMode.Stdio) with
      | Options.TransportMode.Stdio ->
          FsAutoComplete.Stdio.start commands
      | Options.TransportMode.Http ->
          FsAutoComplete.Suave.start commands results
    with
    | :? ArguParseException as ex ->
      printfn "%s" ex.Message
      match ex.ErrorCode with
      | ErrorCode.HelpText -> 0
      | _ -> 1  // Unrecognised arguments
    | e ->
      printfn "Server crashing error - %s \n %s" e.Message e.StackTrace
      3
