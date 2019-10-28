module FsAutoComplete.Program

open System
open System.IO
open FSharp.Compiler
open FsAutoComplete.JsonSerializer
open Argu

[<EntryPoint>]
let entry args =

    try
      System.Threading.ThreadPool.SetMinThreads(16, 16) |> ignore


      let parser = ArgumentParser.Create<Options.CLIArguments>(programName = "fsautocomplete")

      let results = parser.Parse args

      results.TryGetResult(<@ Options.CLIArguments.WaitForDebugger @>)
      |> Option.iter (ignore >> Debug.waitForDebugger)

      results.TryGetResult(<@ Options.CLIArguments.Version @>)
      |> Option.iter (fun _ ->
          let version = Version.info ()
          printfn "FsAutoComplete %s (git sha %s)" (version.Version) (version.GitSha)
          exit 0 )

      Options.apply results

      let backgroundServiceEnabled =
        results.Contains <@ Options.CLIArguments.BackgroundServiceEnabled @>

      let commands = Commands(writeJson, backgroundServiceEnabled)
      let originalFs = AbstractIL.Internal.Library.Shim.FileSystem
      let fs = FileSystem(originalFs, commands.Files.TryFind)
      AbstractIL.Internal.Library.Shim.FileSystem <- fs

      use compilerEventListener = new Debug.FSharpCompilerEventLogger.Listener()

      FsAutoComplete.Lsp.start commands
    with
    | :? ArguParseException as ex ->
      printfn "%s" ex.Message
      match ex.ErrorCode with
      | ErrorCode.HelpText -> 0
      | _ -> 1  // Unrecognised arguments
    | e ->
      printfn "Server crashing error - %s \n %s" e.Message e.StackTrace
      3
