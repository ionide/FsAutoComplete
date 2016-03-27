namespace FsAutoComplete

open System
open System.IO

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices

open Newtonsoft.Json
open Newtonsoft.Json.Converters
open JsonSerializer
open FsAutoComplete

module internal Main =
  module Response = CommandResponse
  let checker = new FSharpCompilerServiceChecker()

  let mutable currentFiles = Map.empty
  let originalFs = AbstractIL.Internal.Library.Shim.FileSystem
  let fs = new FileSystem(originalFs, fun () -> currentFiles)
  AbstractIL.Internal.Library.Shim.FileSystem <- fs

  let commandQueue = new FSharpx.Control.BlockingQueueAgent<Command>(10)

  let main (state:State) : int =
    let state = ref state
    let mutable quit = false

    while not quit do
      currentFiles <- (!state).Files
      try
        let res =
            match commandQueue.Get() with
            | Parse(file,kind,lines) -> async {
                let! (res,state) = Commands.parse writeJson !state checker file lines
                //Hack for tests
                let r = match kind with
                        | Synchronous -> Response.info writeJson "Synchronous parsing started" 
                        | Normal -> Response.info writeJson "Background parsing started"
                return [r] @res,state
                }

            | Project (file,time,verbose) ->
                //THIS SHOULD BE INITIALIZED SOMEWHERE ELSE ?
                let fp = Path.GetFullPath file
                let fsw = new FileSystemWatcher()
                fsw.Path <- Path.GetDirectoryName fp
                fsw.Filter <- Path.GetFileName fp
                fsw.Changed.Add(fun _ -> commandQueue.Add(Project (fp, DateTime.Now, verbose)))
                fsw.EnableRaisingEvents <- true
                Commands.project writeJson !state checker file time verbose

            | Declarations file ->
                Commands.declarations writeJson !state checker file

            | HelpText sym ->
                Commands.helptext writeJson !state checker sym
            | PosCommand(cmd, file, lineStr, line, col, _timeout, filter) ->
                let file = Path.GetFullPath file
                match (!state).TryGetFileCheckerOptionsWithLines (file) with
                | Failure s -> async { return [Response.error writeJson (s)], !state }
                | Success (options) ->
                  let projectOptions, lines = options
                  let ok = line <= lines.Length && line >= 1 &&
                           col <= lineStr.Length + 1 && col >= 1
                  if not ok then
                      async { return [Response.error writeJson "Position is out of range"], !state}
                  else
                    // TODO: Should sometimes pass options.Source in here to force a reparse
                    //       for completions e.g. `(some typed expr).$`
                    let tyResOpt = checker.TryGetRecentTypeCheckResultsForFile(file, projectOptions)
                    match tyResOpt with
                    | None -> async { return [ Response.info writeJson "Cached typecheck results not yet available"], !state }
                    | Some tyRes ->
                    match cmd with
                    | Completion ->
                        Commands.completion writeJson !state checker tyRes line col lineStr filter
                    | ToolTip ->
                        Commands.toolTip writeJson !state checker tyRes line col lineStr
                    | TypeSig ->
                        Commands.typesig writeJson !state checker tyRes line col lineStr
                    | SymbolUse ->
                        Commands.symbolUse writeJson !state checker tyRes line col lineStr
                    | FindDeclaration ->
                        Commands.findDeclarations writeJson !state checker tyRes line col lineStr
                    | Methods ->
                        Commands.methods writeJson !state checker tyRes line col lines
            | CompilerLocation ->
                Commands.compilerLocation writeJson !state checker

            | Colorization enabled ->
                Commands.colorization writeJson !state checker enabled

            | Lint filename ->
                Commands.lint writeJson !state checker filename

            | Error(msg) ->
                Commands.error writeJson !state checker msg

            | Quit ->
                quit <- true
                async {return [], !state}
        let res', state' = res |> Async.RunSynchronously
        state := state'
        res' |> List.iter Console.WriteLine
        ()

      with e ->
        let msg = "Unexpected internal error. Please report at \
                   https://github.com/fsharp/FsAutoComplete/issues, \
                   attaching the exception information:\n"
                   + e.ToString()
        Response.error writeJson msg |> Console.WriteLine

    0

  [<EntryPoint>]
  let entry args =
    System.Threading.ThreadPool.SetMinThreads(8, 8) |> ignore
    Console.InputEncoding <- Text.Encoding.UTF8
    Console.OutputEncoding <- new Text.UTF8Encoding(false, false)
    let extra = Options.p.Parse args
    if extra.Count <> 0 then
      printfn "Unrecognised arguments: %s" (String.concat "," extra)
      1
    else
      try
        async {
          while true do
            let cmd = CommandInput.parseCommand(Console.ReadLine())
            commandQueue.Add(cmd)
        }
        |> Async.Start

        main State.Initial
      finally
        (!Debug.output).Close ()
