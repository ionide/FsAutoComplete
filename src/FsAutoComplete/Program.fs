namespace FsAutoComplete

open System
open System.IO
open Microsoft.FSharp.Compiler
open JsonSerializer
open FsAutoComplete
open FsAutoComplete.Commands

module internal Main =
  module Response = CommandResponse
  let checker = new FSharpCompilerServiceChecker()

  let mutable currentFiles = Map.empty
  let originalFs = AbstractIL.Internal.Library.Shim.FileSystem
  let fs = new FileSystem(originalFs, fun () -> currentFiles)
  AbstractIL.Internal.Library.Shim.FileSystem <- fs

  let commandQueue = new FSharpx.Control.BlockingQueueAgent<Command>(10)

  let main (state : State) : int =
    let mutable state = state
    let mutable quit = false

    let context tyRes =
        { Serialize = writeJson
          State = state
          TyRes = tyRes
          Checker = checker }

    while not quit do
      currentFiles <- state.Files
      async {
          match commandQueue.Get() with
          | Parse (file, kind, lines) -> 
              let! res, state = Commands.parse writeJson state checker file lines
              //Hack for tests
              let r = match kind with
                      | Synchronous -> Response.info writeJson "Synchronous parsing started" 
                      | Normal -> Response.info writeJson "Background parsing started"
              return r :: res, state

          | Project (file, time, verbose) ->
              //THIS SHOULD BE INITIALIZED SOMEWHERE ELSE ?
              let fullPath = Path.GetFullPath file
              let fsw = new FileSystemWatcher(Path = Path.GetDirectoryName fullPath, Filter = Path.GetFileName fullPath)
              fsw.Changed.Add(fun _ -> commandQueue.Add(Project (fullPath, DateTime.Now, verbose)))
              fsw.EnableRaisingEvents <- true
              return! Commands.project writeJson state checker file time verbose

          | Declarations file -> return! Commands.declarations writeJson state checker file
          | HelpText sym -> return! Commands.helptext writeJson state sym
          | PosCommand(cmd, file, lineStr, pos, _timeout, filter) ->
              let file = Path.GetFullPath file
              match state.TryGetFileCheckerOptionsWithLines file with
              | Failure s -> return [Response.error writeJson s], state
              | Success (options) ->
                  let projectOptions, lines = options
                  let ok = pos.Line <= lines.Length && pos.Line >= 1 &&
                           pos.Col <= lineStr.Length + 1 && pos.Col >= 1
                  if not ok then
                      return [Response.error writeJson "Position is out of range"], state
                  else
                    // TODO: Should sometimes pass options.Source in here to force a reparse
                    //       for completions e.g. `(some typed expr).$`
                    let tyResOpt = checker.TryGetRecentTypeCheckResultsForFile(file, projectOptions)
                    match tyResOpt with
                    | None -> return [ Response.info writeJson "Cached typecheck results not yet available"], state
                    | Some tyRes ->
                        let ctx = context tyRes
                        return!
                          match cmd with
                          | Completion -> Commands.completion ctx pos lineStr filter
                          | ToolTip -> Commands.toolTip ctx pos lineStr
                          | TypeSig -> Commands.typesig ctx pos lineStr
                          | SymbolUse -> Commands.symbolUse ctx pos lineStr
                          | FindDeclaration -> Commands.findDeclarations ctx pos lineStr
                          | Methods -> Commands.methods ctx pos lines
                          | SymbolUseProject -> Commands.symbolUseProject ctx pos lineStr

          | CompilerLocation -> return! Commands.compilerLocation writeJson state
          | Colorization enabled -> return! Commands.colorization state enabled
          | Lint filename -> return! Commands.lint writeJson state checker filename
          | Error msg -> return! Commands.error writeJson state msg
          | Quit ->
              quit <- true
              return [], state
      }
      |> Async.Catch
      |> Async.RunSynchronously
      |> function
         | Choice1Of2 (res, state') ->
            state <- state'
            res |> List.iter Console.WriteLine
         | Choice2Of2 exn ->
            exn
            |> sprintf "Unexpected internal error. Please report at https://github.com/fsharp/FsAutoComplete/issues, attaching the exception information:\n%O"
            |> Response.error writeJson 
            |> Console.WriteLine
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
            commandQueue.Add (CommandInput.parseCommand(Console.ReadLine()))
        }
        |> Async.Start

        main State.Initial
      finally
        (!Debug.output).Close ()
