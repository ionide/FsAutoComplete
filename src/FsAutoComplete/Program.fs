namespace FsAutoComplete

open System
open System.IO
open Microsoft.FSharp.Compiler
open JsonSerializer
open FsAutoComplete

module internal Main =
  module Response = CommandResponse
  let mutable currentFiles = Map.empty
  let originalFs = AbstractIL.Internal.Library.Shim.FileSystem
  let fs = new FileSystem(originalFs, fun () -> currentFiles)
  AbstractIL.Internal.Library.Shim.FileSystem <- fs

  let commandQueue = new FSharpx.Control.BlockingQueueAgent<Command>(10)

  let main () : int =
    let mutable quit = false
    let commands = Commands(writeJson)

    while not quit do
      currentFiles <- commands.Files
      async {
          match commandQueue.Get() with
          | Parse (file, kind, lines) -> 
              let! res = commands.Parse file lines
              //Hack for tests
              let r = match kind with
                      | Synchronous -> Response.info writeJson "Synchronous parsing started" 
                      | Normal -> Response.info writeJson "Background parsing started"
              return r :: res

          | Project (file, verbose) ->
              //THIS SHOULD BE INITIALIZED SOMEWHERE ELSE ?
              let fullPath = Path.GetFullPath file
              let fsw = new FileSystemWatcher(Path = Path.GetDirectoryName fullPath, Filter = Path.GetFileName fullPath)
              fsw.Changed.Add(fun _ -> commandQueue.Add(Project (fullPath, verbose)))
              fsw.EnableRaisingEvents <- true
              return! commands.Project file verbose

          | Declarations file -> return! commands.Declarations file
          | HelpText sym -> return commands.Helptext sym
          | PosCommand (cmd, file, lineStr, pos, _timeout, filter) ->
              let file = Path.GetFullPath file
              match commands.TryGetFileCheckerOptionsWithLines file with
              | Failure s -> return [Response.error writeJson s]
              | Success options ->
                  let projectOptions, lines = options
                  let ok = pos.Line <= lines.Length && pos.Line >= 1 &&
                           pos.Col <= lineStr.Length + 1 && pos.Col >= 1
                  if not ok then
                      return [Response.error writeJson "Position is out of range"]
                  else
                    // TODO: Should sometimes pass options.Source in here to force a reparse
                    //       for completions e.g. `(some typed expr).$`
                    let tyResOpt = commands.TryGetRecentTypeCheckResultsForFile(file, projectOptions)
                    match tyResOpt with
                    | None -> return [ Response.info writeJson "Cached typecheck results not yet available"]
                    | Some tyRes ->
                        return!
                            match cmd with
                            | Completion -> commands.Completion tyRes pos lineStr filter
                            | ToolTip -> commands.ToolTip tyRes pos lineStr
                            | TypeSig -> commands.Typesig tyRes pos lineStr
                            | SymbolUse -> commands.SymbolUse tyRes pos lineStr
                            | FindDeclaration -> commands.FindDeclarations tyRes pos lineStr
                            | Methods -> commands.Methods tyRes pos lines
                            | SymbolUseProject -> commands.SymbolUseProject tyRes pos lineStr

          | CompilerLocation -> return commands.CompilerLocation()
          | Colorization enabled -> commands.Colorization enabled; return []
          | Lint filename -> return! commands.Lint filename
          | Error msg -> return commands.Error msg
          | Quit ->
              quit <- true
              return []
      }
      |> Async.Catch
      |> Async.RunSynchronously
      |> function
         | Choice1Of2 res -> res |> List.iter Console.WriteLine
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

        main()
      finally
        (!Debug.output).Close()
