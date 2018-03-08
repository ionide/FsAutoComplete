module FsAutoComplete.Stdio

open System
open System.IO
open FsAutoComplete.JsonSerializer
open System.Collections.Concurrent
module Response = CommandResponse

let main (commands: Commands) (commandQueue: BlockingCollection<Command>) =
    let mutable quit = false

    commands.NotifyErrorsInBackground <- false

    // use a mailboxprocess to queue the send of notifications
    use agent = MailboxProcessor.Start ((fun inbox ->
        let rec messageLoop () = async {

            let! msg = inbox.Receive()

            let msgText =
                match msg with
                | NotificationEvent.ParseError x -> x
                | NotificationEvent.Workspace x -> x

            Console.WriteLine(msgText)

            return! messageLoop ()
            }

        messageLoop ()
        ))

    let _notifications =
        commands.Notify
        |> Observable.subscribe agent.Post

    while not quit do
      async {
          match commandQueue.Take() with
          | Started ->
              return [ 
                  CommandResponse.info writeJson (sprintf "git commit sha: %s" <| commands.GetGitHash);
                  CommandResponse.info writeJson (sprintf "Started (PID=%i)" (System.Diagnostics.Process.GetCurrentProcess().Id))
               ]
          | Parse (file, kind, lines) ->
              let! res = commands.Parse file lines 0
              //Hack for tests
              let r = match kind with
                      | Synchronous -> Response.info writeJson "Synchronous parsing started"
                      | Normal -> Response.info writeJson "Background parsing started"
              return r :: res

          | Project (file, verbose) ->
              return! commands.Project file verbose (fun fullPath -> commandQueue.Add(Project (fullPath, verbose)))
          | Declarations file -> return! commands.Declarations file None None
          | HelpText sym -> return commands.Helptext sym
          | PosCommand (cmd, file, lineStr, pos, _timeout, filter) ->
              let file = Path.GetFullPath file
              match commands.TryGetFileCheckerOptionsWithLines file with
              | ResultOrString.Error s -> return [Response.error writeJson s]
              | ResultOrString.Ok options ->
                  let projectOptions, lines = options
                  let ok = pos.Line <= lines.Length && pos.Line >= 1 &&
                           pos.Column <= lineStr.Length + 1 && pos.Column >= 1
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
                            | Completion -> commands.Completion tyRes pos lineStr lines file filter false false
                            | ToolTip -> commands.ToolTip tyRes pos lineStr
                            | TypeSig -> commands.Typesig tyRes pos lineStr
                            | SymbolUse -> commands.SymbolUse tyRes pos lineStr
                            | FindDeclaration -> commands.FindDeclaration tyRes pos lineStr
                            | FindTypeDeclaration -> commands.FindTypeDeclaration tyRes pos lineStr
                            | Methods -> commands.Methods tyRes pos lines
                            | SymbolUseProject -> commands.SymbolUseProject tyRes pos lineStr
                            | SignatureData -> commands.SignatureData tyRes pos lineStr
          | CompilerLocation -> return commands.CompilerLocation()
          | Colorization enabled -> commands.Colorization enabled; return []
          | Lint filename -> return! commands.Lint filename
          | UnusedDeclarations filename -> return! commands.GetUnusedDeclarations filename
          | SimplifiedNames filename -> return! commands.GetSimplifiedNames filename
          | UnusedOpens filename -> return! commands.GetUnusedOpens filename
          | WorkspacePeek (dir, deep, excludeDir) -> return! commands.WorkspacePeek dir deep (excludeDir |> List.ofArray)
          | WorkspaceLoad files -> return! commands.WorkspaceLoad (fun fullPath -> commandQueue.Add(Project (fullPath, false))) (files |> List.ofArray)
          | Error msg -> return commands.Error msg
          | Quit ->
              quit <- true
              return! commands.Quit()
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

let trimBOM (s: string) =
    let bomUTF8 = Text.Encoding.UTF8.GetString(Text.Encoding.UTF8.GetPreamble())
    if (s.StartsWith(bomUTF8, StringComparison.Ordinal)) then
        s.Remove(0, bomUTF8.Length)
    else
        s

let start (commands: Commands) (args: Argu.ParseResults<Options.CLIArguments>) =
    Console.InputEncoding <- Text.Encoding.UTF8
    Console.OutputEncoding <- new Text.UTF8Encoding(false, false)

    let commandQueue = new BlockingCollection<Command>(10)

    args.TryGetResult(<@ Options.CLIArguments.HostPID @>)
    |> Option.iter (Debug.zombieCheckWithHostPID (fun () -> commandQueue.Add(Command.Quit)))

    try
        async {
          if Debug.verbose then
            commandQueue.Add(Command.Started)

          while true do
            let inputLine = Console.ReadLine()
#if NETCOREAPP2_0
            //on .net core, bom is not stripped.
            //ref https://github.com/dotnet/standard/issues/260
            //TODO tweak console.inputencoding to remove the BOM instead of this ugly hack
            let inputLine = trimBOM inputLine
#endif
            let cmd = CommandInput.parseCommand(inputLine)
            commandQueue.Add(cmd)
        }
        |> Async.Start

        main commands commandQueue
    finally
        (Debug.output).Close()
