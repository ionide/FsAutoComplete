module FsAutoComplete.Suave

open System.IO
open Suave
open Suave.Http
open Suave.Operators
open Suave.Web
open Suave.WebPart
open Suave.WebSocket
open Suave.Sockets.Control
open Suave.Filters
open Newtonsoft.Json
open FsAutoComplete.JsonSerializer
open FsAutoComplete.HttpApiContract

[<AutoOpen>]
module internal Utils =
    let private fromJson<'a> json =
        JsonConvert.DeserializeObject(json, typeof<'a>) :?> 'a

    let getResourceFromReq<'a> (req : HttpRequest) =
        let getString rawForm =
            System.Text.Encoding.UTF8.GetString(rawForm)
        req.rawForm |> getString |> fromJson<'a>

open Argu

let start (commands: Commands) (args: ParseResults<Options.CLIArguments>) =
    let mutable client : WebSocket option  = None

    let handler f : WebPart = fun (r : HttpContext) -> async {
          let data = r.request |> getResourceFromReq
          let! res = Async.Catch (f data)
          match res with
          | Choice1Of2 res ->
             let res' = res |> List.toArray |> Json.toJson
             return! Response.response HttpCode.HTTP_200 res' r
          | Choice2Of2 e ->
            printfn "Unhandled error - %s \n %s" e.Message e.StackTrace
            return! Response.response HttpCode.HTTP_500 (Json.toJson e) r
        }

    let positionHandler (f : PositionRequest -> ParseAndCheckResults -> string -> string [] -> Async<string list>) : WebPart = fun (r : HttpContext) ->
        async {
            let data = r.request |> getResourceFromReq<PositionRequest>
            let file = Path.GetFullPath data.FileName
            let! res =
                match commands.TryGetFileCheckerOptionsWithLinesAndLineStr(file, { Line = data.Line; Col = data.Column }) with
                | Failure s -> async.Return ([CommandResponse.error writeJson s])
                | Success (options, lines, lineStr) ->
                  // TODO: Should sometimes pass options.Source in here to force a reparse
                  //       for completions e.g. `(some typed expr).$`
                  try
                    let tyResOpt = commands.TryGetRecentTypeCheckResultsForFile(file, options)
                    match tyResOpt with
                    | None -> async.Return [CommandResponse.info writeJson "Cached typecheck results not yet available"]
                    | Some tyRes ->
                        async {
                            let! r = Async.Catch (f data tyRes lineStr lines)
                            match r with
                            | Choice1Of2 r -> return r
                            | Choice2Of2 e -> return [CommandResponse.error writeJson e.Message]
                        }
                  with e -> async.Return [CommandResponse.error writeJson e.Message]
            let res' = res |> List.toArray |> Json.toJson
            return! Response.response HttpCode.HTTP_200 res' r
        }


    let echo (webSocket : WebSocket) =
        fun cx ->
            client <- Some webSocket
            socket {
                let loop = ref true
                while !loop do
                    let! msg = webSocket.read()
                    match msg with
                    | (Ping, _, _) -> do! webSocket.send Pong [||] true
                    | (Close, _, _) ->
                        do! webSocket.send Close [||] true
                        client <- None
                        loop := false
                    | _ -> ()
                }

    let app =
        choose [
            // path "/notify" >=> handShake echo
            path "/parse" >=> handler (fun (data : ParseRequest) -> async {
                let! res = commands.Parse data.FileName data.Lines data.Version
                //Hack for tests
                let r = match data.IsAsync with
                        | false -> CommandResponse.info writeJson "Synchronous parsing started"
                        | true -> CommandResponse.info writeJson "Background parsing started"
                return r :: res
                })
            path "/parseProjects" >=> handler (fun (data : ProjectRequest) -> commands.ParseProjectsForFile data.FileName)
            //TODO: Add filewatcher
            path "/parseProjectsInBackground" >=> handler (fun (data : ProjectRequest) -> commands.ParseAndCheckProjectsInBackgroundForFile data.FileName)
            path "/project" >=> handler (fun (data : ProjectRequest) -> commands.Project data.FileName false ignore)
            path "/declarations" >=> handler (fun (data : DeclarationsRequest) -> commands.Declarations data.FileName (Some data.Version) )
            path "/declarationsProjects" >=> fun httpCtx ->
                async {
                    let! errors = commands.DeclarationsInProjects ()
                    let res = errors |> List.toArray |> Json.toJson
                    return! Response.response HttpCode.HTTP_200 res httpCtx
                }
            path "/helptext" >=> handler (fun (data : HelptextRequest) -> commands.Helptext data.Symbol |> async.Return)
            path "/completion" >=> handler (fun (data : CompletionRequest) -> async {
                let file = Path.GetFullPath data.FileName
                match commands.TryGetFileCheckerOptionsWithLines file with
                | Failure s -> return [CommandResponse.error writeJson s]
                | Success (options, lines) ->
                    let line = data.Line
                    let col = data.Column
                    let lineStr = data.SourceLine
                    let ok = line <= lines.Length && line >= 1 && col <= lineStr.Length + 1 && col >= 1
                    if not ok then
                        return [CommandResponse.error writeJson "Position is out of range"]
                    else
                        let tyResOpt = commands.TryGetRecentTypeCheckResultsForFile(file, options)
                        match tyResOpt with
                        | None -> return [ CommandResponse.info writeJson "Cached typecheck results not yet available"]
                        | Some tyRes -> return! commands.Completion tyRes { Line = data.Line; Col = data.Column } lineStr (Some data.Filter) data.IncludeKeywords
                })
            path "/tooltip" >=> positionHandler (fun data tyRes lineStr _ -> commands.ToolTip tyRes { Line = data.Line; Col = data.Column } lineStr)
            path "/signature" >=> positionHandler (fun data tyRes lineStr _ -> commands.Typesig tyRes { Line = data.Line; Col = data.Column } lineStr)
            path "/symboluseproject" >=> positionHandler (fun data tyRes lineStr _ -> commands.SymbolUseProject tyRes { Line = data.Line; Col = data.Column } lineStr)
            path "/symboluse" >=> positionHandler (fun data tyRes lineStr _ -> commands.SymbolUse tyRes { Line = data.Line; Col = data.Column } lineStr)
            path "/signatureData" >=> positionHandler (fun data tyRes lineStr _ -> commands.SignatureData tyRes { Line = data.Line; Col = data.Column } lineStr)
            path "/finddeclaration" >=> positionHandler (fun data tyRes lineStr _ -> commands.FindDeclarations tyRes { Line = data.Line; Col = data.Column } lineStr)
            path "/methods" >=> positionHandler (fun data tyRes _ lines   -> commands.Methods tyRes { Line = data.Line; Col = data.Column } lines)
            path "/help" >=> positionHandler (fun data tyRes line _   -> commands.Help tyRes { Line = data.Line; Col = data.Column } line)
            path "/compilerlocation" >=> fun httpCtx ->
                async {
                    let res = commands.CompilerLocation() |> List.toArray |> Json.toJson
                    return! Response.response HttpCode.HTTP_200 res httpCtx
                }
            path "/lint" >=> handler (fun (data: LintRequest) -> commands.Lint data.FileName)
            path "/namespaces" >=> positionHandler (fun data tyRes lineStr _   -> commands.GetNamespaceSuggestions tyRes { Line = data.Line; Col = data.Column } lineStr)
            path "/unionCaseGenerator" >=> positionHandler (fun data tyRes lineStr lines   -> commands.GetUnionPatternMatchCases tyRes { Line = data.Line; Col = data.Column } lines lineStr)
            path "/workspacePeek" >=> handler (fun (data : WorkspacePeekRequest) -> commands.WorkspacePeek data.Directory data.Deep (data.ExcludedDirs |> List.ofArray))
        ]

    let port = args.GetResult (<@ Options.CLIArguments.Port @>, defaultValue = 8088)

    let defaultBinding = defaultConfig.bindings.[0]
    let withPort = { defaultBinding.socketBinding with port = uint16 port }
    let serverConfig =
        { defaultConfig with bindings = [{ defaultBinding with socketBinding = withPort }]}

    match args.TryGetResult (<@ Options.CLIArguments.HostPID @>) with
    | Some pid ->
        serverConfig.logger.Log Logging.LogLevel.Info (fun () -> Logging.LogLine.mk "FsAutoComplete" Logging.LogLevel.Info Logging.TraceHeader.empty None (sprintf "tracking host PID %i" pid))
        Debug.zombieCheckWithHostPID (fun () -> exit 0) pid
    | None -> ()

    startWebServer serverConfig app
    0
