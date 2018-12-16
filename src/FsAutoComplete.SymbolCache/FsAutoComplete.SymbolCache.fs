module FsAutoComplete.SymbolCacheApi

open Suave
open Suave.Operators
open Suave.Filters
open Newtonsoft.Json
open FsAutoComplete.JsonSerializer
open System.Collections.Concurrent
open FSharp.Compiler.SourceCodeServices
open System
open FsAutoComplete.Utils
open SymbolCache
open System.IO
open Microsoft.Data.Sqlite
open Dapper
open System.Data
open Suave.Logging


[<AutoOpen>]
module internal Utils =
    let private fromJson<'a> json =
        JsonConvert.DeserializeObject(json, typeof<'a>) :?> 'a

    let getResourceFromReq<'a> (req : HttpRequest) =
        let getString (rawForm : byte []) =
            System.Text.Encoding.UTF8.GetString(rawForm)
        req.rawForm |> getString |> fromJson<'a>

let state = ConcurrentDictionary<string, SymbolUseRange[]>()

type private PersistentStateMessage =
    | Save of file : string * symbols : SymbolUseRange[]
    | FillState

type PersistentState (dir) =
    let connectionString = sprintf "Data Source=%s/.ionide/symbolCache.db" dir

    let dir = Path.Combine(dir, ".ionide")
    do if not (Directory.Exists dir) then Directory.CreateDirectory dir |> ignore
    let dbPath = Path.Combine(dir, "symbolCache.db")
    let dbExists = File.Exists dbPath
    let connection = new SqliteConnection(connectionString)

    do if not dbExists then
        let fs = File.Create(dbPath)
        fs.Close()
        let cmd = "CREATE TABLE Symbols(
            FileName TEXT,
            StartLine INT,
            StartColumn INT,
            EndLine INT,
            EndColumn INT,
            IsFromDefinition BOOLEAN,
            IsFromAttribute BOOLEAN,
            IsFromComputationExpression BOOLEAN,
            IsFromDispatchSlotImplementation BOOLEAN,
            IsFromPattern BOOLEAN,
            IsFromType BOOLEAN,
            SymbolFullName TEXT,
            SymbolDisplayName TEXT,
            SymbolIsLocal BOOLEAN
        )"

        connection.Execute(cmd)
        |> ignore


    let agent = MailboxProcessor.Start <| fun mb ->
        let rec loop () = async {
            let! msg = mb.Receive()
            match msg with
            | Save (file, sugs) ->
                try
                    let d = DateTime.Now
                    printfn "[Debug] Updating DB for %s" file
                    if connection.State <> ConnectionState.Open then connection.Open()
                    use tx = connection.BeginTransaction()
                    let delCmd = sprintf "DELETE FROM Symbols WHERE FileName=\"%s\"" file
                    let inserCmd =
                        sprintf "INSERT INTO SYMBOLS(FileName, StartLine, StartColumn, EndLine, EndColumn, IsFromDefinition, IsFromAttribute, IsFromComputationExpression, IsFromDispatchSlotImplementation, IsFromPattern, IsFromType, SymbolFullName, SymbolDisplayName, SymbolIsLocal) VALUES
                        (@FileName, @StartLine, @StartColumn, @EndLine, @EndColumn, @IsFromDefinition, @IsFromAttribute, @IsFromComputationExpression, @IsFromDispatchSlotImplementation, @IsFromPattern, @IsFromType, @SymbolFullName, @SymbolDisplayName, @SymbolIsLocal)"
                    connection.Execute(delCmd, transaction = tx) |> ignore
                    connection.Execute(inserCmd, sugs, transaction = tx) |> ignore
                    tx.Commit()
                    let e = DateTime.Now
                    printfn "[Debug] Updating DB took %fms" (e-d).TotalMilliseconds
                with
                | ex ->
                    printfn "%s" ex.Message
                    printfn "%s" ex.StackTrace
                return! loop()
            | FillState ->
                try
                    printfn "[Debug] Loading initial state"
                    let d = DateTime.Now
                    if connection.State <> ConnectionState.Open then connection.Open()
                    let q = "SELECT * FROM SYMBOLS"
                    let res = connection.Query<SymbolUseRange>(q)
                    res
                    |> Seq.groupBy (fun r -> r.FileName)
                    |> Seq.iter (fun (fn, lst) ->
                        let sms = lst |> Seq.toArray
                        state.AddOrUpdate(fn, sms, fun _ _ -> sms) |> ignore
                    )
                    let e = DateTime.Now
                    printfn "Loaded initial state in %fms" (e-d).TotalMilliseconds

                with
                | ex ->
                    printfn "%s" ex.Message
                    printfn "%s" ex.StackTrace
                return! loop()
        }
        loop ()

    member __.Save(file, sugs) = Save (file, sugs) |> agent.Post
    member __.Initialize () = agent.Post FillState





type BackgroundFSharpCompilerServiceChecker() =
  let checker =
    FSharpChecker.Create(
      projectCacheSize = 1,
      keepAllBackgroundResolutions = false,
      keepAssemblyContents = false)

  do checker.ImplicitlyStartBackgroundWork <- false

  do checker.BeforeBackgroundFileCheck.Add ignore

  member __.CheckProject = checker.ParseAndCheckProject

module Commands =
    let checker = BackgroundFSharpCompilerServiceChecker()

    let buildCacheForProject (onAdded : string -> SymbolUseRange[] -> unit) opts =
        async {
            let start = DateTime.Now
            let! res = checker.CheckProject(opts)
            let! results = res.GetAllUsesOfAllSymbols()
            results
            |> Array.groupBy (fun s -> s.FileName)
            |> Array.iter (fun (fn, symbols) ->
                let sms = symbols |> Array.map (SymbolCache.fromSymbolUse)
                state.AddOrUpdate(fn, sms, fun _ _ -> sms) |> onAdded fn
            )
            let e = DateTime.Now
            printfn "Project %s took %fms" opts.ProjectFileName (e-start).TotalMilliseconds
        } |> Async.RunSynchronously

    let getSymbols symbolName =
        let uses =
            state.Values
            |> Seq.collect id
            |> Seq.where (fun sm -> sm.SymbolFullName = symbolName && not sm.SymbolIsLocal)
            |> Seq.toArray

        writeJson uses

    let getImplementations symbolName =
        let uses =
            state.Values
            |> Seq.collect id
            |> Seq.where (fun sm -> sm.SymbolFullName = symbolName && not sm.SymbolIsLocal && (sm.IsFromDispatchSlotImplementation || sm.IsFromType ))
            |> Seq.toList
        let su : SymbolUseResponse = {Name = uses.[0].SymbolDisplayName; Uses = uses }

        writeJson { Kind = "symbolimplementation"; Data = su }

    let updateSymbols (onAdded : string -> SymbolUseRange[] -> unit) (req: SymbolCacheRequest) =
        state.AddOrUpdate(req.Filename, req.Uses, fun _ _ -> req.Uses)
        |> onAdded req.Filename


let start port dir =
    Directory.SetCurrentDirectory dir
    let pS = PersistentState(dir)
    pS.Initialize ()

    let app =
        choose [
            path "/buildCacheForProject" >=> fun httpCtx ->
                try
                    let d = DateTime.Now
                    let opts = getResourceFromReq<FSharpProjectOptions> httpCtx.request
                    do Commands.buildCacheForProject (fun fn syms -> pS.Save(fn,syms) ) opts
                    let e = DateTime.Now
                    printfn "[Debug] /buildCacheForProject request took %fms" (e-d).TotalMilliseconds
                    Response.response HttpCode.HTTP_200 [||] httpCtx
                with
                | ex ->
                    printfn "%s" ex.Message
                    printfn "%s" ex.StackTrace
                    Response.response HttpCode.HTTP_200 [||] httpCtx

            path "/getSymbols" >=> fun httpCtx ->
                try
                    let d = DateTime.Now
                    let sn = httpCtx.request.rawForm |> System.Text.Encoding.UTF8.GetString
                    let uses = Commands.getSymbols sn
                    let res = System.Text.Encoding.UTF8.GetBytes uses
                    let e = DateTime.Now
                    printfn "[Debug] /getSymbols request took %fms" (e-d).TotalMilliseconds
                    Response.response HttpCode.HTTP_200 res httpCtx
                with
                | ex ->
                    printfn "%s" ex.Message
                    printfn "%s" ex.StackTrace
                    let res = System.Text.Encoding.UTF8.GetBytes "ERROR"
                    Response.response HttpCode.HTTP_200 res httpCtx

            path "/getImplementation" >=> fun httpCtx ->
                try
                    let d = DateTime.Now
                    let sn = httpCtx.request.rawForm |> System.Text.Encoding.UTF8.GetString
                    let uses = Commands.getImplementations sn
                    let res = System.Text.Encoding.UTF8.GetBytes uses
                    let e = DateTime.Now
                    printfn "[Debug] /getImplementation request took %fms" (e-d).TotalMilliseconds
                    Response.response HttpCode.HTTP_200 res httpCtx
                with
                | ex ->
                    printfn "%s" ex.Message
                    printfn "%s" ex.StackTrace
                    let res = System.Text.Encoding.UTF8.GetBytes "ERROR"
                    Response.response HttpCode.HTTP_200 res httpCtx

            path "/updateSymbols" >=> fun httpCtx ->
                try
                    let d = DateTime.Now
                    let req = getResourceFromReq<SymbolCacheRequest> httpCtx.request
                    do Commands.updateSymbols (fun fn syms -> pS.Save(fn,syms) ) req
                    let e = DateTime.Now
                    printfn "[Debug] /updateSymbols request took %fms" (e-d).TotalMilliseconds
                    Response.response HttpCode.HTTP_200 [||] httpCtx
                with
                | ex ->
                    printfn "%s" ex.Message
                    printfn "%s" ex.StackTrace
                    Response.response HttpCode.HTTP_200 [||] httpCtx
        ]

    let defaultBinding = defaultConfig.bindings.[0]
    let withPort = { defaultBinding.socketBinding with port = uint16 port }
    let serverConfig =
        { defaultConfig with
            bindings = [{ defaultBinding with socketBinding = withPort }] }

    printfn "Current directory: %s" Environment.CurrentDirectory
    startWebServer serverConfig app
    0
