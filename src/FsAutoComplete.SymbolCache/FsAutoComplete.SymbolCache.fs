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
    | FillStateForFile of file : string
    | FillState


type PersistentState (dir) =
    let connection = SymbolCache.PersistenCacheImpl.initializeCache dir

    let agent = MailboxProcessor.Start <| fun mb ->
        let rec loop () = async {
            let! msg = mb.Receive()
            match msg with
            | Save (file, sugs) ->
                try
                    PersistenCacheImpl.insert connection file sugs
                with
                | ex ->
                    printfn "%s" ex.Message
                    printfn "%s" ex.StackTrace
                return! loop()
            | FillState ->
                try
                    PersistenCacheImpl.loadAll connection
                    |> Seq.groupBy (fun r -> r.FileName)
                    |> Seq.iter (fun (fn, lst) ->
                        let sms = lst |> Seq.toArray
                        state.AddOrUpdate(fn, sms, fun _ _ -> sms) |> ignore
                    )
                with
                | ex ->
                    printfn "%s" ex.Message
                    printfn "%s" ex.StackTrace
                return! loop()
            | FillStateForFile fn ->
                try
                    let sms =
                        PersistenCacheImpl.loadFile connection fn
                        |> Seq.toArray
                    state.AddOrUpdate(fn, sms, fun _ _ -> sms) |> ignore
                with
                | ex ->
                    printfn "%s" ex.Message
                    printfn "%s" ex.StackTrace
                return! loop()
        }
        loop ()

    member __.Save(file, sugs) = Save (file, sugs) |> agent.Post
    member __.Load(file) = FillStateForFile file |> agent.Post
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
            let! res = checker.CheckProject(opts)
            let! results = res.GetAllUsesOfAllSymbols()
            results
            |> Array.groupBy (fun s -> s.FileName)
            |> Array.iter (fun (fn, symbols) ->
                let sms = symbols |> Array.map (SymbolCache.fromSymbolUse)
                state.AddOrUpdate(fn, sms, fun _ _ -> sms) |> onAdded fn
            )
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

        writeJson uses


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
                    do pS.Load req.Filename
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
