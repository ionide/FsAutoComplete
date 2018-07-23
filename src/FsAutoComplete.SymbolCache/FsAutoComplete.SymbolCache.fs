module FsAutoComplete.SymbolCacheApi

open Suave
open Suave.Operators
open Suave.Filters
open Newtonsoft.Json
open FsAutoComplete.JsonSerializer
open System.Collections.Concurrent
open Microsoft.FSharp.Compiler.SourceCodeServices
open System
open FsAutoComplete.Utils
open CommandResponse
open SymbolCache
open FsAutoComplete.Utils

[<AutoOpen>]
module internal Utils =
    let private fromJson<'a> json =
        JsonConvert.DeserializeObject(json, typeof<'a>) :?> 'a

    let getResourceFromReq<'a> (req : HttpRequest) =
        let getString (rawForm : byte []) =
            System.Text.Encoding.UTF8.GetString(rawForm)
        req.rawForm |> getString |> fromJson<'a>

let state = ConcurrentDictionary<string, SymbolUseRange[]>()


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

    let buildCacheForProject opts =
        async {
            let start = DateTime.Now
            let! res = checker.CheckProject(opts)
            let! results = res.GetAllUsesOfAllSymbols()
            results
            |> Array.groupBy (fun s -> s.FileName)
            |> Array.iter (fun (fn, symbols) ->
                let sms = symbols |> Array.map (SymbolCache.fromSymbolUse)
                state.AddOrUpdate(fn, sms, fun _ _ -> sms) |> ignore
                printfn "%A" (sms |> Array.map (fun s -> s.SymbolFullName))
            )
            let e = DateTime.Now
            printfn "PROJECT: %s TOOK %fms" opts.ProjectFileName (e-start).TotalMilliseconds
        } |> Async.RunSynchronously

    let getSymbols symbolName =
        let uses =
            state.Values
            |> Seq.collect id
            |> Seq.where (fun sm -> sm.SymbolFullName = symbolName && not sm.SymbolIsLocal)
            |> Seq.toList
        let su : SymbolUseResponse = {Name = uses.[0].SymbolDisplayName; Uses = uses }

        writeJson { Kind = "symboluse"; Data = su }

    let updateSymbols (req: SymbolCacheRequest) =
        state.AddOrUpdate(req.Filename, req.Uses, fun _ _ -> req.Uses)
        |> ignore
        printfn "%A" (req.Uses |> Array.map (fun s -> s.SymbolFullName))


let start port =

    let app =
        choose [
            path "/buildCacheForProject" >=> fun httpCtx ->
                try
                    let opts = getResourceFromReq<FSharpProjectOptions> httpCtx.request
                    do Commands.buildCacheForProject opts
                    Response.response HttpCode.HTTP_200 [||] httpCtx
                with
                | ex ->
                    printfn "%s" ex.Message
                    printfn "%s" ex.StackTrace
                    Response.response HttpCode.HTTP_200 [||] httpCtx

            path "/getSymbols" >=> fun httpCtx ->
                try
                    let sn = httpCtx.request.rawForm |> System.Text.Encoding.UTF8.GetString
                    let uses = Commands.getSymbols sn
                    let res = System.Text.Encoding.UTF8.GetBytes uses
                    Response.response HttpCode.HTTP_200 res httpCtx
                with
                | ex ->
                    printfn "%s" ex.Message
                    printfn "%s" ex.StackTrace
                    let res = System.Text.Encoding.UTF8.GetBytes "ERROR"
                    Response.response HttpCode.HTTP_200 res httpCtx

            path "/updateSymbols" >=> fun httpCtx ->
                try
                    let req = getResourceFromReq<SymbolCacheRequest> httpCtx.request
                    do Commands.updateSymbols req
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

    startWebServer serverConfig app
    0
