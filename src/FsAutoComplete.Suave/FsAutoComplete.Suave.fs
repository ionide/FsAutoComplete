module FsAutoComplete.Suave

open System
open System.IO

open Suave
open Suave.Http
open Suave.Operators
open Suave.Web
open Suave.WebPart
open Suave.Filters
open Newtonsoft.Json
open Microsoft.FSharp.Compiler

open FsAutoComplete
open FsAutoComplete.Commands
open FsAutoComplete.JsonSerializer

[<AutoOpen>]
module Contract =
    type ParseRequest = { FileName : string; IsAsync : bool; Lines : string[]}
    type ProjectRequest = { FileName : string;}
    type DeclarationsRequest = {FileName : string}
    type HelptextRequest = {Symbol : string}
    type CompletionRequest = {FileName : string; SourceLine : string; Pos: Pos; Filter : string}
    type PositionRequest = {FileName : string; Pos: Pos; Filter : string}
    type LintRequest = {FileName : string}
 
[<AutoOpen>]
module internal Utils =
    let private fromJson<'a> json =
        JsonConvert.DeserializeObject(json, typeof<'a>) :?> 'a

    let getResourceFromReq<'a> (req : HttpRequest) =
        let getString rawForm =
            System.Text.Encoding.UTF8.GetString(rawForm)
        req.rawForm |> getString |> fromJson<'a>

[<EntryPoint>]
let main argv =
    System.Threading.ThreadPool.SetMinThreads(8, 8) |> ignore
    let mutable state = FsAutoComplete.State.Initial
    let checker = new FSharpCompilerServiceChecker()
    
    let context tyRes =
        { Serialize = writeJson
          State = state
          TyRes = tyRes
          Checker = checker }

    let currentFiles = ref Map.empty
    let originalFs = AbstractIL.Internal.Library.Shim.FileSystem
    let fs = new FileSystem(originalFs, fun () -> !currentFiles)
    AbstractIL.Internal.Library.Shim.FileSystem <- fs

    let handler f : WebPart = fun (r : HttpContext) -> async {
          let data = r.request |> getResourceFromReq
          let! res, state' = f data
          state <- state'
          let res' = res |> List.toArray |> Json.toJson
          return! Response.response HttpCode.HTTP_200 res' r
        }

    let positionHandler (f : PositionRequest -> Context -> string -> string [] -> Async<string list * State>) : WebPart = fun (r : HttpContext) -> async {
        let data = r.request |> getResourceFromReq<PositionRequest>
        let file = Path.GetFullPath data.FileName
        let! (res, (state' : State)) =
            match state.TryGetFileCheckerOptionsWithLinesAndLineStr(file, data.Pos) with
            | Failure s -> async { return [CommandResponse.error writeJson (s)], state }
            | Success (options, lines, lineStr) ->
              // TODO: Should sometimes pass options.Source in here to force a reparse
              //       for completions e.g. `(some typed expr).$`
              let tyResOpt = checker.TryGetRecentTypeCheckResultsForFile(file, options)
              match tyResOpt with
              | None -> async { return [ CommandResponse.info writeJson "Cached typecheck results not yet available"], state }
              | Some tyRes -> f data (context tyRes) lineStr lines
        state <- state'
        let res' = res |> List.toArray |> Json.toJson
        return! Response.response HttpCode.HTTP_200 res' r
    } 
 
    let app =
        Writers.setMimeType "application/json; charset=utf-8" >=>
        POST >=>
        choose [
            path "/parse"        >=> handler (fun (data : ParseRequest) -> Commands.parse writeJson state checker data.FileName data.Lines)
            //TODO: Add filewatcher
            path "/project"      >=> handler (fun (data : ProjectRequest) -> Commands.project writeJson state checker data.FileName DateTime.Now false)
            path "/declarations" >=> handler (fun (data : DeclarationsRequest) -> Commands.declarations writeJson state checker data.FileName)
            path "/helptext"     >=> handler (fun (data : HelptextRequest) -> Commands.helptext writeJson state data.Symbol)
            path "/completion"   >=> handler (fun (data : CompletionRequest) ->  async {
                let file = Path.GetFullPath data.FileName
                match state.TryGetFileCheckerOptionsWithLines(file) with
                | Failure s -> return [CommandResponse.error writeJson s], state
                | Success (options, lines) ->
                    let line = data.Pos.Line
                    let col = data.Pos.Col
                    let lineStr = data.SourceLine
                    let ok = line <= lines.Length && line >= 1 && col <= lineStr.Length + 1 && col >= 1
                    if not ok then
                        return [CommandResponse.error writeJson "Position is out of range"], state
                    else                    
                        let tyResOpt = checker.TryGetRecentTypeCheckResultsForFile(file, options)
                        match tyResOpt with
                        | None -> return [ CommandResponse.info writeJson "Cached typecheck results not yet available"], state
                        | Some tyRes -> return! Commands.completion (context tyRes) data.Pos lineStr (Some data.Filter)
                })
            path "/tooltip"          >=> positionHandler (fun data ctx lineStr _ -> Commands.toolTip ctx data.Pos lineStr )
            path "/symboluseproject" >=> positionHandler (fun data ctx lineStr _ -> Commands.symbolUseProject ctx data.Pos lineStr )
            path "/symboluse"        >=> positionHandler (fun data ctx lineStr _ -> Commands.symbolUse ctx data.Pos lineStr )
            path "/finddeclaration"  >=> positionHandler (fun data ctx lineStr _ -> Commands.findDeclarations ctx data.Pos lineStr )
            path "/methods"          >=> positionHandler (fun data ctx _ lines   -> Commands.methods ctx data.Pos lines )
            path "/compilerlocation" >=> (fun httpCtx -> async {
                let! res, state' = Commands.compilerLocation writeJson state
                state <- state'
                let res' = res |> List.toArray |> Json.toJson
                return! Response.response HttpCode.HTTP_200 res' httpCtx
                })
            path "/lint" >=> handler (fun (data: LintRequest) -> Commands.lint writeJson state checker data.FileName)
        ]


    let port =
        try
            int argv.[0]
        with
        _ -> 8088

    let defaultBinding = defaultConfig.bindings.[0]
    let withPort = { defaultBinding.socketBinding with port = uint16 port }
    let serverConfig =
        { defaultConfig with bindings = [{ defaultBinding with socketBinding = withPort }]}
    startWebServer serverConfig app
    0
