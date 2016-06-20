module FsAutoComplete.Suave

open System
open System.IO

open Suave
open Suave.Http
open Suave.Operators
open Suave.Successful
open Suave.Web
open Suave.WebPart
open Suave.Filters
open Newtonsoft.Json
open Microsoft.FSharp.Compiler

open FsAutoComplete
open FsAutoComplete.JsonSerializer

[<AutoOpen>]
module Contract =
    type ParseRequest = { FileName : string; IsAsync : bool; Lines : string[]}
    type ProjectRequest = { FileName : string;}
    type DeclarationsRequest = {FileName : string}
    type HelptextRequest = {Symbol : string}
    type CompletionRequest = {FileName : string; SourceLine : string; Line : int; Column : int; Filter : string}
    type PositionRequest = {FileName : string; Line : int; Column : int; Filter : string}
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
    let state = ref FsAutoComplete.State.Initial
    let checker = new FSharpCompilerServiceChecker()

    let currentFiles = ref Map.empty
    let originalFs = AbstractIL.Internal.Library.Shim.FileSystem
    let fs = new FileSystem(originalFs, fun () -> !currentFiles)
    AbstractIL.Internal.Library.Shim.FileSystem <- fs

    let handler f : WebPart = fun (r : HttpContext) -> async {
          let data = r.request |> getResourceFromReq
          let! (res,state') = f data
          state := state'
          let res' = res |> List.toArray |> Json.toJson
          return! Response.response HttpCode.HTTP_200 res' r
        }

    let positionHandler (f : PositionRequest -> ParseAndCheckResults -> string -> string [] -> Async<string list * State>) : WebPart = fun (r : HttpContext) -> async {
        let data = r.request |> getResourceFromReq<PositionRequest>
        let file = Path.GetFullPath data.FileName
        let! (res, (state' : State)) =
            match (!state).TryGetFileCheckerOptionsWithLinesAndLineStr(file, data.Line, data.Column) with
            | Failure s -> async { return [CommandResponse.error writeJson (s)], !state }
            | Success (options, lines, lineStr) ->
              // TODO: Should sometimes pass options.Source in here to force a reparse
              //       for completions e.g. `(some typed expr).$`
              let tyResOpt = checker.TryGetRecentTypeCheckResultsForFile(file, options)
              match tyResOpt with
              | None -> async { return [ CommandResponse.info writeJson "Cached typecheck results not yet available"], !state }
              | Some tyRes -> f data tyRes lineStr lines
        state := state'
        let res' = res |> List.toArray |> Json.toJson
        return! Response.response HttpCode.HTTP_200 res' r
    } 
 
    let app =
        Writers.setMimeType "application/json; charset=utf-8" >=>
        POST >=>
        choose [
            path "/parse" >=> handler (fun (data : ParseRequest) -> Commands.parse writeJson !state checker data.FileName data.Lines)
            //TODO: Add filewatcher
            path "/project" >=> handler (fun (data : ProjectRequest) -> Commands.project writeJson !state checker data.FileName DateTime.Now false)
            path "/declarations" >=> handler (fun (data : DeclarationsRequest) -> Commands.declarations writeJson !state checker data.FileName)
            path "/helptext" >=> handler (fun (data : HelptextRequest) -> Commands.helptext writeJson !state checker data.Symbol)
            path "/completion" >=> handler (fun (data : CompletionRequest) ->  async {
                let file = Path.GetFullPath data.FileName
                let! (res, (state' : State)) =
                    match (!state).TryGetFileCheckerOptionsWithLines(file) with
                    | Failure s -> async { return [CommandResponse.error writeJson (s)], !state }
                    | Success (options, lines) ->
                    let line = data.Line
                    let col = data.Column
                    let lineStr = data.SourceLine
                    let ok = line <= lines.Length && line >= 1 && col <= lineStr.Length + 1 && col >= 1
                    if not ok then
                        async { return [CommandResponse.error writeJson "Position is out of range"], !state}
                    else                    
                        let tyResOpt = checker.TryGetRecentTypeCheckResultsForFile(file, options)
                        match tyResOpt with
                        | None -> async { return [ CommandResponse.info writeJson "Cached typecheck results not yet available"], !state }
                        | Some tyRes -> Commands.completion writeJson !state checker tyRes data.Line data.Column lineStr (Some data.Filter)
                return res,state'
                })
            path "/tooltip" >=> positionHandler (fun data tyRes lineStr _ ->  Commands.toolTip writeJson !state checker tyRes data.Line data.Column lineStr )
            path "/symboluseproject" >=> positionHandler (fun data tyRes lineStr _ ->  Commands.symbolUseProject writeJson !state checker tyRes data.FileName data.Line data.Column lineStr )
            path "/symboluse" >=> positionHandler (fun data tyRes lineStr _ ->  Commands.symbolUse writeJson !state checker tyRes data.Line data.Column lineStr )
            path "/finddeclaration" >=> positionHandler (fun data tyRes lineStr _ ->  Commands.findDeclarations writeJson !state checker tyRes data.Line data.Column lineStr )
            path "/methods" >=> positionHandler (fun data tyRes _ lines ->  Commands.methods writeJson !state checker tyRes data.Line data.Column lines )
            path "/compilerlocation" >=> (fun r -> async {
                let! (res,state') = Commands.compilerLocation writeJson !state checker 
                state := state'
                let res' = res |> List.toArray |> Json.toJson
                return! Response.response HttpCode.HTTP_200 res' r
                })
            path "/lint" >=> handler (fun (data: LintRequest) -> Commands.lint writeJson !state checker data.FileName)
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
    0 // return an integer exit code
