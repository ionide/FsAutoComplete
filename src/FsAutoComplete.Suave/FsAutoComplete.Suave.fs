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
    let commands = Commands(writeJson)
    let currentFiles = ref Map.empty
    let originalFs = AbstractIL.Internal.Library.Shim.FileSystem
    let fs = new FileSystem(originalFs, fun () -> !currentFiles)
    AbstractIL.Internal.Library.Shim.FileSystem <- fs

    let handler f : WebPart = fun (r : HttpContext) -> async {
          let data = r.request |> getResourceFromReq
          let! res = f data
          let res' = res |> List.toArray |> Json.toJson
          return! Response.response HttpCode.HTTP_200 res' r
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
                  let tyResOpt = commands.TryGetRecentTypeCheckResultsForFile(file, options)
                  match tyResOpt with
                  | None -> async.Return ([CommandResponse.info writeJson "Cached typecheck results not yet available"])
                  | Some tyRes -> f data tyRes lineStr lines
            let res' = res |> List.toArray |> Json.toJson
            return! Response.response HttpCode.HTTP_200 res' r
        } 

    let app =
        Writers.setMimeType "application/json; charset=utf-8" >=>
        POST >=>
        choose [
            path "/parse" >=> handler (fun (data : ParseRequest) -> commands.Parse data.FileName data.Lines)
            //TODO: Add filewatcher
            path "/project" >=> handler (fun (data : ProjectRequest) -> commands.Project data.FileName DateTime.Now false)
            path "/declarations" >=> handler (fun (data : DeclarationsRequest) -> commands.Declarations data.FileName)
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
                        | Some tyRes -> return! commands.Completion tyRes { Line = data.Line; Col = data.Column } lineStr (Some data.Filter)
                })
            path "/tooltip" >=> positionHandler (fun data tyRes lineStr _ -> commands.ToolTip tyRes { Line = data.Line; Col = data.Column } lineStr)
            path "/symboluseproject" >=> positionHandler (fun data tyRes lineStr _ -> commands.SymbolUseProject tyRes { Line = data.Line; Col = data.Column } lineStr)
            path "/symboluse" >=> positionHandler (fun data tyRes lineStr _ -> commands.SymbolUse tyRes { Line = data.Line; Col = data.Column } lineStr)
            path "/finddeclaration" >=> positionHandler (fun data tyRes lineStr _ -> commands.FindDeclarations tyRes { Line = data.Line; Col = data.Column } lineStr)
            path "/methods" >=> positionHandler (fun data tyRes _ lines   -> commands.Methods tyRes { Line = data.Line; Col = data.Column } lines)
            path "/compilerlocation" >=> fun httpCtx -> 
                async {
                    let res = commands.CompilerLocation() |> List.toArray |> Json.toJson
                    return! Response.response HttpCode.HTTP_200 res httpCtx
                }
            path "/lint" >=> handler (fun (data: LintRequest) -> commands.Lint data.FileName)
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
