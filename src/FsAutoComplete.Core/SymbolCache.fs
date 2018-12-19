module SymbolCache

open System
open System.Diagnostics
open FsAutoComplete
open FSharp.Compiler.SourceCodeServices

open System.Net
open System.IO
open Newtonsoft.Json

let makePostRequest (url : string) (requestBody : string) =
    let req = WebRequest.CreateHttp url
    req.CookieContainer <- new CookieContainer()
    req.Method <- "POST"
    req.ProtocolVersion <- HttpVersion.Version10
    let postBytes = requestBody |> System.Text.Encoding.ASCII.GetBytes
    req.ContentLength <- postBytes.LongLength
    req.ContentType <- "application/json; charset=utf-8"
    async{
        use! reqStream = req.GetRequestStreamAsync() |> Async.AwaitTask
        do! reqStream.WriteAsync(postBytes, 0, postBytes.Length) |> Async.AwaitIAsyncResult |> Async.Ignore
        reqStream.Close()
        use! res = req.AsyncGetResponse()
        use stream = res.GetResponseStream()
        use reader = new StreamReader(stream)
        let! rdata = reader.ReadToEndAsync() |> Async.AwaitTask
        return rdata
    }

let mutable port = 0

[<CLIMutable>]
type SymbolUseRange = {
    FileName: string
    StartLine: int
    StartColumn: int
    EndLine: int
    EndColumn: int
    IsFromDefinition: bool
    IsFromAttribute : bool
    IsFromComputationExpression : bool
    IsFromDispatchSlotImplementation : bool
    IsFromPattern : bool
    IsFromType : bool
    SymbolFullName: string
    SymbolDisplayName: string
    SymbolIsLocal: bool
}

type SymbolCacheRequest = {
    Filename: string
    Uses: SymbolUseRange[]
}
let p =
    let t = typeof<SymbolCacheRequest>
    Path.GetDirectoryName t.Assembly.Location

let pid =
    Process.GetCurrentProcess().Id


let startCache (dir : string) =
    port <- Random().Next(9000,9999)
    let si = ProcessStartInfo()
    si.RedirectStandardOutput <- true
    si.RedirectStandardError <- true
    si.UseShellExecute <- false

    #if DOTNET_SPAWN
    si.FileName <- "dotnet"
    si.Arguments <- Path.Combine(p, "fsautocomplete.symbolcache.dll") + " " + (string port) + " " + (string pid) + " " + (if dir.Contains " " then sprintf "\"%s\"" dir else dir)
    #else
    if Utils.runningOnMono then
        si.FileName <- "mono"
        si.Arguments <- Path.Combine(p, "fsautocomplete.symbolcache.exe") + " " + (string port) + " " + (string pid) + " " + (if dir.Contains " " then sprintf "\"%s\"" dir else dir)
    else
        si.FileName <- Path.Combine(p, "fsautocomplete.symbolcache.exe")
        si.Arguments <- (string port) + " " + (string pid) + " " + (if dir.Contains " " then sprintf "\"%s\"" dir else dir)
    #endif

    let proc = Process.Start(si)
    // proc.OutputDataReceived.Add (fun e -> fprintf stderr "[Symbol Cache]: %s" e.Data)
    // proc.ErrorDataReceived.Add (fun e -> fprintf stderr  "[Symbol Cache]: %s" e.Data)
    proc.BeginOutputReadLine();
    proc.BeginErrorReadLine();
    ()


let fromSymbolUse (su : FSharpSymbolUse) =
    {   StartLine = su.RangeAlternate.StartLine
        StartColumn = su.RangeAlternate.StartColumn + 1
        EndLine = su.RangeAlternate.EndLine
        EndColumn = su.RangeAlternate.EndColumn + 1
        FileName = su.FileName
        IsFromDefinition = su.IsFromDefinition
        IsFromAttribute = su.IsFromAttribute
        IsFromComputationExpression = su.IsFromComputationExpression
        IsFromDispatchSlotImplementation = su.IsFromDispatchSlotImplementation
        IsFromPattern = su.IsFromPattern
        IsFromType = su.IsFromType
        SymbolFullName = su.Symbol.FullName
        SymbolDisplayName = su.Symbol.DisplayName
        SymbolIsLocal = su.Symbol.IsPrivateToFile  }


let sendSymbols (serializer: Serializer) fn (symbols: FSharpSymbolUse[]) =
    let request =
        symbols
        |> Array.map(fromSymbolUse)
        |> fun n -> {Filename = fn; Uses = n}
        |> serializer

    try
        makePostRequest ("http://localhost:" + (string port) + "/updateSymbols") request
        |> Async.Ignore
        |> Async.Start
    with _ -> ()
    ()

let getSymbols symbolName =
    makePostRequest ("http://localhost:" + (string port) + "/getSymbols") symbolName
    |> Async.map (fun n ->
        try
            Some <| JsonConvert.DeserializeObject<SymbolUseRange[]> n
        with
        | _ -> None)

let getImplementation symbolName =
    makePostRequest ("http://localhost:" + (string port) + "/getImplementation") symbolName

let buildProjectCache (serializer: Serializer) (opts: FSharpProjectOptions) =
    opts
    |> serializer
    |> makePostRequest ("http://localhost:" + (string port) + "/buildCacheForProject")
    |> Async.Ignore
