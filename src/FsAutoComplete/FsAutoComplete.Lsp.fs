module FsAutoComplete.Lsp

module JsonRpc =
    open Newtonsoft.Json
    open Newtonsoft.Json.Linq

    type Request = {
        [<JsonProperty("jsonrpc")>] Version: string
        Id: int option
        Method: string
        Params: JToken option
    }
        with
            static member Create(id: int, method: string, rpcParams: JToken) =
                { Version = "2.0"; Id = Some id; Method = method; Params = Some rpcParams }

            static member Create(method: string, rpcParams: JToken) =
                { Version = "2.0"; Id = None; Method = method; Params = Some rpcParams }

    type Error = {
        Code: int
        Message: string
        Data: JToken option
    }

    type Response = {
        [<JsonProperty("jsonrpc")>] Version: string
        Id: int option
        Error: Error option
        Result: JToken option
    }

module LanguageServerProtocol =
    module LowLevel =
        open System
        open System.IO
        open System.Text

        let rec private readHeaders (reader: TextReader) =
            let line = reader.ReadLine()
            if line <> "" then
                let separatorPos = line.IndexOf(": ")
                if separatorPos = -1 then
                    failwithf "Separator not found in header '%s'" line
                else
                    let name = line.Substring(0, separatorPos)
                    let value = line.Substring(separatorPos + 2)
                    (name,value) :: (readHeaders reader)
            else
                []

        let read (stream: Stream) =
            let asciiReader = new StreamReader(stream, Encoding.ASCII, false, 1024 * 1024, true)
            let headers = readHeaders asciiReader
            asciiReader.Dispose()

            let contentLength =
                headers
                |> List.tryFind(fun (name, _) -> name = "Content-Length")
                |> Option.map snd
                |> Option.bind (fun s -> match Int32.TryParse(s) with | true, x -> Some x | _ -> None)

            if contentLength = None then
                failwithf "Content-Length header not found"
            else
                let result = Array.zeroCreate<byte> contentLength.Value
                stream.Read(result, 0, contentLength.Value) |> ignore
                headers, Encoding.UTF8.GetString(result, 0, contentLength.Value)

        let write (stream: Stream) (data: string) =
            let asciiWriter = new StreamWriter(stream, Encoding.ASCII, 1024 * 1024, true)
            let bytes = Encoding.UTF8.GetBytes(data)
            asciiWriter.Write("Content-Length: ")
            asciiWriter.WriteLine(string bytes.Length)
            asciiWriter.WriteLine("Content-Type: utf-8")
            asciiWriter.WriteLine();
            asciiWriter.Dispose()

            stream.Write(bytes, 0, bytes.Length)

    module Server =
        type InitializeParams = {
            ProcessId: int option
            RootPath: string option
            trace: string option
        }

        type ServerCapabilities = {
            HoverProvider: bool option
        }

        type InitializeResult = {
            capabilities
        }

        open System.IO
        let start (input: Stream) (output: Stream) (h
            ()

module FSharpLanguageServer =
    open System.IO

    let start (input: Stream) (output: Stream) =
        ()
*)
open Argu
open System
open OmniSharp.Extensions.LanguageServer.Server
open Microsoft.Extensions.Logging
open FSharp.Control.Tasks
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open System.IO
open System.Threading.Tasks
open OmniSharp.Extensions.LanguageServer.Protocol
open OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities
open System.Threading
open OmniSharp.Extensions.LanguageServer.Protocol.Server.Capabilities
open OmniSharp.Extensions.JsonRpc

let start (commands: Commands) (args: ParseResults<Options.CLIArguments>) =
    use input = Console.OpenStandardInput()
    use output = Console.OpenStandardOutput()
    let server = new LanguageServer(input, output, new LoggerFactory());

    let initialize (p: InitializeParams ) =
        task {
            match p.RootPath with
            | null -> ()
            | rootPath ->
                let projects = Directory.EnumerateFiles(rootPath, "*.fsproj", SearchOption.AllDirectories)
                for project in projects do
                    let! _ = commands.Project project false ignore |> Async.StartAsTask
                    ()
                ()
            ()
        } :> Task

    let filter = DocumentFilter()
    filter.Language <- "fsharp"
    let selector = DocumentSelector(filter)

    server.OnInitialize(InitializeDelegate(initialize)) |> ignore
(*
    server.AddHandler(
        { new IHoverHandler with
            member this.Handle(param: TextDocumentPositionParams, token: CancellationToken): Task<Hover> =
                task { return null }

            member this.GetRegistrationOptions(): TextDocumentRegistrationOptions =
                let opts = TextDocumentRegistrationOptions()
                let filter = DocumentFilter()
                filter.Language <- "fsharp"
                opts.DocumentSelector <- DocumentSelector(filter)
                opts

            member this.SetCapability(capability: HoverCapability) =
                ()

        }) |> ignore
*)
    let z =
        { new ITextDocumentSyncHandler with
            member __.Options with get(): TextDocumentSyncOptions =
                let opts = TextDocumentSyncOptions()
                opts.WillSave <- true
                opts.Change <- TextDocumentSyncKind.Full
                let saveOpts = SaveOptions()
                saveOpts.IncludeText <- true
                opts.Save <- saveOpts
                opts.OpenClose <- true
                opts
            member __.GetTextDocumentAttributes(uri: Uri) =
                new TextDocumentAttributes(uri, "fsharp")
           (* member this.SetCapability(capability: SynchronizationCapability) =
                ()*)
          interface ICapability<SynchronizationCapability> with
            member __.SetCapability(_capability: SynchronizationCapability) =
                ()
          interface IDidOpenTextDocumentHandler with
            member __.Handle(_param: DidOpenTextDocumentParams): Task =
                Task.CompletedTask
          interface IRegistration<TextDocumentRegistrationOptions> with
            member __.GetRegistrationOptions() =
                let opts = TextDocumentRegistrationOptions()
                opts.DocumentSelector <- selector
                opts
          interface IRegistration<TextDocumentSaveRegistrationOptions> with
            member __.GetRegistrationOptions() =
                let opts = TextDocumentSaveRegistrationOptions()
                opts.DocumentSelector <- selector
                opts
          interface IRegistration<TextDocumentChangeRegistrationOptions> with
            member __.GetRegistrationOptions() =
                let opts = TextDocumentChangeRegistrationOptions()
                opts.DocumentSelector <- selector
                opts
          interface IJsonRpcHandler
          interface IDidCloseTextDocumentHandler with
            member this.Handle(param: DidCloseTextDocumentParams): Task =
                Task.CompletedTask
          interface IDidSaveTextDocumentHandler with
            member this.Handle(param: DidSaveTextDocumentParams): Task =
                Task.CompletedTask
          interface IDidChangeTextDocumentHandler with
            member this.Handle(param: DidChangeTextDocumentParams): Task =
                Task.CompletedTask
        }

    let t = task {
        do! server.Initialize()
        do! server.WaitForExit
    }
    t.Wait()
    //FSharpLanguageServer.start input output
    ()