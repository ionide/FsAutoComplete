module FsAutoComplete.Lsp
(*
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

let start (commands: Commands) (args: ParseResults<Options.CLIArguments>) =
    use input = Console.OpenStandardInput()
    use output = Console.OpenStandardOutput()
    //FSharpLanguageServer.start input output
    ()