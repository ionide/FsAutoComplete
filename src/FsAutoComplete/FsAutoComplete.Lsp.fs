module FsAutoComplete.Lsp

let dbgf format = Printf.ksprintf (fun s -> System.Diagnostics.Trace.WriteLine(s)) format

module JsonConverters =
    open Newtonsoft.Json
    open Newtonsoft.Json.Serialization
    open Microsoft.FSharp.Reflection
    open System.Reflection
    open System.Collections.Generic
    open System

    module private ConverterHelpers =
        let inline stringEq (a:string) (b:string) =
            a.Equals(b, System.StringComparison.OrdinalIgnoreCase)

        let inline isOptionType (t:System.Type) =
           t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>>

        let inline toCamel (name:string) =
            if System.Char.IsLower (name, 0) then name
            else string(System.Char.ToLower name.[0]) + name.Substring(1)

        let inline memorise f =
            let d = Dictionary<_, _>()
            fun key ->
                match d.TryGetValue(key) with
                | (true, v) -> v
                | (false, _) ->
                    let result = f key
                    d.[key] <- result
                    result

        let getUnionCaseFieldsMemorised = memorise FSharpValue.PreComputeUnionReader
        let getUnionTagMemorised = memorise FSharpValue.PreComputeUnionTagReader
        let getUnionCasesByTagMemorised = memorise (fun t -> FSharpType.GetUnionCases(t) |> Array.map (fun x -> x.Tag, x) |> dict)

        let getUnionCasesMemorised = memorise (fun (t: Type) -> FSharpType.GetUnionCases t)

        let getUnionTagOfValueMemorised v =
            let t = v.GetType()
            getUnionTagMemorised t v

        let inline getUnionFieldsMemorised v =
            let cases = getUnionCasesByTagMemorised (v.GetType())
            let tag = getUnionTagOfValueMemorised v
            let case = cases.[tag]
            let unionReader = getUnionCaseFieldsMemorised case
            (case, unionReader v)

        let SomeFieldIdentifier = "Some"

        /// Determine if a given type has a field named 'Some' which would cause
        /// ambiguity if nested under an option type without being boxed
        let hasFieldNamedSome =
            memorise
                (fun (t:System.Type) ->
                    isOptionType t // the option type itself has a 'Some' field
                    || (FSharpType.IsRecord t && FSharpType.GetRecordFields t |> Seq.exists (fun r -> stringEq r.Name SomeFieldIdentifier))
                    || (FSharpType.IsUnion t && FSharpType.GetUnionCases t |> Seq.exists (fun r -> stringEq r.Name SomeFieldIdentifier)))

    open ConverterHelpers

    /// Serializers for F# discriminated unions improving upon the stock implementation by JSon.Net
    /// The default formatting used by Json.Net to serialize F# discriminated unions
    /// and Option types is too verbose. This module implements a more succinct serialization
    /// for those data types.
    type CompactUnionJsonConverter() =
        inherit Newtonsoft.Json.JsonConverter()

        let canConvertMemorised =
            memorise
                (fun objectType ->
                    // Include F# discriminated unions
                    FSharpType.IsUnion objectType
                    // and exclude the standard FSharp lists (which are implemented as discriminated unions)
                    && not (objectType.GetTypeInfo().IsGenericType && objectType.GetGenericTypeDefinition() = typedefof<_ list>))

        override __.CanConvert(objectType:System.Type) = canConvertMemorised objectType

        override __.WriteJson(writer:JsonWriter, value:obj, serializer:JsonSerializer) =
            let t = value.GetType()

            let convertName =
                match serializer.ContractResolver with
                | :? CamelCasePropertyNamesContractResolver -> toCamel
                | _ -> id

            // Option type?
            if isOptionType t then
                let cases = getUnionCasesMemorised t
                let none, some = cases.[0], cases.[1]

                let case, fields = getUnionFieldsMemorised value

                if case = none then
                    () // None is serialized as just null

                // Some _
                else
                    // Handle cases `Some None` and `Some null`
                    let innerType = some.GetFields().[0].PropertyType
                    let innerValue = fields.[0]
                    if isNull innerValue then
                        writer.WriteStartObject()
                        writer.WritePropertyName(convertName SomeFieldIdentifier)
                        writer.WriteNull()
                        writer.WriteEndObject()
                    // Some v with v <> null && v <> None
                    else
                        // Is it nesting another option: `(e.g., "Some (Some ... Some ( ... )))"`
                        // or any other type with a field named 'Some'?
                        if hasFieldNamedSome innerType then
                            // Preserved the nested structure through boxing
                            writer.WriteStartObject()
                            writer.WritePropertyName(convertName SomeFieldIdentifier)
                            serializer.Serialize(writer, innerValue)
                            writer.WriteEndObject()
                        else
                            // Type is option<'a> where 'a does not have a field named 'Some
                            // (and therfore in particular is NOT an option type itself)
                            // => we can simplify the Json by omitting the `Some` boxing
                            // and serializing the nested object directly
                            serializer.Serialize(writer, innerValue)
            // Discriminated union
            else
                let case, fields = getUnionFieldsMemorised value

                match fields with
                // Field-less union case
                | [||] ->
                    writer.WriteValue(convertName (sprintf "%A" value))
                // Case with single field
                | [|onefield|] ->
                    writer.WriteStartObject()
                    writer.WritePropertyName(convertName case.Name)
                    serializer.Serialize(writer, onefield)
                    writer.WriteEndObject()
                // Case with list of fields
                | _ ->
                    writer.WriteStartObject()
                    writer.WritePropertyName(convertName case.Name)
                    serializer.Serialize(writer, fields)
                    writer.WriteEndObject()

        override __.ReadJson(reader:JsonReader, objectType:System.Type, existingValue:obj, serializer:JsonSerializer) =
            let cases = FSharpType.GetUnionCases(objectType)
            // Option type?
            if isOptionType objectType then
                let caseNone, caseSome = cases.[0], cases.[1]
                let jToken = Linq.JToken.ReadFrom(reader)
                // Json Null maps to `None`
                if jToken.Type = Linq.JTokenType.Null then
                    FSharpValue.MakeUnion(caseNone, [||])

                // Json that is not null must map to `Some _`
                else
                    let nestedType = objectType.GetTypeInfo().GetGenericArguments().[0]

                    // Try to retrieve the 'Some' attribute:
                    // if the specified Json an object of the form `{ "Some" = token }`
                    // then return `Some token`, otherwise returns `None`.
                    let tryGetSomeAttributeValue (jToken:Linq.JToken) =
                        if jToken.Type = Linq.JTokenType.Object then
                            let jObject = jToken :?> Linq.JObject
                            match jObject.TryGetValue (SomeFieldIdentifier, System.StringComparison.OrdinalIgnoreCase) with
                            | true, token -> Some token
                            | false, _ -> None
                        else
                            None

                    let nestedValue =
                        match tryGetSomeAttributeValue jToken with
                        | Some someAttributeValue when someAttributeValue.Type = Linq.JTokenType.Null ->
                            // The Json object is { "Some" : null } for type option<'a>
                            // where 'a is nullable => deserialized to `Some null`
                            null

                        | Some someAttributeValue when hasFieldNamedSome nestedType ->
                            // Case of Json { "Some" : <obj> } where <obj> is not null
                            // => we just deserialize the nested object recursively
                            someAttributeValue.ToObject(nestedType, serializer)

                        | Some someAttributeValue ->
                            failwithf "Unexpected 'Some' Json attribute. Attribute value: %O" someAttributeValue

                        | None when hasFieldNamedSome nestedType ->
                            failwith "Types with a field named 'Some' and nested under an option type must be boxed under a 'Some' attribute when serialized to Json."

                        | None ->
                            // type is option<'a> where 'a is not an option type and not a
                            // type that would be serialized as a Json object.
                            // i.e. 'a is either a base Json type (e.g. integer or string) or
                            // a Json array.
                            // This means that the Json is not boxed under the `Some` attribute and we can therefore
                            // deserialize the object of type 'a directly without performing any unboxing.
                            jToken.ToObject(nestedType, serializer)

                    FSharpValue.MakeUnion(caseSome, [| nestedValue |])

            // Discriminated union
            else
                // There are three types of union cases:
                //      | Case1 | Case2 of 'a | Case3 of 'a1 * 'a2 ... * 'an
                // Those are respectively serialized to Json as
                //    "Case1"
                //    { "Case2" : value }
                //    { "Case3" : [v1, v2, ... vn] }

                // Load JObject from stream
                let jToken = Linq.JToken.Load(reader)

                if isNull jToken then
                    null

                // Type1: field-less union case
                elif jToken.Type = Linq.JTokenType.String then
                    let caseName = jToken.ToString()
                    let matchingCase =
                        cases
                        |> Array.tryFind (fun case -> stringEq case.Name caseName && (case.GetFields() |> Array.isEmpty))
                    match matchingCase with
                    | Some case -> FSharpValue.MakeUnion(case,[||])
                    | None ->
                        failwithf "Cannot parse DU field-less value: %O. Expected names: %O" caseName (System.String.Join(", ", cases |> Seq.map(fun c->c.Name)))

                // Type 2 or 3: Case with fields
                elif jToken.Type = Linq.JTokenType.Object then
                    let jObject = jToken :?> Linq.JObject
                    let jObjectProperties = jObject.Properties()
                    if Seq.length jObjectProperties <> 1 then
                        failwith "Incorrect Json format for discriminated union. A DU value with fields must be serialized to a Json object with a single Json attribute"

                    let caseProperty = jObjectProperties |> Seq.head
                    /// Lookup the DU case by name
                    let matchingCase =
                        cases
                        |> Seq.tryFind (fun case -> stringEq case.Name caseProperty.Name)

                    match matchingCase with
                    | None ->
                        failwithf "Case with fields '%s' does not exist for discriminated union %s" caseProperty.Name objectType.Name

                    // Type 2: A union case with a single field: Case2 of 'a
                    | Some case when case.GetFields().Length = 1 ->
                        let fieldType = case.GetFields().[0].PropertyType
                        let field = caseProperty.Value.ToObject(fieldType, serializer)
                        FSharpValue.MakeUnion(case, [|field|])

                    // Type 3: A union case with more than one field: Case3 of 'a1 * 'a2 ... * 'an
                    | Some case ->
                        // Here there could be an ambiguity:
                        // the Json values are either the fields of the case
                        // or if the array is Use target type to resolve ambiguity
                        let fields =
                            case.GetFields()
                            |> Seq.zip caseProperty.Value
                            |> Seq.map (fun (v,t) -> v.ToObject(t.PropertyType, serializer))
                            |> Seq.toArray
                        FSharpValue.MakeUnion(case, fields)
                else
                    failwithf "Unexpected Json token type %O: %O" jToken.Type jToken

    let getSettings() =
        let s = JsonSerializerSettings(NullValueHandling = NullValueHandling.Ignore)
        s.Converters.Add(CompactUnionJsonConverter())
        s

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
        static member Create(id: int, method': string, rpcParams: JToken) =
            { Version = "2.0"; Id = Some id; Method = method'; Params = Some rpcParams }

        static member Create(method': string, rpcParams: JToken) =
            { Version = "2.0"; Id = None; Method = method'; Params = Some rpcParams }

    type Error = {
        Code: int
        Message: string
        Data: JToken option
    }
    with
        static member Create(code: int, message: string) =
            { Code = code; Message = message; Data = None }

    type Response = {
        [<JsonProperty("jsonrpc")>] Version: string
        Id: int option
        Error: Error option
        Result: JToken option
    }
    with
        static member Success(id: int, result: JToken) =
            { Version = "2.0"; Id = Some id; Result = Some result; Error = None }
        static member Failure(id: int option, error: Error) =
            { Version = "2.0"; Id = id; Result = None; Error = Some error }

module LanguageServerProtocol =
    module LowLevel =
        open System
        open System.IO
        open System.Text

        let headerBufferSize = 300
        let minimumHeaderLength = 21
        let cr = byte '\r'
        let lf = byte '\f'
        let headerEncoding = Encoding.ASCII

        let private readLine (stream: Stream) =
            let buffer = Array.zeroCreate<byte> headerBufferSize
            let mutable count = stream.Read(buffer, 0, 2)
            if count < 2 then
                None
            else
                // TODO: Check that we don't over-fill headerBufferSize
                while count < headerBufferSize && (buffer.[count-2] <> cr && buffer.[count-1] <> lf) do
                     let additionalBytesRead  = stream.Read(buffer, count, 1)
                     // TODO: exit when additionalBytesRead = 0, end of stream
                     count <- count + additionalBytesRead

                if count >= headerBufferSize then
                    None
                else
                    Some (headerEncoding.GetString(buffer, 0, count - 2))

        let rec private readHeaders (stream: Stream) =
            let line = readLine stream
            match line with
            | Some "" ->  []
            | Some line ->
                let separatorPos = line.IndexOf(": ")
                if separatorPos = -1 then
                    failwithf "Separator not found in header '%s'" line
                else
                    let name = line.Substring(0, separatorPos)
                    let value = line.Substring(separatorPos + 2)
                    (name,value) :: (readHeaders stream)
            | None ->
                raise (EndOfStreamException())

        let read (stream: Stream) =
            dbgf "Starting READ"
            let headers = readHeaders stream

            dbgf "Headers = %A" headers
            let contentLength =
                headers
                |> List.tryFind(fun (name, _) -> name = "Content-Length")
                |> Option.map snd
                |> Option.bind (fun s -> match Int32.TryParse(s) with | true, x -> Some x | _ -> None)

            if contentLength = None then
                dbgf "Content-Length header not found"
                failwithf "Content-Length header not found"
            else
                dbgf "Reading %d bytes of data" contentLength.Value
                let result = Array.zeroCreate<byte> contentLength.Value
                let readCount = stream.Read(result, 0, contentLength.Value)
                dbgf "Read %d bytes" readCount
                let str = Encoding.UTF8.GetString(result, 0, readCount)
                dbgf "Read '%s'" str
                headers, str

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
        open System.IO
        open Newtonsoft.Json
        open Newtonsoft.Json.Converters

        type TextDocumentSyncKind =
            | None = 0
            | Full = 1
            | Incremental = 2

        type TraceSetting =
            | Off = 0
            | Messages = 1
            | Verbose = 2

        type InitializeParams = {
            ProcessId: int option
            RootPath: string option
            [<JsonConverter(typeof<StringEnumConverter>, true)>]
            trace: string option
        }

        type ServerCapabilities = {
            HoverProvider: bool option
            TextDocumentSync: TextDocumentSyncKind option
        }
        with
            static member Default =
                {
                    HoverProvider = None
                    TextDocumentSync = None
                }

        type InitializeResult = {
            Capabilities: ServerCapabilities
        }
        with
            static member Default =
                {
                    Capabilities = ServerCapabilities.Default
                }

        type Request =
            | Initialize of InitializeParams
            | Shutdown
            | Exit

        type Response =
            | NoResponse
            | InvalidRequest of string
            | UnhandledRequest
            | InitializeResponse of InitializeResult

        with
            member this.AsJsonSerializable
                with get() =
                    match this with
                    | InitializeResponse x -> box x
                    | InvalidRequest _
                    | UnhandledRequest -> failwith "Technical responses can't be sent as JSON"
(*
        let start (input: Stream) (output: Stream)
            ()*)

module FSharpLanguageServer =
    open System.IO
    open Newtonsoft.Json
    open Newtonsoft.Json.Linq
    open Newtonsoft.Json.Serialization
    open LanguageServerProtocol
    open LanguageServerProtocol.Server

    let jsonSettings =
        let result = JsonConverters.getSettings()
        result.ContractResolver <- CamelCasePropertyNamesContractResolver()
        result

    let jsonSerializer = JsonSerializer.Create(jsonSettings)

    let private parseRequest<'a> (f: 'a -> Request) (paramsToken: JToken option) =
        paramsToken
        |> Option.map (fun paramsToken ->
            paramsToken.ToObject<'a>(jsonSerializer) |> f)

    let private parseEmpty (req: Request) (_params: JToken option) =
        Some req

    let requestParser = function
    | "initialize" -> parseRequest<InitializeParams> Initialize
    | "shutdown" -> parseEmpty Shutdown
    | "exit" -> parseEmpty Exit
    | _ -> fun _ -> None

    let start (input: Stream) (output: Stream) (handler: Request -> Response) =
        dbgf "Starting up !"
        let jsonSettings = JsonConverters.getSettings()
        jsonSettings.ContractResolver <- CamelCasePropertyNamesContractResolver()

        while true do
            try
                let _, rpcRequestString = LowLevel.read input
                dbgf "Received: %s" rpcRequestString

                let rpcRequest = JsonConvert.DeserializeObject<JsonRpc.Request>(rpcRequestString, jsonSettings)
                let request = requestParser rpcRequest.Method rpcRequest.Params
                dbgf "Parsed as: %A" request

                match request with
                | Some request ->
                    let response = handler request
                    dbgf "Will answer: %A" response
                    let rpcResponse =
                        match response with
                        | NoResponse -> None
                        | InvalidRequest message ->
                            JsonRpc.Response.Failure(rpcRequest.Id, JsonRpc.Error.Create(-32602, message)) |> Some
                        | UnhandledRequest ->
                            JsonRpc.Response.Failure(rpcRequest.Id, JsonRpc.Error.Create(-32601, "Method not found")) |> Some
                        | response ->
                            let serializedResponse = JToken.FromObject(response.AsJsonSerializable, jsonSerializer)
                            JsonRpc.Response.Success(rpcRequest.Id.Value, serializedResponse) |> Some
                    match rpcResponse with
                    | Some rpcResponse ->
                        let rpcResponseString = JsonConvert.SerializeObject(rpcResponse, jsonSettings)
                        dbgf "Response: %s" rpcResponseString
                        LowLevel.write output rpcResponseString
                    | _ -> ()
                | _ ->
                    dbgf "Unable to parse request: %s" rpcRequestString
            with
            | :? EndOfStreamException ->
                dbgf "Client closed the input stream"
                System.Environment.Exit(0)
            | ex -> dbgf "%O" ex
        ()

open Argu
open System
open LanguageServerProtocol.Server
open System.Diagnostics

let start (commands: Commands) (_args: ParseResults<Options.CLIArguments>) =
    Trace.Listeners.Clear()

    System.IO.File.WriteAllText(@"C:\temp\fsac.txt", "")
    let twtl = new TextWriterTraceListener(@"C:\temp\fsac.txt")
    twtl.Name <- "TextLogger";
    twtl.TraceOutputOptions <- TraceOptions.ThreadId ||| TraceOptions.DateTime;

    Trace.Listeners.Add(twtl);
    Trace.AutoFlush <- true;

    use input = Console.OpenStandardInput()
    use output = Console.OpenStandardOutput()

    let requestHandler = function
    | Initialize _p -> InitializeResponse InitializeResult.Default
    | Shutdown ->
        NoResponse
    | Exit ->
        Environment.Exit(0)
        NoResponse
    | _ -> UnhandledRequest

    FSharpLanguageServer.start input output requestHandler
    ()
