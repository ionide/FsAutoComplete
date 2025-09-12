namespace FsAutoComplete.BuildServer

open System
open System.IO
open System.Text
open System.Threading.Tasks
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open FsAutoComplete.Logging
open BuildServerProtocol
open WorkspaceOperations

/// JSON RPC server for Build Server Protocol communication
module JsonRpcServer =

  let private logger = LogProvider.getLoggerByName "JsonRpcServer"

  type RequestHandler = JsonRpcRequest -> Task<JsonRpcResponse>
  type NotificationHandler = JsonRpcNotification -> Task<unit>

  let private jsonSettings = 
    JsonSerializerSettings(
      NullValueHandling = NullValueHandling.Ignore,
      DefaultValueHandling = DefaultValueHandling.Ignore)

  let private serialize obj = 
    JsonConvert.SerializeObject(obj, jsonSettings)

  let private deserialize<'T> (json: string) = 
    JsonConvert.DeserializeObject<'T>(json, jsonSettings)

  let private tryDeserialize<'T> (token: JToken) = 
    try
      Some (token.ToObject<'T>())
    with
    | _ -> None

  /// Create a successful response
  let private createSuccessResponse (id: JToken option) (result: obj) =
    { Id = id
      Result = Some (JToken.FromObject(result))
      Error = None }

  /// Create an error response
  let private createErrorResponse (id: JToken option) (code: int) (message: string) =
    { Id = id
      Result = None
      Error = Some { Code = code; Message = message; Data = None } }

  /// Handle BSP requests
  let private handleRequest (request: JsonRpcRequest) : Task<JsonRpcResponse> =
    task {
      logger.debug $"Handling request: {request.Method}"
      
      try
        match request.Method with
        
        // Build/Initialize
        | "build/initialize" ->
          let capabilities = 
            { CompileProvider = Some true
              TestProvider = Some false
              RunProvider = Some false
              DebugProvider = Some false
              InverseSourcesProvider = Some false
              DependencySourcesProvider = Some false
              DependencyModulesProvider = Some false
              ResourcesProvider = Some false
              OutputPathsProvider = Some false
              BuildTargetChangedProvider = Some true
              JvmRunEnvironmentProvider = Some false
              JvmTestEnvironmentProvider = Some false
              CanReload = Some true }
          return createSuccessResponse request.Id capabilities

        | "build/initialized" ->
          logger.info "Build server initialized"
          return createSuccessResponse request.Id ()

        | "build/shutdown" ->
          logger.info "Build server shutting down"
          shutdown()
          return createSuccessResponse request.Id ()

        // Workspace operations
        | "workspace/peek" | "fsharp/workspacePeek" ->
          match request.Params |> Option.bind tryDeserialize<FSharpWorkspacePeekRequest> with
          | Some peekRequest ->
            let! result = peekWorkspace peekRequest
            match result with
            | Ok response -> return createSuccessResponse request.Id response
            | Error error -> return createErrorResponse request.Id -1 error
          | None ->
            return createErrorResponse request.Id -32602 "Invalid parameters for workspace/peek"

        | "workspace/load" | "fsharp/workspaceLoad" ->
          match request.Params |> Option.bind tryDeserialize<FSharpWorkspaceLoadRequest> with
          | Some loadRequest ->
            let! result = loadWorkspace loadRequest
            match result with
            | Ok response -> return createSuccessResponse request.Id response
            | Error error -> return createErrorResponse request.Id -1 error
          | None ->
            return createErrorResponse request.Id -32602 "Invalid parameters for workspace/load"

        | "fsharp/project" ->
          match request.Params |> Option.bind tryDeserialize<FSharpProjectRequest> with
          | Some projectRequest ->
            let! result = getProject projectRequest
            match result with
            | Ok response -> return createSuccessResponse request.Id response
            | Error error -> return createErrorResponse request.Id -1 error
          | None ->
            return createErrorResponse request.Id -32602 "Invalid parameters for fsharp/project"

        // Build operations
        | "buildTarget/compile" ->
          match request.Params |> Option.bind tryDeserialize<CompileParams> with
          | Some compileRequest ->
            let! result = compileTargets compileRequest
            match result with
            | Ok response -> return createSuccessResponse request.Id response
            | Error error -> return createErrorResponse request.Id -1 error
          | None ->
            return createErrorResponse request.Id -32602 "Invalid parameters for buildTarget/compile"

        | _ ->
          logger.warn $"Unhandled request method: {request.Method}"
          return createErrorResponse request.Id -32601 $"Method not found: {request.Method}"

      with
      | ex ->
        logger.error $"Error handling request {request.Method}: {ex.Message}"
        return createErrorResponse request.Id -32603 "Internal error"
    }

  /// Handle notifications (no response expected)
  let private handleNotification (notification: JsonRpcNotification) : Task<unit> =
    task {
      logger.debug $"Handling notification: {notification.Method}"
      
      match notification.Method with
      | "build/exit" ->
        logger.info "Received exit notification"
        Environment.Exit(0)
      | _ ->
        logger.debug $"Unhandled notification: {notification.Method}"
    }

  /// Parse a JSON RPC message from a string
  let private parseMessage (json: string) =
    try
      let jobj = JObject.Parse(json)
      
      if jobj.ContainsKey("id") then
        // Request
        let request = jobj.ToObject<JsonRpcRequest>()
        Some (Choice1Of2 request)
      else
        // Notification
        let notification = jobj.ToObject<JsonRpcNotification>()
        Some (Choice2Of2 notification)
    with
    | ex ->
      logger.error $"Failed to parse JSON RPC message: {ex.Message}"
      None

  /// Read a single message from the input stream
  let private readMessage (reader: StreamReader) : Task<string option> =
    task {
      try
        // Read headers
        let mutable contentLength = 0
        let mutable line = ""
        
        // Read headers until empty line
        let mutable keepReading = true
        while keepReading do
          let! currentLine = reader.ReadLineAsync()
          line <- currentLine
          
          if String.IsNullOrEmpty(line) then
            keepReading <- false
          elif line.StartsWith("Content-Length: ") then
            Int32.TryParse(line.Substring(16), &contentLength) |> ignore
        
        if contentLength > 0 then
          // Read content
          let buffer = Array.zeroCreate<char> contentLength
          let! bytesRead = reader.ReadAsync(buffer, 0, contentLength)
          
          if bytesRead = contentLength then
            return Some (String(buffer))
          else
            logger.warn $"Expected {contentLength} bytes but read {bytesRead}"
            return None
        else
          logger.warn "No content length found"
          return None
      with
      | ex ->
        logger.error $"Error reading message: {ex.Message}"
        return None
    }

  /// Write a response message to the output stream
  let private writeMessage (writer: StreamWriter) (message: string) : Task<unit> =
    task {
      try
        let contentBytes = Encoding.UTF8.GetBytes(message)
        let header = $"Content-Length: {contentBytes.Length}\r\n\r\n"
        
        do! writer.WriteAsync(header)
        do! writer.WriteAsync(message)
        do! writer.FlushAsync()
      with
      | ex ->
        logger.error $"Error writing message: {ex.Message}"
    }

  /// Main server loop
  let runServer (input: Stream) (output: Stream) : Task<unit> =
    task {
      logger.info "Starting JSON RPC server"
      
      use reader = new StreamReader(input, Encoding.UTF8)
      use writer = new StreamWriter(output, Encoding.UTF8)
      
      let mutable keepRunning = true
      
      while keepRunning do
        let! messageOpt = readMessage reader
        
        match messageOpt with
        | Some json ->
          match parseMessage json with
          | Some (Choice1Of2 request) ->
            let! response = handleRequest request
            let responseJson = serialize response
            do! writeMessage writer responseJson
            
          | Some (Choice2Of2 notification) ->
            do! handleNotification notification
            
          | None ->
            logger.warn "Failed to parse message"
            
        | None ->
          logger.info "No more messages, stopping server"
          keepRunning <- false
    }