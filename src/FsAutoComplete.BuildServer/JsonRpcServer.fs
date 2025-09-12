namespace FsAutoComplete.BuildServer

open System
open System.IO
open System.Text
open System.Threading.Tasks
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open FsAutoComplete.Logging
open FsAutoComplete.BuildServerProtocol.JsonRpc
open FsAutoComplete.BuildServerProtocol.BuildServerProtocol
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
  let private handleBspRequest (request: JsonRpcRequest) : Task<JsonRpcResponse> =
    task {
      try
        logger.info (Log.setMessage "Handling BSP request: {method}" >> Log.addContext "method" request.Method)

        match request.Method with
        | "build/initialize" ->
          let! result = initializeWorkspace()
          match result with
          | Result.Ok () ->
            let capabilities = { 
              CompileProvider = Some true
              TestProvider = None
              RunProvider = None
              DebugProvider = None
              InverseSourcesProvider = None
              DependencySourcesProvider = None
              DependencyModulesProvider = None
              ResourcesProvider = None
              OutputPathsProvider = None
              BuildTargetChangedProvider = None
              JvmRunEnvironmentProvider = None
              JvmTestEnvironmentProvider = None
              CanReload = Some true 
            }
            return createSuccessResponse (Some request.Id) capabilities
          | Result.Error msg ->
            return createErrorResponse (Some request.Id) ErrorCodes.InternalError msg

        | "build/shutdown" ->
          let! result = shutdown()
          match result with
          | Result.Ok () ->
            return createSuccessResponse (Some request.Id) ()
          | Result.Error msg ->
            return createErrorResponse (Some request.Id) ErrorCodes.InternalError msg

        | "workspace/buildTargets" ->
          // Return empty build targets for now
          let result = { Targets = [||] }
          return createSuccessResponse (Some request.Id) result

        | "fsharp/workspacePeek" ->
          match request.Params with
          | Some parameters ->
            match tryDeserialize<WorkspacePeekRequest> parameters with
            | Some peekRequest ->
              let! result = peekWorkspace peekRequest
              match result with
              | Result.Ok response ->
                return createSuccessResponse (Some request.Id) response
              | Result.Error msg ->
                return createErrorResponse (Some request.Id) ErrorCodes.InternalError msg
            | None ->
              return createErrorResponse (Some request.Id) ErrorCodes.InvalidParams "Invalid workspace peek parameters"
          | None ->
            return createErrorResponse (Some request.Id) ErrorCodes.InvalidParams "Missing workspace peek parameters"

        | "fsharp/workspaceLoad" ->
          match request.Params with
          | Some parameters ->
            match tryDeserialize<WorkspaceLoadRequest> parameters with
            | Some loadRequest ->
              let! result = loadWorkspace loadRequest  
              match result with
              | Result.Ok response ->
                return createSuccessResponse (Some request.Id) response
              | Result.Error msg ->
                return createErrorResponse (Some request.Id) ErrorCodes.InternalError msg
            | None ->
              return createErrorResponse (Some request.Id) ErrorCodes.InvalidParams "Invalid workspace load parameters"
          | None ->
            return createErrorResponse (Some request.Id) ErrorCodes.InvalidParams "Missing workspace load parameters"

        | _ ->
          logger.warn (Log.setMessage "Unknown method: {method}" >> Log.addContext "method" request.Method)
          return createErrorResponse (Some request.Id) ErrorCodes.MethodNotFound $"Method not found: {request.Method}"
      with
      | ex ->
        logger.error (Log.setMessage "Error handling request: {error}" >> Log.addContext "error" ex.Message)
        return createErrorResponse (Some request.Id) ErrorCodes.InternalError ex.Message
    }

  /// Handle JSON RPC notifications
  let private handleNotification (notification: JsonRpcNotification) : Task<unit> =
    task {
      try
        logger.info (Log.setMessage "Handling notification: {method}" >> Log.addContext "method" notification.Method)
        
        match notification.Method with
        | "build/exit" ->
          logger.info (Log.setMessage "Received exit notification")
          Environment.Exit(0)
        | _ ->
          logger.warn (Log.setMessage "Unknown notification method: {method}" >> Log.addContext "method" notification.Method)
      with
      | ex ->
        logger.error (Log.setMessage "Error handling notification: {error}" >> Log.addContext "error" ex.Message)
    }

  /// Process a single JSON RPC message
  let processMessage (messageText: string) : Task<string option> =
    task {
      try
        let message = JObject.Parse(messageText)
        
        if message.ContainsKey("id") then
          // This is a request
          let request = message.ToObject<JsonRpcRequest>()
          let! response = handleBspRequest request
          return Some (serialize response)
        else
          // This is a notification
          let notification = message.ToObject<JsonRpcNotification>()
          do! handleNotification notification
          return None
      with
      | ex ->
        logger.error (Log.setMessage "Error processing message: {error}" >> Log.addContext "error" ex.Message)
        let errorResponse = createErrorResponse None ErrorCodes.ParseError "Parse error"
        return Some (serialize errorResponse)
    }

  /// Main server loop for stdin/stdout communication
  let runServer () =
    task {
      logger.info (Log.setMessage "Starting JSON RPC server...")
      
      use reader = new StreamReader(Console.OpenStandardInput())
      use writer = new StreamWriter(Console.OpenStandardOutput())
      writer.AutoFlush <- true

      let mutable keepRunning = true
      
      while keepRunning do
        try
          let! line = reader.ReadLineAsync()
          if not (isNull line) then
            let! response = processMessage line
            match response with
            | Some responseText ->
              do! writer.WriteLineAsync(responseText)
            | None ->
              () // No response needed for notifications
          else
            keepRunning <- false
        with
        | ex ->
          logger.error (Log.setMessage "Server loop error: {error}" >> Log.addContext "error" ex.Message)
          keepRunning <- false
      
      logger.info (Log.setMessage "JSON RPC server stopped")
    }