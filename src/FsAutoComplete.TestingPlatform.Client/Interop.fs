module internal FsAutoComplete.TestingPlatform.Client.Interop

open System
open System.Collections.Generic

let private tryString (key: string) (node: IReadOnlyDictionary<string, obj>) : string option =
  match node.TryGetValue key with
  | true, (:? string as s) -> Some s
  | _ -> None

let private tryNumber (key: string) (node: IReadOnlyDictionary<string, obj>) : float option =
  match node.TryGetValue key with
  | true, (:? float as d) -> Some d
  | true, (:? single as f) -> Some(float f)
  | true, (:? int as i) -> Some(float i)
  | true, (:? int64 as l) -> Some(float l)
  | true, (:? int16 as s) -> Some(float s)
  | true, (:? decimal as m) -> Some(float m)
  | _ -> None

let toNodeType (wire: string) : NodeType =
  match wire with
  | "action" -> NodeType.Action
  | "group" -> NodeType.Group
  | other -> NodeType.Other other

let toExecutionState (wire: string) : ExecutionState =
  match wire with
  | "discovered" -> ExecutionState.Discovered
  | "in-progress" -> ExecutionState.InProgress
  | "passed" -> ExecutionState.Passed
  | "skipped" -> ExecutionState.Skipped
  | "failed" -> ExecutionState.Failed
  | "timed-out" -> ExecutionState.TimedOut
  | "error" -> ExecutionState.Error
  | "canceled" -> ExecutionState.Canceled
  | other -> ExecutionState.Other other

let toUpdate
  (log: ClientLogLevel -> string -> unit)
  (parentUid: string option)
  (node: IReadOnlyDictionary<string, obj>)
  : TestNodeUpdate option =
  match tryString "uid" node with
  | None ->
    log ClientLogLevel.Warning "Dropped a test node update that carries no uid."
    None
  | Some uid ->
    let location =
      tryString "location.file" node
      |> Option.map (fun file ->
        { File = file
          LineStart = tryNumber "location.line-start" node |> Option.map int
          LineEnd = tryNumber "location.line-end" node |> Option.map int })

    let methodIdentifier =
      tryString "location.type" node
      |> Option.map (fun typeName ->
        { Namespace = tryString "location.namespace" node
          TypeName = typeName
          MethodName = tryString "location.method" node })

    let error =
      match tryString "error.message" node, tryString "error.stacktrace" node with
      | None, None -> None
      | message, stackTrace ->
        Some
          { Message = message
            StackTrace = stackTrace }

    Some
      { Uid = uid
        DisplayName = tryString "display-name" node
        NodeType = tryString "node-type" node |> Option.map toNodeType
        ExecutionState = tryString "execution-state" node |> Option.map toExecutionState
        ParentUid = parentUid
        Location = location
        MethodIdentifier = methodIdentifier
        Duration = tryNumber "time.duration-ms" node |> Option.map TimeSpan.FromMilliseconds
        Error = error
        StandardOutput = tryString "standardOutput" node
        StandardError = tryString "standardError" node
        Raw = node }

let toUpdateBatch
  (log: ClientLogLevel -> string -> unit)
  (e: Microsoft.Testing.Platform.ServerMode.Client.MtpTestNodeUpdateEventArgs)
  : TestNodeUpdateBatch =
  { RunId = e.RunId
    Updates =
      e.Changes
      |> Seq.choose (fun u -> toUpdate log (Option.ofObj u.ParentUid) u.Node)
      |> List.ofSeq }

let toCapabilities (c: Microsoft.Testing.Platform.ServerMode.Client.MtpServerCapabilities) : ServerCapabilities =
  { ServerProcessId = Option.ofNullable c.ServerProcessId
    ServerName = Option.ofObj c.ServerName
    ServerVersion = Option.ofObj c.ServerVersion
    ProtocolVersion = Option.ofObj c.ProtocolVersion
    SupportsDiscovery = c.SupportsDiscovery
    MultiRequestSupport = c.MultiRequestSupport
    VSTestProviderSupport = c.VSTestProviderSupport
    SupportsAttachments = c.SupportsAttachments
    MultiConnectionProvider = c.MultiConnectionProvider }

let toAttachment (a: Microsoft.Testing.Platform.ServerMode.Client.MtpAttachment) : Attachment =
  { Uri = Option.ofObj a.Uri
    Producer = Option.ofObj a.Producer
    Type = Option.ofObj a.Type
    DisplayName = Option.ofObj a.DisplayName
    Description = Option.ofObj a.Description }

let toRunResult (r: Microsoft.Testing.Platform.ServerMode.Client.MtpRunResult) : RunResult =
  { Attachments = r.Artifacts |> Seq.map toAttachment |> List.ofSeq }

let toLogLevel (level: Microsoft.Testing.Platform.ServerMode.Client.MtpClientLogLevel) : ClientLogLevel =
  match level with
  | Microsoft.Testing.Platform.ServerMode.Client.MtpClientLogLevel.Trace -> ClientLogLevel.Trace
  | Microsoft.Testing.Platform.ServerMode.Client.MtpClientLogLevel.Debug -> ClientLogLevel.Debug
  | Microsoft.Testing.Platform.ServerMode.Client.MtpClientLogLevel.Information -> ClientLogLevel.Information
  | Microsoft.Testing.Platform.ServerMode.Client.MtpClientLogLevel.Warning -> ClientLogLevel.Warning
  | _ -> ClientLogLevel.Error

let toLogMessage (e: Microsoft.Testing.Platform.ServerMode.Client.MtpLogEventArgs) : LogMessage =
  let level =
    match e.Level with
    | "Trace" -> ClientLogLevel.Trace
    | "Debug" -> ClientLogLevel.Debug
    | "Information" -> ClientLogLevel.Information
    | "Warning" -> ClientLogLevel.Warning
    | _ -> ClientLogLevel.Error

  { Level = level; Message = e.Message }

let toTelemetry (e: Microsoft.Testing.Platform.ServerMode.Client.MtpTelemetryEventArgs) : TelemetryEvent =
  { EventName = e.EventName
    Metrics = e.Metrics }

let ofOptions (o: MtpClientOptions) : Microsoft.Testing.Platform.ServerMode.Client.MtpServerClientOptions =
  let upstream = Microsoft.Testing.Platform.ServerMode.Client.MtpServerClientOptions()
  upstream.ClientName <- o.ClientName
  upstream.ClientVersion <- o.ClientVersion
  upstream.SupportedProtocolVersions <- (List.toArray o.SupportedProtocolVersions :> IReadOnlyCollection<string>)
  upstream.DebuggerProvider <- o.DebuggerProvider
  upstream.IsStateful <- o.IsStateful |> Option.defaultValue false
  upstream.ConnectionTimeout <- o.ConnectionTimeout
  upstream.ServerShutdownTimeout <- o.ServerShutdownTimeout

  for KeyValue(key, value) in o.EnvironmentVariables do
    upstream.EnvironmentVariables[key] <- Option.toObj value

  upstream.Logger <-
    match o.Logger with
    | Some log ->
      Microsoft.Testing.Platform.ServerMode.Client.DelegateMtpClientLogger(fun level message ->
        log (toLogLevel level) message)
      :> Microsoft.Testing.Platform.ServerMode.Client.IMtpClientLogger
    | None -> null

  upstream

let translateException (e: exn) : exn =
  match e with
  | :? Microsoft.Testing.Platform.ServerMode.Client.MtpServerConnectionClosedException as x ->
    MtpConnectionClosedException(x.Message, x) :> exn
  | :? Microsoft.Testing.Platform.ServerMode.Client.MtpServerErrorException as x ->
    MtpProtocolErrorException(x.ErrorCode, x.Message, x) :> exn
  | :? Microsoft.Testing.Platform.ServerMode.Client.MtpServerClientException as x ->
    MtpClientException(x.Message, x) :> exn
  | other -> other
