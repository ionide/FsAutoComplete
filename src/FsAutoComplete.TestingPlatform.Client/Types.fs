namespace FsAutoComplete.TestingPlatform.Client

open System
open System.Collections.Generic

type ClientLogLevel =
  | Trace
  | Debug
  | Information
  | Warning
  | Error

/// <summary>Client-side settings for a server-mode session.</summary>
type MtpClientOptions =
  {
    ClientName: string
    ClientVersion: string
    SupportedProtocolVersions: string list
    DebuggerProvider: bool
    IsStateful: bool option
    ConnectionTimeout: TimeSpan
    ServerShutdownTimeout: TimeSpan
    /// <summary>Variables applied to a child-process server. Ignored by in-process hosting.</summary>
    EnvironmentVariables: Map<string, string option>
    Logger: (ClientLogLevel -> string -> unit) option
  }

  static member Default: MtpClientOptions =
    { ClientName = "FsAutoComplete.TestingPlatform.Client"
      ClientVersion =
        typeof<MtpClientOptions>.Assembly.GetName().Version
        |> Option.ofObj
        |> Option.map string
        |> Option.defaultValue "1.0.0"
      SupportedProtocolVersions =
        Microsoft.Testing.Platform.ServerMode.Client.MtpServerClientOptions().SupportedProtocolVersions
        |> List.ofSeq
      DebuggerProvider = false
      IsStateful = None
      ConnectionTimeout = TimeSpan.FromSeconds 90.0
      ServerShutdownTimeout = TimeSpan.FromSeconds 30.0
      EnvironmentVariables = Map.empty
      Logger = None }

[<RequireQualifiedAccess>]
type NodeType =
  | Action
  | Group
  | Other of string

[<RequireQualifiedAccess>]
type ExecutionState =
  | Discovered
  | InProgress
  | Passed
  | Skipped
  | Failed
  | TimedOut
  | Error
  | Canceled
  | Other of string

type SourceLocation =
  { File: string
    LineStart: int option
    LineEnd: int option }

type NodeError =
  { Message: string option
    StackTrace: string option }

/// <summary>The method a test node runs, as the framework identifies it.</summary>
type MethodIdentifier =
  {
    /// <summary>Reported apart from the type before platform 2.0, which folds it into the type name.</summary>
    Namespace: string option
    TypeName: string
    /// <summary>The method name, followed by its parameter types when it has any.</summary>
    MethodName: string option
  }

/// <summary>One node in a <c>testing/testUpdates/tests</c> notification.</summary>
type TestNodeUpdate =
  {
    Uid: string
    DisplayName: string option
    NodeType: NodeType option
    ExecutionState: ExecutionState option
    ParentUid: string option
    Location: SourceLocation option
    MethodIdentifier: MethodIdentifier option
    Duration: TimeSpan option
    Error: NodeError option
    StandardOutput: string option
    StandardError: string option
    /// <summary>Every property on the node as published, including extension properties.</summary>
    Raw: IReadOnlyDictionary<string, obj>
  }

type TestNodeUpdateBatch =
  { RunId: Guid
    Updates: TestNodeUpdate list }

type ServerCapabilities =
  { ServerProcessId: int option
    ServerName: string option
    ServerVersion: string option
    ProtocolVersion: string option
    SupportsDiscovery: bool
    MultiRequestSupport: bool
    VSTestProviderSupport: bool
    SupportsAttachments: bool
    MultiConnectionProvider: bool }

type Attachment =
  { Uri: string option
    Producer: string option
    Type: string option
    DisplayName: string option
    Description: string option }

type RunResult = { Attachments: Attachment list }

type LogMessage =
  { Level: ClientLogLevel
    Message: string }

type TelemetryEvent =
  { EventName: string
    Metrics: IReadOnlyDictionary<string, obj> }

/// <summary>Raised for any failure reported by the server-mode client.</summary>
type MtpClientException(message: string, inner: exn) =
  inherit Exception(message, inner)

/// <summary>Raised when the server closed the connection or never connected.</summary>
type MtpConnectionClosedException(message: string, inner: exn) =
  inherit MtpClientException(message, inner)

/// <summary>Raised when the server answered a request with a JSON-RPC error.</summary>
type MtpProtocolErrorException(code: int, message: string, inner: exn) =
  inherit MtpClientException(message, inner)
  member _.ErrorCode = code
