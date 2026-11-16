namespace FsAutoComplete.TestingPlatform.Client

open System
open System.Collections.Generic
open System.Threading
open System.Threading.Tasks

/// <summary>
/// Handles a server-to-client request such as <c>telemetry/update</c> or
/// <c>client/attachDebugger</c>. Receives the method name and its parameters, and returns the
/// result object.
/// </summary>
type ServerRequestHandler =
  string
    -> IReadOnlyDictionary<string, obj> option
    -> CancellationToken
    -> Task<IReadOnlyDictionary<string, obj> option>

module private Rethrow =
  /// <summary>
  /// Raises the client-facing translation of <paramref name="e"/>. An exception that passes
  /// through untranslated keeps its original stack trace.
  /// </summary>
  let translated (e: exn) : 'a =
    let translation = Interop.translateException e

    if obj.ReferenceEquals(translation, e) then
      System.Runtime.ExceptionServices.ExceptionDispatchInfo.Capture(e).Throw()

    raise translation

/// <summary>
/// A client for a Microsoft.Testing.Platform application running in server mode. The client owns
/// the launched or hosted application for its lifetime.
/// </summary>
/// <remarks>
/// <c>Dispose</c> tears down synchronously; prefer <c>ShutdownAsync</c> on UI or watchdog threads.
/// </remarks>
type MtpClient
  internal
  (inner: Microsoft.Testing.Platform.ServerMode.Client.IMtpServerClient, options: MtpClientOptions, ownsProcess: bool) =
  let log = options.Logger |> Option.defaultValue (fun _ _ -> ())
  let updates = Event<TestNodeUpdateBatch>()
  let logs = Event<LogMessage>()
  let telemetry = Event<TelemetryEvent>()
  let attachments = Event<Attachment list>()
  let mutable requestHandler: ServerRequestHandler option = None
  let mutable disposed = false

  let onUpdates =
    EventHandler<Microsoft.Testing.Platform.ServerMode.Client.MtpTestNodeUpdateEventArgs>(fun _ e ->
      updates.Trigger(Interop.toUpdateBatch log e))

  let onLog =
    EventHandler<Microsoft.Testing.Platform.ServerMode.Client.MtpLogEventArgs>(fun _ e ->
      logs.Trigger(Interop.toLogMessage e))

  let onTelemetry =
    EventHandler<Microsoft.Testing.Platform.ServerMode.Client.MtpTelemetryEventArgs>(fun _ e ->
      telemetry.Trigger(Interop.toTelemetry e))

  let onAttachments =
    EventHandler<Microsoft.Testing.Platform.ServerMode.Client.MtpAttachmentsEventArgs>(fun _ e ->
      attachments.Trigger(e.Attachments |> Seq.map Interop.toAttachment |> List.ofSeq))

  do
    inner.TestNodesUpdated.AddHandler onUpdates
    inner.LogReceived.AddHandler onLog
    inner.TelemetryReceived.AddHandler onTelemetry
    inner.AttachmentsReceived.AddHandler onAttachments

  let guard (work: unit -> Task<'a>) : Task<'a> =
    task {
      try
        return! work ()
      with e ->
        return Rethrow.translated e
    }

  let guardUnit (work: unit -> Task) : Task =
    task {
      try
        do! work ()
      with e ->
        return Rethrow.translated e
    }
    :> Task

  static let launchCore
    (launch: unit -> Task<Microsoft.Testing.Platform.ServerMode.Client.MtpServerClient>)
    (options: MtpClientOptions)
    (ownsProcess: bool)
    =
    task {
      try
        let! inner = launch ()
        return new MtpClient(inner, options, ownsProcess)
      with e ->
        return Rethrow.translated e
    }

  /// <summary>Launches <paramref name="source"/> as a child process and blocks until it connects.</summary>
  static member Launch(source: string, ?options: MtpClientOptions) : MtpClient =
    MtpClient.LaunchAsync(source, ?options = options).GetAwaiter().GetResult()

  /// <summary>
  /// Launches the test application at <paramref name="source"/> as a child process in server
  /// mode. <paramref name="source"/> is a managed <c>.dll</c> or a native executable.
  /// </summary>
  static member LaunchAsync
    (source: string, ?options: MtpClientOptions, ?cancellationToken: CancellationToken)
    : Task<MtpClient> =
    let options = defaultArg options MtpClientOptions.Default
    let token = defaultArg cancellationToken CancellationToken.None

    launchCore
      (fun () ->
        Microsoft.Testing.Platform.ServerMode.Client.MtpServerClient.LaunchAsync(
          source,
          Interop.ofOptions options,
          token
        ))
      options
      true

  /// <summary>
  /// Hosts the test application in the current process. <paramref name="run"/> receives the
  /// complete server-mode argument array and returns the application's exit code.
  /// </summary>
  static member LaunchInProcessAsync
    (run: string[] -> CancellationToken -> Task<int>, ?options: MtpClientOptions, ?cancellationToken: CancellationToken)
    : Task<MtpClient> =
    let options = defaultArg options MtpClientOptions.Default
    let token = defaultArg cancellationToken CancellationToken.None
    let entry = Func<string[], CancellationToken, Task<int>>(fun args ct -> run args ct)

    launchCore
      (fun () ->
        Microsoft.Testing.Platform.ServerMode.Client.MtpServerClient.LaunchInProcessAsync(
          entry,
          Interop.ofOptions options,
          token
        ))
      options
      false

  member _.ProcessId: int = inner.ProcessId
  /// <summary>
  /// The exit code of the server. For a child process, available once <c>ExitAsync</c> has
  /// observed the process exit; for an in-process host, available after <c>ShutdownAsync</c>.
  /// </summary>
  member _.ServerExitCode: int option = Option.ofNullable inner.ServerExitCode

  member _.Capabilities: ServerCapabilities option =
    inner.Capabilities |> Option.ofObj |> Option.map Interop.toCapabilities

  /// <summary>Performs the <c>initialize</c> handshake. Must complete before any other request.</summary>
  member _.InitializeAsync(?cancellationToken: CancellationToken) : Task<ServerCapabilities> =
    let token = defaultArg cancellationToken CancellationToken.None

    guard (fun () ->
      task {
        let! c = inner.InitializeAsync token
        return Interop.toCapabilities c
      })

  member _.DiscoverTestsAsync(?cancellationToken: CancellationToken) : Task =
    let token = defaultArg cancellationToken CancellationToken.None
    guardUnit (fun () -> inner.DiscoverTestsAsync token)

  member _.DiscoverTestsAsync(uids: string seq, ?cancellationToken: CancellationToken) : Task =
    let token = defaultArg cancellationToken CancellationToken.None
    let list = List<string>(uids) :> IReadOnlyCollection<string>
    guardUnit (fun () -> inner.DiscoverTestsAsync(list, token))

  /// <summary>Discovers the nodes matching an MTP graph filter such as <c>/parser/**</c>.</summary>
  member _.DiscoverTestsWithFilterAsync(filter: string, ?cancellationToken: CancellationToken) : Task =
    let token = defaultArg cancellationToken CancellationToken.None
    guardUnit (fun () -> inner.DiscoverTestsWithFilterAsync(filter, token))

  member _.RunTestsAsync(?cancellationToken: CancellationToken) : Task<RunResult> =
    let token = defaultArg cancellationToken CancellationToken.None

    guard (fun () ->
      task {
        let! r = inner.RunTestsAsync token
        return Interop.toRunResult r
      })

  member _.RunTestsAsync(uids: string seq, ?cancellationToken: CancellationToken) : Task<RunResult> =
    let token = defaultArg cancellationToken CancellationToken.None
    let list = List<string>(uids) :> IReadOnlyCollection<string>

    guard (fun () ->
      task {
        let! r = inner.RunTestsAsync(list, token)
        return Interop.toRunResult r
      })

  /// <summary>Runs the nodes matching an MTP graph filter such as <c>/parser/**</c>.</summary>
  member _.RunTestsWithFilterAsync(filter: string, ?cancellationToken: CancellationToken) : Task<RunResult> =
    let token = defaultArg cancellationToken CancellationToken.None

    guard (fun () ->
      task {
        let! r = inner.RunTestsWithFilterAsync(filter, token)
        return Interop.toRunResult r
      })

  /// <summary>
  /// Sends the protocol <c>exit</c> notification. For a child-process server, waits for up to
  /// <c>ServerShutdownTimeout</c> for the process to exit, leaving one still running at the
  /// deadline to <c>ShutdownAsync</c>; cancelling the token ends the wait and returns. An
  /// in-process host reports its exit code from <c>ShutdownAsync</c> instead.
  /// </summary>
  member _.ExitAsync(?cancellationToken: CancellationToken) : Task =
    let token = defaultArg cancellationToken CancellationToken.None

    guardUnit (fun () ->
      task {
        do! inner.ExitAsync token

        if ownsProcess then
          let elapsed = System.Diagnostics.Stopwatch.StartNew()

          try
            while not inner.ServerExitCode.HasValue
                  && elapsed.Elapsed < options.ServerShutdownTimeout do
              do! Task.Delay(25, token)
          with :? OperationCanceledException ->
            ()
      }
      :> Task)

  /// <summary>Tears down the connection and the server without blocking. Idempotent.</summary>
  member _.ShutdownAsync() : Task = guardUnit (fun () -> inner.ShutdownAsync())

  [<CLIEvent>]
  member _.TestNodesUpdated: IEvent<TestNodeUpdateBatch> = updates.Publish

  [<CLIEvent>]
  member _.LogReceived: IEvent<LogMessage> = logs.Publish

  [<CLIEvent>]
  member _.TelemetryReceived: IEvent<TelemetryEvent> = telemetry.Publish

  [<CLIEvent>]
  member _.AttachmentsReceived: IEvent<Attachment list> = attachments.Publish

  member _.TestNodeUpdates: IObservable<TestNodeUpdateBatch> = updates.Publish :> _
  member _.Logs: IObservable<LogMessage> = logs.Publish :> _
  member _.Telemetry: IObservable<TelemetryEvent> = telemetry.Publish :> _
  member _.Attachments: IObservable<Attachment list> = attachments.Publish :> _

  member _.ServerRequestHandler
    with get () = requestHandler
    and set (value: ServerRequestHandler option) =
      requestHandler <- value

      inner.ServerRequestHandler <-
        match value with
        | None -> null
        | Some handler ->
          Func<string, IDictionary<string, obj>, CancellationToken, Task<IDictionary<string, obj>>>
            (fun name parameters token ->
              task {
                let input =
                  parameters
                  |> Option.ofObj
                  |> Option.map (fun p -> Dictionary<string, obj>(p) :> IReadOnlyDictionary<string, obj>)

                let! output = handler name input token

                return
                  match output with
                  | Some o -> Dictionary<string, obj>(o) :> IDictionary<string, obj>
                  | None -> null
              })

  interface IDisposable with
    member _.Dispose() =
      if not disposed then
        disposed <- true
        inner.TestNodesUpdated.RemoveHandler onUpdates
        inner.LogReceived.RemoveHandler onLog
        inner.TelemetryReceived.RemoveHandler onTelemetry
        inner.AttachmentsReceived.RemoveHandler onAttachments
        inner.Dispose()
