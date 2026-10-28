namespace FsAutoComplete.Lsp


open FsAutoComplete
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc
open FsAutoComplete.LspHelpers
open System
open System.Threading.Tasks
open System.Threading
open IcedTasks


type FSharpLspClient(sendServerNotification: ClientNotificationSender, sendServerRequest: ClientRequestSender) =

  inherit LspClient()

  member val ClientCapabilities: ClientCapabilities option = None with get, set

  override __.WindowShowMessage(p) = sendServerNotification "window/showMessage" (box p) |> Async.Ignore

  override __.WindowShowMessageRequest(p) = sendServerRequest.Send "window/showMessageRequest" (box p)

  override __.WindowLogMessage(p) = sendServerNotification "window/logMessage" (box p) |> Async.Ignore

  override __.WindowShowDocument(p) = sendServerRequest.Send "window/showDocument" (box p)

  override __.TelemetryEvent(p) = sendServerNotification "telemetry/event" (box p) |> Async.Ignore

  override __.ClientRegisterCapability(p) = sendServerRequest.Send "client/registerCapability" (box p)

  override __.ClientUnregisterCapability(p) = sendServerRequest.Send "client/unregisterCapability" (box p)

  override __.WorkspaceWorkspaceFolders() = sendServerRequest.Send "workspace/workspaceFolders" ()

  override __.WorkspaceConfiguration(p) = sendServerRequest.Send "workspace/configuration" (box p)

  override __.WorkspaceApplyEdit(p: ApplyWorkspaceEditParams) : AsyncLspResult<ApplyWorkspaceEditResult> =
    sendServerRequest.Send "workspace/applyEdit" (box p)

  override __.WorkspaceSemanticTokensRefresh() = sendServerRequest.Send "workspace/semanticTokens/refresh" ()

  override __.TextDocumentPublishDiagnostics(p: PublishDiagnosticsParams) =
    sendServerNotification "textDocument/publishDiagnostics" (box p) |> Async.Ignore

  ///Custom notification for workspace/solution/project loading events
  member __.NotifyWorkspace(p: PlainNotification) =
    sendServerNotification "fsharp/notifyWorkspace" (box p) |> Async.Ignore

  ///Custom notification for initial workspace peek
  member __.NotifyWorkspacePeek(p: PlainNotification) =
    sendServerNotification "fsharp/notifyWorkspacePeek" (box p) |> Async.Ignore

  member __.NotifyCancelledRequest(p: PlainNotification) =
    sendServerNotification "fsharp/notifyCancel" (box p) |> Async.Ignore

  member __.NotifyFileParsed(p: PlainNotification) = sendServerNotification "fsharp/fileParsed" (box p) |> Async.Ignore

  member __.NotifyDocumentAnalyzed(p: DocumentAnalyzedNotification) =
    sendServerNotification "fsharp/documentAnalyzed" (box p) |> Async.Ignore

  member __.NotifyTestDetected(p: TestDetectedNotification) =
    sendServerNotification "fsharp/testDetected" (box p) |> Async.Ignore

  member __.NotifyTestDiscoveryUpdate(p: TestDiscoveryUpdateNotification) =
    sendServerNotification "test/testDiscoveryUpdate" (box { Content = JsonSerializer.writeJson p })
    |> Async.Ignore

  member __.NotifyTestRunUpdate(p: TestRunProgress) =
    sendServerNotification "test/testRunProgressUpdate" (box { Content = JsonSerializer.writeJson p })
    |> Async.Ignore

  member __.AttachDebuggerForTestRun(processId: int) : AsyncLspResult<bool> =
    sendServerRequest.Send "test/processWaitingForDebugger" (box { Content = string processId })

  member x.CodeLensRefresh() =
    match x.ClientCapabilities with
    | Some {
             Workspace = Some {
                                CodeLens = Some { RefreshSupport = Some true }
                              }
           } -> sendServerNotification "workspace/codeLens/refresh" () |> Async.Ignore
    | _ -> async { return () }

  override x.WindowWorkDoneProgressCreate(token) =
    match x.ClientCapabilities with
    | Some {
             Window = Some { WorkDoneProgress = Some true }
           } ->
      let progressCreate: WorkDoneProgressCreateParams = { Token = token.Token }
      sendServerRequest.Send "window/workDoneProgress/create" (box progressCreate)
    | _ -> async { return Error(JsonRpc.Error.InternalError "workDoneProgress is disabled") }


  override x.Progress(p: ProgressParams) = sendServerNotification "$/progress" p |> Async.Ignore

  member x.Progress(token, value) =

    let progress: ProgressParams =
      {
        Token = token
        Value = Json.fromObject value
      }

    x.Progress progress

type ServerProgressReport(lspClient: FSharpLspClient, ?token: ProgressToken, ?cancellableDefault: bool) =

  let mutable canReportProgress = false
  let mutable endSent = false

  let locker = new SemaphoreSlim(1, 1)
  let cts = new CancellationTokenSource()

  member val ProgressToken = defaultArg token (ProgressToken.C2((Guid.NewGuid().ToString())))

  member val CancellationToken = cts.Token

  member x.Cancel() =
    try
      cts.Cancel()
    with _ ->
      ()


  member x.Begin(title, ?cancellable, ?message, ?percentage) =
    cancellableTask {
      use! __ = fun (ct: CancellationToken) -> locker.LockAsync(ct)

      if not endSent then
        let! result = lspClient.WindowWorkDoneProgressCreate { Token = x.ProgressToken }

        match result with
        | Ok() -> canReportProgress <- true
        | Error _e -> canReportProgress <- false

        if canReportProgress then
          do!
            lspClient.Progress(
              x.ProgressToken,
              WorkDoneProgressBegin.Create(
                title,
                ?cancellable = (cancellable |> Option.orElse cancellableDefault),
                ?message = message,
                ?percentage = percentage
              )
            )
    }

  member x.Report(?cancellable, ?message, ?percentage) =
    cancellableTask {
      use! __ = fun ct -> locker.LockAsync(ct)

      if canReportProgress && not endSent then
        do!
          lspClient.Progress(
            x.ProgressToken,
            WorkDoneProgressReport.Create(
              ?cancellable = (cancellable |> Option.orElse cancellableDefault),
              ?message = message,
              ?percentage = percentage
            )
          )
    }

  member x.End(?message) =
    cancellableTask {
      use! __ = fun ct -> locker.LockAsync(ct)
      let stillNeedsToSend = canReportProgress && not endSent

      if stillNeedsToSend then
        do! lspClient.Progress(x.ProgressToken, WorkDoneProgressEnd.Create(?message = message))
        endSent <- true
    }

  interface IAsyncDisposable with
    member x.DisposeAsync() =
      task {
        cts.Dispose()
        do! x.End () CancellationToken.None
      }
      |> ValueTask

  interface IDisposable with
    member x.Dispose() = (x :> IAsyncDisposable).DisposeAsync() |> ignore


open FsAutoComplete.Logging

/// <summary>One operation tracked by a <see cref="SharedProgressReporter"/>. Dispose it when the operation ends.</summary>
[<Sealed>]
type SharedProgressScope(cancellationToken: CancellationToken, onDispose: unit -> unit) =
  let mutable disposed = 0

  member _.CancellationToken = cancellationToken

  interface IDisposable with
    member _.Dispose() =
      if Interlocked.Exchange(&disposed, 1) = 0 then
        onDispose ()

/// <summary>A batch of operations tracked by a <see cref="SharedProgressReporter"/>. Dispose it when the batch ends.</summary>
[<Sealed>]
type SharedProgressBatch
  (cancellationToken: CancellationToken, beginOperation: unit -> SharedProgressScope, onDispose: unit -> unit) =
  let mutable disposed = 0

  member _.CancellationToken = cancellationToken

  member _.Begin() = beginOperation ()

  interface IDisposable with
    member _.Dispose() =
      if Interlocked.Exchange(&disposed, 1) = 0 then
        onDispose ()

/// One progress report of a SharedProgressReporter, from its creation until its last operation ends.
/// Only access it while holding the lock of the reporter.
type SharedProgressGeneration(report: ServerProgressReport) =
  member val Report = report

  /// The running operations and their labels, in the order they started.
  member val Operations = Map.empty<int64, string option> with get, set
  member val Batches = 0 with get, set
  member val BatchTotal = 0 with get, set
  member val BatchCompleted = 0 with get, set
  /// Set by the agent once it read this generation as the wanted state.
  member val Seen = false with get, set
  member x.IsIdle = Map.isEmpty x.Operations && x.Batches = 0

type SharedProgressRequest =
  | SyncProgress
  | FlushProgress of AsyncReplyChannel<unit>

/// <summary>
/// Shows concurrent operations, such as typechecks, as one LSP progress report.
/// The report starts with the first operation, lists the running operations in its message,
/// and ends when the last operation ends.
/// </summary>
[<Sealed>]
type SharedProgressReporter(title: string, createReport: unit -> ServerProgressReport, ?isEnabled: unit -> bool) =

  let isEnabled = defaultArg isEnabled (fun () -> true)
  let logger = LogProvider.getLoggerByName "SharedProgressReporter"
  let syncRoot = obj ()
  let mutable current: SharedProgressGeneration option = None
  let mutable lastOperationId = 0L
  let mutable syncPending = 0
  let mutable disposed = false

  let buildMessage (generation: SharedProgressGeneration) =
    let labels =
      generation.Operations
      |> Map.values
      |> Seq.choose id
      |> Seq.distinct
      |> Seq.toList

    let labelsPart =
      match labels with
      | [] -> None
      | [ label ] -> Some label
      | labels ->
        let shown = labels |> List.truncate 3 |> String.concat ", "
        let hidden = labels.Length - 3

        Some(if hidden > 0 then $"{shown} (+{hidden} more)" else shown)

    let batchPart =
      if generation.BatchTotal > 0 then
        Some $"{generation.BatchCompleted}/{generation.BatchTotal} completed"
      else
        None

    match labelsPart, batchPart with
    | Some labels, Some batch -> Some $"{labels} ({batch})"
    | Some labels, None -> Some labels
    | None, Some batch -> Some batch
    | None, None -> None

  let calcPercentage (generation: SharedProgressGeneration) =
    if generation.BatchTotal > 0 then
      min 100 (generation.BatchCompleted * 100 / generation.BatchTotal)
      |> uint32
      |> Some
    else
      None

  let send (kind: string) (work: CancellationToken -> Task<unit>) =
    task {
      try
        do! work CancellationToken.None
      with e ->
        logger.warn (
          Log.setMessage "Could not send {kind} of {title} progress"
          >> Log.addContext "kind" kind
          >> Log.addContext "title" title
          >> Log.addExn e
        )
    }

  /// Brings the client from the shown progress state to the wanted one.
  /// A report that started and ended between two syncs is never sent.
  let sync
    (shown: (ServerProgressReport * string option * uint32 option) option)
    (wanted: (ServerProgressReport * string option * uint32 option) option)
    =
    task {
      match shown, wanted with
      | Some(shownReport, shownMessage, shownPercentage), Some(report, message, percentage) when
        obj.ReferenceEquals(shownReport, report)
        ->
        if shownMessage <> message || shownPercentage <> percentage then
          do! send "report" (report.Report(?message = message, ?percentage = percentage))
      | _ ->
        match shown with
        | Some(shownReport, _, _) ->
          do! send "end" (shownReport.End())
          (shownReport :> IDisposable).Dispose()
        | None -> ()

        match wanted with
        | Some(report, message, percentage) ->
          do! send "begin" (report.Begin(title, ?message = message, ?percentage = percentage))
        | None -> ()

      return wanted
    }

  // The callers only change the state; this agent alone talks to the client, in order,
  // so no caller waits for a client round-trip.
  let sender =
    MailboxProcessor<SharedProgressRequest>.Start(fun inbox ->
      let rec loop shown =
        async {
          let! request = inbox.Receive()

          let! shown =
            async {
              try
                let wanted =
                  lock syncRoot (fun () ->
                    syncPending <- 0

                    current
                    |> Option.map (fun generation ->
                      generation.Seen <- true
                      generation.Report, buildMessage generation, calcPercentage generation))

                return! sync shown wanted |> Async.AwaitTask
              with e ->
                logger.warn (
                  Log.setMessage "Could not sync {title} progress"
                  >> Log.addContext "title" title
                  >> Log.addExn e
                )

                return shown
            }

          match request with
          | FlushProgress reply -> reply.Reply()
          | SyncProgress -> ()

          return! loop shown
        }

      loop None)

  let requestSync () =
    if Interlocked.Exchange(&syncPending, 1) = 0 then
      sender.Post SyncProgress

  /// Call while holding syncRoot.
  let currentGeneration () =
    match current with
    | Some generation when not generation.Report.CancellationToken.IsCancellationRequested -> generation
    | _ ->
      // The user cancelled the current report: new operations get a fresh report and token.
      let generation = SharedProgressGeneration(createReport ())
      current <- Some generation
      generation

  /// Call while holding syncRoot.
  let retireIfIdle (generation: SharedProgressGeneration) =
    match current with
    | Some c when obj.ReferenceEquals(c, generation) && generation.IsIdle ->
      current <- None

      // The agent never saw this report, so it will not end and dispose it either.
      if not generation.Seen then
        (generation.Report :> IDisposable).Dispose()
    | _ -> ()

  /// Call while holding syncRoot. onEnd is called while holding syncRoot.
  let startOperation (generation: SharedProgressGeneration) (label: string option) (onEnd: unit -> unit) =
    lastOperationId <- lastOperationId + 1L
    let operationId = lastOperationId
    generation.Operations <- Map.add operationId label generation.Operations

    new SharedProgressScope(
      generation.Report.CancellationToken,
      fun () ->
        lock syncRoot (fun () ->
          generation.Operations <- Map.remove operationId generation.Operations
          onEnd ()
          retireIfIdle generation)

        requestSync ()
    )

  let disabledScope () = new SharedProgressScope(CancellationToken.None, ignore)

  let disabledBatch () = new SharedProgressBatch(CancellationToken.None, disabledScope, ignore)

  /// <summary>Starts an operation. Its label shows in the message of the report until the scope is disposed.</summary>
  member _.Begin(label: string) =
    if not (isEnabled ()) then
      disabledScope ()
    else
      let scope =
        lock syncRoot (fun () ->
          if disposed then
            disabledScope ()
          else
            startOperation (currentGeneration ()) (Some label) ignore)

      requestSync ()
      scope

  /// <summary>Starts a batch of <paramref name="count"/> operations. The report shows how many of them completed.</summary>
  member _.BeginBatch(count: int) =
    if count <= 0 || not (isEnabled ()) then
      disabledBatch ()
    else
      let generation =
        lock syncRoot (fun () ->
          if disposed then
            None
          else
            let generation = currentGeneration ()
            generation.Batches <- generation.Batches + 1
            generation.BatchTotal <- generation.BatchTotal + count
            Some generation)

      match generation with
      | None -> disabledBatch ()
      | Some generation ->
        requestSync ()

        let token = generation.Report.CancellationToken
        // Only accessed while holding syncRoot.
        let completed = ref 0
        let ended = ref false

        let completeOperation () =
          if not ended.Value then
            completed.Value <- completed.Value + 1
            generation.BatchCompleted <- generation.BatchCompleted + 1

        let beginOperation () =
          let scope =
            lock syncRoot (fun () ->
              if token.IsCancellationRequested then
                // The user cancelled the batch, so its remaining operations do not show.
                new SharedProgressScope(token, ignore)
              else
                startOperation generation None completeOperation)

          requestSync ()
          scope

        let endBatch () =
          lock syncRoot (fun () ->
            ended.Value <- true
            generation.Batches <- generation.Batches - 1

            if generation.Batches = 0 then
              generation.BatchTotal <- 0
              generation.BatchCompleted <- 0
            else
              // Operations of this batch that never ran no longer count towards the batches still running.
              generation.BatchTotal <- generation.BatchTotal - max 0 (count - completed.Value)

            retireIfIdle generation)

          requestSync ()

        new SharedProgressBatch(token, beginOperation, endBatch)

  /// <summary>Completes when the client has been sent the current progress state.</summary>
  member _.Flush() : Task<unit> = sender.PostAndAsyncReply FlushProgress |> Async.StartImmediateAsTask

  interface IDisposable with
    member _.Dispose() =
      lock syncRoot (fun () ->
        disposed <- true
        current <- None)

      requestSync ()


open System.Diagnostics.Tracing
open System.Collections.Concurrent
open System.Diagnostics
open Ionide.ProjInfo.Logging
open System.Text.RegularExpressions


/// <summary>listener for the the events generated from the fsc ActivitySource</summary>
type ProgressListener(lspClient: FSharpLspClient, traceNamespace: string array) =

  let traceNamespace =
    traceNamespace |> Array.map (fun x -> Regex(x, RegexOptions.Compiled))

  let isOneOf list string = list |> Array.exists (fun f -> f string)

  let strContains (substring: Regex) (str: string) = substring.IsMatch str

  let interestingActivities = traceNamespace |> Array.map strContains

  let logger = LogProvider.getLoggerByName "Compiler"

  let mutable isDisposed = false

  let inflightEvents =
    ConcurrentDictionary<_, System.Diagnostics.Activity * ServerProgressReport>()

  let isStopped (activity: Activity) =
#if NET7_0_OR_GREATER
    activity.IsStopped
    ||
#endif
    // giving this 2 seconds to report something, otherwise assume it's a dead activity
    ((DateTime.UtcNow - activity.StartTimeUtc) > TimeSpan.FromSeconds(2.)
     && activity.Duration = TimeSpan.Zero)

  let getTagItemSafe key (a: Activity) = a.GetTagItem key |> Option.ofObj

  let getFileName =
    getTagItemSafe Tracing.SemanticConventions.FCS.fileName
    >> Option.map string
    >> Option.map IO.Path.GetFileName
    >> Option.defaultValue String.Empty

  let getProject =
    getTagItemSafe Tracing.SemanticConventions.FCS.project
    >> Option.map string
    >> Option.map IO.Path.GetFileName
    >> Option.defaultValue String.Empty

  let getUserOpName =
    getTagItemSafe Tracing.SemanticConventions.FCS.userOpName
    >> Option.map string
    >> Option.defaultValue String.Empty

  let mbp =
    MailboxProcessor.Start(fun inbox ->
      async {
        while not isDisposed do
          for (a, p) in inflightEvents.Values do
            // We don't get always get a corresponding ActivityStopped for a given Activity event so this looks at anything that hasn't had a duration increased at all
            // We don't seem to get a corresponding ActivityStopped on cancelled type-checks.
            if isStopped a then
              do! p.End()
              inflightEvents.TryRemove(a.Id) |> ignore
            else
              // FSC doesn't start their spans with tags so we have to see if it's been added later https://github.com/dotnet/fsharp/issues/14776
              let message =
                [ getFileName a; getProject a; getUserOpName a ]
                |> List.filter (String.IsNullOrEmpty >> not)
                |> String.join " - "

              do! p.Report(message = message)

          match! inbox.TryReceive(250) with
          | None ->
            // if we don't get an event in 250 ms just loop again so we can analyze inflightEvents
            ()
          | Some(action, activity: Activity) ->

            match action with
            | "start" ->
              let fileName = getFileName activity
              let userOpName = getUserOpName activity

              logger.trace (
                Log.setMessageI
                  $"Started : {activity.DisplayName:DisplayName} - {userOpName:UserOpName} - {fileName:fileName}"
              )

              if
                activity.DisplayName |> isOneOf interestingActivities
                && not (isStopped activity)
              then
                let progressReport = new ServerProgressReport(lspClient)

                if inflightEvents.TryAdd(activity.Id, (activity, progressReport)) then

                  do! progressReport.Begin($"{activity.DisplayName}")

            | "stop" ->
              let fileName = getFileName activity
              let userOpName = getUserOpName activity
              let duration = activity.Duration.ToString()

              logger.trace (
                Log.setMessageI
                  $"Finished : {activity.DisplayName:DisplayName} - {userOpName:UserOpName} - {fileName:fileName} - took {duration:duration}"
              )

              if activity.DisplayName |> isOneOf interestingActivities then
                match inflightEvents.TryRemove(activity.Id) with
                | true, (_old, progressReport) -> do! progressReport.End()
                | _ -> ()

            | _ -> ()

      })


  let shouldListenTo (act: ActivitySource) = act.Name = Tracing.fscServiceName

  let activityStarted (act: Activity) = mbp.Post("start", act)

  let activityStopped (act: Activity) = mbp.Post("stop", act)

  let listener =
    new ActivityListener(
      ShouldListenTo = shouldListenTo,
      Sample = (fun _ -> ActivitySamplingResult.AllDataAndRecorded),
      ActivityStarted = activityStarted,
      ActivityStopped = activityStopped
    )

  do ActivitySource.AddActivityListener listener

  interface IDisposable with
    member this.Dispose() : unit = (this :> IAsyncDisposable).DisposeAsync() |> ignore

  interface IAsyncDisposable with
    member this.DisposeAsync() : ValueTask =
      // was getting a compile error for the state machine in CI to `task`
      asyncEx {
        if not isDisposed then
          isDisposed <- true
          dispose listener

          for (a, p) in inflightEvents.Values do
            do! disposeAsync p
            inflightEvents.TryRemove(a.Id) |> ignore
      }
      |> Async.StartImmediateAsTask
      |> ValueTask
