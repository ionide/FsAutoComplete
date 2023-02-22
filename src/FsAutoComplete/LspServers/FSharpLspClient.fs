namespace FsAutoComplete.Lsp


open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types.LspResult
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete.LspHelpers
open System
open System.Threading.Tasks
open FsAutoComplete.Utils


type FSharpLspClient(sendServerNotification: ClientNotificationSender, sendServerRequest: ClientRequestSender) =

  inherit LspClient()

  member val ClientCapabilities: ClientCapabilities option = None with get, set

  override __.WindowShowMessage(p) =
    sendServerNotification "window/showMessage" (box p) |> Async.Ignore

  override __.WindowShowMessageRequest(p) =
    sendServerRequest.Send "window/showMessageRequest" (box p)

  override __.WindowLogMessage(p) =
    sendServerNotification "window/logMessage" (box p) |> Async.Ignore

  override __.TelemetryEvent(p) =
    sendServerNotification "telemetry/event" (box p) |> Async.Ignore

  override __.ClientRegisterCapability(p) =
    sendServerRequest.Send "client/registerCapability" (box p)

  override __.ClientUnregisterCapability(p) =
    sendServerRequest.Send "client/unregisterCapability" (box p)

  override __.WorkspaceWorkspaceFolders() =
    sendServerRequest.Send "workspace/workspaceFolders" ()

  override __.WorkspaceConfiguration(p) =
    sendServerRequest.Send "workspace/configuration" (box p)

  override __.WorkspaceApplyEdit(p) =
    sendServerRequest.Send "workspace/applyEdit" (box p)

  override __.WorkspaceSemanticTokensRefresh() =
    sendServerNotification "workspace/semanticTokens/refresh" () |> Async.Ignore

  override __.TextDocumentPublishDiagnostics(p) =
    sendServerNotification "textDocument/publishDiagnostics" (box p) |> Async.Ignore

  ///Custom notification for workspace/solution/project loading events
  member __.NotifyWorkspace(p: PlainNotification) =
    sendServerNotification "fsharp/notifyWorkspace" (box p) |> Async.Ignore

  ///Custom notification for initial workspace peek
  member __.NotifyWorkspacePeek(p: PlainNotification) =
    sendServerNotification "fsharp/notifyWorkspacePeek" (box p) |> Async.Ignore

  member __.NotifyCancelledRequest(p: PlainNotification) =
    sendServerNotification "fsharp/notifyCancel" (box p) |> Async.Ignore

  member __.NotifyFileParsed(p: PlainNotification) =
    sendServerNotification "fsharp/fileParsed" (box p) |> Async.Ignore

  member __.NotifyDocumentAnalyzed(p: DocumentAnalyzedNotification) =
    sendServerNotification "fsharp/documentAnalyzed" (box p) |> Async.Ignore

  member __.NotifyTestDetected(p: TestDetectedNotification) =
    sendServerNotification "fsharp/testDetected" (box p) |> Async.Ignore

  member x.CodeLensRefresh() =
    match x.ClientCapabilities with
    | Some { Workspace = Some { CodeLens = Some { RefreshSupport = Some true } } } ->
      sendServerNotification "workspace/codeLens/refresh" () |> Async.Ignore
    | _ -> async { return () }

  override x.WorkDoneProgressCreate(token) =
    match x.ClientCapabilities with
    | Some { Window = Some { workDoneProgress = Some true } } ->
      let progressCreate: WorkDoneProgressCreateParams = { token = token }
      sendServerRequest.Send "window/workDoneProgress/create" (box progressCreate)
    | _ -> async { return Error(JsonRpc.Error.InternalErrorMessage "workDoneProgress is disabled") }

  override x.Progress(token, value) =
    let progress: ProgressParams<_> = { token = token; value = value }
    sendServerNotification "$/progress" (box progress) |> Async.Ignore



type ServerProgressReport(lspClient: FSharpLspClient, ?token: ProgressToken) =

  let mutable canReportProgress = true
  let mutable endSent = false

  member val Token = defaultArg token (ProgressToken.Second((Guid.NewGuid().ToString())))

  member x.Begin(title, ?cancellable, ?message, ?percentage) =
    async {
      let! result = lspClient.WorkDoneProgressCreate x.Token

      match result with
      | Ok() -> ()
      | Error e -> canReportProgress <- false

      if canReportProgress then
        do!
          lspClient.Progress(
            x.Token,
            WorkDoneProgressBegin.Create(
              title,
              ?cancellable = cancellable,
              ?message = message,
              ?percentage = percentage
            )
          )
    }

  member x.Report(?cancellable, ?message, ?percentage) =
    async {
      if canReportProgress then
        do!
          lspClient.Progress(
            x.Token,
            WorkDoneProgressReport.Create(?cancellable = cancellable, ?message = message, ?percentage = percentage)
          )
    }

  member x.End(?message) =
    async {
      if canReportProgress && not endSent then
        do! lspClient.Progress(x.Token, WorkDoneProgressEnd.Create(?message = message))
        endSent <- true
    }

  interface IAsyncDisposable with
    member x.DisposeAsync() = task { do! x.End() } |> ValueTask

  interface IDisposable with
    member x.Dispose() =
      (x :> IAsyncDisposable).DisposeAsync().GetAwaiter().GetResult()


open System.Diagnostics.Tracing
open System.Collections.Concurrent
open System.Diagnostics


/// listener for the the events generated from the fsc ActivitySource
type ProgressListener(lspClient: FSharpLspClient) =

  let mutable isDisposed = false

  let inflightEvents =
    ConcurrentDictionary<_, System.Diagnostics.Activity * ServerProgressReport>()

  let isStopped (activity: Activity) =
    // giving this 5 seconds to report something, otherwise assume it's a dead activity
    (DateTime.UtcNow - activity.StartTimeUtc) > TimeSpan.FromSeconds(5.)
    && activity.Duration = TimeSpan.Zero

  let mbp =
    MailboxProcessor.Start(fun inbox ->
      async {
        while not isDisposed do
          for (a, p) in inflightEvents.Values do
            // We don't get always get a corresponding ActivityStopped for a given Activity event so this looks at anything that hasn't had a duration incresed at all
            // We don't seem to get a corresponding ActivityStopped on cancelled typechecks.
            if isStopped a then
              do! p.End()
              inflightEvents.TryRemove(a.Id) |> ignore
            else
              // FSC doesn't start their spans with tags so we have to see if it's been added later https://github.com/dotnet/fsharp/issues/14776
              let fileName = a.GetTagItem "fileName" |> string
              do! p.Report(message = fileName)

          match! inbox.TryReceive(250) with
          | None ->
            // if we don't get an event in 250 ms just loop again so we can analyze inflightEvents
            ()
          | Some(action, activity: Activity, reply: AsyncReplyChannel<unit>) ->

            match action with
            | "start" ->
              if activity.DisplayName = "BoundModel.TypeCheck" && not (isStopped activity) then
                let progressReport = new ServerProgressReport(lspClient)

                if inflightEvents.TryAdd(activity.Id, (activity, progressReport)) then

                  do! progressReport.Begin($"Dependent Typecheck {activity.DisplayName}")

            | "stop" ->
              if activity.DisplayName = "BoundModel.TypeCheck" then
                match inflightEvents.TryRemove(activity.Id) with
                | true, (old, progressReport) -> do! progressReport.End()
                | _ -> ()

            | _ -> ()

            reply.Reply()
      })


  let shouldListenTo (act: ActivitySource) = act.Name = Tracing.fscServiceName

  let activityStarted (act: Activity) =
    mbp.PostAndReply(fun reply -> "start", act, reply)

  let activityStopped (act: Activity) =
    mbp.PostAndReply(fun reply -> "stop", act, reply)

  let listener =
    new ActivityListener(
      ShouldListenTo = shouldListenTo,
      Sample = (fun _ -> ActivitySamplingResult.AllDataAndRecorded),
      ActivityStarted = activityStarted,
      ActivityStopped = activityStopped
    )

  do ActivitySource.AddActivityListener listener

  interface IDisposable with
    member this.Dispose() : unit =
      (this :> IAsyncDisposable).DisposeAsync().GetAwaiter().GetResult()

  interface IAsyncDisposable with
    member this.DisposeAsync() : ValueTask =
      task {
        if not isDisposed then
          isDisposed <- true
          dispose listener

          for (a, p) in inflightEvents.Values do
            do! disposeAsync p
            inflightEvents.TryRemove(a.Id) |> ignore
      }
      |> ValueTask
