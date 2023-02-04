namespace FsAutoComplete.Lsp


open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types.LspResult
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete.LspHelpers
open System
open System.Threading.Tasks


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
      | Ok () -> ()
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

/// listener for the the events generated by the `FSharp.Compiler.FSharpCompilerEventSource`
type ProgressListener(lspClient) =
  inherit EventListener()
  let mutable isDisposed = false
  let tryDispose (d: #IDisposable) = try d.Dispose() with _ -> ()
  let mutable source = null

  let mutable inflightEvents = ConcurrentDictionary<_, ServerProgressReport>()

  member _.EnableEvents(eventSource ,level,matchAnyKeyword) =
    ``base``.EnableEvents(eventSource, level, matchAnyKeyword)

  override x.OnEventSourceCreated newSource =
    lock x <| fun () ->
      if newSource.Name = "FSharpCompiler" then
        x.EnableEvents(newSource, EventLevel.LogAlways, EventKeywords.All)
        source <- newSource

  override x.OnEventWritten eventArgs =

    lock x
    <| fun () ->
         try
           if isDisposed then
             ()
           else
             let message =
               match eventArgs.EventId with
               | 5 ->
                 let progressReport = new ServerProgressReport(lspClient)
                 let message = eventArgs.Payload.[0] |> string
                 let fileName = IO.Path.GetFileName message

                 if inflightEvents.TryAdd(eventArgs.Task, progressReport) then
                   progressReport.Begin($"Dependent Typecheck {fileName}", message = message)
                   |> Async.Start
               | 6 ->
                 let message = eventArgs.Payload.[0] |> string

                 match inflightEvents.TryRemove(eventArgs.Task) with
                 | true, report ->
                   report.End($"Finished {message}") |> Async.Start
                   tryDispose report
                   ()
                 | false, _ ->

                   ()
               | other -> ()

             message
         with e ->
           ()

  member _.DisableEvents(source) = ``base``.DisableEvents(source)

  interface System.IDisposable with
    member this.Dispose() =
      lock this
      <| fun () ->
           if isNull source then () else this.DisableEvents(source)
           isDisposed <- true
           inflightEvents.Values |> Seq.iter (tryDispose)
           inflightEvents <- null
