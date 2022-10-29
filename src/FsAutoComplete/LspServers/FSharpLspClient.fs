namespace FsAutoComplete.Lsp


open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types.LspResult
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete.LspHelpers
open System
open System.Threading.Tasks

type WorkDoneProgressCreateParams ={
  ///The token to be used to report progress.
  token: string
}

/// The base protocol offers also support to report progress in a generic fashion.
/// This mechanism can be used to report any kind of progress including work done progress
/// (usually used to report progress in the user interface using a progress bar) and partial
/// result progress to support streaming of results.
type ProgressParams<'T> = {
  /// The progress token provided by the client or server.
  token : string
  /// The progress data.
  value : 'T
}
type WorkDoneProgressKind =
  | Begin
  | Report
  | End
  with
    override x.ToString() =
      match x with
      | Begin -> "begin"
      | Report -> "report"
      | End -> "end"

type WorkDoneProgressEnd = {
  kind : string
  message : string option
}
type WorkDoneProgress = {
  kind : string
  title : string option
  cancellable : bool option
  message : string option
  percentage : uint option
}
  with
  static member WorkDoneProgressBegin(title, ?cancellable, ?message, ?percentage) = {
    kind = WorkDoneProgressKind.Begin.ToString()
    title = Some title
    cancellable = cancellable
    message = message
    percentage = percentage
  }

  static member WorkDoneProgressReport(title, ?cancellable, ?message, ?percentage) = {
    kind = WorkDoneProgressKind.Report.ToString()
    title = Some title
    cancellable = cancellable
    message = message
    percentage = percentage
  }
  static member WorkDoneProgressEnd(?message) = {
    kind = WorkDoneProgressKind.End.ToString()
    title = None
    cancellable = None
    message = message
    percentage = None
  }


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

  member x.WorkDoneProgressCreate(token : string) =
    let progressCreate : WorkDoneProgressCreateParams = { token = token }
    sendServerRequest.Send "window/workDoneProgress/create" (box progressCreate)

  member x.Progress(token, value) =
    let progress : ProgressParams<_> = {
      token = token
      value = value
    }
    sendServerNotification "$/progress" (box progress) |> Async.Ignore



type ServerProgressReport(lspClient : FSharpLspClient, ?token : string) =

  let mutable canReportProgress = true
  let mutable endSent = false

  member val Token = defaultArg token (Guid.NewGuid().ToString()) with get

  member x.Begin(title, ?cancellable, ?message, ?percentage) = async {
    let! result = lspClient.WorkDoneProgressCreate x.Token
    match result with
    | Ok () -> ()
    | Error e ->
      canReportProgress <- false
    if canReportProgress then
      do! lspClient.Progress(x.Token, WorkDoneProgress.WorkDoneProgressBegin(title, ?cancellable = cancellable, ?message = message,  ?percentage = percentage))
  }

  member x.Report(title, ?cancellable, ?message, ?percentage) = async {
    if canReportProgress then
      do! lspClient.Progress(x.Token, WorkDoneProgress.WorkDoneProgressReport(title, ?cancellable = cancellable, ?message = message,  ?percentage = percentage))
  }

  member x.End(?message) = async {
    if canReportProgress && not endSent then
      do!  lspClient.Progress(x.Token, WorkDoneProgress.WorkDoneProgressEnd(?message = message))
      endSent <- true
  }

  interface IAsyncDisposable with
    member x.DisposeAsync () =
      task {
        do! x.End()
      } |>  ValueTask

  interface IDisposable with
    member x.Dispose () =
      (x :> IAsyncDisposable).DisposeAsync().GetAwaiter().GetResult()


open System.Diagnostics.Tracing
open System.Collections.Concurrent

    /// listener for the the events generated by the `FSharp.Compiler.FSharpCompilerEventSource`
  type ProgressListener(lspClient) =
      inherit EventListener()
      let dispose (d : #IDisposable) = d.Dispose()
      let mutable source = null

      let inflightEvents = ConcurrentDictionary<_, ServerProgressReport>()

      override __.OnEventSourceCreated newSource =
        if newSource.Name = "FSharpCompiler" then
          ``base``.EnableEvents(newSource, EventLevel.LogAlways, EventKeywords.All)
          source <- newSource

      override __.OnEventWritten eventArgs =

        let message =
          match eventArgs.EventId with

          // | 3 ->
          //   let progressReport = new ServerProgressReport(lspClient)
          //   inflightEvents.TryAdd(eventArgs.Task, progressReport) |> ignore
          //   progressReport.
          //   // Log.setMessage "Started {function}" >> logFunctionName eventArgs.Payload.[0]
          // | 4 ->
          //   match inflightEvents.TryRemove(eventArgs.Task) with
          //   | true, startTime ->
          //     let delta = DateTimeOffset.UtcNow - startTime

          //     // Log.setMessage "Finished {function} in {seconds}"
          //     // >> logFunctionName eventArgs.Payload.[0]
          //     // >> Log.addContextDestructured "seconds" delta.TotalSeconds
          //     ()
          //   | false, _ ->
          //     // Log.setMessage "Finished {function}" >> logFunctionName eventArgs.Payload.[0]
          //     ()
          | 5 ->
            let progressReport = new ServerProgressReport(lspClient)
            let message = eventArgs.Payload.[0] |> string
            if inflightEvents.TryAdd(eventArgs.Task, progressReport) then
              progressReport.Begin("Typechecking", message = message) |> Async.Start

          | 6 ->
            let message = eventArgs.Payload.[0] |> string
            match inflightEvents.TryRemove(eventArgs.Task) with
            | true, report ->
              report.End($"Finished {message}") |> Async.Start
              dispose report
              // Log.setMessage "Finished {function}: {message} ({seconds} seconds)"
              // >> logFunctionName eventArgs.Payload.[1]
              // >> Log.addContextDestructured "seconds" delta.TotalSeconds
              // >> Log.addContextDestructured "message" (eventArgs.Payload.[0])
              ()
            | false, _ ->

              ()
          | other ->
            ()
            // Log.setMessage "Unknown event {name}({id}) with payload {payload}"
            // >> Log.addContext "id" other
            // >> Log.addContextDestructured "name" eventArgs.EventName
            // >> Log.addContextDestructured "payload" (eventArgs.Payload |> Seq.toList)

        message

      interface System.IDisposable with
        member __.Dispose() =
          if isNull source then () else ``base``.DisableEvents(source)
          inflightEvents.Values |> Seq.iter(dispose)
