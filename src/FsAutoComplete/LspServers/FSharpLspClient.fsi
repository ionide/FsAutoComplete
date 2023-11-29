namespace FsAutoComplete.Lsp

open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete.LspHelpers
open System
open IcedTasks

type FSharpLspClient =
  new: sendServerNotification: ClientNotificationSender * sendServerRequest: ClientRequestSender -> FSharpLspClient
  inherit LspClient
  member ClientCapabilities: ClientCapabilities option with get, set
  override WindowShowMessage: ShowMessageParams -> Async<unit>
  override WindowShowMessageRequest: ShowMessageRequestParams -> AsyncLspResult<MessageActionItem option>
  override WindowLogMessage: LogMessageParams -> Async<unit>
  override TelemetryEvent: Newtonsoft.Json.Linq.JToken -> Async<unit>
  override ClientRegisterCapability: RegistrationParams -> AsyncLspResult<unit>
  override ClientUnregisterCapability: UnregistrationParams -> AsyncLspResult<unit>
  override WorkspaceWorkspaceFolders: unit -> AsyncLspResult<WorkspaceFolder array option>
  override WorkspaceConfiguration: ConfigurationParams -> AsyncLspResult<Newtonsoft.Json.Linq.JToken array>
  override WorkspaceApplyEdit: ApplyWorkspaceEditParams -> AsyncLspResult<ApplyWorkspaceEditResponse>
  override WorkspaceSemanticTokensRefresh: unit -> Async<unit>
  override TextDocumentPublishDiagnostics: PublishDiagnosticsParams -> Async<unit>
  ///Custom notification for workspace/solution/project loading events
  member NotifyWorkspace: p: PlainNotification -> Async<unit>
  ///Custom notification for initial workspace peek
  member NotifyWorkspacePeek: p: PlainNotification -> Async<unit>
  member NotifyCancelledRequest: p: PlainNotification -> Async<unit>
  member NotifyFileParsed: p: PlainNotification -> Async<unit>
  member NotifyDocumentAnalyzed: p: DocumentAnalyzedNotification -> Async<unit>
  member NotifyTestDetected: p: TestDetectedNotification -> Async<unit>
  member CodeLensRefresh: unit -> Async<unit>
  override WorkDoneProgressCreate: ProgressToken -> AsyncLspResult<unit>
  override Progress: ProgressToken * 'Progress -> Async<unit>

type ServerProgressReport =
  new: lspClient: FSharpLspClient * ?token: ProgressToken -> ServerProgressReport
  member Token: ProgressToken
  member Begin: title: string * ?cancellable: bool * ?message: string * ?percentage: uint -> CancellableTask<unit>
  member Report: ?cancellable: bool * ?message: string * ?percentage: uint -> CancellableTask<unit>
  member End: ?message: string -> CancellableTask<unit>
  interface IAsyncDisposable
  interface IDisposable

/// <summary>listener for the the events generated from the fsc ActivitySource</summary>
type ProgressListener =
  new: lspClient: FSharpLspClient * traceNamespace: string array -> ProgressListener
  interface IDisposable
  interface IAsyncDisposable
