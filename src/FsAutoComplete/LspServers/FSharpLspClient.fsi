namespace FsAutoComplete.Lsp

open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete.LspHelpers
open System
open System.Threading
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
  override WorkspaceApplyEdit: ApplyWorkspaceEditParams -> AsyncLspResult<ApplyWorkspaceEditResult>
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

///<summary>
/// Represents a progress report that can be used to report progress to the client.
/// </summary>
///
/// <remarks>
/// This implements <see cref="T:System.IAsyncDisposable"/> and <see cref="T:System.IDisposable"/> to allow for the ending of the progress report without explicitly calling End.
///
/// See <see href="https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workDoneProgress">LSP Spec on WorkDoneProgress</see> for more information.
/// </remarks>
type ServerProgressReport =
  new: lspClient: FSharpLspClient * ?token: ProgressToken * ?cancellableDefault: bool -> ServerProgressReport
  /// The progress token to identify the progress report.
  member ProgressToken: ProgressToken
  /// A cancellation token that can be used to used to cancel actions that are associated with this progress report.
  member CancellationToken: CancellationToken
  /// Triggers the CancellationToken to cancel.
  member Cancel: unit -> unit
  /// <summary>Used to start reporting progress to the client. </summary>
  /// <param name="title">Mandatory title of the progress operation</param>
  /// <param name="cancellable">Controls if a cancel button should show to allow the user to cancel the long running operation</param>
  /// <param name="message">more detailed associated progress message. Contains complementary information to the `title`.</param>
  /// <param name="percentage">percentage to display (value 100 is considered 100%). If not provided infinite progress is assumed</param>
  member Begin: title: string * ?cancellable: bool * ?message: string * ?percentage: uint -> CancellableValueTask<unit>
  /// <summary>Report additional progress</summary>
  /// <param name="cancellable">Controls if a cancel button should show to allow the user to cancel the long running operation</param>
  /// <param name="message">more detailed associated progress message. Contains complementary information to the `title`.</param>
  /// <param name="percentage">percentage to display (value 100 is considered 100%). If not provided infinite progress is assumed</param>
  member Report: ?cancellable: bool * ?message: string * ?percentage: uint -> CancellableValueTask<unit>
  /// <summary>Signaling the end of a progress reporting is done.</summary>
  /// <param name="message">more detailed associated progress message. Contains complementary information to the `title`.</param>
  /// <remarks>
  /// This will be called if this object is disposed either via Dispose or DisposeAsync.
  /// </remarks>
  /// <returns></returns>
  member End: ?message: string -> CancellableValueTask<unit>
  interface IAsyncDisposable
  interface IDisposable

/// <summary>listener for the the events generated from the fsc ActivitySource</summary>
type ProgressListener =
  new: lspClient: FSharpLspClient * traceNamespace: string array -> ProgressListener
  interface IDisposable
  interface IAsyncDisposable
