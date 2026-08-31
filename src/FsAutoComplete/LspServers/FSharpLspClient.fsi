namespace FsAutoComplete.Lsp

open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc
open FsAutoComplete.LspHelpers
open System
open System.Threading
open System.Threading.Tasks
open IcedTasks

type FSharpLspClient =
  new: sendServerNotification: ClientNotificationSender * sendServerRequest: ClientRequestSender -> FSharpLspClient
  inherit LspClient
  member ClientCapabilities: ClientCapabilities option with get, set
  override WindowShowMessage: ShowMessageParams -> Async<unit>
  override WindowShowMessageRequest: ShowMessageRequestParams -> AsyncLspResult<MessageActionItem option>
  override WindowLogMessage: LogMessageParams -> Async<unit>
  override WindowShowDocument: ShowDocumentParams -> AsyncLspResult<ShowDocumentResult>
  override TelemetryEvent: Newtonsoft.Json.Linq.JToken -> Async<unit>
  override ClientRegisterCapability: RegistrationParams -> AsyncLspResult<unit>
  override ClientUnregisterCapability: UnregistrationParams -> AsyncLspResult<unit>
  override WorkspaceWorkspaceFolders: unit -> AsyncLspResult<WorkspaceFolder array option>
  override WorkspaceConfiguration: ConfigurationParams -> AsyncLspResult<Newtonsoft.Json.Linq.JToken array>
  override WorkspaceApplyEdit: ApplyWorkspaceEditParams -> AsyncLspResult<ApplyWorkspaceEditResult>
  override WorkspaceSemanticTokensRefresh: unit -> AsyncLspResult<unit>
  override TextDocumentPublishDiagnostics: PublishDiagnosticsParams -> Async<unit>
  ///Custom notification for workspace/solution/project loading events
  member NotifyWorkspace: p: PlainNotification -> Async<unit>
  ///Custom notification for initial workspace peek
  member NotifyWorkspacePeek: p: PlainNotification -> Async<unit>
  member NotifyCancelledRequest: p: PlainNotification -> Async<unit>
  member NotifyFileParsed: p: PlainNotification -> Async<unit>
  member NotifyDocumentAnalyzed: p: DocumentAnalyzedNotification -> Async<unit>
  member NotifyTestDetected: p: TestDetectedNotification -> Async<unit>
  member NotifyTestDiscoveryUpdate: p: TestDiscoveryUpdateNotification -> Async<unit>
  member NotifyTestRunUpdate: p: TestRunProgress -> Async<unit>
  member AttachDebuggerForTestRun: processId: int -> AsyncLspResult<bool>
  member CodeLensRefresh: unit -> Async<unit>
  override WindowWorkDoneProgressCreate: WorkDoneProgressCreateParams -> AsyncLspResult<unit>
  member Progress: ProgressToken * 'Progress -> Async<unit>
  override Progress: ProgressParams -> Async<unit>

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
  member Begin: title: string * ?cancellable: bool * ?message: string * ?percentage: uint -> CancellableTask<unit>
  /// <summary>Report additional progress</summary>
  /// <param name="cancellable">Controls if a cancel button should show to allow the user to cancel the long running operation</param>
  /// <param name="message">more detailed associated progress message. Contains complementary information to the `title`.</param>
  /// <param name="percentage">percentage to display (value 100 is considered 100%). If not provided infinite progress is assumed</param>
  member Report: ?cancellable: bool * ?message: string * ?percentage: uint -> CancellableTask<unit>
  /// <summary>Signaling the end of a progress reporting is done.</summary>
  /// <param name="message">more detailed associated progress message. Contains complementary information to the `title`.</param>
  /// <remarks>
  /// This will be called if this object is disposed either via Dispose or DisposeAsync.
  /// </remarks>
  /// <returns></returns>
  member End: ?message: string -> CancellableTask<unit>
  interface IAsyncDisposable
  interface IDisposable

/// <summary>
/// A shared progress reporter that consolidates multiple concurrent typecheck operations
/// into a single LSP progress notification. Instead of creating a new Begin/End cycle per file,
/// this maintains one notification that updates its message with the current file being checked.
/// </summary>
type SharedTypecheckProgressReporter =
  new:
    title: string * createReport: (unit -> ServerProgressReport) * ?isEnabled: (unit -> bool) ->
      SharedTypecheckProgressReporter

  /// <summary>Begin tracking a file being typechecked. Returns an IAsyncDisposable that ends tracking on dispose.</summary>
  member Begin: fileName: string -> CancellableTask<IAsyncDisposable>

  /// <summary>Set up a batch of files to be typechecked. Returns an IAsyncDisposable that clears the batch on dispose.</summary>
  member BeginBatch: files: string array -> CancellableTask<IAsyncDisposable>

  /// <summary>Gets the cancellation token from the current progress report, or CancellationToken.None if no report is active.</summary>
  member GetCancellationToken: unit -> Task<CancellationToken>

  /// <summary>Set up a batch of files synchronously (for contexts where CancellableTask is not available).
  /// Sets up batch tracking and adds all files to activeFiles so their names appear in the message.
  /// Returns an IDisposable that ends all file tracking and the batch on dispose.</summary>
  member BeginBatchSync: files: string array -> IDisposable
  interface IDisposable

/// <summary>listener for the the events generated from the fsc ActivitySource</summary>
type ProgressListener =
  new: lspClient: FSharpLspClient * traceNamespace: string array -> ProgressListener
  interface IDisposable
  interface IAsyncDisposable
