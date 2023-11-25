module rec Utils.Server

open System
open System.IO
open FsAutoComplete.Lsp
open FsAutoComplete
open FsToolkit.ErrorHandling
open Helpers
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol
open FsAutoComplete.LspHelpers
open FSharp.Control.Reactive
open FSharpx.Control
open Expecto
open Utils
open Ionide.ProjInfo.Logging

type Server =
  { RootPath: string option
    Server: IFSharpLspServer
    Events: ClientEvents
    mutable UntitledCounter: int }

/// `Server` cached with `Async.Cache`
type CachedServer = Async<Server>

type Document =
  { Server: Server
    FilePath: string
    Uri: DocumentUri
    mutable Version: int }

  member TextDocumentIdentifier: TextDocumentIdentifier
  member VersionedTextDocumentIdentifier: VersionedTextDocumentIdentifier
  member Diagnostics: IObservable<Diagnostic array>
  member CompilerDiagnostics: IObservable<Diagnostic array>
  interface IDisposable

module Server =
  val create:
    path: string option ->
    config: FSharpConfigDto ->
    createServer: (unit -> IFSharpLspServer * IObservable<string * obj>) ->
      CachedServer

  val shutdown: server: CachedServer -> Async<unit>
  val createUntitledDocument: initialText: string -> server: CachedServer -> Async<Document * Diagnostic array>
  /// `path` can be absolute or relative.
  /// For relative path `server.RootPath` must be specified!
  ///
  /// Note: When `path` is relative: relative to `server.RootPath`!
  val openDocument: path: string -> server: CachedServer -> Async<Document * Diagnostic array>

  /// Like `Server.openDocument`, but instead of reading source text from `path`,
  /// this here instead uses `initialText` (which can be different from content of `path`!).
  ///
  /// This way an existing file with different text can be faked.
  /// Logically equal to `Server.openDocument`, and later changing its text via `Document.changeTextTo`.
  /// But this here doesn't have to parse and check everything twice (once for open, once for changed)
  /// and is WAY faster than `Server.openDocument` followed by `Document.changeTextTo` when involving multiple documents.
  /// (For example with CodeFix tests using `fsi` file and corresponding `fs` file)
  val openDocumentWithText:
    path: string -> initialText: string -> server: CachedServer -> Async<Document * Diagnostic array>

module Document =
  open System.Reactive.Linq
  open System.Threading.Tasks

  val typedEvents: eventName: string -> (ClientEvents -> IObservable<'t>)

  /// `textDocument/publishDiagnostics`
  ///
  /// Note: for each analyzing round there are might be multiple `publishDiagnostics` events (F# compiler, for each built-in Analyzer, for Custom Analyzers)
  ///
  /// Note: Because source `doc.Server.Events` is `ReplaySubject`, subscribing to Stream returns ALL past diagnostics too!
  val diagnosticsStream: doc: Document -> IObservable<Diagnostic array>
  /// `fsharp/documentAnalyzed`
  val analyzedStream: doc: Document -> IObservable<DocumentAnalyzedNotification>
  /// in ms
  /// Waits (if necessary) and gets latest diagnostics.
  ///
  /// To detect newest diags:
  /// * Waits for `fsharp/documentAnalyzed` for passed `doc` and its `doc.Version`.
  /// * Then waits a but more for potential late diags.
  /// * Then returns latest diagnostics.
  ///
  ///
  /// ### Explanation: Get latest & correct diagnostics
  /// Diagnostics aren't collected and then sent once, but instead sent after each parsing/analyzing step.
  /// -> There are multiple `textDocument/publishDiagnostics` sent for each parsing/analyzing round:
  /// * one when file parsed by F# compiler
  /// * one for each built-in (enabled) Analyzers (in `src\FsAutoComplete\FsAutoComplete.Lsp.fs` > `FsAutoComplete.Lsp.FSharpLspServer.analyzeFile`),
  /// * for linter (currently disabled)
  /// * for custom analyzers
  ///
  /// -> To receive ALL diagnostics: use Diagnostics of last `textDocument/publishDiagnostics` event.
  ///
  /// Issue: What is the last `publishDiagnostics`? Might already be here or arrive in future.
  /// -> `fsharp/documentAnalyzed` was introduced. Notification when a doc was completely analyzed
  /// -> wait for `documentAnalyzed`
  ///
  /// But issue: last `publishDiagnostics` might be received AFTER `documentAnalyzed` (because of async notifications & sending)
  /// -> after receiving `documentAnalyzed` wait a bit for late `publishDiagnostics`
  ///
  /// But issue: Wait for how long? Too long: extends test execution time. Too short: Might miss diags.
  /// -> unresolved. Current wait based on testing on modern_ish PC. Seems to work on CI too.
  ///
  ///
  /// *Inconvenience*: Only newest diags can be retrieved this way. Diags for older file versions cannot be extracted reliably:
  /// `doc.Server.Events` is a `ReplaySubject` -> returns ALL previous events on new subscription
  /// -> All past `documentAnalyzed` events and their diags are all received at once
  /// -> waiting a bit after a version-specific `documentAnalyzed` always returns latest diags.
  val waitForLatestDiagnostics: timeout: TimeSpan -> doc: Document -> Async<Diagnostic array>
  val openWith: initialText: string -> doc: Document -> Async<Diagnostic array>
  val close: doc: Document -> Async<unit>
  ///<summary>
  /// Fire a <code>textDocument/didChange</code> request for the specified document with the given text
  /// as the entire new text of the document, then wait for diagnostics for the document.
  /// </summary>
  val changeTextTo: text: string -> doc: Document -> Async<Diagnostic array>
  val saveText: text: string -> doc: Document -> Async<Diagnostic array>

  /// Note: diagnostics aren't filtered to match passed range in here
  val codeActionAt:
    diagnostics: Diagnostic[] -> range: Range -> doc: Document -> Async<TextDocumentCodeActionResult option>

  val inlayHintsAt: range: Range -> doc: Document -> Async<InlayHint array>
  val resolveInlayHint: inlayHint: InlayHint -> doc: Document -> Async<InlayHint>
