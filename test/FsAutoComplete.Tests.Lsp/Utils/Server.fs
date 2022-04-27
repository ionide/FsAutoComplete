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

let private logger = LogProvider.getLoggerByName "Utils.Server"

type Server = {
  RootPath: string option
  Server: FSharpLspServer
  Events: ClientEvents
  mutable UntitledCounter: int
}
/// `Server` cached with `Async.Cache`
type CachedServer = Async<Server>
type Document =
  {
    Server: Server
    Uri: DocumentUri
    mutable Version: int
  }
  member doc.TextDocumentIdentifier =
    { Uri = doc.Uri }
  member doc.VersionedTextDocumentIdentifier =
    { Uri = doc.Uri; Version = Some doc.Version }

  interface IDisposable with
    override doc.Dispose(): unit =
      doc
      |> Document.close
      |> Async.RunSynchronously


module Server =
  let private initialize path (config: FSharpConfigDto) state = async {
    logger.trace (
      Log.setMessage "Initialize Server in {path}"
      >> Log.addContextDestructured "path" path
    )

    match path with
    | None -> ()
    | Some path ->
        dotnetCleanup path
        if
          System.IO.Directory.EnumerateFiles(path, "*.fsproj")
          |> Seq.isEmpty
          |> not
        then
          do! dotnetRestore path

    let (server, events) = createServer state
    events
    |> Observable.add logEvent

    let p: InitializeParams = {
      ProcessId = Some 1
      RootPath = path
      RootUri = path |> Option.map (sprintf "file://%s")
      InitializationOptions = Some (Server.serialize config)
      Capabilities = Some clientCaps
      trace = None
    }
    match! server.Initialize p with
    | Ok _ ->
        return {
          RootPath = path
          Server = server
          Events = events
          UntitledCounter = 0
        }
    | Result.Error error ->
        return failwith $"Initialization failed: %A{error}"
  }

  let create path config state : CachedServer =
    async {
      let! server = initialize path config state

      if path |> Option.isSome then
        do! waitForWorkspaceFinishedParsing server.Events

      return server
    }
    |> Async.Cache

  let shutdown (server: CachedServer) = async {
    let! server = server
    do! server.Server.Shutdown ()
  }

  let private createDocument uri server = {
    Server = server
    Uri = uri
    Version = 0
  }

  let private untitledDocUrif = sprintf "untitled:Untitled-%i"
  /// Note: mutates passed `server`: increments `server.UntitledCounter`
  let private nextUntitledDocUri (server: Server) =
    let next = System.Threading.Interlocked.Increment(&server.UntitledCounter)
    untitledDocUrif (next-1)

  let createUntitledDocument initialText (server: CachedServer) = async {
    let! server = server

    let doc = server |> createDocument (server |> nextUntitledDocUri)
    let! diags = doc |> Document.openWith initialText

    return (doc, diags)
  }

  /// `path` can be absolute or relative.
  /// For relative path `server.RootPath` must be specified!
  ///
  /// Note: When `path` is relative: relative to `server.RootPath`!
  let openDocument (path: string) (server: CachedServer) = async {
    let! server = server
    // two possibilities:
    // * relative path -> relative to `server.RootPath` (-> failure when no `RootPath`)
    // * absolute path
    let fullPath =
      if Path.IsPathRooted path then
        path
      else
        Expect.isSome server.RootPath "relative path is only possible when `server.RootPath` is specified!"
        Path.Combine(server.RootPath.Value, path)
    let doc =
      server
      |> createDocument (
            fullPath
            // normalize path is necessary: otherwise might be different lower/upper cases in uri for tests and LSP server:
            // on windows `E:\...`: `file:///E%3A/...` (before normalize) vs. `file:///e%3A/..` (after normalize)
            |> normalizePath
            |> Path.LocalPathToUri
         )
    let! diags = doc |> Document.openWith (File.ReadAllText fullPath)

    return (doc, diags)
  }

  /// Like `Server.openDocument`, but instead of reading source text from `path`,
  /// this here instead uses `initialText` (which can be different from content of `path`!).
  ///
  /// This way an existing file with different text can be faked.
  /// Logically equal to `Server.openDocument`, and later changing its text via `Document.changeTextTo`.
  /// But this here doesn't have to parse and check everything twice (once for open, once for changed)
  /// and is WAY faster than `Server.openDocument` followed by `Document.changeTextTo` when involving multiple documents.
  /// (For example with CodeFix tests using `fsi` file and corresponding `fs` file)
  let openDocumentWithText path (initialText: string) (server: CachedServer) = async {
    let! server = server
    assert(server.RootPath |> Option.isSome)

    let fullPath = Path.Combine(server.RootPath.Value, path)
    let doc = server |> createDocument (Path.FilePathToUri fullPath)
    let! diags = doc |> Document.openWith initialText

    return (doc, diags)
  }

module Document =
  let private typedEvents<'t> typ : _ -> System.IObservable<'t> =
    Observable.choose (fun (typ', _o) -> if typ' = typ then Some (unbox _o) else None)

  /// `textDocument/publishDiagnostics`
  ///
  /// Note: for each analyzing round there are might be multiple `publishDiagnostics` events (F# compiler, for each built-in Analyzer, for Custom Analyzers)
  ///
  /// Note: Because source `doc.Server.Events` is `ReplaySubject`, subscribing to Stream returns ALL past diagnostics too!
  let diagnosticsStream (doc: Document) =
    doc.Server.Events
    |> typedEvents<PublishDiagnosticsParams> "textDocument/publishDiagnostics"
    |> Observable.choose (fun n -> if n.Uri = doc.Uri then Some n.Diagnostics else None)
  /// `fsharp/documentAnalyzed`
  let analyzedStream (doc: Document) =
    doc.Server.Events
    |> typedEvents<DocumentAnalyzedNotification> "fsharp/documentAnalyzed"
    |> Observable.filter (fun n -> n.TextDocument.Uri = doc.Uri)

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
  //ENHANCEMENT: Send `publishDiagnostics` with Doc Version (LSP `3.15.0`) -> can correlate `documentAnalyzed` and `publishDiagnostics`
  let waitForLatestDiagnostics timeout (doc: Document) : Async<Diagnostic[]> = async {
    logger.trace (
      Log.setMessage "Waiting for diags for {uri} at version {version}"
      >> Log.addContext "uri" doc.Uri
      >> Log.addContext "version" doc.Version
    )

    return!
      doc
      |> diagnosticsStream
      |> Observable.takeUntilOther (
          doc
          // `fsharp/documentAnalyzed` signals all checks & analyzers done
          |> analyzedStream
          |> Observable.filter (fun n -> n.TextDocument.Version = Some doc.Version)
          // wait for late diagnostics
          |> Observable.delay 5
      )
      |> Observable.last
      |> Observable.timeoutSpan timeout
      |> Async.AwaitObservable
  }
  let private defaultTimeout = TimeSpan.FromSeconds 5.0

  /// Note: Mutates passed `doc`
  let private incrVersion (doc: Document) =
    System.Threading.Interlocked.Increment(&doc.Version)
  /// Note: Mutates passed `doc`
  let private incrVersionedTextDocumentIdentifier (doc: Document) =
    { Uri = doc.Uri; Version = Some (doc |> incrVersion) }


  let openWith initialText (doc: Document) = async {
    let p: DidOpenTextDocumentParams = {
      TextDocument = {
        Uri = doc.Uri
        LanguageId = "fsharp"
        Version = doc.Version
        Text = initialText
      }
    }
    do! doc.Server.Server.TextDocumentDidOpen p
    try
      return!
        doc
        |> waitForLatestDiagnostics defaultTimeout
    with
      | :? TimeoutException ->
        return failwith $"Timeout waiting for latest diagnostics for {doc.Uri}"
  }

  let close (doc: Document) = async {
    let p: DidCloseTextDocumentParams = {
      TextDocument = doc.TextDocumentIdentifier
    }
    do! doc.Server.Server.TextDocumentDidClose p
  }

  let changeTextTo (text: string) (doc: Document) = async {
    let p: DidChangeTextDocumentParams = {
        TextDocument = doc |> incrVersionedTextDocumentIdentifier
        ContentChanges = [|
          {
            Range = None
            RangeLength = None
            Text = text
          }
        |]
    }
    do! doc.Server.Server.TextDocumentDidChange p
    return!
      doc
      |> waitForLatestDiagnostics defaultTimeout
  }

  let private assertOk result =
    Expect.isOk result "Expected success"
    result |> Result.defaultWith (fun _ -> failtest "not reachable")

  /// Note: diagnostics aren't filtered to match passed range in here
  let codeActionAt (diagnostics: Diagnostic[]) (range: Range) (doc: Document) = async {
    let ps: CodeActionParams = {
      TextDocument = doc.TextDocumentIdentifier
      Range = range
      Context = { Diagnostics = diagnostics }
    }
    let! res = doc.Server.Server.TextDocumentCodeAction ps
    return res |> assertOk
  }

  let inlayHintsAt range (doc: Document) = async {
    let ps: FSharpInlayHintsRequest = {
      Range =  range
      TextDocument = doc.TextDocumentIdentifier
    }
    let! res = doc.Server.Server.FSharpInlayHints(ps)
    return res |> assertOk
  }
