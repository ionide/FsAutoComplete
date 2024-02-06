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

#nowarn "44" //we're testing so need to be able to use deprecated fields

let private logger = LogProvider.getLoggerByName "Utils.Server"

type Server =
  { RootPath: string
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

  member doc.TextDocumentIdentifier: TextDocumentIdentifier = { Uri = doc.Uri }

  member doc.VersionedTextDocumentIdentifier: VersionedTextDocumentIdentifier =
    { Uri = doc.Uri; Version = doc.Version }

  member x.Diagnostics =
    x.Server.Events |> fileDiagnosticsForUri x.TextDocumentIdentifier.Uri

  member x.CompilerDiagnostics = x.Diagnostics |> diagnosticsFromSource "F# Compiler"

  interface IDisposable with
    override doc.Dispose() : unit = doc |> Document.close |> Async.RunSynchronously

module Server =
  let private initialize path (config: FSharpConfigDto) createServer =
    async {
      logger.trace (
        Log.setMessage "Initialize Server in {path}"
        >> Log.addContextDestructured "path" path
      )

      let path =
        match path with
        | Some p -> p
        | None -> DisposableDirectory.Create().DirectoryInfo.FullName

      dotnetCleanup path

      for file in System.IO.Directory.EnumerateFiles(path, "*.fsproj", SearchOption.AllDirectories) do
        do! file |> Path.GetDirectoryName |> dotnetRestore

      let (server: IFSharpLspServer, events: IObservable<_>) = createServer ()
      events |> Observable.add logEvent

      let p: InitializeParams =
        { ProcessId = Some 1
          RootPath = None
          Locale = None
          RootUri = None
          InitializationOptions = Some(Server.serialize config)
          Capabilities = Some clientCaps
          ClientInfo =
            Some
              { Name = "FSAC Tests"
                Version = Some "0.0.0" }
          WorkspaceFolders =
            Some
              [| { Uri = Path.FilePathToUri path
                   Name = "Test Folder" } |]
          trace = None }

      match! server.Initialize p with
      | Ok _ ->
        do! server.Initialized(InitializedParams())

        return
          { RootPath = path
            Server = server
            Events = events
            UntitledCounter = 0 }
      | Result.Error error -> return failwith $"Initialization failed: %A{error}"
    }

  let create path config createServer : CachedServer =
    async {
      let! server = initialize path config createServer

      do! waitForWorkspaceFinishedParsing server.Events

      return server
    }
    |> Async.Cache

  let shutdown (server: CachedServer) =
    async {
      let! server = server
      do! server.Server.Shutdown()
    }

  let private createDocument fullPath uri server =
    { Server = server
      Uri = uri
      FilePath = fullPath
      Version = 0 }

  let private untitledDocUrif = sprintf "untitled:Untitled-%i"

  /// Note: mutates passed `server`: increments `server.UntitledCounter`
  let private nextUntitledDocUri (server: Server) =
    let next = System.Threading.Interlocked.Increment(&server.UntitledCounter)
    untitledDocUrif (next - 1)

  let createUntitledDocument initialText (server: CachedServer) =
    async {
      let! server = server

      let doc = server |> createDocument String.Empty (server |> nextUntitledDocUri)

      let! diags = doc |> Document.openWith initialText

      return (doc, diags)
    }

  /// `path` can be absolute or relative.
  /// For relative path `server.RootPath` must be specified!
  ///
  /// Note: When `path` is relative: relative to `server.RootPath`!
  let openDocument (path: string) (server: CachedServer) =
    async {
      let! server = server
      // two possibilities:
      // * relative path -> relative to `server.RootPath` (-> failure when no `RootPath`)
      // * absolute path
      let fullPath =
        if Path.IsPathRooted path then
          path
        else
          Path.Combine(server.RootPath, path)

      let doc =
        server
        |> createDocument
          fullPath
          (fullPath
           // normalize path is necessary: otherwise might be different lower/upper cases in uri for tests and LSP server:
           // on windows `E:\...`: `file:///E%3A/...` (before normalize) vs. `file:///e%3A/..` (after normalize)
           |> normalizePath
           |> Path.LocalPathToUri)

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
  let openDocumentWithText path (initialText: string) (server: CachedServer) =
    async {
      let! server = server

      let fullPath =
        Path.Combine(server.RootPath, path)
        |> Utils.normalizePath
        |> FSharp.UMX.UMX.untag

      // To avoid hitting the typechecker cache, we need to update the file's timestamp
      IO.File.SetLastWriteTimeUtc(fullPath, DateTime.UtcNow)

      let doc = server |> createDocument fullPath (Path.FilePathToUri fullPath)

      let! diags = doc |> Document.openWith initialText

      return (doc, diags)
    }

module Document =
  open System.Reactive.Linq
  open System.Threading.Tasks

  let private typedEvents<'t> typ : _ -> System.IObservable<'t> =
    Observable.choose (fun (typ', _o) -> if typ' = typ then Some(unbox _o) else None)

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


  /// in ms
  let private waitForLateDiagnosticsDelay =
    let envVar = "FSAC_WaitForLateDiagnosticsDelay"

    System.Environment.GetEnvironmentVariable envVar
    |> Option.ofObj
    |> Option.map (fun d ->
      match System.Int32.TryParse d with
      | (true, d) -> d
      | (false, _) -> failwith $"Environment Variable '%s{envVar}' exists, but is not a correct int number ('%s{d}')")
    |> Option.orElseWith (fun _ ->
      // set in Github Actions: https://docs.github.com/en/actions/learn-github-actions/environment-variables#default-environment-variables
      match System.Environment.GetEnvironmentVariable "CI" with
      | null -> None
      | _ -> Some 25)
    |> Option.defaultValue 7 // testing locally

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
  let waitForLatestDiagnostics timeout (doc: Document) : Async<Diagnostic[]> =
    async {
      logger.trace (
        Log.setMessage "Waiting for diags for {uri} at version {version}"
        >> Log.addContext "uri" doc.Uri
        >> Log.addContext "version" doc.Version
      )

      let tcs = TaskCompletionSource<_>()

      use _ =
        doc
        |> diagnosticsStream
        |> Observable.takeUntilOther (
          doc
          // `fsharp/documentAnalyzed` signals all checks & analyzers done
          |> analyzedStream
          |> Observable.filter (fun n -> n.TextDocument.Version = doc.Version)
          // wait for late diagnostics
          |> Observable.delay waitForLateDiagnosticsDelay
        )
        |> Observable.bufferSpan (timeout)
        // |> Observable.timeoutSpan timeout
        |> Observable.subscribe (fun x -> tcs.SetResult x)

      let! result = tcs.Task |> Async.AwaitTask

      return result |> Seq.last
    }


  /// Note: Mutates passed `doc`
  let private incrVersion (doc: Document) = System.Threading.Interlocked.Increment(&doc.Version)

  /// Note: Mutates passed `doc`
  let private incrVersionedTextDocumentIdentifier (doc: Document) : VersionedTextDocumentIdentifier =
    { Uri = doc.Uri
      Version = incrVersion doc }

  let openWith initialText (doc: Document) =
    async {
      let p: DidOpenTextDocumentParams =
        { TextDocument =
            { Uri = doc.Uri
              LanguageId = "fsharp"
              Version = doc.Version
              Text = initialText } }

      do! doc.Server.Server.TextDocumentDidOpen p

      try
        return! doc |> waitForLatestDiagnostics Helpers.defaultTimeout
      with :? TimeoutException ->
        return failwith $"Timeout waiting for latest diagnostics for {doc.Uri}"
    }

  let close (doc: Document) =
    async {
      let p: DidCloseTextDocumentParams = { TextDocument = doc.TextDocumentIdentifier }
      do! doc.Server.Server.TextDocumentDidClose p
    }

  ///<summary>
  /// Fire a <code>textDocument/didChange</code> request for the specified document with the given text
  /// as the entire new text of the document, then wait for diagnostics for the document.
  /// </summary>
  let changeTextTo (text: string) (doc: Document) =
    async {
      let p: DidChangeTextDocumentParams =
        { TextDocument = doc |> incrVersionedTextDocumentIdentifier
          ContentChanges =
            [| { Range = None
                 RangeLength = None
                 Text = text } |] }

      do! doc.Server.Server.TextDocumentDidChange p
      do! Async.Sleep(TimeSpan.FromMilliseconds 250.)
      return! doc |> waitForLatestDiagnostics Helpers.defaultTimeout
    }

  let saveText (text: string) (doc: Document) =
    async {
      let p: DidSaveTextDocumentParams =
        { Text = Some text
          TextDocument = doc.TextDocumentIdentifier }
      // Simulate the file being written to disk so we don't hit the typechecker cache
      IO.File.SetLastWriteTimeUtc(doc.FilePath, DateTime.UtcNow)
      do! doc.Server.Server.TextDocumentDidSave p
      do! Async.Sleep(TimeSpan.FromMilliseconds 250.)
      return! doc |> waitForLatestDiagnostics Helpers.defaultTimeout
    }

  let private assertOk result =
    Expect.isOk result "Expected success"

    result |> Result.defaultWith (fun _ -> failtest "not reachable")

  let private assertSome opt =
    Expect.isSome opt "Expected to have Some"

    opt |> Option.defaultWith (fun _ -> failtest "not reachable")

  /// Note: diagnostics aren't filtered to match passed range in here
  let codeActionAt (diagnostics: Diagnostic[]) (range: Range) (doc: Document) =
    async {
      let ps: CodeActionParams =
        { TextDocument = doc.TextDocumentIdentifier
          Range = range
          Context =
            { Diagnostics = diagnostics
              Only = None
              TriggerKind = None } }

      let! res = doc.Server.Server.TextDocumentCodeAction ps
      return res |> assertOk
    }

  let inlayHintsAt range (doc: Document) =
    async {
      let ps: InlayHintParams =
        { Range = range
          TextDocument = doc.TextDocumentIdentifier }

      let! res = doc.Server.Server.TextDocumentInlayHint ps
      return res |> assertOk |> assertSome
    }

  let resolveInlayHint inlayHint (doc: Document) =
    async {
      let! res = doc.Server.Server.InlayHintResolve inlayHint
      return res |> assertOk
    }
