module Helpers

open System
open Expecto
open System.IO
open FsAutoComplete.Lsp
open LanguageServerProtocol
open LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.Control.Reactive
open System.Threading
open FSharp.UMX

let rec private copyDirectory sourceDir destDir =
  // Get the subdirectories for the specified directory.
      let dir = DirectoryInfo(sourceDir);

      if not dir.Exists then raise (DirectoryNotFoundException("Source directory does not exist or could not be found: " + sourceDir))

      let dirs = dir.GetDirectories()

      // If the destination directory doesn't exist, create it.
      Directory.CreateDirectory(destDir) |> ignore

      // Get the files in the directory and copy them to the new location.
      dir.GetFiles()
      |> Seq.iter (fun file ->
          let tempPath = Path.Combine(destDir, file.Name);
          file.CopyTo (tempPath, false) |> ignore
      )

      // If copying subdirectories, copy them and their contents to new location.
      dirs
      |> Seq.iter (fun dir ->
          let tempPath = Path.Combine(destDir, dir.Name);
          copyDirectory dir.FullName tempPath
      )
type DisposableDirectory (directory : string) =
    static member Create() =
        let tempPath = IO.Path.Combine(IO.Path.GetTempPath(), Guid.NewGuid().ToString("n"))
        printfn "Creating directory %s" tempPath
        IO.Directory.CreateDirectory tempPath |> ignore
        new DisposableDirectory(tempPath)

    static member From sourceDir =
      let self = DisposableDirectory.Create()
      copyDirectory sourceDir self.DirectoryInfo.FullName
      self

    member x.DirectoryInfo: DirectoryInfo = IO.DirectoryInfo(directory)
    interface IDisposable with
        member x.Dispose() =
            printfn "Deleting directory %s" x.DirectoryInfo.FullName
            IO.Directory.Delete(x.DirectoryInfo.FullName,true)



/// Helper that can be used for writing CPS-style code that resumes
/// on the same thread where the operation was started.
let synchronize f =
  let ctx = System.Threading.SynchronizationContext.Current
  f (fun g ->
    let nctx = System.Threading.SynchronizationContext.Current
    if ctx <> null && ctx <> nctx then ctx.Post((fun _ -> g()), null)
    else g() )

type Async =
  /// Creates an asynchronous workflow that non-deterministically returns the
  /// result of one of the two specified workflows (the one that completes
  /// first). This is similar to Task.WhenAny.
  static member WhenAny([<ParamArray>] works:Async<'T>[]) : Async<'T> =
    Async.FromContinuations(fun (cont, econt, ccont) ->
      // Results from the two
      let results = Array.map (fun _ -> Choice1Of3()) works
      let handled = ref false
      let lockObj = new obj()
      let synchronized f = lock lockObj f

      // Called when one of the workflows completes
      let complete () =
        let op =
          synchronized (fun () ->
            // If we already handled result (and called continuation)
            // then ignore. Otherwise, if the computation succeeds, then
            // run the continuation and mark state as handled.
            // Only throw if all workflows failed.
            if !handled then ignore
            else
              let succ = Seq.tryPick (function Choice2Of3 v -> Some v | _ -> None) results
              match succ with
              | Some value -> handled := true; (fun () -> cont value)
              | _ ->
                  if Seq.forall (function Choice3Of3 _ -> true | _ -> false) results then
                    let exs = Array.map (function Choice3Of3 ex -> ex | _ -> failwith "!") results
                    (fun () -> econt (AggregateException(exs)))
                  else ignore )
        // Actually run the continuation
        // (this shouldn't be done in the lock)
        op()

            // Run a workflow and write result (or exception to a ref cell)
      let run index workflow = async {
        try
          let! res = workflow
          synchronized (fun () -> results.[index] <- Choice2Of3 res)
        with e ->
          synchronized (fun () -> results.[index] <- Choice3Of3 e)
        complete() }

      // Start all work items - using StartImmediate, because it
      // should be started on the current synchronization context
      works |> Seq.iteri (fun index work ->
        Async.StartImmediate(run index work)) )

  /// Creates an asynchronous workflow that will be resumed when the
  /// specified observables produces a value. The workflow will return
  /// the value produced by the observable.
  static member AwaitObservable(observable : IObservable<'T1>) =
      let removeObj : IDisposable option ref = ref None
      let removeLock = new obj()
      let setRemover r =
          lock removeLock (fun () -> removeObj := Some r)
      let remove() =
          lock removeLock (fun () ->
              match !removeObj with
              | Some d -> removeObj := None
                          d.Dispose()
              | None   -> ())
      synchronize (fun f ->
        let workflow =
            Async.FromContinuations((fun (cont,econt,ccont) ->
                let rec finish cont value =
                    remove()
                    f (fun () -> cont value)
                setRemover <|
                    observable.Subscribe
                        ({ new IObserver<_> with
                            member x.OnNext(v) = finish cont v
                            member x.OnError(e) = finish econt e
                            member x.OnCompleted() =
                                let msg = "Cancelling the workflow, because the Observable awaited using AwaitObservable has completed."
                                finish ccont (new System.OperationCanceledException(msg)) })
                () ))
        async {
            let! cToken = Async.CancellationToken
            let token : CancellationToken = cToken
            use registration = token.Register((fun _ -> remove()), null)
            return! workflow
        })

  /// Creates an asynchronous workflow that runs the asynchronous workflow
  /// given as an argument at most once. When the returned workflow is
  /// started for the second time, it reuses the result of the
  /// previous execution.
  static member Cache (input:Async<'T>) =
      let agent = MailboxProcessor<AsyncReplyChannel<_>>.Start(fun agent -> async {
          let! repl = agent.Receive()
          let! res = input |> Async.Catch
          repl.Reply(res)
          while true do
              let! repl = agent.Receive()
              repl.Reply(res) })

      async {
        let! result = agent.PostAndAsyncReply(id)
        return match result with | Choice1Of2 v -> v | Choice2Of2 exn -> raise exn
      }

let logger = Expecto.Logging.Log.create "LSPTests"

type ClientEvents = IObservable<string * obj>

let createServer (state: State) =
  let event = new System.Reactive.Subjects.ReplaySubject<_>()
  let client = FSharpLspClient ((fun name o -> event.OnNext (name ,o); AsyncLspResult.success ()), { new LanguageServerProtocol.Server.ClientRequestSender with member __.Send _ _ = AsyncLspResult.notImplemented})
  let originalFs = FSharp.Compiler.SourceCodeServices.FileSystemAutoOpens.FileSystem
  let fs = FsAutoComplete.FileSystem(originalFs, state.Files.TryFind)
  FSharp.Compiler.SourceCodeServices.FileSystemAutoOpens.FileSystem <- fs
  let server = new FSharpLspServer(false, state, client)
  server, event :> ClientEvents

let defaultConfigDto : FSharpConfigDto =
  { WorkspaceModePeekDeepLevel = None
    ExcludeProjectDirectories = None
    KeywordsAutocomplete = None
    ExternalAutocomplete = None
    Linter = None
    LinterConfig = None
    UnionCaseStubGeneration = None
    UnionCaseStubGenerationBody = None
    RecordStubGeneration = None
    RecordStubGenerationBody = None
    UnusedOpensAnalyzer = None
    UnusedDeclarationsAnalyzer = None
    SimplifyNameAnalyzer = None
    ResolveNamespaces = None
    EnableReferenceCodeLens = None
    EnableAnalyzers = None
    AnalyzersPath = None
    DisableInMemoryProjectReferences = None
    AutomaticWorkspaceInit = Some true
    InterfaceStubGeneration = None
    InterfaceStubGenerationObjectIdentifier = None
    InterfaceStubGenerationMethodBody = None
    LineLens = None
    UseSdkScripts = Some true
    DotNetRoot = None
    FSIExtraParameters = None
    FSICompilerToolLocations = None
    TooltipMode = None
    GenerateBinlog = None
    AbstractClassStubGeneration = None
    AbstractClassStubGenerationMethodBody = None
    AbstractClassStubGenerationObjectIdentifier = None }

let clientCaps : ClientCapabilities =
  let dynCaps : DynamicCapabilities = { DynamicRegistration = Some true}
  let workspaceCaps : WorkspaceClientCapabilities =
    let weCaps : WorkspaceEditCapabilities = { DocumentChanges = Some true}
    let symbolCaps: SymbolCapabilities = { DynamicRegistration = Some true
                                           SymbolKind = None}
    let semanticTokenCaps: SemanticTokensWorkspaceClientCapabilities = { RefreshSupport = Some true }

    { ApplyEdit = Some true
      WorkspaceEdit = Some weCaps
      DidChangeConfiguration = Some dynCaps
      DidChangeWatchedFiles = Some dynCaps
      Symbol = Some symbolCaps
      SemanticTokens = Some semanticTokenCaps }

  let textCaps: TextDocumentClientCapabilities =
    let syncCaps : SynchronizationCapabilities =
      { DynamicRegistration = Some true
        WillSave = Some true
        WillSaveWaitUntil = Some true
        DidSave = Some true}

    let diagCaps: PublishDiagnosticsCapabilites =
      let diagnosticTags: DiagnosticTagSupport = { ValueSet = [||] }
      { RelatedInformation = Some true
        TagSupport = Some diagnosticTags }

    let ciCaps: CompletionItemCapabilities =
      { SnippetSupport = Some true
        CommitCharactersSupport = Some true
        DocumentationFormat = None}

    let cikCaps: CompletionItemKindCapabilities = { ValueSet = None}

    let compCaps: CompletionCapabilities =
      { DynamicRegistration = Some true
        CompletionItem = Some ciCaps
        CompletionItemKind = Some cikCaps
        ContextSupport = Some true}

    let hoverCaps: HoverCapabilities =
      { DynamicRegistration = Some true
        ContentFormat = Some [| "markdown" |]}

    let sigCaps: SignatureHelpCapabilities =
      let siCaps: SignatureInformationCapabilities = { DocumentationFormat = Some [| "markdown" |]}
      { DynamicRegistration = Some true
        SignatureInformation = Some siCaps}

    let docSymCaps: DocumentSymbolCapabilities =
      let skCaps: SymbolKindCapabilities = { ValueSet = None}
      { DynamicRegistration = Some true
        SymbolKind = Some skCaps}

    let foldingRangeCaps: FoldingRangeCapabilities =
      { DynamicRegistration = Some true
        LineFoldingOnly = Some true
        RangeLimit = Some 100 }

    let semanticTokensCaps: SemanticTokensClientCapabilities =
      {
        DynamicRegistration = Some true
        Requests = {
          Range = Some (U2.First true)
          Full = Some (U2.First true)
        }
        TokenTypes = [| |]
        TokenModifiers = [| |]
        Formats = [| TokenFormat.Relative |]
        OverlappingTokenSupport = None
        MultilineTokenSupport = None
      }

    { Synchronization = Some syncCaps
      PublishDiagnostics = diagCaps
      Completion = Some compCaps
      Hover = Some hoverCaps
      SignatureHelp = Some sigCaps
      References = Some dynCaps
      DocumentHighlight = Some dynCaps
      DocumentSymbol = Some docSymCaps
      Formatting = Some dynCaps
      RangeFormatting = Some dynCaps
      OnTypeFormatting = Some dynCaps
      Definition = Some dynCaps
      CodeAction = Some dynCaps
      CodeLens = Some dynCaps
      DocumentLink = Some dynCaps
      Rename = Some dynCaps
      FoldingRange = Some foldingRangeCaps
      SelectionRange = Some dynCaps
      SemanticTokens = Some semanticTokensCaps }


  { Workspace = Some workspaceCaps
    TextDocument = Some textCaps
    Experimental = None}

open Expecto.Logging
open Expecto.Logging.Message
open System.Threading
open FsAutoComplete.CommandResponse


let logEvent (name, payload) =
  logger.debug (eventX "{name}: {payload}" >> setField "name" name >> setField "payload" payload)

let logDotnetRestore section line =
  if not (String.IsNullOrWhiteSpace(line)) then
    logger.debug (eventX "[{section}] dotnet restore: {line}" >> setField "section" section >> setField "line" line)

let dotnetCleanup baseDir =
  ["obj"; "bin"]
  |> List.map (fun f -> Path.Combine(baseDir, f))
  |> List.filter Directory.Exists
  |> List.iter (fun path -> Directory.Delete(path, true))

let runProcess (log: string -> unit) (workingDir: string) (exePath: string) (args: string) = async {
  let psi = System.Diagnostics.ProcessStartInfo()
  psi.FileName <- exePath
  psi.WorkingDirectory <- workingDir
  psi.RedirectStandardOutput <- true
  psi.RedirectStandardError <- true
  psi.Arguments <- args
  psi.CreateNoWindow <- true
  psi.UseShellExecute <- false

  use p = new System.Diagnostics.Process()
  p.StartInfo <- psi

  p.OutputDataReceived.Add(fun ea -> log (ea.Data))

  p.ErrorDataReceived.Add(fun ea -> log (ea.Data))

  let! ctok = Async.CancellationToken
  p.Start() |> ignore<bool>
  p.BeginOutputReadLine()
  p.BeginErrorReadLine()
  do! p.WaitForExitAsync(ctok) |> Async.AwaitTask

  let exitCode = p.ExitCode

  return exitCode, (workingDir, exePath, args)
}

let inline expectExitCodeZero (exitCode, _) =
  Expect.equal exitCode 0 (sprintf "expected exit code zero but was %i" exitCode)

let serverInitialize path (config: FSharpConfigDto) state = async {
  dotnetCleanup path
  let files = Directory.GetFiles(path)

  if files |> Seq.exists (fun p -> p.EndsWith ".fsproj") then
    do! runProcess (logDotnetRestore ("Restore" + path)) path "dotnet" "restore"
        |> Async.map expectExitCodeZero

  let server, event = createServer state

  event
  |> Observable.add logEvent

  let p : InitializeParams =
    { ProcessId = Some 1
      RootPath = Some path
      RootUri = Some (sprintf "file://%s" path)
      InitializationOptions = Some (Server.serialize config)
      Capabilities = Some clientCaps
      trace = None }

  let! result = server.Initialize p
  match result with
  | Result.Ok res -> return (server, event)
  | Result.Error e ->
    return failwith "Initialization failed"
}

let loadDocument path : TextDocumentItem =
  { Uri = Path.FilePathToUri path
    LanguageId = "fsharp"
    Version = 0
    Text = File.ReadAllText path  }

let parseProject projectFilePath (server: FSharpLspServer) = async {
  let projectParams: ProjectParms =
    { Project = { Uri = Path.FilePathToUri projectFilePath } }

  let projectName = Path.GetFileNameWithoutExtension projectFilePath
  let! result = server.FSharpProject projectParams
  do! Async.Sleep (TimeSpan.FromSeconds 3.)
  logger.debug (eventX "{project} parse result: {result}" >> setField "result" (sprintf "%A" result) >> setField "project" projectName)
}

let (|UnwrappedPlainNotification|_|) eventType (notification: PlainNotification): 't option =
  notification.Content
  |> JsonSerializer.readJson<ResponseMsg<'t>>
  |> fun r -> if r.Kind = eventType then Some r.Data else None

let waitForWorkspaceFinishedParsing (events : ClientEvents) =
  let chooser (name, payload) =
    match name, unbox payload with
    | "fsharp/notifyWorkspace", (UnwrappedPlainNotification "workspaceLoad" (workspaceLoadResponse: FsAutoComplete.CommandResponse.WorkspaceLoadResponse) )->
      if workspaceLoadResponse.Status = "finished" then Some () else None
    | _ -> None

  logger.debug (eventX "waiting for workspace to finish loading")
  events
  |> Observable.choose chooser
  |> Async.AwaitObservable

let private typedEvents<'t> typ =
  Observable.choose (fun (typ', _o) -> if typ' = typ then Some (unbox _o) else None)

let private payloadAs<'t> =
  Observable.map (fun (_typ, o) -> unbox<'t> o)

let private getDiagnosticsEvents =
  typedEvents<LanguageServerProtocol.Types.PublishDiagnosticsParams> "textDocument/publishDiagnostics"

/// note that the files here are intended to be the filename only., not the full URI.
let private matchFiles (files: string Set) =
  Observable.choose (fun (p: LanguageServerProtocol.Types.PublishDiagnosticsParams) ->
    let filename = p.Uri.Split([| '/'; '\\' |], StringSplitOptions.RemoveEmptyEntries) |> Array.last
    if Set.contains filename files
    then Some (filename, p)
    else None
  )

let fileDiagnostics file (events: ClientEvents) =
  logger.info (eventX "waiting for events on file {file}" >> setField "file" file)
  events
  |> getDiagnosticsEvents
  |> matchFiles (Set.ofList [file])

let analyzerEvents file events =
  fileDiagnostics file events
  |> Observable.map snd
  |> Observable.filter (fun payload -> payload.Diagnostics |> Array.exists (fun d -> d.Source.StartsWith "F# Analyzers"))

let waitForParseResultsForFile file (events: ClientEvents) =
  let matchingFileEvents = fileDiagnostics file events
  async {
    let! (filename, args) = Async.AwaitObservable matchingFileEvents
    match args.Diagnostics with
    | [||] -> return Ok ()
    | errors -> return Core.Result.Error errors
  }

let waitForFailingParseResultsForFile file (events: ClientEvents) =
  let matchingFileEvents = fileDiagnostics file events
  let withErrors = matchingFileEvents |> Observable.filter (fun (fn, args) -> args.Diagnostics <> [||])
  async {
    let! (filename, args) = Async.AwaitObservable withErrors
    return Core.Result.Error args.Diagnostics
  }


let waitForParsedScript (event: ClientEvents) =
  event
  |> typedEvents<LanguageServerProtocol.Types.PublishDiagnosticsParams> "textDocument/publishDiagnostics"
  |> Observable.choose (fun n ->
    let filename = n.Uri.Replace('\\', '/').Split('/') |> Array.last
    if filename = "Script.fs" then Some n else None
  )
  |> Async.AwaitObservable
