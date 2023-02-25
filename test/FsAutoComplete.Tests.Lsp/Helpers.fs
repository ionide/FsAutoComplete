module Helpers

open System
open Expecto
open System.IO
open FsAutoComplete.Lsp
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.Control.Reactive
open System.Threading
open FSharp.UMX

module Expecto =
  open System.Threading.Tasks
  let inline testBuilderWithTimeout (ts : TimeSpan) name testCase focus =
    TestLabel(name, TestCase (Test.timeout (int ts.TotalMilliseconds) (testCase), focus), focus)

  let inline testCaseWithTimeout (ts : TimeSpan) name test =
    testBuilderWithTimeout ts name (Sync test) Normal
  let inline ftestCaseWithTimeout (ts : TimeSpan) name test =
    testBuilderWithTimeout ts name (Sync test) Focused
  let inline ptestCaseWithTimeout (ts : TimeSpan) name test =
    testBuilderWithTimeout ts name (Sync test) Pending

  let inline testCaseAsyncWithTimeout (ts : TimeSpan) name test =
    testBuilderWithTimeout ts name (Async test) Normal
  let inline ftestCaseAsyncWithTimeout (ts : TimeSpan) name test =
    testBuilderWithTimeout ts name (Async test) Focused
  let inline ptestCaseAsyncWithTimeout (ts : TimeSpan) name test =
    testBuilderWithTimeout ts name (Async test) Pending

  let inline testCaseTaskWithTimeout (ts : TimeSpan) name (test : unit -> Task<unit>) =
    testCaseAsyncWithTimeout ts name (async.Delay(fun () -> Async.AwaitTask(test ())))
  let inline ftestCaseTaskWithTimeout (ts : TimeSpan) name (test : unit -> Task<unit>)  =
    ftestCaseAsyncWithTimeout ts name (async.Delay(fun () -> Async.AwaitTask(test ())))
  let inline ptestCaseTaskWithTimeout (ts : TimeSpan) name (test : unit -> Task<unit>)  =
    ptestCaseAsyncWithTimeout ts name (async.Delay(fun () -> Async.AwaitTask(test ())))

  // millisecond
  let DEFAULT_TIMEOUT =
    Environment.GetEnvironmentVariable "FSAC_TEST_DEFAULT_TIMEOUT"
    |> Option.ofObj
    |> Option.bind(fun x -> match Int32.TryParse x with | (true, v) -> Some v | _ -> None )
    |> Option.defaultValue (60000)
    |> TimeSpan.FromMilliseconds

  /// Contains testCase functions that have a `DEFAULT_TIMEOUT` set to them
  module ShadowedTimeouts =

    let testCase = testCaseWithTimeout DEFAULT_TIMEOUT
    let ptestCase = ptestCaseWithTimeout DEFAULT_TIMEOUT
    let ftestCase = ptestCaseWithTimeout DEFAULT_TIMEOUT

    let testCaseAsync = testCaseAsyncWithTimeout DEFAULT_TIMEOUT
    let ptestCaseAsync = ptestCaseAsyncWithTimeout DEFAULT_TIMEOUT
    let ftestCaseAsync = ptestCaseAsyncWithTimeout DEFAULT_TIMEOUT

let rec private copyDirectory sourceDir destDir =
  // Get the subdirectories for the specified directory.
  let dir = DirectoryInfo(sourceDir)

  if not dir.Exists then
    raise (DirectoryNotFoundException("Source directory does not exist or could not be found: " + sourceDir))

  let dirs = dir.GetDirectories()

  // If the destination directory doesn't exist, create it.
  Directory.CreateDirectory(destDir) |> ignore

  // Get the files in the directory and copy them to the new location.
  dir.GetFiles()
  |> Seq.iter (fun file ->
    let tempPath = Path.Combine(destDir, file.Name)
    file.CopyTo(tempPath, false) |> ignore)

  // If copying subdirectories, copy them and their contents to new location.
  dirs
  |> Seq.iter (fun dir ->
    let tempPath = Path.Combine(destDir, dir.Name)
    copyDirectory dir.FullName tempPath)

type DisposableDirectory(directory: string) =
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
      IO.Directory.Delete(x.DirectoryInfo.FullName, true)

type Async =
  /// Behaves like AwaitObservable, but calls the specified guarding function
  /// after a subscriber is registered with the observable.
  static member GuardedAwaitObservable (ev1: IObservable<'T1>) guardFunction =
    async {
      let! token = Async.CancellationToken // capture the current cancellation token

      return!
        Async.FromContinuations(fun (cont, econt, ccont) ->
          // start a new mailbox processor which will await the result
          MailboxProcessor.Start(
            (fun (mailbox: MailboxProcessor<Choice<'T1, exn, OperationCanceledException>>) ->
              async {
                // register a callback with the cancellation token which posts a cancellation message
                use __ =
                  token.Register(
                    (fun _ -> mailbox.Post(Choice3Of3(OperationCanceledException("The operation was cancelled.")))),
                    null
                  )

                // subscribe to the observable: if an error occurs post an error message and post the result otherwise
                use __ =
                  ev1.Subscribe(
                    { new IObserver<'T1> with
                        member __.OnNext result = mailbox.Post(Choice1Of3 result)
                        member __.OnError exn = mailbox.Post(Choice2Of3 exn)

                        member __.OnCompleted() =
                          let msg =
                            "Cancelling the workflow, because the Observable awaited using AwaitObservable has completed."

                          mailbox.Post(Choice3Of3(OperationCanceledException(msg))) }
                  )

                guardFunction () // call the guard function

                // wait for the first of these messages and call the appropriate continuation function
                let! message = mailbox.Receive()

                match message with
                | Choice1Of3 reply -> cont reply
                | Choice2Of3 exn -> econt exn
                | Choice3Of3 exn -> ccont exn
              })
          )
          |> ignore)
    }

  /// Creates an asynchronous workflow that will be resumed when the
  /// specified observables produces a value. The workflow will return
  /// the value produced by the observable.
  static member AwaitObservable(ev1: IObservable<'T1>) = Async.GuardedAwaitObservable ev1 ignore

  /// Creates an asynchronous workflow that runs the asynchronous workflow
  /// given as an argument at most once. When the returned workflow is
  /// started for the second time, it reuses the result of the
  /// previous execution.
  static member Cache(input: Async<'T>) =
    let agent =
      MailboxProcessor<AsyncReplyChannel<_>>.Start
        (fun agent ->
          async {
            let! repl = agent.Receive()
            let! res = input |> Async.Catch
            repl.Reply(res)

            while true do
              let! repl = agent.Receive()
              repl.Reply(res)
          })

    async {
      let! result = agent.PostAndAsyncReply(id)

      return
        match result with
        | Choice1Of2 v -> v
        | Choice2Of2 exn -> raise exn
    }

let logger = lazy Serilog.Log.Logger.ForContext("SourceContext", "LSPTests")

type Cacher<'t> = System.Reactive.Subjects.ReplaySubject<'t>
type ClientEvents = IObservable<string * obj>

module Range =
  let rangeContainsPos (range: Range) (pos: Position) = range.Start <= pos && pos <= range.End

let record (cacher: Cacher<_>) =
  fun name payload ->
    cacher.OnNext(name, payload)
    AsyncLspResult.success Unchecked.defaultof<_>

let createServer (state: unit -> State) =
  let serverInteractions = new Cacher<_>()
  let recordNotifications = record serverInteractions

  let recordRequests =
    { new Server.ClientRequestSender with
        member __.Send name payload = record serverInteractions name payload }

  let innerState = state ()
  let client = FSharpLspClient(recordNotifications, recordRequests)
  let originalFs = FSharp.Compiler.IO.FileSystemAutoOpens.FileSystem
  let fs = FsAutoComplete.FileSystem(originalFs, innerState.Files.TryFind)
  FSharp.Compiler.IO.FileSystemAutoOpens.FileSystem <- fs
  let server = new FSharpLspServer(innerState, client)
  server :> IFSharpLspServer, serverInteractions :> ClientEvents

let createAdaptiveServer (workspaceLoader) =
  let serverInteractions = new Cacher<_>()
  let recordNotifications = record serverInteractions

  let recordRequests =
    { new Server.ClientRequestSender with
        member __.Send name payload = record serverInteractions name payload }

  let loader = workspaceLoader ()
  let client = FSharpLspClient(recordNotifications, recordRequests)
  let server = new AdaptiveFSharpLspServer(loader, client)
  server :> IFSharpLspServer, serverInteractions :> ClientEvents

let defaultConfigDto: FSharpConfigDto =
  { WorkspaceModePeekDeepLevel = None
    ExcludeProjectDirectories = None
    KeywordsAutocomplete = None
    ExternalAutocomplete = None
    Linter = None
    LinterConfig = None
    IndentationSize = None
    UnionCaseStubGeneration = None
    UnionCaseStubGenerationBody = None
    RecordStubGeneration = None
    RecordStubGenerationBody = None
    AddPrivateAccessModifier = None
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
    GenerateBinlog = Some true
    AbstractClassStubGeneration = None
    AbstractClassStubGenerationMethodBody = None
    AbstractClassStubGenerationObjectIdentifier = None
    CodeLenses =
      Some
        { Signature = Some {| Enabled = Some true |}
          References = Some {| Enabled = Some true |} }
    InlayHints =
      Some
        { typeAnnotations = Some true
          parameterNames = Some true
          disableLongTooltip = Some true }
    PipelineHints =
      Some
        { Enabled = Some true
          Prefix = Some "//" }
    Notifications = None
    Fsac = None
    BuildOptions = None
    Debug = None }

let clientCaps: ClientCapabilities =
  let dynCaps: DynamicCapabilities = { DynamicRegistration = Some true }

  let workspaceCaps: WorkspaceClientCapabilities =
    let weCaps: WorkspaceEditCapabilities =
      { DocumentChanges = Some true
        ResourceOperations = None
        FailureHandling = None
        NormalizesLineEndings = None
        ChangeAnnotationSupport = None }

    let symbolCaps: SymbolCapabilities =
      { DynamicRegistration = Some true
        SymbolKind = None }

    let semanticTokenCaps: SemanticTokensWorkspaceClientCapabilities =
      { RefreshSupport = Some true }

    let inlayHintCaps: InlayHintWorkspaceClientCapabilities =
      { RefreshSupport = Some false }

    let inlineValueCaps: InlineValueWorkspaceClientCapabilities =
      { RefreshSupport = Some false }

    let codeLensCaps: CodeLensWorkspaceClientCapabilities =
      { RefreshSupport = Some true }

    { ApplyEdit = Some true
      WorkspaceEdit = Some weCaps
      DidChangeConfiguration = Some dynCaps
      DidChangeWatchedFiles = Some dynCaps
      Symbol = Some symbolCaps
      SemanticTokens = Some semanticTokenCaps
      InlayHint = Some inlayHintCaps
      InlineValue = Some inlineValueCaps
      CodeLens = Some codeLensCaps }

  let textCaps: TextDocumentClientCapabilities =
    let syncCaps: SynchronizationCapabilities =
      { DynamicRegistration = Some true
        WillSave = Some true
        WillSaveWaitUntil = Some true
        DidSave = Some true }

    let diagCaps: PublishDiagnosticsCapabilities =
      let diagnosticTags: DiagnosticTagSupport = { ValueSet = [||] }

      { RelatedInformation = Some true
        TagSupport = Some diagnosticTags }

    let ciCaps: CompletionItemCapabilities =
      { SnippetSupport = Some true
        CommitCharactersSupport = Some true
        DocumentationFormat = None }

    let cikCaps: CompletionItemKindCapabilities = { ValueSet = None }

    let compCaps: CompletionCapabilities =
      { DynamicRegistration = Some true
        CompletionItem = Some ciCaps
        CompletionItemKind = Some cikCaps
        ContextSupport = Some true }

    let hoverCaps: HoverCapabilities =
      { DynamicRegistration = Some true
        ContentFormat = Some [| "markdown" |] }

    let sigCaps: SignatureHelpCapabilities =
      let siCaps: SignatureInformationCapabilities =
        { DocumentationFormat = Some [| "markdown" |] }

      { DynamicRegistration = Some true
        SignatureInformation = Some siCaps }

    let docSymCaps: DocumentSymbolCapabilities =
      let skCaps: SymbolKindCapabilities = { ValueSet = None }

      { DynamicRegistration = Some true
        SymbolKind = Some skCaps
        HierarchicalDocumentSymbolSupport = Some false }

    let foldingRangeCaps: FoldingRangeCapabilities =
      { DynamicRegistration = Some true
        LineFoldingOnly = Some true
        RangeLimit = Some 100 }

    let semanticTokensCaps: SemanticTokensClientCapabilities =
      { DynamicRegistration = Some true
        Requests =
          { Range = Some true
            Full = Some(U2.First true) }
        TokenTypes = [||]
        TokenModifiers = [||]
        Formats = [| TokenFormat.Relative |]
        OverlappingTokenSupport = None
        MultilineTokenSupport = None }

    let codeActionCaps =
      { DynamicRegistration = Some true
        CodeActionLiteralSupport = None
        IsPreferredSupport = None
        DisabledSupport = None
        DataSupport = None
        ResolveSupport = None
        HonorsChangeAnnotations = None }

    let inlayHintCaps: InlayHintClientCapabilities =
      { DynamicRegistration = Some true
        ResolveSupport = None }

    let inlineValueCaps: InlineValueClientCapabilities =
      { DynamicRegistration = Some true
        ResolveSupport = None }

    let renameCaps: RenameClientCapabilities =
      { DynamicRegistration = Some true
        HonorsChangeAnnotations = Some false
        PrepareSupport = Some false }

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
      CodeAction = Some codeActionCaps
      CodeLens = Some dynCaps
      DocumentLink = Some dynCaps
      Rename = Some renameCaps
      FoldingRange = Some foldingRangeCaps
      SelectionRange = Some dynCaps
      SemanticTokens = Some semanticTokensCaps
      InlayHint = Some inlayHintCaps }
      // InlineValue = Some inlineValueCaps }


  { Workspace = Some workspaceCaps
    TextDocument = Some textCaps
    Experimental = None
    Window = None }

open Expecto.Logging
open Expecto.Logging.Message
open System.Threading
open FsAutoComplete.CommandResponse
open CliWrap
open CliWrap.Buffered

let logEvent (name, payload) =
  logger.Value.Debug("{name}: {payload}", name, payload)

let logDotnetRestore section line =
  if not (String.IsNullOrWhiteSpace(line)) then
    logger.Value.Debug("[{section}] dotnet restore: {line}", section, line)

let dotnetCleanup baseDir =
  [ "obj"; "bin" ]
  |> List.map (fun f -> Path.Combine(baseDir, f))
  |> List.filter Directory.Exists
  |> List.iter (fun path -> Directory.Delete(path, true))

let runProcess (workingDir: string) (exePath: string) (args: string) =
  async {
    let! ctok = Async.CancellationToken

    let! result =
      Cli.Wrap(exePath).WithArguments(args).WithWorkingDirectory(
        workingDir
      )
        .WithValidation(
        CommandResultValidation.None
      )
        .ExecuteBufferedAsync(
        ctok
      )
        .Task
      |> Async.AwaitTask

    return result
  }

let inline expectExitCodeZero (r: BufferedCommandResult) =
  Expect.equal
    r.ExitCode
    0
    $"Expected exit code zero but was %i{r.ExitCode}.\nStdOut: %s{r.StandardOutput}\nStdErr: %s{r.StandardError}"

let dotnetRestore dir =
  runProcess dir "dotnet" "restore" |> Async.map expectExitCodeZero

let dotnetToolRestore dir =
  runProcess dir "dotnet" "tool restore" |> Async.map expectExitCodeZero

let serverInitialize path (config: FSharpConfigDto) createServer =
  async {
    dotnetCleanup path

    for file in System.IO.Directory.EnumerateFiles(path, "*.fsproj", SearchOption.AllDirectories) do
      do! file |> Path.GetDirectoryName |> dotnetRestore

    let (server: IFSharpLspServer), clientNotifications = createServer ()
    clientNotifications |> Observable.add logEvent

    let p: InitializeParams =
      { ProcessId = Some 1
        RootPath = Some path
        RootUri = Some(sprintf "file://%s" path)
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

    let! result = server.Initialize p

    match result with
    | Result.Ok res ->
      do! server.Initialized(InitializedParams())
      return (server, clientNotifications)
    | Result.Error e -> return failwith "Initialization failed"
  }

let loadDocument path : TextDocumentItem =
  { Uri = Path.FilePathToUri path
    LanguageId = "fsharp"
    Version = 0
    Text = File.ReadAllText path }

let parseProject projectFilePath (server: IFSharpLspServer) =
  async {
    let projectParams: ProjectParms =
      { Project = { Uri = Path.FilePathToUri projectFilePath } }

    let projectName = Path.GetFileNameWithoutExtension projectFilePath
    let! result = server.FSharpProject projectParams
    do! Async.Sleep(TimeSpan.FromSeconds 3.)
    logger.Value.Debug("{project} parse result: {result}", projectName, result)
  }

let (|UnwrappedPlainNotification|_|) eventType (notification: PlainNotification) : 't option =
  notification.Content
  |> JsonSerializer.readJson<ResponseMsg<'t>>
  |> fun r -> if r.Kind = eventType then Some r.Data else None

let internal defaultTimeout = TimeSpan.FromSeconds 10.0

let waitForWorkspaceFinishedParsing (events: ClientEvents) =
  let chooser (name, payload) =
    match name with
    | "fsharp/notifyWorkspace" ->
      match unbox payload with
      | (UnwrappedPlainNotification "workspaceLoad"
                                    (workspaceLoadResponse: FsAutoComplete.CommandResponse.WorkspaceLoadResponse)) ->
        if workspaceLoadResponse.Status = "finished" then
          Some()
        else
          None
      | _ -> None
    | _ -> None

  logger.Value.Debug "waiting for workspace to finish loading"

  events
  |> Observable.choose chooser
  |> Observable.timeoutSpan defaultTimeout
  |> Async.AwaitObservable

let private typedEvents<'t> (typ: string) : IObservable<string * obj> -> IObservable<'t> =
  Observable.choose (fun (typ', _o) -> if typ' = typ then Some(unbox _o) else None)

let private payloadAs<'t> = Observable.map (fun (_typ, o) -> unbox<'t> o)

let private getDiagnosticsEvents: IObservable<string * obj> -> IObservable<_> =
  typedEvents<Ionide.LanguageServerProtocol.Types.PublishDiagnosticsParams> "textDocument/publishDiagnostics"

let private fileName (u: DocumentUri) =
  u.Split([| '/'; '\\' |], StringSplitOptions.RemoveEmptyEntries) |> Array.last

/// note that the files here are intended to be the filename only., not the full URI.
let private matchFiles (files: string Set) =
  Observable.choose (fun (p: Ionide.LanguageServerProtocol.Types.PublishDiagnosticsParams) ->
    let filename = fileName p.Uri

    if Set.contains filename files then
      Some(filename, p)
    else
      None)

let workspaceEdits: IObservable<string * obj> -> IObservable<_> =
  typedEvents<ApplyWorkspaceEditParams> "workspace/applyEdit"

let editsFor (file: string) =
  let fileAsUri = Path.FilePathToUri file

  Observable.choose (fun (p: ApplyWorkspaceEditParams) ->
    let edits = p.Edit.DocumentChanges.Value

    let forFile =
      edits
      |> Array.collect (fun e -> if e.TextDocument.Uri = fileAsUri then e.Edits else [||])

    match forFile with
    | [||] -> None
    | edits -> Some edits)

let fileDiagnostics (file: string) =
  logger.Value.Information("waiting for events on file {file}", file)

  getDiagnosticsEvents
  >> matchFiles (Set.ofList [ file ])
  >> Observable.map snd
  >> Observable.map (fun d -> d.Diagnostics)

let fileDiagnosticsForUri (uri: string) =
  logger.Value.Information("waiting for events on file {file}", uri)

  getDiagnosticsEvents
  >> Observable.choose (fun n -> if n.Uri = uri then Some n.Diagnostics else None)

let diagnosticsFromSource (desiredSource: String) =
  Observable.choose (fun (diags: Diagnostic[]) ->
    match diags
          |> Array.choose (fun d ->
            match d.Source with
            | Some s -> if s.StartsWith desiredSource then Some d else None
            | None -> None)
      with
    | [||] -> None
    | diags -> Some diags)

let analyzerDiagnostics file =
  fileDiagnostics file >> diagnosticsFromSource "F# Analyzers"

let linterDiagnostics file =
  fileDiagnostics file >> diagnosticsFromSource "F# Linter"

let fsacDiagnostics file =
  fileDiagnostics file >> diagnosticsFromSource "FSAC"

let compilerDiagnostics file =
  fileDiagnostics file >> diagnosticsFromSource "F# Compiler"

let diagnosticsToResult =
  Observable.map (function
    | [||] -> Ok()
    | diags -> Core.Error diags)

let waitForParseResultsForFile file =
  fileDiagnostics file >> diagnosticsToResult >> Async.AwaitObservable

let waitForFsacDiagnosticsForFile file =
  fsacDiagnostics file >> diagnosticsToResult >> Async.AwaitObservable

let waitForCompilerDiagnosticsForFile file =
  compilerDiagnostics file >> diagnosticsToResult >> Async.AwaitObservable

let waitForParsedScript (event: ClientEvents) =
  event
  |> typedEvents<Ionide.LanguageServerProtocol.Types.PublishDiagnosticsParams> "textDocument/publishDiagnostics"
  |> Observable.choose (fun n ->
    let filename = n.Uri.Replace('\\', '/').Split('/') |> Array.last

    if filename = "Script.fs" then Some n else None)
  |> Async.AwaitObservable

let waitForTestDetected (fileName: string) (events: ClientEvents) : Async<TestDetectedNotification> =
  typedEvents<TestDetectedNotification> "fsharp/testDetected" events
  |> Observable.filter (fun (tdn: TestDetectedNotification) ->
    let testNotificationFileName = Path.GetFileName(tdn.File)
    testNotificationFileName = fileName)
  |> Async.AwaitObservable

let waitForEditsForFile file =
  workspaceEdits >> editsFor file >> Async.AwaitObservable

let trySerialize (t: string) : 't option =
  try
    JsonSerializer.readJson t |> Some
  with _ ->
    None

let (|As|_|) (m: PlainNotification) : 't option =
  match trySerialize m.Content with
  | Some (r: FsAutoComplete.CommandResponse.ResponseMsg<'t>) -> Some r.Data
  | None -> None

let (|CodeActions|_|) (t: TextDocumentCodeActionResult) =
  let actions =
    t
    |> Array.choose (function
      | U2.Second action -> Some action
      | _ -> None)

  match actions with
  | [||] -> None
  | actions -> Some actions
