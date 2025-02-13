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

  let inline testBuilderWithTimeout (ts: TimeSpan) name testCase focus =
    TestLabel(name, TestCase(Test.timeout (int ts.TotalMilliseconds) (testCase), focus), focus)

  let inline testCaseWithTimeout (ts: TimeSpan) name test = testBuilderWithTimeout ts name (Sync test) Normal
  let inline ftestCaseWithTimeout (ts: TimeSpan) name test = testBuilderWithTimeout ts name (Sync test) Focused
  let inline ptestCaseWithTimeout (ts: TimeSpan) name test = testBuilderWithTimeout ts name (Sync test) Pending

  let inline testCaseAsyncWithTimeout (ts: TimeSpan) name test = testBuilderWithTimeout ts name (Async test) Normal
  let inline ftestCaseAsyncWithTimeout (ts: TimeSpan) name test = testBuilderWithTimeout ts name (Async test) Focused
  let inline ptestCaseAsyncWithTimeout (ts: TimeSpan) name test = testBuilderWithTimeout ts name (Async test) Pending

  let inline testCaseTaskWithTimeout (ts: TimeSpan) name (test: unit -> Task<unit>) =
    testCaseAsyncWithTimeout ts name (async.Delay(fun () -> Async.AwaitTask(test ())))

  let inline ftestCaseTaskWithTimeout (ts: TimeSpan) name (test: unit -> Task<unit>) =
    ftestCaseAsyncWithTimeout ts name (async.Delay(fun () -> Async.AwaitTask(test ())))

  let inline ptestCaseTaskWithTimeout (ts: TimeSpan) name (test: unit -> Task<unit>) =
    ptestCaseAsyncWithTimeout ts name (async.Delay(fun () -> Async.AwaitTask(test ())))

  // millisecond
  let DEFAULT_TIMEOUT =
    Environment.GetEnvironmentVariable "FSAC_TEST_DEFAULT_TIMEOUT"
    |> Option.ofObj
    |> Option.bind (fun x ->
      match Int32.TryParse x with
      | (true, v) -> Some v
      | _ -> None)
    |> Option.defaultValue (60000)
    |> TimeSpan.FromMilliseconds

  /// Contains testCase functions that have a `DEFAULT_TIMEOUT` set to them
  module ShadowedTimeouts =

    let testCase = testCaseWithTimeout DEFAULT_TIMEOUT
    let ptestCase = ptestCaseWithTimeout DEFAULT_TIMEOUT
    let ftestCase = ftestCaseWithTimeout DEFAULT_TIMEOUT

    let testCaseAsync = testCaseAsyncWithTimeout DEFAULT_TIMEOUT
    let ptestCaseAsync = ptestCaseAsyncWithTimeout DEFAULT_TIMEOUT
    let ftestCaseAsync = ftestCaseAsyncWithTimeout DEFAULT_TIMEOUT

let rec private copyDirectory (sourceDir: DirectoryInfo) destDir =
  // Get the subdirectories for the specified directory.
  // let dir = DirectoryInfo(sourceDir)

  if not sourceDir.Exists then
    raise (DirectoryNotFoundException("Source directory does not exist or could not be found: " + sourceDir.FullName))

  let dirs = sourceDir.GetDirectories()

  // If the destination directory doesn't exist, create it.
  Directory.CreateDirectory(destDir) |> ignore

  // Get the files in the directory and copy them to the new location.
  sourceDir.GetFiles()
  |> Seq.iter (fun file ->
    let tempPath = Path.Combine(destDir, file.Name)
    file.CopyTo(tempPath, false) |> ignore)

  // If copying subdirectories, copy them and their contents to new location.
  dirs
  |> Seq.iter (fun dir ->
    let tempPath = Path.Combine(destDir, dir.Name)
    copyDirectory dir tempPath)

type DisposableDirectory(directory: string, deleteParentDir) =
  static member Create(?name: string) =
    let tempPath, deleteParentDir =
      match name with
      | Some name -> IO.Path.GetTempPath() </> Guid.NewGuid().ToString("n") </> name, true
      | None -> IO.Path.Combine(IO.Path.GetTempPath(), Guid.NewGuid().ToString("n")), false
    // printfn "Creating directory %s" tempPath
    IO.Directory.CreateDirectory tempPath |> ignore
    new DisposableDirectory(tempPath, deleteParentDir)

  static member From(sourceDir: DirectoryInfo) =
    let self = DisposableDirectory.Create(sourceDir.Name)
    copyDirectory sourceDir self.DirectoryInfo.FullName
    self

  member x.DirectoryInfo: DirectoryInfo = IO.DirectoryInfo(directory)

  interface IDisposable with
    member x.Dispose() =
      let dirToDelete =
        if deleteParentDir then
          x.DirectoryInfo.Parent
        else
          x.DirectoryInfo

      let mutable attempts = 25

      // Handle odd cases with windows file locking
      while attempts > 0 do
        try
          IO.Directory.Delete(dirToDelete.FullName, true)
          attempts <- 0
        with _ ->
          attempts <- attempts - 1

          if attempts = 0 then
            reraise ()

          Thread.Sleep(15)




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
      MailboxProcessor<AsyncReplyChannel<_>>.Start(fun agent ->
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


let createAdaptiveServer workspaceLoader sourceTextFactory useTransparentCompiler =
  let serverInteractions = new Cacher<_>()
  let recordNotifications = record serverInteractions

  let recordRequests =
    { new Server.ClientRequestSender with
        member __.Send name payload = record serverInteractions name payload }

  let loader = workspaceLoader ()
  let client = FSharpLspClient(recordNotifications, recordRequests)

  let server =
    new AdaptiveFSharpLspServer(loader, client, sourceTextFactory, useTransparentCompiler)

  server :> IFSharpLspServer, serverInteractions :> ClientEvents

let defaultConfigDto: FSharpConfigDto =
  { WorkspaceModePeekDeepLevel = None
    ExcludeProjectDirectories = None
    KeywordsAutocomplete = None
    ExternalAutocomplete = None
    FullNameExternalAutocomplete = None
    Linter = None
    LinterConfig = None
    IndentationSize = None
    UnionCaseStubGeneration = None
    UnionCaseStubGenerationBody = None
    RecordStubGeneration = None
    RecordStubGenerationBody = None
    AddPrivateAccessModifier = None
    UnusedOpensAnalyzer = None
    UnusedOpensAnalyzerExclusions = None
    UnusedDeclarationsAnalyzer = None
    UnusedDeclarationsAnalyzerExclusions = None
    SimplifyNameAnalyzer = None
    SimplifyNameAnalyzerExclusions = None
    UnnecessaryParenthesesAnalyzer = None
    ResolveNamespaces = None
    EnableReferenceCodeLens = None
    EnableAnalyzers = None
    AnalyzersPath = None
    ExcludeAnalyzers = None
    IncludeAnalyzers = None
    DisableInMemoryProjectReferences = None
    AutomaticWorkspaceInit = Some true
    InterfaceStubGeneration = None
    InterfaceStubGenerationObjectIdentifier = None
    InterfaceStubGenerationMethodBody = None
    LineLens = None
    UseSdkScripts = Some true
    DotNetRoot = None
    FSIExtraParameters = None
    FSIExtraInteractiveParameters = None
    FSIExtraSharedParameters = None
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
    Debug = None }

let clientCaps: ClientCapabilities =

  let didChangeConfigCaps: DidChangeConfigurationClientCapabilities =
    { DynamicRegistration = Some true }

  let executeCommandsClientCaps: ExecuteCommandClientCapabilities =
    { DynamicRegistration = Some true }

  let workspaceCaps: WorkspaceClientCapabilities =
    let weCaps: WorkspaceEditClientCapabilities =
      { DocumentChanges = Some true
        ResourceOperations = None
        FailureHandling = None
        NormalizesLineEndings = None
        ChangeAnnotationSupport = None }

    let symbolCaps: WorkspaceSymbolClientCapabilities =
      { DynamicRegistration = Some true
        SymbolKind = None
        TagSupport = None
        ResolveSupport = None }

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
      DidChangeConfiguration = Some didChangeConfigCaps
      DidChangeWatchedFiles = None
      Symbol = Some symbolCaps
      SemanticTokens = Some semanticTokenCaps
      InlayHint = Some inlayHintCaps
      InlineValue = Some inlineValueCaps
      CodeLens = Some codeLensCaps
      ExecuteCommand = Some executeCommandsClientCaps
      WorkspaceFolders = Some false
      Configuration = Some true
      FileOperations = None
      Diagnostics = Some { RefreshSupport = Some false } }

  let textCaps: TextDocumentClientCapabilities =
    let syncCaps: TextDocumentSyncClientCapabilities =
      { DynamicRegistration = Some true
        WillSave = Some true
        WillSaveWaitUntil = Some true
        DidSave = Some true }

    let publishDiagCaps: PublishDiagnosticsClientCapabilities =
      { RelatedInformation = Some true
        TagSupport = Some { ValueSet = [||] }
        VersionSupport = Some false
        CodeDescriptionSupport = Some true
        DataSupport = Some false }

    let compCaps: CompletionClientCapabilities =
      { DynamicRegistration = Some true
        CompletionItem =
          Some
            { SnippetSupport = Some true
              CommitCharactersSupport = Some true
              DocumentationFormat = None
              DeprecatedSupport = Some false
              PreselectSupport = Some false
              TagSupport = None
              InsertReplaceSupport = Some false
              ResolveSupport = None
              InsertTextModeSupport = None
              LabelDetailsSupport = Some true }
        CompletionItemKind = Some { ValueSet = None }
        ContextSupport = Some true
        InsertTextMode = Some InsertTextMode.AsIs
        CompletionList = Some { ItemDefaults = None } }

    let hoverCaps: HoverClientCapabilities =
      { DynamicRegistration = Some true
        ContentFormat = Some [| MarkupKind.Markdown |] }

    let sigCaps: SignatureHelpClientCapabilities =
      { DynamicRegistration = Some true
        SignatureInformation =
          Some
            { DocumentationFormat = Some [| MarkupKind.Markdown |]
              ParameterInformation = Some { LabelOffsetSupport = Some true }
              ActiveParameterSupport = Some true }
        ContextSupport = Some true }

    let docSymCaps: DocumentSymbolClientCapabilities =
      { DynamicRegistration = Some true
        SymbolKind = Some { ValueSet = None }
        HierarchicalDocumentSymbolSupport = Some false
        TagSupport = None
        LabelSupport = Some true }

    let foldingRangeCaps: FoldingRangeClientCapabilities =
      { DynamicRegistration = Some true
        LineFoldingOnly = Some true
        RangeLimit = Some 100u
        FoldingRange = Some { CollapsedText = Some true }
        FoldingRangeKind = None }

    let semanticTokensCaps: SemanticTokensClientCapabilities =
      { DynamicRegistration = Some true
        Requests =
          { Range = Some(U2.C1 true)
            Full = Some(U2.C1 true) }
        TokenTypes = [||]
        TokenModifiers = [||]
        Formats = [| TokenFormat.Relative |]
        OverlappingTokenSupport = None
        MultilineTokenSupport = None
        ServerCancelSupport = Some true
        AugmentsSyntaxTokens = Some true }

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

    let _inlineValueCaps: InlineValueClientCapabilities =
      { DynamicRegistration = Some true }

    let renameCaps: RenameClientCapabilities =
      { DynamicRegistration = Some true
        HonorsChangeAnnotations = Some false
        PrepareSupport = Some false
        PrepareSupportDefaultBehavior = Some PrepareSupportDefaultBehavior.Identifier }

    let referenceCaps: ReferenceClientCapabilities = { DynamicRegistration = Some true }

    let declarationCaps: DeclarationClientCapabilities =
      { DynamicRegistration = Some true
        LinkSupport = Some false }

    let defCaps: DefinitionClientCapabilities =
      { DynamicRegistration = Some true
        LinkSupport = Some false }

    let typeDefCaps: TypeDefinitionClientCapabilities =
      { DynamicRegistration = Some true
        LinkSupport = Some false }

    let implCaps: ImplementationClientCapabilities =
      { DynamicRegistration = Some true
        LinkSupport = Some false }

    let docLinkCaps: DocumentLinkClientCapabilities =
      { DynamicRegistration = Some true
        TooltipSupport = Some true }

    let diagCaps: DiagnosticClientCapabilities =
      { DynamicRegistration = Some true
        RelatedDocumentSupport = Some true }

    let highlightCaps: DocumentHighlightClientCapabilities =
      { DynamicRegistration = Some true }

    let formattingCaps: DocumentFormattingClientCapabilities =
      { DynamicRegistration = Some true }

    let rangeFormattingCaps: DocumentRangeFormattingClientCapabilities =
      { DynamicRegistration = Some true }

    let onTypeFormattingCaps: DocumentOnTypeFormattingClientCapabilities =
      { DynamicRegistration = Some true }

    let codeLensCaps: CodeLensClientCapabilities = { DynamicRegistration = Some true }

    let selectionRangeCaps: SelectionRangeClientCapabilities =
      { DynamicRegistration = Some true }

    let monikerCaps: MonikerClientCapabilities = { DynamicRegistration = Some true }

    let colorProviderCaps: DocumentColorClientCapabilities =
      { DynamicRegistration = Some true }

    let linkedEditingRangeCaps: LinkedEditingRangeClientCapabilities =
      { DynamicRegistration = Some true }

    let inlineValueCaps: InlineValueClientCapabilities =
      { DynamicRegistration = Some true }

    { Synchronization = Some syncCaps
      PublishDiagnostics = Some publishDiagCaps
      Completion = Some compCaps
      Hover = Some hoverCaps
      SignatureHelp = Some sigCaps
      References = Some referenceCaps
      DocumentHighlight = Some highlightCaps
      DocumentSymbol = Some docSymCaps
      Formatting = Some formattingCaps
      RangeFormatting = Some rangeFormattingCaps
      OnTypeFormatting = Some onTypeFormattingCaps
      Definition = Some defCaps
      CodeAction = Some codeActionCaps
      CodeLens = Some codeLensCaps
      DocumentLink = Some docLinkCaps
      Rename = Some renameCaps
      FoldingRange = Some foldingRangeCaps
      SelectionRange = Some selectionRangeCaps
      SemanticTokens = Some semanticTokensCaps
      InlayHint = Some inlayHintCaps
      CallHierarchy = None
      TypeHierarchy = None
      Declaration = Some declarationCaps
      TypeDefinition = Some typeDefCaps
      Implementation = Some implCaps
      ColorProvider = Some colorProviderCaps
      LinkedEditingRange = Some linkedEditingRangeCaps
      Moniker = Some monikerCaps
      InlineValue = Some inlineValueCaps
      Diagnostic = Some diagCaps }

  { Workspace = Some workspaceCaps
    TextDocument = Some textCaps
    Experimental = None
    Window = None
    General = None
    NotebookDocument = None }

open Expecto.Logging
open Expecto.Logging.Message
open System.Threading
open FsAutoComplete.CommandResponse
open CliWrap
open CliWrap.Buffered

let logEvent (name, payload) = logger.Value.Debug("{name}: {payload}", name, payload)

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
      Cli
        .Wrap(exePath)
        .WithArguments(args)
        .WithWorkingDirectory(workingDir)
        .WithValidation(CommandResultValidation.None)
        .ExecuteBufferedAsync(ctok)
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
  async {
    let! r = runProcess (DirectoryInfo(dir).FullName) "dotnet" "restore -v d"
    return expectExitCodeZero r
  }

let dotnetToolRestore dir =
  async {
    let! r = runProcess (DirectoryInfo(dir).FullName) "dotnet" "tool restore"
    return expectExitCodeZero r
  }

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
        Capabilities = clientCaps
        ClientInfo =
          Some
            { Name = "FSAC Tests"
              Version = Some "0.0.0" }
        WorkspaceFolders =
          Some
            [| { Uri = Path.FilePathToUri path
                 Name = "Test Folder" } |]
        Trace = None
        Locale = None
        WorkDoneToken = None }

    let! result = server.Initialize p

    match result with
    | Result.Ok _res ->
      do! server.Initialized()
      return (server, clientNotifications)
    | Result.Error _e -> return failwith "Initialization failed"
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
      | (UnwrappedPlainNotification "workspaceLoad" (workspaceLoadResponse:
                                      FsAutoComplete.CommandResponse.WorkspaceLoadResponse)) ->
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

let private getDiagnosticsEvents: IObservable<string * obj> -> IObservable<_> =
  typedEvents<Ionide.LanguageServerProtocol.Types.PublishDiagnosticsParams> "textDocument/publishDiagnostics"

let private fileName (u: DocumentUri) = u.Split([| '/'; '\\' |], StringSplitOptions.RemoveEmptyEntries) |> Array.last

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
      |> Array.collect (fun e ->
        match e with
        | U4.C1 e -> if e.TextDocument.Uri = fileAsUri then e.Edits else [||]
        | _ -> [||])

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
    match
      diags
      |> Array.choose (fun d ->
        match d.Source with
        | Some s ->
          if s.StartsWith(desiredSource, StringComparison.Ordinal) then
            Some d
          else
            None
        | None -> None)
    with
    | [||] -> None
    | diags -> Some diags)

let analyzerDiagnostics file = fileDiagnostics file >> diagnosticsFromSource "F# Analyzers"

let linterDiagnostics file = fileDiagnostics file >> diagnosticsFromSource "F# Linter"

let fsacDiagnostics file = fileDiagnostics file >> diagnosticsFromSource "FSAC"

let compilerDiagnostics file = fileDiagnostics file >> diagnosticsFromSource "F# Compiler"

let diagnosticsToResult =
  Observable.map (function
    | [||] -> Ok()
    | diags -> Core.Error diags)

let waitForParseResultsForFile file = fileDiagnostics file >> diagnosticsToResult >> Async.AwaitObservable

let waitForDiagnosticErrorForFile file =
  fileDiagnostics file
  >> Observable.choose (function
    | [||] -> None
    | diags -> Some diags)
  >> diagnosticsToResult
  >> Async.AwaitObservable

let waitForFsacDiagnosticsForFile file = fsacDiagnostics file >> diagnosticsToResult >> Async.AwaitObservable

let waitForCompilerDiagnosticsForFile file = compilerDiagnostics file >> diagnosticsToResult >> Async.AwaitObservable

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

let waitForEditsForFile file = workspaceEdits >> editsFor file >> Async.AwaitObservable

let trySerialize (t: string) : 't option =
  try
    JsonSerializer.readJson t |> Some
  with _ ->
    None

let (|As|_|) (m: PlainNotification) : 't option =
  match trySerialize m.Content with
  | Some(r: FsAutoComplete.CommandResponse.ResponseMsg<'t>) -> Some r.Data
  | None -> None

let (|CodeActions|_|) (t: TextDocumentCodeActionResult) =
  let actions =
    t
    |> Array.choose (function
      | U2.C2 action -> Some action
      | _ -> None)

  match actions with
  | [||] -> None
  | actions -> Some actions
