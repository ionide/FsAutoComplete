module Helpers

open System
open Expecto
open System.IO
open System.Diagnostics
open FsAutoComplete.Lsp
open LanguageServerProtocol
open LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

let logger = Expecto.Logging.Log.create "LSPTests"

let createServer () =
  let event = Event<string * obj> ()
  let client = FSharpLspClient (fun name o -> event.Trigger (name,o); AsyncLspResult.success () )
  let commands = Commands(FsAutoComplete.JsonSerializer.writeJson, false)
  let server = FsharpLspServer(commands, client)
  server, event

let defaultConfigDto : FSharpConfigDto =
  { WorkspaceModePeekDeepLevel = None
    ExcludeProjectDirectories = None
    KeywordsAutocomplete = None
    ExternalAutocomplete = None
    Linter = None
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
    UseSdkScripts = Some true }

let clientCaps : ClientCapabilities =
  let dynCaps : DynamicCapabilities = { DynamicRegistration = Some true}
  let workspaceCaps : WorkspaceClientCapabilities =
    let weCaps : WorkspaceEditCapabilities = { DocumentChanges = Some true}
    let symbolCaps: SymbolCapabilities = { DynamicRegistration = Some true
                                           SymbolKind = None}

    { ApplyEdit = Some true
      WorkspaceEdit = Some weCaps
      DidChangeConfiguration = Some dynCaps
      DidChangeWatchedFiles = Some dynCaps
      Symbol = Some symbolCaps}

  let textCaps: TextDocumentClientCapabilities =
    let syncCaps : SynchronizationCapabilities =
      { DynamicRegistration = Some true
        WillSave = Some true
        WillSaveWaitUntil = Some true
        DidSave = Some true}

    let diagCaps: PublishDiagnosticsCapabilites =
      { RelatedInformation = Some true
        TagSupport = Some true}

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
      Rename = Some dynCaps}


  { Workspace = Some workspaceCaps
    TextDocument = Some textCaps
    Experimental = None}

open Expecto.Logging
open Expecto.Logging.Message

let logEvent n =
  logger.debug (eventX "event: {e}" >> setField "e" n)

let serverInitialize path (config: FSharpConfigDto) =
  let server, event = createServer()

  event.Publish
  |> Event.add logEvent

  let p : InitializeParams =
      { ProcessId = Some 1
        RootPath = Some path
        RootUri = Some (sprintf "file://%s" path)
        InitializationOptions = Some (Server.serialize config)
        Capabilities = Some clientCaps
        trace = None}

  let result = server.Initialize p |> Async.RunSynchronously
  match result with
  | Result.Ok res ->
    (server, event)
  | Result.Error e ->
    failwith "Initialization failed"

let loadDocument path : TextDocumentItem =
  { Uri = filePathToUri path
    LanguageId = "fsharp"
    Version = 0
    Text = File.ReadAllText path  }

let parseProject projectFilePath (server: FsharpLspServer) = async {
  let projectParams: ProjectParms = 
    { Project = { Uri = filePathToUri projectFilePath } }
  // first restore the project
  let psi = System.Diagnostics.ProcessStartInfo()
  psi.FileName <- "dotnet"
  psi.Arguments <- sprintf "restore %s" projectFilePath
  let proc = System.Diagnostics.Process.Start(psi)
  proc.WaitForExit()
  if 0 <> proc.ExitCode then failwithf "could not restore project %s" projectFilePath
  let projectName = Path.GetFileNameWithoutExtension projectFilePath
  let! result = server.FSharpProject projectParams
  logger.debug (eventX "{project} parse result: {result}" >> setField "result" (sprintf "%A" result) >> setField "project" projectName)
}

let waitForWorkspaceFinishedParsing (event : Event<string * obj>) =
  event.Publish
  |> Event.map (fun n -> System.Diagnostics.Debug.WriteLine(sprintf "n: %A" n); n)
  |> Event.filter (fun (typ, o) -> typ = "fsharp/notifyWorkspace")
  |> Event.map (fun (typ, o) -> unbox<PlainNotification> o)
  |> Event.filter (fun o -> (o.Content.Contains "error") || (o.Content.Contains "workspaceLoad" && o.Content.Contains "finished"))
  |> Async.AwaitEvent
  |> Async.RunSynchronously
  |> fun o ->
        if o.Content.Contains """{"Kind":"error","""
        then failtestf "error loading project: %A" o

let expectExitCodeZero (exitCode, _) =
  Expect.equal exitCode 0 (sprintf "expected exit code zero but was %i" exitCode)

let dotnetCleanup baseDir =
  ["obj"; "bin"]
  |> List.map (fun f -> Path.Combine(baseDir, f))
  |> List.filter Directory.Exists
  |> List.iter (fun path -> Directory.Delete(path, true))
