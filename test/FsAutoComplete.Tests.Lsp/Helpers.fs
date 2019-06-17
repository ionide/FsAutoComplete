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
    LineLens = None}

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

let serverTest path (config: FSharpConfigDto) ts  : Test=
  let server, event = createServer()

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
    ts (server, event)
  | Result.Error e ->
    failwith "Initialization failed"

let loadDocument path : TextDocumentItem =
  { Uri = filePathToUri path
    LanguageId = "fsharp"
    Version = 0
    Text = File.ReadAllText path  }

let waitForWorkspaceFinishedParsing (event : Event<string * obj>) =
  event.Publish
  |> Event.filter (fun (typ, o) -> typ = "fsharp/notifyWorkspace")
  |> Event.map (fun (typ, o) -> unbox<PlainNotification> o)
  |> Event.filter (fun o -> o.Content.Contains "workspaceLoad" && o.Content.Contains "finished")
  |> Async.AwaitEvent
  |> Async.RunSynchronously
  |> ignore