module FsAutoComplete.Tests.Lsp

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
  let client = FSharpLspClient (fun _ _ -> AsyncLspResult.success ())
  let commands = Commands(FsAutoComplete.JsonSerializer.writeJson)
  let server = FsharpLspServer(commands, client)
  server

let defaultConfigDto : FSharpConfigDto =
  { WorkspaceModePeekDeepLevel = None
    WorkspaceExcludedDirs = None
    KeywordsAutocomplete = None
    ExternalAutocomplete = None
    Linter = None
    RecordStubGeneration = None
    UnusedOpensAnalyzer = None
    UnusedDeclarationsAnalyzer = None
    SimplifyNameAnalyzer = None
    ResolveNamespaces = None
    MinimizeBackgroundParsing = None
    EnableBackgroundSymbolCache = None
    EnableReferenceCodeLens = None
    EnableAnalyzers = None
    AnalyzersPath = None
    DisableInMemoryProjectReferences = None}

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
  let server = createServer()

  let p : InitializeParams =
      { ProcessId = Some 1
        RootPath = Some path
        RootUri = None
        InitializationOptions = Some (Server.serialize config)
        Capabilities = Some clientCaps
        trace = None}

  let result = server.Initialize p |> Async.RunSynchronously
  match result with
  | Result.Ok res ->
    ts server
  | Result.Error e ->
    failwith "Initialization failed"

[<Tests>]
let tests =
  test "InitTest" {
    let server = createServer()

    let p : InitializeParams =
      { ProcessId = Some 1
        RootPath = Some __SOURCE_DIRECTORY__
        RootUri = None
        InitializationOptions = Some (Server.serialize defaultConfigDto)
        Capabilities = Some clientCaps
        trace = None}

    let result = server.Initialize p |> Async.RunSynchronously
    match result with
    | Result.Ok res ->
      Expect.equal res.Capabilities.CodeActionProvider (Some true) "Code Action Provider"
      Expect.equal res.Capabilities.CodeLensProvider (Some {CodeLensOptions.ResolveProvider = Some true}) "Code Lens Provider"
      Expect.equal res.Capabilities.DefinitionProvider (Some true) "Definition Provider"
      Expect.equal res.Capabilities.DocumentFormattingProvider None "Document Formatting Provider"
      Expect.equal res.Capabilities.DocumentHighlightProvider (Some true) "Document Highligthing Provider"
      Expect.equal res.Capabilities.DocumentLinkProvider None "Document Link Provider"
      Expect.equal res.Capabilities.DocumentOnTypeFormattingProvider None "Document OnType Formatting Provider"
      Expect.equal res.Capabilities.DocumentRangeFormattingProvider None "Document Range Formatting Provider"
      Expect.equal res.Capabilities.DocumentSymbolProvider (Some true) "Document Symbol Provider"
      Expect.equal res.Capabilities.ExecuteCommandProvider None "Execute Command Provider"
      Expect.equal res.Capabilities.Experimental None "Experimental"
      Expect.equal res.Capabilities.HoverProvider (Some true) "Hover Provider"
      Expect.equal res.Capabilities.ImplementationProvider (Some true) "Implementation Provider"
      Expect.equal res.Capabilities.ReferencesProvider (Some true) "References Provider"
      Expect.equal res.Capabilities.RenameProvider (Some true) "Rename Provider"
      Expect.equal res.Capabilities.SignatureHelpProvider (Some {SignatureHelpOptions.TriggerCharacters = Some [| "("; ","|]} ) "Signature Help Provider"
      let td =
        { TextDocumentSyncOptions.Default with
            OpenClose = Some true
            Change = Some TextDocumentSyncKind.Full
            Save = Some { IncludeText = Some true }
        }
      Expect.equal res.Capabilities.TextDocumentSync (Some td) "Text Document Provider"
      Expect.equal res.Capabilities.TypeDefinitionProvider (Some true) "Type Definition Provider"
      Expect.equal res.Capabilities.WorkspaceSymbolProvider (Some true) "Workspace Symbol Provider"
    | Result.Error e ->
      failwith "Initialization failed"
  }

let loadDocument path : TextDocumentItem =
  { Uri = filePathToUri path
    LanguageId = "fsharp"
    Version = 0
    Text = File.ReadAllText path  }

[<Tests>]
let basicTests =
  let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "BasicTest")
  serverTest path defaultConfigDto (fun server ->
    let path = Path.Combine(path, "Script.fsx")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path}
    do server.TextDocumentDidOpen tdop |> Async.RunSynchronously
    testList "Basic Tests" [
      testList "Hover Tests" [

        test "Hover Tests - simple symbol" {
          let p : TextDocumentPositionParams =
            { TextDocument = { Uri = filePathToUri path}
              Position = { Line = 0; Character = 4}}
          let res = server.TextDocumentHover p |> Async.RunSynchronously
          match res with
          | Result.Error e -> failtest "Request failed"
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->
            let expected =
              MarkedStrings
                [|  MarkedString.WithLanguage {Language = "fsharp"; Value = "val t : int"}
                    MarkedString.String ""
                    MarkedString.String "*Full name: Script.t*"
                    MarkedString.String "*Assembly: Script*"|]

            Expect.equal res.Contents expected "Hover test - simple symbol"
        }

        test "Hover Tests - let keyword" {
          let p : TextDocumentPositionParams =
            { TextDocument = { Uri = filePathToUri path}
              Position = { Line = 0; Character = 2}}
          let res = server.TextDocumentHover p |> Async.RunSynchronously
          match res with
          | Result.Error e -> failtest "Request failed"
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->
            let expected =
              MarkedStrings
                [|  MarkedString.WithLanguage {Language = "fsharp"; Value = "let"}
                    MarkedString.String "Used to associate, or bind, a name to a value or function."|]

            Expect.equal res.Contents expected "Hover test - let keyword"
        }

        test "Hover Tests - out of position" {
          let p : TextDocumentPositionParams =
            { TextDocument = { Uri = filePathToUri path}
              Position = { Line = 1; Character = 2}}
          let res = server.TextDocumentHover p |> Async.RunSynchronously
          match res with
          | Result.Error e -> ()
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->
            failtest "Expected failure"
        }
      ]
      testList "Document Symbol Tests" [
        test "Document Symbol" {
          let p : DocumentSymbolParams = { TextDocument = { Uri = filePathToUri path}}
          server.FileInit <- true
          let res = server.TextDocumentDocumentSymbol p |> Async.RunSynchronously
          match res with
          | Result.Error e -> failtest "Request failed"
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->

            Expect.equal res.Length 2 "Document Symbol has all symbols"
        }
      ]
      testList "Code Lens Tests" [
        test "Get Code Lens" {
          let p : CodeLensParams = { TextDocument = { Uri = filePathToUri path}}
          let res = server.TextDocumentCodeLens p |> Async.RunSynchronously
          match res with
          | Result.Error e -> failtest "Request failed"
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->

            Expect.equal res.Length 1 "Get Code Lens has all locations"
        }

        test "Resolve Code Lens" {
          let p : CodeLensParams = { TextDocument = { Uri = filePathToUri path}}
          let res = server.TextDocumentCodeLens p |> Async.RunSynchronously
          match res with
          | Result.Error e -> failtest "Request failed"
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->
            let cl = res.[0]
            let res = server.CodeLensResolve cl |> Async.RunSynchronously
            match res with
            | Result.Error e -> failtest "Request failed"
            | Result.Ok cl ->
              Expect.equal cl.Command.Value.Title "int" "Code Lens contains signature"
        }
      ]


    ])