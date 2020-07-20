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

let logger = Expecto.Logging.Log.create "LSPTests"

let createServer () =
  let event = Event<string * obj> ()
  let client = FSharpLspClient (fun name o -> event.Trigger (name,o); AsyncLspResult.success () )
  let commands = Commands(FsAutoComplete.JsonSerializer.writeJson, false)
  let originalFs = FSharp.Compiler.AbstractIL.Internal.Library.Shim.FileSystem
  let fs = FsAutoComplete.FileSystem(originalFs, commands.Files.TryFind)
  FSharp.Compiler.AbstractIL.Internal.Library.Shim.FileSystem <- fs
  let server = FsharpLspServer(commands, client)
  server, event

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
    GenerateBinlog = None }

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
      FoldingRange = Some foldingRangeCaps }


  { Workspace = Some workspaceCaps
    TextDocument = Some textCaps
    Experimental = None}

open Expecto.Logging
open Expecto.Logging.Message


let logEvent n =
  logger.debug (eventX "event: {e}" >> setField "e" n)

let logDotnetRestore section line =
  if not (String.IsNullOrWhiteSpace(line)) then
    logger.debug (eventX "[{section}] dotnet restore: {line}" >> setField "section" section >> setField "line" line)

let dotnetCleanup baseDir =
  ["obj"; "bin"]
  |> List.map (fun f -> Path.Combine(baseDir, f))
  |> List.filter Directory.Exists
  |> List.iter (fun path -> Directory.Delete(path, true))



let runProcess (log: string -> unit) (workingDir: string) (exePath: string) (args: string) =
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

  p.Start() |> ignore
  p.BeginOutputReadLine()
  p.BeginErrorReadLine()
  p.WaitForExit()

  let exitCode = p.ExitCode

  exitCode, (workingDir, exePath, args)

let expectExitCodeZero (exitCode, _) =
  Expect.equal exitCode 0 (sprintf "expected exit code zero but was %i" exitCode)


let serverInitialize path (config: FSharpConfigDto) =
  dotnetCleanup path
  let files = Directory.GetFiles(path)

  if files |> Seq.exists (fun p -> p.EndsWith ".fsproj") then
    runProcess (logDotnetRestore ("Restore" + path)) path "dotnet" "restore"
    |> expectExitCodeZero

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
  { Uri = Path.FilePathToUri path
    LanguageId = "fsharp"
    Version = 0
    Text = File.ReadAllText path  }

let parseProject projectFilePath (server: FsharpLspServer) = async {
  let projectParams: ProjectParms =
    { Project = { Uri = Path.FilePathToUri projectFilePath } }

  let projectName = Path.GetFileNameWithoutExtension projectFilePath
  let! result = server.FSharpProject projectParams
  do! Async.Sleep 1000
  logger.debug (eventX "{project} parse result: {result}" >> setField "result" (sprintf "%A" result) >> setField "project" projectName)
}

let waitForWorkspaceFinishedParsing (event : Event<string * obj>) =
  let withTimeout dueTime comp =
    let success = async {
        let! x = comp
        return (Some x)
    }
    let timeout = async {
        do! Async.Sleep(dueTime)
        return None
    }
    Async.WhenAny(success, timeout)

  event.Publish
  |> Event.map (fun n -> System.Diagnostics.Debug.WriteLine(sprintf "n: %A" n); n)
  |> Event.filter (fun (typ, o) -> typ = "fsharp/notifyWorkspace")
  |> Event.map (fun (typ, o) -> unbox<PlainNotification> o)
  |> Event.filter (fun o -> (o.Content.Contains "error") || (o.Content.Contains "workspaceLoad" && o.Content.Contains "finished"))
  |> Async.AwaitEvent
  |> withTimeout 5000
  |> Async.RunSynchronously
  |> fun o ->
    match o with
    | Some o ->
      if o.Content.Contains """{"Kind":"error","""
      then failtestf "error loading project: %A" o
    | None ->
      logger.debug (eventX "Timeout waiting for workspace finished")

//This is currently used for single tests, hence the naive implementation is working just fine.
//Revisit if more tests will use this scenario.
let mutable projectOptsList : FSharp.Compiler.SourceCodeServices.FSharpProjectOptions list = []
let waitForScriptFilePropjectOptions (server: FsharpLspServer) =
  server.ScriptFileProjectOptions
  |> Event.add (fun n -> projectOptsList <- n::projectOptsList)

let private typedEvents typ =
  Event.filter (fun (typ', _o) -> typ' = typ)

let private payloadAs<'t> =
  Event.map (fun (_typ, o) -> unbox<'t> o)

let private getDiagnosticsEvents =
  typedEvents "textDocument/publishDiagnostics"
  >> payloadAs<LanguageServerProtocol.Types.PublishDiagnosticsParams>

/// note that the files here are intended to be the filename only., not the full URI.
let private matchFiles (files: string Set) =
  Event.choose (fun (p: LanguageServerProtocol.Types.PublishDiagnosticsParams) ->
    let filename = p.Uri.Split([| '/'; '\\' |], StringSplitOptions.RemoveEmptyEntries) |> Array.last
    if Set.contains filename files
    then Some (filename, p)
    else None
  )

let private fileDiagnostics file (events: Event<string*obj>) =
  logger.info (eventX "waiting for events on file {file}" >> setField "file" file)
  events.Publish
  |> getDiagnosticsEvents
  |> matchFiles (Set.ofList [file])

let analyzerEvents file events =
  fileDiagnostics file events
  |> Event.map snd
  |> Event.filter (fun payload -> payload.Diagnostics |> Array.exists (fun d -> d.Source.StartsWith "F# Analyzers"))

let waitForParseResultsForFile file (events: Event<string*obj>) =
  let matchingFileEvents = fileDiagnostics file events
  async {
    let! (filename, args) = Async.AwaitEvent matchingFileEvents
    match args.Diagnostics with
    | [||] -> return Ok ()
    | errors -> return Core.Result.Error errors
  }
  |> Async.RunSynchronously

let inline waitForParsedScript (m: System.Threading.ManualResetEvent) (event: Event<string * obj>) =

  let bag = new System.Collections.Concurrent.ConcurrentBag<LanguageServerProtocol.Types.PublishDiagnosticsParams>()

  event.Publish
  |> Event.filter (fun (typ, o) -> typ = "textDocument/publishDiagnostics")
  |> Event.map (fun (typ, o) -> unbox<LanguageServerProtocol.Types.PublishDiagnosticsParams> o)
  |> Event.filter (fun n ->
    let filename = n.Uri.Replace('\\', '/').Split('/') |> Array.last
    filename = "Script.fs")
  |> Event.add (fun n ->
    bag.Add(n)
    m.Set() |> ignore
  )

  bag
