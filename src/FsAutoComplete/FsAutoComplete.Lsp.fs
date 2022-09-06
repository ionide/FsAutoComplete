module FsAutoComplete.Lsp

open System
open System.IO
open System.Threading
open FsAutoComplete
open FsAutoComplete.Core
open FsAutoComplete.LspHelpers
open FsAutoComplete.CodeFix
open FsAutoComplete.CodeFix.Types
open FsAutoComplete.Logging
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types.LspResult
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Newtonsoft.Json.Linq
open Ionide.ProjInfo.ProjectSystem
open FsToolkit.ErrorHandling
open FSharp.UMX
open FSharp.Analyzers
open FSharp.Compiler.Text
open CliWrap
open CliWrap.Buffered
open FSharp.Compiler.Tokenization
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Symbols
open Fantomas.Client.Contracts
open FSharp.Control.Reactive.Observable

module FcsRange = FSharp.Compiler.Text.Range
type FcsRange = FSharp.Compiler.Text.Range
module FcsPos = FSharp.Compiler.Text.Position
type FcsPos = FSharp.Compiler.Text.Position

type OptionallyVersionedTextDocumentPositionParams =
  {
    /// The text document.
    TextDocument: VersionedTextDocumentIdentifier
    /// The position inside the text document.
    Position: Ionide.LanguageServerProtocol.Types.Position
  }

  interface ITextDocumentPositionParams with
    member this.TextDocument = { Uri = this.TextDocument.Uri }
    member this.Position = this.Position

module Result =
  let ofCoreResponse (r: CoreResponse<'a>) =
    match r with
    | CoreResponse.Res a -> Ok a
    | CoreResponse.ErrorRes msg
    | CoreResponse.InfoRes msg -> Error(JsonRpc.Error.InternalErrorMessage msg)

module AsyncResult =
  let ofCoreResponse (ar: Async<CoreResponse<'a>>) = ar |> Async.map Result.ofCoreResponse

  let ofStringErr (ar: Async<Result<'a, string>>) =
    ar |> AsyncResult.mapError JsonRpc.Error.InternalErrorMessage


type FSharpLspClient(sendServerNotification: ClientNotificationSender, sendServerRequest: ClientRequestSender) =

  inherit LspClient()

  member val ClientCapabilities: ClientCapabilities option = None with get, set

  override __.WindowShowMessage(p) =
    sendServerNotification "window/showMessage" (box p) |> Async.Ignore

  override __.WindowShowMessageRequest(p) =
    sendServerRequest.Send "window/showMessageRequest" (box p)

  override __.WindowLogMessage(p) =
    sendServerNotification "window/logMessage" (box p) |> Async.Ignore

  override __.TelemetryEvent(p) =
    sendServerNotification "telemetry/event" (box p) |> Async.Ignore

  override __.ClientRegisterCapability(p) =
    sendServerRequest.Send "client/registerCapability" (box p)

  override __.ClientUnregisterCapability(p) =
    sendServerRequest.Send "client/unregisterCapability" (box p)

  override __.WorkspaceWorkspaceFolders() =
    sendServerRequest.Send "workspace/workspaceFolders" ()

  override __.WorkspaceConfiguration(p) =
    sendServerRequest.Send "workspace/configuration" (box p)

  override __.WorkspaceApplyEdit(p) =
    sendServerRequest.Send "workspace/applyEdit" (box p)

  override __.WorkspaceSemanticTokensRefresh() =
    sendServerNotification "workspace/semanticTokens/refresh" () |> Async.Ignore

  override __.TextDocumentPublishDiagnostics(p) =
    sendServerNotification "textDocument/publishDiagnostics" (box p) |> Async.Ignore

  ///Custom notification for workspace/solution/project loading events
  member __.NotifyWorkspace(p: PlainNotification) =
    sendServerNotification "fsharp/notifyWorkspace" (box p) |> Async.Ignore

  ///Custom notification for initial workspace peek
  member __.NotifyWorkspacePeek(p: PlainNotification) =
    sendServerNotification "fsharp/notifyWorkspacePeek" (box p) |> Async.Ignore

  member __.NotifyCancelledRequest(p: PlainNotification) =
    sendServerNotification "fsharp/notifyCancel" (box p) |> Async.Ignore

  member __.NotifyFileParsed(p: PlainNotification) =
    sendServerNotification "fsharp/fileParsed" (box p) |> Async.Ignore

  member __.NotifyDocumentAnalyzed(p: DocumentAnalyzedNotification) =
    sendServerNotification "fsharp/documentAnalyzed" (box p) |> Async.Ignore

  member __.NotifyTestDetected(p: TestDetectedNotification) =
    sendServerNotification "fsharp/testDetected" (box p) |> Async.Ignore

  member x.CodeLensRefresh() =
    match x.ClientCapabilities with
    | Some { Workspace = Some { CodeLens = Some { RefreshSupport = Some true } } } ->
      sendServerNotification "workspace/codeLens/refresh" () |> Async.Ignore
    | _ -> async { return () }

type DiagnosticMessage =
  | Add of source: string * diags: Diagnostic[]
  | Clear of source: string

/// a type that handles bookkeeping for sending file diagnostics.  It will debounce calls and handle sending diagnostics via the configured function when safe
type DiagnosticCollection(sendDiagnostics: DocumentUri -> Diagnostic[] -> Async<unit>) =
  let send uri (diags: Map<string, Diagnostic[]>) =
    Map.toArray diags |> Array.collect snd |> sendDiagnostics uri

  let agents =
    System.Collections.Concurrent.ConcurrentDictionary<DocumentUri, MailboxProcessor<DiagnosticMessage> * CancellationTokenSource>
      ()

  let rec restartAgent (fileUri: DocumentUri) =
    removeAgent fileUri
    getOrAddAgent fileUri |> ignore

  and removeAgent (fileUri: DocumentUri) =
    match agents.TryRemove(fileUri) with
    | false, _ -> ()
    | true, (_, ctok) -> ctok.Cancel()

  and agentFor (uri: DocumentUri) cTok =
    let logger = LogProvider.getLoggerByName $"Diagnostics/{uri}"

    let mailbox =
      MailboxProcessor.Start(
        (fun inbox ->
          let rec loop (state: Map<string, Diagnostic[]>) =
            async {
              match! inbox.Receive() with
              | Add (source, diags) ->
                let newState = state |> Map.add source diags
                do! send uri newState
                return! loop newState
              | Clear source ->
                let newState = state |> Map.remove source
                do! send uri newState
                return! loop newState
            }

          loop Map.empty),
        cTok
      )

    mailbox.Error.Add(fun exn ->
      logger.error (
        Log.setMessage "Error while sending diagnostics: {message}"
        >> Log.addExn exn
        >> Log.addContext "message" exn.Message
      ))

    mailbox.Error.Add(fun exn -> restartAgent uri)
    mailbox

  and getOrAddAgent fileUri =
    agents.GetOrAdd(
      fileUri,
      fun fileUri ->
        let cts = new CancellationTokenSource()
        let mailbox = agentFor fileUri cts.Token
        (mailbox, cts)
    )
    |> fst

  member x.SetFor(fileUri: DocumentUri, kind: string, values: Diagnostic[]) =
    let mailbox = getOrAddAgent fileUri

    match values with
    | [||] -> mailbox.Post(Clear kind)
    | values -> mailbox.Post(Add(kind, values))

  member x.ClearFor(fileUri: DocumentUri) =
    removeAgent fileUri
    sendDiagnostics fileUri [||] |> Async.Start

  member x.ClearFor(fileUri: DocumentUri, kind: string) =
    let mailbox = getOrAddAgent fileUri
    mailbox.Post(Clear kind)

  interface IDisposable with
    member x.Dispose() =
      for (_, cts) in agents.Values do
        cts.Cancel()



open FSharp.Data.Adaptive
open Ionide.ProjInfo
open FSharp.Compiler.CodeAnalysis
open System.Linq
open System.Reactive
open System.Reactive.Linq
open Microsoft.Build.Graph


type Microsoft.FSharp.Control.Async with

 /// Creates an asynchronous workflow that will be resumed when the
      /// specified observables produces a value. The workflow will return
      /// the value produced by the observable.
  static member AwaitObservable(observable : IObservable<'T1>) =

          /// Helper that can be used for writing CPS-style code that resumes
          /// on the same thread where the operation was started.
          let synchronize f =
            let ctx = System.Threading.SynchronizationContext.Current
            f (fun g ->
              let nctx = System.Threading.SynchronizationContext.Current
              if ctx <> null && ctx <> nctx then ctx.Post((fun _ -> g()), null)
              else g() )

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

type IObservable<'T> with
  /// Fires an event only after the specified interval has passed in which no other pending event has fired. Buffers all events leading up to that emit.
  member x.BufferedDebounce(ts : TimeSpan) =
    x
      .GroupByUntil((fun _ -> true), id, (fun (g: IGroupedObservable<_,_>) -> g.Throttle(ts)))
      .SelectMany(fun x -> x.ToList())

module Helpers =
  let notImplemented<'t> = async.Return LspResult.notImplemented<'t>
  let ignoreNotification = async.Return(())

  let fullPathNormalized = Path.GetFullPath >> Utils.normalizePath >> UMX.untag

type ChangeableHashMap<'Key, 'Value> with
  member x.AddOrUpdate(key, adder, updater) =
    match x.TryGetValue key with
    | None -> x.[key] <- adder key
    | Some v -> x.[key] <- updater key v

module ASet =
  let mapAtoAMap mapper src =
      src
      |> ASet.mapToAMap mapper
      |> AMap.mapA (fun _ v -> v)

type FileInfo = {
  LastWriteTime : DateTime
  NamedText : NamedText
}

type AdaptiveFSharpLspServer (workspaceLoader : IWorkspaceLoader, lspClient : FSharpLspClient) =
  inherit LspServer()

  let disposables = new System.Reactive.Disposables.CompositeDisposable()

  // let foo =
  //   workspaceLoader.Notifications.Subscribe(fun x ->

  //   )
  let hasAnalyzers = false
  let checker =
      FSharpChecker.Create(
        projectCacheSize = 200,
        keepAllBackgroundResolutions = true,
        keepAssemblyContents = hasAnalyzers,
        suggestNamesForErrors = true,
        enablePartialTypeChecking = not hasAnalyzers,
        enableBackgroundItemKeyStoreAndSemanticClassification = true,
        keepAllBackgroundSymbolUses = true
      )

  let logger = LogProvider.getLoggerByName "AdaptiveFSharpLspServer"

  let sendDiagnostics (uri: DocumentUri) (diags: Diagnostic[]) =
    logger.info (
      Log.setMessage "SendDiag for {file}: {diags} entries"
      >> Log.addContextDestructured "file" uri
      >> Log.addContextDestructured "diags" diags.Length
    )

    { Uri = uri; Diagnostics = diags } |> lspClient.TextDocumentPublishDiagnostics

  let diagnosticCollections = new DiagnosticCollection(sendDiagnostics)

  let notifications = Event<NotificationEvent>()

  let handleCommandEvents (n: NotificationEvent) =
    async {
      try
        match n with
        | NotificationEvent.FileParsed fn ->
          let uri = Path.LocalPathToUri fn

          do! lspClient.CodeLensRefresh()
          do! ({ Content = UMX.untag uri }: PlainNotification)
              |> lspClient.NotifyFileParsed
        | NotificationEvent.Workspace ws ->

          let ws =
            match ws with
            | ProjectResponse.Project (x, _) -> CommandResponse.project JsonSerializer.writeJson x
            | ProjectResponse.ProjectError (_, errorDetails) ->
              CommandResponse.projectError JsonSerializer.writeJson errorDetails
            | ProjectResponse.ProjectLoading (projectFileName) ->
              CommandResponse.projectLoading JsonSerializer.writeJson projectFileName
            | ProjectResponse.WorkspaceLoad (finished) -> CommandResponse.workspaceLoad JsonSerializer.writeJson finished
            | ProjectResponse.ProjectChanged (projectFileName) -> failwith "Not Implemented"
          logger.info (Log.setMessage "Workspace Notify {ws}" >> Log.addContextDestructured "ws" ws)
          do!
            ({ Content = ws }: PlainNotification)
            |> lspClient.NotifyWorkspace

        | NotificationEvent.ParseError (errors, file) ->
          let uri = Path.LocalPathToUri file
          let diags = errors |> Array.map fcsErrorToDiagnostic
          diagnosticCollections.SetFor(uri, "F# Compiler", diags)

        | NotificationEvent.UnusedOpens (file, opens) ->
          let uri = Path.LocalPathToUri file

          let diags =
            opens
            |> Array.map (fun n ->
              { Range = fcsRangeToLsp n
                Code = Some "FSAC0001"
                Severity = Some DiagnosticSeverity.Hint
                Source = "FSAC"
                Message = "Unused open statement"
                RelatedInformation = None
                Tags = Some [| DiagnosticTag.Unnecessary |]
                Data = None
                CodeDescription = None })

          diagnosticCollections.SetFor(uri, "F# Unused opens", diags)

        | NotificationEvent.UnusedDeclarations (file, decls) ->
          let uri = Path.LocalPathToUri file

          let diags =
            decls
            |> Array.map (fun n ->
              { Range = fcsRangeToLsp n
                Code = Some "FSAC0003"
                Severity = Some DiagnosticSeverity.Hint
                Source = "FSAC"
                Message = "This value is unused"
                RelatedInformation = Some [||]
                Tags = Some [| DiagnosticTag.Unnecessary |]
                Data = None
                CodeDescription = None })

          diagnosticCollections.SetFor(uri, "F# Unused declarations", diags)

        | NotificationEvent.SimplifyNames (file, decls) ->
          let uri = Path.LocalPathToUri file

          let diags =
            decls
            |> Array.map



              (fun ({ Range = range
                      RelativeName = _relName }) ->
                { Diagnostic.Range = fcsRangeToLsp range
                  Code = Some "FSAC0002"
                  Severity = Some DiagnosticSeverity.Hint
                  Source = "FSAC"
                  Message = "This qualifier is redundant"
                  RelatedInformation = Some [||]
                  Tags = Some [| DiagnosticTag.Unnecessary |]
                  Data = None
                  CodeDescription = None })
          diagnosticCollections.SetFor(uri, "F# simplify names", diags)

        // | NotificationEvent.Lint (file, warnings) ->
        //     let uri = Path.LocalPathToUri file
        //     // let fs =
        //     //     warnings |> List.choose (fun w ->
        //     //         w.Warning.Details.SuggestedFix
        //     //         |> Option.bind (fun f ->
        //     //             let f = f.Force()
        //     //             let range = fcsRangeToLsp w.Warning.Details.Range
        //     //             f |> Option.map (fun f -> range, {Range = range; NewText = f.ToText})
        //     //         )
        //     //     )

        //     let diags =
        //         warnings
        //         |> List.map(fun w ->
        //             let range = fcsRangeToLsp w.Warning.Details.Range
        //             let fixes =
        //               match w.Warning.Details.SuggestedFix with
        //               | None -> None
        //               | Some lazyFix ->
        //                 match lazyFix.Value with
        //                 | None -> None
        //                 | Some fix ->
        //                   Some (box [ { Range = fcsRangeToLsp fix.FromRange; NewText = fix.ToText } ] )
        //             let uri = Option.ofObj w.HelpUrl |> Option.map (fun url -> { Href = Some (Uri url) })
        //             { Range = range
        //               Code = Some w.Code
        //               Severity = Some DiagnosticSeverity.Information
        //               Source = "F# Linter"
        //               Message = w.Warning.Details.Message
        //               RelatedInformation = None
        //               Tags = None
        //               Data = fixes
        //               CodeDescription = uri }
        //         )
        //         |> List.sortBy (fun diag -> diag.Range)
        //         |> List.toArray
        //     diagnosticCollections.SetFor(uri, "F# Linter", diags)

        | NotificationEvent.Canceled (msg) ->
          let ntf: PlainNotification = { Content = msg }

          do! lspClient.NotifyCancelledRequest ntf
        | NotificationEvent.AnalyzerMessage (messages, file) ->
          let uri = Path.LocalPathToUri file

          match messages with
          | [||] -> diagnosticCollections.SetFor(uri, "F# Analyzers", [||])
          | messages ->
            let diags =
              messages
              |> Array.map (fun m ->
                let range = fcsRangeToLsp m.Range

                let severity =
                  match m.Severity with
                  | FSharp.Analyzers.SDK.Info -> DiagnosticSeverity.Information
                  | FSharp.Analyzers.SDK.Warning -> DiagnosticSeverity.Warning
                  | FSharp.Analyzers.SDK.Error -> DiagnosticSeverity.Error

                let fixes =
                  match m.Fixes with
                  | [] -> None
                  | fixes ->
                    fixes
                    |> List.map (fun fix ->
                      { Range = fcsRangeToLsp fix.FromRange
                        NewText = fix.ToText })
                    |> Ionide.LanguageServerProtocol.Server.serialize
                    |> Some

                { Range = range
                  Code = Option.ofObj m.Code
                  Severity = Some severity
                  Source = $"F# Analyzers (%s{m.Type})"
                  Message = m.Message
                  RelatedInformation = None
                  Tags = None
                  CodeDescription = None
                  Data = fixes })

            diagnosticCollections.SetFor(uri, "F# Analyzers", diags)
        | NotificationEvent.TestDetected (file, tests) ->
          let rec map
            (r: TestAdapter.TestAdapterEntry<FSharp.Compiler.Text.range>)
            : TestAdapter.TestAdapterEntry<Ionide.LanguageServerProtocol.Types.Range> =
            { Id = r.Id
              List = r.List
              Name = r.Name
              Type = r.Type
              Range = fcsRangeToLsp r.Range
              Childs = ResizeArray(r.Childs |> Seq.map map) }
          do!
            { File = Path.LocalPathToUri file
              Tests = tests |> Array.map map }
            |> lspClient.NotifyTestDetected
      with ex ->
        logger.error (
          Log.setMessage "Exception while handling command event {evt}: {ex}"
          >> Log.addContextDestructured "evt" n
          >> Log.addContext "ex" ex.Message
        )

      ()
    }
    |> Async.RunSynchronously
  do disposables.Add(
      (notifications.Publish :> IObservable<_>)
        // .GroupBy(fun e -> e)

        .BufferedDebounce(TimeSpan.FromMilliseconds(200.))
        .SelectMany(fun l -> l.Distinct())

        .Subscribe(fun e -> handleCommandEvents e)
  )


  let adaptiveFile filePath =
    AdaptiveFile.GetLastWriteTimeUtc filePath
    |> AVal.map(fun writeTime -> filePath, writeTime)

  let loader = cval<Ionide.ProjInfo.IWorkspaceLoader> workspaceLoader
  let rootpath = cval<string option> None
  let clientCapabilities = cval<ClientCapabilities option> None
  let glyphToCompletionKind =  clientCapabilities |> AVal.map(glyphToCompletionKindGenerator)
  let glyphToSymbolKind = clientCapabilities |> AVal.map glyphToSymbolKindGenerator
  let config = cval<FSharpConfig> FSharpConfig.Default
  let entityCache = EntityCache()
  let entryPoints =
    (rootpath, config)
    ||> AVal.map2(fun rootPath config ->
      rootPath
      |> Option.bind(fun rootPath ->
        let peeks =
          WorkspacePeek.peek rootPath config.WorkspaceModePeekDeepLevel (config.ExcludeProjectDirectories |> List.ofArray)
          |> List.map Workspace.mapInteresting
          |> List.sortByDescending (fun x ->
            match x with
            | CommandResponse.WorkspacePeekFound.Solution sln -> Workspace.countProjectsInSln sln
            | CommandResponse.WorkspacePeekFound.Directory _ -> -1)
        logger.info (
            Log.setMessage "Choosing from interesting items {items}"
            >> Log.addContextDestructured "items" peeks
          )
        match peeks with
        | [] -> None
        | [ CommandResponse.WorkspacePeekFound.Directory projs ] ->
          projs.Fsprojs
          |> List.map adaptiveFile
          |> Some
        | CommandResponse.WorkspacePeekFound.Solution sln :: _ ->
          Some [
            sln.Path |> adaptiveFile
          ]
        | _ -> None
      )
      |> Option.defaultValue []
    )
    |> ASet.ofAVal
    |> ASet.mapA id
  let loadedProjectOptions =
    let entrypointsBatched =
      entryPoints
      |> ASet.toAVal
    (entrypointsBatched, loader)
    ||> AVal.map2(fun items loader->
        let (file,time) = items |> Seq.maxBy(fun (name, time) -> time)
        logger.info (Log.setMessage "running load because of {file}" >> Log.addContextDestructured "file" file)
        let graph = ProjectGraph(file)
        graph.ProjectNodesTopologicallySorted
        |> Seq.iter(fun proj ->
          proj.ProjectInstance.ProjectFileLocation.LocationString
          |> ProjectResponse.ProjectLoading
          |> NotificationEvent.Workspace
          |> notifications.Trigger
        )
        let projectOptions =
          loader.LoadSln(file)
          |> Seq.toList

        let options =
          projectOptions
          |> List.map(fun o ->
            let fso = FCS.mapToFSharpProjectOptions o projectOptions
            fso, o)
        // options
        // |> Seq.iter(fun (fso,extraInfo) ->
        //   logger.info(
        //     Log.setMessage "Project Options : {options}"
        //     >> Log.addContextDestructured "options" fso
        //   )
        // )
        // Debugging.waitForDebuggerAttachedAndBreak "ionide"

        options
        |> List.iter(fun (opts, extraInfo) ->
          let projectFileName = opts.ProjectFileName
          let projViewerItemsNormalized = Ionide.ProjInfo.ProjectViewer.render extraInfo
          let responseFiles =
            projViewerItemsNormalized.Items
            |> List.map (function
                | ProjectViewerItem.Compile (p, c) -> ProjectViewerItem.Compile(Helpers.fullPathNormalized p, c))
            |> List.choose (function
                  | ProjectViewerItem.Compile (p, _) -> Some p)
          let references = FscArguments.references (opts.OtherOptions |> List.ofArray)
          logger.info (Log.setMessage "ProjectLoaded {file}" >> Log.addContextDestructured "file" opts.ProjectFileName)
          let ws =
            {
              ProjectFileName = opts.ProjectFileName
              ProjectFiles = responseFiles
              OutFileOpt = Option.ofObj extraInfo.TargetPath
              References = references
              Extra = extraInfo
              ProjectItems = projViewerItemsNormalized.Items
              Additionals = Map.empty
            }
          ProjectResponse.Project(ws, false)
          |> NotificationEvent.Workspace
          |> notifications.Trigger
        )

        options
    )

  let projectsThatContainFile (file : string<LocalPath>) =
    let untagged = UMX.untag file
    loadedProjectOptions
    |> AVal.force
    |> Seq.choose (fun (p, _) ->
        if p.SourceFiles |> Array.contains untagged then
          Some p
        else
          None)
      |> Seq.distinct
      |> Seq.toList
  let openFiles = cset<string<LocalPath>> []

  let adaptiveFile (filepath : string<LocalPath>) = aval {
    let untagged = UMX.untag filepath
    let! lastWriteTime = AdaptiveFile.GetLastWriteTimeUtc untagged
    let! text = AdaptiveFile.ReadAllText untagged
    let namedText = NamedText(filepath, text)
    return {LastWriteTime = lastWriteTime; NamedText = namedText}
  }


  let knownFsFilesWithFileSystemUpdates =
    openFiles
    |> ASet.mapAtoAMap adaptiveFile

  let knownFsTextChanges = cmap<string<LocalPath>, clist<TextDocumentContentChangeEvent>>()
  let knownFsFilesWithUpdates =
    knownFsFilesWithFileSystemUpdates
    |> AMap.mapA(fun filePath updates ->
        aval {
          match! AMap.tryFind filePath knownFsTextChanges with
          | None -> return updates
          | Some textChanges ->
            let! contentChanges = textChanges |> AList.toAVal
            let namedText =
              (updates.NamedText, contentChanges)
              ||> Seq.fold (fun text change ->

                match change.Range with
                | None -> // replace entire content
                  NamedText(filePath, change.Text)
                | Some rangeToReplace ->
                  // replace just this slice
                  let fcsRangeToReplace = protocolRangeToRange (UMX.untag filePath) rangeToReplace
                  try
                    match text.ModifyText(fcsRangeToReplace, change.Text) with
                    | Ok text -> text
                    | Error message ->
                      logger.error (
                        Log.setMessage "Error applying change to document {file} for version {version}: {message}"
                        >> Log.addContextDestructured "file" filePath
                        >> Log.addContextDestructured "message" message
                      )
                      text
                  with e ->
                      logger.error (
                        Log.setMessage "Error applying change to document {file} for version {version}: {message}"
                        >> Log.addContextDestructured "file" filePath
                        >> Log.addContextDestructured "message" (string e)
                        >> Log.addExn e
                      )
                      text
              )

            return {LastWriteTime = DateTime.UtcNow; NamedText = namedText}
         }

    )
  let getFileInfoForFile file =
    knownFsFilesWithUpdates
    |> AMap.tryFind file

  let knownFsFilesToProjectOptions =
    knownFsFilesWithUpdates
    |> AMap.mapA(fun file _ ->
      loadedProjectOptions
      |> AVal.map(fun opts ->
          opts
          |> List.tryFind(fun (opts, _) ->
            opts.SourceFiles |> Array.contains(UMX.untag file)
          )
    ))
    |> AMap.choose (fun _ v -> v)

  let getProjectOptionsForFile file =
    knownFsFilesToProjectOptions
    |> AMap.tryFind file
  let autoCompleteItems : cmap<DeclName,DeclarationListItem * Position * string<LocalPath> * (Position -> option<string>) * FSharp.Compiler.Syntax.ParsedInput> = cmap ()
  let getAutoCompleteByDeclName name =
    autoCompleteItems
    |> AMap.tryFind name

  let autoCompleteNamespaces =
    autoCompleteItems
    |> AMap.choose(fun name (d, pos, fn, getline,  ast) ->

      Commands.calculateNamespaceInsert (fun () -> Some ast) d pos getline
    )
  let getAutoCompleteNamespacesByDeclName name =
    autoCompleteNamespaces
    |> AMap.tryFind name

  let parseAndCheckFile (file : string<LocalPath>) fileVersion sourceText opts = async {
    logger.info (
      Log.setMessage "Getting typecheck results for {file} - {hash}"
      >> Log.addContextDestructured "file" file
      >> Log.addContextDestructured "hash" (sourceText.GetHashCode())
      )
    let! (parseResults, checkResults) = checker.ParseAndCheckFileInProject((UMX.untag file), fileVersion, sourceText, opts)

    logger.info (Log.setMessage "Got typecheck results for {file}" >> Log.addContextDestructured "file" file)
    notifications.Trigger(NotificationEvent.FileParsed(file))

    match checkResults with
    | FSharpCheckFileAnswer.Aborted ->
      let parseErrors = parseResults.Diagnostics |> Array.map (fun p -> p.Message)
      // logger.info (
      //   Log.setMessage "{opName} completed with errors: {errors}"
      //   >> Log.addContextDestructured "opName" opName
      //   >> Log.addContextDestructured "errors" (List.ofArray p.Diagnostics)
      // )

      // return ResultOrString.Error(sprintf "Check aborted (%A). Errors: %A" c parseErrors)
      return failwithf "Check aborted (%A). Errors: %A" checkResults parseErrors
    | FSharpCheckFileAnswer.Succeeded (c) ->
      // logger.info (
      //   Log.setMessage "{opName} completed successfully"
      //   >> Log.addContextDestructured "opName" opName
      // )
      let opts = opts
      let text = sourceText
      let parseAndCheck = ParseAndCheckResults(parseResults, c, entityCache)
      let checkErrors = parseAndCheck.GetParseResults.Diagnostics

      let parseErrors = parseAndCheck.GetCheckResults.Diagnostics
      // Debug.waitForDebuggerAttached "AdaptiveIonide"
      let errors =
        Array.append checkErrors parseErrors
        |> Array.distinctBy (fun e ->
          e.Severity, e.ErrorNumber, e.StartLine, e.StartColumn, e.EndLine, e.EndColumn, e.Message)

      let uri = Path.LocalPathToUri (file)
      let diags = errors |> Array.map fcsErrorToDiagnostic
      diagnosticCollections.SetFor(uri, "F# Compiler", diags)
      return parseAndCheck
  }
  let knownFsFilesToCheckedFilesResults =
    knownFsFilesWithUpdates
    |> AMap.mapA(fun file info ->aval {
      let fileVersion = info.LastWriteTime.Ticks |> int
      let sourceText = info.NamedText
      match! getProjectOptionsForFile file with
      | Some (opts, extraInfo) ->
        let parseAndCheck =
          Debug.measure "parseAndCheckFile" <| fun () ->
            parseAndCheckFile file fileVersion sourceText opts |> Async.RunSynchronously
        return Some parseAndCheck
      | None -> return None

      }
    )
    |> AMap.choose(fun _ v -> v)



  let getTypeCheckResults filename =
    knownFsFilesToCheckedFilesResults
    |> AMap.tryFind(filename)

  let knownFsFilesToCheckedDeclarations =
    knownFsFilesToCheckedFilesResults
    |> AMap.map(fun name (parseAndCheck) ->
      parseAndCheck.GetParseResults.GetNavigationItems().Declarations
    )

  let getDeclarations filename =
    knownFsFilesToCheckedDeclarations
    |> AMap.tryFind(filename)


  let textDocumentDidChangeNotifications = new Event<DidChangeTextDocumentParams>()
  let textDocumentDidChangeDebounced =
    textDocumentDidChangeNotifications.Publish
      .BufferedDebounce(TimeSpan.FromMilliseconds(200.))
      .Publish()
      .RefCount()

  do disposables.Add(
    textDocumentDidChangeDebounced
      .Subscribe(fun ps ->
        logger.info (
          Log.setMessage "textDocumentDidChangeNotifications Request: {parms}"
          >> Log.addContextDestructured "parms" ps
        )
        ps
        |> Seq.groupBy(fun p -> p.TextDocument.GetFilePath() |> Utils.normalizePath)
        |> Seq.iter(fun (filePath, items) ->
          let items = items |> Seq.sortBy(fun p -> p.TextDocument.Version)
        // AddOrUpdate inmemory changes
          let contentChanges = items |> Seq.collect(fun i -> i.ContentChanges)
        // let filePath =  ps.TextDocument.GetFilePath() |> Utils.normalizePath
        // let contentChanges = ps.ContentChanges
          let adder key = clist contentChanges
          let updater key (value : clist<_>) = value.AddRange contentChanges; value
          transact(fun () -> knownFsTextChanges.AddOrUpdate(filePath, adder, updater))
        )
      )
    )

  override x.Shutdown() =
    x.Dispose() |> async.Return

  override _.Initialize(p : InitializeParams) = async {
    try
      logger.info (Log.setMessage "Initialize Request {p}" >> Log.addContextDestructured "p" p)
      let c =
        p.InitializationOptions
        |> Option.bind (fun options -> if options.HasValues then Some options else None)
        |> Option.map Server.deserialize<FSharpConfigDto>
        |> Option.map FSharpConfig.FromDto
        |> Option.defaultValue FSharpConfig.Default
      transact(fun () ->
        rootpath.Value <- p.RootPath
        clientCapabilities.Value <- p.Capabilities
        config.Value <- c
      )
      return
          { InitializeResult.Default with
              Capabilities =
                { ServerCapabilities.Default with
                    HoverProvider = Some true
                    RenameProvider = Some(U2.First true)
                    DefinitionProvider = Some true
                    TypeDefinitionProvider = Some true
                    ImplementationProvider = Some true
                    ReferencesProvider = Some true
                    DocumentHighlightProvider = Some true
                    DocumentSymbolProvider = Some true
                    WorkspaceSymbolProvider = Some true
                    DocumentFormattingProvider = Some true
                    DocumentRangeFormattingProvider = Some true
                    SignatureHelpProvider =
                      Some
                        { TriggerCharacters = Some [| '('; ','; ' ' |]
                          RetriggerCharacters = Some [| ','; ')'; ' ' |] }
                    CompletionProvider =
                      Some
                        { ResolveProvider = Some true
                          TriggerCharacters = Some([| '.'; ''' |])
                          AllCommitCharacters = None //TODO: what chars shoudl commit completions?
                        }
                    CodeLensProvider = Some { CodeLensOptions.ResolveProvider = Some true }
                    CodeActionProvider = None
                      // Some
                      //   { CodeActionKinds = None
                      //     ResolveProvider = None }
                    TextDocumentSync =
                      Some
                        { TextDocumentSyncOptions.Default with
                            OpenClose = Some true
                            Change = Some TextDocumentSyncKind.Incremental
                            Save = Some { IncludeText = Some true } }
                    FoldingRangeProvider = Some false
                    SelectionRangeProvider = Some true
                    SemanticTokensProvider =
                      Some
                        { Legend =
                            createTokenLegend<ClassificationUtils.SemanticTokenTypes, ClassificationUtils.SemanticTokenModifier>
                          Range = Some true
                          Full = Some(U2.First true) }
                    InlayHintProvider = None } //Some { ResolveProvider = Some false } }
                  }
          |> success
    with e ->
      logger.error (Log.setMessage "Initialize Request Errored {p}" >> Log.addContextDestructured "p" e)
      return LspResult.internalError"initialization error"
  }

  override __.Initialized(p: InitializedParams) =
    async {
      try
        logger.info (
          Log.setMessage "Initialized request {p}"
          >> Log.addContextDestructured "p" p
        )

        let options = loadedProjectOptions |> AVal.force

        return ()
      with e ->
        logger.error (Log.setMessage "Initialized Request Errored {p}" >> Log.addContextDestructured "p" e)
        return ()
    }

  override __.TextDocumentDidOpen(p: DidOpenTextDocumentParams) = async {
    try
      logger.info (
        Log.setMessage "TextDocumentDidOpen Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      let doc = p.TextDocument
      let filePath = doc.GetFilePath() |> Utils.normalizePath
      transact(fun () -> openFiles.Add filePath) |> ignore
      return ()
    with e ->
      logger.error (Log.setMessage "TextDocumentDidOpen Request Errored {p}" >> Log.addContextDestructured "p" e)
      return ()
  }
  override __.TextDocumentDidClose(p : DidCloseTextDocumentParams) = async {
    try
      logger.info (
        Log.setMessage "TextDocumentDidClose Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )
      let doc = p.TextDocument
      let filePath = doc.GetFilePath() |> Utils.normalizePath
      transact(fun () -> openFiles.Remove(filePath) |> ignore)
      diagnosticCollections.ClearFor(Path.LocalPathToUri filePath)
      return ()
    with e ->
      logger.error (Log.setMessage "TextDocumentDidClose Request Errored {p}" >> Log.addContextDestructured "p" e)
      return ()
  }
  override __.TextDocumentDidChange(p: DidChangeTextDocumentParams) = async {
    try
      logger.info (
        Log.setMessage "TextDocumentDidChange Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )
      let filePath =  p.TextDocument.GetFilePath() |> Utils.normalizePath
      let contentChanges = p.ContentChanges
      let adder key = clist contentChanges
      let updater key (value : clist<_>) = value.AddRange contentChanges; value
      transact(fun () -> knownFsTextChanges.AddOrUpdate(filePath, adder, updater))

      // textDocumentDidChangeNotifications.Trigger p
      return ()
    with e ->
      logger.error (Log.setMessage "TextDocumentDidChange Request Errored {p}" >> Log.addContextDestructured "p" e)
      return ()

  }

  override __.TextDocumentDidSave(p) = async {
    try
      logger.info (
        Log.setMessage "TextDocumentDidSave Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      let doc = p.TextDocument
      let filePath = doc.GetFilePath() |> Utils.normalizePath

      // This removes any in memory changes, it will re-read from the filesystem
      transact(fun () ->
          knownFsTextChanges.Remove filePath |> ignore
        )

      let forceTypeCheck f = async {
        logger.info(
          Log.setMessage "TextDocumentDidSave Forced Check : {file}"
          >> Log.addContextDestructured "file" f
        )
        match getFileInfoForFile f |> AVal.force, getProjectOptionsForFile f |> AVal.force with
        | Some fileInfo, Some (opts, _) ->
          let fileversion = int fileInfo.LastWriteTime.Ticks
          return! parseAndCheckFile f fileversion fileInfo.NamedText opts |> Async.Ignore
        | _, _ -> ()
      }

      let knownFiles =
        openFiles
        |> ASet.force
      do!
        knownFiles
        |> Seq.map forceTypeCheck
        |> Async.Sequential
        |> Async.Ignore
      return ()
    with e ->
      logger.error (Log.setMessage "TextDocumentDidSave Request Errored {p}" >> Log.addContextDestructured "p" e)
      return ()
  }

  override __.TextDocumentCompletion(p: CompletionParams) =     Debug.measureAsync "TextDocumentCompletion" <| async {
    try
      logger.info (
        Log.setMessage "TextDocumentCompletion Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )


      let file = p.TextDocument.GetFilePath() |> Utils.normalizePath
      let pos = p.GetFcsPos()
      // if p.Context |> Option.exists(fun c -> c.triggerKind = CompletionTriggerKind.TriggerCharacter) then
      //   do!
      //     textDocumentDidChangeDebounced
      //     |> Async.AwaitObservable
      //     |> Async.Ignore

      match Debug.measure "TextDocumentCompletion.getFileInfoForFile" (fun () -> getFileInfoForFile file |> AVal.force) with
      | None -> return None |> success
      | Some updates ->

        let lines = updates.NamedText
        let lineStr = lines.GetLine pos
        match lines.GetLine pos with
        | None -> return success None
        | Some lineStr ->
          let completionList =
            { IsIncomplete = false
              Items = KeywordList.hashSymbolCompletionItems }

          if lineStr.StartsWith "#" then
            let completionList =
              { IsIncomplete = false
                Items = KeywordList.hashSymbolCompletionItems }

            return success (Some completionList)
          else
            let config = AVal.force config
            let rec retryAsyncOption (delay : TimeSpan) timesLeft action = async {
              match! action with
              | Some x -> return Some x
              | None when timesLeft >=0 ->
                do! Async.Sleep (delay)
                return! retryAsyncOption delay (timesLeft - 1) action
              | None -> return None
            }
            let getCompletions = async {
              match Debug.measure "TextDocumentCompletion.getTypeCheckResults" (fun ()  -> getTypeCheckResults (file) |> AVal.force)  with
              | None -> return None
              | Some typeCheckResults ->
                let getAllSymbols () =
                  if config.ExternalAutocomplete then typeCheckResults.GetAllEntities true else []

                match! Debug.measure "TextDocumentCompletion.TryGetCompletions" (fun () -> typeCheckResults.TryGetCompletions pos lineStr None getAllSymbols) with
                | None -> return None
                | Some (decls, residue, shouldKeywords) -> return Some (decls, residue, shouldKeywords, typeCheckResults, getAllSymbols)
            }

            match! retryAsyncOption (TimeSpan.FromMilliseconds (100.)) 5 getCompletions with
            | None ->
              return success (Some completionList)
            | Some (decls, residue, shouldKeywords, typeCheckResults, getAllSymbols) ->
                  // Debug
                  // logger.info(
                  //   Log.setMessage "TextDocumentCompletion found decls: {decls}"
                  //   >> Log.addContextDestructured "decls" decls
                  // )
                  return
                    Debug.measure "TextDocumentCompletion.TryGetCompletions success" <| fun () ->
                    transact( fun () ->
                      HashMap.OfList ([
                        for d in decls do
                          d.Name, (d, pos, file, lines.GetLine, typeCheckResults.GetAST)
                      ])
                      |> autoCompleteItems.UpdateTo
                    ) |> ignore
                    let includeKeywords = config.KeywordsAutocomplete && shouldKeywords
                    let items =
                      decls
                      |> Array.mapi (fun id d ->
                        let code =
                          if System.Text.RegularExpressions.Regex.IsMatch(d.Name, """^[a-zA-Z][a-zA-Z0-9']+$""") then
                            d.Name
                          elif d.NamespaceToOpen.IsSome then
                            d.Name
                          else
                            FSharpKeywords.AddBackticksToIdentifierIfNeeded d.Name

                        let label =
                          match d.NamespaceToOpen with
                          | Some no -> sprintf "%s (open %s)" d.Name no
                          | None -> d.Name

                        { CompletionItem.Create(d.Name) with
                            Kind = (AVal.force glyphToCompletionKind) d.Glyph
                            InsertText = Some code
                            SortText = Some(sprintf "%06d" id)
                            FilterText = Some d.Name })

                    let its =
                      if not includeKeywords then
                        items
                      else
                        Array.append items KeywordList.keywordCompletionItems

                    let completionList = { IsIncomplete = false; Items = its }
                    success (Some completionList)

    with e ->
      logger.error (Log.setMessage "TextDocumentCompletion Request Errored {p}" >> Log.addContextDestructured "p" e)
      return LspResult.internalError (string e)
  }

  override __.CompletionItemResolve(ci: CompletionItem) =
    let mapHelpText (ci: CompletionItem) (text: HelpText) =
      match text with
      | HelpText.Simple (symbolName, text) ->
        let d = Documentation.Markup(markdown text)
        { ci with
            Detail = Some symbolName
            Documentation = Some d }
      | HelpText.Full (name, tip, additionalEdit) ->
        let (si, comment) = TipFormatter.formatCompletionItemTip tip

        let edits, label =
          match additionalEdit with
          | None -> None, ci.Label
          | Some { Namespace = ns; Position = fcsPos } ->
            let text =
              let indentation = String(' ', fcsPos.Column)
              $"{indentation}open {ns}\n"

            let insertPos = { (fcsPos |> fcsPosToLsp) with Character = 0 }

            Some
              [| { TextEdit.NewText = text
                   TextEdit.Range = { Start = insertPos; End = insertPos } } |],
            $"{ci.Label} (open {ns})"

        let d = Documentation.Markup(markdown comment)

        { ci with
            Detail = Some si
            Documentation = Some d
            AdditionalTextEdits = edits
            Label = label }
    let helpText sym =
      match KeywordList.keywordDescriptions.TryGetValue sym with
      | true, s -> CoreResponse.Res(HelpText.Simple(sym, s))
      | _ ->
        match KeywordList.hashDirectives.TryGetValue sym with
        | true, s -> CoreResponse.Res(HelpText.Simple(sym, s))
        | _ ->
          let sym =
            if sym.StartsWith "``" && sym.EndsWith "``" then
              sym.TrimStart([| '`' |]).TrimEnd([| '`' |])
            else
              sym
          let decls =
            knownFsFilesToCheckedDeclarations
            |> AMap.force
            |> Seq.collect(snd)
          match getAutoCompleteByDeclName sym |> AVal.force with
          | None -> //Isn't in sync filled cache, we don't have result
            CoreResponse.ErrorRes(sprintf "No help text available for symbol '%s'" sym)
          | Some (decl, pos, fn, _ , _) -> //Is in sync filled cache, try to get results from async filled caches or calculate if it's not there
            let source = getFileInfoForFile fn |> AVal.force

            match source with
            | None -> CoreResponse.ErrorRes(sprintf "No help text available for symbol '%s'" sym)
            | Some source ->
              let tip = decl.Description

              let n =
                match getAutoCompleteNamespacesByDeclName sym |> AVal.force with
                | None -> None
                | Some s -> Some s

              CoreResponse.Res(HelpText.Full(sym, tip, n))
    Debug.measureAsync "CompletionItemResolve" <|
    async {
      logger.info (
        Log.setMessage "CompletionItemResolve Request: {parms}"
        >> Log.addContextDestructured "parms" ci
      )
      return
        match ci.InsertText with
        | None -> LspResult.internalError "No InsertText"
        | Some insertText ->
          helpText insertText
          |> Result.ofCoreResponse
          |> Result.fold (mapHelpText ci) (fun _ -> ci)
          |> success
    }

  override x.TextDocumentSignatureHelp(sigHelpParams: SignatureHelpParams) = asyncResult {
    logger.info (
      Log.setMessage "TextDocumentSignatureHelp Request: {parms}"
      >> Log.addContextDestructured "parms" sigHelpParams
    )

    let pos = sigHelpParams.GetFcsPos()
    let file = sigHelpParams.GetFilePath() |> Utils.normalizePath
    match getFileInfoForFile file |> AVal.force with
    | None -> return! success None
    | Some updates ->
      let namedText = updates.NamedText
      let lineStr = namedText.GetLine pos
      let tyRes = (getTypeCheckResults (file)) |> AVal.force
      match (getTypeCheckResults (file)) |> AVal.force with
      | None -> return! LspResult.internalError $"No type check results for {file}"
      | Some tyRes ->
        let charAtCaret = sigHelpParams.Context |> Option.bind (fun c -> c.TriggerCharacter)

        match!
          SignatureHelp.getSignatureHelpFor (tyRes, pos, namedText, charAtCaret, None)
          |> AsyncResult.ofStringErr
        with
        | None ->
          ()
          return! success None
        | Some sigHelp ->
          // sigHelpKind <- Some sigHelp.SigHelpKind

          let sigs =
            sigHelp.Methods
            |> Array.map (fun m ->
              let (signature, comment) = TipFormatter.formatPlainTip m.Description

              let parameters =
                m.Parameters
                |> Array.map (fun p ->
                  { ParameterInformation.Label = p.ParameterName
                    Documentation = Some(Documentation.String p.CanonicalTypeTextForSorting) })

              let d = Documentation.Markup(markdown comment)

              { SignatureInformation.Label = signature
                Documentation = Some d
                Parameters = Some parameters })

          let res =
            { Signatures = sigs
              ActiveSignature = sigHelp.ActiveOverload
              ActiveParameter = sigHelp.ActiveParameter }

          return! success (Some res)
      }
  override x.TextDocumentHover(p: TextDocumentPositionParams) = async {
    logger.info (
      Log.setMessage "TextDocumentHover Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )
    // Debug.waitForDebuggerAttached "adaptive-ionide"
    let pos = p.GetFcsPos()
    let file = p.GetFilePath() |> Utils.normalizePath
    match getFileInfoForFile file |> AVal.force with
    | None -> return success None
    | Some updates ->
      let namedText = updates.NamedText
      let lineStr = namedText.GetLine pos
      let tyRes = (getTypeCheckResults (file)) |> AVal.force
      match (getTypeCheckResults (file)) |> AVal.force with
      | None -> return LspResult.internalError $"No type check results for {file}"
      | Some tyRes ->
        match tyRes.TryGetToolTipEnhanced pos lineStr.Value with
        | Ok(Some (tip, signature, footer, typeDoc)) ->
            let formatCommentStyle =
              let config = AVal.force config
              if config.TooltipMode = "full" then
                TipFormatter.FormatCommentStyle.FullEnhanced
              else if config.TooltipMode = "summary" then
                TipFormatter.FormatCommentStyle.SummaryOnly
              else
                TipFormatter.FormatCommentStyle.Legacy

            match TipFormatter.formatTipEnhanced tip signature footer typeDoc formatCommentStyle with
            | (sigCommentFooter :: _) :: _ ->
              let signature, comment, footer = sigCommentFooter

              let markStr lang (value: string) =
                MarkedString.WithLanguage { Language = lang; Value = value }

              let fsharpBlock (lines: string[]) =
                lines |> String.concat Environment.NewLine |> markStr "fsharp"

              let sigContent =
                let lines =
                  signature.Split Environment.NewLine
                  |> Array.filter (not << String.IsNullOrWhiteSpace)

                match lines |> Array.splitAt (lines.Length - 1) with
                | (h, [| StartsWith "Full name:" fullName |]) ->
                  [| yield fsharpBlock h; yield MarkedString.String("*" + fullName + "*") |]
                | _ -> [| fsharpBlock lines |]


              let commentContent = comment |> MarkedString.String

              let footerContent =
                footer.Split Environment.NewLine
                |> Array.filter (not << String.IsNullOrWhiteSpace)
                |> Array.map (fun n -> MarkedString.String("*" + n + "*"))


              let response =
                { Contents = MarkedStrings [| yield! sigContent; yield commentContent; yield! footerContent |]
                  Range = None }

              return success (Some response)
            | _ ->
              return success None
        | Ok (None) ->

          return LspResult.internalError $"No TryGetToolTipEnhanced results for {file}"
        | Error e ->
          logger.error(
            Log.setMessage "Failed with {error}"
            >> Log.addContext "error" e
          )
          return LspResult.internalError e
  }

  override x.TextDocumentRename(p: RenameParams) =
    logger.info (
      Log.setMessage "TextDocumentRename Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )
    Helpers.notImplemented

  override x.TextDocumentDefinition(p : TextDocumentPositionParams) = async {
    logger.info (
      Log.setMessage "TextDocumentDefinition Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )
    let pos = p.GetFcsPos()
    let file = p.GetFilePath() |> Utils.normalizePath
    match getFileInfoForFile file |> AVal.force with
    | None -> return success None
    | Some updates ->
      let namedText = updates.NamedText
      let lineStr = namedText.GetLine pos
      match (getTypeCheckResults (file)) |> AVal.force with
      | None -> return! Helpers.notImplemented
      | Some tyRes ->
        match! tyRes.TryFindDeclaration pos lineStr.Value with
        | Error e -> return LspResult.internalError e
        | Ok decl -> return decl |> findDeclToLspLocation |> GotoResult.Single |> Some |> success
  }

  override x.TextDocumentTypeDefinition(p: TextDocumentPositionParams) = async {
    logger.info (
      Log.setMessage "TextDocumentTypeDefinition Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )
    let pos = p.GetFcsPos()
    let file = p.GetFilePath() |> Utils.normalizePath
    match getFileInfoForFile file |> AVal.force with
    | None -> return success None
    | Some updates ->
      let namedText = updates.NamedText
      let lineStr = namedText.GetLine pos
      match (getTypeCheckResults (file)) |> AVal.force with
      | None -> return! Helpers.notImplemented
      | Some tyRes ->
        match! tyRes.TryFindTypeDeclaration pos lineStr.Value with
        | Error e -> return LspResult.internalError e
        | Ok decl -> return decl |> findDeclToLspLocation |> GotoResult.Single |> Some |> success
  }
  override x.TextDocumentReferences(p: ReferenceParams) = asyncResult {
    logger.info (
      Log.setMessage "TextDocumentReferences Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )
    let pos = p.GetFcsPos()
    let file = p.GetFilePath() |> Utils.normalizePath
    match getFileInfoForFile file |> AVal.force with
    | None -> return! success None
    | Some updates ->
      let namedText = updates.NamedText
      let lineStr = namedText.GetLine pos
      match (getTypeCheckResults (file)) |> AVal.force with
      | None -> return! Helpers.notImplemented
      | Some tyRes ->
        let findReferencesForSymbolInFile (file,project, symbol) =
          checker.FindBackgroundReferencesInFile(
                file,
                project,
                symbol,
                canInvalidateProject = false,
                userOpName = "find references"
              )
        let tryGetFileSource file =
          getFileInfoForFile file
          |> AVal.force
          |> Option.map (fun f -> f.NamedText)
          |> Option.defaultWith(fun () ->
            let change = File.ReadAllText(UMX.untag file)
            NamedText(file, change)
          )
          |> Ok

        let getProjectOptions file =
          getProjectOptionsForFile file
          |> AVal.force
          |> Option.map fst
        let projectsThatContainFile file =
          projectsThatContainFile file
        let getDependentProjectsOfProjects ps =
          let projectSnapshot = loadedProjectOptions |> AVal.force |> Seq.map fst
          let allDependents = System.Collections.Generic.HashSet<FSharpProjectOptions>()

          let currentPass = ResizeArray()
          currentPass.AddRange(ps |> List.map (fun p -> p.ProjectFileName))

          let mutable continueAlong = true

          while continueAlong do
            let dependents =
              projectSnapshot
              |> Seq.filter (fun p ->
                p.ReferencedProjects
                |> Seq.exists (fun r ->
                  match r.ProjectFilePath with
                  | None -> false
                  | Some p -> currentPass.Contains(p)))

            if Seq.isEmpty dependents then
              continueAlong <- false
              currentPass.Clear()
            else
              for d in dependents do
                allDependents.Add d |> ignore<bool>

              currentPass.Clear()
              currentPass.AddRange(dependents |> Seq.map (fun p -> p.ProjectFileName))

          Seq.toList allDependents
        let getProjectOptionsForFsproj file =
          loadedProjectOptions
          |> AVal.force
          |> Seq.map fst
          |> Seq.tryFind(fun x -> x.ProjectFileName = file)
        let! usages =
          Commands.symbolUseWorkspace( findReferencesForSymbolInFile, tryGetFileSource, getProjectOptions, projectsThatContainFile, getDependentProjectsOfProjects, getProjectOptionsForFsproj, pos, lineStr.Value, namedText, tyRes)
           |> AsyncResult.mapError (JsonRpc.Error.InternalErrorMessage)
          // match! tyRes.TryFindTypeDeclaration pos lineStr.Value with
          // | Error e -> return LspResult.internalError e
          // | Ok decl ->
        match usages with
        | Choice1Of2 (decls, usages) ->
          return
            Seq.append decls.Values usages.Values
            |> Seq.collect (fun kvp -> kvp |> Array.map fcsRangeToLspLocation)
            |> Seq.toArray
            |> Some
        | Choice2Of2 combinedRanges ->
          return
            combinedRanges.Values
            |> Seq.collect (fun kvp -> kvp |> Array.map fcsRangeToLspLocation)
            |> Seq.toArray
            |> Some
  }

  override x.TextDocumentDocumentHighlight(p) =
    logger.info (
      Log.setMessage "TextDocumentDocumentHighlight Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )
    Helpers.notImplemented
  override x.TextDocumentImplementation(p) =
    logger.info (
      Log.setMessage "TextDocumentImplementation Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )
    Helpers.notImplemented
  override __.TextDocumentDocumentSymbol(p) = async {
    logger.info (
      Log.setMessage "TextDocumentDocumentSymbol Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )
    let fn = p.TextDocument.GetFilePath() |> Utils.normalizePath
    match getDeclarations fn |> AVal.force with
    | Some decls ->
      return
        glyphToSymbolKind
        |> AVal.map(fun glyphToSymbolKind ->
          decls
          |> Array.collect (fun top -> getSymbolInformations p.TextDocument.Uri glyphToSymbolKind top (fun s -> true))

        )
        |> AVal.force
        |> U2.First
        |> Some
        |> success
    | None ->
      return LspResult.internalError $"No declarations for {fn}"
  }




    // Helpers.notImplemented
  override __.WorkspaceSymbol(symbolRequest: WorkspaceSymbolParams) =
    logger.info (
      Log.setMessage "WorkspaceSymbol Request: {parms}"
      >> Log.addContextDestructured "parms" symbolRequest
    )
    Helpers.notImplemented

  override x.TextDocumentFormatting(p: DocumentFormattingParams) =
    logger.info (
      Log.setMessage "TextDocumentFormatting Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )
    Helpers.notImplemented
  override x.TextDocumentRangeFormatting(p: DocumentRangeFormattingParams) =
    logger.info (
      Log.setMessage "TextDocumentRangeFormatting Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )
    Helpers.notImplemented
  override x.TextDocumentCodeAction(codeActionParams: CodeActionParams) =
    // logger.info (
    //   Log.setMessage "TextDocumentCodeAction Request: {parms}"
    //   >> Log.addContextDestructured "parms" codeActionParams
    // )
    Helpers.notImplemented
  override __.TextDocumentCodeLens(p: CodeLensParams) = asyncResult {
    // logger.info (
    //   Log.setMessage "TextDocumentCodeLens Request: {parms}"
    //   >> Log.addContextDestructured "parms" p
    // )
    let fn = p.TextDocument.GetFilePath() |> Utils.normalizePath
    match getDeclarations (fn) |> AVal.force with
    | None -> return None
    | Some decls ->
      let config = AVal.force config
      let res =
        [| if config.LineLens.Enabled <> "replaceCodeLens" then
              if config.CodeLenses.Signature.Enabled then
                yield! decls |> Array.collect (getCodeLensInformation p.TextDocument.Uri "signature")

              // we have two options here because we're deprecating the EnableReferenceCodeLens one (namespacing, etc)
              if config.EnableReferenceCodeLens || config.CodeLenses.References.Enabled then
                yield! decls |> Array.collect (getCodeLensInformation p.TextDocument.Uri "reference") |]

      return Some res
  }

  override __.CodeLensResolve(p) =
    // logger.info (
    //   Log.setMessage "CodeLensResolve Request: {parms}"
    //   >> Log.addContextDestructured "parms" p
    // )

    let handler f (arg: CodeLens) =
      async {
        let pos = protocolPosToPos arg.Range.Start

        let data = arg.Data.Value.ToObject<string[]>()

        let file = Path.FileUriToLocalPath data.[0] |> Utils.normalizePath

        try
          let tyRes = getTypeCheckResults file |> AVal.force

          match tyRes with
          | None -> return success { p with Command = None }
          | Some tyRes ->

            logger.info (
              Log.setMessage "CodeLensResolve - Cached typecheck results now available for {file}."
              >> Log.addContextDestructured "file" file
            )

            match getFileInfoForFile file |> AVal.force with
            | None -> return success { p with Command = None }
            | Some updates ->
              let lines = updates.NamedText
              let lineStr = lines.GetLine pos

              let typ = data.[1]
              let! r = Async.Catch(f arg pos tyRes lines lineStr typ file)

              match r with
              | Choice1Of2 (r: LspResult<CodeLens option>) ->
                match r with
                | Ok (Some r) -> return Ok r
                | _ -> return Ok Unchecked.defaultof<_>
              | Choice2Of2 e ->
                logger.error (
                  Log.setMessage "CodeLensResolve - Child operation failed for {file}"
                  >> Log.addContextDestructured "file" file
                  >> Log.addExn e
                )

                let title = if typ = "signature" then "" else "0 References"

                let codeLens =
                  { p with
                      Command =
                        Some
                          { Title = title
                            Command = ""
                            Arguments = None } }

                return success codeLens
        with e ->
          logger.error (
            Log.setMessage "CodeLensResolve - Operation failed on {file}"
            >> Log.addContextDestructured "file" file
            >> Log.addExn e
          )

          return { p with Command = None } |> success
      }

    let writePayload (sourceFile: string<LocalPath>, triggerPos: pos, usageLocations: range[]) =
      Some
        [| JToken.FromObject(Path.LocalPathToUri sourceFile)
           JToken.FromObject(fcsPosToLsp triggerPos)
           JToken.FromObject(usageLocations |> Array.map fcsRangeToLspLocation) |]

    handler
      (fun p pos tyRes lines lineStr typ file ->
        async {
          if typ = "signature" then
            match Debug.measure "TryGetSignatureData" (fun () ->  tyRes.TryGetSignatureData pos lineStr.Value |> Result.bimap CoreResponse.Res CoreResponse.ErrorRes) with
            | CoreResponse.InfoRes msg
            | CoreResponse.ErrorRes msg ->
              logger.error (
                Log.setMessage "CodeLensResolve - error on file {file}"
                >> Log.addContextDestructured "file" file
                >> Log.addContextDestructured "error" msg
              )

              return { p with Command = None } |> Some |> success
            | CoreResponse.Res (typ, parms, _) ->
              let formatted = SigantureData.formatSignature typ parms

              let cmd =
                { Title = formatted
                  Command = ""
                  Arguments = None }

              return { p with Command = Some cmd } |> Some |> success
          else
            return { p with Command = None } |> Some |> success


            // let lol = cmap<string, int>()


            // let res =
            //   match res with
            //   | Core.Result.Error msg ->
            //     logger.error (
            //       Log.setMessage "CodeLensResolve - error getting symbol use for {file}"
            //       >> Log.addContextDestructured "file" file
            //       >> Log.addContextDestructured "error" msg
            //     )

            //     success (
            //       Some
            //         { p with
            //             Command =
            //               Some
            //                 { Title = ""
            //                   Command = ""
            //                   Arguments = None } }
            //     )
            //   | Ok res ->
            //     match res with
            //     | Choice1Of2 (_, uses) ->
            //       let allUses = uses.Values |> Array.concat

            //       let cmd =
            //         if allUses.Length = 0 then
            //           { Title = "0 References"
            //             Command = ""
            //             Arguments = None }
            //         else
            //           { Title = $"%d{allUses.Length} References"
            //             Command = "fsharp.showReferences"
            //             Arguments = writePayload (file, pos, allUses) }

            //       { p with Command = Some cmd } |> Some |> success
            //     | Choice2Of2 mixedUsages ->
            //       // mixedUsages will contain the declaration, so we need to do a bit of work here
            //       let allUses = mixedUsages.Values |> Array.concat

            //       let cmd =
            //         if allUses.Length <= 1 then
            //           // 1 reference means that it's only the declaration, so it's actually 0 references
            //           { Title = "0 References"
            //             Command = ""
            //             Arguments = None }
            //         else
            //           // multiple references means that the declaration _and_ the references are all present.
            //           // this is kind of a pain, so for now at least, we just return all of them
            //           { Title = $"%d{allUses.Length - 1} References"
            //             Command = "fsharp.showReferences"
            //             Arguments = writePayload (file, pos, allUses) }

            //       { p with Command = Some cmd } |> Some |> success

            // return res
        })
      p
  override __.WorkspaceDidChangeWatchedFiles(p) =

    logger.info (
      Log.setMessage "WorkspaceDidChangeWatchedFiles Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )
    Helpers.ignoreNotification
  override __.WorkspaceDidChangeConfiguration(p: DidChangeConfigurationParams) =
    logger.info (
      Log.setMessage "WorkspaceDidChangeConfiguration Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )
    Helpers.ignoreNotification
  override __.TextDocumentFoldingRange(rangeP: FoldingRangeParams) =
    logger.info (
      Log.setMessage "TextDocumentFoldingRange Request: {parms}"
      >> Log.addContextDestructured "parms" rangeP
    )
    Helpers.notImplemented

  override __.TextDocumentSelectionRange(selectionRangeP: SelectionRangeParams) =
    logger.info (
      Log.setMessage "TextDocumentSelectionRange Request: {parms}"
      >> Log.addContextDestructured "parms" selectionRangeP
    )
    Helpers.notImplemented

  member private x.handleSemanticTokens (filePath : string<LocalPath>) range
    : LspResult<SemanticTokens option> =

    match getTypeCheckResults (filePath) |> AVal.force with
    | None -> LspResult.internalError $"No typecheck result for {filePath}"
    | Some tyRes ->
      let r = tyRes.GetCheckResults.GetSemanticClassification(range)
      let filteredRanges = Commands.scrubRanges r


      let lspTypedRanges =
        filteredRanges
        |> Array.map (fun item ->
          let ty, mods = ClassificationUtils.map item.Type
          struct (fcsRangeToLsp item.Range, ty, mods))

      match encodeSemanticHighlightRanges lspTypedRanges with
      | None -> success None
      | Some encoded -> success (Some { Data = encoded; ResultId = None }) // TODO: provide a resultId when we support delta ranges


  override x.TextDocumentSemanticTokensFull(p: SemanticTokensParams) : AsyncLspResult<SemanticTokens option> = async {
    logger.info (
      Log.setMessage "Semantic highlighing request: {parms}"
      >> Log.addContextDestructured "parms" p
    )
    let fn = p.TextDocument.GetFilePath() |> Utils.normalizePath
    return x.handleSemanticTokens fn None
  }



  override x.TextDocumentSemanticTokensRange(p: SemanticTokensRangeParams) : AsyncLspResult<SemanticTokens option> = async {
      logger.info (
        Log.setMessage "Semantic highlighing range request: {parms}"
        >> Log.addContextDestructured "parms" p
      )
      let fn = p.TextDocument.GetFilePath() |> Utils.normalizePath
      let fcsRange = protocolRangeToRange (UMX.untag fn) p.Range
      return x.handleSemanticTokens fn (Some fcsRange)
    }

  override x.TextDocumentInlayHint(p: InlayHintParams) : AsyncLspResult<InlayHint[] option> =
    logger.info (
      Log.setMessage "TextDocumentInlayHint Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )
    Helpers.notImplemented

// -----------------
// FSharp Operations
// -----------------

  member x.FSharpSignature(p: TextDocumentPositionParams) =
    logger.info (
      Log.setMessage "FSharpSignature Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )
    Helpers.notImplemented

  member x.FSharpSignatureData(p: TextDocumentPositionParams) = Debug.measureAsync "FSharpSignatureData" <| async {
    logger.info (
      Log.setMessage "FSharpSignatureData Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )
    let pos = FcsPos.mkPos (p.Position.Line) (p.Position.Character + 2)
    let file = p.TextDocument.GetFilePath() |> Utils.normalizePath
    match getFileInfoForFile file |> AVal.force with
    | None -> return LspResult.internalError $"No fileinfo results for {file}"
    | Some updates ->
      let lines = updates.NamedText
      let lineStr = lines.GetLine pos
      return
        match  getTypeCheckResults file |> AVal.force with
        | Some tyRes ->
          match  tyRes.TryGetSignatureData pos lineStr.Value with
          | Ok (typ, parms, generics) ->
              { Content = CommandResponse.signatureData FsAutoComplete.JsonSerializer.writeJson (typ, parms, generics) }
              |> success
          | Error e -> LspResult.internalError e
        | None -> LspResult.internalError $"No typecheck results for {file}"
  }

    // let handler f (arg: TextDocumentPositionParams) =
    //   Debug.measureAsync "FSharpSignatureData.handler" (async {
    //     let pos = FcsPos.mkPos (p.Position.Line) (p.Position.Character + 2)

    //     let file = p.TextDocument.GetFilePath() |> Utils.normalizePath

    //     let updates = knownFsFilesWithUpdates |> AMap.find file |> AVal.force
    //     let lines = updates.NamedText
    //     let lineStr = lines.GetLine pos
    //     let tyResOpt = getTypeCheckResults file |> AVal.force

    //     match tyResOpt with
    //     | None -> return LspResult.internalError "No typecheck results"
    //     | Some tyRes ->
    //       let! r = Async.Catch(f arg pos tyRes lineStr)
    //       match r with
    //       | Choice1Of2 r -> return r
    //       | Choice2Of2 e -> return LspResult.internalError e.Message
    // })


    // p
    // |> handler (fun p pos tyRes lineStr ->

    //   (match Debug.measure "TryGetSignatureData" (fun () ->  tyRes.TryGetSignatureData pos lineStr.Value) with
    //    | Error msg -> LspResult.internalError msg
    //    | Ok (typ, parms, generics) ->
    //      { Content = CommandResponse.signatureData FsAutoComplete.JsonSerializer.writeJson (typ, parms, generics) }
    //      |> success)
    //   |> async.Return)

  member x.FSharpDocumentationGenerator(p: OptionallyVersionedTextDocumentPositionParams) =
    logger.info (
      Log.setMessage "FSharpDocumentationGenerator Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )
    Helpers.notImplemented

  member __.FSharpLineLense(p) =async {
    logger.info (
      Log.setMessage "FSharpLineLense Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )

    let fn = p.Project.GetFilePath() |> Utils.normalizePath
    match getDeclarations fn |> AVal.force with
    | None ->
      //TODO Error handle
      return! Helpers.notImplemented
    | Some decls ->
        let decls = decls |> Array.map(fun d -> d, fn)
        return
          { Content = CommandResponse.declarations FsAutoComplete.JsonSerializer.writeJson decls }
          |> success

  }
  member __.FSharpCompilerLocation(p) =
    logger.info (
      Log.setMessage "FSharpCompilerLocation Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )
    Helpers.notImplemented

  member __.FSharpWorkspaceLoad(p: WorkspaceLoadParms) = async {
    logger.info (
      Log.setMessage "FSharpWorkspaceLoad Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )
    try
      let options = loadedProjectOptions |> AVal.force
      return { Content = CommandResponse.workspaceLoad FsAutoComplete.JsonSerializer.writeJson true }  |> success

    with e ->
      return LspResult.internalError (string e)

  }


  member __.FSharpWorkspacePeek(p: WorkspacePeekRequest) =
    async {
      logger.info (
        Log.setMessage "FSharpWorkspacePeek Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )
      let res = WorkspacePeek.peek p.Directory p.Deep (p.ExcludedDirs |> List.ofArray) |> CoreResponse.Res
      // let! res = commands.WorkspacePeek p.Directory p.Deep (p.ExcludedDirs |> List.ofArray)

      let res =
        match res with
        | CoreResponse.InfoRes msg
        | CoreResponse.ErrorRes msg -> LspResult.internalError msg
        | CoreResponse.Res found ->
          { Content = CommandResponse.workspacePeek FsAutoComplete.JsonSerializer.writeJson found }
          |> success

      return res
    }
  member __.FSharpProject(p) =
    logger.info (
      Log.setMessage "FSharpProject Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )

    Helpers.notImplemented

  member __.FSharpFsdn(p: FsdnRequest) =
    logger.info (
      Log.setMessage "FSharpFsdn Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )

    Helpers.notImplemented

  member __.FSharpDotnetNewList(p: DotnetNewListRequest) =
    logger.info (
      Log.setMessage "FSharpDotnetNewList Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )
    Helpers.notImplemented

  member __.FSharpDotnetNewRun(p: DotnetNewRunRequest) =
    logger.info (
      Log.setMessage "FSharpDotnetNewRun Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )
    Helpers.notImplemented

  member __.FSharpDotnetAddProject(p: DotnetProjectRequest) =
    logger.info (
      Log.setMessage "FSharpDotnetAddProject Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )
    Helpers.notImplemented

  member __.FSharpDotnetRemoveProject(p: DotnetProjectRequest) =
    logger.info (
      Log.setMessage "FSharpDotnetRemoveProject Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )
    Helpers.notImplemented
  member __.FSharpDotnetSlnAdd(p: DotnetProjectRequest) =
    logger.info (
      Log.setMessage "FSharpDotnetSlnAdd Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )
    Helpers.notImplemented

  member x.FSharpHelp(p: TextDocumentPositionParams) =
    logger.info (
      Log.setMessage "FSharpHelp Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )
    Helpers.notImplemented

  member x.FSharpDocumentation(p: TextDocumentPositionParams) =
    logger.info (
      Log.setMessage "FSharpDocumentation Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )
    Helpers.notImplemented

  member x.FSharpDocumentationSymbol(p: DocumentationForSymbolReuqest) =
    logger.info (
      Log.setMessage "FSharpDocumentationSymbol Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )
    Helpers.notImplemented

  member __.LoadAnalyzers(path) =
    logger.info (
      Log.setMessage "LoadAnalyzers Request: {parms}"
      >> Log.addContextDestructured "parms" path
    )
    Helpers.notImplemented

  member x.FSharpPipelineHints(p: FSharpPipelineHintRequest) =
    logger.info (
      Log.setMessage "FSharpPipelineHints Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )
    Helpers.notImplemented

  member __.FsProjMoveFileUp(p: DotnetFileRequest) =
    logger.info (
      Log.setMessage "FsProjMoveFileUp Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )
    Helpers.notImplemented


  member __.FsProjMoveFileDown(p: DotnetFileRequest) =
    logger.info (
      Log.setMessage "FsProjMoveFileDown Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )
    Helpers.notImplemented

  member __.FsProjAddFileAbove(p: DotnetFile2Request) =
    logger.info (
      Log.setMessage "FsProjAddFileAbove Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )
    Helpers.notImplemented

  member __.FsProjAddFileBelow(p: DotnetFile2Request) =
    logger.info (
      Log.setMessage "FsProjAddFileBelow Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )
    Helpers.notImplemented


  member __.FsProjAddFile(p: DotnetFileRequest) =
    logger.info (
      Log.setMessage "FsProjAddFile Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )
    Helpers.notImplemented

  member _.FsProjRemoveFile(p: DotnetFileRequest) =
    logger.info (
      Log.setMessage "FsProjRemoveFile Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )
    Helpers.notImplemented
  override x.Dispose() =
    disposables.Dispose()


type FSharpLspServer(state: State, lspClient: FSharpLspClient) =
  inherit LspServer()

  let logger = LogProvider.getLoggerByName "LSP"
  let fantomasLogger = LogProvider.getLoggerByName "Fantomas"

  let mutable rootPath: string option = None

  let mutable commands =
    new Commands(FSharpCompilerServiceChecker(false), state, false, rootPath)

  let mutable commandDisposables = ResizeArray()
  let mutable clientCapabilities: ClientCapabilities option = None
  let mutable glyphToCompletionKind = glyphToCompletionKindGenerator None
  let mutable glyphToSymbolKind = glyphToSymbolKindGenerator None
  let mutable config = FSharpConfig.Default
  let mutable codeFixes: CodeFix[] = [||]
  let mutable sigHelpKind = None
  let mutable binaryLogConfig = Ionide.ProjInfo.BinaryLogGeneration.Off

  let analyzeFile (filePath, version) =
    let analyzers =
      [
        // if config.Linter then
        //   commands.Lint filePath |> Async.Ignore
        if config.UnusedOpensAnalyzer then
          commands.CheckUnusedOpens filePath
        if config.UnusedDeclarationsAnalyzer then
          commands.CheckUnusedDeclarations filePath
        if config.SimplifyNameAnalyzer then
          commands.CheckSimplifiedNames filePath ]

    async {
      do! analyzers |> Async.Parallel |> Async.Ignore<unit[]>

      do!
        lspClient.NotifyDocumentAnalyzed
          { TextDocument =
              { Uri = filePath |> Path.LocalPathToUri
                Version = Some version } }
    }

  /// UTC Time of start of last `checkFile` call
  /// -> `lastCheckFile - DateTime.UtcNow` is duration between two `checkFile` calls,
  ///    but doesn't include execution time of previous `checkFile`!
  ///
  /// `UtcNow` instead if `Utc`: faster, and we're only interested in elapsed duration
  ///
  /// `DateTime` instead of `Stopwatch`: stopwatch doesn't work with multiple simultaneous consumers
  let mutable lastCheckFile = DateTime.UtcNow

  let checkFile (filePath: string<LocalPath>, version: int, content: NamedText, isFirstOpen: bool) =
    asyncResult {

      let start =
        if config.Debug.LogDurationBetweenCheckFiles then
          let now = DateTime.UtcNow
          let d = now - lastCheckFile
          lastCheckFile <- now

          logger.warn (
            Log.setMessage "Start: checkFile({file}, version={version}), {duration} after last checkFile start"
            >> Log.addContext "file" filePath
            >> Log.addContext "version" version
            >> Log.addContext "duration" d
          )

          now
        elif config.Debug.LogCheckFileDuration then
          DateTime.UtcNow
        else
          Unchecked.defaultof<_>

      let tfmConfig =
        config.UseSdkScripts
        |> function
          | true -> FSIRefs.TFM.NetCore
          | false -> FSIRefs.TFM.NetFx

      if config.Debug.DontCheckRelatedFiles then
        do!
          commands.CheckFile(
            filePath,
            version,
            content,
            tfmConfig,
            checkFilesThatThisFileDependsOn = false,
            checkFilesThatDependsOnFile = false,
            checkDependentProjects = false
          )
      else
        do! commands.CheckFileAndAllDependentFilesInAllProjects(filePath, version, content, tfmConfig, isFirstOpen)

      analyzeFile (filePath, version) |> Async.Start

      if config.Debug.LogCheckFileDuration then
        let d = DateTime.UtcNow - start

        logger.warn (
          Log.setMessage "Finished: checkFile({file}, version={version}) in {duration}"
          >> Log.addContext "file" filePath
          >> Log.addContext "version" version
          >> Log.addContext "duration" d
        )
    }

  let checkChangedFile (p: DidChangeTextDocumentParams) =

    async {
      let doc = p.TextDocument
      let filePath = doc.GetFilePath() |> Utils.normalizePath
      let version = doc.Version.Value // this always has a value, despite the Option type

      match state.TryGetFileSource(filePath) with
      | Ok content ->

        logger.info (
          Log.setMessage "ParseFile - Parsing {file}"
          >> Log.addContextDestructured "file" filePath
        )

        do!
          checkFile (filePath, version, content, false)
          |> AsyncResult.foldResult id (fun e ->
            logger.info (
              Log.setMessage "Error while parsing {file} - {message}"
              >> Log.addContextDestructured "message" e
              >> Log.addContextDestructured "file" filePath
            ))
      | Error _notFound ->
        logger.info (
          Log.setMessage "ParseFile - Unknown file {file}"
          >> Log.addContextDestructured "file" filePath
        )
    }

  let checkFileDebouncer =
    Debounce(DebugConfig.Default.CheckFileDebouncerTimeout, checkChangedFile)

  let sendDiagnostics (uri: DocumentUri) (diags: Diagnostic[]) =
    logger.info (
      Log.setMessage "SendDiag for {file}: {diags} entries"
      >> Log.addContextDestructured "file" uri
      >> Log.addContextDestructured "diags" diags.Length
    )

    { Uri = uri; Diagnostics = diags } |> lspClient.TextDocumentPublishDiagnostics

  let diagnosticCollections = new DiagnosticCollection(sendDiagnostics)

  let handleCommandEvents (n: NotificationEvent) =
    try
      match n with
      | NotificationEvent.FileParsed fn ->
        let uri = Path.LocalPathToUri fn

        lspClient.CodeLensRefresh() |> Async.Start

        ({ Content = UMX.untag uri }: PlainNotification)
        |> lspClient.NotifyFileParsed
        |> Async.Start
      | NotificationEvent.Workspace ws ->
        // logger.info (Log.setMessage "Workspace Notify {ws}" >> Log.addContextDestructured "ws" ws)

        let ws =
          match ws with
          | ProjectResponse.Project (x, _) -> CommandResponse.project JsonSerializer.writeJson x
          | ProjectResponse.ProjectError (_, errorDetails) ->
            CommandResponse.projectError JsonSerializer.writeJson errorDetails
          | ProjectResponse.ProjectLoading (projectFileName) ->
            CommandResponse.projectLoading JsonSerializer.writeJson projectFileName
          | ProjectResponse.WorkspaceLoad (finished) -> CommandResponse.workspaceLoad JsonSerializer.writeJson finished
          | ProjectResponse.ProjectChanged (projectFileName) -> failwith "Not Implemented"

        ({ Content = ws }: PlainNotification)
        |> lspClient.NotifyWorkspace
        |> Async.Start

      | NotificationEvent.ParseError (errors, file) ->
        let uri = Path.LocalPathToUri file
        let diags = errors |> Array.map fcsErrorToDiagnostic
        diagnosticCollections.SetFor(uri, "F# Compiler", diags)

      | NotificationEvent.UnusedOpens (file, opens) ->
        let uri = Path.LocalPathToUri file

        let diags =
          opens
          |> Array.map (fun n ->
            { Range = fcsRangeToLsp n
              Code = Some "FSAC0001"
              Severity = Some DiagnosticSeverity.Hint
              Source = "FSAC"
              Message = "Unused open statement"
              RelatedInformation = None
              Tags = Some [| DiagnosticTag.Unnecessary |]
              Data = None
              CodeDescription = None })

        diagnosticCollections.SetFor(uri, "F# Unused opens", diags)

      | NotificationEvent.UnusedDeclarations (file, decls) ->
        let uri = Path.LocalPathToUri file

        let diags =
          decls
          |> Array.map (fun n ->
            { Range = fcsRangeToLsp n
              Code = Some "FSAC0003"
              Severity = Some DiagnosticSeverity.Hint
              Source = "FSAC"
              Message = "This value is unused"
              RelatedInformation = Some [||]
              Tags = Some [| DiagnosticTag.Unnecessary |]
              Data = None
              CodeDescription = None })

        diagnosticCollections.SetFor(uri, "F# Unused declarations", diags)

      | NotificationEvent.SimplifyNames (file, decls) ->
        let uri = Path.LocalPathToUri file

        let diags =
          decls
          |> Array.map



            (fun ({ Range = range
                    RelativeName = _relName }) ->
              { Diagnostic.Range = fcsRangeToLsp range
                Code = Some "FSAC0002"
                Severity = Some DiagnosticSeverity.Hint
                Source = "FSAC"
                Message = "This qualifier is redundant"
                RelatedInformation = Some [||]
                Tags = Some [| DiagnosticTag.Unnecessary |]
                Data = None
                CodeDescription = None })
        diagnosticCollections.SetFor(uri, "F# simplify names", diags)

      // | NotificationEvent.Lint (file, warnings) ->
      //     let uri = Path.LocalPathToUri file
      //     // let fs =
      //     //     warnings |> List.choose (fun w ->
      //     //         w.Warning.Details.SuggestedFix
      //     //         |> Option.bind (fun f ->
      //     //             let f = f.Force()
      //     //             let range = fcsRangeToLsp w.Warning.Details.Range
      //     //             f |> Option.map (fun f -> range, {Range = range; NewText = f.ToText})
      //     //         )
      //     //     )

      //     let diags =
      //         warnings
      //         |> List.map(fun w ->
      //             let range = fcsRangeToLsp w.Warning.Details.Range
      //             let fixes =
      //               match w.Warning.Details.SuggestedFix with
      //               | None -> None
      //               | Some lazyFix ->
      //                 match lazyFix.Value with
      //                 | None -> None
      //                 | Some fix ->
      //                   Some (box [ { Range = fcsRangeToLsp fix.FromRange; NewText = fix.ToText } ] )
      //             let uri = Option.ofObj w.HelpUrl |> Option.map (fun url -> { Href = Some (Uri url) })
      //             { Range = range
      //               Code = Some w.Code
      //               Severity = Some DiagnosticSeverity.Information
      //               Source = "F# Linter"
      //               Message = w.Warning.Details.Message
      //               RelatedInformation = None
      //               Tags = None
      //               Data = fixes
      //               CodeDescription = uri }
      //         )
      //         |> List.sortBy (fun diag -> diag.Range)
      //         |> List.toArray
      //     diagnosticCollections.SetFor(uri, "F# Linter", diags)

      | NotificationEvent.Canceled (msg) ->
        let ntf: PlainNotification = { Content = msg }

        lspClient.NotifyCancelledRequest ntf |> Async.Start
      | NotificationEvent.AnalyzerMessage (messages, file) ->
        let uri = Path.LocalPathToUri file

        match messages with
        | [||] -> diagnosticCollections.SetFor(uri, "F# Analyzers", [||])
        | messages ->
          let diags =
            messages
            |> Array.map (fun m ->
              let range = fcsRangeToLsp m.Range

              let severity =
                match m.Severity with
                | FSharp.Analyzers.SDK.Info -> DiagnosticSeverity.Information
                | FSharp.Analyzers.SDK.Warning -> DiagnosticSeverity.Warning
                | FSharp.Analyzers.SDK.Error -> DiagnosticSeverity.Error

              let fixes =
                match m.Fixes with
                | [] -> None
                | fixes ->
                  fixes
                  |> List.map (fun fix ->
                    { Range = fcsRangeToLsp fix.FromRange
                      NewText = fix.ToText })
                  |> Ionide.LanguageServerProtocol.Server.serialize
                  |> Some

              { Range = range
                Code = Option.ofObj m.Code
                Severity = Some severity
                Source = $"F# Analyzers (%s{m.Type})"
                Message = m.Message
                RelatedInformation = None
                Tags = None
                CodeDescription = None
                Data = fixes })

          diagnosticCollections.SetFor(uri, "F# Analyzers", diags)
      | NotificationEvent.TestDetected (file, tests) ->
        let rec map
          (r: TestAdapter.TestAdapterEntry<FSharp.Compiler.Text.range>)
          : TestAdapter.TestAdapterEntry<Ionide.LanguageServerProtocol.Types.Range> =
          { Id = r.Id
            List = r.List
            Name = r.Name
            Type = r.Type
            Range = fcsRangeToLsp r.Range
            Childs = ResizeArray(r.Childs |> Seq.map map) }

        { File = Path.LocalPathToUri file
          Tests = tests |> Array.map map }
        |> lspClient.NotifyTestDetected
        |> Async.Start
    with ex ->
      logger.error (
        Log.setMessage "Exception while handling command event {evt}: {ex}"
        >> Log.addContextDestructured "evt" n
        >> Log.addContext "ex" ex.Message
      )

  let updateDebugConfig (newConfig: DebugConfig, oldConfig: DebugConfig) =
    if newConfig.DontCheckRelatedFiles <> oldConfig.DontCheckRelatedFiles then
      if newConfig.DontCheckRelatedFiles then
        logger.warn (Log.setMessage "Checking of related files disabled!")
      else
        logger.warn (Log.setMessage "Checking of related files enabled (default)")


    if newConfig.CheckFileDebouncerTimeout < 0 then
      logger.error (
        Log.setMessage "CheckFileDebouncerTimeout cannot be negative, but is {timeout}"
        >> Log.addContext "timeout" newConfig.CheckFileDebouncerTimeout
      )
    elif newConfig.CheckFileDebouncerTimeout <> oldConfig.CheckFileDebouncerTimeout then
      if DebugConfig.Default.CheckFileDebouncerTimeout = newConfig.CheckFileDebouncerTimeout then
        logger.warn (
          Log.setMessage "Changing checkFileDebouncer.Timeout to {newTimeout} (default) (from {oldTimeout})"
          >> Log.addContext "newTimeout" newConfig.CheckFileDebouncerTimeout
          >> Log.addContext "oldTimeout" checkFileDebouncer.Timeout
        )
      else
        logger.warn (
          Log.setMessage "Changing checkFileDebouncer.Timeout to {newTimeout} (from {oldTimeout})"
          >> Log.addContext "newTimeout" newConfig.CheckFileDebouncerTimeout
          >> Log.addContext "oldTimeout" checkFileDebouncer.Timeout
        )

      checkFileDebouncer.Timeout <- newConfig.CheckFileDebouncerTimeout

    if newConfig.LogDurationBetweenCheckFiles <> oldConfig.LogDurationBetweenCheckFiles then
      if newConfig.LogDurationBetweenCheckFiles then
        logger.warn (Log.setMessage "Enabled: log duration between checkFile")
      else
        logger.warn (Log.setMessage "Disabled: log duration between checkFile (default)")

    if newConfig.LogCheckFileDuration <> oldConfig.LogCheckFileDuration then
      if newConfig.LogCheckFileDuration then
        logger.warn (Log.setMessage "Enabled: log checkFile duration")
      else
        logger.warn (Log.setMessage "Disabled: log checkFile duration (default)")

  /// centralize any state changes when the config is updated here
  let updateConfig (newConfig: FSharpConfig) =
    updateDebugConfig (newConfig.Debug, config.Debug)

    let toCompilerToolArgument (path: string) = sprintf "--compilertool:%s" path
    config <- newConfig

    let hadAnalyzersBefore = SDK.Client.registeredAnalyzers.Count <> 0

    if config.EnableAnalyzers then
      Loggers.analyzers.info (
        Log.setMessage "Using analyzer roots of {roots}"
        >> Log.addContextDestructured "roots" config.AnalyzersPath
      )

      config.AnalyzersPath
      |> Array.iter (fun analyzerPath ->
        match rootPath with
        | None -> ()
        | Some workspacePath ->
          let dir =
            if
              System.IO.Path.IsPathRooted analyzerPath
            // if analyzer is using absolute path, use it as is
            then
              analyzerPath
            // otherwise, it is a relative path and should be combined with the workspace path
            else
              System.IO.Path.Combine(workspacePath, analyzerPath)

          Loggers.analyzers.info (
            Log.setMessage "Loading analyzers from {dir}"
            >> Log.addContextDestructured "dir" dir
          )

          let (n, m) = dir |> SDK.Client.loadAnalyzers

          Loggers.analyzers.info (
            Log.setMessage "From {name}: {dllNo} dlls including {analyzersNo} analyzers"
            >> Log.addContextDestructured "name" analyzerPath
            >> Log.addContextDestructured "dllNo" n
            >> Log.addContextDestructured "analyzersNo" m
          ))
    // otherwise, it is a relative path and should be combined with the workspace path
    else
      Loggers.analyzers.info (Log.setMessage "Analyzers disabled")

    let hasAnalyzersNow = SDK.Client.registeredAnalyzers.Count <> 0

    if hadAnalyzersBefore <> hasAnalyzersNow then
      let oldCommands = commands
      let oldDisposables = commandDisposables

      let newCommands =
        new Commands(FSharpCompilerServiceChecker(hasAnalyzersNow), state, hasAnalyzersNow, rootPath)

      commands <- newCommands
      commandDisposables <- ResizeArray()
      commands.SetWorkspaceRoot rootPath
      commandDisposables.Add(commands.Notify.Subscribe handleCommandEvents)

      for (disposable: IDisposable) in oldDisposables do
        disposable.Dispose()

      (oldCommands :> IDisposable).Dispose()

    // only update the dotnet root if it's both a directory and exists
    let di = DirectoryInfo config.DotNetRoot

    if di.Exists then
      let dotnetBinary =
        if
          System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(Runtime.InteropServices.OSPlatform.Windows)
        then
          FileInfo(Path.Combine(di.FullName, "dotnet.exe"))
        else
          FileInfo(Path.Combine(di.FullName, "dotnet"))

      if dotnetBinary.Exists then
        commands.SetDotnetSDKRoot dotnetBinary
      else
        ()
    else
      // if we were mistakenly given the path to a dotnet binary
      // then use the parent directory as the dotnet root instead
      let fi = FileInfo(di.FullName)

      if fi.Exists && (fi.Name = "dotnet" || fi.Name = "dotnet.exe") then
        commands.SetDotnetSDKRoot fi

    commands.SetFSIAdditionalArguments
      [| yield! config.FSICompilerToolLocations |> Array.map toCompilerToolArgument
         yield! config.FSIExtraParameters |]

    commands.SetLinterConfigRelativePath config.LinterConfig
    // TODO(CH): make the destination part of config, so that non-FSAC editors don't have the '.ionide' path
    binaryLogConfig <-
      match config.GenerateBinlog, rootPath with
      | _, None
      | false, _ -> Ionide.ProjInfo.BinaryLogGeneration.Off
      | true, Some rootPath ->
        Ionide.ProjInfo.BinaryLogGeneration.Within(DirectoryInfo(Path.Combine(rootPath, ".ionide")))

    ()

  do commandDisposables.Add <| commands.Notify.Subscribe handleCommandEvents

  /// Removes all caches (state & diagnostics) if:
  /// * file doesn't exist on disk (untitled or deleted)
  /// * file is outside of current workspace (and not part of any project)
  let forgetDocument (uri: DocumentUri) =
    let filePath = uri |> Path.FileUriToLocalPath |> Utils.normalizePath

    // remove cached data for
    // * non-existing files (untitled & deleted)
    // * files outside of workspace (and not used by any project)
    let doesNotExist (file: string<LocalPath>) = not (File.Exists(UMX.untag file))

    let isOutsideWorkspace (file: string<LocalPath>) =
      match rootPath with
      | None ->
        // no workspace specified
        true
      | Some rootPath ->
        let rec isInside (rootDir: DirectoryInfo, dirToCheck: DirectoryInfo) =
          if String.Equals(rootDir.FullName, dirToCheck.FullName, StringComparison.InvariantCultureIgnoreCase) then
            true
          else
            match dirToCheck.Parent with
            | null -> false
            | parent -> isInside (rootDir, parent)

        let rootDir = DirectoryInfo(rootPath)
        let fileDir = FileInfo(UMX.untag file).Directory

        if isInside (rootDir, fileDir) then
          false
        else
          // file might be outside of workspace but part of a project
          match state.GetProjectOptions file with
          | None -> true
          | Some projOptions ->
            if doesNotExist (UMX.tag projOptions.ProjectFileName) then
              // case for script file
              true
            else
              // issue: fs-file does never get removed from project options (-> requires reload of FSAC to register)
              // -> don't know if file still part of project (file might have been removed from project)
              // -> keep cache for file
              false

    if doesNotExist filePath || isOutsideWorkspace filePath then
      logger.info (
        Log.setMessage "Removing cached data for {file}"
        >> Log.addContext "file" filePath
      )

      state.Forget filePath
      diagnosticCollections.ClearFor uri

  ///Helper function for handling Position requests using **recent** type check results
  member x.positionHandler<'a, 'b when 'b :> ITextDocumentPositionParams>
    (f: 'b -> FcsPos -> ParseAndCheckResults -> string -> NamedText -> AsyncLspResult<'a>)
    (arg: 'b)
    : AsyncLspResult<'a> =
    async {
      let pos = arg.GetFcsPos()
      let file = arg.GetFilePath() |> Utils.normalizePath
      // logger.info (Log.setMessage "PositionHandler - Position request: {file} at {pos}" >> Log.addContextDestructured "file" file >> Log.addContextDestructured "pos" pos)

      match commands.TryGetFileCheckerOptionsWithLinesAndLineStr(file, pos) with
      | ResultOrString.Error s ->
        logger.error (
          Log.setMessage "PositionHandler - Getting file checker options for {file} failed"
          >> Log.addContextDestructured "error" s
          >> Log.addContextDestructured "file" file
        )

        return! AsyncLspResult.internalError s
      | ResultOrString.Ok (options, lines, lineStr) ->
        try
          let! tyResOpt = commands.TryGetRecentTypeCheckResultsForFile(file, options, lines)

          match tyResOpt with
          | None ->
            logger.info (
              Log.setMessage "PositionHandler - Cached typecheck results not yet available for {file}"
              >> Log.addContextDestructured "file" file
            )

            return! AsyncLspResult.success Unchecked.defaultof<_>
          | Some tyRes ->
            let! r = Async.Catch(f arg pos tyRes lineStr lines)

            match r with
            | Choice1Of2 r -> return r
            | Choice2Of2 e ->
              logger.error (
                Log.setMessage "PositionHandler - Failed during child operation on file {file}"
                >> Log.addContextDestructured "file" file
                >> Log.addExn e
              )

              return LspResult.internalError e.Message
        with e ->
          logger.error (
            Log.setMessage "PositionHandler - Operation failed for file {file}"
            >> Log.addContextDestructured "file" file
            >> Log.addExn e
          )

          return! AsyncLspResult.internalError e.Message
    }

  ///Helper function for handling file requests using **recent** type check results
  member x.fileHandler<'a>
    (f: string<LocalPath> -> ParseAndCheckResults -> NamedText -> AsyncLspResult<'a>)
    (arg: TextDocumentIdentifier)
    : AsyncLspResult<'a> =
    async {
      let file = arg.GetFilePath() |> Utils.normalizePath

      // logger.info (Log.setMessage "PositionHandler - Position request: {file} at {pos}" >> Log.addContextDestructured "file" file >> Log.addContextDestructured "pos" pos)
      match commands.TryGetFileCheckerOptionsWithLines(file) with
      | ResultOrString.Error s ->
        logger.error (
          Log.setMessage "FileHandler - Getting file checker options for {file} failed"
          >> Log.addContextDestructured "error" s
          >> Log.addContextDestructured "file" file
        )

        return LspResult.internalError s
      | ResultOrString.Ok (options, lines) ->
        try
          let! tyResOpt = commands.TryGetRecentTypeCheckResultsForFile(file)

          match tyResOpt with
          | None ->
            logger.info (
              Log.setMessage "FileHandler - Cached typecheck results not yet available for {file}"
              >> Log.addContextDestructured "file" file
            )

            return LspResult.internalError "Cached typecheck results not yet available"
          | Some tyRes ->
            let! r = Async.Catch(f file tyRes lines)

            match r with
            | Choice1Of2 r -> return r
            | Choice2Of2 e ->
              logger.error (
                Log.setMessage "FileHandler - Failed during child operation on file {file}"
                >> Log.addContextDestructured "file" file
                >> Log.addExn e
              )

              return LspResult.internalError e.Message
        with e ->
          logger.error (
            Log.setMessage "FileHandler - Operation failed for file {file}"
            >> Log.addContextDestructured "file" file
            >> Log.addExn e
          )

          return LspResult.internalError e.Message
    }

  override __.Shutdown() =
    async {
      for (dispose: IDisposable) in commandDisposables do
        dispose.Dispose()

      (commands :> IDisposable).Dispose()
      (diagnosticCollections :> IDisposable).Dispose()
    }

  override _.Initialize(p: InitializeParams) =
    async {
      logger.info (Log.setMessage "Initialize Request {p}" >> Log.addContextDestructured "p" p)

      let actualRootPath =
        match p.RootUri with
        | Some rootUri -> Some(Path.FileUriToLocalPath rootUri)
        | None -> p.RootPath

      rootPath <- actualRootPath
      commands.SetWorkspaceRoot actualRootPath

      let c =
        p.InitializationOptions
        |> Option.bind (fun options -> if options.HasValues then Some options else None)
        |> Option.map Server.deserialize<FSharpConfigDto>
        |> Option.map FSharpConfig.FromDto
        |> Option.defaultValue FSharpConfig.Default

      updateConfig c


      clientCapabilities <- p.Capabilities
      lspClient.ClientCapabilities <- clientCapabilities
      glyphToCompletionKind <- glyphToCompletionKindGenerator clientCapabilities
      glyphToSymbolKind <- glyphToSymbolKindGenerator clientCapabilities

      let tryGetParseResultsForFile fileName pos =
        asyncResult {
          let! (projectOptions, fileLines, lineAtPos) =
            commands.TryGetFileCheckerOptionsWithLinesAndLineStr(fileName, pos)

          match! commands.TryGetRecentTypeCheckResultsForFile(fileName) with
          | None -> return! Error $"No typecheck results available for %A{fileName}"
          | Some tyRes -> return tyRes, lineAtPos, fileLines
        }


      let getFileLines = commands.TryGetFileCheckerOptionsWithLines >> Result.map snd

      let getLineText (lines: NamedText) (range: Ionide.LanguageServerProtocol.Types.Range) =
        lines.GetText(protocolRangeToRange (UMX.untag lines.FileName) range)

      let getRangeText fileName (range: Ionide.LanguageServerProtocol.Types.Range) =
        getFileLines fileName
        |> Result.bind (fun lines -> lines.GetText(protocolRangeToRange (UMX.untag fileName) range))

      let getProjectOptsAndLines = commands.TryGetFileCheckerOptionsWithLinesAndLineStr

      let tryGetProjectOptions =
        commands.TryGetFileCheckerOptionsWithLines >> Result.map fst

      let implementInterfaceConfig () : ImplementInterface.Config =
        { ObjectIdentifier = config.InterfaceStubGenerationObjectIdentifier
          MethodBody = config.InterfaceStubGenerationMethodBody
          IndentationSize = config.IndentationSize }

      let unionCaseStubReplacements () =
        Map.ofList [ "$1", config.UnionCaseStubGenerationBody ]

      let getUnionCaseStubReplacements () = unionCaseStubReplacements ()

      let recordStubReplacements () =
        Map.ofList [ "$1", config.RecordStubGenerationBody ]

      let getRecordStubReplacements () = recordStubReplacements ()

      let abstractClassStubReplacements () =
        Map.ofList
          [ "$objectIdent", config.AbstractClassStubGenerationObjectIdentifier
            "$methodBody", config.AbstractClassStubGenerationMethodBody ]

      let getAbstractClassStubReplacements () = abstractClassStubReplacements ()

      codeFixes <-
        [| Run.ifEnabled (fun _ -> config.UnusedOpensAnalyzer) (RemoveUnusedOpens.fix getFileLines)
           Run.ifEnabled
             (fun _ -> config.ResolveNamespaces)
             (ResolveNamespace.fix tryGetParseResultsForFile commands.GetNamespaceSuggestions)
           ReplaceWithSuggestion.fix
           RemoveRedundantQualifier.fix
           Run.ifEnabled (fun _ -> config.UnusedDeclarationsAnalyzer) (RenameUnusedValue.fix tryGetParseResultsForFile)
           AddNewKeywordToDisposableConstructorInvocation.fix getRangeText
           Run.ifEnabled
             (fun _ -> config.UnionCaseStubGeneration)
             (GenerateUnionCases.fix
               getFileLines
               tryGetParseResultsForFile
               commands.GetUnionPatternMatchCases
               getUnionCaseStubReplacements)
           ExternalSystemDiagnostics.linter
           ExternalSystemDiagnostics.analyzers
           Run.ifEnabled
             (fun _ -> config.InterfaceStubGeneration)
             (ImplementInterface.fix tryGetParseResultsForFile tryGetProjectOptions implementInterfaceConfig)
           Run.ifEnabled
             (fun _ -> config.RecordStubGeneration)
             (GenerateRecordStub.fix tryGetParseResultsForFile commands.GetRecordStub getRecordStubReplacements)
           Run.ifEnabled
             (fun _ -> config.AbstractClassStubGeneration)
             (GenerateAbstractClassStub.fix
               tryGetParseResultsForFile
               commands.GetAbstractClassStub
               getAbstractClassStubReplacements)
           AddMissingEqualsToTypeDefinition.fix getFileLines
           ChangePrefixNegationToInfixSubtraction.fix getFileLines
           ConvertDoubleEqualsToSingleEquals.fix getRangeText
           ChangeEqualsInFieldTypeToColon.fix
           WrapExpressionInParentheses.fix getRangeText
           ChangeRefCellDerefToNot.fix tryGetParseResultsForFile
           ChangeDowncastToUpcast.fix getRangeText
           MakeDeclarationMutable.fix tryGetParseResultsForFile tryGetProjectOptions
           UseMutationWhenValueIsMutable.fix tryGetParseResultsForFile
           ConvertInvalidRecordToAnonRecord.fix tryGetParseResultsForFile
           RemoveUnnecessaryReturnOrYield.fix tryGetParseResultsForFile getLineText
           ConvertCSharpLambdaToFSharpLambda.fix tryGetParseResultsForFile getLineText
           AddMissingFunKeyword.fix getFileLines getLineText
           MakeOuterBindingRecursive.fix tryGetParseResultsForFile getLineText
           AddMissingRecKeyword.fix getFileLines getLineText
           ConvertBangEqualsToInequality.fix getRangeText
           ChangeDerefBangToValue.fix tryGetParseResultsForFile getLineText
           RemoveUnusedBinding.fix tryGetParseResultsForFile
           AddTypeToIndeterminateValue.fix tryGetParseResultsForFile tryGetProjectOptions
           ChangeTypeOfNameToNameOf.fix tryGetParseResultsForFile
           AddMissingInstanceMember.fix
           AddExplicitTypeAnnotation.fix tryGetParseResultsForFile
           ConvertPositionalDUToNamed.fix tryGetParseResultsForFile getRangeText
           UseTripleQuotedInterpolation.fix tryGetParseResultsForFile getRangeText
           RenameParamToMatchSignature.fix tryGetParseResultsForFile |]


      match p.RootPath, c.AutomaticWorkspaceInit with
      | None, _
      | _, false -> ()
      | Some p, true ->
        async {
          let! peek =
            commands.WorkspacePeek p config.WorkspaceModePeekDeepLevel (List.ofArray config.ExcludeProjectDirectories)

          match peek with
          | CoreResponse.InfoRes msg
          | CoreResponse.ErrorRes msg -> ()
          | CoreResponse.Res ints ->

            let serialized = CommandResponse.workspacePeek JsonSerializer.writeJson ints

            lspClient.NotifyWorkspacePeek { Content = serialized } |> Async.Start

            let peeks =
              ints
              |> List.map Workspace.mapInteresting
              |> List.sortByDescending (fun x ->
                match x with
                | CommandResponse.WorkspacePeekFound.Solution sln -> Workspace.countProjectsInSln sln
                | CommandResponse.WorkspacePeekFound.Directory _ -> -1)

            logger.info (
              Log.setMessage "Choosing from interesting items {items}"
              >> Log.addContextDestructured "items" peeks
            )

            match peeks with
            | [] -> ()
            | [ CommandResponse.WorkspacePeekFound.Directory projs ] ->
              commands.WorkspaceLoad projs.Fsprojs false binaryLogConfig
              |> Async.Ignore
              |> Async.Start
            | CommandResponse.WorkspacePeekFound.Solution sln :: _ ->
              let projs = sln.Items |> List.collect Workspace.foldFsproj |> List.map fst

              commands.WorkspaceLoad projs false binaryLogConfig
              |> Async.Ignore
              |> Async.Start
            | _ ->
              //TODO: Above case always picks solution with most projects, should be changed
              ()


          return ()
        }
        |> Async.Start

      return
        { InitializeResult.Default with
            Capabilities =
              { ServerCapabilities.Default with
                  HoverProvider = Some true
                  RenameProvider = Some(U2.First true)
                  DefinitionProvider = Some true
                  TypeDefinitionProvider = Some true
                  ImplementationProvider = Some true
                  ReferencesProvider = Some true
                  DocumentHighlightProvider = Some true
                  DocumentSymbolProvider = Some true
                  WorkspaceSymbolProvider = Some true
                  DocumentFormattingProvider = Some true
                  DocumentRangeFormattingProvider = Some true
                  SignatureHelpProvider =
                    Some
                      { TriggerCharacters = Some [| '('; ','; ' ' |]
                        RetriggerCharacters = Some [| ','; ')'; ' ' |] }
                  CompletionProvider =
                    Some
                      { ResolveProvider = Some true
                        TriggerCharacters = Some([| '.'; ''' |])
                        AllCommitCharacters = None //TODO: what chars shoudl commit completions?
                      }
                  CodeLensProvider = Some { CodeLensOptions.ResolveProvider = Some true }
                  CodeActionProvider =
                    Some
                      { CodeActionKinds = None
                        ResolveProvider = None }
                  TextDocumentSync =
                    Some
                      { TextDocumentSyncOptions.Default with
                          OpenClose = Some true
                          Change = Some TextDocumentSyncKind.Full
                          Save = Some { IncludeText = Some true } }
                  FoldingRangeProvider = Some true
                  SelectionRangeProvider = Some true
                  SemanticTokensProvider =
                    Some
                      { Legend =
                          createTokenLegend<ClassificationUtils.SemanticTokenTypes, ClassificationUtils.SemanticTokenModifier>
                        Range = Some true
                        Full = Some(U2.First true) }
                  InlayHintProvider = Some { ResolveProvider = Some false } } }
        |> success
    }

  override __.Initialized(p: InitializedParams) =
    async {
      logger.info (Log.setMessage "Initialized request")
      return ()
    }

  override __.TextDocumentDidOpen(p: DidOpenTextDocumentParams) =
    async {
      let doc = p.TextDocument
      let filePath = doc.GetFilePath() |> Utils.normalizePath
      let content = NamedText(filePath, doc.Text)

      logger.info (
        Log.setMessage "TextDocumentDidOpen Request: {parms}"
        >> Log.addContextDestructured "parms" filePath
      )

      commands.SetFileContent(filePath, content, Some doc.Version)

      do!
        checkFile (filePath, doc.Version, content, true)
        |> AsyncResult.foldResult id (fun e ->
          logger.info (
            Log.setMessage "Error while parsing {file} - {message}"
            >> Log.addContextDestructured "message" e
            >> Log.addContextDestructured "file" filePath
          ))
    }

  override __.TextDocumentDidChange(p: DidChangeTextDocumentParams) =
    async {

      let doc = p.TextDocument
      let filePath = doc.GetFilePath() |> Utils.normalizePath
      // types are incorrect for this endpoint - version is always supplied by the client
      let endVersion = doc.Version.Value

      logger.info (
        Log.setMessage "TextDocumentDidChange Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      let initialText =
        state.TryGetFileSource(filePath)
        |> Result.fold id (fun _ -> NamedText(filePath, ""))

      let evolvedFileContent =
        (initialText, p.ContentChanges)
        ||> Array.fold (fun text change ->
          match change.Range with
          | None -> // replace entire content
            NamedText(filePath, change.Text)
          | Some rangeToReplace ->
            // replace just this slice
            let fcsRangeToReplace = protocolRangeToRange (UMX.untag filePath) rangeToReplace

            match text.ModifyText(fcsRangeToReplace, change.Text) with
            | Ok text -> text
            | Error message ->
              logger.error (
                Log.setMessage "Error applying change to document {file} for version {version}: {message}"
                >> Log.addContextDestructured "file" filePath
                >> Log.addContextDestructured "version" endVersion
                >> Log.addContextDestructured "message" message
              )
              text
        )


      commands.SetFileContent(filePath, evolvedFileContent, Some endVersion)

      checkFileDebouncer.Bounce p
    }

  //TODO: Investigate if this should be done at all
  override __.TextDocumentDidSave(p) =
    async {
      logger.info (
        Log.setMessage "TextDocumentDidSave Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      ()
    }

  override _.TextDocumentDidClose(p) =
    async {
      logger.info (
        Log.setMessage "TextDocumentDidOpen Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      forgetDocument p.TextDocument.Uri
    }

  override __.TextDocumentCompletion(p: CompletionParams) =
    asyncResult {
      logger.info (
        Log.setMessage "TextDocumentCompletion Request: {context}"
        >> Log.addContextDestructured "context" p
      )
      // Sublime-lsp doesn't like when we answer null so we answer an empty list instead
      let file = p.TextDocument.GetFilePath() |> Utils.normalizePath

      let pos = p.GetFcsPos()

      match commands.TryGetFileCheckerOptionsWithLines file with
      | Error _ -> return! success None
      | Ok (options, lines) ->

        match lines.GetLine pos with
        | None -> return! success None
        | Some lineStr ->

          if lineStr.StartsWith "#" then
            let completionList =
              { IsIncomplete = false
                Items = KeywordList.hashSymbolCompletionItems }

            return! success (Some completionList)
          else
            let! typeCheckResults =
              commands.TryGetRecentTypeCheckResultsForFile(file)
              |> AsyncResult.ofOption (fun _ -> JsonRpc.Error.InternalErrorMessage "No Typecheck results")


            match!
              commands.Completion
                typeCheckResults
                pos
                lineStr
                lines
                file
                None
                (config.KeywordsAutocomplete)
                (config.ExternalAutocomplete)
            with
            | CoreResponse.Res (decls, keywords) ->
              let items =
                decls
                |> Array.mapi (fun id d ->
                  let code =
                    if System.Text.RegularExpressions.Regex.IsMatch(d.Name, """^[a-zA-Z][a-zA-Z0-9']+$""") then
                      d.Name
                    elif d.NamespaceToOpen.IsSome then
                      d.Name
                    else
                      FSharpKeywords.AddBackticksToIdentifierIfNeeded d.Name

                  let label =
                    match d.NamespaceToOpen with
                    | Some no -> sprintf "%s (open %s)" d.Name no
                    | None -> d.Name

                  { CompletionItem.Create(d.Name) with
                      Kind = glyphToCompletionKind d.Glyph
                      InsertText = Some code
                      SortText = Some(sprintf "%06d" id)
                      FilterText = Some d.Name })

              let its =
                if not keywords then
                  items
                else
                  Array.append items KeywordList.keywordCompletionItems

              let completionList = { IsIncomplete = false; Items = its }
              return! success (Some completionList)
            | _ ->
              logger.info (Log.setMessage "TextDocumentCompletion - no completion results")
              return! success (Some { IsIncomplete = false; Items = [||] })
    }

  override __.CompletionItemResolve(ci: CompletionItem) =
    let mapHelpText (ci: CompletionItem) (text: HelpText) =
      match text with
      | HelpText.Simple (symbolName, text) ->
        let d = Documentation.Markup(markdown text)

        { ci with
            Detail = Some symbolName
            Documentation = Some d }
      | HelpText.Full (name, tip, additionalEdit) ->
        let (si, comment) = TipFormatter.formatCompletionItemTip tip

        let edits, label =
          match additionalEdit with
          | None -> None, ci.Label
          | Some { Namespace = ns; Position = fcsPos } ->
            let text =
              let indentation = String(' ', fcsPos.Column)
              $"{indentation}open {ns}\n"

            let insertPos = { (fcsPos |> fcsPosToLsp) with Character = 0 }

            Some
              [| { TextEdit.NewText = text
                   TextEdit.Range = { Start = insertPos; End = insertPos } } |],
            $"{ci.Label} (open {ns})"

        let d = Documentation.Markup(markdown comment)

        { ci with
            Detail = Some si
            Documentation = Some d
            AdditionalTextEdits = edits
            Label = label }

    async {
      logger.info (
        Log.setMessage "CompletionItemResolve Request: {parms}"
        >> Log.addContextDestructured "parms" ci
      )

      let! res =
        commands.Helptext ci.InsertText.Value
        |> AsyncResult.ofCoreResponse
        |> AsyncResult.foldResult (mapHelpText ci) (fun _ -> ci)

      return success res
    }

  override x.TextDocumentSignatureHelp(sigHelpParams: SignatureHelpParams) =
    logger.info (
      Log.setMessage "TextDocumentSignatureHelp Request: {parms}"
      >> Log.addContextDestructured "parms" sigHelpParams
    )

    sigHelpParams
    |> x.positionHandler (fun sigHelpParams fcsPos tyRes lineStr lines ->
      asyncResult {
        let charAtCaret = sigHelpParams.Context |> Option.bind (fun c -> c.TriggerCharacter)

        match!
          commands.MethodsForSignatureHelp(tyRes, fcsPos, lines, charAtCaret, sigHelpKind)
          |> AsyncResult.ofStringErr
        with
        | None ->
          sigHelpKind <- None
          return! success None
        | Some sigHelp ->
          sigHelpKind <- Some sigHelp.SigHelpKind

          let sigs =
            sigHelp.Methods
            |> Array.map (fun m ->
              let (signature, comment) = TipFormatter.formatPlainTip m.Description

              let parameters =
                m.Parameters
                |> Array.map (fun p ->
                  { ParameterInformation.Label = p.ParameterName
                    Documentation = Some(Documentation.String p.CanonicalTypeTextForSorting) })

              let d = Documentation.Markup(markdown comment)

              { SignatureInformation.Label = signature
                Documentation = Some d
                Parameters = Some parameters })

          let res =
            { Signatures = sigs
              ActiveSignature = sigHelp.ActiveOverload
              ActiveParameter = sigHelp.ActiveParameter }

          return! success (Some res)
      })

  override x.TextDocumentHover(p: TextDocumentPositionParams) =
    logger.info (
      Log.setMessage "TextDocumentHover Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )

    p
    |> x.positionHandler (fun p pos tyRes lineStr lines ->
      match commands.ToolTip tyRes pos lineStr with
      | CoreResponse.InfoRes msg
      | CoreResponse.ErrorRes msg -> LspResult.internalError msg |> async.Return
      | CoreResponse.Res None -> async.Return(success None)
      | CoreResponse.Res (Some (tip, signature, footer, typeDoc)) ->
        let formatCommentStyle =
          if config.TooltipMode = "full" then
            TipFormatter.FormatCommentStyle.FullEnhanced
          else if config.TooltipMode = "summary" then
            TipFormatter.FormatCommentStyle.SummaryOnly
          else
            TipFormatter.FormatCommentStyle.Legacy

        match TipFormatter.formatTipEnhanced tip signature footer typeDoc formatCommentStyle with
        | (sigCommentFooter :: _) :: _ ->
          let signature, comment, footer = sigCommentFooter

          let markStr lang (value: string) =
            MarkedString.WithLanguage { Language = lang; Value = value }

          let fsharpBlock (lines: string[]) =
            lines |> String.concat Environment.NewLine |> markStr "fsharp"

          let sigContent =
            let lines =
              signature.Split Environment.NewLine
              |> Array.filter (not << String.IsNullOrWhiteSpace)

            match lines |> Array.splitAt (lines.Length - 1) with
            | (h, [| StartsWith "Full name:" fullName |]) ->
              [| yield fsharpBlock h; yield MarkedString.String("*" + fullName + "*") |]
            | _ -> [| fsharpBlock lines |]


          let commentContent = comment |> MarkedString.String

          let footerContent =
            footer.Split Environment.NewLine
            |> Array.filter (not << String.IsNullOrWhiteSpace)
            |> Array.map (fun n -> MarkedString.String("*" + n + "*"))


          let response =
            { Contents = MarkedStrings [| yield! sigContent; yield commentContent; yield! footerContent |]
              Range = None }

          async.Return(success (Some response))
        | _ -> async.Return(success None))

  override x.TextDocumentRename(p: RenameParams) =
    logger.info (
      Log.setMessage "TextDocumentRename Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )

    p
    |> x.positionHandler (fun p pos tyRes lineStr lines ->
      asyncResult {
        let! documentsAndRanges =
          commands.RenameSymbol(pos, tyRes, lineStr, lines)
          |> Async.Catch
          |> Async.map (function
            | Choice1Of2 v -> v
            | Choice2Of2 err -> Error err.Message)
          |> AsyncResult.mapError (JsonRpc.Error.InternalErrorMessage)

        let documentChanges =
          documentsAndRanges
          |> Seq.map (fun (namedText, symbols) ->
            let edits =
              let newName =
                p.NewName
                |> FSharp.Compiler.Syntax.PrettyNaming.AddBackticksToIdentifierIfNeeded

              symbols
              |> Seq.map (fun sym ->
                let range = fcsRangeToLsp sym
                { Range = range; NewText = newName })
              |> Array.ofSeq

            { TextDocument =
                { Uri = Path.FilePathToUri(UMX.untag namedText.FileName)
                  Version = commands.TryGetFileVersion namedText.FileName }
              Edits = edits })
          |> Array.ofSeq

        return WorkspaceEdit.Create(documentChanges, clientCapabilities.Value) |> Some
      })

  override x.TextDocumentDefinition(p) =
    logger.info (
      Log.setMessage "TextDocumentDefinition Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )

    p
    |> x.positionHandler (fun p pos tyRes lineStr lines ->
      async {
        let! res = commands.FindDeclaration tyRes pos lineStr

        let res =
          match res with
          | CoreResponse.InfoRes msg
          | CoreResponse.ErrorRes msg -> LspResult.internalError msg
          | CoreResponse.Res r -> findDeclToLspLocation r |> GotoResult.Single |> Some |> success

        return res
      })

  override x.TextDocumentTypeDefinition(p: TextDocumentPositionParams) =
    logger.info (
      Log.setMessage "TextDocumentTypeDefinition Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )

    p
    |> x.positionHandler (fun p pos tyRes lineStr lines ->
      async {
        let! res = commands.FindTypeDeclaration tyRes pos lineStr

        let res =
          match res with
          | CoreResponse.InfoRes msg
          | CoreResponse.ErrorRes msg -> LspResult.internalError msg
          | CoreResponse.Res r -> findDeclToLspLocation r |> GotoResult.Single |> Some |> success

        return res
      })

  override x.TextDocumentReferences(p: ReferenceParams) =
    logger.info (
      Log.setMessage "TextDocumentReferences Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )

    p
    |> x.positionHandler (fun p pos tyRes lineStr lines ->
      asyncResult {
        let! usages =
          commands.SymbolUseWorkspace(pos, lineStr, lines, tyRes)
          |> AsyncResult.mapError (JsonRpc.Error.InternalErrorMessage)

        match usages with
        | Choice1Of2 (decls, usages) ->
          return
            Seq.append decls.Values usages.Values
            |> Seq.collect (fun kvp -> kvp |> Array.map fcsRangeToLspLocation)
            |> Seq.toArray
            |> Some
        | Choice2Of2 combinedRanges ->
          return
            combinedRanges.Values
            |> Seq.collect (fun kvp -> kvp |> Array.map fcsRangeToLspLocation)
            |> Seq.toArray
            |> Some
      })

  override x.TextDocumentDocumentHighlight(p) =
    logger.info (
      Log.setMessage "TextDocumentDocumentHighlight Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )

    p
    |> x.positionHandler (fun p pos tyRes lineStr lines ->
      match commands.SymbolUse tyRes pos lineStr with
      | CoreResponse.InfoRes msg
      | CoreResponse.ErrorRes msg -> async.Return(LspResult.internalError msg)
      | CoreResponse.Res (symbol, uses) ->
        uses
        |> Array.map (fun s ->
          { DocumentHighlight.Range = fcsRangeToLsp s.Range
            Kind = None })
        |> Some
        |> success
        |> async.Return)


  override x.TextDocumentImplementation(p) =
    logger.info (
      Log.setMessage "TextDocumentImplementation Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )

    p
    |> x.positionHandler (fun p pos tyRes lineStr lines ->
      asyncResult {
        let! res =
          commands.SymbolImplementationProject tyRes pos lineStr
          |> AsyncResult.ofCoreResponse

        let ranges: FSharp.Compiler.Text.Range[] =
          match res with
          | LocationResponse.Use (_, uses) -> uses |> Array.map (fun u -> u.Range)

        let mappedRanges = ranges |> Array.map fcsRangeToLspLocation

        match mappedRanges with
        | [||] -> return None
        | [| single |] -> return Some(GotoResult.Single single)
        | multiple -> return Some(GotoResult.Multiple multiple)
      })

  override __.TextDocumentDocumentSymbol(p) =
    async {
      logger.info (
        Log.setMessage "TextDocumentDocumentSymbol Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      let fn = p.TextDocument.GetFilePath() |> Utils.normalizePath

      let! res = commands.Declarations fn None (commands.TryGetFileVersion fn)

      let res =
        match res with
        | CoreResponse.InfoRes msg
        | CoreResponse.ErrorRes msg -> LspResult.internalError msg
        | CoreResponse.Res decls ->
          decls
          |> Array.collect (
            fst
            >> fun top -> getSymbolInformations p.TextDocument.Uri glyphToSymbolKind top (fun s -> true)
          )
          |> U2.First
          |> Some
          |> success

      return res
    }

  override __.WorkspaceSymbol(symbolRequest: WorkspaceSymbolParams) =
    async {
      logger.info (
        Log.setMessage "WorkspaceSymbol Request: {parms}"
        >> Log.addContextDestructured "parms" symbolRequest
      )

      let! res = commands.DeclarationsInProjects()

      let res =
        match res with
        | CoreResponse.InfoRes msg
        | CoreResponse.ErrorRes msg -> LspResult.internalError msg
        | CoreResponse.Res (decls) ->
          decls
          |> Array.collect (fun (n, p) ->
            let uri = Path.LocalPathToUri p
            getSymbolInformations uri glyphToSymbolKind n (applyQuery symbolRequest.Query))
          |> Some
          |> success

      return res
    }

  member __.HandleFormatting
    (
      fileName: string<LocalPath>,
      action: unit -> Async<Result<FormatDocumentResponse, string>>,
      handlerFormattedDoc: (NamedText * string) -> TextEdit[],
      handleFormattedRange: (NamedText * string * FormatSelectionRange) -> TextEdit[]
    ) =
    async {
      let! res = action ()

      match res with
      | Ok (FormatDocumentResponse.Formatted (lines, formatted)) ->
        let result = handlerFormattedDoc (lines, formatted)

        return LspResult.success (Some(result))
      | Ok (FormatDocumentResponse.FormattedRange (lines, formatted, range)) ->
        let result = handleFormattedRange (lines, formatted, range)

        return LspResult.success (Some(result))
      | Ok FormatDocumentResponse.Ignored ->
        let fileName = UMX.untag fileName |> Path.GetFileName

        do!
          lspClient.WindowShowMessage
            { Type = MessageType.Info
              Message = (sprintf "\"%s\" is ignored by a .fantomasignore file." fileName) }

        return LspResult.success None
      | Ok FormatDocumentResponse.UnChanged -> return LspResult.success None
      | Ok FormatDocumentResponse.ToolNotPresent ->
        let actions =
          [| if Option.isSome rootPath then
               { Title = "Install locally" }
             { Title = "Install globally" } |]

        let! response =
          lspClient.WindowShowMessageRequest
            { Type = MessageType.Warning
              Message = "No Fantomas install was found."
              Actions = Some actions }

        match response with
        | Ok (Some { Title = "Install locally" }) ->
          do!
            rootPath
            |> Option.map (fun rootPath ->
              async {
                let dotConfig = Path.Combine(rootPath, ".config", "dotnet-tools.json")

                if not (File.Exists dotConfig) then
                  let! result =
                    Cli.Wrap("dotnet").WithArguments("new tool-manifest").WithWorkingDirectory(
                      rootPath
                    )
                      .ExecuteBufferedAsync()
                      .Task
                    |> Async.AwaitTask

                  if result.ExitCode <> 0 then
                    fantomasLogger.warn (Log.setMessage (sprintf "Unable to create a new tool manifest in %s" rootPath))
                else
                  let dotConfigContent = File.ReadAllText dotConfig

                  if dotConfigContent.Contains("fantomas-tool") then
                    // uninstall a older, non-compatible version of fantomas-tool
                    let! result =
                      Cli.Wrap("dotnet").WithArguments(
                        "tool uninstall fantomas-tool"
                      )
                        .WithWorkingDirectory(
                        rootPath
                      )
                        .ExecuteBufferedAsync()
                        .Task
                      |> Async.AwaitTask

                    if result.ExitCode <> 0 then
                      fantomasLogger.warn (
                        Log.setMessage (
                          sprintf "Unable to uninstall a non compatible version of fantomas-tool in %s" rootPath
                        )
                      )

                let! result =
                  Cli.Wrap("dotnet").WithArguments(
                    "tool install fantomas-tool"
                  )
                    .WithWorkingDirectory(
                    rootPath
                  )
                    .ExecuteBufferedAsync()
                    .Task
                  |> Async.AwaitTask

                if result.ExitCode = 0 then
                  fantomasLogger.info (Log.setMessage (sprintf "fantomas was installed locally at %A" rootPath))

                  do!
                    lspClient.WindowShowMessage
                      { Type = MessageType.Info
                        Message = "fantomas-tool was installed locally" }

                  commands.ClearFantomasCache()
                else
                  fantomasLogger.warn (
                    Log.setMessage (sprintf "Unable to install a compatible version of fantomas-tool in %s" rootPath)
                  )
              }

            )
            |> Option.defaultValue (async { return () })
        | Ok (Some { Title = "Install globally" }) ->
          let! result =
            Cli.Wrap("dotnet").WithArguments(
              "tool install -g fantomas-tool"
            )
              .ExecuteBufferedAsync()
              .Task
            |> Async.AwaitTask

          if result.ExitCode = 0 then
            fantomasLogger.info (Log.setMessage "fantomas was installed globally")

            do!
              lspClient.WindowShowMessage
                { Type = MessageType.Info
                  Message = "fantomas-tool was installed globally" }

            commands.ClearFantomasCache()
          else
            fantomasLogger.warn (Log.setMessage "Unable to install a compatible version of fantomas-tool globally")
        | _ -> ()

        return LspResult.internalError "Fantomas install not found."
      | Ok (FormatDocumentResponse.Error ex)
      | Error ex -> return LspResult.internalError ex
    }

  override x.TextDocumentFormatting(p: DocumentFormattingParams) =
    let doc = p.TextDocument
    let fileName = doc.GetFilePath() |> Utils.normalizePath

    let action () =
      logger.info (
        Log.setMessage "TextDocumentFormatting Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      commands.FormatDocument fileName

    let handlerFormattedDoc (lines: NamedText, formatted: string) =
      let range =
        let zero = { Line = 0; Character = 0 }
        let lastPos = lines.LastFilePosition

        { Start = zero
          End = fcsPosToLsp lastPos }

      [| { Range = range; NewText = formatted } |]

    x.HandleFormatting(fileName, action, handlerFormattedDoc, (fun (_, _, _) -> [||]))

  override x.TextDocumentRangeFormatting(p: DocumentRangeFormattingParams) =
    let doc = p.TextDocument
    let fileName = doc.GetFilePath() |> Utils.normalizePath

    let action () =
      logger.info (
        Log.setMessage "TextDocumentRangeFormatting Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      let range =
        FormatSelectionRange(
          p.Range.Start.Line + 1,
          p.Range.Start.Character,
          p.Range.End.Line + 1,
          p.Range.End.Character
        )

      commands.FormatSelection(fileName, range)

    let handlerFormattedRangeDoc (lines: NamedText, formatted: string, range: FormatSelectionRange) =
      let range =
        { Start =
            { Line = range.StartLine - 1
              Character = range.StartColumn }
          End =
            { Line = range.EndLine - 1
              Character = range.EndColumn } }

      [| { Range = range; NewText = formatted } |]


    x.HandleFormatting(fileName, action, (fun (_, _) -> [||]), handlerFormattedRangeDoc)




  member private x.HandleTypeCheckCodeAction file pos f =
    async {
      return!
        match commands.TryGetFileCheckerOptionsWithLinesAndLineStr(file, pos) with
        | ResultOrString.Error s -> async.Return []
        | ResultOrString.Ok (options, lines, lineStr) ->
          try
            async {
              let! tyResOpt = commands.TryGetRecentTypeCheckResultsForFile(file)

              match tyResOpt with
              | None -> return []
              | Some tyRes ->
                let! r = Async.Catch(f tyRes lineStr lines)

                match r with
                | Choice1Of2 r -> return (List.singleton r)
                | Choice2Of2 e -> return []

            }
          with e ->
            async.Return []
    }

  override x.TextDocumentCodeAction(codeActionParams: CodeActionParams) =
    logger.info (
      Log.setMessage "TextDocumentCodeAction Request: {parms}"
      >> Log.addContextDestructured "parms" codeActionParams
    )

    let fn = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath

    match commands.TryGetFileCheckerOptionsWithLines fn with
    | ResultOrString.Error s -> AsyncLspResult.internalError s
    | ResultOrString.Ok (opts, lines) ->
      asyncResult {
        let (fixes: Async<Result<Fix list, string>[]>) =
          codeFixes
          |> Array.map (fun codeFix -> codeFix codeActionParams)
          |> Async.Parallel

        let! fixes = fixes
        let (actions: Fix list[], errors: string[]) = Array.partitionResults fixes
        let actions = actions |> List.concat

        if errors.Length <> 0 then
          logger.trace (
            Log.setMessage "Errors while processing code action request for {file} with {context} at {range}: {errors}"
            >> Log.addContextDestructured "file" codeActionParams.TextDocument.Uri
            >> Log.addContextDestructured "context" codeActionParams.Context
            >> Log.addContextDestructured "range" codeActionParams.Range
            >> Log.addContextDestructured "errors" errors
          )

        match actions with
        | [] -> return None
        | actions ->
          return
            actions
            |> List.map (
              CodeAction.OfFix commands.TryGetFileVersion clientCapabilities.Value
              >> U2.Second
            )
            |> List.toArray
            |> Some
      }

  override __.TextDocumentCodeLens(p: CodeLensParams) =
    asyncResult {
      logger.info (
        Log.setMessage "TextDocumentCodeLens Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      let fn = p.TextDocument.GetFilePath() |> Utils.normalizePath

      let! decls =
        commands.Declarations fn None (commands.TryGetFileVersion fn)
        |> AsyncResult.ofCoreResponse
        |> AsyncResult.map (Array.map fst)

      let res =
        [| if config.LineLens.Enabled <> "replaceCodeLens" then
             if config.CodeLenses.Signature.Enabled then
               yield! decls |> Array.collect (getCodeLensInformation p.TextDocument.Uri "signature")

             // we have two options here because we're deprecating the EnableReferenceCodeLens one (namespacing, etc)
             if config.EnableReferenceCodeLens || config.CodeLenses.References.Enabled then
               yield! decls |> Array.collect (getCodeLensInformation p.TextDocument.Uri "reference") |]

      return Some res
    }

  override __.CodeLensResolve(p) =
    logger.info (
      Log.setMessage "CodeLensResolve Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )

    let handler f (arg: CodeLens) =
      async {
        let pos = protocolPosToPos arg.Range.Start

        let data = arg.Data.Value.ToObject<string[]>()

        let file = Path.FileUriToLocalPath data.[0] |> Utils.normalizePath

        match commands.TryGetFileCheckerOptionsWithLinesAndLineStr(file, pos) with
        | ResultOrString.Error s ->
          logger.error (
            Log.setMessage "CodeLensResolve - Getting file checker options failed for {file}"
            >> Log.addContextDestructured "file" file
            >> Log.addContextDestructured "error" s
          )

          return { p with Command = None } |> success
        | ResultOrString.Ok (options, lines, lineStr) ->
          try
            let! tyRes = commands.TryGetRecentTypeCheckResultsForFile(file)

            match tyRes with
            | None -> return success { p with Command = None }
            | Some tyRes ->

              logger.info (
                Log.setMessage "CodeLensResolve - Cached typecheck results now available for {file}."
                >> Log.addContextDestructured "file" file
              )

              let typ = data.[1]
              let! r = Async.Catch(f arg pos tyRes lines lineStr typ file)

              match r with
              | Choice1Of2 (r: LspResult<CodeLens option>) ->
                match r with
                | Ok (Some r) -> return Ok r
                | _ -> return Ok Unchecked.defaultof<_>
              | Choice2Of2 e ->
                logger.error (
                  Log.setMessage "CodeLensResolve - Child operation failed for {file}"
                  >> Log.addContextDestructured "file" file
                  >> Log.addExn e
                )

                let title = if typ = "signature" then "" else "0 References"

                let codeLens =
                  { p with
                      Command =
                        Some
                          { Title = title
                            Command = ""
                            Arguments = None } }

                return success codeLens
          with e ->
            logger.error (
              Log.setMessage "CodeLensResolve - Operation failed on {file}"
              >> Log.addContextDestructured "file" file
              >> Log.addExn e
            )

            return { p with Command = None } |> success
      }

    let writePayload (sourceFile: string<LocalPath>, triggerPos: pos, usageLocations: range[]) =
      Some
        [| JToken.FromObject(Path.LocalPathToUri sourceFile)
           JToken.FromObject(fcsPosToLsp triggerPos)
           JToken.FromObject(usageLocations |> Array.map fcsRangeToLspLocation) |]

    handler
      (fun p pos tyRes lines lineStr typ file ->
        async {
          if typ = "signature" then
            match commands.SignatureData tyRes pos lineStr with
            | CoreResponse.InfoRes msg
            | CoreResponse.ErrorRes msg ->
              logger.error (
                Log.setMessage "CodeLensResolve - error on file {file}"
                >> Log.addContextDestructured "file" file
                >> Log.addContextDestructured "error" msg
              )

              return { p with Command = None } |> Some |> success
            | CoreResponse.Res (typ, parms, _) ->
              let formatted = SigantureData.formatSignature typ parms

              let cmd =
                { Title = formatted
                  Command = ""
                  Arguments = None }

              return { p with Command = Some cmd } |> Some |> success
          else
            let! res = commands.SymbolUseWorkspace(pos, lineStr, lines, tyRes)

            let res =
              match res with
              | Core.Result.Error msg ->
                logger.error (
                  Log.setMessage "CodeLensResolve - error getting symbol use for {file}"
                  >> Log.addContextDestructured "file" file
                  >> Log.addContextDestructured "error" msg
                )

                success (
                  Some
                    { p with
                        Command =
                          Some
                            { Title = ""
                              Command = ""
                              Arguments = None } }
                )
              | Ok res ->
                match res with
                | Choice1Of2 (_, uses) ->
                  let allUses = uses.Values |> Array.concat

                  let cmd =
                    if allUses.Length = 0 then
                      { Title = "0 References"
                        Command = ""
                        Arguments = None }
                    else
                      { Title = $"%d{allUses.Length} References"
                        Command = "fsharp.showReferences"
                        Arguments = writePayload (file, pos, allUses) }

                  { p with Command = Some cmd } |> Some |> success
                | Choice2Of2 mixedUsages ->
                  // mixedUsages will contain the declaration, so we need to do a bit of work here
                  let allUses = mixedUsages.Values |> Array.concat

                  let cmd =
                    if allUses.Length <= 1 then
                      // 1 reference means that it's only the declaration, so it's actually 0 references
                      { Title = "0 References"
                        Command = ""
                        Arguments = None }
                    else
                      // multiple references means that the declaration _and_ the references are all present.
                      // this is kind of a pain, so for now at least, we just return all of them
                      { Title = $"%d{allUses.Length - 1} References"
                        Command = "fsharp.showReferences"
                        Arguments = writePayload (file, pos, allUses) }

                  { p with Command = Some cmd } |> Some |> success

            return res
        })
      p

  override __.WorkspaceDidChangeWatchedFiles(p) =
    async {
      logger.info (
        Log.setMessage "WorkspaceDidChangeWatchedFiles Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      p.Changes
      |> Array.iter (fun c ->
        if c.Type = FileChangeType.Deleted then
          forgetDocument c.Uri

        ())

      return ()
    }

  override __.WorkspaceDidChangeConfiguration(p: DidChangeConfigurationParams) =
    async {
      let dto = p.Settings |> Server.deserialize<FSharpConfigRequest>

      logger.info (
        Log.setMessage "WorkspaceDidChangeConfiguration Request: {parms}"
        >> Log.addContextDestructured "parms" dto
      )

      let c = config.AddDto dto.FSharp
      updateConfig c

      logger.info (
        Log.setMessage "Workspace configuration changed"
        >> Log.addContextDestructured "config" c
      )

      return ()
    }

  override __.TextDocumentFoldingRange(rangeP: FoldingRangeParams) =
    async {
      logger.info (
        Log.setMessage "TextDocumentFoldingRange Request: {parms}"
        >> Log.addContextDestructured "parms" rangeP
      )

      let file = rangeP.TextDocument.GetFilePath() |> Utils.normalizePath

      match! commands.ScopesForFile file with
      | Ok scopes ->
        let ranges = scopes |> Seq.map Structure.toFoldingRange |> Set.ofSeq |> List.ofSeq

        return LspResult.success (Some ranges)
      | Result.Error error -> return LspResult.internalError error
    }

  override __.TextDocumentSelectionRange(selectionRangeP: SelectionRangeParams) =
    async {
      logger.info (
        Log.setMessage "TextDocumentSelectionRange Request: {parms}"
        >> Log.addContextDestructured "parms" selectionRangeP
      )

      let rec mkSelectionRanges =
        function
        | [] -> None
        | r :: xs ->
          Some
            { Range = fcsRangeToLsp r
              Parent = mkSelectionRanges xs }

      let file = selectionRangeP.TextDocument.GetFilePath() |> Utils.normalizePath

      let poss = selectionRangeP.Positions |> Array.map protocolPosToPos |> Array.toList

      let! res = commands.GetRangesAtPosition file poss

      match res with
      | CoreResponse.InfoRes msg
      | CoreResponse.ErrorRes msg -> return internalError msg
      | CoreResponse.Res ranges ->
        let response = ranges |> List.choose mkSelectionRanges
        // logger.info (Log.setMessage "TextDocumentSelectionRange Response: {parms}" >> Log.addContextDestructured "parms" response)
        return success (Some response)
    }

  member x.FSharpSignature(p: TextDocumentPositionParams) =
    logger.info (
      Log.setMessage "FSharpSignature Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )

    async {
      return!
        p
        |> x.positionHandler (fun p pos tyRes lineStr lines ->
          async {
            match commands.Typesig tyRes pos lineStr with
            | CoreResponse.InfoRes msg
            | CoreResponse.ErrorRes msg -> return LspResult.internalError msg
            | CoreResponse.Res tip ->
              return
                { Content = CommandResponse.typeSig FsAutoComplete.JsonSerializer.writeJson tip }
                |> success
          })
    }

  member x.FSharpSignatureData(p: TextDocumentPositionParams) =
    logger.info (
      Log.setMessage "FSharpSignatureData Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )

    let handler f (arg: TextDocumentPositionParams) =
      async {
        let pos = FcsPos.mkPos (p.Position.Line) (p.Position.Character + 2)

        let file = p.TextDocument.GetFilePath() |> Utils.normalizePath

        logger.info (
          Log.setMessage "FSharpSignatureData - Position request for {file} at {pos}"
          >> Log.addContextDestructured "file" file
          >> Log.addContextDestructured "pos" pos
        )

        return!
          match commands.TryGetFileCheckerOptionsWithLinesAndLineStr(file, pos) with
          | ResultOrString.Error s -> AsyncLspResult.internalError "No options"
          | ResultOrString.Ok (options, _, lineStr) ->
            try
              async {
                let tyResOpt = commands.TryGetRecentTypeCheckResultsForFile(file)

                match! tyResOpt with
                | None -> return LspResult.internalError "No typecheck results"
                | Some tyRes ->
                  let! r = Async.Catch(f arg pos tyRes lineStr)

                  match r with
                  | Choice1Of2 r -> return r
                  | Choice2Of2 e -> return LspResult.internalError e.Message
              }
            with e ->
              AsyncLspResult.internalError e.Message
      }

    p
    |> handler (fun p pos tyRes lineStr ->
      (match commands.SignatureData tyRes pos lineStr with
       | CoreResponse.InfoRes msg
       | CoreResponse.ErrorRes msg -> LspResult.internalError msg
       | CoreResponse.Res (typ, parms, generics) ->
         { Content = CommandResponse.signatureData FsAutoComplete.JsonSerializer.writeJson (typ, parms, generics) }
         |> success)
      |> async.Return)

  member x.FSharpDocumentationGenerator(p: OptionallyVersionedTextDocumentPositionParams) =
    logger.info (
      Log.setMessage "FSharpDocumentationGenerator Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )

    p
    |> x.positionHandler (fun p pos tyRes lineStr lines ->
      asyncResult {
        let! { InsertPosition = insertPos
               InsertText = text } =
          commands.GenerateXmlDocumentation(tyRes, pos, lineStr)
          |> AsyncResult.ofStringErr

        let edit: ApplyWorkspaceEditParams =
          { Label = Some "Generate Xml Documentation"
            Edit =
              { DocumentChanges =
                  Some
                    [| { TextDocument = p.TextDocument
                         Edits =
                           [| { Range = fcsPosToProtocolRange insertPos
                                NewText = text } |] } |]
                Changes = None } }

        let! response = lspClient.WorkspaceApplyEdit edit
        return ()
      })

  member __.FSharpLineLense(p) =
    async {
      logger.info (
        Log.setMessage "FSharpLineLense Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      let fn = p.Project.GetFilePath() |> Utils.normalizePath

      let! res = commands.Declarations fn None (commands.TryGetFileVersion fn)

      let res =
        match res with
        | CoreResponse.InfoRes msg
        | CoreResponse.ErrorRes msg -> LspResult.internalError msg
        | CoreResponse.Res (decls) ->
          { Content = CommandResponse.declarations FsAutoComplete.JsonSerializer.writeJson decls }
          |> success

      return res
    }

  member x.LineLensResolve(p) =
    logger.info (
      Log.setMessage "LineLensResolve Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )

    p
    |> x.positionHandler (fun p pos tyRes lineStr lines ->
      (match commands.SignatureData tyRes pos lineStr with
       | CoreResponse.InfoRes msg
       | CoreResponse.ErrorRes msg -> LspResult.internalError msg
       | CoreResponse.Res (typ, parms, generics) ->
         { Content = CommandResponse.signatureData FsAutoComplete.JsonSerializer.writeJson (typ, parms, generics) }
         |> success)
      |> async.Return)

  member __.FSharpCompilerLocation(p) =
    async {
      logger.info (
        Log.setMessage "FSharpCompilerLocation Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      let res = commands.CompilerLocation()

      let res =
        match res with
        | CoreResponse.InfoRes msg
        | CoreResponse.ErrorRes msg -> LspResult.internalError msg
        | CoreResponse.Res (fsc, fsi, msbuild, sdk) ->
          { Content =
              CommandResponse.compilerLocation
                FsAutoComplete.JsonSerializer.writeJson
                fsc
                fsi
                msbuild
                (sdk |> Option.map (fun (di: DirectoryInfo) -> di.FullName)) }
          |> success

      return res
    }

  member __.FSharpWorkspaceLoad(p: WorkspaceLoadParms) =
    async {
      logger.info (
        Log.setMessage "FSharpWorkspaceLoad Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      let fns = p.TextDocuments |> Array.map (fun fn -> fn.GetFilePath()) |> Array.toList

      let! res = commands.WorkspaceLoad fns config.DisableInMemoryProjectReferences binaryLogConfig

      let res =
        match res with
        | CoreResponse.InfoRes msg
        | CoreResponse.ErrorRes msg -> LspResult.internalError msg
        | CoreResponse.Res fin ->
          { Content = CommandResponse.workspaceLoad FsAutoComplete.JsonSerializer.writeJson fin }
          |> success

      return res
    }

  member __.FSharpWorkspacePeek(p: WorkspacePeekRequest) =
    async {
      logger.info (
        Log.setMessage "FSharpWorkspacePeek Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      let! res = commands.WorkspacePeek p.Directory p.Deep (p.ExcludedDirs |> List.ofArray)

      let res =
        match res with
        | CoreResponse.InfoRes msg
        | CoreResponse.ErrorRes msg -> LspResult.internalError msg
        | CoreResponse.Res found ->
          { Content = CommandResponse.workspacePeek FsAutoComplete.JsonSerializer.writeJson found }
          |> success

      return res
    }

  member __.FSharpProject(p) =
    async {
      logger.info (
        Log.setMessage "FSharpProject Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      let fn = p.Project.GetFilePath()
      let! res = commands.Project fn binaryLogConfig

      let res =
        match res with
        | CoreResponse.InfoRes msg
        | CoreResponse.ErrorRes msg -> LspResult.internalError msg
        | CoreResponse.Res fin ->
          { Content = CommandResponse.projectLoad FsAutoComplete.JsonSerializer.writeJson fin }
          |> success

      return res
    }

  member __.FSharpFsdn(p: FsdnRequest) =
    async {
      logger.info (
        Log.setMessage "FSharpFsdn Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      let! res = commands.Fsdn p.Query

      let res =
        match res with
        | CoreResponse.InfoRes msg
        | CoreResponse.ErrorRes msg -> LspResult.internalError msg
        | CoreResponse.Res (funcs) ->
          { Content = CommandResponse.fsdn FsAutoComplete.JsonSerializer.writeJson funcs }
          |> success

      return res
    }

  member __.FSharpDotnetNewList(p: DotnetNewListRequest) =
    async {
      logger.info (
        Log.setMessage "FSharpDotnetNewList Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      let! res = commands.DotnetNewList()

      let res =
        match res with
        | CoreResponse.InfoRes msg
        | CoreResponse.ErrorRes msg -> LspResult.internalError msg
        | CoreResponse.Res (funcs) ->
          { Content = CommandResponse.dotnetnewlist FsAutoComplete.JsonSerializer.writeJson funcs }
          |> success

      return res
    }

  member __.FSharpDotnetNewRun(p: DotnetNewRunRequest) =
    async {
      logger.info (
        Log.setMessage "FSharpDotnetNewRun Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      let! res = commands.DotnetNewRun p.Template p.Name p.Output []

      let res =
        match res with
        | CoreResponse.InfoRes msg
        | CoreResponse.ErrorRes msg -> LspResult.internalError msg
        | CoreResponse.Res (_) -> { Content = "" } |> success

      return res
    }

  member __.FSharpDotnetAddProject(p: DotnetProjectRequest) =
    async {
      logger.info (
        Log.setMessage "FSharpDotnetAddProject Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      let! res = commands.DotnetAddProject p.Target p.Reference

      let res =
        match res with
        | CoreResponse.InfoRes msg
        | CoreResponse.ErrorRes msg -> LspResult.internalError msg
        | CoreResponse.Res (_) -> { Content = "" } |> success

      return res
    }

  member __.FSharpDotnetRemoveProject(p: DotnetProjectRequest) =
    async {
      logger.info (
        Log.setMessage "FSharpDotnetRemoveProject Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      let! res = commands.DotnetRemoveProject p.Target p.Reference

      let res =
        match res with
        | CoreResponse.InfoRes msg
        | CoreResponse.ErrorRes msg -> LspResult.internalError msg
        | CoreResponse.Res (_) -> { Content = "" } |> success

      return res
    }

  member __.FSharpDotnetSlnAdd(p: DotnetProjectRequest) =
    async {
      logger.info (
        Log.setMessage "FSharpDotnetSlnAdd Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      let! res = commands.DotnetSlnAdd p.Target p.Reference

      let res =
        match res with
        | CoreResponse.InfoRes msg
        | CoreResponse.ErrorRes msg -> LspResult.internalError msg
        | CoreResponse.Res (_) -> { Content = "" } |> success

      return res
    }

  member __.FsProjMoveFileUp(p: DotnetFileRequest) =
    async {
      logger.info (
        Log.setMessage "FsProjMoveFileUp Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      let! res = commands.FsProjMoveFileUp p.FsProj p.FileVirtualPath

      let res =
        match res with
        | CoreResponse.InfoRes msg
        | CoreResponse.ErrorRes msg -> LspResult.internalError msg
        | CoreResponse.Res (_) -> { Content = "" } |> success

      return res
    }

  member __.FsProjMoveFileDown(p: DotnetFileRequest) =
    async {
      logger.info (
        Log.setMessage "FsProjMoveFileDown Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      let! res = commands.FsProjMoveFileDown p.FsProj p.FileVirtualPath

      let res =
        match res with
        | CoreResponse.InfoRes msg
        | CoreResponse.ErrorRes msg -> LspResult.internalError msg
        | CoreResponse.Res (_) -> { Content = "" } |> success

      return res
    }

  member __.FsProjAddFileAbove(p: DotnetFile2Request) =
    async {
      logger.info (
        Log.setMessage "FsProjAddFileAbove Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      let! res = commands.FsProjAddFileAbove p.FsProj p.FileVirtualPath p.NewFile

      let res =
        match res with
        | CoreResponse.InfoRes msg
        | CoreResponse.ErrorRes msg -> LspResult.internalError msg
        | CoreResponse.Res (_) -> { Content = "" } |> success

      return res
    }

  member __.FsProjAddFileBelow(p: DotnetFile2Request) =
    async {
      logger.info (
        Log.setMessage "FsProjAddFileBelow Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      let! res = commands.FsProjAddFileBelow p.FsProj p.FileVirtualPath p.NewFile

      let res =
        match res with
        | CoreResponse.InfoRes msg
        | CoreResponse.ErrorRes msg -> LspResult.internalError msg
        | CoreResponse.Res (_) -> { Content = "" } |> success

      return res
    }

  member __.FsProjAddFile(p: DotnetFileRequest) =
    async {
      logger.info (
        Log.setMessage "FsProjAddFile Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      let! res = commands.FsProjAddFile p.FsProj p.FileVirtualPath

      let res =
        match res with
        | CoreResponse.InfoRes msg
        | CoreResponse.ErrorRes msg -> LspResult.internalError msg
        | CoreResponse.Res (_) -> { Content = "" } |> success

      return res
    }

  member _.FsProjAddExistingFile(p: DotnetFileRequest) =
    async {
      logger.info (
        Log.setMessage "FsProjAddExistingFile Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      let! res = commands.FsProjAddExistingFile p.FsProj p.FileVirtualPath

      let res =
        match res with
        | CoreResponse.InfoRes msg
        | CoreResponse.ErrorRes msg -> LspResult.internalError msg
        | CoreResponse.Res (_) -> { Content = "" } |> success

      return res
    }

  member _.FsProjRemoveFile(p: DotnetFileRequest) =
    async {
      logger.info (
        Log.setMessage "FsProjRemoveFile Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      let! res = commands.FsProjRemoveFile p.FsProj p.FileVirtualPath

      let res =
        match res with
        | CoreResponse.InfoRes msg
        | CoreResponse.ErrorRes msg -> LspResult.internalError msg
        | CoreResponse.Res (_) -> { Content = "" } |> success

      return res
    }

  member x.FSharpHelp(p: TextDocumentPositionParams) =
    logger.info (
      Log.setMessage "FSharpHelp Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )

    p
    |> x.positionHandler (fun p pos tyRes lineStr lines ->
      (match commands.Help tyRes pos lineStr with
       | CoreResponse.InfoRes msg
       | CoreResponse.ErrorRes msg -> LspResult.internalError msg
       | CoreResponse.Res (t) ->
         { Content = CommandResponse.help FsAutoComplete.JsonSerializer.writeJson t }
         |> success)
      |> async.Return)


  member x.FSharpDocumentation(p: TextDocumentPositionParams) =
    logger.info (
      Log.setMessage "FSharpDocumentation Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )

    p
    |> x.positionHandler (fun p pos tyRes lineStr lines ->
      (match commands.FormattedDocumentation tyRes pos lineStr with
       | CoreResponse.InfoRes msg
       | CoreResponse.ErrorRes msg -> LspResult.internalError msg
       | CoreResponse.Res (tip, xml, signature, footer, cm) ->
         let notification: PlainNotification =
           { Content =
               CommandResponse.formattedDocumentation
                 FsAutoComplete.JsonSerializer.writeJson
                 (tip, xml, signature, footer, cm) }

         success notification)
      |> async.Return)

  member x.FSharpDocumentationSymbol(p: DocumentationForSymbolReuqest) =
    logger.info (
      Log.setMessage "FSharpDocumentationSymbol Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )

    match commands.LastCheckResult with
    | None -> AsyncLspResult.internalError "error"
    | Some tyRes ->
      match commands.FormattedDocumentationForSymbol tyRes p.XmlSig p.Assembly with
      | Ok (CoreResponse.InfoRes msg)
      | Ok (CoreResponse.ErrorRes msg) -> AsyncLspResult.internalError msg
      | Ok (CoreResponse.Res (xml, assembly, doc, signature, footer, cn)) ->
        let xmldoc =
          match doc with
          | FSharpXmlDoc.None -> [||]
          | FSharpXmlDoc.FromXmlFile _ -> [||]
          | FSharpXmlDoc.FromXmlText d -> d.GetElaboratedXmlLines()

        { Content =
            CommandResponse.formattedDocumentationForSymbol
              FsAutoComplete.JsonSerializer.writeJson
              xml
              assembly
              xmldoc
              (signature, footer, cn) }
        |> success
        |> async.Return
      | Error e -> AsyncLspResult.internalError e

  member __.LoadAnalyzers(path) =
    async {
      logger.info (
        Log.setMessage "LoadAnalyzers Request: {parms}"
        >> Log.addContextDestructured "parms" path
      )

      try
        // since the analyzer state handling code is in `updateConfig`, re-trigger it here
        updateConfig config
        return LspResult.success ()
      with ex ->
        Loggers.analyzers.error (Log.setMessage "Loading failed" >> Log.addExn ex)
        return LspResult.success ()
    }


  member private x.handleSemanticTokens
    (getTokens: Async<CoreResponse<SemanticClassificationItem array> option>)
    : AsyncLspResult<SemanticTokens option> =
    asyncResult {
      match! getTokens with
      | None -> return! success None
      | Some rangesAndHighlights ->
        let! rangesAndHighlights = rangesAndHighlights |> Result.ofCoreResponse


        let lspTypedRanges =
          rangesAndHighlights
          |> Array.map (fun item ->
            let ty, mods = ClassificationUtils.map item.Type
            struct (fcsRangeToLsp item.Range, ty, mods))

        match encodeSemanticHighlightRanges lspTypedRanges with
        | None -> return! success None
        | Some encoded -> return! success (Some { Data = encoded; ResultId = None }) // TODO: provide a resultId when we support delta ranges
    }

  override x.TextDocumentSemanticTokensFull(p: SemanticTokensParams) : AsyncLspResult<SemanticTokens option> =
    logger.info (
      Log.setMessage "Semantic highlighing request: {parms}"
      >> Log.addContextDestructured "parms" p
    )

    let fn = p.TextDocument.GetFilePath() |> Utils.normalizePath

    x.handleSemanticTokens (commands.GetHighlighting(fn, None))

  override x.TextDocumentSemanticTokensRange(p: SemanticTokensRangeParams) : AsyncLspResult<SemanticTokens option> =
    logger.info (
      Log.setMessage "Semantic highlighing range request: {parms}"
      >> Log.addContextDestructured "parms" p
    )

    let fn = p.TextDocument.GetFilePath() |> Utils.normalizePath

    let fcsRange = protocolRangeToRange (UMX.untag fn) p.Range

    x.handleSemanticTokens (commands.GetHighlighting(fn, Some fcsRange))

  member __.ScriptFileProjectOptions = commands.ScriptFileProjectOptions

  // member __.FSharpLiterate (p: FSharpLiterateRequest) = async {
  //   logger.info (Log.setMessage "FSharpLiterate Request: {parms}" >> Log.addContextDestructured "parms" p )

  //   let fn = p.TextDocument.GetFilePath() |> Utils.normalizePath
  //   let! res = commands.FSharpLiterate fn
  //   let res =
  //     match res with
  //     | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
  //         LspResult.internalError msg
  //     | CoreResponse.Res (res) ->
  //         { Content = CommandResponse.fsharpLiterate FsAutoComplete.JsonSerializer.writeJson res }
  //         |> success

  //   return res
  // }

  override x.TextDocumentInlayHint(p: InlayHintParams) : AsyncLspResult<InlayHint[] option> =
    logger.info (
      Log.setMessage "TextDocumentInlayHint Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )

    p.TextDocument
    |> x.fileHandler (fun fn tyRes lines ->
      async {
        let fcsRange = protocolRangeToRange (UMX.untag fn) p.Range

        let! hints =
          commands.InlayHints(
            lines,
            tyRes,
            fcsRange,
            showTypeHints = config.InlayHints.typeAnnotations,
            showParameterHints = config.InlayHints.parameterNames
          )

        let getTooltip (h: InlayHints.Hint) =
          let hideTypeAnnotations = "fsharp.inlayHints.hideTypeAnnotations"
          let hideParameterNames = "fsharp.inlayHints.hideParameterNames"
          let hideAll = "fsharp.inlayHints.hideAll"
          let setToToggle = "fsharp.inlayHints.setToToggle"
          let disableLongTooltip = "fsharp.inlayHints.disableLongTooltip"

          if config.InlayHints.disableLongTooltip then
            h.Tooltip |> Option.map (InlayHintTooltip.String)
          else
            let lines = ResizeArray()

            let hideCommand =
              match h.Kind with
              | InlayHints.HintKind.Type -> hideTypeAnnotations
              | InlayHints.HintKind.Parameter -> hideParameterNames

            lines.Add $"To hide these hints, [click here](command:{hideCommand})."
            lines.Add $"To hide *ALL* hints, [click here](command:{hideAll})."

            // We don't have access to generic VSCode config so we don't know what inlay hints mode is used
            // if not isSetToToggle && toggleSupported then
            lines.Add
              $"Hints can also be hidden by default, and shown when Ctrl/Cmd+Alt is pressed. To do this, [click here](command:{setToToggle})."

            lines.Add $"Finally, to dismiss this long tooltip forever, [click here](command:{disableLongTooltip})."

            h.Tooltip
            |> Option.iter (fun t ->
              lines.Add ""
              lines.Add(t))

            String.concat "\n" lines |> markdown |> InlayHintTooltip.Markup |> Some

        let hints: InlayHint[] =
          hints
          |> Array.map (fun h ->
            { Position = fcsPosToLsp h.Pos
              Label = InlayHintLabel.String h.Text
              Kind =
                match h.Kind with
                | InlayHints.HintKind.Type -> Types.InlayHintKind.Type
                | InlayHints.HintKind.Parameter -> Types.InlayHintKind.Parameter
                |> Some
              TextEdits =
                match h.Insertions with
                | None -> None
                | Some insertions ->
                  insertions
                  |> Array.map (fun insertion ->
                    { Range = fcsPosToProtocolRange insertion.Pos
                      NewText = insertion.Text })
                  |> Some
              Tooltip = getTooltip h
              PaddingLeft =
                match h.Kind with
                | InlayHints.HintKind.Type -> Some true
                | _ -> None
              PaddingRight =
                match h.Kind with
                | InlayHints.HintKind.Parameter -> Some true
                | _ -> None
              Data = None })

        return success (Some hints)
      })

  member x.FSharpPipelineHints(p: FSharpPipelineHintRequest) =
    logger.info (
      Log.setMessage "FSharpPipelineHints Request: {parms}"
      >> Log.addContextDestructured "parms" p
    )

    p.TextDocument
    |> x.fileHandler (fun fn tyRes lines ->
      match commands.PipelineHints tyRes with
      | CoreResponse.InfoRes msg
      | CoreResponse.ErrorRes msg -> AsyncLspResult.internalError msg
      | CoreResponse.Res (res) ->
        { Content = CommandResponse.pipelineHint FsAutoComplete.JsonSerializer.writeJson res }
        |> success
        |> async.Return)

  override x.Dispose() = x.Shutdown() |> Async.Start

let startCore toolsPath stateStorageDir workspaceLoaderFactory =
  use input = Console.OpenStandardInput()
  use output = Console.OpenStandardOutput()

  let requestsHandlings =
    // (defaultRequestHandlings (): Map<string, ServerRequestHandling<FSharpLspServer>>)
    (defaultRequestHandlings (): Map<string, ServerRequestHandling<AdaptiveFSharpLspServer>>)
    |> Map.add "fsharp/signature" (serverRequestHandling (fun s p -> s.FSharpSignature(p)))
    |> Map.add "fsharp/signatureData" (serverRequestHandling (fun s p -> s.FSharpSignatureData(p)))
    |> Map.add "fsharp/documentationGenerator" (serverRequestHandling (fun s p -> s.FSharpDocumentationGenerator(p)))
    |> Map.add "fsharp/lineLens" (serverRequestHandling (fun s p -> s.FSharpLineLense(p)))
    |> Map.add "fsharp/compilerLocation" (serverRequestHandling (fun s p -> s.FSharpCompilerLocation(p)))
    |> Map.add "fsharp/workspaceLoad" (serverRequestHandling (fun s p -> s.FSharpWorkspaceLoad(p)))
    |> Map.add "fsharp/workspacePeek" (serverRequestHandling (fun s p -> s.FSharpWorkspacePeek(p)))
    |> Map.add "fsharp/project" (serverRequestHandling (fun s p -> s.FSharpProject(p)))
    |> Map.add "fsharp/fsdn" (serverRequestHandling (fun s p -> s.FSharpFsdn(p)))
    |> Map.add "fsharp/dotnetnewlist" (serverRequestHandling (fun s p -> s.FSharpDotnetNewList(p)))
    |> Map.add "fsharp/dotnetnewrun" (serverRequestHandling (fun s p -> s.FSharpDotnetNewRun(p)))
    |> Map.add "fsharp/dotnetaddproject" (serverRequestHandling (fun s p -> s.FSharpDotnetAddProject(p)))
    |> Map.add "fsharp/dotnetremoveproject" (serverRequestHandling (fun s p -> s.FSharpDotnetRemoveProject(p)))
    |> Map.add "fsharp/dotnetaddsln" (serverRequestHandling (fun s p -> s.FSharpDotnetSlnAdd(p)))
    |> Map.add "fsharp/f1Help" (serverRequestHandling (fun s p -> s.FSharpHelp(p)))
    |> Map.add "fsharp/documentation" (serverRequestHandling (fun s p -> s.FSharpDocumentation(p)))
    |> Map.add "fsharp/documentationSymbol" (serverRequestHandling (fun s p -> s.FSharpDocumentationSymbol(p)))
    |> Map.add "fsharp/loadAnalyzers" (serverRequestHandling (fun s p -> s.LoadAnalyzers(p)))
    // // |> Map.add "fsharp/fsharpLiterate" (serverRequestHandling (fun s p -> s.FSharpLiterate(p) ))
    |> Map.add "fsharp/pipelineHint" (serverRequestHandling (fun s p -> s.FSharpPipelineHints(p)))
    |> Map.add "fsproj/moveFileUp" (serverRequestHandling (fun s p -> s.FsProjMoveFileUp(p)))
    |> Map.add "fsproj/moveFileDown" (serverRequestHandling (fun s p -> s.FsProjMoveFileDown(p)))
    |> Map.add "fsproj/addFileAbove" (serverRequestHandling (fun s p -> s.FsProjAddFileAbove(p)))
    |> Map.add "fsproj/addFileBelow" (serverRequestHandling (fun s p -> s.FsProjAddFileBelow(p)))
    |> Map.add "fsproj/addFile" (serverRequestHandling (fun s p -> s.FsProjAddFile(p)))
    |> Map.add "fsproj/addExistingFile" (serverRequestHandling (fun s p -> s.FsProjAddExistingFile(p)))
    |> Map.add "fsproj/removeFile" (serverRequestHandling (fun s p -> s.FsProjRemoveFile(p)))

  let regularServer lspClient =
    let state = State.Initial toolsPath stateStorageDir workspaceLoaderFactory
    let originalFs = FSharp.Compiler.IO.FileSystemAutoOpens.FileSystem
    FSharp.Compiler.IO.FileSystemAutoOpens.FileSystem <- FsAutoComplete.FileSystem(originalFs, state.Files.TryFind)
    new FSharpLspServer(state, lspClient)
  let adaptiveServer lspClient =
    let loader = workspaceLoaderFactory toolsPath
    new AdaptiveFSharpLspServer(loader, lspClient)

  Ionide.LanguageServerProtocol.Server.start requestsHandlings input output FSharpLspClient (fun lspClient ->
    adaptiveServer lspClient
    // regularServer lspClient
    )

let start toolsPath stateStorageDir workspaceLoaderFactory =
  let logger = LogProvider.getLoggerByName "Startup"
  try
    let result = startCore toolsPath stateStorageDir workspaceLoaderFactory

    logger.info (
      Log.setMessage "Start - Ending LSP mode with {reason}"
      >> Log.addContextDestructured "reason" result
    )

    int result
  with ex ->
    logger.error (Log.setMessage "Start - LSP mode crashed" >> Log.addExn ex)

    3
