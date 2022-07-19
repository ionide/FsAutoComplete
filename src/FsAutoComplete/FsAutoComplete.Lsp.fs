module FsAutoComplete.Lsp

open System
open System.IO
open System.Threading
open System.Diagnostics
open FsAutoComplete
open FsAutoComplete.Core
open FsAutoComplete.LspHelpers
open FsAutoComplete.Utils
open FsAutoComplete.CodeFix
open FsAutoComplete.CodeFix.Types
open FsAutoComplete.Logging
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types.LspResult
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open LspHelpers
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
open FSharp.UMX
open StreamJsonRpc
open Fantomas.Client.Contracts

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

  member x.ClearFor(fileUri: DocumentUri) = removeAgent fileUri

  member x.ClearFor(fileUri: DocumentUri, kind: string) =
    let mailbox = getOrAddAgent fileUri
    mailbox.Post(Clear kind)

  interface IDisposable with
    member x.Dispose() =
      for (_, cts) in agents.Values do
        cts.Cancel()

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

  let checkFile (filePath: string<LocalPath>, version: int, content: NamedText, isFirstOpen: bool) =
    asyncResult {
      let tfmConfig =
        config.UseSdkScripts
        |> function
          | true -> FSIRefs.TFM.NetCore
          | false -> FSIRefs.TFM.NetFx

      do! commands.CheckFileAndAllDependentFilesInAllProjects(filePath, version, content, tfmConfig, isFirstOpen)

      analyzeFile (filePath, version) |> Async.Start
    }

  let checkChangedFile (p: DidChangeTextDocumentParams) =

    async {
      let doc = p.TextDocument
      let filePath = doc.GetFilePath() |> Utils.normalizePath
      let contentChange = p.ContentChanges |> Seq.tryLast

      match contentChange, doc.Version with
      | Some contentChange, Some version ->
        if contentChange.Range.IsNone && contentChange.RangeLength.IsNone then
          let content = NamedText(filePath, contentChange.Text)

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

        else
          logger.warn (Log.setMessage "ParseFile - Parse not started, received partial change")
      | _ ->
        logger.info (
          Log.setMessage "ParseFile - Found no change for {file}"
          >> Log.addContextDestructured "file" filePath
        )
    }
    |> Async.Start

  let checkFileDebouncer = Debounce(250, checkChangedFile)

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

        ({ Content = UMX.untag uri }: PlainNotification)
        |> lspClient.NotifyFileParsed
        |> Async.Start
      | NotificationEvent.Workspace ws ->
        logger.info (Log.setMessage "Workspace Notify {ws}" >> Log.addContextDestructured "ws" ws)

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
          |> Array.map (fun (n, t) ->
            { Range = fcsRangeToLsp n
              Code = (if t then Some "FSAC0003" else None)
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

  /// centralize any state changes when the config is updated here
  let updateConfig (newConfig: FSharpConfig) =
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

  override __.Initialize(p: InitializeParams) =
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
           Run.ifEnabled (fun _ -> config.UnusedDeclarationsAnalyzer) (RenameUnusedValue.fix getRangeText)
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
                  RenameProvider = Some true
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

  override __.TextDocumentDidChange(p) =
    async {

      let doc = p.TextDocument
      let filePath = doc.GetFilePath() |> Utils.normalizePath
      let contentChange = p.ContentChanges |> Seq.tryLast

      logger.info (
        Log.setMessage "TextDocumentDidChange Request: {parms}"
        >> Log.addContextDestructured "parms" filePath
      )

      match contentChange, doc.Version with
      | Some contentChange, Some version ->
        if contentChange.Range.IsNone && contentChange.RangeLength.IsNone then
          let content = NamedText(filePath, contentChange.Text)
          commands.SetFileContent(filePath, content, Some version)
        else
          ()
      | _ -> ()

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
    async {
      logger.info (
        Log.setMessage "CompletionItemResolve Request: {parms}"
        >> Log.addContextDestructured "parms" ci
      )

      let! res = commands.Helptext ci.InsertText.Value

      let res =
        match res with
        | CoreResponse.InfoRes msg
        | CoreResponse.ErrorRes msg -> ci
        | CoreResponse.Res (HelpText.Simple (name, str)) ->
          let d = Documentation.Markup(markdown str)

          { ci with
              Detail = Some name
              Documentation = Some d }
        | CoreResponse.Res (HelpText.Full (name, tip, additionalEdit)) ->
          let (si, comment) = (TipFormatter.formatTip tip) |> List.collect id |> List.head

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
          |> AsyncResult.mapError (JsonRpc.Error.InternalErrorMessage)

        let documentChanges =
          documentsAndRanges
          |> Seq.map (fun (namedText, symbols) ->
            let edits =
              symbols
              |> Seq.map (fun sym ->
                let range = fcsRangeToLsp sym
                { Range = range; NewText = p.NewName })
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

  member __.HandleFormatting(fileName : string<LocalPath>, action: unit -> Async<Result<FormatDocumentResponse, string>>, handlerFormattedDoc: (NamedText * string) -> TextEdit [], handleFormattedRange: (NamedText * string * FormatSelectionRange) -> TextEdit []) =
    async {
      let! res = action()

      match res with
      | Ok (FormatDocumentResponse.Formatted (lines, formatted)) ->
        let result = handlerFormattedDoc (lines, formatted)

        return LspResult.success (Some(result))
      | Ok(FormatDocumentResponse.FormattedRange(lines, formatted, range)) ->
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

    x.HandleFormatting(fileName, action, handlerFormattedDoc, fun (_, _, _) -> [| |])

  override x.TextDocumentRangeFormatting(p: DocumentRangeFormattingParams) =
    let doc = p.TextDocument
    let fileName = doc.GetFilePath() |> Utils.normalizePath
    let action () =
      logger.info (
        Log.setMessage "TextDocumentRangeFormatting Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      let range = FormatSelectionRange(p.Range.Start.Line + 1, p.Range.Start.Character, p.Range.End.Line + 1, p.Range.End.Character)

      commands.FormatSelection (fileName, range)

    let handlerFormattedRangeDoc (lines: NamedText, formatted: string, range : FormatSelectionRange) =
      let range = {
        Start = {
          Line = range.StartLine - 1
          Character = range.StartColumn
        }
        End = {
          Line = range.EndLine - 1
          Character = range.EndColumn
        }
      }

      [| { Range = range; NewText = formatted } |]


    x.HandleFormatting(fileName, action, (fun (_, _) -> [| |]), handlerFormattedRangeDoc )




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
        let pos = FcsPos.mkPos (arg.Range.Start.Line + 1) (arg.Range.Start.Character + 2)

        let data = arg.Data.Value.ToObject<string[]>()

        let file = Path.FileUriToLocalPath data.[0] |> Utils.normalizePath

        return!
          match commands.TryGetFileCheckerOptionsWithLinesAndLineStr(file, pos) with
          | ResultOrString.Error s ->
            logger.error (
              Log.setMessage "CodeLensResolve - Getting file checker options failed for {file}"
              >> Log.addContextDestructured "file" file
              >> Log.addContextDestructured "error" s
            )

            { p with Command = None } |> success |> async.Return
          | ResultOrString.Ok (options, lines, lineStr) ->
            try
              async {
                let! tyRes = commands.TryGetRecentTypeCheckResultsForFile(file)

                match tyRes with
                | None -> return { p with Command = None } |> success
                | Some tyRes ->

                  logger.info (
                    Log.setMessage "CodeLensResolve - Cached typecheck results now available for {file}."
                    >> Log.addContextDestructured "file" file
                  )

                  let! r = Async.Catch(f arg pos tyRes lines lineStr data.[1] file)

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

                    return { p with Command = None } |> success
              }
            with e ->
              logger.error (
                Log.setMessage "CodeLensResolve - Operation failed on {file}"
                >> Log.addContextDestructured "file" file
                >> Log.addExn e
              )

              { p with Command = None } |> success |> async.Return
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
          let uri = c.Uri
          diagnosticCollections.ClearFor uri

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
              Tooltip = h.Tooltip |> Option.map (InlayHintTooltip.String)
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
    (defaultRequestHandlings (): Map<string, ServerRequestHandling<FSharpLspServer>>)
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
    // |> Map.add "fsharp/fsharpLiterate" (serverRequestHandling (fun s p -> s.FSharpLiterate(p) ))
    |> Map.add "fsharp/pipelineHint" (serverRequestHandling (fun s p -> s.FSharpPipelineHints(p)))
    |> Map.add "fsproj/moveFileUp" (serverRequestHandling (fun s p -> s.FsProjMoveFileUp(p)))
    |> Map.add "fsproj/moveFileDown" (serverRequestHandling (fun s p -> s.FsProjMoveFileDown(p)))
    |> Map.add "fsproj/addFileAbove" (serverRequestHandling (fun s p -> s.FsProjAddFileAbove(p)))
    |> Map.add "fsproj/addFileBelow" (serverRequestHandling (fun s p -> s.FsProjAddFileBelow(p)))
    |> Map.add "fsproj/addFile" (serverRequestHandling (fun s p -> s.FsProjAddFile(p)))

  let state = State.Initial toolsPath stateStorageDir workspaceLoaderFactory

  let originalFs = FSharp.Compiler.IO.FileSystemAutoOpens.FileSystem

  FSharp.Compiler.IO.FileSystemAutoOpens.FileSystem <- FsAutoComplete.FileSystem(originalFs, state.Files.TryFind)

  Ionide.LanguageServerProtocol.Server.start requestsHandlings input output FSharpLspClient (fun lspClient ->
    new FSharpLspServer(state, lspClient))

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
