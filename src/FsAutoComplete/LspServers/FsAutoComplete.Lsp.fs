namespace FsAutoComplete.Lsp

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
open FSharp.Control.Reactive.Observable
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
open Fantomas.Client.LSPFantomasService
open FSharp.Compiler.Text.Position
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

open FsAutoComplete.Lsp
open Ionide.LanguageServerProtocol.Types.AsyncLspResult
open Ionide.LanguageServerProtocol.Types.LspResult
open StreamJsonRpc
open FsAutoComplete.FCSPatches


type FSharpLspServer(state: State, lspClient: FSharpLspClient, sourceTextFactory: ISourceTextFactory) =


  let logger = LogProvider.getLoggerByName "LSP"
  let fantomasLogger = LogProvider.getLoggerByName "Fantomas"

  let mutable rootPath: string option = None

  let mutable commands =
    new Commands(FSharpCompilerServiceChecker(false, 200L, false), state, false, rootPath, sourceTextFactory)

  let mutable commandDisposables = ResizeArray()
  let mutable clientCapabilities: ClientCapabilities option = None
  let mutable glyphToCompletionKind = glyphToCompletionKindGenerator None
  let mutable glyphToSymbolKind = glyphToSymbolKindGenerator None
  let mutable config = FSharpConfig.Default
  let mutable codefixes: CodeFix[] = [||]
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
      do! analyzers |> Async.parallel75 |> Async.Ignore<unit[]>

      do!
        lspClient.NotifyDocumentAnalyzed
          { TextDocument =
              { Uri = filePath |> Path.LocalPathToUri
                Version = version } }
    }

  /// UTC Time of start of last `checkFile` call
  /// -> `lastCheckFile - DateTime.UtcNow` is duration between two `checkFile` calls,
  ///    but doesn't include execution time of previous `checkFile`!
  ///
  /// `UtcNow` instead if `Utc`: faster, and we're only interested in elapsed duration
  ///
  /// `DateTime` instead of `Stopwatch`: stopwatch doesn't work with multiple simultaneous consumers
  let mutable lastCheckFile = DateTime.UtcNow

  let checkFile (filePath: string<LocalPath>, version: int, content: IFSACSourceText, isFirstOpen: bool) =
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
      let version = doc.Version

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

    //TODO: it would be _amazing_ to flow version through to here
    { Uri = uri
      Diagnostics = diags
      Version = None }
    |> lspClient.TextDocumentPublishDiagnostics

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
        logger.info (Log.setMessage "Workspace Notify {ws}" >> Log.addContextDestructured "ws" ws)

        let ws =
          match ws with
          | ProjectResponse.Project(x, _) -> CommandResponse.project JsonSerializer.writeJson x
          | ProjectResponse.ProjectError(_, errorDetails) ->
            CommandResponse.projectError JsonSerializer.writeJson errorDetails
          | ProjectResponse.ProjectLoading(projectFileName) ->
            CommandResponse.projectLoading JsonSerializer.writeJson projectFileName
          | ProjectResponse.WorkspaceLoad(finished) -> CommandResponse.workspaceLoad JsonSerializer.writeJson finished
          | ProjectResponse.ProjectChanged(projectFileName) -> failwith "Not Implemented"

        ({ Content = ws }: PlainNotification)
        |> lspClient.NotifyWorkspace
        |> Async.Start

      | NotificationEvent.ParseError(errors, file) ->
        let uri = Path.LocalPathToUri file
        let diags = errors |> Array.map fcsErrorToDiagnostic
        diagnosticCollections.SetFor(uri, "F# Compiler", diags)

      | NotificationEvent.UnusedOpens(file, opens) ->
        let uri = Path.LocalPathToUri file

        let diags =
          opens
          |> Array.map (fun n ->
            { Range = fcsRangeToLsp n
              Code = Some "FSAC0001"
              Severity = Some DiagnosticSeverity.Hint
              Source = Some "FSAC"
              Message = "Unused open statement"
              RelatedInformation = None
              Tags = Some [| DiagnosticTag.Unnecessary |]
              Data = None
              CodeDescription = None })

        diagnosticCollections.SetFor(uri, "F# Unused opens", diags)

      | NotificationEvent.UnusedDeclarations(file, decls) ->
        let uri = Path.LocalPathToUri file

        let diags =
          decls
          |> Array.map (fun n ->
            { Range = fcsRangeToLsp n
              Code = Some "FSAC0003"
              Severity = Some DiagnosticSeverity.Hint
              Source = Some "FSAC"
              Message = "This value is unused"
              RelatedInformation = Some [||]
              Tags = Some [| DiagnosticTag.Unnecessary |]
              Data = None
              CodeDescription = None })

        diagnosticCollections.SetFor(uri, "F# Unused declarations", diags)

      | NotificationEvent.SimplifyNames(file, decls) ->
        let uri = Path.LocalPathToUri file

        let diags =
          decls
          |> Array.map



            (fun
                 ({ Range = range
                    RelativeName = _relName }) ->
              { Diagnostic.Range = fcsRangeToLsp range
                Code = Some "FSAC0002"
                Severity = Some DiagnosticSeverity.Hint
                Source = Some "FSAC"
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

      | NotificationEvent.Canceled(msg) ->
        let ntf: PlainNotification = { Content = msg }

        lspClient.NotifyCancelledRequest ntf |> Async.Start
      | NotificationEvent.AnalyzerMessage(messages, file) ->
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
                Source = Some $"F# Analyzers (%s{m.Type})"
                Message = m.Message
                RelatedInformation = None
                Tags = None
                CodeDescription = None
                Data = fixes })

          diagnosticCollections.SetFor(uri, "F# Analyzers", diags)
      | NotificationEvent.TestDetected(file, tests) ->
        let rec map
          (r: TestAdapter.TestAdapterEntry<FSharp.Compiler.Text.range>)
          : TestAdapter.TestAdapterEntry<Ionide.LanguageServerProtocol.Types.Range> =
          { Id = r.Id
            List = r.List
            Name = r.Name
            Type = r.Type
            ModuleType = r.ModuleType
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
        new Commands(
          FSharpCompilerServiceChecker(
            hasAnalyzersNow,
            config.Fsac.CachedTypeCheckCount,
            config.Fsac.ParallelReferenceResolution
          ),
          state,
          hasAnalyzersNow,
          rootPath,
          sourceTextFactory
        )

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



  member private x.HandleTypeCheckCodeAction file pos f =
    async {
      return!
        match commands.TryGetFileCheckerOptionsWithLinesAndLineStr(file, pos) with
        | ResultOrString.Error s -> async.Return []
        | ResultOrString.Ok(options, lines, lineStr) ->
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


  ///Helper function for handling Position requests using **recent** type check results
  member x.positionHandler<'a, 'b when 'b :> ITextDocumentPositionParams>
    (f: 'b -> FcsPos -> ParseAndCheckResults -> string -> IFSACSourceText -> AsyncLspResult<'a>)
    (arg: 'b)
    : AsyncLspResult<'a> =
    async {
      let pos = arg.GetFcsPos()
      let file = arg.GetFilePath() |> Utils.normalizePath

      match commands.TryGetFileCheckerOptionsWithLinesAndLineStr(file, pos) with
      | ResultOrString.Error s ->
        logger.error (
          Log.setMessage "PositionHandler - Getting file checker options for {file} failed"
          >> Log.addContextDestructured "error" s
          >> Log.addContextDestructured "file" file
        )

        return! AsyncLspResult.internalError s
      | ResultOrString.Ok(options, lines, lineStr) ->
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
    (f: string<LocalPath> -> ParseAndCheckResults -> IFSACSourceText -> AsyncLspResult<'a>)
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
      | ResultOrString.Ok(options, lines) ->
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

  member __.HandleFormatting
    (
      fileName: string<LocalPath>,
      action: unit -> Async<Result<FormatDocumentResponse, string>>,
      handlerFormattedDoc: (IFSACSourceText * string) -> TextEdit[],
      handleFormattedRange: (IFSACSourceText * string * FormatSelectionRange) -> TextEdit[]
    ) =
    async {
      let! res = action ()

      match res with
      | Ok(FormatDocumentResponse.Formatted(lines, formatted)) ->
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
        | Ok(Some { Title = "Install locally" }) ->
          do!
            rootPath
            |> Option.map (fun rootPath ->
              async {
                let dotConfig = Path.Combine(rootPath, ".config", "dotnet-tools.json")

                if not (File.Exists dotConfig) then
                  let! result =
                    Cli
                      .Wrap("dotnet")
                      .WithArguments("new tool-manifest")
                      .WithWorkingDirectory(rootPath)
                      .ExecuteBufferedAsync()
                      .Task
                    |> Async.AwaitTask

                  if result.ExitCode <> 0 then
                    fantomasLogger.warn (
                      Log.setMessage (sprintf "Unable to create a new tool manifest in %s" rootPath)
                    )
                else
                  let dotConfigContent = File.ReadAllText dotConfig

                  if dotConfigContent.Contains("fantomas") then
                    // uninstall a older, non-compatible version of fantomas
                    let! result =
                      Cli
                        .Wrap("dotnet")
                        .WithArguments("tool uninstall fantomas")
                        .WithWorkingDirectory(rootPath)
                        .ExecuteBufferedAsync()
                        .Task
                      |> Async.AwaitTask

                    if result.ExitCode <> 0 then
                      fantomasLogger.warn (
                        Log.setMessage (
                          sprintf "Unable to uninstall a non compatible version of fantomas in %s" rootPath
                        )
                      )

                let! result =
                  Cli
                    .Wrap("dotnet")
                    .WithArguments("tool install fantomas")
                    .WithWorkingDirectory(rootPath)
                    .ExecuteBufferedAsync()
                    .Task
                  |> Async.AwaitTask

                if result.ExitCode = 0 then
                  fantomasLogger.info (Log.setMessage (sprintf "fantomas was installed locally at %A" rootPath))

                  do!
                    lspClient.WindowShowMessage
                      { Type = MessageType.Info
                        Message = "fantomas was installed locally" }

                  commands.ClearFantomasCache()
                else
                  fantomasLogger.warn (
                    Log.setMessage (sprintf "Unable to install a compatible version of fantomas in %s" rootPath)
                  )
              }

            )
            |> Option.defaultValue (async { return () })
        | Ok(Some { Title = "Install globally" }) ->
          let! result =
            Cli
              .Wrap("dotnet")
              .WithArguments("tool install -g fantomas")
              .ExecuteBufferedAsync()
              .Task
            |> Async.AwaitTask

          if result.ExitCode = 0 then
            fantomasLogger.info (Log.setMessage "fantomas was installed globally")

            do!
              lspClient.WindowShowMessage
                { Type = MessageType.Info
                  Message = "fantomas was installed globally" }

            commands.ClearFantomasCache()
          else
            fantomasLogger.warn (Log.setMessage "Unable to install a compatible version of fantomas globally")
        | _ -> ()

        return LspResult.internalError "Fantomas install not found."
      | Ok(FormatDocumentResponse.Error ex)
      | Error ex -> return LspResult.internalError ex
    }

  member private x.handleSemanticTokens
    (getTokens: Async<CoreResponse<SemanticClassificationItem array> option>)
    : AsyncLspResult<SemanticTokens option> =
    asyncResult {
      match! getTokens with
      | None -> return! success None
      | Some rangesAndHighlights ->
        let! rangesAndHighlights = rangesAndHighlights |> Result.ofCoreResponse

        match rangesAndHighlights with
        | None -> return! success None
        | Some rangesAndHighlights ->

          let lspTypedRanges =
            rangesAndHighlights
            |> Array.map (fun item ->
              let ty, mods = ClassificationUtils.map item.Type
              struct (fcsRangeToLsp item.Range, ty, mods))

          match encodeSemanticHighlightRanges lspTypedRanges with
          | None -> return! success None
          | Some encoded -> return! success (Some { Data = encoded; ResultId = None }) // TODO: provide a resultId when we support delta ranges
    }

  member private x.logUnimplementedRequest<'t, 'u>
    (
      argValue: 't,
      [<CallerMemberName; Optional; DefaultParameterValue("")>] caller: string
    ) =
    logger.info (
      Log.setMessage $"{caller} request: {{parms}}"
      >> Log.addContextDestructured "parms" argValue
    )

    Helpers.notImplemented<'u>

  member private x.logIgnoredNotification<'t>
    (
      argValue: 't,
      [<CallerMemberName; Optional; DefaultParameterValue("")>] caller: string
    ) =
    logger.info (
      Log.setMessage $"{caller} request: {{parms}}"
      >> Log.addContextDestructured "parms" argValue
    )

    Helpers.ignoreNotification

  member __.ScriptFileProjectOptions = commands.ScriptFileProjectOptions

  interface IFSharpLspServer with


    //unsupported -- begin

    override x.CodeActionResolve(p) = x.logUnimplementedRequest (p)

    override x.DocumentLinkResolve(p) = x.logUnimplementedRequest (p)

    override x.Exit() = x.logIgnoredNotification (())

    override x.InlayHintResolve p = x.logUnimplementedRequest (p)

    override x.TextDocumentDocumentColor p = x.logUnimplementedRequest (p)

    override x.TextDocumentColorPresentation p = x.logUnimplementedRequest (p)

    override x.TextDocumentDocumentLink p = x.logUnimplementedRequest (p)

    override x.TextDocumentOnTypeFormatting p = x.logUnimplementedRequest (p)

    override x.TextDocumentSemanticTokensFullDelta p = x.logUnimplementedRequest (p)

    override x.TextDocumentWillSave p = x.logIgnoredNotification (p)

    override x.TextDocumentWillSaveWaitUntil p = x.logUnimplementedRequest (p)

    override x.WorkspaceDidChangeWorkspaceFolders p = x.logIgnoredNotification (p)

    override x.WorkspaceDidCreateFiles p = x.logIgnoredNotification (p)

    override x.WorkspaceDidDeleteFiles p = x.logIgnoredNotification (p)

    override x.WorkspaceDidRenameFiles p = x.logIgnoredNotification (p)

    override x.WorkspaceExecuteCommand p = x.logUnimplementedRequest (p)

    override x.WorkspaceWillCreateFiles p = x.logUnimplementedRequest (p)

    override x.WorkspaceWillDeleteFiles p = x.logUnimplementedRequest (p)

    override x.WorkspaceWillRenameFiles p = x.logUnimplementedRequest (p)

    override x.TextDocumentDeclaration p = x.logUnimplementedRequest (p)
    override x.TextDocumentDiagnostic p = x.logUnimplementedRequest (p)
    override x.TextDocumentLinkedEditingRange p = x.logUnimplementedRequest (p)
    override x.TextDocumentMoniker p = x.logUnimplementedRequest (p)
    override x.WorkspaceDiagnostic p = x.logUnimplementedRequest (p)
    override x.WorkspaceSymbolResolve p = x.logUnimplementedRequest (p)

    //unsupported -- end

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

        logger.info (
          Log.setMessage "Intialization options {items}"
          >> Log.addContextDestructured "items" c
        )

        clientCapabilities <- p.Capabilities
        lspClient.ClientCapabilities <- clientCapabilities

        diagnosticCollections.ClientSupportsDiagnostics <-
          match p.Capabilities with
          | Some { TextDocument = Some { PublishDiagnostics = Some _ } } -> true
          | _ -> false

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

        let tryGetLanguageVersion (fileName: string<LocalPath>) =
          async {
            let result =
              let langVersion =
                state.LanguageVersions |> Seq.tryFind (fun (k, _) -> UMX.untag fileName = (k))

              match langVersion with
              | Some(_, v) -> v
              | None -> LanguageVersionShim.defaultLanguageVersion.Value

            return result
          }

        let inlineValueToggle: InlineValueOptions option =
          match c.InlineValues.Enabled with
          | Some true -> Some { ResolveProvider = Some false }
          | Some false -> None
          | None -> None

        let getFileLines =
          commands.TryGetFileCheckerOptionsWithLines >> Result.map snd >> Async.singleton

        let getLineText (lines: IFSACSourceText) (range: Ionide.LanguageServerProtocol.Types.Range) =
          lines.GetText(protocolRangeToRange (UMX.untag lines.FileName) range)
          |> Async.singleton

        let getRangeText fileName (range: Ionide.LanguageServerProtocol.Types.Range) =
          asyncResult {
            let! lines = getFileLines fileName
            return! lines.GetText(protocolRangeToRange (UMX.untag fileName) range)
          }

        let getProjectOptsAndLines = commands.TryGetFileCheckerOptionsWithLinesAndLineStr

        let tryGetProjectOptions =
          commands.TryGetFileCheckerOptionsWithLines >> Result.map fst >> Async.singleton

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

        let symbolUseWorkspace
          (includeDeclarations: bool)
          (includeBackticks: bool)
          (errorOnFailureToFixRange: bool)
          pos
          lineStr
          text
          tyRes
          =
          commands.SymbolUseWorkspace(
            pos,
            lineStr,
            text,
            tyRes,
            includeDeclarations,
            includeBackticks,
            errorOnFailureToFixRange
          )

        codefixes <-
          [| Run.ifEnabled (fun _ -> config.UnusedOpensAnalyzer) (RemoveUnusedOpens.fix getFileLines)
             Run.ifEnabled
               (fun _ -> config.ResolveNamespaces)
               (ResolveNamespace.fix tryGetParseResultsForFile commands.GetNamespaceSuggestions)
             ReplaceWithSuggestion.fix
             RemoveRedundantQualifier.fix
             Run.ifEnabled
               (fun _ -> config.UnusedDeclarationsAnalyzer)
               (RenameUnusedValue.fix tryGetParseResultsForFile)
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
             AddMissingWildcardOperator.fix tryGetParseResultsForFile
             MakeOuterBindingRecursive.fix tryGetParseResultsForFile getLineText
             AddMissingRecKeyword.fix getFileLines getLineText
             ConvertBangEqualsToInequality.fix getRangeText
             ChangeDerefBangToValue.fix tryGetParseResultsForFile getLineText
             RemoveUnusedBinding.fix tryGetParseResultsForFile
             AddTypeToIndeterminateValue.fix tryGetParseResultsForFile tryGetProjectOptions
             ChangeTypeOfNameToNameOf.fix tryGetParseResultsForFile
             AddMissingInstanceMember.fix
             AddMissingXmlDocumentation.fix tryGetParseResultsForFile
             AddExplicitTypeAnnotation.fix tryGetParseResultsForFile
             ConvertPositionalDUToNamed.fix tryGetParseResultsForFile getRangeText
             ConvertTripleSlashCommentToXmlTaggedDoc.fix tryGetParseResultsForFile getRangeText
             GenerateXmlDocumentation.fix tryGetParseResultsForFile
             RemoveRedundantAttributeSuffix.fix tryGetParseResultsForFile
             Run.ifEnabled
               (fun _ -> config.AddPrivateAccessModifier)
               (AddPrivateAccessModifier.fix tryGetParseResultsForFile symbolUseWorkspace)
             UseTripleQuotedInterpolation.fix tryGetParseResultsForFile getRangeText
             RenameParamToMatchSignature.fix tryGetParseResultsForFile
             RemovePatternArgument.fix tryGetParseResultsForFile
             ToInterpolatedString.fix tryGetParseResultsForFile tryGetLanguageVersion

             |]


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
                { Helpers.defaultServerCapabilities with
                    InlineValueProvider = inlineValueToggle } }
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
        let content = sourceTextFactory.Create(filePath, doc.Text)

        logger.info (
          Log.setMessage "TextDocumentDidOpen Request: {parms}"
          >> Log.addContextDestructured "parms" filePath
        )

        commands.SetFileContent(filePath, content, doc.Version)

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
        let endVersion = doc.Version

        logger.info (
          Log.setMessage "TextDocumentDidChange Request: {parms}"
          >> Log.addContextDestructured "parms" filePath
        )

        let initialText =
          state.TryGetFileSource(filePath)
          |> Result.bimap id (fun _ -> sourceTextFactory.Create(filePath, ""))

        let evolvedFileContent =
          (initialText, p.ContentChanges)
          ||> Array.fold (fun text change ->
            match change.Range with
            | None -> // replace entire content
              sourceTextFactory.Create(filePath, change.Text)
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

                text)


        commands.SetFileContent(filePath, evolvedFileContent, endVersion)

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
        | Ok(options, lines) ->

          match lines.GetLine pos with
          | None -> return! success None
          | Some lineStr ->

            if lineStr.StartsWith "#" then
              let completionList =
                { IsIncomplete = false
                  Items = KeywordList.hashSymbolCompletionItems
                  ItemDefaults = None }

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
              | CoreResponse.Res(decls, keywords) ->
                let items =
                  decls
                  |> Array.mapi (fun id d ->
                    let code =
                      if System.Text.RegularExpressions.Regex.IsMatch(d.NameInList, """^[a-zA-Z][a-zA-Z0-9']+$""") then
                        d.NameInList
                      elif d.NamespaceToOpen.IsSome then
                        d.NameInList
                      else
                        FSharpKeywords.NormalizeIdentifierBackticks d.NameInList

                    let label =
                      match d.NamespaceToOpen with
                      | Some no -> sprintf "%s (open %s)" d.NameInList no
                      | None -> d.NameInList

                    { CompletionItem.Create(d.NameInList) with
                        Kind = glyphToCompletionKind d.Glyph
                        InsertText = Some code
                        SortText = Some(sprintf "%06d" id)
                        FilterText = Some d.NameInList
                        Label = label })

                let its =
                  if not keywords then
                    items
                  else
                    Array.append items KeywordList.keywordCompletionItems

                let completionList =
                  { IsIncomplete = false
                    Items = its
                    ItemDefaults = None }

                return! success (Some completionList)
              | _ ->
                logger.info (Log.setMessage "TextDocumentCompletion - no completion results")

                return!
                  success (
                    Some
                      { IsIncomplete = false
                        Items = [||]
                        ItemDefaults = None }
                  )
      }

    override __.CompletionItemResolve(ci: CompletionItem) =
      let mapHelpText (ci: CompletionItem) (text: HelpText) =
        match text with
        | HelpText.Simple(symbolName, text) ->
          let d = Documentation.Markup(markdown text)

          { ci with
              Detail = Some symbolName
              Documentation = Some d }
        | HelpText.Full(name, tip, additionalEdit) ->
          let (si, comment) = TipFormatter.formatCompletionItemTip tip

          let edits, label =
            match additionalEdit with
            | None -> None, ci.Label
            | Some { Namespace = ns; Position = fcsPos } ->
              let text =
                let indentation = String(' ', fcsPos.Column)
                $"{indentation}open {ns}\n"

              let insertPos =
                { (fcsPos |> fcsPosToLsp) with
                    Character = 0 }

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
          |> AsyncResult.foldResult
            (function
            | None -> ci
            | Some text -> mapHelpText ci text)
            (fun _ -> ci)

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
                    { ParameterInformation.Label = U2.First p.ParameterName
                      Documentation = Some(Documentation.String p.CanonicalTypeTextForSorting) })

                let d = Documentation.Markup(markdown comment)

                { SignatureInformation.Label = signature
                  Documentation = Some d
                  Parameters = Some parameters
                  ActiveParameter = None })

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
        | CoreResponse.InfoRes msg -> async.Return(success None)
        | CoreResponse.ErrorRes msg -> LspResult.internalError msg |> async.Return
        | CoreResponse.Res None -> async.Return(success None)
        | CoreResponse.Res(Some tooltipResult) ->
          let formatCommentStyle =
            if config.TooltipMode = "full" then
              TipFormatter.FormatCommentStyle.FullEnhanced
            else if config.TooltipMode = "summary" then
              TipFormatter.FormatCommentStyle.SummaryOnly
            else
              TipFormatter.FormatCommentStyle.Legacy

          match TipFormatter.tryFormatTipEnhanced tooltipResult.ToolTipText formatCommentStyle with
          | TipFormatter.TipFormatterResult.Success tooltipInfo ->

            // Display the signature as a code block
            let signature =
              tooltipResult.Signature
              |> TipFormatter.prepareSignature
              |> (fun content -> MarkedString.WithLanguage { Language = "fsharp"; Value = content })

            // Display each footer line as a separate line
            let footerLines =
              tooltipResult.Footer
              |> TipFormatter.prepareFooterLines
              |> Array.map MarkedString.String

            let contents =
              [| signature
                 MarkedString.String tooltipInfo.DocComment
                 match tooltipResult.SymbolInfo with
                 | TryGetToolTipEnhancedResult.Keyword _ -> ()
                 | TryGetToolTipEnhancedResult.Symbol symbolInfo ->
                   TipFormatter.renderShowDocumentationLink
                     tooltipInfo.HasTruncatedExamples
                     symbolInfo.XmlDocSig
                     symbolInfo.Assembly
                   |> MarkedString.String
                 yield! footerLines |]

            let response =
              { Contents = MarkedStrings contents
                Range = None }

            async.Return(success (Some response))

          | TipFormatter.TipFormatterResult.Error error ->
            let contents = [| MarkedString.String "<Note>"; MarkedString.String error |]

            let response =
              { Contents = MarkedStrings contents
                Range = None }

            async.Return(success (Some response))

          | TipFormatter.TipFormatterResult.None -> async.Return(success None))

    override x.TextDocumentPrepareRename p =
      logger.info (
        Log.setMessage "TextDocumentOnPrepareRename Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      p
      |> x.positionHandler (fun p pos tyRes lineStr lines ->
        asyncResult {
          let! (_, _, range) =
            commands.RenameSymbolRange(pos, tyRes, lineStr, lines)
            |> AsyncResult.mapError (fun msg -> JsonRpc.Error.Create(JsonRpc.ErrorCodes.invalidParams, msg))

          return range |> fcsRangeToLsp |> PrepareRenameResult.Range |> Some
        })

    override x.TextDocumentRename(p: RenameParams) =
      logger.info (
        Log.setMessage "TextDocumentRename Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      p
      |> x.positionHandler (fun p pos tyRes lineStr lines ->
        asyncResult {
          // validate name and surround with backticks if necessary
          let! newName =
            Commands.adjustRenameSymbolNewName pos lineStr lines tyRes p.NewName
            |> AsyncResult.mapError (fun msg -> JsonRpc.Error.Create(JsonRpc.ErrorCodes.invalidParams, msg))

          let! ranges =
            commands.RenameSymbol(pos, tyRes, lineStr, lines)
            |> AsyncResult.mapError (fun msg -> JsonRpc.Error.Create(JsonRpc.ErrorCodes.invalidParams, msg))

          let documentChanges =
            ranges
            |> Seq.map (fun kvp ->
              let edits =
                kvp.Value
                |> Array.map (fun range ->
                  let range = fcsRangeToLsp range
                  { Range = range; NewText = newName })

              let file: string<LocalPath> = kvp.Key

              { TextDocument =
                  { Uri = Path.FilePathToUri(UMX.untag file)
                    Version = commands.TryGetFileVersion file }
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
            | CoreResponse.InfoRes msg -> success None
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
            | CoreResponse.InfoRes msg -> success None
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
            commands.SymbolUseWorkspace(pos, lineStr, lines, tyRes, true, true, false)
            |> AsyncResult.mapError (JsonRpc.Error.InternalErrorMessage)

          let references =
            usages.Values |> Seq.collect (Seq.map fcsRangeToLspLocation) |> Seq.toArray

          return Some references
        })

    override x.TextDocumentDocumentHighlight(p) =
      logger.info (
        Log.setMessage "TextDocumentDocumentHighlight Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      p
      |> x.positionHandler (fun p pos tyRes lineStr lines ->
        match commands.SymbolUse tyRes pos lineStr with
        | CoreResponse.InfoRes msg -> async.Return(success None)
        | CoreResponse.ErrorRes msg -> async.Return(LspResult.internalError msg)
        | CoreResponse.Res(symbol, uses) ->
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
            | Some(LocationResponse.Use(_, uses)) -> uses |> Array.map (fun u -> u.Range)
            | None -> [||]

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
          | CoreResponse.InfoRes msg -> success None
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
          | CoreResponse.InfoRes msg -> success None
          | CoreResponse.ErrorRes msg -> LspResult.internalError msg
          | CoreResponse.Res(decls) ->
            decls
            |> Array.collect (fun (n, p) ->
              let uri = Path.LocalPathToUri p
              getSymbolInformations uri glyphToSymbolKind n (applyQuery symbolRequest.Query))
            |> U2.First
            |> Some
            |> success

        return res
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

      let handlerFormattedDoc (lines: IFSACSourceText, formatted: string) =
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

      let handlerFormattedRangeDoc (lines: IFSACSourceText, formatted: string, range: FormatSelectionRange) =
        let range =
          { Start =
              { Line = range.StartLine - 1
                Character = range.StartColumn }
            End =
              { Line = range.EndLine - 1
                Character = range.EndColumn } }

        [| { Range = range; NewText = formatted } |]


      x.HandleFormatting(fileName, action, (fun (_, _) -> [||]), handlerFormattedRangeDoc)



    override x.TextDocumentCodeAction(codeActionParams: CodeActionParams) =
      logger.info (
        Log.setMessage "TextDocumentCodeAction Request: {parms}"
        >> Log.addContextDestructured "parms" codeActionParams
      )

      let fn = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath

      match commands.TryGetFileCheckerOptionsWithLines fn with
      | ResultOrString.Error s -> AsyncLspResult.internalError s
      | ResultOrString.Ok(opts, lines) ->
        asyncResult {
          let (fixes: Async<Result<Fix list, string>[]>) =
            codefixes
            |> Array.map (fun codeFix ->
              async {
                try
                  return! codeFix codeActionParams
                with e ->
                  return Ok []
              })
            |> Async.parallel75

          let! fixes = fixes
          let (actions: Fix list[], errors: string[]) = Array.partitionResults fixes
          let actions = actions |> List.concat

          if errors.Length <> 0 then
            logger.trace (
              Log.setMessage
                "Errors while processing code action request for {file} with {context} at {range}: {errors}"
              >> Log.addContextDestructured "file" codeActionParams.TextDocument.Uri
              >> Log.addContextDestructured "context" codeActionParams.Context
              >> Log.addContextDestructured "range" codeActionParams.Range
              >> Log.addContextDestructured "errors" errors
            )

          match actions with
          | [] -> return None
          | actions ->
            let! fixes =
              actions
              |> List.map (CodeAction.OfFix (commands.TryGetFileVersion >> Async.singleton) clientCapabilities.Value)
              |> Async.parallel75

            return Some(fixes |> Array.map U2.Second)
        }

    override __.TextDocumentCodeLens(p: CodeLensParams) =
      asyncResult {
        logger.info (
          Log.setMessage "TextDocumentCodeLens Request: {parms}"
          >> Log.addContextDestructured "parms" p
        )

        let fn = p.TextDocument.GetFilePath() |> Utils.normalizePath

        let! declsAndPaths =
          commands.Declarations fn None (commands.TryGetFileVersion fn)
          |> AsyncResult.ofCoreResponse

        match declsAndPaths with
        | None -> return None
        | Some declsAndPaths ->
          let decls = Array.map fst declsAndPaths

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
          | ResultOrString.Ok(options, lines, lineStr) ->
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
                | Choice1Of2(r: LspResult<CodeLens option>) ->
                  match r with
                  | Ok(Some r) -> return Ok r
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
              match Commands.SignatureData tyRes pos lineStr with
              | CoreResponse.InfoRes msg
              | CoreResponse.ErrorRes msg ->
                logger.error (
                  Log.setMessage "CodeLensResolve - error on file {file}"
                  >> Log.addContextDestructured "file" file
                  >> Log.addContextDestructured "error" msg
                )

                return { p with Command = None } |> Some |> success
              | CoreResponse.Res(typ, parms, _) ->
                let formatted = SigantureData.formatSignature typ parms

                let cmd =
                  { Title = formatted
                    Command = ""
                    Arguments = None }

                return { p with Command = Some cmd } |> Some |> success
            else
              let! uses =
                commands.SymbolUseWorkspace(pos, lineStr, lines, tyRes (*includeDeclarations:*) , false, true, false)

              match uses with
              | Error msg ->
                logger.error (
                  Log.setMessage "CodeLensResolve - error getting symbol use for {file}"
                  >> Log.addContextDestructured "file" file
                  >> Log.addContextDestructured "error" msg
                )

                return
                  success (
                    Some
                      { p with
                          Command =
                            Some
                              { Title = ""
                                Command = ""
                                Arguments = None } }
                  )

              | Ok uses ->
                let allUses = uses.Values |> Array.concat

                let cmd =
                  if Array.isEmpty allUses then
                    { Title = "0 References"
                      Command = ""
                      Arguments = None }
                  else
                    { Title = $"%d{allUses.Length} References"
                      Command = "fsharp.showReferences"
                      Arguments = writePayload (file, pos, allUses) }

                return { p with Command = Some cmd } |> Some |> success
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

        dto.FSharp
        |> Option.iter (fun fsharpConfig ->
          let c = config.AddDto fsharpConfig
          updateConfig c

          logger.info (
            Log.setMessage "Workspace configuration changed"
            >> Log.addContextDestructured "config" c
          ))

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
      asyncResult {
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

        let! ranges = commands.GetRangesAtPosition file poss |> AsyncResult.ofStringErr

        return ranges |> List.choose mkSelectionRanges |> Some
      }

    override x.FSharpSignature(p: TextDocumentPositionParams) =
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
              | CoreResponse.InfoRes msg -> return success None
              | CoreResponse.ErrorRes msg -> return LspResult.internalError msg
              | CoreResponse.Res tip ->
                return
                  { Content = CommandResponse.typeSig FsAutoComplete.JsonSerializer.writeJson tip }
                  |> Some
                  |> success
            })
      }

    override x.FSharpSignatureData(p: TextDocumentPositionParams) =
      logger.info (
        Log.setMessage "FSharpSignatureData Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      let handler f (arg: TextDocumentPositionParams) =
        async {

          let pos =
            FSharp.Compiler.Text.Position.mkPos (p.Position.Line) (p.Position.Character + 2)

          let file = p.TextDocument.GetFilePath() |> Utils.normalizePath

          logger.info (
            Log.setMessage "FSharpSignatureData - Position request for {file} at {pos}"
            >> Log.addContextDestructured "file" file
            >> Log.addContextDestructured "pos" pos
          )

          return!
            match commands.TryGetFileCheckerOptionsWithLinesAndLineStr(file, pos) with
            | ResultOrString.Error s -> AsyncLspResult.internalError "No options"
            | ResultOrString.Ok(options, _, lineStr) ->
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
        (match Commands.SignatureData tyRes pos lineStr with
         | CoreResponse.InfoRes msg -> success None
         | CoreResponse.ErrorRes msg -> LspResult.internalError msg
         | CoreResponse.Res(typ, parms, generics) ->
           { Content = CommandResponse.signatureData FsAutoComplete.JsonSerializer.writeJson (typ, parms, generics) }
           |> Some
           |> success)
        |> async.Return)

    override x.FSharpDocumentationGenerator(p: OptionallyVersionedTextDocumentPositionParams) =
      logger.info (
        Log.setMessage "FSharpDocumentationGenerator Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      p
      |> x.positionHandler (fun p pos tyRes lineStr lines ->
        asyncResult {
          match!
            Commands.GenerateXmlDocumentation(tyRes, pos, lineStr)
            |> AsyncResult.ofStringErr
          with
          | None -> return ()
          | Some { InsertPosition = insertPos
                   InsertText = text } ->

            let edit: ApplyWorkspaceEditParams =
              { Label = Some "Generate Xml Documentation"
                Edit =
                  { DocumentChanges =
                      Some
                        [| { TextDocument =
                               { Uri = p.TextDocument.Uri
                                 Version = Some p.TextDocument.Version }
                             Edits =
                               [| { Range = fcsPosToProtocolRange insertPos
                                    NewText = text } |] } |]
                    Changes = None } }

            let! response = lspClient.WorkspaceApplyEdit edit
            return ()
        })

    override __.FSharpLineLens(p) =
      async {
        logger.info (
          Log.setMessage "FSharpLineLense Request: {parms}"
          >> Log.addContextDestructured "parms" p
        )

        let fn = p.Project.GetFilePath() |> Utils.normalizePath

        let! res = commands.Declarations fn None (commands.TryGetFileVersion fn)

        let res =
          match res with
          | CoreResponse.InfoRes msg -> success None
          | CoreResponse.ErrorRes msg -> LspResult.internalError msg
          | CoreResponse.Res(decls) ->
            { Content = CommandResponse.declarations FsAutoComplete.JsonSerializer.writeJson decls }
            |> Some
            |> success

        return res
      }

    override __.FSharpWorkspaceLoad(p: WorkspaceLoadParms) =
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

    override __.FSharpWorkspacePeek(p: WorkspacePeekRequest) =
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

    override __.FSharpProject(p) =
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

    override __.FSharpFsdn(p: FsdnRequest) =
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
          | CoreResponse.Res(funcs) ->
            { Content = CommandResponse.fsdn FsAutoComplete.JsonSerializer.writeJson funcs }
            |> success

        return res
      }

    override __.FSharpDotnetNewList(p: DotnetNewListRequest) =
      async {
        logger.info (
          Log.setMessage "FSharpDotnetNewList Request: {parms}"
          >> Log.addContextDestructured "parms" p
        )

        let! res = Commands.DotnetNewList()

        match res with
        | CoreResponse.InfoRes msg -> return success None
        | CoreResponse.ErrorRes msg -> return LspResult.internalError msg
        | CoreResponse.Res(funcs) ->
          return
            success (Some { Content = CommandResponse.dotnetnewlist FsAutoComplete.JsonSerializer.writeJson funcs })

      }

    override __.FSharpDotnetNewRun(p: DotnetNewRunRequest) =
      async {
        logger.info (
          Log.setMessage "FSharpDotnetNewRun Request: {parms}"
          >> Log.addContextDestructured "parms" p
        )

        let! res = Commands.DotnetNewRun p.Template p.Name p.Output []

        let res =
          match res with
          | CoreResponse.InfoRes msg -> success None
          | CoreResponse.ErrorRes msg -> LspResult.internalError msg
          | CoreResponse.Res(_) -> success None

        return res
      }

    override __.FSharpDotnetAddProject(p: DotnetProjectRequest) =
      async {
        logger.info (
          Log.setMessage "FSharpDotnetAddProject Request: {parms}"
          >> Log.addContextDestructured "parms" p
        )

        let! res = Commands.DotnetAddProject p.Target p.Reference

        let res =
          match res with
          | CoreResponse.InfoRes msg -> success None
          | CoreResponse.ErrorRes msg -> LspResult.internalError msg
          | CoreResponse.Res(_) -> success None

        return res
      }

    override __.FSharpDotnetRemoveProject(p: DotnetProjectRequest) =
      async {
        logger.info (
          Log.setMessage "FSharpDotnetRemoveProject Request: {parms}"
          >> Log.addContextDestructured "parms" p
        )

        let! res = Commands.DotnetRemoveProject p.Target p.Reference

        let res =
          match res with
          | CoreResponse.InfoRes msg -> success None
          | CoreResponse.ErrorRes msg -> LspResult.internalError msg
          | CoreResponse.Res(_) -> success None

        return res
      }

    override __.FSharpDotnetSlnAdd(p: DotnetProjectRequest) =
      async {
        logger.info (
          Log.setMessage "FSharpDotnetSlnAdd Request: {parms}"
          >> Log.addContextDestructured "parms" p
        )

        let! res = Commands.DotnetSlnAdd p.Target p.Reference

        let res =
          match res with
          | CoreResponse.InfoRes msg -> success None
          | CoreResponse.ErrorRes msg -> LspResult.internalError msg
          | CoreResponse.Res(_) -> success None

        return res
      }

    override __.FsProjMoveFileUp(p: DotnetFileRequest) =
      async {
        logger.info (
          Log.setMessage "FsProjMoveFileUp Request: {parms}"
          >> Log.addContextDestructured "parms" p
        )

        let! res = Commands.FsProjMoveFileUp p.FsProj p.FileVirtualPath

        let res =
          match res with
          | CoreResponse.InfoRes msg -> success None
          | CoreResponse.ErrorRes msg -> LspResult.internalError msg
          | CoreResponse.Res(_) -> success None

        return res
      }

    override __.FsProjMoveFileDown(p: DotnetFileRequest) =
      async {
        logger.info (
          Log.setMessage "FsProjMoveFileDown Request: {parms}"
          >> Log.addContextDestructured "parms" p
        )

        let! res = Commands.FsProjMoveFileDown p.FsProj p.FileVirtualPath

        let res =
          match res with
          | CoreResponse.InfoRes msg -> success None
          | CoreResponse.ErrorRes msg -> LspResult.internalError msg
          | CoreResponse.Res(_) -> success None

        return res
      }

    override __.FsProjAddFileAbove(p: DotnetFile2Request) =
      async {
        logger.info (
          Log.setMessage "FsProjAddFileAbove Request: {parms}"
          >> Log.addContextDestructured "parms" p
        )

        let! res = commands.FsProjAddFileAbove p.FsProj p.FileVirtualPath p.NewFile

        let res =
          match res with
          | CoreResponse.InfoRes msg -> success None
          | CoreResponse.ErrorRes msg -> LspResult.internalError msg
          | CoreResponse.Res(_) -> success None

        return res
      }

    override __.FsProjAddFileBelow(p: DotnetFile2Request) =
      async {
        logger.info (
          Log.setMessage "FsProjAddFileBelow Request: {parms}"
          >> Log.addContextDestructured "parms" p
        )

        let! res = commands.FsProjAddFileBelow p.FsProj p.FileVirtualPath p.NewFile

        let res =
          match res with
          | CoreResponse.InfoRes msg -> success None
          | CoreResponse.ErrorRes msg -> LspResult.internalError msg
          | CoreResponse.Res(_) -> success None

        return res
      }

    override __.FsProjRenameFile(p: DotnetRenameFileRequest) =
      async {
        logger.info (
          Log.setMessage "FsProjRenameFile Request: {parms}"
          >> Log.addContextDestructured "parms" p
        )

        let! res = commands.FsProjRenameFile p.FsProj p.OldFileVirtualPath p.NewFileName

        let res =
          match res with
          | CoreResponse.InfoRes msg -> success None
          | CoreResponse.ErrorRes msg -> LspResult.internalError msg
          | CoreResponse.Res(_) -> success None

        return res
      }

    override __.FsProjAddFile(p: DotnetFileRequest) =
      async {
        logger.info (
          Log.setMessage "FsProjAddFile Request: {parms}"
          >> Log.addContextDestructured "parms" p
        )

        let! res = commands.FsProjAddFile p.FsProj p.FileVirtualPath

        let res =
          match res with
          | CoreResponse.InfoRes msg -> success None
          | CoreResponse.ErrorRes msg -> LspResult.internalError msg
          | CoreResponse.Res(_) -> success None

        return res
      }

    override _.FsProjAddExistingFile(p: DotnetFileRequest) =
      async {
        logger.info (
          Log.setMessage "FsProjAddExistingFile Request: {parms}"
          >> Log.addContextDestructured "parms" p
        )

        let! res = commands.FsProjAddExistingFile p.FsProj p.FileVirtualPath

        let res =
          match res with
          | CoreResponse.InfoRes msg -> success None
          | CoreResponse.ErrorRes msg -> LspResult.internalError msg
          | CoreResponse.Res(_) -> success None

        return res
      }

    override _.FsProjRemoveFile(p: DotnetFileRequest) =
      async {
        logger.info (
          Log.setMessage "FsProjRemoveFile Request: {parms}"
          >> Log.addContextDestructured "parms" p
        )

        let fullPath = Path.Combine(Path.GetDirectoryName p.FsProj, p.FileVirtualPath)
        let! res = commands.FsProjRemoveFile p.FsProj p.FileVirtualPath fullPath
        // Compute the fileUri, as this is the key used by the diagnostics collection.
        let fileUri = Path.FilePathToUri fullPath

        diagnosticCollections.ClearFor fileUri

        let res =
          match res with
          | CoreResponse.InfoRes msg -> success None
          | CoreResponse.ErrorRes msg -> LspResult.internalError msg
          | CoreResponse.Res(_) -> success None

        return res
      }

    override x.FSharpHelp(p: TextDocumentPositionParams) =
      logger.info (
        Log.setMessage "FSharpHelp Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      p
      |> x.positionHandler (fun p pos tyRes lineStr lines ->
        (match Commands.Help tyRes pos lineStr with
         | CoreResponse.InfoRes msg -> success None
         | CoreResponse.ErrorRes msg -> LspResult.internalError msg
         | CoreResponse.Res(t) ->
           { Content = CommandResponse.help FsAutoComplete.JsonSerializer.writeJson t }
           |> Some
           |> success)
        |> async.Return)


    override x.FSharpDocumentation(p: TextDocumentPositionParams) =
      logger.info (
        Log.setMessage "FSharpDocumentation Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      p
      |> x.positionHandler (fun p pos tyRes lineStr lines ->
        (match Commands.FormattedDocumentation tyRes pos lineStr with
         | CoreResponse.InfoRes msg -> success None
         | CoreResponse.ErrorRes msg -> LspResult.internalError msg
         | CoreResponse.Res(tip, xml, signature, footer, xmlKey) ->
           let notification: PlainNotification =
             { Content =
                 CommandResponse.formattedDocumentation
                   JsonSerializer.writeJson
                   {| Tip = tip
                      XmlSig = xml
                      Signature = signature
                      Footer = footer
                      XmlKey = xmlKey |} }

           success (Some notification))
        |> async.Return)

    override x.FSharpDocumentationSymbol(p: DocumentationForSymbolReuqest) =
      async {
        logger.info (
          Log.setMessage "FSharpDocumentationSymbol Request: {parms}"
          >> Log.addContextDestructured "parms" p
        )

        match commands.LastCheckResult with
        | None -> return success None
        | Some tyRes ->
          match Commands.FormattedDocumentationForSymbol tyRes p.XmlSig p.Assembly with
          | (CoreResponse.InfoRes msg) -> return success None
          | (CoreResponse.ErrorRes msg) -> return! AsyncLspResult.internalError msg
          | (CoreResponse.Res(xml, assembly, xmlDoc, signature, footer, xmlKey)) ->

            return
              { Content =
                  CommandResponse.formattedDocumentationForSymbol
                    JsonSerializer.writeJson
                    {| Xml = xml
                       Assembly = assembly
                       XmlDoc = xmlDoc
                       Signature = signature
                       Footer = footer
                       XmlKey = xmlKey |} }
              |> Some
              |> success
      }

    override __.LoadAnalyzers(path) =
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

    override __.FSharpLiterateRequest(p: FSharpLiterateRequest) = Helpers.notImplemented

    // override __.FSharpLiterate (p: FSharpLiterateRequest) = async {
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
            Commands.InlayHints(
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

    override x.FSharpPipelineHints(p: FSharpPipelineHintRequest) =
      logger.info (
        Log.setMessage "FSharpPipelineHints Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      p.TextDocument
      |> x.fileHandler (fun fn tyRes lines ->
        async {
          match! commands.PipelineHints tyRes with
          | CoreResponse.InfoRes msg -> return! async.Return(success None)
          | CoreResponse.ErrorRes msg -> return! AsyncLspResult.internalError msg
          | CoreResponse.Res(res) ->
            return
              { Content = CommandResponse.pipelineHint FsAutoComplete.JsonSerializer.writeJson res }
              |> Some
              |> success
        })

    override x.TextDocumentInlineValue(p: InlineValueParams) : AsyncLspResult<InlineValue[] option> =
      logger.info (
        Log.setMessage "TextDocumentInlineValue Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      p.TextDocument
      |> x.fileHandler (fun fn tyRes lines ->
        async {
          let fcsRange = protocolRangeToRange (UMX.untag fn) p.Range

          let! pipelineHints = Commands.InlineValues(lines, tyRes)

          let hints =
            pipelineHints
            |> Array.map (fun (pos, linehints) ->
              { InlineValueText.Range = fcsPosToProtocolRange pos
                Text = (linehints.ToString()) }
              |> InlineValue.InlineValueText)

          return success (Some hints)
        })

    override x.Dispose() =
      (x :> ILspServer).Shutdown() |> Async.Start

    member this.WorkDoneProgessCancel(arg1: ProgressToken) : Async<unit> = failwith "Not Implemented"

    member this.CallHierarchyIncomingCalls
      (arg1: CallHierarchyIncomingCallsParams)
      : AsyncLspResult<CallHierarchyIncomingCall array option> =
      failwith "Not Implemented"

    member this.CallHierarchyOutgoingCalls
      (arg1: CallHierarchyOutgoingCallsParams)
      : AsyncLspResult<CallHierarchyOutgoingCall array option> =
      failwith "Not Implemented"

    member this.TextDocumentPrepareCallHierarchy
      (arg1: CallHierarchyPrepareParams)
      : AsyncLspResult<CallHierarchyItem array option> =
      failwith "Not Implemented"

    member this.TextDocumentPrepareTypeHierarchy
      (arg1: TypeHierarchyPrepareParams)
      : AsyncLspResult<TypeHierarchyItem array option> =
      failwith "Not Implemented"

    member this.TypeHierarchySubtypes
      (arg1: TypeHierarchySubtypesParams)
      : AsyncLspResult<TypeHierarchyItem array option> =
      failwith "Not Implemented"

    member this.TypeHierarchySupertypes
      (arg1: TypeHierarchySupertypesParams)
      : AsyncLspResult<TypeHierarchyItem array option> =
      failwith "Not Implemented"

module FSharpLspServer =

  open System.Threading.Tasks
  open StreamJsonRpc

  let createRpc (handler: IJsonRpcMessageHandler) : JsonRpc =
    let rec (|HandleableException|_|) (e: exn) =
      match e with
      | :? LocalRpcException -> Some()
      | :? TaskCanceledException -> Some()
      | :? OperationCanceledException -> Some()
      | :? System.AggregateException as aex ->
        if aex.InnerExceptions.Count = 1 then
          (|HandleableException|_|) aex.InnerException
        else
          None
      | _ -> None

    { new JsonRpc(handler) with
        member this.IsFatalException(ex: Exception) =
          match ex with
          | HandleableException -> false
          | _ -> true }

  let startCore toolsPath stateStorageDir workspaceLoaderFactory sourceTextFactory =
    use input = Console.OpenStandardInput()
    use output = Console.OpenStandardOutput()

    let requestsHandlings =
      (defaultRequestHandlings (): Map<string, ServerRequestHandling<IFSharpLspServer>>)
      |> Map.add "fsharp/signature" (serverRequestHandling (fun s p -> s.FSharpSignature(p)))
      |> Map.add "fsharp/signatureData" (serverRequestHandling (fun s p -> s.FSharpSignatureData(p)))
      |> Map.add "fsharp/documentationGenerator" (serverRequestHandling (fun s p -> s.FSharpDocumentationGenerator(p)))
      |> Map.add "fsharp/lineLens" (serverRequestHandling (fun s p -> s.FSharpLineLens(p)))
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
      |> Map.add "fsproj/addExistingFile" (serverRequestHandling (fun s p -> s.FsProjAddExistingFile(p)))
      |> Map.add "fsproj/renameFile" (serverRequestHandling (fun s p -> s.FsProjRenameFile(p)))
      |> Map.add "fsproj/removeFile" (serverRequestHandling (fun s p -> s.FsProjRemoveFile(p)))

    let regularServer lspClient =
      let state = State.Initial toolsPath stateStorageDir workspaceLoaderFactory
      let originalFs = FSharp.Compiler.IO.FileSystemAutoOpens.FileSystem
      FSharp.Compiler.IO.FileSystemAutoOpens.FileSystem <- FsAutoComplete.FileSystem(originalFs, state.Files.TryFind)
      new FSharpLspServer(state, lspClient, sourceTextFactory) :> IFSharpLspServer


    Ionide.LanguageServerProtocol.Server.start requestsHandlings input output FSharpLspClient regularServer createRpc

  let start (startCore: unit -> LspCloseReason) =
    let logger = LogProvider.getLoggerByName "Startup"

    try
      let result = startCore ()

      logger.info (
        Log.setMessage "Start - Ending LSP mode with {reason}"
        >> Log.addContextDestructured "reason" result
      )

      int result
    with ex ->
      logger.error (Log.setMessage "Start - LSP mode crashed" >> Log.addExn ex)

      3
