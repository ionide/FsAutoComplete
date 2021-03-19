module FsAutoComplete.Lsp

open FsAutoComplete
open FsAutoComplete.CodeFix
open FsAutoComplete.CodeFix.Types
open FsAutoComplete.Logging
open FSharp.Compiler.SourceCodeServices
open LanguageServerProtocol
open LanguageServerProtocol.LspResult
open LanguageServerProtocol.Types
open LspHelpers
open Newtonsoft.Json.Linq
open Ionide.ProjInfo.ProjectSystem
open System
open System.IO
open FsToolkit.ErrorHandling
open FSharp.UMX

module FcsRange = FSharp.Compiler.Text.Range
type FcsRange = FSharp.Compiler.Text.Range
module FcsPos = FSharp.Compiler.Text.Pos
type FcsPos = FSharp.Compiler.Text.Pos

module AsyncResult =
  let ofCoreResponse (ar: Async<CoreResponse<'a>>) =
    ar |> Async.map (function | CoreResponse.Res a -> Ok a | CoreResponse.ErrorRes msg | CoreResponse.InfoRes msg -> Error (JsonRpc.Error.InternalErrorMessage msg))


open FSharp.Analyzers

type FSharpLspClient(sendServerNotification: ClientNotificationSender, sendServerRequest: ClientRequestSender) =
    inherit LspClient ()

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

    override __.WorkspaceWorkspaceFolders () =
        sendServerRequest.Send "workspace/workspaceFolders" ()

    override __.WorkspaceConfiguration (p) =
        sendServerRequest.Send "workspace/configuration" (box p)

    override __.WorkspaceApplyEdit (p) =
        sendServerRequest.Send "workspace/applyEdit" (box p)

    override __.WorkspaceSemanticTokensRefresh () =
      sendServerNotification "workspace/semanticTokens/refresh" () |> Async.Ignore

    override __.TextDocumentPublishDiagnostics(p) =
        sendServerNotification "textDocument/publishDiagnostics" (box p) |> Async.Ignore

    ///Custom notification for workspace/solution/project loading events
    member __.NotifyWorkspace (p: PlainNotification) =
        sendServerNotification "fsharp/notifyWorkspace" (box p) |> Async.Ignore

    ///Custom notification for initial workspace peek
    member __.NotifyWorkspacePeek (p: PlainNotification) =
        sendServerNotification "fsharp/notifyWorkspacePeek" (box p) |> Async.Ignore

    member __.NotifyCancelledRequest (p: PlainNotification) =
        sendServerNotification "fsharp/notifyCancel" (box p) |> Async.Ignore

    member __.NotifyFileParsed (p: PlainNotification) =
        sendServerNotification "fsharp/fileParsed" (box p) |> Async.Ignore

type FSharpLspServer(commands: Commands, input, output, serializer) as this =
    inherit LspServer(input, output, serializer)

    let logger = LogProvider.getLoggerByName "LSP"
    let fantomasLogger = LogProvider.getLoggerByName "Fantomas"

    let mutable clientCapabilities: ClientCapabilities option = None
    let mutable glyphToCompletionKind = glyphToCompletionKindGenerator None
    let mutable glyphToSymbolKind = glyphToSymbolKindGenerator None
    let subscriptions = ResizeArray<IDisposable>()

    let mutable config = FSharpConfig.Default
    let mutable rootPath : string option = None
    let mutable codeFixes = fun p -> [||]
    let mutable lspClient: FSharpLspClient = Unchecked.defaultof<_>

    do
      this.AddRequestHandler("fsharp/signature", this.FSharpSignature)
      this.AddRequestHandler("fsharp/signatureData", this.FSharpSignatureData)
      this.AddRequestHandler("fsharp/documentationGenerator", this.FSharpDocumentationGenerator)
      this.AddRequestHandler("fsharp/lineLens", this.FSharpLineLense)
      this.AddRequestHandler("fsharp/compilerLocation", this.FSharpCompilerLocation)
      this.AddRequestHandler("fsharp/workspaceLoad", this.FSharpWorkspaceLoad)
      this.AddRequestHandler("fsharp/workspacePeek", this.FSharpWorkspacePeek)
      this.AddRequestHandler("fsharp/project", this.FSharpProject)
      this.AddRequestHandler("fsharp/fsdn", this.FSharpFsdn)
      this.AddRequestHandler("fsharp/dotnetnewlist", this.FSharpDotnetNewList)
      this.AddRequestHandler("fsharp/dotnetnewrun", this.FSharpDotnetNewRun)
      this.AddRequestHandler("fsharp/dotnetaddproject", this.FSharpDotnetAddProject)
      this.AddRequestHandler("fsharp/dotnetremoveproject", this.FSharpDotnetRemoveProject)
      this.AddRequestHandler("fsharp/dotnetaddsln", this.FSharpDotnetSlnAdd)
      this.AddRequestHandler("fsharp/f1Help", this.FSharpHelp)
      this.AddRequestHandler("fsharp/documentation", this.FSharpDocumentation)
      this.AddRequestHandler("fsharp/documentationSymbol", this.FSharpDocumentationSymbol)
      this.AddRequestHandler("fsharp/loadAnalyzers", this.LoadAnalyzers)
      this.AddRequestHandler("fsharp/fsharpLiterate", this.FSharpLiterate)
      this.AddRequestHandler("fsharp/pipelineHint", this.FSharpPipelineHints)
      this.AddRequestHandler("fsproj/moveFileUp", this.FsProjMoveFileUp)
      this.AddRequestHandler("fsproj/moveFileDown", this.FsProjMoveFileDown)
      this.AddRequestHandler("fsproj/addFileAbove", this.FsProjAddFileAbove)
      this.AddRequestHandler("fsproj/addFileBelow", this.FsProjAddFileBelow)
      this.AddRequestHandler("fsproj/addFile", this.FsProjAddFile)

      lspClient <- FSharpLspClient(this.sendServerNotification, { new ClientRequestSender with member x.Send ty payload =  this.sendServerRequest ty payload })

    /// centralize any state changes when the config is updated here
    let updateConfig (newConfig: FSharpConfig) =
        let toCompilerToolArgument (path: string) = sprintf "--compilertool:%s" path
        config <- newConfig

        // only update the dotnet root if it's both a directory and exists
        let di = DirectoryInfo config.DotNetRoot
        if di.Exists
        then
          commands.SetDotnetSDKRoot di
        else
          // if we were mistakenly given the path to a dotnet binary
          // then use the parent directory as the dotnet root instead
          let fi = FileInfo (di.FullName)
          if fi.Exists &&
            ( fi.Name = "dotnet" || fi.Name = "dotnet.exe")
          then
            commands.SetDotnetSDKRoot (fi.Directory)

        commands.SetFSIAdditionalArguments [| yield! config.FSICompilerToolLocations |> Array.map toCompilerToolArgument; yield! config.FSIExtraParameters |]
        commands.SetLinterConfigRelativePath config.LinterConfig
        match config.AnalyzersPath with
        | [||] ->
          Loggers.analyzers.info(Log.setMessage "Analyzers unregistered")
          SDK.Client.registeredAnalyzers.Clear()
        | paths ->
          for path in paths do
            let (newlyFound, total) = SDK.Client.loadAnalyzers path
            Loggers.analyzers.info(Log.setMessage "Registered {count} analyzers from {path}" >> Log.addContextDestructured "count" newlyFound >> Log.addContextDestructured "path" path)
          let total = SDK.Client.registeredAnalyzers.Count
          Loggers.analyzers.info(Log.setMessage "{count} Analyzers registered overall" >> Log.addContextDestructured "count" total)


    //TODO: Thread safe version
    let lintFixes = System.Collections.Generic.Dictionary<DocumentUri, (LanguageServerProtocol.Types.Range * TextEdit) list>()
    let analyzerFixes = System.Collections.Generic.Dictionary<DocumentUri, System.Collections.Generic.Dictionary<string, (LanguageServerProtocol.Types.Range * TextEdit) list>>()


    let parseFile (p: DidChangeTextDocumentParams) =

        async {
            if not commands.IsWorkspaceReady && rootPath.IsSome then
                logger.warn (Log.setMessage "ParseFile - Workspace not ready")
            else
                let doc = p.TextDocument
                let filePath = doc.GetFilePath() |> Utils.normalizePath
                let contentChange = p.ContentChanges |> Seq.tryLast
                match contentChange, doc.Version with
                | Some contentChange, Some version ->
                    if contentChange.Range.IsNone && contentChange.RangeLength.IsNone then
                        let content = contentChange.Text.Split('\n')
                        let tfmConfig = config.UseSdkScripts
                        logger.info (Log.setMessage "ParseFile - Parsing {file}" >> Log.addContextDestructured "file" filePath)
                        do! (commands.Parse filePath content version (Some tfmConfig) |> Async.Ignore)

                        // if config.Linter then do! (commands.Lint filePath |> Async.Ignore)
                        if config.UnusedOpensAnalyzer then  Async.Start (commands.CheckUnusedOpens filePath)
                        if config.UnusedDeclarationsAnalyzer then Async.Start (commands.CheckUnusedDeclarations filePath) //fire and forget this analyzer now that it's syncronous
                        if config.SimplifyNameAnalyzer then Async.Start (commands.CheckSimplifiedNames filePath)
                    else
                        logger.warn (Log.setMessage "ParseFile - Parse not started, received partial change")
                | _ ->
                    logger.info (Log.setMessage "ParseFile - Found no change for {file}" >> Log.addContextDestructured "file" filePath)
        } |> Async.Start

    let parseFileDebuncer = Debounce(500, parseFile)

    let diagnosticCollections = System.Collections.Concurrent.ConcurrentDictionary<DocumentUri * string,Diagnostic[]>()

    let sendDiagnostics (uri: DocumentUri) =
        let diags =
            diagnosticCollections
            |> Seq.collect (fun kv ->
                let (u, _) = kv.Key
                if u = uri then kv.Value else [||])
            |> Seq.sortBy (fun n ->
                n.Range.Start.Line
            )
            |> Seq.toArray
        logger.info (Log.setMessage "SendDiag for {file}: {diags} entries" >> Log.addContextDestructured "file" uri >> Log.addContextDestructured "diags" diags.Length )
        {Uri = uri; Diagnostics = diags}
        |> lspClient.TextDocumentPublishDiagnostics
        |> Async.Start

    do
        commands.Notify.Subscribe(fun n ->
            try
                match n with
                | NotificationEvent.FileParsed fn ->
                    {Content = UMX.untag fn }
                    |> lspClient.NotifyFileParsed
                    |> Async.Start
                | NotificationEvent.Workspace ws ->
                    logger.info (Log.setMessage "Workspace Notify {ws}" >> Log.addContextDestructured "ws" ws)
                    let ws =
                        match ws with
                        | ProjectResponse.Project (x, _) -> CommandResponse.project JsonSerializer.writeJson x
                        | ProjectResponse.ProjectError(_,errorDetails) -> CommandResponse.projectError JsonSerializer.writeJson errorDetails
                        | ProjectResponse.ProjectLoading(projectFileName) -> CommandResponse.projectLoading JsonSerializer.writeJson projectFileName
                        | ProjectResponse.WorkspaceLoad(finished) -> CommandResponse.workspaceLoad JsonSerializer.writeJson finished
                        | ProjectResponse.ProjectChanged(projectFileName) -> CommandResponse.projectChanged JsonSerializer.writeJson projectFileName

                    {Content = ws}
                    |> lspClient.NotifyWorkspace
                    |> Async.Start

                | NotificationEvent.ParseError (errors, file) ->
                    let uri = Path.LocalPathToUri file
                    diagnosticCollections.AddOrUpdate((uri, "F# Compiler"), [||], fun _ _ -> [||]) |> ignore

                    let diags = errors |> Array.map (fcsErrorToDiagnostic)
                    diagnosticCollections.AddOrUpdate((uri, "F# Compiler"), diags, fun _ _ -> diags) |> ignore
                    sendDiagnostics uri

                | NotificationEvent.UnusedOpens (file, opens) ->
                    let uri = Path.LocalPathToUri file
                    diagnosticCollections.AddOrUpdate((uri, "F# Unused opens"), [||], fun _ _ -> [||]) |> ignore

                    let diags = opens |> Array.map(fun n ->
                        {Diagnostic.Range = fcsRangeToLsp n; Code = None; Severity = Some DiagnosticSeverity.Hint; Source = "FSAC"; Message = "Unused open statement"; RelatedInformation = Some [||]; Tags = Some [| DiagnosticTag.Unnecessary |] }
                    )
                    diagnosticCollections.AddOrUpdate((uri, "F# Unused opens"), diags, fun _ _ -> diags) |> ignore
                    sendDiagnostics uri

                | NotificationEvent.UnusedDeclarations (file, decls) ->
                    let uri = Path.LocalPathToUri file
                    diagnosticCollections.AddOrUpdate((uri, "F# Unused declarations"), [||], fun _ _ -> [||]) |> ignore

                    let diags = decls |> Array.map(fun (n, t) ->
                        {Diagnostic.Range = fcsRangeToLsp n; Code = (if t then Some "1" else None); Severity = Some DiagnosticSeverity.Hint; Source = "FSAC"; Message = "This value is unused"; RelatedInformation = Some [||]; Tags = Some [| DiagnosticTag.Unnecessary |] }
                    )
                    diagnosticCollections.AddOrUpdate((uri, "F# Unused declarations"), diags, fun _ _ -> diags) |> ignore
                    sendDiagnostics uri

                | NotificationEvent.SimplifyNames (file, decls) ->
                    let uri = Path.LocalPathToUri file
                    diagnosticCollections.AddOrUpdate((uri, "F# simplify names"), [||], fun _ _ -> [||]) |> ignore

                    let diags = decls |> Array.map(fun ({ Range = range; RelativeName = _relName }) ->
                        { Diagnostic.Range = fcsRangeToLsp range
                          Code = None
                          Severity = Some DiagnosticSeverity.Hint
                          Source = "FSAC"
                          Message = "This qualifier is redundant"
                          RelatedInformation = Some [| |]
                          Tags = Some [| DiagnosticTag.Unnecessary |] }
                    )
                    diagnosticCollections.AddOrUpdate((uri, "F# simplify names"), diags, fun _ _ -> diags) |> ignore
                    sendDiagnostics uri

                // | NotificationEvent.Lint (file, warnings) ->
                //     let uri = Path.LocalPathToUri file
                //     diagnosticCollections.AddOrUpdate((uri, "F# Linter"), [||], fun _ _ -> [||]) |> ignore

                //     let fs =
                //         warnings |> List.choose (fun w ->
                //             w.Warning.Details.SuggestedFix
                //             |> Option.bind (fun f ->
                //                 let f = f.Force()
                //                 let range = fcsRangeToLsp w.Warning.Details.Range
                //                 f |> Option.map (fun f -> range, {Range = range; NewText = f.ToText})
                //             )
                //         )

                //     lintFixes.[uri] <- fs
                //     let diags =
                //         warnings |> List.map(fun w ->
                //             // ideally we'd be able to include a clickable link to the docs page for this errorlint code, but that is not the case here
                //             // neither the Message or the RelatedInformation structures support markdown.
                //             let range = fcsRangeToLsp w.Warning.Details.Range
                //             { Diagnostic.Range = range
                //               Code = Some w.Code
                //               Severity = Some DiagnosticSeverity.Information
                //               Source = "F# Linter"
                //               Message = w.Warning.Details.Message
                //               RelatedInformation = None
                //               Tags = None }
                //         )
                //         |> List.toArray
                //     diagnosticCollections.AddOrUpdate((uri, "F# Linter"), diags, fun _ _ -> diags) |> ignore
                //     sendDiagnostics uri

                | NotificationEvent.Canceled (msg) ->
                    let ntf = {Content = msg}
                    lspClient.NotifyCancelledRequest ntf
                    |> Async.Start
                | NotificationEvent.Diagnostics(p) ->
                    p
                    |> lspClient.TextDocumentPublishDiagnostics
                    |> Async.Start
                | NotificationEvent.AnalyzerMessage(messages, file) ->

                    let uri = Path.LocalPathToUri file
                    diagnosticCollections.AddOrUpdate((uri, "F# Analyzers"), [||], fun _ _ -> [||]) |> ignore
                    match messages with
                    | [||] ->
                      diagnosticCollections.AddOrUpdate((uri, "F# Analyzers"), [||], fun _ _ -> [||]) |> ignore
                    | messages ->
                      let fs =
                          messages
                          |> Seq.collect (fun w ->
                              w.Fixes
                              |> List.map (fun f ->
                                  let range = fcsRangeToLsp f.FromRange
                                  range, {Range = range; NewText = f.ToText})
                          )
                          |> Seq.toList
                      let aName = messages.[0].Type

                      if analyzerFixes.ContainsKey uri then () else analyzerFixes.[uri] <- new System.Collections.Generic.Dictionary<_,_>()
                      analyzerFixes.[uri].[aName] <- fs

                      let diag =
                          messages |> Array.map (fun m ->
                              let range = fcsRangeToLsp m.Range
                              let s =
                                  match m.Severity with
                                  | FSharp.Analyzers.SDK.Info -> DiagnosticSeverity.Information
                                  | FSharp.Analyzers.SDK.Warning -> DiagnosticSeverity.Warning
                                  | FSharp.Analyzers.SDK.Error -> DiagnosticSeverity.Error
                              { Diagnostic.Range = range
                                Code = None
                                Severity = Some s
                                Source = sprintf "F# Analyzers (%s)" m.Type
                                Message = m.Message
                                RelatedInformation = None
                                Tags = None }
                          )
                      diagnosticCollections.AddOrUpdate((uri, "F# Analyzers"), diag, fun _ _ -> diag) |> ignore
                      sendDiagnostics uri
            with
            | _ -> ()
        ) |> subscriptions.Add

    ///Helper function for handling Position requests using **recent** type check results
    member x.positionHandler<'a, 'b when 'b :> ITextDocumentPositionParams> (f: 'b -> FcsPos -> ParseAndCheckResults -> string -> string [] ->  AsyncLspResult<'a>) (arg: 'b) : AsyncLspResult<'a> =
        async {
            let pos = arg.GetFcsPos()
            let file = arg.GetFilePath() |> Utils.normalizePath
            // logger.info (Log.setMessage "PositionHandler - Position request: {file} at {pos}" >> Log.addContextDestructured "file" file >> Log.addContextDestructured "pos" pos)

            return!
                match commands.TryGetFileCheckerOptionsWithLinesAndLineStr(file, pos) with
                | ResultOrString.Error s ->
                    logger.error (Log.setMessage "PositionHandler - Getting file checker options for {file} failed" >> Log.addContextDestructured "error" s >> Log.addContextDestructured "file" file)
                    AsyncLspResult.internalError s
                | ResultOrString.Ok (options, lines, lineStr) ->
                    try
                        let tyResOpt = commands.TryGetRecentTypeCheckResultsForFile(file, options)
                        match tyResOpt with
                        | None ->
                            logger.info (Log.setMessage "PositionHandler - Cached typecheck results not yet available for {file}" >> Log.addContextDestructured "file" file)
                            AsyncLspResult.internalError "Cached typecheck results not yet available"
                        | Some tyRes ->
                            async {
                                let! r = Async.Catch (f arg pos tyRes lineStr lines)
                                match r with
                                | Choice1Of2 r -> return r
                                | Choice2Of2 e ->
                                    logger.error (Log.setMessage "PositionHandler - Failed during child operation on file {file}" >> Log.addContextDestructured "file" file >> Log.addExn e)
                                    return LspResult.internalError e.Message
                            }
                    with e ->
                        logger.error (Log.setMessage "PositionHandler - Operation failed for file {file}" >> Log.addContextDestructured "file" file >> Log.addExn e)
                        AsyncLspResult.internalError e.Message
        }

    ///Helper function for handling Position requests using **latest** type check results
    member x.positionHandlerWithLatest<'a, 'b when 'b :> ITextDocumentPositionParams> (f: 'b -> FcsPos -> ParseAndCheckResults -> string -> string [] ->  AsyncLspResult<'a>) (arg: 'b) : AsyncLspResult<'a> =
        async {
            let pos = arg.GetFcsPos()
            let file = arg.GetFilePath() |> Utils.normalizePath
            // logger.info (Log.setMessage "PositionHandler - Position request: {file} at {pos}" >> Log.addContextDestructured "file" file >> Log.addContextDestructured "pos" pos)

            return!
                    try
                        async {
                        let! tyResOpt = commands.TryGetLatestTypeCheckResultsForFile(file)
                        return!
                            match tyResOpt with
                            | None ->
                                logger.error (Log.setMessage "PositionHandler - Cached typecheck results for {file} not yet available and are required" >> Log.addContextDestructured "file" file)
                                AsyncLspResult.internalError "Cached typecheck results not yet available"
                            | Some tyRes ->
                                match commands.TryGetFileCheckerOptionsWithLinesAndLineStr(file, pos) with
                                | ResultOrString.Error s ->
                                    logger.error (Log.setMessage "PositionHandler - Getting file checker options for {file} failed" >> Log.addContextDestructured "error" s >> Log.addContextDestructured "file" file)
                                    AsyncLspResult.internalError s
                                | ResultOrString.Ok (options, lines, lineStr) ->
                                    async {
                                        let! r = Async.Catch (f arg pos tyRes lineStr lines)
                                        match r with
                                        | Choice1Of2 r -> return r
                                        | Choice2Of2 e ->
                                            logger.error (Log.setMessage "PositionHandler - Failed during child operation on file {file}" >> Log.addContextDestructured "file" file >> Log.addExn e)
                                            return LspResult.internalError e.Message
                                    }
                        }
                    with e ->
                        logger.error (Log.setMessage "PositionHandler - Operation failed for file {file}" >> Log.addContextDestructured "file" file >> Log.addExn e)
                        AsyncLspResult.internalError e.Message
        }

    ///Helper function for handling file requests using **recent** type check results
    member x.fileHandler<'a> (f: string<LocalPath> -> ParseAndCheckResults -> string [] -> AsyncLspResult<'a>) (file: string<LocalPath>) : AsyncLspResult<'a> =
        async {

            // logger.info (Log.setMessage "PositionHandler - Position request: {file} at {pos}" >> Log.addContextDestructured "file" file >> Log.addContextDestructured "pos" pos)

            return!
                match commands.TryGetFileCheckerOptionsWithLines(file) with
                | ResultOrString.Error s ->
                    logger.error (Log.setMessage "FileHandler - Getting file checker options for {file} failed" >> Log.addContextDestructured "error" s >> Log.addContextDestructured "file" file)
                    AsyncLspResult.internalError s
                | ResultOrString.Ok (options, lines) ->
                    try
                        let tyResOpt = commands.TryGetRecentTypeCheckResultsForFile(file, options)
                        match tyResOpt with
                        | None ->
                            logger.info (Log.setMessage "FileHandler - Cached typecheck results not yet available for {file}" >> Log.addContextDestructured "file" file)
                            AsyncLspResult.internalError "Cached typecheck results not yet available"
                        | Some tyRes ->
                            async {
                                let! r = Async.Catch (f file tyRes lines)
                                match r with
                                | Choice1Of2 r -> return r
                                | Choice2Of2 e ->
                                    logger.error (Log.setMessage "FileHandler - Failed during child operation on file {file}" >> Log.addContextDestructured "file" file >> Log.addExn e)
                                    return LspResult.internalError e.Message
                            }
                    with e ->
                        logger.error (Log.setMessage "FileHandler - Operation failed for file {file}" >> Log.addContextDestructured "file" file >> Log.addExn e)
                        AsyncLspResult.internalError e.Message
        }


    override __.Initialize(p: InitializeParams) = async {
        logger.info (Log.setMessage "Initialize Request {p}" >> Log.addContextDestructured "p" p )

        let actualRootPath =
          match p.RootUri with
          | Some rootUri -> Some (Path.FileUriToLocalPath rootUri)
          | None -> p.RootPath

        commands.StartBackgroundService actualRootPath
        rootPath <- actualRootPath
        commands.SetWorkspaceRoot actualRootPath
        clientCapabilities <- p.Capabilities
        glyphToCompletionKind <- glyphToCompletionKindGenerator clientCapabilities
        glyphToSymbolKind <- glyphToSymbolKindGenerator clientCapabilities

        let tryGetParseResultsForFile fileName pos = asyncResult {
          let! (projectOptions, fileLines, lineAtPos) = commands.TryGetFileCheckerOptionsWithLinesAndLineStr(fileName, pos)
          match! commands.TryGetLatestTypeCheckResultsForFile(fileName) with
          | None -> return! Error $"No typecheck results available for %A{fileName}"
          | Some tyRes ->
            return tyRes, lineAtPos, fileLines
        }

        let getFileLines = commands.TryGetFileCheckerOptionsWithLines >> Result.map snd
        let getProjectOptsAndLines = commands.TryGetFileCheckerOptionsWithLinesAndLineStr
        let tryGetProjectOptions = commands.TryGetFileCheckerOptionsWithLines >> Result.map fst

        let interfaceStubReplacements =
          Map.ofList [
            "$objectIdent", config.InterfaceStubGenerationObjectIdentifier
            "$methodBody", config.InterfaceStubGenerationMethodBody
          ]

        let getInterfaceStubReplacements () = interfaceStubReplacements

        let unionCaseStubReplacements =
          Map.ofList [
            "$1", config.UnionCaseStubGenerationBody
          ]

        let getUnionCaseStubReplacements () = unionCaseStubReplacements

        let recordStubReplacements =
          Map.ofList [
            "$1", config.RecordStubGenerationBody
          ]

        let getRecordStubReplacements () = recordStubReplacements

        let abstractClassStubReplacements =
          Map.ofList [
            "$objectIdent", config.AbstractClassStubGenerationObjectIdentifier
            "$methodBody", config.AbstractClassStubGenerationMethodBody
          ]

        let getAbstractClassStubReplacements () = abstractClassStubReplacements

        codeFixes <- fun p ->
          [|
            ifEnabled (fun _ -> config.UnusedOpensAnalyzer) Fixes.unusedOpens
            ifEnabled (fun _ -> config.ResolveNamespaces) (Fixes.resolveNamespace tryGetParseResultsForFile commands.GetNamespaceSuggestions)
            Fixes.errorSuggestion
            Fixes.redundantQualifier
            Fixes.unusedValue getFileLines
            Fixes.newWithDisposables getFileLines
            ifEnabled (fun _ -> config.UnionCaseStubGeneration)
              (Fixes.generateUnionCases getFileLines tryGetParseResultsForFile commands.GetUnionPatternMatchCases getUnionCaseStubReplacements)
            Fixes.mapLinterDiagnostics (fun fileUri -> match lintFixes.TryGetValue(fileUri) with | (true, v) -> Some v | (false, _) -> None )
            Fixes.mapAnalyzerDiagnostics (fun fileUri -> match analyzerFixes.TryGetValue(fileUri) with | (true, v) -> Some (v.Values |> Seq.concat |> Seq.toList) | (false, _) -> None )
            ifEnabled (fun _ -> config.InterfaceStubGeneration)
              (Fixes.generateInterfaceStub tryGetParseResultsForFile commands.GetInterfaceStub getInterfaceStubReplacements)
            ifEnabled (fun _ -> config.RecordStubGeneration)
              (Fixes.generateRecordStub tryGetParseResultsForFile commands.GetRecordStub getRecordStubReplacements)
            ifEnabled (fun _ -> config.AbstractClassStubGeneration)
              (Fixes.generateAbstractClassStub tryGetParseResultsForFile commands.GetAbstractClassStub getAbstractClassStubReplacements)
            Fixes.addMissingEqualsToTypeDefinition getFileLines
            Fixes.changeNegationToSubtraction getFileLines
            Fixes.doubleEqualsToSingleEquality getFileLines
            Fixes.addMissingColonToFieldDefinition
            Fixes.parenthesizeExpression getFileLines
            Fixes.refCellDerefToNot tryGetParseResultsForFile
            Fixes.upcastUsage getFileLines
            Fixes.makeDeclarationMutable tryGetParseResultsForFile tryGetProjectOptions
            Fixes.comparisonToMutableAssignment tryGetParseResultsForFile
            Fixes.partialOrInvalidRecordExpressionToAnonymousRecord tryGetParseResultsForFile
            Fixes.removeUnnecessaryReturnOrYield tryGetParseResultsForFile
            Fixes.rewriteCSharpLambdaToFSharpLambda tryGetParseResultsForFile
            Fixes.addMissingFunKeyword getFileLines
            Fixes.makeOuterBindingRecursive tryGetParseResultsForFile
            Fixes.addMissingRecToMutuallyRecFunctions getFileLines
            Fixes.convertBangEqualsToInequality getFileLines
            Fixes.replaceBangWithValue tryGetParseResultsForFile
          |]
          |> Array.map (fun fixer -> async {
              let! fixes = fixer p
              return List.map (CodeAction.OfFix commands.TryGetFileVersion clientCapabilities.Value) fixes
           })

        let c =
            p.InitializationOptions
            |> Option.bind (fun options -> if options.HasValues then Some options else None)
            |> Option.map this.deserialize<FSharpConfigDto>
            |> Option.map FSharpConfig.FromDto
            |> Option.defaultValue FSharpConfig.Default

        updateConfig c

        match p.RootPath, c.AutomaticWorkspaceInit with
        | None, _
        | _, false -> ()
        | Some p, true ->
            async {
                let! peek = commands.WorkspacePeek p config.WorkspaceModePeekDeepLevel (List.ofArray config.ExcludeProjectDirectories)

                match peek with
                | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                    ()
                | CoreResponse.Res ints ->

                    let serialized = CommandResponse.workspacePeek JsonSerializer.writeJson ints
                    lspClient.NotifyWorkspacePeek {Content = serialized} |> Async.Start

                    let peeks =
                        ints
                        |> List.map Workspace.mapInteresting
                        |> List.sortByDescending (fun x ->
                            match x with
                            | CommandResponse.WorkspacePeekFound.Solution sln -> Workspace.countProjectsInSln sln
                            | CommandResponse.WorkspacePeekFound.Directory _ -> -1)
                    logger.info (Log.setMessage "Choosing from interesting items {items}" >> Log.addContextDestructured "items" peeks)
                    match peeks with
                    | [] -> ()
                    | [CommandResponse.WorkspacePeekFound.Directory projs] ->
                        commands.WorkspaceLoad projs.Fsprojs false config.ScriptTFM config.GenerateBinlog
                        |> Async.Ignore
                        |> Async.Start
                    | CommandResponse.WorkspacePeekFound.Solution sln::_ ->
                        let projs =
                            sln.Items
                            |> List.collect Workspace.foldFsproj
                            |> List.map fst
                        commands.WorkspaceLoad projs false config.ScriptTFM config.GenerateBinlog
                        |> Async.Ignore
                        |> Async.Start
                    | _ ->
                        //TODO: Above case always picks solution with most projects, should be changed
                        ()


                return ()
            } |> Async.Start

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
                        DocumentRangeFormattingProvider = Some false
                        SignatureHelpProvider = Some {
                            TriggerCharacters = Some [| '('; ','; ' ' |]
                            RetriggerCharacters = Some [| ','; ')'; ' '|]
                        }
                        CompletionProvider =
                            Some {
                                ResolveProvider = Some true
                                TriggerCharacters = Some ([| '.'; '''; |])
                                AllCommitCharacters = None //TODO: what chars shoudl commit completions?
                            }
                        CodeLensProvider = Some {
                            CodeLensOptions.ResolveProvider = Some true
                        }
                        CodeActionProvider = Some true
                        TextDocumentSync =
                            Some { TextDocumentSyncOptions.Default with
                                     OpenClose = Some true
                                     Change = Some TextDocumentSyncKind.Full
                                     Save = Some { IncludeText = Some true }
                                 }
                        FoldingRangeProvider = Some true
                        SelectionRangeProvider = Some true
                        SemanticTokensProvider = Some {
                          Legend = createTokenLegend<ClassificationUtils.SemanticTokenTypes, ClassificationUtils.SemanticTokenModifier>
                          Range = Some (U2.First true)
                          Full = Some (U2.First true)
                        }
                    }
            }
            |> success
    }

    override __.Initialized(p: InitializedParams) = async {
        logger.info (Log.setMessage "Initialized request")
        return ()
    }

    override __.TextDocumentDidOpen(p: DidOpenTextDocumentParams) = async {
        let doc = p.TextDocument
        let filePath = doc.GetFilePath() |> Utils.normalizePath
        let content = doc.Text.Split('\n')
        let tfmConfig = config.UseSdkScripts
        logger.info (Log.setMessage "TextDocumentDidOpen Request: {parms}" >> Log.addContextDestructured "parms" filePath )

        commands.SetFileContent(filePath, content, Some doc.Version, config.ScriptTFM)


        if not commands.IsWorkspaceReady && rootPath.IsSome then
            do! commands.WorkspaceReady |> Async.AwaitEvent
            logger.info (Log.setMessage "TextDocumentDidOpen - workspace ready")

        do! (commands.Parse filePath content doc.Version (Some tfmConfig) |> Async.Ignore)

        // if config.Linter then do! (commands.Lint filePath |> Async.Ignore)
        if config.UnusedOpensAnalyzer then Async.Start (commands.CheckUnusedOpens filePath)
        if config.UnusedDeclarationsAnalyzer then Async.Start (commands.CheckUnusedDeclarations filePath)
        if config.SimplifyNameAnalyzer then Async.Start (commands.CheckSimplifiedNames filePath)
    }

    override __.TextDocumentDidChange(p) = async {

        let doc = p.TextDocument
        let filePath = doc.GetFilePath() |> Utils.normalizePath
        let contentChange = p.ContentChanges |> Seq.tryLast

        logger.info (Log.setMessage "TextDocumentDidChange Request: {parms}" >> Log.addContextDestructured "parms" filePath )
        match contentChange, doc.Version with
        | Some contentChange, Some version ->
            if contentChange.Range.IsNone && contentChange.RangeLength.IsNone then
                let content = contentChange.Text.Split('\n')
                commands.SetFileContent(filePath, content, Some version, config.ScriptTFM)
            else ()
        | _ -> ()

        parseFileDebuncer.Bounce p
    }

    //TODO: Investigate if this should be done at all
    override __.TextDocumentDidSave(p) = async {
      logger.info (Log.setMessage "TextDocumentDidSave Request: {parms}" >> Log.addContextDestructured "parms" p )
      ()
    }

    override __.TextDocumentCompletion(p: CompletionParams) =
      let ensureInBounds (lines: LineStr array) (line, col) =
        let lineStr = lines.[line]
        if line <= lines.Length && line >= 0 && col <= lineStr.Length + 1 && col >= 0
        then Ok ()
        else
          logger.info (Log.setMessage "TextDocumentCompletion Not OK:\n COL: {col}\n LINE_STR: {lineStr}\n LINE_STR_LENGTH: {lineStrLength}"
                           >> Log.addContextDestructured "col" col
                           >> Log.addContextDestructured "lineStr" lineStr
                           >> Log.addContextDestructured "lineStrLength" lineStr.Length)

          Error (JsonRpc.Error.InternalErrorMessage "not ok")

      asyncResult {
          logger.info (Log.setMessage "TextDocumentCompletion Request: {context}" >> Log.addContextDestructured "context" p)
          // Sublime-lsp doesn't like when we answer null so we answer an empty list instead
          let file = p.TextDocument.GetFilePath() |> Utils.normalizePath
          let pos = p.GetFcsPos()
          let! (options, lines) = commands.TryGetFileCheckerOptionsWithLines file |> Result.mapError JsonRpc.Error.InternalErrorMessage
          let line, col = p.Position.Line, p.Position.Character
          let lineStr = lines.[line]
          let word = lineStr.Substring(0, col)

          do! ensureInBounds lines (line, col)

          if (lineStr.StartsWith "#" && (KeywordList.hashDirectives.Keys |> Seq.exists (fun k -> k.StartsWith word ) || word.Contains "\n" )) then
              let completionList = { IsIncomplete = false; Items = KeywordList.hashSymbolCompletionItems }
              return! success (Some completionList)
          else
            let! typeCheckResults =
              match p.Context with
              | None ->
                commands.TryGetRecentTypeCheckResultsForFile(file, options)
                |> Result.ofOption (fun _ -> JsonRpc.Error.InternalErrorMessage "No Typecheck results")
                |> async.Return
              | Some ctx ->
                  //ctx.triggerKind = CompletionTriggerKind.Invoked ||
                  if  (ctx.triggerCharacter = Some '.') then
                      commands.TryGetLatestTypeCheckResultsForFile(file)
                      |> Async.map (Result.ofOption (fun _ -> JsonRpc.Error.InternalErrorMessage "No Typecheck results"))
                  else
                      commands.TryGetRecentTypeCheckResultsForFile(file, options)
                      |> Result.ofOption (fun _ -> JsonRpc.Error.InternalErrorMessage "No Typecheck results")
                      |> async.Return
            match! commands.Completion typeCheckResults pos lineStr lines file None (config.KeywordsAutocomplete) (config.ExternalAutocomplete) with
            | CoreResponse.Res(decls, keywords) ->
                let items =
                    decls
                    |> Array.mapi (fun id d ->
                        let code =
                            if System.Text.RegularExpressions.Regex.IsMatch(d.Name, """^[a-zA-Z][a-zA-Z0-9']+$""") then d.Name
                            elif d.NamespaceToOpen.IsSome then d.Name
                            else FSharpKeywords.QuoteIdentifierIfNeeded d.Name
                        let label =
                            match d.NamespaceToOpen with
                            | Some no -> sprintf "%s (open %s)" d.Name no
                            | None -> d.Name

                        { CompletionItem.Create(d.Name) with
                            Kind = glyphToCompletionKind d.Glyph
                            InsertText = Some code
                            SortText = Some (sprintf "%06d" id)
                            FilterText = Some d.Name
                        }
                    )
                let its = if not keywords then items else Array.append items KeywordList.keywordCompletionItems
                let completionList = { IsIncomplete = false; Items = its}
                return! success (Some completionList)
            | _ ->
              logger.info (Log.setMessage "TextDocumentCompletion - no completion results")
              return! success (Some { IsIncomplete = true; Items = [||] })
      }

    override __.CompletionItemResolve(ci: CompletionItem) = async {
        logger.info (Log.setMessage "CompletionItemResolve Request: {parms}" >> Log.addContextDestructured "parms" ci )
        let! res = commands.Helptext ci.InsertText.Value
        let res =
            match res with
            | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                ci
            | CoreResponse.Res (HelpText.Simple (name, str)) ->
                let d = Documentation.Markup (markdown str)
                {ci with Detail = Some name; Documentation = Some d  }
            | CoreResponse.Res (HelpText.Full (name, tip, additionalEdit)) ->
                let (si, comment) = (TipFormatter.formatTip tip) |> List.collect id |> List.head
                let edits, label =
                  match additionalEdit with
                  | None -> None, ci.Label
                  | Some { Namespace = ns; Position = fcsPos } ->
                    Some [| { TextEdit.NewText = $"open {ns}"; TextEdit.Range = fcsPosToProtocolRange fcsPos } |], $"{ci.Label} (open {ns})"
                let d = Documentation.Markup (markdown comment)
                { ci with Detail = Some si
                          Documentation = Some d
                          AdditionalTextEdits = edits
                          Label = label }
        return success res
    }

    override x.TextDocumentSignatureHelp(sigHelpParams: SignatureHelpParams) =
        logger.info (Log.setMessage "TextDocumentSignatureHelp Request: {parms}" >> Log.addContextDestructured "parms" sigHelpParams )
        sigHelpParams |> x.positionHandlerWithLatest (fun p fcsPos tyRes lineStr lines ->
            asyncResult {
                let! (methods, commas) = commands.Methods tyRes fcsPos lines |> AsyncResult.ofCoreResponse
                let sigs =
                    methods.Methods |> Array.map(fun m ->
                        let (sign, comm) = TipFormatter.formatTip m.Description |> List.head |> List.head
                        let parameters =
                            m.Parameters |> Array.map (fun p ->
                                {ParameterInformation.Label = p.ParameterName; Documentation = Some (Documentation.String p.CanonicalTypeTextForSorting)}
                            )
                        let d = Documentation.Markup (markdown comm)
                        { SignatureInformation.Label = sign; Documentation = Some d; Parameters = Some parameters }
                    )

                let activSig =
                    let sigs = sigs |> Seq.sortBy (fun n -> n.Parameters.Value.Length)
                    sigs
                    |> Seq.findIndex (fun s -> s.Parameters.Value.Length >= commas)
                    |> fun index -> if index + 1 >= (sigs |> Seq.length) then index else index + 1

                let res = {Signatures = sigs;
                           ActiveSignature = Some activSig;
                           ActiveParameter = Some commas }

                return! success (Some res)
            }
        )

    override x.TextDocumentHover(p: TextDocumentPositionParams) =
        logger.info (Log.setMessage "TextDocumentHover Request: {parms}" >> Log.addContextDestructured "parms" p )
        p
        |> x.positionHandler (fun p pos tyRes lineStr lines ->
                match commands.ToolTip tyRes pos lineStr with
                | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                    LspResult.internalError msg
                    |> async.Return
                | CoreResponse.Res(tip, signature, footer, typeDoc) ->
                    let formatCommentStyle =
                        if config.TooltipMode = "full" then
                            TipFormatter.FormatCommentStyle.FullEnhanced
                        else if config.TooltipMode = "summary" then
                            TipFormatter.FormatCommentStyle.SummaryOnly
                        else
                            TipFormatter.FormatCommentStyle.Legacy

                    match TipFormatter.formatTipEnhanced tip signature footer typeDoc formatCommentStyle with
                    | (sigCommentFooter::_)::_ ->
                        let signature, comment, footer = sigCommentFooter
                        let markStr lang (value:string) = MarkedString.WithLanguage { Language = lang ; Value = value }
                        let fsharpBlock (lines: string[]) = lines |> String.concat "\n" |> markStr "fsharp"

                        let sigContent =
                            let lines =
                                signature.Split '\n'
                                |> Array.filter (not << String.IsNullOrWhiteSpace)

                            match lines |> Array.splitAt (lines.Length - 1) with
                            | (h, [| StartsWith "Full name:" fullName |]) ->
                                [| yield fsharpBlock h
                                   yield MarkedString.String ("*" + fullName + "*") |]
                            | _ -> [| fsharpBlock lines |]


                        let commentContent =
                            comment
                            |> MarkedString.String

                        let footerContent =
                            footer.Split '\n'
                            |> Array.filter (not << String.IsNullOrWhiteSpace)
                            |> Array.map (fun n -> MarkedString.String ("*" + n + "*"))


                        let response =
                            {
                                Contents =
                                    MarkedStrings
                                        [|
                                            yield! sigContent
                                            yield commentContent
                                            yield! footerContent
                                        |]
                                Range = None
                            }
                        async.Return (success (Some response))
                    | _ -> async.Return (success None)
            )

    override x.TextDocumentRename(p) =
        logger.info (Log.setMessage "TextDocumentRename Request: {parms}" >> Log.addContextDestructured "parms" p )
        p |> x.positionHandler (fun p pos tyRes lineStr lines ->
            async {
                let! res = commands.SymbolUseProject tyRes pos lineStr
                let res =
                    match res with
                    | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                        LspResult.internalError msg
                    | CoreResponse.Res (LocationResponse.Use (_, uses)) ->
                        let documentChanges =
                            uses
                            |> Array.groupBy (fun sym -> sym.FileName)
                            |> Array.map(fun (fileName, symbols) ->
                                let edits =
                                    symbols
                                    |> Array.map (fun sym ->
                                        let range = fcsRangeToLsp sym.RangeAlternate
                                        let range = {range with Start = { Line = range.Start.Line; Character = range.End.Character - sym.Symbol.DisplayName.Length }}
                                        {
                                            Range = range
                                            NewText = p.NewName
                                        })
                                    |> Array.distinct
                                {
                                    TextDocument =
                                        {
                                            Uri = Path.FilePathToUri fileName
                                            Version = commands.TryGetFileVersion (UMX.tag fileName) // from compiler, is safe
                                        }
                                    Edits = edits
                                }
                            )
                        WorkspaceEdit.Create(documentChanges, clientCapabilities.Value) |> Some |> success
                    | CoreResponse.Res (LocationResponse.UseRange uses) ->
                        let documentChanges =
                            uses
                            |> Array.groupBy (fun sym -> sym.FileName)
                            |> Array.map(fun (fileName, symbols) ->
                                let edits =
                                    symbols
                                    |> Array.map (fun sym ->
                                        let range = symbolUseRangeToLsp sym
                                        let range = {range with Start = { Line = range.Start.Line; Character = range.End.Character - sym.SymbolDisplayName.Length }}

                                        {
                                            Range = range
                                            NewText = p.NewName
                                        })
                                    |> Array.distinct
                                {
                                    TextDocument =
                                        {
                                            Uri = Path.FilePathToUri fileName
                                            Version = commands.TryGetFileVersion (UMX.tag fileName) // from compiler, is safe
                                        }
                                    Edits = edits
                                }
                            )
                        WorkspaceEdit.Create(documentChanges, clientCapabilities.Value) |> Some |> success
                return res
            })

    override x.TextDocumentDefinition(p) =
        logger.info (Log.setMessage "TextDocumentDefinition Request: {parms}" >> Log.addContextDestructured "parms" p )
        p |> x.positionHandler (fun p pos tyRes lineStr lines ->
            async {
                let! res = commands.FindDeclaration tyRes pos lineStr
                let res =
                    match res with
                    | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                        LspResult.internalError msg
                    | CoreResponse.Res r ->
                        findDeclToLspLocation r
                        |> GotoResult.Single
                        |> Some
                        |> success
                return res
            })

    override x.TextDocumentTypeDefinition(p) =
        logger.info (Log.setMessage "TextDocumentTypeDefinition Request: {parms}" >> Log.addContextDestructured "parms" p )
        p |> x.positionHandler (fun p pos tyRes lineStr lines ->
            async {
                let! res = commands.FindTypeDeclaration tyRes pos lineStr
                let res =
                    match res with
                    | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                        LspResult.internalError msg
                    | CoreResponse.Res r ->
                        findDeclToLspLocation r
                        |> GotoResult.Single
                        |> Some
                        |> success
                return res
            })

    override x.TextDocumentReferences(p) =
        logger.info (Log.setMessage "TextDocumentReferences Request: {parms}" >> Log.addContextDestructured "parms" p )
        p |> x.positionHandler (fun p pos tyRes lineStr lines ->
            async {
                let! res = commands.SymbolUseProject tyRes pos lineStr
                let res =
                    match res with
                    | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                        LspResult.internalError msg
                    | CoreResponse.Res (LocationResponse.Use (_, uses)) ->
                        uses
                        |> Array.map (fun n -> fcsRangeToLspLocation n.RangeAlternate)
                        |> Some
                        |> success
                    | CoreResponse.Res (LocationResponse.UseRange uses) ->
                        uses
                        |> Array.map symbolUseRangeToLspLocation
                        |> Some
                        |> success
                return res
            })

    override x.TextDocumentDocumentHighlight(p) =
        logger.info (Log.setMessage "TextDocumentDocumentHighlight Request: {parms}" >> Log.addContextDestructured "parms" p )
        p |> x.positionHandler (fun p pos tyRes lineStr lines ->
              match commands.SymbolUse tyRes pos lineStr with
              | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                  async.Return (LspResult.internalError msg)
              | CoreResponse.Res (symbol, uses) ->
                  uses
                  |> Array.map (fun s ->
                  {
                      DocumentHighlight.Range = fcsRangeToLsp s.RangeAlternate
                      Kind = None
                  })
                  |> Some
                  |> success
                  |> async.Return
            )

    override x.TextDocumentImplementation(p) =
        logger.info (Log.setMessage "TextDocumentImplementation Request: {parms}" >> Log.addContextDestructured "parms" p )
        p |> x.positionHandler (fun p pos tyRes lineStr lines ->
            async {
                let! res = commands.SymbolImplementationProject tyRes pos lineStr
                let res =
                    match res with
                    | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                        LspResult.internalError msg
                    | CoreResponse.Res (LocationResponse.Use (symbol, uses)) ->
                        uses
                        |> Array.map (fun n -> fcsRangeToLspLocation n.RangeAlternate)
                        |> GotoResult.Multiple
                        |> Some
                        |> success
                    | CoreResponse.Res (LocationResponse.UseRange uses) ->
                        uses
                        |> Array.map symbolUseRangeToLspLocation
                        |> GotoResult.Multiple
                        |> Some
                        |> success
                return res
            })

    override __.TextDocumentDocumentSymbol(p) = async {
        logger.info (Log.setMessage "TextDocumentDocumentSymbol Request: {parms}" >> Log.addContextDestructured "parms" p )
        let fn = p.TextDocument.GetFilePath() |> Utils.normalizePath
        let! res = commands.Declarations fn None (commands.TryGetFileVersion fn)
        let res =
            match res with
            | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                LspResult.internalError msg
            | CoreResponse.Res decls ->
                decls
                |> Array.collect (fst >> fun top -> getSymbolInformations p.TextDocument.Uri glyphToSymbolKind top (fun s -> true))
                |> Some
                |> success
        return res
    }

    override __.WorkspaceSymbol(symbolRequest: WorkspaceSymbolParams) = async {
        logger.info (Log.setMessage "WorkspaceSymbol Request: {parms}" >> Log.addContextDestructured "parms" symbolRequest)
        let! res = commands.DeclarationsInProjects ()
        let res =
            match res with
            | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                LspResult.internalError msg
            | CoreResponse.Res (decls) ->
                decls
                |> Array.collect (fun (n,p) ->
                    let uri = Path.LocalPathToUri p
                    getSymbolInformations uri glyphToSymbolKind n (applyQuery symbolRequest.Query)
                )
                |> Some
                |> success
        return res
    }

    override __.TextDocumentFormatting(p: DocumentFormattingParams) = async {
        logger.info (Log.setMessage "TextDocumentFormatting Request: {parms}" >> Log.addContextDestructured "parms" p )
        let doc = p.TextDocument
        let fileName = doc.GetFilePath() |> Utils.normalizePath
        let! res = commands.FormatDocument fileName
        match res with
        | Some (lines, formatted) ->
            let range =
                let zero = { Line = 0; Character = 0 }
                let endLine = Array.length lines - 1
                let endCharacter =
                    Array.tryLast lines
                    |> Option.map (fun line -> line.Length)
                    |> Option.defaultValue 0
                { Start = zero; End = { Line = endLine; Character = endCharacter } }

            return LspResult.success(Some([| { Range = range; NewText = formatted  } |]))
        | None ->
            return LspResult.notImplemented
    }

    member private x.HandleTypeCheckCodeAction file pos f =
        async {
                return!
                    match commands.TryGetFileCheckerOptionsWithLinesAndLineStr(file, pos) with
                    | ResultOrString.Error s ->
                        async.Return []
                    | ResultOrString.Ok (options, lines, lineStr) ->
                        try
                            async {
                                let! tyResOpt = commands.TryGetLatestTypeCheckResultsForFile(file)
                                match tyResOpt with
                                | None ->
                                    return []
                                | Some tyRes ->
                                        let! r = Async.Catch (f tyRes lineStr lines)
                                        match r with
                                        | Choice1Of2 r -> return (List.singleton r)
                                        | Choice2Of2 e ->
                                            return []

                            }
                        with e ->
                            async.Return []
            }

    override x.TextDocumentCodeAction(codeActionParams: CodeActionParams) =
        logger.info (Log.setMessage "TextDocumentCodeAction Request: {parms}" >> Log.addContextDestructured "parms" codeActionParams )

        let fn = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath
        match commands.TryGetFileCheckerOptionsWithLines fn with
        | ResultOrString.Error s ->
            AsyncLspResult.internalError s
        | ResultOrString.Ok (opts, lines) ->
        async {
            let! actions =
              Async.Parallel (codeFixes codeActionParams)
              |> Async.map (List.concat >> Array.ofList)
            return actions |> TextDocumentCodeActionResult.CodeActions |> Some |> success
        }

    override __.TextDocumentCodeLens(p) = async {
        logger.info (Log.setMessage "TextDocumentCodeLens Request: {parms}" >> Log.addContextDestructured "parms" p )

        let fn = p.TextDocument.GetFilePath() |> Utils.normalizePath
        let! res = commands.Declarations fn None (commands.TryGetFileVersion fn)
        let res =
            if config.LineLens.Enabled <> "replaceCodeLens" then
                match res with
                | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                    LspResult.internalError msg
                | CoreResponse.Res (decls) ->
                    let res =
                        decls
                        |> Array.map (fst >> getCodeLensInformation p.TextDocument.Uri "signature")
                        |> Array.collect id
                    let res2 =
                        if config.EnableReferenceCodeLens then
                            decls
                            |> Array.map (fst >> getCodeLensInformation p.TextDocument.Uri "reference")
                            |> Array.collect id
                        else
                            [||]

                    [| yield! res2; yield! res |]
                    |> Some
                    |> success
            else
                [| |]
                |> Some
                |> success
        return res
    }

    override __.CodeLensResolve(p) =
        logger.info (Log.setMessage "CodeLensResolve Request: {parms}" >> Log.addContextDestructured "parms" p )

        let handler f (arg: CodeLens) =
            async {
                let pos = FcsPos.mkPos (arg.Range.Start.Line + 1) (arg.Range.Start.Character + 2)
                let data = arg.Data.Value.ToObject<string[]>()
                let file = Path.FileUriToLocalPath data.[0] |> Utils.normalizePath
                return!
                    match commands.TryGetFileCheckerOptionsWithLinesAndLineStr(file, pos) with
                    | ResultOrString.Error s ->
                        logger.error (Log.setMessage "CodeLensResolve - Getting file checker options failed for {file}" >> Log.addContextDestructured "file" file >> Log.addContextDestructured "error" s)
                        let cmd = { Title = "No options"; Command = None; Arguments = None }
                        { p with Command = Some cmd } |> success |> async.Return
                    | ResultOrString.Ok (options, _, lineStr) ->
                        try
                            async {
                                let! tyResOpt = commands.TryGetLatestTypeCheckResultsForFile(file)
                                return!
                                    match tyResOpt with
                                    | None ->
                                        logger.warn (Log.setMessage "CodeLensResolve - Cached typecheck results not yet available for {file}" >> Log.addContextDestructured "file" file)
                                        let cmd = { Title = "No typecheck results"; Command = None; Arguments = None }
                                        { p with Command = Some cmd } |> success |> async.Return
                                    | Some tyRes ->
                                        async {
                                            let! r = Async.Catch (f arg pos tyRes lineStr data.[1] file)
                                            match r with
                                            | Choice1Of2 r -> return r
                                            | Choice2Of2 e ->
                                                logger.error (Log.setMessage "CodeLensResolve - Child operation failed for {file}" >> Log.addContextDestructured "file" file >> Log.addExn e)
                                                let cmd = { Title = ""; Command = None; Arguments = None }
                                                return { p with Command = Some cmd } |> success
                                        }
                            }
                        with e ->
                            logger.error (Log.setMessage "CodeLensResolve - Operation failed on {file}" >> Log.addContextDestructured "file" file >> Log.addExn e)
                            let cmd = { Title = ""; Command = None; Arguments = None }
                            { p with Command = Some cmd } |> success |> async.Return
            }


        handler (fun p pos tyRes lineStr typ file ->
            async {
                if typ = "signature" then
                    match commands.SignatureData tyRes pos lineStr with
                    | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                        logger.error (Log.setMessage "CodeLensResolve - error on file {file}" >> Log.addContextDestructured "file" file >> Log.addContextDestructured "error" msg)
                        let cmd = { Title = ""; Command = None; Arguments = None }
                        return
                          { p with Command = Some cmd }
                          |> success
                    | CoreResponse.Res (typ, parms, _) ->
                        let formatted = SigantureData.formatSignature typ parms
                        let cmd = { Title = formatted; Command = None; Arguments = None }
                        return
                          { p with Command = Some cmd }
                          |> success
                else
                    let! res = commands.SymbolUseProject tyRes pos lineStr
                    let res =
                        match res with
                        | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                            logger.error (Log.setMessage "CodeLensResolve - error getting symbol use for {file}" >> Log.addContextDestructured "file" file >> Log.addContextDestructured "error" msg)
                            let cmd = {Title = ""; Command = None; Arguments = None}
                            {p with Command = Some cmd} |> success
                        | CoreResponse.Res (LocationResponse.Use (sym, uses)) ->
                            let formatted =
                                if uses.Length = 1 then "1 Reference"
                                else sprintf "%d References" uses.Length
                            let locs =
                                uses
                                |> Array.map (fun n -> fcsRangeToLspLocation n.RangeAlternate)

                            let args = [|
                                JToken.FromObject (Path.LocalPathToUri file)
                                JToken.FromObject (fcsPosToLsp pos)
                                JToken.FromObject locs
                            |]

                            let cmd = {Title = formatted; Command = Some "fsharp.showReferences"; Arguments = Some args}
                            {p with Command = Some cmd} |> success
                        | CoreResponse.Res (LocationResponse.UseRange (uses)) ->
                            let formatted =
                                if uses.Length - 1 = 1 then "1 Reference"
                                elif uses.Length = 0 then "0 References"
                                else sprintf "%d References" (uses.Length - 1)
                            let locs =
                                uses
                                |> Array.map symbolUseRangeToLspLocation

                            let args = [|
                                JToken.FromObject (Path.LocalPathToUri file)
                                JToken.FromObject (fcsPosToLsp pos)
                                JToken.FromObject locs
                            |]

                            let cmd = {Title = formatted; Command = Some "fsharp.showReferences"; Arguments = Some args}
                            {p with Command = Some cmd} |> success
                    return res
            }
        ) p

    override __.WorkspaceDidChangeWatchedFiles(p) = async {
        logger.info (Log.setMessage "WorkspaceDidChangeWatchedFiles Request: {parms}" >> Log.addContextDestructured "parms" p )

        p.Changes
        |> Array.iter (fun c ->
            if c.Type = FileChangeType.Deleted then
                let uri = c.Uri
                diagnosticCollections.AddOrUpdate((uri, "F# Compiler"), [||], fun _ _ -> [||]) |> ignore
                diagnosticCollections.AddOrUpdate((uri, "F# Unused opens"), [||], fun _ _ -> [||]) |> ignore
                diagnosticCollections.AddOrUpdate((uri, "F# Unused declarations"), [||], fun _ _ -> [||]) |> ignore
                diagnosticCollections.AddOrUpdate((uri, "F# simplify names"), [||], fun _ _ -> [||]) |> ignore
                diagnosticCollections.AddOrUpdate((uri, "F# Linter"), [||], fun _ _ -> [||]) |> ignore
                sendDiagnostics uri
            ()
        )

        return ()
    }

    override __.WorkspaceDidChangeConfiguration(p: DidChangeConfigurationParams) = async {
        let dto =
            p.Settings
            |> this.deserialize<FSharpConfigRequest>
        logger.info (Log.setMessage "WorkspaceDidChangeConfiguration Request: {parms}" >> Log.addContextDestructured "parms" dto )

        let c = config.AddDto dto.FSharp
        updateConfig c
        logger.info (Log.setMessage "Workspace configuration changed" >> Log.addContextDestructured "config" c)
        return ()
    }

    override __.TextDocumentFoldingRange(rangeP: FoldingRangeParams) = async {
        logger.info (Log.setMessage "TextDocumentFoldingRange Request: {parms}" >> Log.addContextDestructured "parms" rangeP )

        let file = rangeP.TextDocument.GetFilePath() |> Utils.normalizePath
        match! commands.ScopesForFile file with
        | Ok scopes ->
            let ranges = scopes |> Seq.map Structure.toFoldingRange |> Set.ofSeq |> List.ofSeq
            return LspResult.success (Some ranges)
        | Result.Error error ->
            return LspResult.internalError error
    }

    override __.TextDocumentSelectionRange(selectionRangeP: SelectionRangeParams) = async {
        logger.info (Log.setMessage "TextDocumentSelectionRange Request: {parms}" >> Log.addContextDestructured "parms" selectionRangeP)

        let rec mkSelectionRanges = function
            | [] -> None
            | r :: xs -> Some { Range = fcsRangeToLsp r; Parent = mkSelectionRanges xs }

        let file = selectionRangeP.TextDocument.GetFilePath() |> Utils.normalizePath
        let poss = selectionRangeP.Positions |> Array.map protocolPosToPos |> Array.toList
        let! res = commands.GetRangesAtPosition file poss
        match res with
        | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
            return internalError msg
        | CoreResponse.Res ranges ->
            let response = ranges |> List.choose mkSelectionRanges
            // logger.info (Log.setMessage "TextDocumentSelectionRange Response: {parms}" >> Log.addContextDestructured "parms" response)
            return success (Some response)
    }

    member x.FSharpSignature(p: TextDocumentPositionParams) =
        logger.info (Log.setMessage "FSharpSignature Request: {parms}" >> Log.addContextDestructured "parms" p )

        p |> x.positionHandler (fun p pos tyRes lineStr lines ->
            match commands.Typesig tyRes pos lineStr with
            | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                LspResult.internalError msg
            | CoreResponse.Res tip ->
                { Content =  CommandResponse.typeSig FsAutoComplete.JsonSerializer.writeJson tip }
                |> success
            |> async.Return
        )

    member x.FSharpSignatureData(p: TextDocumentPositionParams) =
        logger.info (Log.setMessage "FSharpSignatureData Request: {parms}" >> Log.addContextDestructured "parms" p )

        let handler f (arg: TextDocumentPositionParams) =
            async {
                let pos = FcsPos.mkPos (p.Position.Line) (p.Position.Character + 2)
                let file = p.TextDocument.GetFilePath() |> Utils.normalizePath
                logger.info (Log.setMessage "FSharpSignatureData - Position request for {file} at {pos}" >> Log.addContextDestructured "file" file >> Log.addContextDestructured "pos" pos)
                return!
                    match commands.TryGetFileCheckerOptionsWithLinesAndLineStr(file, pos) with
                    | ResultOrString.Error s ->
                        AsyncLspResult.internalError "No options"
                    | ResultOrString.Ok (options, _, lineStr) ->
                        try
                            async {
                                let! tyResOpt = commands.TryGetLatestTypeCheckResultsForFile(file)
                                return!
                                    match tyResOpt with
                                    | None ->
                                        AsyncLspResult.internalError "No typecheck results"
                                    | Some tyRes ->
                                        async {
                                            let! r = Async.Catch (f arg pos tyRes lineStr)
                                            match r with
                                            | Choice1Of2 r -> return r
                                            | Choice2Of2 e ->
                                                return LspResult.internalError e.Message
                                        }
                            }
                        with e ->
                            AsyncLspResult.internalError e.Message
            }

        p |> handler (fun p pos tyRes lineStr ->
          match commands.SignatureData tyRes pos lineStr with
          | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
              LspResult.internalError msg
          | CoreResponse.Res (typ, parms, generics) ->
              { Content =  CommandResponse.signatureData FsAutoComplete.JsonSerializer.writeJson (typ, parms, generics) }
              |> success
          |> async.Return
        )

    member x.FSharpDocumentationGenerator(p: TextDocumentPositionParams) =
        logger.info (Log.setMessage "FSharpDocumentationGenerator Request: {parms}" >> Log.addContextDestructured "parms" p )

        p |> x.positionHandler (fun p pos tyRes lineStr lines ->
          match commands.SignatureData tyRes pos lineStr with
          | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
              LspResult.internalError msg
          | CoreResponse.Res (typ, parms, generics) ->
              { Content =  CommandResponse.signatureData FsAutoComplete.JsonSerializer.writeJson (typ, parms, generics) }
              |> success
          |> async.Return
        )

    member __.FSharpLineLense(p) = async {
        logger.info (Log.setMessage "FSharpLineLense Request: {parms}" >> Log.addContextDestructured "parms" p )

        let fn = p.Project.GetFilePath() |> Utils.normalizePath
        let! res = commands.Declarations fn None (commands.TryGetFileVersion fn)
        let res =
            match res with
            | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                LspResult.internalError msg
            | CoreResponse.Res (decls) ->
                { Content =  CommandResponse.declarations FsAutoComplete.JsonSerializer.writeJson decls }
                |> success
        return res
    }

    member x.LineLensResolve(p) =
        logger.info (Log.setMessage "LineLensResolve Request: {parms}" >> Log.addContextDestructured "parms" p )

        p |> x.positionHandler (fun p pos tyRes lineStr lines ->
          match commands.SignatureData tyRes pos lineStr with
          | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
              LspResult.internalError msg
          | CoreResponse.Res(typ, parms, generics) ->
              { Content =  CommandResponse.signatureData FsAutoComplete.JsonSerializer.writeJson (typ, parms, generics) }
              |> success
          |> async.Return
        )

    member __.FSharpCompilerLocation(p) = async {
        logger.info (Log.setMessage "FSharpCompilerLocation Request: {parms}" >> Log.addContextDestructured "parms" p )

        let res = commands.CompilerLocation ()
        let res =
            match res with
            | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                LspResult.internalError msg
            | CoreResponse.Res (fsc, fsi, msbuild, sdk) ->
                { Content =  CommandResponse.compilerLocation FsAutoComplete.JsonSerializer.writeJson fsc fsi msbuild (sdk |> Option.map (fun (di: DirectoryInfo) -> di.FullName)) }
                |> success

        return res
    }

    member __.FSharpWorkspaceLoad(p: WorkspaceLoadParms) = async {
        logger.info (Log.setMessage "FSharpWorkspaceLoad Request: {parms}" >> Log.addContextDestructured "parms" p )

        let fns = p.TextDocuments |> Array.map (fun fn -> fn.GetFilePath() ) |> Array.toList
        let! res = commands.WorkspaceLoad fns config.DisableInMemoryProjectReferences config.ScriptTFM config.GenerateBinlog
        let res =
            match res with
            | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                LspResult.internalError msg
            | CoreResponse.Res fin ->
                { Content =  CommandResponse.workspaceLoad FsAutoComplete.JsonSerializer.writeJson fin }
                |> success

        return res
    }

    member __.FSharpWorkspacePeek(p: WorkspacePeekRequest) = async {
        logger.info (Log.setMessage "FSharpWorkspacePeek Request: {parms}" >> Log.addContextDestructured "parms" p )

        let! res = commands.WorkspacePeek p.Directory p.Deep (p.ExcludedDirs |> List.ofArray)
        let res =
            match res with
            | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                LspResult.internalError msg
            | CoreResponse.Res found ->
                { Content =  CommandResponse.workspacePeek FsAutoComplete.JsonSerializer.writeJson found }
                |> success

        return res
    }

    member __.FSharpProject(p) = async {
        logger.info (Log.setMessage "FSharpProject Request: {parms}" >> Log.addContextDestructured "parms" p )

        let fn = p.Project.GetFilePath()
        let! res = commands.Project fn config.GenerateBinlog
        let res =
            match res with
            | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                LspResult.internalError msg
            | CoreResponse.Res fin ->
                { Content =  CommandResponse.projectLoad FsAutoComplete.JsonSerializer.writeJson fin }
                |> success
        return res
    }

    member __.FSharpFsdn(p: FsdnRequest) = async {
        logger.info (Log.setMessage "FSharpFsdn Request: {parms}" >> Log.addContextDestructured "parms" p )

        let! res = commands.Fsdn p.Query
        let res =
            match res with
            | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                LspResult.internalError msg
            | CoreResponse.Res (funcs) ->
                { Content = CommandResponse.fsdn FsAutoComplete.JsonSerializer.writeJson funcs }
                |> success

        return res
    }

    member __.FSharpDotnetNewList(p: DotnetNewListRequest) = async {
        logger.info (Log.setMessage "FSharpDotnetNewList Request: {parms}" >> Log.addContextDestructured "parms" p )

        let! res = commands.DotnetNewList ()
        let res =
            match res with
            | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                LspResult.internalError msg
            | CoreResponse.Res (funcs) ->
                { Content = CommandResponse.dotnetnewlist FsAutoComplete.JsonSerializer.writeJson funcs }
                |> success

        return res
    }

    member __.FSharpDotnetNewRun(p: DotnetNewRunRequest) = async {
        logger.info (Log.setMessage "FSharpDotnetNewRun Request: {parms}" >> Log.addContextDestructured "parms" p )

        let! res = commands.DotnetNewRun p.Template p.Name p.Output []
        let res =
            match res with
            | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                LspResult.internalError msg
            | CoreResponse.Res (_) ->
                { Content = "" }
                |> success

        return res
    }

    member __.FSharpDotnetAddProject(p: DotnetProjectRequest) = async {
        logger.info (Log.setMessage "FSharpDotnetAddProject Request: {parms}" >> Log.addContextDestructured "parms" p )

        let! res = commands.DotnetAddProject p.Target p.Reference
        let res =
            match res with
            | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                LspResult.internalError msg
            | CoreResponse.Res (_) ->
                { Content = "" }
                |> success

        return res
    }

    member __.FSharpDotnetRemoveProject(p: DotnetProjectRequest) = async {
        logger.info (Log.setMessage "FSharpDotnetRemoveProject Request: {parms}" >> Log.addContextDestructured "parms" p )

        let! res = commands.DotnetRemoveProject p.Target p.Reference
        let res =
            match res with
            | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                LspResult.internalError msg
            | CoreResponse.Res (_) ->
                { Content = "" }
                |> success

        return res
    }

    member __.FSharpDotnetSlnAdd(p: DotnetProjectRequest) = async {
        logger.info (Log.setMessage "FSharpDotnetSlnAdd Request: {parms}" >> Log.addContextDestructured "parms" p )

        let! res = commands.DotnetSlnAdd p.Target p.Reference
        let res =
            match res with
            | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                LspResult.internalError msg
            | CoreResponse.Res (_) ->
                { Content = "" }
                |> success

        return res
    }

    member __.FsProjMoveFileUp(p: DotnetFileRequest) = async {
        logger.info (Log.setMessage "FsProjMoveFileUp Request: {parms}" >> Log.addContextDestructured "parms" p )

        let! res = commands.FsProjMoveFileUp p.FsProj p.FileVirtualPath
        let res =
            match res with
            | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                LspResult.internalError msg
            | CoreResponse.Res (_) ->
                { Content = "" }
                |> success

        return res
    }

    member __.FsProjMoveFileDown(p: DotnetFileRequest) = async {
        logger.info (Log.setMessage "FsProjMoveFileDown Request: {parms}" >> Log.addContextDestructured "parms" p )

        let! res = commands.FsProjMoveFileDown p.FsProj p.FileVirtualPath
        let res =
            match res with
            | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                LspResult.internalError msg
            | CoreResponse.Res (_) ->
                { Content = "" }
                |> success

        return res
    }

    member __.FsProjAddFileAbove(p: DotnetFile2Request) = async {
        logger.info (Log.setMessage "FsProjAddFileAbove Request: {parms}" >> Log.addContextDestructured "parms" p )

        let! res = commands.FsProjAddFileAbove p.FsProj p.FileVirtualPath p.NewFile
        let res =
            match res with
            | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                LspResult.internalError msg
            | CoreResponse.Res (_) ->
                { Content = "" }
                |> success

        return res
    }

    member __.FsProjAddFileBelow(p: DotnetFile2Request) = async {
        logger.info (Log.setMessage "FsProjAddFileBelow Request: {parms}" >> Log.addContextDestructured "parms" p )

        let! res = commands.FsProjAddFileBelow p.FsProj p.FileVirtualPath p.NewFile
        let res =
            match res with
            | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                LspResult.internalError msg
            | CoreResponse.Res (_) ->
                { Content = "" }
                |> success

        return res
    }

    member __.FsProjAddFile(p: DotnetFileRequest) = async {
        logger.info (Log.setMessage "FsProjAddFile Request: {parms}" >> Log.addContextDestructured "parms" p )

        let! res = commands.FsProjAddFile p.FsProj p.FileVirtualPath
        let res =
            match res with
            | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                LspResult.internalError msg
            | CoreResponse.Res (_) ->
                { Content = "" }
                |> success

        return res
    }

    member x.FSharpHelp(p: TextDocumentPositionParams) =
        logger.info (Log.setMessage "FSharpHelp Request: {parms}" >> Log.addContextDestructured "parms" p )

        p |> x.positionHandler (fun p pos tyRes lineStr lines ->
          match commands.Help tyRes pos lineStr with
          | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
              LspResult.internalError msg
          | CoreResponse.Res(t) ->
              { Content =  CommandResponse.help FsAutoComplete.JsonSerializer.writeJson t }
              |> success
          |> async.Return
        )


    member x.FSharpDocumentation(p: TextDocumentPositionParams) =
        logger.info (Log.setMessage "FSharpDocumentation Request: {parms}" >> Log.addContextDestructured "parms" p )

        p |> x.positionHandler (fun p pos tyRes lineStr lines ->
          match commands.FormattedDocumentation tyRes pos lineStr with
          | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
              LspResult.internalError msg
          | CoreResponse.Res (tip, xml, signature, footer, cm) ->
              { Content =  CommandResponse.formattedDocumentation FsAutoComplete.JsonSerializer.writeJson (tip, xml, signature, footer, cm) }
              |> success
          |> async.Return
        )

    member x.FSharpDocumentationSymbol(p: DocumentationForSymbolReuqest) =
        logger.info (Log.setMessage "FSharpDocumentationSymbol Request: {parms}" >> Log.addContextDestructured "parms" p )

        match commands.LastCheckResult with
        | None -> AsyncLspResult.internalError "error"
        | Some tyRes ->
          match commands.FormattedDocumentationForSymbol tyRes p.XmlSig p.Assembly with
          | Ok (CoreResponse.InfoRes msg)
          | Ok (CoreResponse.ErrorRes msg) ->
              AsyncLspResult.internalError msg
          | Ok (CoreResponse.Res (xml, assembly, doc, signature, footer, cn)) ->
              { Content = CommandResponse.formattedDocumentationForSymbol FsAutoComplete.JsonSerializer.writeJson xml assembly doc (signature, footer, cn) }
              |> success
              |> async.Return
          | Error e -> AsyncLspResult.internalError e


    // member __.FakeTargets(p:FakeTargetsRequest) = async {
    //     logger.info (Log.setMessage "FakeTargets Request: {parms}" >> Log.addContextDestructured "parms" p )

    //     let! res = commands.FakeTargets (p.FileName |> Utils.normalizePath) (p.FakeContext)
    //     let res =
    //         match res with
    //         | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
    //             LspResult.internalError msg
    //         | CoreResponse.Res (targets) ->
    //             { Content = CommandResponse.fakeTargets FsAutoComplete.JsonSerializer.writeJson targets }
    //             |> success

    //     return res
    // }

    // member __.FakeRuntimePath(p) = async {
    //     logger.info (Log.setMessage "FakeRuntimePath Request: {parms}" >> Log.addContextDestructured "parms" p )


    //     let! res = commands.FakeRuntime ()
    //     let res =
    //         match res with
    //         | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
    //             LspResult.internalError msg
    //         | CoreResponse.Res (runtimePath) ->
    //             { Content = CommandResponse.fakeRuntime FsAutoComplete.JsonSerializer.writeJson runtimePath }
    //             |> success
    //     return res
    // }

    member __.LoadAnalyzers(path) = async {
        logger.info (Log.setMessage "LoadAnalyzers Request: {parms}" >> Log.addContextDestructured "parms" path )

        try
            if config.EnableAnalyzers then

                Loggers.analyzers.info (Log.setMessage "Using analyzer roots of {roots}" >> Log.addContextDestructured "roots" config.AnalyzersPath)
                config.AnalyzersPath
                |> Array.iter (fun analyzerPath ->
                    match rootPath with
                    | None -> ()
                    | Some workspacePath ->
                        let dir =
                          if System.IO.Path.IsPathRooted analyzerPath
                          // if analyzer is using absolute path, use it as is
                          then analyzerPath
                          // otherwise, it is a relative path and should be combined with the workspace path
                          else System.IO.Path.Combine(workspacePath, analyzerPath)
                        Loggers.analyzers.info (Log.setMessage "Loading analyzers from {dir}" >> Log.addContextDestructured "dir" dir)
                        let (n,m) = dir |> SDK.Client.loadAnalyzers
                        Loggers.analyzers.info (Log.setMessage "From {name}: {dllNo} dlls including {analyzersNo} analyzers" >> Log.addContextDestructured "name" analyzerPath >> Log.addContextDestructured "dllNo" n >> Log.addContextDestructured "analyzersNo" m)
                )
            else
                Loggers.analyzers.info (Log.setMessage "Analyzers disabled")
            return LspResult.success ()
        with
        | ex ->
            Loggers.analyzers.error (Log.setMessage "Loading failed" >> Log.addExn ex)
            return LspResult.success ()
    }


    member private x.handleSemanticTokens(getTokens: Async<CoreResponse<option<(struct(FcsRange * SemanticClassificationType)) array>>>): AsyncLspResult<SemanticTokens option> = asyncResult {
      match! getTokens |> AsyncResult.ofCoreResponse with
      | None ->
        return! LspResult.internalError "No highlights found"
      | Some rangesAndHighlights ->
        let lspTypedRanges =
          rangesAndHighlights
          |> Array.map (fun (struct(fcsRange, fcsTokenType)) ->
            let ty, mods = ClassificationUtils.map fcsTokenType
            struct(fcsRangeToLsp fcsRange, ty, mods)
          )
        match encodeSemanticHighlightRanges lspTypedRanges with
        | None ->
          return! success None
        | Some encoded ->
          return! success (Some { Data = encoded; ResultId = None }) // TODO: provide a resultId when we support delta ranges
    }

    override x.TextDocumentSemanticTokensFull (p: SemanticTokensParams): AsyncLspResult<SemanticTokens option> =
      logger.info (Log.setMessage "Semantic highlighing request: {parms}" >> Log.addContextDestructured "parms" p )
      let fn = p.TextDocument.GetFilePath() |> Utils.normalizePath
      x.handleSemanticTokens(commands.GetHighlighting(fn, None))

    override x.TextDocumentSemanticTokensRange (p: SemanticTokensRangeParams): AsyncLspResult<SemanticTokens option> =
      logger.info (Log.setMessage "Semantic highlighing range request: {parms}" >> Log.addContextDestructured "parms" p )
      let fn = p.TextDocument.GetFilePath() |> Utils.normalizePath
      let fcsRange = protocolRangeToRange (UMX.untag fn) p.Range
      x.handleSemanticTokens(commands.GetHighlighting(fn, Some fcsRange))

    member __.ScriptFileProjectOptions = commands.ScriptFileProjectOptions

    member __.FSharpLiterate (p: FSharpLiterateRequest) = async {
      logger.info (Log.setMessage "FSharpLiterate Request: {parms}" >> Log.addContextDestructured "parms" p )

      let fn = p.FileName |> Utils.normalizePath
      let! res = commands.FSharpLiterate fn
      let res =
        match res with
        | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
            LspResult.internalError msg
        | CoreResponse.Res (res) ->
            { Content = CommandResponse.fsharpLiterate FsAutoComplete.JsonSerializer.writeJson res }
            |> success

      return res
    }

    member x.FSharpPipelineHints (p: FSharpPipelineHintRequest) =
      logger.info (Log.setMessage "FSharpPipelineHints Request: {parms}" >> Log.addContextDestructured "parms" p )
      let fn = p.FileName |> Utils.normalizePath
      fn |> x.fileHandler (fun fn tyRes lines ->
          match commands.PipelineHints tyRes with
          | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
            AsyncLspResult.internalError msg
          | CoreResponse.Res (res) ->
            { Content = CommandResponse.pipelineHint FsAutoComplete.JsonSerializer.writeJson res }
            |> success
            |> async.Return
      )

    override x.Dispose () = ()

let startCore (commands: Commands) =
    use input = Console.OpenStandardInput()
    use output = Console.OpenStandardOutput()
    let server = new FSharpLspServer(commands, input, output, defaultSerializer)
    server.Run()

let start (commands: Commands) =
    let logger = LogProvider.getLoggerByName "Startup"

    try
        let result = startCore commands
        logger.info (Log.setMessage "Start - Ending LSP mode with {reason}" >> Log.addContextDestructured "reason" result)
        int result
    with
    | ex ->
        logger.error (Log.setMessage "Start - LSP mode crashed" >> Log.addExn ex)
        3
