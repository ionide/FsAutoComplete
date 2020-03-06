module FsAutoComplete.Lsp

open Argu
open System
open LanguageServerProtocol.Server
open LanguageServerProtocol.Types
open FsAutoComplete.Utils
open FSharp.Compiler.SourceCodeServices
open LanguageServerProtocol
open LanguageServerProtocol.LspResult
open FsAutoComplete
open Newtonsoft.Json.Linq
open LspHelpers
open ProjectSystem
module FcsRange = FSharp.Compiler.Range
open FsAutoComplete.Logging
#if ANALYZER_SUPPORT
open FSharp.Analyzers
#endif

type FSharpLspClient(sendServerRequest: ClientNotificationSender) =
    inherit LspClient ()

    override __.WindowShowMessage(p) =
        sendServerRequest "window/showMessage" (box p) |> Async.Ignore

    override __.WindowLogMessage(p) =
        sendServerRequest "window/logMessage" (box p) |> Async.Ignore

    override __.TextDocumentPublishDiagnostics(p) =
        sendServerRequest "textDocument/publishDiagnostics" (box p) |> Async.Ignore

    ///Custom notification for workspace/solution/project loading events
    member __.NotifyWorkspace (p: PlainNotification) =
        sendServerRequest "fsharp/notifyWorkspace" (box p) |> Async.Ignore

    ///Custom notification for initial workspace peek
    member __.NotifyWorkspacePeek (p: PlainNotification) =
        sendServerRequest "fsharp/notifyWorkspacePeek" (box p) |> Async.Ignore

    member __.NotifyCancelledRequest (p: PlainNotification) =
        sendServerRequest "fsharp/notifyCancel" (box p) |> Async.Ignore

    member __.NotifyFileParsed (p: PlainNotification) =
        sendServerRequest "fsharp/fileParsed" (box p) |> Async.Ignore

    // TODO: Add the missing notifications
    // TODO: Implement requests

type FsharpLspServer(commands: Commands, lspClient: FSharpLspClient) =
    inherit LspServer()

    let logger = LogProvider.getLoggerByName "LSP"
    let fantomasLogger = LogProvider.getLoggerByName "Fantomas"

    let mutable clientCapabilities: ClientCapabilities option = None
    let mutable glyphToCompletionKind = glyphToCompletionKindGenerator None
    let mutable glyphToSymbolKind = glyphToSymbolKindGenerator None
    let subscriptions = ResizeArray<IDisposable>()

    let mutable config = FSharpConfig.Default
    let mutable rootPath : string option = None

    /// centralize any state changes when the config is updated here
    let updateConfig (newConfig: FSharpConfig) =
        config <- newConfig
        commands.SetDotnetSDKRoot config.DotNetRoot
        commands.SetFSIAdditionalArguments config.FSIExtraParameters
        commands.SetLinterConfigRelativePath config.LinterConfig

    //TODO: Thread safe version
    let fixes = System.Collections.Generic.Dictionary<DocumentUri, (LanguageServerProtocol.Types.Range * TextEdit) list>()
    let analyzerFixes = System.Collections.Generic.Dictionary<(DocumentUri * string), (LanguageServerProtocol.Types.Range * TextEdit) list>()


    let parseFile (p: DidChangeTextDocumentParams) =

        async {
            if not commands.IsWorkspaceReady then
                logger.warn (Log.setMessage "ParseFile - Workspace not ready")
            else
                let doc = p.TextDocument
                let filePath = doc.GetFilePath()
                let contentChange = p.ContentChanges |> Seq.tryLast
                match contentChange, doc.Version with
                | Some contentChange, Some version ->
                    if contentChange.Range.IsNone && contentChange.RangeLength.IsNone then
                        let content = contentChange.Text.Split('\n')
                        let tfmConfig = config.UseSdkScripts
                        logger.info (Log.setMessage "ParseFile - Parsing {file}" >> Log.addContextDestructured "file" filePath)
                        do! (commands.Parse filePath content version (Some tfmConfig) |> Async.Ignore)

                        if config.Linter then do! (commands.Lint filePath |> Async.Ignore)
                        if config.UnusedOpensAnalyzer then do! (commands.GetUnusedOpens filePath |> Async.Ignore)
                        if config.UnusedDeclarationsAnalyzer then do! (commands.GetUnusedDeclarations filePath |> Async.Ignore)
                        if config.SimplifyNameAnalyzer then do! (commands.GetSimplifiedNames filePath |> Async.Ignore)
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
        logger.info (Log.setMessage "SendDiag for {file}: {diags}" >> Log.addContextDestructured "file" uri >> Log.addContextDestructured "diags" diags )
        {Uri = uri; Diagnostics = diags}
        |> lspClient.TextDocumentPublishDiagnostics
        |> Async.Start

    /// convert structure scopes to known kinds of folding range.
    /// this lets commands like 'fold all comments' work sensibly.
    /// impl note: implemented as an exhaustive match here so that
    /// if new structure kinds appear we have to handle them.
    let scopeToKind (scope: Structure.Scope): string option =
        match scope with
        | Structure.Scope.Open -> Some FoldingRangeKind.Imports
        | Structure.Scope.Comment
        | Structure.Scope.XmlDocComment -> Some FoldingRangeKind.Comment
        | Structure.Scope.Namespace
        | Structure.Scope.Module
        | Structure.Scope.Type
        | Structure.Scope.Member
        | Structure.Scope.LetOrUse
        | Structure.Scope.Val
        | Structure.Scope.CompExpr
        | Structure.Scope.IfThenElse
        | Structure.Scope.ThenInIfThenElse
        | Structure.Scope.ElseInIfThenElse
        | Structure.Scope.TryWith
        | Structure.Scope.TryInTryWith
        | Structure.Scope.WithInTryWith
        | Structure.Scope.TryFinally
        | Structure.Scope.TryInTryFinally
        | Structure.Scope.FinallyInTryFinally
        | Structure.Scope.ArrayOrList
        | Structure.Scope.ObjExpr
        | Structure.Scope.For
        | Structure.Scope.While
        | Structure.Scope.Match
        | Structure.Scope.MatchBang
        | Structure.Scope.MatchLambda
        | Structure.Scope.MatchClause
        | Structure.Scope.Lambda
        | Structure.Scope.CompExprInternal
        | Structure.Scope.Quote
        | Structure.Scope.Record
        | Structure.Scope.SpecialFunc
        | Structure.Scope.Do
        | Structure.Scope.New
        | Structure.Scope.Attribute
        | Structure.Scope.Interface
        | Structure.Scope.HashDirective
        | Structure.Scope.LetOrUseBang
        | Structure.Scope.TypeExtension
        | Structure.Scope.YieldOrReturn
        | Structure.Scope.YieldOrReturnBang
        | Structure.Scope.Tuple
        | Structure.Scope.UnionCase
        | Structure.Scope.EnumCase
        | Structure.Scope.RecordField
        | Structure.Scope.RecordDefn
        | Structure.Scope.UnionDefn -> None

    let toFoldingRange (item: Structure.ScopeRange): FoldingRange =
        let kind = scopeToKind item.Scope
        // map the collapserange to the foldingRange
        let lsp = fcsRangeToLsp item.CollapseRange
        { StartCharacter   = Some lsp.Start.Character
          StartLine        = lsp.Start.Line
          EndCharacter     = Some lsp.End.Character
          EndLine          = lsp.End.Line
          Kind             = kind }

    do
        commands.Notify.Subscribe(fun n ->
            try
                // logger.info (Log.setMessage "Notify {event}" >> Log.addContextDestructured "event" n)
                match n with
                | NotificationEvent.FileParsed fn ->
                    {Content = fn}
                    |> lspClient.NotifyFileParsed
                    |> Async.Start
                | NotificationEvent.Workspace ws ->
                    let ws =
                        match ws with
                        | ProjectResponse.Project x -> CommandResponse.project JsonSerializer.writeJson x
                        | ProjectResponse.ProjectError(errorDetails) -> CommandResponse.projectError JsonSerializer.writeJson errorDetails
                        | ProjectResponse.ProjectLoading(projectFileName) -> CommandResponse.projectLoading JsonSerializer.writeJson projectFileName
                        | ProjectResponse.WorkspaceLoad(finished) -> CommandResponse.workspaceLoad JsonSerializer.writeJson finished

                    {Content = ws}
                    |> lspClient.NotifyWorkspace
                    |> Async.Start

                | NotificationEvent.ParseError (errors, file) ->
                    let uri = filePathToUri file
                    diagnosticCollections.AddOrUpdate((uri, "F# Compiler"), [||], fun _ _ -> [||]) |> ignore

                    let diags = errors |> Array.map (fcsErrorToDiagnostic)
                    diagnosticCollections.AddOrUpdate((uri, "F# Compiler"), diags, fun _ _ -> diags) |> ignore
                    sendDiagnostics uri

                | NotificationEvent.UnusedOpens (file, opens) ->
                    let uri = filePathToUri file
                    diagnosticCollections.AddOrUpdate((uri, "F# Unused opens"), [||], fun _ _ -> [||]) |> ignore

                    let diags = opens |> Array.map(fun n ->
                        {Diagnostic.Range = fcsRangeToLsp n; Code = None; Severity = Some DiagnosticSeverity.Hint; Source = "FSAC"; Message = "Unused open statement"; RelatedInformation = Some [||]; Tags = Some [| DiagnosticTag.Unnecessary |] }
                    )
                    diagnosticCollections.AddOrUpdate((uri, "F# Unused opens"), diags, fun _ _ -> diags) |> ignore
                    sendDiagnostics uri

                | NotificationEvent.UnusedDeclarations (file, decls) ->
                    let uri = filePathToUri file
                    diagnosticCollections.AddOrUpdate((uri, "F# Unused declarations"), [||], fun _ _ -> [||]) |> ignore

                    let diags = decls |> Array.map(fun (n, t) ->
                        {Diagnostic.Range = fcsRangeToLsp n; Code = (if t then Some "1" else None); Severity = Some DiagnosticSeverity.Hint; Source = "FSAC"; Message = "This value is unused"; RelatedInformation = Some [||]; Tags = Some [| DiagnosticTag.Unnecessary |] }
                    )
                    diagnosticCollections.AddOrUpdate((uri, "F# Unused declarations"), diags, fun _ _ -> diags) |> ignore
                    sendDiagnostics uri

                | NotificationEvent.SimplifyNames (file, decls) ->
                    let uri = filePathToUri file
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

                | NotificationEvent.Lint (file, warnings) ->
                    let uri = filePathToUri file
                    diagnosticCollections.AddOrUpdate((uri, "F# Linter"), [||], fun _ _ -> [||]) |> ignore

                    let fs =
                        warnings |> List.choose (fun w ->
                            w.Warning.Details.SuggestedFix
                            |> Option.bind (fun f ->
                                let f = f.Force()
                                let range = fcsRangeToLsp w.Warning.Details.Range
                                f |> Option.map (fun f -> range, {Range = range; NewText = f.ToText})
                            )
                        )

                    fixes.[uri] <- fs
                    let diags =
                        warnings |> List.map(fun w ->
                            // ideally we'd be able to include a clickable link to the docs page for this errorlint code, but that is not the case here
                            // neither the Message or the RelatedInformation structures support markdown.
                            let range = fcsRangeToLsp w.Warning.Details.Range
                            { Diagnostic.Range = range
                              Code = Some w.Code
                              Severity = Some DiagnosticSeverity.Information
                              Source = "F# Linter"
                              Message = w.Warning.Details.Message
                              RelatedInformation = None
                              Tags = None }
                        )
                        |> List.toArray
                    diagnosticCollections.AddOrUpdate((uri, "F# Linter"), diags, fun _ _ -> diags) |> ignore
                    sendDiagnostics uri
                | NotificationEvent.Canceled (msg) ->
                    let ntf = {Content = msg}
                    lspClient.NotifyCancelledRequest ntf
                    |> Async.Start
                | NotificationEvent.Diagnostics(p) ->
                    p
                    |> lspClient.TextDocumentPublishDiagnostics
                    |> Async.Start
                | NotificationEvent.AnalyzerMessage(messages, file) ->
#if ANALYZER_SUPPORT
                    let messages = messages :?> SDK.Message []
                    let uri = filePathToUri file
                    diagnosticCollections.AddOrUpdate((uri, "F# Analyzers"), [||], fun _ _ -> [||]) |> ignore
                    let fs =
                        messages |> Seq.collect (fun w ->
                            w.Fixes
                            |> List.map (fun f ->
                                let range = fcsRangeToLsp f.FromRange
                                range, {Range = range; NewText = f.ToText})
                        )
                        |> Seq.toList
                    let aName = (messages |> Seq.head).Type


                    analyzerFixes.[(uri, aName)] <- fs

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
#else
                    ()
#endif
            with
            | _ -> ()
        ) |> subscriptions.Add

    ///Helper function for handling Position requests using **recent** type check results
    member x.positionHandler<'a, 'b when 'b :> ITextDocumentPositionParams> (f: 'b -> FcsRange.pos -> ParseAndCheckResults -> string -> string [] ->  AsyncLspResult<'a>) (arg: 'b) : AsyncLspResult<'a> =
        async {
            let pos = arg.GetFcsPos()
            let file = arg.GetFilePath()
            logger.info (Log.setMessage "PositionHandler - Position request: {file} at {pos}" >> Log.addContextDestructured "file" file >> Log.addContextDestructured "pos" pos)

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
    member x.positionHandlerWithLatest<'a, 'b when 'b :> ITextDocumentPositionParams> (f: 'b -> FcsRange.pos -> ParseAndCheckResults -> string -> string [] ->  AsyncLspResult<'a>) (arg: 'b) : AsyncLspResult<'a> =
        async {
            let pos = arg.GetFcsPos()
            let file = arg.GetFilePath()
            logger.info (Log.setMessage "PositionHandler - Position request: {file} at {pos}" >> Log.addContextDestructured "file" file >> Log.addContextDestructured "pos" pos)

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


    override __.Initialize(p: InitializeParams) = async {
        let actualRootPath =
          match p.RootUri with
          | Some rootUri -> Some (fileUriToLocalPath rootUri)
          | None -> p.RootPath

        commands.StartBackgroundService actualRootPath
        rootPath <- actualRootPath
        commands.SetWorkspaceRoot actualRootPath
        clientCapabilities <- p.Capabilities
        glyphToCompletionKind <- glyphToCompletionKindGenerator clientCapabilities
        glyphToSymbolKind <- glyphToSymbolKindGenerator clientCapabilities

#if ANALYZER_SUPPORT
        let analyzerHandler (file, content, pt, tast, symbols, getAllEnts) =
          let ctx : SDK.Context = {
            FileName = file
            Content = content
            ParseTree = pt
            TypedTree = tast
            Symbols = symbols
            GetAllEntities = getAllEnts
          }
          SDK.Client.runAnalyzers ctx |> Array.ofList |> box
        commands.AnalyzerHandler <- Some analyzerHandler
#endif
        let c =
            p.InitializationOptions
            |> Option.bind (fun options -> if options.HasValues then Some options else None)
            |> Option.map Server.deserialize<FSharpConfigDto>
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

                    match peeks with
                    | [] -> ()
                    | [CommandResponse.WorkspacePeekFound.Directory projs] ->
                        commands.WorkspaceLoad ignore projs.Fsprojs false config.ScriptTFM
                        |> Async.Ignore
                        |> Async.Start
                    | CommandResponse.WorkspacePeekFound.Solution sln::_ ->
                        let projs =
                            sln.Items
                            |> List.collect Workspace.foldFsproj
                            |> List.map fst
                        commands.WorkspaceLoad ignore projs false config.ScriptTFM
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
                            SignatureHelpOptions.TriggerCharacters = Some [| "("; ","|]
                        }
                        CompletionProvider =
                            Some {
                                ResolveProvider = Some true
                                TriggerCharacters = Some ([| "."; "'"; |])
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
                    }
            }
            |> success
    }

    override __.Initialized(p: InitializedParams) = async {
        return ()
    }

    override __.TextDocumentDidOpen(p: DidOpenTextDocumentParams) = async {

        let doc = p.TextDocument
        let filePath = doc. GetFilePath()
        let content = doc.Text.Split('\n')
        let tfmConfig = config.UseSdkScripts

        commands.SetFileContent(filePath, content, Some doc.Version, config.ScriptTFM)


        if not commands.IsWorkspaceReady then
            do! commands.WorkspaceReady |> Async.AwaitEvent
            logger.info (Log.setMessage "TextDocumentDidOpen - workspace ready")

        do! (commands.Parse filePath content doc.Version (Some tfmConfig) |> Async.Ignore)

        if config.Linter then do! (commands.Lint filePath |> Async.Ignore)
        if config.UnusedOpensAnalyzer then do! (commands.GetUnusedOpens filePath |> Async.Ignore)
        if config.UnusedDeclarationsAnalyzer then do! (commands.GetUnusedDeclarations filePath |> Async.Ignore)
        if config.SimplifyNameAnalyzer then do! (commands.GetSimplifiedNames filePath |> Async.Ignore)
    }

    override __.TextDocumentDidChange(p) = async {

        let doc = p.TextDocument
        let filePath = doc.GetFilePath()
        let contentChange = p.ContentChanges |> Seq.tryLast
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
      ()
    }

    override __.TextDocumentCompletion(p: CompletionParams) = async {

        logger.info (Log.setMessage "TextDocumentCompletion triggered with {context}" >> Log.addContextDestructured "context" p.Context)
        // Sublime-lsp doesn't like when we answer null so we answer an empty list instead
        let noCompletion = success (Some { IsIncomplete = true; Items = [||] })
        let doc = p.TextDocument
        let file = doc.GetFilePath()
        let pos = p.GetFcsPos()
        let! res =
            match commands.TryGetFileCheckerOptionsWithLines file with
            | ResultOrString.Error s -> AsyncLspResult.internalError s
            | ResultOrString.Ok (options, lines) ->
                let line = p.Position.Line
                let col = p.Position.Character
                let lineStr = lines.[line]
                let word = lineStr.Substring(0, col)
                let ok = line <= lines.Length && line >= 0 && col <= lineStr.Length + 1 && col >= 0
                if not ok then
                    AsyncLspResult.internalError "not ok"
                elif (lineStr.StartsWith "#" && (KeywordList.hashDirectives.Keys |> Seq.exists (fun k -> k.StartsWith word ) || word.Contains "\n" )) then
                    let completionList = { IsIncomplete = false; Items = KeywordList.hashSymbolCompletionItems }
                    async.Return (success (Some completionList))
                else
                    async {
                        let! tyResOpt =
                            match p.Context with
                            | None -> commands.TryGetRecentTypeCheckResultsForFile(file, options) |> async.Return
                            | Some ctx ->
                                //ctx.triggerKind = CompletionTriggerKind.Invoked ||
                                if  (ctx.triggerCharacter = Some ".") then
                                    commands.TryGetLatestTypeCheckResultsForFile(file)
                                else
                                    commands.TryGetRecentTypeCheckResultsForFile(file, options) |> async.Return

                        match tyResOpt with
                        | None -> return LspResult.internalError "no type check results"
                        | Some tyRes ->
                            let! res = commands.Completion tyRes pos lineStr lines file None (config.KeywordsAutocomplete) (config.ExternalAutocomplete)
                            let res =
                                match res with
                                | CoreResponse.Res(decls, keywords) ->
                                    let items =
                                        decls
                                        |> Array.mapi (fun id d ->
                                            let code =
                                                if System.Text.RegularExpressions.Regex.IsMatch(d.Name, """^[a-zA-Z][a-zA-Z0-9']+$""") then d.Name
                                                elif d.NamespaceToOpen.IsSome then d.Name
                                                else PrettyNaming.QuoteIdentifierIfNeeded d.Name
                                            let label =
                                                match d.NamespaceToOpen with
                                                | Some no -> sprintf "%s (open %s)" d.Name no
                                                | None -> d.Name

                                            { CompletionItem.Create(d.Name) with
                                                Kind = glyphToCompletionKind d.Glyph
                                                InsertText = Some code
                                                SortText = Some (sprintf "%06d" id)
                                                FilterText = Some d.Name
                                                Label = label
                                            }
                                        )
                                    let its = if not keywords then items else Array.append items KeywordList.keywordCompletionItems
                                    let completionList = { IsIncomplete = false; Items = its}
                                    success (Some completionList)
                                | _ -> noCompletion
                            return res
                    }
        return res
    }

    override __.CompletionItemResolve(ci) = async {

        let res = commands.Helptext ci.InsertText.Value
        let res =
            match res with
            | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                ci
            | CoreResponse.Res (HelpText.Simple (name, str)) ->
                let d = Documentation.Markup (markdown str)
                {ci with Detail = Some name; Documentation = Some d  }
            | CoreResponse.Res (HelpText.Full (name, tip, additionalEdit)) ->
                let (si, comment) = (TipFormatter.formatTip tip) |> List.collect id |> List.head
                //TODO: Add insert namespace
                let d = Documentation.Markup (markdown comment)
                {ci with Detail = Some si; Documentation = Some d  }
        return success res
    }

    override x.TextDocumentSignatureHelp(p) =

        p |> x.positionHandlerWithLatest (fun p pos tyRes lineStr lines ->
            async {
                let! res = commands.Methods tyRes  pos lines
                let res =
                    match res with
                    | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                        LspResult.internalError msg
                    | CoreResponse.Res (methods, commas) ->
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



                        success (Some res)
                return res
            }
        )

    override x.TextDocumentHover(p: TextDocumentPositionParams) =

        p |> x.positionHandler (fun p pos tyRes lineStr lines ->
            async {
                let! res = commands.ToolTip tyRes pos lineStr
                let res =
                    match res with
                    | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                        LspResult.internalError msg
                    | CoreResponse.Res(tip, signature, footer, typeDoc) ->
                        match TipFormatter.formatTipEnhanced tip signature footer typeDoc with
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
                                // |> Markdown.createCommentBlock
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
                            success (Some response)
                        | _ -> success None
                return res
            })

    override x.TextDocumentRename(p) =

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
                                            Uri = filePathToUri fileName
                                            Version = commands.TryGetFileVersion fileName
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
                                            Uri = filePathToUri fileName

                                            Version = commands.TryGetFileVersion fileName
                                        }
                                    Edits = edits
                                }
                            )
                        WorkspaceEdit.Create(documentChanges, clientCapabilities.Value) |> Some |> success
                return res
            })

    override x.TextDocumentDefinition(p) =

        p |> x.positionHandler (fun p pos tyRes lineStr lines ->
            async {
                //TODO: Add #load reference
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

        p |> x.positionHandler (fun p pos tyRes lineStr lines ->
            async {
                let! res = commands.FindTypeDeclaration tyRes pos lineStr
                let res =
                    match res with
                    | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                        LspResult.internalError msg
                    | CoreResponse.Res r ->
                        fcsRangeToLspLocation r
                        |> GotoResult.Single
                        |> Some
                        |> success
                return res
            })

    override x.TextDocumentReferences(p) =

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

        p |> x.positionHandler (fun p pos tyRes lineStr lines ->
            async {
                let! res = commands.SymbolUse tyRes pos lineStr
                let res =
                    match res with
                    | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                        LspResult.internalError msg
                    | CoreResponse.Res (symbol, uses) ->
                        uses
                        |> Array.map (fun s ->
                        {
                            DocumentHighlight.Range = fcsRangeToLsp s.RangeAlternate
                            Kind = None
                        })
                        |> Some
                        |> success
                return res
            })

    override x.TextDocumentImplementation(p) =

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

        let fn = p.TextDocument.GetFilePath()
        let! res = commands.Declarations fn None (commands.TryGetFileVersion fn)
        let res =
            match res with
            | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                LspResult.internalError msg
            | CoreResponse.Res decls ->
                decls
                |> Array.map (fst >> getSymbolInformations p.TextDocument.Uri glyphToSymbolKind)
                |> Seq.collect id
                |> Seq.toArray
                |> Some
                |> success
        return res
    }

    override __.WorkspaceSymbol(p) = async {

        let! res = commands.DeclarationsInProjects ()
        let res =
            match res with
            | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                LspResult.internalError msg
            | CoreResponse.Res (decls) ->
                decls
                |> Array.map (fun (n,p) ->
                    let uri = filePathToUri p
                    getSymbolInformations uri glyphToSymbolKind n)
                |> Seq.collect id
                |> Seq.filter(fun symbolInfo -> symbolInfo.Name.StartsWith(p.Query))
                |> Seq.toArray
                |> Some
                |> success
        return res
    }

    override __.TextDocumentFormatting(p: DocumentFormattingParams) = async {
        let doc = p.TextDocument
        let fileName = doc.GetFilePath()
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

    member private __.IfDiagnostic (str: string) handler p =
        let diag =
            p.Context.Diagnostics |> Seq.tryFind (fun n -> n.Message.Contains str)
        match diag with
        | None -> async.Return []
        | Some d -> handler d

    member private __.IfDiagnosticType (str: string) handler p =
        let diag =
            p.Context.Diagnostics |> Seq.tryFind (fun n -> n.Source.Contains str)
        match diag with
        | None -> async.Return []
        | Some d -> handler d

    member private __.CreateFix uri fn title (d: Diagnostic option) range replacement  =
        let e =
            {
                Range = range
                NewText = replacement
            }
        let edit =
            {
                TextDocument =
                    {
                        Uri = uri
                        Version = commands.TryGetFileVersion fn
                    }
                Edits = [|e|]
            }
        let we = WorkspaceEdit.Create([|edit|], clientCapabilities.Value)


        { CodeAction.Title = title
          Kind = Some "quickfix"
          Diagnostics = d |> Option.map Array.singleton
          Edit = we
          Command = None}

    member private x.GetUnusedOpensCodeActions fn p =
        if config.UnusedOpensAnalyzer then
            p |> x.IfDiagnostic "Unused open statement" (fun d ->
                let range = {
                    Start = {Line = d.Range.Start.Line - 1; Character = 1000}
                    End = {Line = d.Range.End.Line; Character = d.Range.End.Character}
                }
                let action = x.CreateFix p.TextDocument.Uri fn "Remove unused open" (Some d) range ""
                async.Return [action])
        else
            async.Return []

    member private x.GetErrorSuggestionsCodeActions fn p =
        p |> x.IfDiagnostic "Maybe you want one of the following:" (fun d ->
            d.Message.Split('\n').[1..]
            |> Array.map (fun suggestion ->
                let s = suggestion.Trim()
                let s =
                    if System.Text.RegularExpressions.Regex.IsMatch(s, """^[a-zA-Z][a-zA-Z0-9']+$""") then
                        s
                    else
                        "``" + s + "``"
                let title = sprintf "Replace with %s" s
                let action = x.CreateFix p.TextDocument.Uri fn title (Some d) d.Range s
                action)
            |> Array.toList
            |> async.Return
        )

    member private x.GetNewKeywordSuggestionCodeAction fn p lines =
        p |> x.IfDiagnostic "It is recommended that objects supporting the IDisposable interface are created using the syntax" (fun d ->
            let s = "new " + getText lines d.Range
            x.CreateFix p.TextDocument.Uri fn "Add new" (Some d) d.Range s
            |> List.singleton
            |> async.Return
        )

    member private x.GetUnusedCodeAction fn p lines =
        p |> x.IfDiagnostic "is unused" (fun d ->
            match d.Code with
            | None ->
                let s = "_"
                let s2 = "_" + getText lines d.Range
                [
                    x.CreateFix p.TextDocument.Uri fn "Replace with _" (Some d) d.Range s
                    x.CreateFix p.TextDocument.Uri fn "Prefix with _" (Some d) d.Range s2
                ] |> async.Return
            | Some _ ->
                [
                    x.CreateFix p.TextDocument.Uri fn "Replace with __" (Some d) d.Range "__"
                ] |> async.Return

        )

    member private x.GetRedundantQualfierCodeAction fn p =
        p |> x.IfDiagnostic "This qualifier is redundant" (fun d ->
            [
                x.CreateFix p.TextDocument.Uri fn "Remove redundant qualifier" (Some d) d.Range ""
            ] |> async.Return
        )

    member private x.GetLinterCodeAction fn p =
        p |> x.IfDiagnostic "Lint:" (fun d ->
            let uri = filePathToUri fn

            match fixes.TryGetValue uri with
            | false, _ -> async.Return []
            | true, lst ->
                match lst |> Seq.tryFind (fun (r, te) -> r = d.Range) with
                | None -> async.Return []
                | Some (r, te) ->
                    x.CreateFix p.TextDocument.Uri fn (sprintf "Replace with %s" te.NewText) (Some d) te.Range te.NewText
                    |> List.singleton
                    |> async.Return
        )

    member private x.GetAnalyzerCodeAction fn p =
        p |> x.IfDiagnosticType "F# Analyzers" (fun d ->
            let uri = filePathToUri fn

            let res =
                analyzerFixes
                |> Seq.map (|KeyValue|)
                |> Seq.tryPick (fun ((u, _), lst) ->
                    if u = uri then Some lst else None
                )

            match res with
            | None -> async.Return []
            | Some lst ->
                lst
                |> List.filter (fun (r, te) -> r = d.Range)
                |> List.map (fun (r,te) ->
                    x.CreateFix p.TextDocument.Uri fn (sprintf "Replace with %s" te.NewText) (Some d) te.Range te.NewText
                )
                |> async.Return
        )

    member private x.GetUnionCaseGeneratorCodeAction fn p (lines: string[]) =
        p |> x.IfDiagnostic "Incomplete pattern matches on this expression. For example" (fun d ->
            async {
                if config.UnionCaseStubGeneration then
                    let caseLine = d.Range.Start.Line + 1
                    let col = lines.[caseLine].IndexOf('|') + 3 // Find column of first case in patern matching
                    let pos = FcsRange.mkPos (caseLine + 1) (col + 1) //Must points on first case in 1-based system
                    let! res = x.HandleTypeCheckCodeAction fn pos (fun tyRes line lines -> commands.GetUnionPatternMatchCases tyRes pos lines line)
                    let res =
                        match res.[0] with
                        | CoreResponse.Res (text, position) ->
                            let range = {
                                Start = fcsPosToLsp position
                                End = fcsPosToLsp position
                            }
                            let text = text.Replace("$1", config.UnionCaseStubGenerationBody)
                            [x.CreateFix p.TextDocument.Uri fn "Generate union pattern match case" (Some d) range text ]
                        | _ ->
                            []
                    return res
                else
                    return []
            }
        )

    member private x.GetInterfaceStubCodeAction fn (p: CodeActionParams) (lines: string[]) =
        async {
            if config.InterfaceStubGeneration then
                let pos = protocolPosToPos p.Range.Start
                let! res = x.HandleTypeCheckCodeAction fn pos (fun tyRes line lines -> commands.GetInterfaceStub tyRes pos lines line)
                let res =
                    match res with
                    | CoreResponse.Res (text, position)::_ ->
                        let range = {
                            Start = fcsPosToLsp position
                            End = fcsPosToLsp position
                        }
                        let text =
                            text.Replace("$objectIdent", config.InterfaceStubGenerationObjectIdentifier)
                                .Replace("$methodBody", config.InterfaceStubGenerationMethodBody)
                        [x.CreateFix p.TextDocument.Uri fn "Generate interface stubs" None range text ]
                    | _ ->
                        []
                return res
            else
                return []
        }

    member private x.GetRecordStubCodeAction fn (p: CodeActionParams) (lines: string[]) =
        async {
            if config.RecordStubGeneration then
                let pos = protocolPosToPos p.Range.Start
                let! res = x.HandleTypeCheckCodeAction fn pos (fun tyRes line lines -> commands.GetRecordStub tyRes pos lines line)
                let res =
                    match res with
                    | CoreResponse.Res (text, position)::_ ->
                        let range = {
                            Start = fcsPosToLsp position
                            End = fcsPosToLsp position
                        }
                        let text = text.Replace("$1", config.RecordStubGenerationBody)
                        [x.CreateFix p.TextDocument.Uri fn "Generate record stubs" None range text ]
                    | _ ->
                        []
                return res
            else
                return []
        }

    member private x.GetResolveNamespaceActions fn (p: CodeActionParams) =
        let insertLine line lineStr =
            {
                Range = {
                    Start = {Line = line; Character = 0}
                    End = {Line = line; Character = 0}
                }
                NewText = lineStr
            }


        let adjustInsertionPoint (lines: string[]) (ctx : InsertContext) =
            let l = ctx.Pos.Line
            match ctx.ScopeKind with
            | TopModule when l > 1 ->
                let line = lines.[l - 2]
                let isImpliciteTopLevelModule = not (line.StartsWith "module" && not (line.EndsWith "="))
                if isImpliciteTopLevelModule then 1 else l
            | TopModule -> 1
            | ScopeKind.Namespace when l > 1 ->
                [0..l - 1]
                |> List.mapi (fun i line -> i, lines.[line])
                |> List.tryPick (fun (i, lineStr) ->
                    if lineStr.StartsWith "namespace" then Some i
                    else None)
                |> function
                    // move to the next line below "namespace" and convert it to F# 1-based line number
                    | Some line -> line + 2
                    | None -> l
            | ScopeKind.Namespace -> 1
            | _ -> l

        if config.ResolveNamespaces then
            p |> x.IfDiagnostic "is not defined" (fun d ->
                async {
                    let pos = protocolPosToPos d.Range.Start
                    return!
                        x.HandleTypeCheckCodeAction fn pos (fun tyRes line lines ->
                            async {
                                let! res = commands.GetNamespaceSuggestions tyRes pos line
                                let res =
                                    match res with
                                    | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                                        []
                                    | CoreResponse.Res (word, opens, qualifiers) ->
                                        let quals =
                                            qualifiers
                                            |> List.map (fun (name, qual) ->
                                                let e =
                                                    {
                                                        Range = d.Range
                                                        NewText = qual
                                                    }
                                                let edit =
                                                    {
                                                        TextDocument =
                                                            {
                                                                Uri = p.TextDocument.Uri
                                                                Version = commands.TryGetFileVersion fn
                                                            }
                                                        Edits = [|e|]
                                                    }
                                                let we = WorkspaceEdit.Create([|edit|], clientCapabilities.Value)


                                                { CodeAction.Title = sprintf "Use %s" qual
                                                  Kind = Some "quickfix"
                                                  Diagnostics = Some [| d |]
                                                  Edit = we
                                                  Command = None}
                                            )
                                        let ops =
                                            opens
                                            |> List.map (fun (ns, name, ctx, multiple) ->
                                                let insertPoint = adjustInsertionPoint lines ctx
                                                let docLine = insertPoint - 1
                                                let s =
                                                    if name.EndsWith word && name <> word then
                                                        let prefix = name.Substring(0, name.Length - word.Length).TrimEnd('.')
                                                        ns + "." + prefix
                                                    else ns



                                                let lineStr = (String.replicate ctx.Pos.Column " ") + "open " + s + "\n"
                                                let edits =
                                                    [|
                                                        yield insertLine docLine lineStr
                                                        if lines.[docLine + 1].Trim() <> "" then yield insertLine (docLine + 1) ""
                                                        if (ctx.Pos.Column = 0 || ctx.ScopeKind = Namespace) && docLine > 0 && not ((lines.[docLine - 1]).StartsWith "open" ) then
                                                            yield insertLine (docLine - 1) ""
                                                    |]
                                                let edit =
                                                    {
                                                        TextDocument =
                                                            {
                                                                Uri = p.TextDocument.Uri
                                                                Version = commands.TryGetFileVersion fn
                                                            }
                                                        Edits = edits
                                                    }
                                                let we = WorkspaceEdit.Create([|edit|], clientCapabilities.Value)


                                                { CodeAction.Title = sprintf "Open %s" s
                                                  Kind = Some "quickfix"
                                                  Diagnostics = Some [| d |]
                                                  Edit = we
                                                  Command = None}

                                            )
                                        [yield! ops; yield! quals; ]
                                return res
                            }
                        )
                })
        else
            async.Return []

    override x.TextDocumentCodeAction(p) =

        let fn = p.TextDocument.GetFilePath()
        match commands.TryGetFileCheckerOptionsWithLines fn with
        | ResultOrString.Error s ->
            AsyncLspResult.internalError s
        | ResultOrString.Ok (opts, lines) ->
        async {
            let! unusedOpensActions = x.GetUnusedOpensCodeActions fn p
            let! resolveNamespaceActions = x.GetResolveNamespaceActions fn p
            let! errorSuggestionActions = x.GetErrorSuggestionsCodeActions fn p
            let! unusedActions = x.GetUnusedCodeAction fn p lines
            let! redundantActions = x.GetRedundantQualfierCodeAction fn p
            let! newKeywordAction = x.GetNewKeywordSuggestionCodeAction fn p lines
            let! duCaseActions = x.GetUnionCaseGeneratorCodeAction fn p lines
            let! linterActions = x.GetLinterCodeAction fn p
            let! analyzerActions = x.GetAnalyzerCodeAction fn p

            let! interfaceGenerator = x.GetInterfaceStubCodeAction fn p lines
            let! recordGenerator = x.GetRecordStubCodeAction fn p lines


            let res =
                [|
                    yield! unusedOpensActions
                    yield! (List.concat resolveNamespaceActions)
                    yield! errorSuggestionActions
                    yield! unusedActions
                    yield! newKeywordAction
                    yield! duCaseActions
                    yield! linterActions
                    yield! analyzerActions
                    yield! interfaceGenerator
                    yield! recordGenerator
                    yield! redundantActions
                |]


            return res |> TextDocumentCodeActionResult.CodeActions |> Some |> success
        }

    override __.TextDocumentCodeLens(p) = async {

        let fn = p.TextDocument.GetFilePath()
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

        let handler f (arg: CodeLens) =
            async {
                let pos = FcsRange.mkPos (arg.Range.Start.Line + 1) (arg.Range.Start.Character + 2)
                let data = arg.Data.Value.ToObject<string[]>()
                let file = fileUriToLocalPath data.[0]
                logger.info (Log.setMessage "CodeLensResolve - Position request for {file} at {pos}" >> Log.addContextDestructured "file" file >> Log.addContextDestructured "pos" pos)
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
                    let! res = commands.SignatureData tyRes pos lineStr
                    let res =
                        match res with
                        | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                            logger.error (Log.setMessage "CodeLensResolve - error on file {file}" >> Log.addContextDestructured "file" file >> Log.addContextDestructured "error" msg)
                            let cmd = { Title = ""; Command = None; Arguments = None }
                            {p with Command = Some cmd} |> success
                        | CoreResponse.Res (typ, parms, _) ->
                            let formatted = SigantureData.formatSignature typ parms
                            let cmd = { Title = formatted; Command = None; Arguments = None }
                            { p with Command = Some cmd } |> success
                    return res
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
                                JToken.FromObject (filePathToUri file)
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
                                JToken.FromObject (filePathToUri file)
                                JToken.FromObject (fcsPosToLsp pos)
                                JToken.FromObject locs
                            |]

                            let cmd = {Title = formatted; Command = Some "fsharp.showReferences"; Arguments = Some args}
                            {p with Command = Some cmd} |> success
                    return res
            }
        ) p

    override __.WorkspaceDidChangeWatchedFiles(p) = async {

        p.Changes
        |> Array.iter (fun c ->
            if c.Type = FileChangeType.Deleted then
                let uri = c.Uri
                diagnosticCollections.AddOrUpdate((uri, "F# Compiler"), [||], fun _ _ -> [||]) |> ignore
                diagnosticCollections.AddOrUpdate((uri, "F# Unused opens"), [||], fun _ _ -> [||]) |> ignore
                diagnosticCollections.AddOrUpdate((uri, "F# Unused declarations"), [||], fun _ _ -> [||]) |> ignore
                diagnosticCollections.AddOrUpdate((uri, "F# simplify names"), [||], fun _ _ -> [||]) |> ignore
                diagnosticCollections.AddOrUpdate((uri, "F# Linter"), [||], fun _ _ -> [||]) |> ignore
            ()
        )

        return ()
    }

    override __.WorkspaceDidChangeConfiguration(p: DidChangeConfigurationParams) = async {
        let dto =
            p.Settings
            |> Server.deserialize<FSharpConfigRequest>
        let c = config.AddDto dto.FSharp
        updateConfig c
        logger.info (Log.setMessage "Workspace configuration changed" >> Log.addContextDestructured "config" c)
        return ()
    }

    override __.TextDocumentFoldingRange(rangeP: FoldingRangeParams) = async {

        let file = rangeP.TextDocument.GetFilePath()
        match! commands.ScopesForFile file with
        | Ok scopes ->
            let ranges = scopes |> Seq.map toFoldingRange |> Set.ofSeq |> List.ofSeq
            return LspResult.success (Some ranges)
        | Result.Error error ->
            return LspResult.internalError error
    }

    member x.FSharpSignature(p: TextDocumentPositionParams) =

        p |> x.positionHandler (fun p pos tyRes lineStr lines ->
            async {
                let! res = commands.Typesig tyRes pos lineStr
                let res =
                    match res with
                    | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                        LspResult.internalError msg
                    | CoreResponse.Res tip ->
                        { Content =  CommandResponse.typeSig FsAutoComplete.JsonSerializer.writeJson tip }
                        |> success

                return res
            }
        )

    member x.FSharpSignatureData(p: TextDocumentPositionParams) =

        let handler f (arg: TextDocumentPositionParams) =
            async {
                let pos = FcsRange.mkPos (p.Position.Line) (p.Position.Character + 2)
                let file = IO.Path.GetFullPath (p.TextDocument.Uri)
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
            async {
                let! res = commands.SignatureData tyRes pos lineStr
                let res =
                    match res with
                    | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                        LspResult.internalError msg
                    | CoreResponse.Res (typ, parms, generics) ->
                        { Content =  CommandResponse.signatureData FsAutoComplete.JsonSerializer.writeJson (typ, parms, generics) }
                        |> success

                return res
            }
        )

    member x.FSharpDocumentationGenerator(p: TextDocumentPositionParams) =

        p |> x.positionHandler (fun p pos tyRes lineStr lines ->
            async {
                let! res = commands.SignatureData tyRes pos lineStr
                let res =
                    match res with
                    | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                        LspResult.internalError msg
                    | CoreResponse.Res (typ, parms, generics) ->
                        { Content =  CommandResponse.signatureData FsAutoComplete.JsonSerializer.writeJson (typ, parms, generics) }
                        |> success

                return res
            }
        )

    member __.FSharpLineLense(p) = async {

        let fn = p.Project.GetFilePath()
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

        p |> x.positionHandler (fun p pos tyRes lineStr lines ->
            async {
                let! res = commands.SignatureData tyRes pos lineStr
                let res =
                    match res with
                    | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                        LspResult.internalError msg
                    | CoreResponse.Res(typ, parms, generics) ->
                        { Content =  CommandResponse.signatureData FsAutoComplete.JsonSerializer.writeJson (typ, parms, generics) }
                        |> success

                return res
            }
        )

    member __.FSharpCompilerLocation(p) = async {

        let res = commands.CompilerLocation ()
        let res =
            match res with
            | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                LspResult.internalError msg
            | CoreResponse.Res (fsc, fsi, msbuild, sdk) ->
                { Content =  CommandResponse.compilerLocation FsAutoComplete.JsonSerializer.writeJson fsc fsi msbuild sdk}
                |> success

        return res
    }

    member __.FSharpCompile(p) = async {

        let fn = p.Project.GetFilePath()
        let! res = commands.Compile fn
        let res =
            match res with
            | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                LspResult.internalError msg
            | CoreResponse.Res(ers, code) ->
                { Content =  CommandResponse.compile FsAutoComplete.JsonSerializer.writeJson (ers, code) }
                |> success

        return res
    }

    member __.FSharpWorkspaceLoad(p) = async {

        let fns = p.TextDocuments |> Array.map (fun fn -> fn.GetFilePath() ) |> Array.toList
        let! res = commands.WorkspaceLoad ignore fns config.DisableInMemoryProjectReferences config.ScriptTFM
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

        let fn = p.Project.GetFilePath()
        let! res = commands.Project fn ignore config.ScriptTFM
        let res =
            match res with
            | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                LspResult.internalError msg
            | CoreResponse.Res (ProjectResponse.Project proj) ->
                { Content =  CommandResponse.project FsAutoComplete.JsonSerializer.writeJson proj }
                |> success
            | CoreResponse.Res (ProjectResponse.ProjectError er) ->
                { Content =  CommandResponse.projectError FsAutoComplete.JsonSerializer.writeJson er }
                |> success
            | _ -> LspResult.notImplemented
        return res
    }

    member __.FSharpFsdn(p: FsdnRequest) = async {

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

    member x.FSharpHelp(p: TextDocumentPositionParams) =

        p |> x.positionHandler (fun p pos tyRes lineStr lines ->
            async {
                let! res = commands.Help tyRes pos lineStr
                let res =
                    match res with
                    | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                        LspResult.internalError msg
                    | CoreResponse.Res(t) ->
                        { Content =  CommandResponse.help FsAutoComplete.JsonSerializer.writeJson t }
                        |> success

                return res
            }
        )

    member x.FSharpDocumentation(p: TextDocumentPositionParams) =

        p |> x.positionHandler (fun p pos tyRes lineStr lines ->
            async {
                let! res = commands.FormattedDocumentation tyRes pos lineStr
                let res =
                    match res with
                    | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                        LspResult.internalError msg
                    | CoreResponse.Res (tip, xml, signature, footer, cm) ->
                        { Content =  CommandResponse.formattedDocumentation FsAutoComplete.JsonSerializer.writeJson (tip, xml, signature, footer, cm) }
                        |> success

                return res
            }
        )


    member x.FSharpDocumentationSymbol(p: DocumentationForSymbolReuqest) =
        match commands.LastCheckResult with
        | None -> AsyncLspResult.internalError "error"
        | Some tyRes ->
            async {
                let! res = commands.FormattedDocumentationForSymbol tyRes p.XmlSig p.Assembly
                let res =
                    match res with
                    | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                        LspResult.internalError msg
                    | CoreResponse.Res (xml, assembly, doc, signature, footer, cn) ->
                        { Content = CommandResponse.formattedDocumentationForSymbol FsAutoComplete.JsonSerializer.writeJson xml assembly doc (signature, footer, cn) }
                        |> success

                return res
            }

    member __.FakeTargets(p:FakeTargetsRequest) = async {
        let! res = commands.FakeTargets (p.FileName) (p.FakeContext)
        let res =
            match res with
            | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                LspResult.internalError msg
            | CoreResponse.Res (targets) ->
                { Content = CommandResponse.fakeTargets FsAutoComplete.JsonSerializer.writeJson targets }
                |> success

        return res
    }

    member __.FakeRuntimePath(p) = async {
        let! res = commands.FakeRuntime ()
        let res =
            match res with
            | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                LspResult.internalError msg
            | CoreResponse.Res (runtimePath) ->
                { Content = CommandResponse.fakeRuntime FsAutoComplete.JsonSerializer.writeJson runtimePath }
                |> success
        return res
    }

    member __.LoadAnalyzers(path) = async {
#if ANALYZER_SUPPORT
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
#else
        return LspResult.success ()
#endif
    }

let startCore (commands: Commands) =
    use input = Console.OpenStandardInput()
    use output = Console.OpenStandardOutput()

    let requestsHandlings =
        defaultRequestHandlings<FsharpLspServer> ()
        |> Map.add "fsharp/signature" (requestHandling (fun s p -> s.FSharpSignature(p) ))
        |> Map.add "fsharp/signatureData" (requestHandling (fun s p -> s.FSharpSignatureData(p) ))
        |> Map.add "fsharp/documentationGenerator" (requestHandling (fun s p -> s.FSharpDocumentationGenerator(p) ))
        |> Map.add "fsharp/lineLens" (requestHandling (fun s p -> s.FSharpLineLense(p) ))
        |> Map.add "fsharp/compilerLocation" (requestHandling (fun s p -> s.FSharpCompilerLocation(p) ))
        |> Map.add "fsharp/compile" (requestHandling (fun s p -> s.FSharpCompile(p) ))
        |> Map.add "fsharp/workspaceLoad" (requestHandling (fun s p -> s.FSharpWorkspaceLoad(p) ))
        |> Map.add "fsharp/workspacePeek" (requestHandling (fun s p -> s.FSharpWorkspacePeek(p) ))
        |> Map.add "fsharp/project" (requestHandling (fun s p -> s.FSharpProject(p) ))
        |> Map.add "fsharp/fsdn" (requestHandling (fun s p -> s.FSharpFsdn(p) ))
        |> Map.add "fsharp/dotnetnewlist" (requestHandling (fun s p -> s.FSharpDotnetNewList(p) ))
        |> Map.add "fsharp/dotnetnewrun" (requestHandling (fun s p -> s.FSharpDotnetNewRun(p) ))
        |> Map.add "fsharp/f1Help" (requestHandling (fun s p -> s.FSharpHelp(p) ))
        |> Map.add "fsharp/documentation" (requestHandling (fun s p -> s.FSharpDocumentation(p) ))
        |> Map.add "fsharp/documentationSymbol" (requestHandling (fun s p -> s.FSharpDocumentationSymbol(p) ))
        |> Map.add "fsharp/loadAnalyzers" (requestHandling (fun s p -> s.LoadAnalyzers(p) ))
        |> Map.add "fake/listTargets" (requestHandling (fun s p -> s.FakeTargets(p) ))
        |> Map.add "fake/runtimePath" (requestHandling (fun s p -> s.FakeRuntimePath(p) ))



    LanguageServerProtocol.Server.start requestsHandlings input output FSharpLspClient (fun lspClient -> FsharpLspServer(commands, lspClient))

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
