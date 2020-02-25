namespace FsAutoComplete

open System
open System.IO
open FSharp.Compiler.SourceCodeServices
open FsAutoComplete.Logging
open FsAutoComplete.UnionPatternMatchCaseGenerator
open FsAutoComplete.RecordStubGenerator
open FsAutoComplete.InterfaceStubGenerator
open System.Threading
open Utils
open FSharp.Compiler.Range
open ProjectSystem



[<RequireQualifiedAccess>]
type LocationResponse<'a,'b> =
    | Use of 'a
    | UseRange of 'b

[<RequireQualifiedAccess>]
type HelpText =
    | Simple of symbol: string * text: string
    | Full of symbol: string * tip: FSharpToolTipText * moreInfo: option<string * int * int * string>


[<RequireQualifiedAccess>]
type CoreResponse<'a> =
    | InfoRes of text: string
    | ErrorRes of text: string
    | Res of 'a

[<RequireQualifiedAccess>]
type NotificationEvent =
    | ParseError of errors: FSharpErrorInfo[] * file: string
    | Workspace of ProjectSystem.ProjectResponse
    | AnalyzerMessage of  messages: obj * file: string
    | UnusedOpens of file: string * opens: range[]
    | Lint of file: string * warningsWithCodes: Lint.EnrichedLintWarning list
    | UnusedDeclarations of file: string * decls: (range * bool)[]
    | SimplifyNames of file: string * names: SimplifyNames.SimplifiableRange []
    | Canceled of string
    | Diagnostics of LanguageServerProtocol.Types.PublishDiagnosticsParams
    | FileParsed of string

type Commands (serialize : Serializer, backgroundServiceEnabled) =
    let checker = FSharpCompilerServiceChecker(backgroundServiceEnabled)
    let state = State.Initial (checker.GetFSharpChecker())
    let fileParsed = Event<FSharpParseFileResults>()
    let fileChecked = Event<ParseAndCheckResults * string * int>()

    let mutable lastVersionChecked = -1
    let mutable lastCheckResult : ParseAndCheckResults option = None
    let mutable analyzerHandler : ((string * string [] * FSharp.Compiler.Ast.ParsedInput * FSharpImplementationFileContents * FSharpEntity list * (bool -> AssemblySymbol list)) -> obj) option = None

    let notify = Event<NotificationEvent>()

    let fileStateSet = Event<unit>()
    let commandsLogger = LogProvider.getLoggerByName "Commands"
    let checkerLogger = LogProvider.getLoggerByName "CheckerEvents"
    let fantomasLogger = LogProvider.getLoggerByName "Fantomas"

    do state.ProjectController.NotifyWorkspace.Add (NotificationEvent.Workspace >> notify.Trigger)

    do BackgroundServices.messageRecived.Publish.Add (fun n ->
       match n with
       | BackgroundServices.Diagnostics d -> notify.Trigger (NotificationEvent.Diagnostics d)
    )

    do fileParsed.Publish.Add (fun parseRes ->
        let decls = parseRes.GetNavigationItems().Declarations
        state.NavigationDeclarations.[parseRes.FileName] <- decls
        state.ParseResults.[parseRes.FileName] <- parseRes
    )

    do checker.ScriptTypecheckRequirementsChanged.Add (fun () ->
        checkerLogger.debug (Log.setMessage "Script typecheck dependencies changed, purging expired script options")
        let mutable count = 0
        state.FSharpProjectOptions
        |> Seq.choose (function | (path, _) when path.EndsWith ".fsx" -> Some path
                                | _ -> None)
        |> Seq.iter (fun path ->
            state.RemoveProjectOptions path
            count <- count + 1
        )
        checkerLogger.debug (Log.setMessage "Script typecheck dependencies changed, purged {optionCount} expired script options" >> Log.addContextDestructured "optionCount" count)
    )

    do if not backgroundServiceEnabled then
            checker.FileChecked.Add (fun (n,_) ->
                checkerLogger.info (Log.setMessage "{file} checked" >> Log.addContextDestructured "file" n)
                async {
                    try
                        match state.GetProjectOptions n with
                        | Some opts ->
                            let! res = checker.GetBackgroundCheckResultsForFileInProject(n, opts)
                            fileChecked.Trigger (res, res.FileName, -1)
                        | _ -> ()
                    with
                    | _ -> ()
                } |> Async.Start
            )

    //Triggered by `FSharpChecker.FileChecked` if background service is disabled; and by `Parse` command
    do fileChecked.Publish.Add (fun (parseAndCheck, file, version) ->
        async {
            try
                NotificationEvent.FileParsed file
                |> notify.Trigger

                let checkErrors = parseAndCheck.GetParseResults.Errors
                let parseErrors = parseAndCheck.GetCheckResults.Errors
                let errors =
                    Array.append checkErrors parseErrors
                    |> Array.distinctBy (fun e -> e.Severity, e.ErrorNumber, e.StartLineAlternate, e.StartColumn, e.EndLineAlternate, e.EndColumn, e.Message)
                (errors, file)
                |> NotificationEvent.ParseError
                |> notify.Trigger
            with
            | _ -> ()
        }
        |> Async.Start

        async {
            try
              match analyzerHandler with
              | None -> ()
              | Some handler ->

                Loggers.analyzers.info (Log.setMessage "begin analysis of {file}" >> Log.addContextDestructured "file" file)
                match parseAndCheck.GetParseResults.ParseTree, parseAndCheck.GetCheckResults.ImplementationFile with
                | Some pt, Some tast ->
                    let res = handler (file, state.Files.[file].Lines, pt, tast, parseAndCheck.GetCheckResults.PartialAssemblySignature.Entities |> Seq.toList, parseAndCheck.GetAllEntities)
                    (res, file)
                    |> NotificationEvent.AnalyzerMessage
                    |> notify.Trigger
                | _ -> ()
            with
            | ex ->
                Loggers.analyzers.error (Log.setMessage "Run failed for {file}" >> Log.addContextDestructured "file" file >> Log.addExn ex)
        } |> Async.Start
    )

    let parseFilesInTheBackground fsiScriptTFM files =
        async {
            files
            |> List.toArray
            |> Array.Parallel.iter (fun file ->
                try
                    let sourceOpt =
                        match state.Files.TryFind file with
                        | Some f -> Some (f.Lines)
                        | None when File.Exists(file) ->
                            let ctn = File.ReadAllLines file
                            state.Files.[file] <- { Touched = DateTime.Now; Lines = ctn; Version = None }
                            let payload =
                                if Utils.isAScript file
                                then BackgroundServices.ScriptFile(file, fsiScriptTFM)
                                else BackgroundServices.SourceFile file
                            if backgroundServiceEnabled then BackgroundServices.updateFile(payload, ctn |> String.concat "\n", 0)
                            Some (ctn)
                        | None -> None
                    match sourceOpt with
                    | None -> ()
                    | Some source ->
                        let opts = state.GetProjectOptions' file |> Utils.projectOptionsToParseOptions
                        let parseRes = checker.ParseFile(file, source |> String.concat "\n", opts) |> Async.RunSynchronously
                        fileParsed.Trigger parseRes
                with
                | :? System.Threading.ThreadAbortException as ex ->
                    // on mono, if background parsing is aborted a ThreadAbortException
                    // is raised, who can be ignored
                    ()
                | ex ->
                    commandsLogger.error (Log.setMessage "Failed to parse file '{file}'" >> Log.addContextDestructured "file" file >> Log.addExn ex)
            ) }

    let calculateNamespaceInser (decl : FSharpDeclarationListItem) (pos : pos) getLine =
        let getLine i =
            try
                getLine i
            with
            | _ -> ""
        let idents = decl.FullName.Split '.'
        decl.NamespaceToOpen
        |> Option.bind (fun n ->
            state.CurrentAST
            |> Option.map (fun ast -> ParsedInput.findNearestPointToInsertOpenDeclaration (pos.Line) ast idents TopLevel )
            |> Option.map (fun ic -> n, ic.Pos.Line, ic.Pos.Column, ic.ScopeKind.ToString()))

    let fillHelpTextInTheBackground decls (pos : pos) fn getLine =
        let declName (d: FSharpDeclarationListItem) = d.Name

        //Fill list of declarations synchronously to know which declarations should be in cache.
        for d in decls do
            state.Declarations.[declName d] <- (d, pos, fn)

        //Fill namespace insertion cache asynchronously.
        async {
            for decl in decls do
                let n = declName decl
                let insert = calculateNamespaceInser decl pos getLine
                if insert.IsSome then state.CompletionNamespaceInsert.[n] <- insert.Value
        } |> Async.Start

    let onProjectLoaded projectFileName (response: ProjectCrackerCache) tfmForScripts =
        if backgroundServiceEnabled then
            BackgroundServices.updateProject(projectFileName, response.Options)

        response.Items
        |> List.choose (function Dotnet.ProjInfo.Workspace.ProjectViewerItem.Compile(p, _) -> Some p)
        |> parseFilesInTheBackground tfmForScripts
        |> Async.Start

    member __.Notify = notify.Publish

    member __.WorkspaceReady = state.ProjectController.WorkspaceReady

    member __.FileChecked = fileChecked.Publish


    member __.IsWorkspaceReady
        with get() = state.ProjectController.IsWorkspaceReady

    member __.LastVersionChecked
        with get() = lastVersionChecked

    member __.AnalyzerHandler
        with get() = analyzerHandler
        and  set v = analyzerHandler <- v

    member __.LastCheckResult
        with get() = lastCheckResult

    member __.SetFileContent(file: SourceFilePath, lines: LineStr[], version, tfmIfScript) =
        state.AddFileText(file, lines, version)
        let payload =
            if Utils.isAScript file
            then BackgroundServices.ScriptFile(file, tfmIfScript)
            else BackgroundServices.SourceFile file

        if backgroundServiceEnabled then BackgroundServices.updateFile(payload, lines |> String.concat "\n", defaultArg version 0)

    member private x.MapResultAsync (successToString: 'a -> Async<CoreResponse<'b>>, ?failureToString: string -> CoreResponse<'b>) =
        Async.bind <| function
            // A failure is only info here, as this command is expected to be
            // used 'on idle', and frequent errors are expected.
            | ResultOrString.Error e -> async.Return ((defaultArg failureToString CoreResponse.InfoRes) e)
            | ResultOrString.Ok r -> successToString r

    member private x.MapResult (successToString: 'a -> CoreResponse<'b>, ?failureToString: string -> CoreResponse<'b>) =
        x.MapResultAsync ((fun x -> successToString x |> async.Return), ?failureToString = failureToString)

    member x.Fsdn (querystr) = async {
            let results = Fsdn.query querystr
            return CoreResponse.Res results
        }

    member x.DotnetNewList () = async {
            let results = DotnetNewTemplate.installedTemplates ()
            return CoreResponse.Res results
        }


    member x.DotnetNewRun (templateShortName : string) (name: string option) (output: string option) (parameterStr : (string * obj) list) = async {
            let! results = DotnetNewTemplate.dotnetnewCreateCli templateShortName name output parameterStr
            return CoreResponse.Res results
        }

    member private x.AsCancellable (filename : SourceFilePath) (action : Async<CoreResponse<'b>>) =
        let cts = new CancellationTokenSource()
        state.AddCancellationToken(filename, cts)
        Async.StartCatchCancellation(action, cts.Token)
        |> Async.Catch
        |> Async.map (function
            | Choice1Of2 res -> res
            | Choice2Of2 err ->
                let cld = CoreResponse.InfoRes (sprintf "Request cancelled (exn was %A)" err)
                notify.Trigger (NotificationEvent.Canceled (sprintf "Request cancelled (exn was %A)" err))
                cld)

    member private x.CancelQueue (filename : SourceFilePath) =
        let filename = Path.GetFullPath filename
        state.GetCancellationTokens filename |> List.iter (fun cts -> cts.Cancel() )

    member x.TryGetRecentTypeCheckResultsForFile(file, opts) =
        let file = Path.GetFullPath file
        checker.TryGetRecentCheckResultsForFile(file, opts)

    ///Gets recent type check results, waiting for the results of in-progress type checking
    /// if version of file in memory is grater than last type checked version.
    /// It also waits if there are no FSharpProjectOptions avaliable for given file
    member x.TryGetLatestTypeCheckResultsForFile(file) =
        let file = Path.GetFullPath file
        let stateVersion = state.TryGetFileVersion file
        let checkedVersion = state.TryGetLastCheckedVersion file
        commandsLogger.debug (Log.setMessage "TryGetLatestTypeCheckResultsFor {file}, State@{stateVersion}, Checked@{checkedVersion}"
                              >> Log.addContextDestructured "file" file
                              >> Log.addContextDestructured "stateVersion" stateVersion
                              >> Log.addContextDestructured "checkedVersion" checkedVersion)

        match stateVersion, checkedVersion with
        | Some sv, Some cv when cv < sv ->
            x.FileChecked
            |> Event.filter (fun (_,n,_) -> n = file )
            |> Event.map ignore
            |> Async.AwaitEvent
            |> Async.bind (fun _ -> x.TryGetLatestTypeCheckResultsForFile(file))
        | Some _, None
        | None, Some _ ->
            x.FileChecked
            |> Event.filter (fun (_,n,_) -> n = file )
            |> Event.map ignore
            |> Async.AwaitEvent
            |> Async.bind (fun _ -> x.TryGetLatestTypeCheckResultsForFile(file))
        | _ ->
            match state.TryGetFileCheckerOptionsWithLines(file) with
            | ResultOrString.Ok (opts, _ ) ->
                x.TryGetRecentTypeCheckResultsForFile(file, opts)
                |> async.Return
            | ResultOrString.Error _ ->
                x.FileChecked
                |> Event.filter (fun (_,n,_) -> n = file )
                |> Event.map ignore
                |> Async.AwaitEvent
                |> Async.bind (fun _ -> x.TryGetLatestTypeCheckResultsForFile(file))




    member x.TryGetFileCheckerOptionsWithLinesAndLineStr(file, pos) =
        let file = Path.GetFullPath file
        state.TryGetFileCheckerOptionsWithLinesAndLineStr(file, pos)

    member x.TryGetFileCheckerOptionsWithLines(file) =
        let file = Path.GetFullPath file
        state.TryGetFileCheckerOptionsWithLines file

    member x.Files = state.Files

    member x.TryGetFileVersion = state.TryGetFileVersion

    member x.Parse file lines version (isSdkScript: bool option) =
        let file = Path.GetFullPath file
        let tmf = isSdkScript |> Option.map (fun n -> if n then FSIRefs.NetCore else FSIRefs.NetFx) |> Option.defaultValue FSIRefs.NetFx

        do x.CancelQueue file
        async {
            let colorizations = state.ColorizationOutput
            let parse' fileName text options =
                async {
                    let! result = checker.ParseAndCheckFileInProject(fileName, version, text, options)
                    return
                        match result with
                        | ResultOrString.Error e ->
                            CoreResponse.ErrorRes e
                        | ResultOrString.Ok (parseAndCheck) ->
                            let parseResult = parseAndCheck.GetParseResults
                            let results = parseAndCheck.GetCheckResults
                            do fileParsed.Trigger parseResult
                            do lastVersionChecked <- version
                            do lastCheckResult <- Some parseAndCheck
                            do state.SetLastCheckedVersion fileName version
                            do fileChecked.Trigger (parseAndCheck, fileName, version)
                            let errors = Array.append results.Errors parseResult.Errors
                            CoreResponse.Res (errors, fileName)
                }
            let normalizeOptions (opts : FSharpProjectOptions) =
                { opts with
                    SourceFiles = opts.SourceFiles |> Array.map (Path.GetFullPath)
                    OtherOptions = opts.OtherOptions |> Array.map (fun n -> if FscArguments.isCompileFile(n) then Path.GetFullPath n else n)
                }
            let text = String.concat "\n" lines

            if Utils.isAScript file then
                let! checkOptions = checker.GetProjectOptionsFromScript(file, text, tmf)
                state.AddFileTextAndCheckerOptions(file, lines, normalizeOptions checkOptions, Some version)
                fileStateSet.Trigger ()
                return! parse' file text checkOptions
            else
                let! checkOptions =
                    match state.GetCheckerOptions(file, lines) with
                    | Some c ->
                        state.SetFileVersion file version
                        async.Return c
                    | None -> async {
                        let! checkOptions = checker.GetProjectOptionsFromScript(file, text, tmf)
                        state.AddFileTextAndCheckerOptions(file, lines, normalizeOptions checkOptions, Some version)
                        return checkOptions
                    }
                fileStateSet.Trigger ()
                return! parse' file text checkOptions
        } |> x.AsCancellable file


    member x.Project projectFileName onChange tfmForScripts = async {
        let! res = state.ProjectController.LoadProject projectFileName onChange tfmForScripts onProjectLoaded
        return CoreResponse.Res res
    }

    member x.Declarations file lines version = async {
        let file = Path.GetFullPath file
        match state.TryGetFileCheckerOptionsWithSource file, lines with
        | ResultOrString.Error s, None ->
            match state.TryGetFileSource file with
            | ResultOrString.Error s -> return CoreResponse.ErrorRes s
            | ResultOrString.Ok l ->
                let text = String.concat "\n" l
                let files = Array.singleton file
                let parseOptions = { FSharpParsingOptions.Default with SourceFiles = files}
                let! decls = checker.GetDeclarations(file, text, parseOptions, version)
                let decls = decls |> Array.map (fun a -> a,file)
                return CoreResponse.Res decls
        | ResultOrString.Error _, Some l ->
            let text = String.concat "\n" l
            let files = Array.singleton file
            let parseOptions = { FSharpParsingOptions.Default with SourceFiles = files}
            let! decls = checker.GetDeclarations(file, text, parseOptions, version)
            let decls = decls |> Array.map (fun a -> a,file)
            return CoreResponse.Res decls
        | ResultOrString.Ok (checkOptions, source), _ ->
            let text =
                match lines with
                | Some l -> String.concat "\n" l
                | None -> source

            let parseOptions = Utils.projectOptionsToParseOptions checkOptions
            let! decls = checker.GetDeclarations(file, text, parseOptions, version)

            state.NavigationDeclarations.[file] <- decls

            let decls = decls |> Array.map (fun a -> a,file)
            return CoreResponse.Res decls
    }

    member x.DeclarationsInProjects () = async {
        let decls =
            state.NavigationDeclarations.ToArray()
            |> Array.collect (fun (KeyValue(p, decls)) -> decls |> Array.map (fun d -> d,p))
        return CoreResponse.Res decls
    }

    member __.Helptext sym =
        match KeywordList.keywordDescriptions.TryGetValue sym with
        | true, s ->
            CoreResponse.Res (HelpText.Simple (sym, s))
        | _ ->
        match KeywordList.hashDirectives.TryGetValue sym with
        | true, s ->
            CoreResponse.Res (HelpText.Simple (sym, s))
        | _ ->
        let sym = if sym.StartsWith "``" && sym.EndsWith "``" then sym.TrimStart([|'`'|]).TrimEnd([|'`'|]) else sym
        match state.Declarations.TryFind sym with
        | None -> //Isn't in sync filled cache, we don't have result
            CoreResponse.ErrorRes (sprintf "No help text available for symbol '%s'" sym)
        | Some (decl, pos, fn) -> //Is in sync filled cache, try to get results from async filled cahces or calculate if it's not there
            let source =
                state.Files.TryFind fn
                |> Option.map (fun n -> n.Lines)
            match source with
            | None -> CoreResponse.ErrorRes (sprintf "No help text available for symbol '%s'" sym)
            | Some source ->
                let getSource = fun i -> source.[i - 1]

                let tip =
                    match state.HelpText.TryFind sym with
                    | None -> decl.DescriptionText
                    | Some tip -> tip
                state.HelpText.[sym] <- tip

                let n =
                    match state.CompletionNamespaceInsert.TryFind sym with
                    | None -> calculateNamespaceInser decl pos getSource
                    | Some s -> Some s
                CoreResponse.Res (HelpText.Full (sym, tip, n))

    member x.CompilerLocation () = CoreResponse.Res (Environment.fsc, Environment.fsi, Environment.msbuild, checker.GetDotnetRoot())
    member x.Colorization enabled = state.ColorizationOutput <- enabled
    member x.Error msg = [CoreResponse.ErrorRes msg]

    member x.Completion (tyRes : ParseAndCheckResults) (pos: pos) lineStr (lines : string[]) (fileName : SourceFilePath) filter includeKeywords includeExternal =
        async {

            let fileName = Path.GetFullPath fileName
            let getAllSymbols () =
                if includeExternal then tyRes.GetAllEntities true else []
            let! res = tyRes.TryGetCompletions pos lineStr filter getAllSymbols
            return
                match res with
                | Some (decls, residue, shouldKeywords) ->
                    let declName (d: FSharpDeclarationListItem) = d.Name
                    let getLine = fun i -> lines.[i - 1]

                    //Init cache for current list
                    state.Declarations.Clear()
                    state.HelpText.Clear()
                    state.CompletionNamespaceInsert.Clear()
                    state.CurrentAST <- tyRes.GetAST

                    //Fill cache for current list
                    do fillHelpTextInTheBackground decls pos fileName getLine

                    // Send the first helptext without being requested.
                    // This allows it to be displayed immediately in the editor.
                    let firstMatchOpt =
                      decls
                      |> Array.sortBy declName
                      |> Array.tryFind (fun d -> (declName d).StartsWith(residue, StringComparison.InvariantCultureIgnoreCase))

                    let includeKeywords = includeKeywords && shouldKeywords

                    CoreResponse.Res (decls, includeKeywords)

                | None -> CoreResponse.ErrorRes "Timed out while fetching completions"
        }

    member x.ToolTip (tyRes : ParseAndCheckResults) (pos: pos) lineStr =
        tyRes.TryGetToolTipEnhanced pos lineStr
        |> x.MapResult CoreResponse.Res
        |> x.AsCancellable (Path.GetFullPath tyRes.FileName)

    member x.FormattedDocumentation (tyRes : ParseAndCheckResults) (pos: pos) lineStr =
        tyRes.TryGetFormattedDocumentation pos lineStr
        |> x.MapResult CoreResponse.Res

    member x.FormattedDocumentationForSymbol (tyRes : ParseAndCheckResults) (xmlSig: string) (assembly: string) =
        tyRes.TryGetFormattedDocumentationForSymbol xmlSig assembly
        |> x.MapResult CoreResponse.Res

    member x.Typesig (tyRes : ParseAndCheckResults) (pos: pos) lineStr =
        tyRes.TryGetToolTip pos lineStr
        |> x.MapResult CoreResponse.Res
        |> x.AsCancellable (Path.GetFullPath tyRes.FileName)

    member x.SymbolUse (tyRes : ParseAndCheckResults) (pos: pos) lineStr =
        tyRes.TryGetSymbolUse pos lineStr
        |> x.MapResult CoreResponse.Res
        |> x.AsCancellable (Path.GetFullPath tyRes.FileName)

    member x.SignatureData (tyRes : ParseAndCheckResults) (pos: pos) lineStr =
        tyRes.TryGetSignatureData pos lineStr
        |> x.MapResult CoreResponse.Res
        |> x.AsCancellable (Path.GetFullPath tyRes.FileName)

    member x.Help (tyRes : ParseAndCheckResults) (pos: pos) lineStr =
        tyRes.TryGetF1Help pos lineStr
        |> x.MapResult CoreResponse.Res
        |> x.AsCancellable (Path.GetFullPath tyRes.FileName)

    member x.SymbolUseProject (tyRes : ParseAndCheckResults) (pos: pos) lineStr =
        let fn = tyRes.FileName
        tyRes.TryGetSymbolUse pos lineStr |> x.MapResultAsync (fun (sym, usages) ->
            async {
                let fsym = sym.Symbol
                if fsym.IsPrivateToFile then
                    return CoreResponse.Res (LocationResponse.Use (sym, usages))
                elif backgroundServiceEnabled then
                    match! SymbolCache.getSymbols fsym.FullName with
                    | None ->
                        if fsym.IsInternalToProject then
                            let opts = state.GetProjectOptions' tyRes.FileName
                            let! symbols = checker.GetUsesOfSymbol (fn, [tyRes.FileName, opts] , sym.Symbol)
                            return CoreResponse.Res (LocationResponse.Use (sym, symbols))
                        else
                            let! symbols = checker.GetUsesOfSymbol (fn, state.FSharpProjectOptions, sym.Symbol)
                            return CoreResponse.Res (LocationResponse.Use (sym, symbols))
                    | Some res ->
                        return CoreResponse.Res (LocationResponse.UseRange res)
                elif fsym.IsInternalToProject then
                    let opts = state.GetProjectOptions' tyRes.FileName
                    let! symbols = checker.GetUsesOfSymbol (fn, [tyRes.FileName, opts] , sym.Symbol)
                    return CoreResponse.Res (LocationResponse.Use (sym, symbols))
                else
                    let! symbols = checker.GetUsesOfSymbol (fn, state.FSharpProjectOptions, sym.Symbol)
                    return CoreResponse.Res (LocationResponse.Use (sym, symbols))
            })
        |> x.AsCancellable (Path.GetFullPath fn)

    member x.SymbolImplementationProject (tyRes : ParseAndCheckResults) (pos: pos) lineStr =
        let fn = tyRes.FileName
        let filterSymbols symbols =
            symbols
            |> Array.where (fun (su: FSharpSymbolUse) -> su.IsFromDispatchSlotImplementation || (su.IsFromType && not (UntypedAstUtils.isTypedBindingAtPosition tyRes.GetAST su.RangeAlternate )) )

        tyRes.TryGetSymbolUse pos lineStr |> x.MapResultAsync (fun (sym, usages) ->
            async {
                let fsym = sym.Symbol
                if fsym.IsPrivateToFile then
                    return CoreResponse.Res (LocationResponse.Use (sym, filterSymbols usages))
                elif backgroundServiceEnabled then
                    match! SymbolCache.getImplementation fsym.FullName with
                    | None ->
                        if fsym.IsInternalToProject then
                            let opts = state.GetProjectOptions' tyRes.FileName
                            let! symbols = checker.GetUsesOfSymbol (fn, [tyRes.FileName, opts] , sym.Symbol)
                            return CoreResponse.Res (LocationResponse.Use (sym, filterSymbols symbols ))
                        else
                            let! symbols = checker.GetUsesOfSymbol (fn, state.FSharpProjectOptions, sym.Symbol)
                            return CoreResponse.Res (LocationResponse.Use (sym, filterSymbols symbols))
                    | Some res ->
                        return CoreResponse.Res (LocationResponse.UseRange res)
                elif fsym.IsInternalToProject then
                    let opts = state.GetProjectOptions' tyRes.FileName
                    let! symbols = checker.GetUsesOfSymbol (fn, [tyRes.FileName, opts] , sym.Symbol)
                    return CoreResponse.Res (LocationResponse.Use (sym, filterSymbols symbols ))
                else
                    let! symbols = checker.GetUsesOfSymbol (fn, state.FSharpProjectOptions, sym.Symbol)
                    let symbols = filterSymbols symbols
                    return CoreResponse.Res (LocationResponse.Use (sym, symbols ))
            })
        |> x.AsCancellable (Path.GetFullPath fn)

    member x.FindDeclaration (tyRes : ParseAndCheckResults) (pos: pos) lineStr =
        tyRes.TryFindDeclaration pos lineStr
        |> x.MapResult (CoreResponse.Res, CoreResponse.ErrorRes)
        |> x.AsCancellable (Path.GetFullPath tyRes.FileName)

    member x.FindTypeDeclaration (tyRes : ParseAndCheckResults) (pos: pos) lineStr =
        tyRes.TryFindTypeDeclaration pos lineStr
        |> x.MapResult (CoreResponse.Res, CoreResponse.ErrorRes)
        |> x.AsCancellable (Path.GetFullPath tyRes.FileName)

    member x.Methods (tyRes : ParseAndCheckResults) (pos: pos) (lines: LineStr[]) =
        tyRes.TryGetMethodOverrides lines pos
        |> x.MapResult (CoreResponse.Res, CoreResponse.ErrorRes)
        |> x.AsCancellable (Path.GetFullPath tyRes.FileName)

    member x.Lint (file: SourceFilePath) =
        let file = Path.GetFullPath file
        async {
            match state.TryGetFileCheckerOptionsWithSource file with
            | Error s -> return CoreResponse.ErrorRes s
            | Ok (options, source) ->
                let tyResOpt = checker.TryGetRecentCheckResultsForFile(file, options)

                match tyResOpt with
                | None -> return CoreResponse.InfoRes "Cached typecheck results not yet available"
                | Some tyRes ->
                    match tyRes.GetAST with
                    | None -> return CoreResponse.InfoRes "Something went wrong during parsing"
                    | Some tree ->
                        try
                            let! ctok = Async.CancellationToken
                            let fsharpLintConfig = Lint.loadConfiguration file
                            match Lint.lintWithConfiguration fsharpLintConfig ctok tree source tyRes.GetCheckResults with
                            | Error e -> return CoreResponse.InfoRes e
                            | Ok enrichedWarnings ->
                                let res = CoreResponse.Res (file, enrichedWarnings)
                                notify.Trigger (NotificationEvent.Lint (file, enrichedWarnings))
                                return res
                        with _ex ->
                            return CoreResponse.InfoRes "Something went wrong during linter"
        } |> x.AsCancellable file

    member x.GetNamespaceSuggestions (tyRes : ParseAndCheckResults) (pos: pos) (line: LineStr) =
        async {
            match tyRes.GetAST with
            | None -> return CoreResponse.InfoRes "Parsed Tree not avaliable"
            | Some parsedTree ->
            match Lexer.findLongIdents(pos.Column, line) with
            | None -> return CoreResponse.InfoRes "Ident not found"
            | Some (_,idents) ->
            match UntypedParseImpl.GetEntityKind(pos, parsedTree)  with
            | None -> return CoreResponse.InfoRes "EntityKind not found"
            | Some entityKind ->

            let symbol = Lexer.getSymbol pos.Line pos.Column line SymbolLookupKind.Fuzzy [||]
            match symbol with
            | None -> return CoreResponse.InfoRes "Symbol at position not found"
            | Some sym ->


            let entities = tyRes.GetAllEntities true

            let isAttribute = entityKind = EntityKind.Attribute
            let entities =
                entities
                |> List.filter (fun e ->
                    match entityKind, (e.Kind LookupType.Fuzzy) with
                    | EntityKind.Attribute, EntityKind.Attribute
                    | EntityKind.Type, (EntityKind.Type | EntityKind.Attribute)
                    | EntityKind.FunctionOrValue _, _ -> true
                    | EntityKind.Attribute, _
                    | _, EntityKind.Module _
                    | EntityKind.Module _, _
                    | EntityKind.Type, _ -> false)

            let maybeUnresolvedIdents =
                idents
                |> Array.map (fun ident -> { Ident = ident; Resolved = false})

            let entities =
                entities
                |> List.collect (fun e ->
                      [ yield e.TopRequireQualifiedAccessParent, e.AutoOpenParent, e.Namespace, e.CleanedIdents
                        if isAttribute then
                            let lastIdent = e.CleanedIdents.[e.CleanedIdents.Length - 1]
                            if (e.Kind LookupType.Fuzzy) = EntityKind.Attribute && lastIdent.EndsWith "Attribute" then
                                yield
                                    e.TopRequireQualifiedAccessParent,
                                    e.AutoOpenParent,
                                    e.Namespace,
                                    e.CleanedIdents
                                    |> Array.replace (e.CleanedIdents.Length - 1) (lastIdent.Substring(0, lastIdent.Length - 9)) ])
            let createEntity = ParsedInput.tryFindInsertionContext pos.Line parsedTree maybeUnresolvedIdents TopLevel
            let word = sym.Text
            let candidates = entities |> Seq.collect createEntity |> Seq.toList

            let openNamespace =
                candidates
                |> List.choose (fun (entity, ctx) -> entity.Namespace |> Option.map (fun ns -> ns, entity.Name, ctx))
                |> List.groupBy (fun (ns, _, _) -> ns)
                |> List.map (fun (ns, xs) ->
                    ns,
                    xs
                    |> List.map (fun (_, name, ctx) -> name, ctx)
                    |> List.distinctBy (fun (name, _) -> name)
                    |> List.sortBy fst)
                |> List.collect (fun (ns, names) ->
                    let multipleNames = match names with | [] -> false | [_] -> false | _ -> true
                    names |> List.map (fun (name, ctx) -> ns, name, ctx, multipleNames))

            let qualifySymbolActions =
                candidates
                |> List.map (fun (entity, _) -> entity.FullRelativeName, entity.Qualifier)
                |> List.distinct
                |> List.sort

            return CoreResponse.Res (word, openNamespace, qualifySymbolActions)
        } |> x.AsCancellable (Path.GetFullPath tyRes.FileName)

    member x.GetUnionPatternMatchCases (tyRes : ParseAndCheckResults) (pos: pos) (lines: LineStr[]) (line: LineStr) =
        async {
            let codeGenService = CodeGenerationService(checker, state)
            let doc = {
                Document.LineCount = lines.Length
                FullName = tyRes.FileName
                GetText = fun _ -> lines |> String.concat "\n"
                GetLineText0 = fun i -> lines.[i]
                GetLineText1 = fun i -> lines.[i - 1]
            }

            let! res = tryFindUnionDefinitionFromPos codeGenService pos doc
            match res with
            | None -> return CoreResponse.InfoRes "Union at position not found"
            | Some (patMatchExpr, unionTypeDefinition, insertionPos) ->

            if shouldGenerateUnionPatternMatchCases patMatchExpr unionTypeDefinition then
                let result = formatMatchExpr insertionPos "$1" patMatchExpr unionTypeDefinition
                let pos = mkPos insertionPos.InsertionPos.Line insertionPos.InsertionPos.Column

                return CoreResponse.Res (result, pos)
            else
                return CoreResponse.InfoRes "Union at position not found"
        } |> x.AsCancellable (Path.GetFullPath tyRes.FileName)

    member x.GetRecordStub (tyRes : ParseAndCheckResults) (pos: pos) (lines: LineStr[]) (line: LineStr) =
        async {
            let codeGenServer = CodeGenerationService(checker, state)
            let doc = {
                Document.LineCount = lines.Length
                FullName = tyRes.FileName
                GetText = fun _ -> lines |> String.concat "\n"
                GetLineText0 = fun i -> lines.[i]
                GetLineText1 = fun i -> lines.[i - 1]
            }

            let! res = tryFindRecordDefinitionFromPos codeGenServer pos doc
            match res with
            | None -> return CoreResponse.InfoRes "Record at position not found"
            | Some(recordEpr, (Some recordDefinition), insertionPos) ->
                if shouldGenerateRecordStub recordEpr recordDefinition then
                    let result = formatRecord insertionPos "$1" recordDefinition recordEpr.FieldExprList
                    let pos = mkPos insertionPos.InsertionPos.Line insertionPos.InsertionPos.Column
                    return CoreResponse.Res (result, pos)
                else
                    return CoreResponse.InfoRes "Record at position not found"
            | _ -> return CoreResponse.InfoRes "Record at position not found"
        } |> x.AsCancellable (Path.GetFullPath tyRes.FileName)

    member x.GetInterfaceStub (tyRes : ParseAndCheckResults) (pos: pos) (lines: LineStr[]) (lineStr: LineStr) =
        async {
            let codeGenServer = CodeGenerationService(checker, state)
            let doc = {
                Document.LineCount = lines.Length
                FullName = tyRes.FileName
                GetText = fun _ -> lines |> String.concat "\n"
                GetLineText0 = fun i -> lines.[i]
                GetLineText1 = fun i -> lines.[i - 1]
            }

            let! res = tryFindInterfaceExprInBufferAtPos codeGenServer pos doc
            match res with
            | None -> return CoreResponse.InfoRes "Interface at position not found"
            | Some interfaceData ->
                let! stubInfo = handleImplementInterface codeGenServer pos doc lines lineStr interfaceData

                match stubInfo with
                | Some (insertPosition, generatedCode) ->
                    return CoreResponse.Res (generatedCode, insertPosition)
                | None -> return CoreResponse.InfoRes "Interface at position not found"
        } |> x.AsCancellable (Path.GetFullPath tyRes.FileName)

    member x.WorkspacePeek (dir: string) (deep: int) (excludedDirs: string list) = async {
        let d = state.ProjectController.PeekWorkspace dir deep excludedDirs
        return CoreResponse.Res d
    }

    member x.WorkspaceLoad onChange (files: string list) (disableInMemoryProjectReferences: bool) tfmForScripts = async {
        checker.DisableInMemoryProjectReferences <- disableInMemoryProjectReferences
        let! res = state.ProjectController.LoadWorkspace onChange files tfmForScripts onProjectLoaded
        return CoreResponse.Res res
    }

    member x.GetUnusedDeclarations file =
        let file = Path.GetFullPath file
        let isScript = file.EndsWith ".fsx"

        async {
            match state.TryGetFileCheckerOptionsWithSource file with
            | Error s ->  return CoreResponse.ErrorRes s
            | Ok (opts, _) ->
                let tyResOpt = checker.TryGetRecentCheckResultsForFile(file, opts)
                match tyResOpt with
                | None -> return CoreResponse.InfoRes "Cached typecheck results not yet available"
                | Some tyRes ->
                    let! allUses = tyRes.GetCheckResults.GetAllUsesOfAllSymbolsInFile ()
                    let unused = UnusedDeclarationsAnalyzer.getUnusedDeclarationRanges allUses isScript
                    let res = CoreResponse.Res (file, unused)
                    notify.Trigger (NotificationEvent.UnusedDeclarations (file, unused))
                    return res
        } |> x.AsCancellable file

    member x.GetSimplifiedNames file =
        let file = Path.GetFullPath file
        async {
            match state.TryGetFileCheckerOptionsWithLines file with
            | Error s ->  return CoreResponse.ErrorRes s
            | Ok (opts, source) ->
                let tyResOpt = checker.TryGetRecentCheckResultsForFile(file, opts)
                match tyResOpt with
                | None -> return CoreResponse.InfoRes "Cached typecheck results not yet available"
                | Some tyRes ->
                    let getSourceLine lineNo = source.[lineNo]
                    let! simplified = SimplifyNames.getSimplifiableNames(tyRes.GetCheckResults, getSourceLine)
                    let simplified = Array.ofList simplified
                    let res = CoreResponse.Res (file, simplified)
                    notify.Trigger (NotificationEvent.SimplifyNames (file, simplified))
                    return res
        } |> x.AsCancellable file

    member x.GetUnusedOpens file =
        let file = Path.GetFullPath file
        async {
            match state.TryGetFileCheckerOptionsWithLines file with
            | Error s ->  return CoreResponse.ErrorRes s
            | Ok (opts, source) ->
                let tyResOpt = checker.TryGetRecentCheckResultsForFile(file, opts)
                match tyResOpt with
                | None -> return CoreResponse.InfoRes "Cached typecheck results not yet available"
                | Some tyRes ->
                    let! unused = UnusedOpens.getUnusedOpens(tyRes.GetCheckResults, fun i -> source.[i - 1])
                    let res = CoreResponse.Res (file, (unused |> List.toArray))
                    notify.Trigger (NotificationEvent.UnusedOpens (file, (unused |> List.toArray)))
                    return res
        } |> x.AsCancellable file


    member x.GetRangesAtPosition file positions =
        let file = Path.GetFullPath file
        let parseResult = state.ParseResults.TryFind file
        match parseResult with
        | None -> CoreResponse.InfoRes "Cached typecheck results not yet available"
        | Some pr ->
            positions |> List.map (fun x ->
                UntypedAstUtils.getRangesAtPosition pr.ParseTree x
            )
            |> CoreResponse.Res

    member x.Compile projectFileName = async {
        let projectFileName = Path.GetFullPath projectFileName
        match state.GetProject projectFileName with
        | None -> return CoreResponse.InfoRes "Project not found"
        | Some proj ->
        match proj.Response with
        | None -> return CoreResponse.InfoRes "Project not found"
        | Some proj ->
            let! errors,code = checker.Compile(proj.Options.OtherOptions)
            return CoreResponse.Res (errors,code)
    }

    member __.StartBackgroundService (workspaceDir : string option) =
        if backgroundServiceEnabled && workspaceDir.IsSome then
            SymbolCache.initCache workspaceDir.Value
            BackgroundServices.start ()

    member __.ProcessProjectsInBackground (file) =
        if backgroundServiceEnabled then
            BackgroundServices.saveFile file

    member x.GetGitHash () =
        let version = Version.info ()
        version.GitSha

    member __.Quit () =
        async {
            return [ CoreResponse.InfoRes "quitting..." ]
        }

    member x.FakeTargets file ctx = async {
        let file = Path.GetFullPath file
        let! targets = FakeSupport.getTargets file ctx
        return CoreResponse.Res targets
    }

    member x.FakeRuntime () = async {
        let! runtimePath = FakeSupport.getFakeRuntime ()
        return CoreResponse.Res runtimePath
    }

    member x.ScopesForFile (file: string) = async {
        let file = Path.GetFullPath file
        match state.TryGetFileCheckerOptionsWithLines file with
        | Error s -> return Error s
        | Ok (opts, sourceLines) ->
            let parseOpts = Utils.projectOptionsToParseOptions opts
            let allSource = sourceLines |> String.concat "\n"
            let! ast = checker.ParseFile(file, allSource, parseOpts)
            match ast.ParseTree with
            | None -> return Error (ast.Errors |> Array.map string |> String.concat "\n")
            | Some ast' ->
                let ranges = Structure.getOutliningRanges sourceLines ast'
                return Ok ranges
    }

    member __.SetDotnetSDKRoot(path) = checker.SetDotnetRoot(path)
    member __.SetFSIAdditionalArguments args = checker.SetFSIAdditionalArguments args

    member x.FormatDocument (file: SourceFilePath) = async {
        let file = Path.GetFullPath file

        match x.TryGetFileCheckerOptionsWithLines file with
        | Result.Ok (opts, lines) ->
            let source = String.concat "\n" lines
            let parsingOptions = Utils.projectOptionsToParseOptions opts
            let checker : FSharpChecker = checker.GetFSharpChecker()
            // ENHANCEMENT: consider caching the Fantomas configuration and reevaluate when the configuration file changes.
            let config =
                let currentFolder = Path.GetDirectoryName(file)
                let result = Fantomas.CodeFormatter.ReadConfiguration currentFolder
                match result with
                | Fantomas.FormatConfig.Success c -> c
                | Fantomas.FormatConfig.PartialSuccess(c,warnings) ->
                    match warnings with
                    | [] ->
                      c
                    | warnings ->
                      fantomasLogger.warn (Log.setMessage "Warnings while parsing the configuration file at {path}" >> Log.addContextDestructured "path" currentFolder >> Log.addContextDestructured "warnings" warnings)
                      c
                | Fantomas.FormatConfig.Failure err ->
                    fantomasLogger.error (Log.setMessage "Error while parsing the configuration files at {path}. Using default configuration" >> Log.addContextDestructured "path" currentFolder >> Log.addExn err)
                    Fantomas.FormatConfig.FormatConfig.Default

            let! formatted =
                Fantomas.CodeFormatter.FormatDocumentAsync(file,
                                                           Fantomas.SourceOrigin.SourceString source,
                                                           config,
                                                           parsingOptions,
                                                           checker)
            return Some (lines, formatted)
        | Result.Error er ->
            return None
    }
