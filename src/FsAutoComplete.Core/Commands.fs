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
open FsToolkit.ErrorHandling

module Result =
  let bimap okF errF r = match r with | Ok x -> okF x | Error y -> errF y
  let ofOption recover o = match o with | Some x -> Ok x | None -> Error (recover ())

[<RequireQualifiedAccess>]
type LocationResponse<'a,'b> =
    | Use of 'a
    | UseRange of 'b

[<RequireQualifiedAccess>]
type HelpText =
    | Simple of symbol: string * text: string
    | Full of symbol: string * tip: FSharpToolTipText * textEdits: CompletionNamespaceInsert option


[<RequireQualifiedAccess>]
type CoreResponse<'a> =
    | InfoRes of text: string
    | ErrorRes of text: string
    | Res of 'a

module AsyncResult =
  let bimap okF errF ar = async {
    match! ar with
    | Ok a -> return okF a
    | Error e -> return errF e
  }

  let inline mapErrorRes ar: Async<CoreResponse<'a>> =
    AsyncResult.foldResult id CoreResponse.ErrorRes ar

  let recoverCancellationGeneric (ar: Async<Result<'t, exn>>) recoverInternal = AsyncResult.foldResult id recoverInternal ar
  let recoverCancellation (ar: Async<Result<CoreResponse<'t>, exn>>) = recoverCancellationGeneric ar (sprintf "Request cancelled (exn was %A)" >> CoreResponse.InfoRes)
  let recoverCancellationIgnore (ar: Async<Result<unit, exn>>) = AsyncResult.foldResult id ignore ar

[<RequireQualifiedAccess>]
type NotificationEvent<'analyzer> =
    | ParseError of errors: FSharpErrorInfo[] * file: string
    | Workspace of ProjectSystem.ProjectResponse
    | AnalyzerMessage of  messages: 'analyzer [] * file: string
    | UnusedOpens of file: string * opens: range[]
    | Lint of file: string * warningsWithCodes: Lint.EnrichedLintWarning list
    | UnusedDeclarations of file: string * decls: (range * bool)[]
    | SimplifyNames of file: string * names: SimplifyNames.SimplifiableRange []
    | Canceled of string
    | Diagnostics of LanguageServerProtocol.Types.PublishDiagnosticsParams
    | FileParsed of string

type Commands<'analyzer> (serialize : Serializer, backgroundServiceEnabled) =
    let checker = FSharpCompilerServiceChecker(backgroundServiceEnabled)
    let state = State.Initial (checker.GetFSharpChecker())
    let fileParsed = Event<FSharpParseFileResults>()
    let fileChecked = Event<ParseAndCheckResults * string * int>()
    let scriptFileProjectOptions = Event<FSharpProjectOptions>()

    let mutable workspaceRoot: string option = None
    let mutable linterConfigFileRelativePath: string option = None
    let mutable linterConfiguration: FSharpLint.Application.Lint.ConfigurationParam = FSharpLint.Application.Lint.ConfigurationParam.Default
    let mutable lastVersionChecked = -1
    let mutable lastCheckResult : ParseAndCheckResults option = None
    let mutable analyzerHandler : ((string * string [] * FSharp.Compiler.SyntaxTree.ParsedInput * FSharpImplementationFileContents * FSharpEntity list * (bool -> AssemblySymbol list)) -> 'analyzer []) option = None

    let notify = Event<NotificationEvent<_>>()

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
        checkerLogger.info (Log.setMessage "Script typecheck dependencies changed, purging expired script options")
        let count = state.ScriptProjectOptions.Count
        state.ScriptProjectOptions.Clear ()
        checkerLogger.info (Log.setMessage "Script typecheck dependencies changed, purged {optionCount} expired script options" >> Log.addContextDestructured "optionCount" count)
    )

    do if not backgroundServiceEnabled then
            checker.FileChecked.Add (fun (n,_) ->
                checkerLogger.info (Log.setMessage "{file} checked" >> Log.addContextDestructured "file" n)
                async {
                    try
                        match state.GetProjectOptions n with
                        | Some opts ->
                            let! res = checker.GetBackgroundCheckResultsForFileInProject(n, opts)
                            fileChecked.Trigger (res, Utils.normalizePath res.FileName, -1)
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
                  match state.Files.TryGetValue file with
                  | true, fileData ->

                    let res = handler (file, fileData.Lines, pt, tast, parseAndCheck.GetCheckResults.PartialAssemblySignature.Entities |> Seq.toList, parseAndCheck.GetAllEntities)

                    (res, file)
                    |> NotificationEvent.AnalyzerMessage
                    |> notify.Trigger
                    Loggers.analyzers.info (Log.setMessage "end analysis of {file}" >> Log.addContextDestructured "file" file)
                  | false, _ ->
                    let otherKeys = state.Files.Keys |> Array.ofSeq
                    Loggers.analyzers.info (Log.setMessage "No file contents found for {file}. Current files are {files}"
                                            >> Log.addContextDestructured "file" file
                                            >> Log.addContextDestructured "files" otherKeys)
                | _ ->
                  Loggers.analyzers.info (Log.setMessage "missing components of {file} to run analyzers, skipped them" >> Log.addContextDestructured "file" file)
                  ()
            with
            | ex ->
                Loggers.analyzers.error (Log.setMessage "Run failed for {file}" >> Log.addContextDestructured "file" file >> Log.addExn ex)
        } |> Async.Start
    )

    let parseFilesInTheBackground fsiScriptTFM files =
        async {
          for file in files do
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
                    async {
                      let! parseRes = checker.ParseFile(file, source |> String.concat "\n", opts)
                      fileParsed.Trigger parseRes
                    }
                    |> Async.Start
            with
            | :? System.Threading.ThreadAbortException as ex ->
                // on mono, if background parsing is aborted a ThreadAbortException
                // is raised, who can be ignored
                ()
            | ex ->
                commandsLogger.error (Log.setMessage "Failed to parse file '{file}'" >> Log.addContextDestructured "file" file >> Log.addExn ex)
         }

    let codeGenServer = CodeGenerationService(checker, state)

    let docForText (lines: string []) (tyRes: ParseAndCheckResults): Document =
      {
          LineCount = lines.Length
          FullName = tyRes.FileName
          GetText = fun _ -> lines |> String.concat "\n"
          GetLineText0 = fun i -> lines.[i]
          GetLineText1 = fun i -> lines.[i - 1]
      }

    let calculateNamespaceInsert (decl : FSharpDeclarationListItem) (pos : pos) getLine: CompletionNamespaceInsert option =
        let getLine i =
            try
                getLine i
            with
            | _ -> ""
        let idents = decl.FullName.Split '.'
        decl.NamespaceToOpen
        |> Option.bind (fun n ->
            state.CurrentAST
            |> Option.map (fun ast -> ParsedInput.findNearestPointToInsertOpenDeclaration (pos.Line) ast idents Nearest)
            |> Option.map (fun ic -> { Namespace = n; Position = ic.Pos; Scope = ic.ScopeKind })
        )

    let fillHelpTextInTheBackground decls (pos : pos) fn getLine =
        let declName (d: FSharpDeclarationListItem) = d.Name

        //Fill list of declarations synchronously to know which declarations should be in cache.
        for d in decls do
            state.Declarations.[declName d] <- (d, pos, fn)

        //Fill namespace insertion cache asynchronously.
        async {
            for decl in decls do
                let n = declName decl
                match calculateNamespaceInsert decl pos getLine with
                | Some insert -> state.CompletionNamespaceInsert.[n] <- insert
                | None -> ()
        } |> Async.Start

    let onProjectLoaded projectFileName (response: ProjectCrackerCache) tfmForScripts (isFromCache: bool) =
        if backgroundServiceEnabled then
            BackgroundServices.updateProject(projectFileName, response.Options)

        if not isFromCache then
          response.Items
          |> List.choose (function Dotnet.ProjInfo.Workspace.ProjectViewerItem.Compile(p, _) -> Some p)
          |> parseFilesInTheBackground tfmForScripts
          |> Async.Start
        else
          commandsLogger.info (Log.setMessage "Project from cache '{file}'" >> Log.addContextDestructured "file" projectFileName)

    member __.Notify = notify.Publish

    member __.WorkspaceReady = state.ProjectController.WorkspaceReady

    member __.FileChecked = fileChecked.Publish

    member __.ScriptFileProjectOptions = scriptFileProjectOptions.Publish

    member __.IsWorkspaceReady
        with get() = state.ProjectController.IsWorkspaceReady

    member __.LastVersionChecked
        with get() = lastVersionChecked

    member __.AnalyzerHandler
        with get() = analyzerHandler
        and  set v =
          Loggers.analyzers.info (Log.setMessage "updated analyzer handler")
          analyzerHandler <- v

    member __.LastCheckResult
        with get() = lastCheckResult

    member __.SetFileContent(file: SourceFilePath, lines: LineStr[], version, tfmIfScript) =
        let file = Path.GetFullPath file
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
        let! results = DotnetCli.dotnetNew templateShortName name output parameterStr
        return CoreResponse.Res results
    }

    member x.DotnetAddProject (toProject: string) (reference: string) =  async {
      let! result = DotnetCli.dotnetAddProject toProject reference
      return CoreResponse.Res result
    }

    member x.DotnetRemoveProject (fromProject: string) (reference: string) =  async {
      let! result = DotnetCli.dotnetRemoveProject fromProject reference
      return CoreResponse.Res result
    }

    member x.DotnetSlnAdd (sln: string) (project: string) = async {
      let! result = DotnetCli.dotnetSlnAdd sln project
      return CoreResponse.Res result
    }

    member _.FsProjMoveFileUp (fsprojPath: string) (file: string) = async {
      FsProjEditor.moveFileUp fsprojPath file
      return CoreResponse.Res ()
    }

    member _.FsProjMoveFileDown (fsprojPath: string) (file: string) = async {
      FsProjEditor.moveFileDown fsprojPath file
      return CoreResponse.Res ()
    }

    member _.FsProjAddFileAbove (fsprojPath: string) (file: string) (newFileName: string) = async {
      try
        let dir = Path.GetDirectoryName fsprojPath
        let newFilePath = Path.Combine(dir, newFileName)
        File.Create newFilePath |> ignore

        FsProjEditor.addFileAbove fsprojPath file newFileName
        return CoreResponse.Res ()
      with
      | ex ->
        return CoreResponse.ErrorRes ex.Message
    }

    member _.FsProjAddFileBelow (fsprojPath: string) (file: string) (newFileName: string) = async {
      try
        let dir = Path.GetDirectoryName fsprojPath
        let newFilePath = Path.Combine(dir, newFileName)
        File.Create newFilePath |> ignore

        FsProjEditor.addFileBelow fsprojPath file newFileName
        return CoreResponse.Res ()
      with
      | ex ->
        return CoreResponse.ErrorRes ex.Message
    }

    member _.FsProjAddFile (fsprojPath: string) (file: string) = async {
      try
        let dir = Path.GetDirectoryName fsprojPath
        let newFilePath = Path.Combine(dir, file)
        File.Create newFilePath |> ignore

        FsProjEditor.addFile fsprojPath file
        return CoreResponse.Res ()
      with
      | ex ->
        return CoreResponse.ErrorRes ex.Message
    }

    member private x.AsCancellable (filename : SourceFilePath) (action : Async<'t>) =
        let cts = new CancellationTokenSource()
        state.AddCancellationToken(filename, cts)
        Async.StartCatchCancellation(action, cts.Token)
        |> Async.Catch
        |> Async.map (function
            | Choice1Of2 res -> Ok res
            | Choice2Of2 err ->
                notify.Trigger (NotificationEvent.Canceled (sprintf "Request cancelled (exn was %A)" err))
                Error err
            )


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
                    SourceFiles = opts.SourceFiles |> Array.filter FscArguments.isCompileFile |> Array.map (Path.GetFullPath)
                    OtherOptions = opts.OtherOptions |> Array.map (fun n -> if FscArguments.isCompileFile(n) then Path.GetFullPath n else n)
                }
            let text = String.concat "\n" lines

            if Utils.isAScript file
            then
                commandsLogger.info (Log.setMessage "Checking script file '{file}'" >> Log.addContextDestructured "file" file)
                let hash  =
                  lines
                  |> Array.filter (fun n -> n.StartsWith "#r" || n.StartsWith "#load" || n.StartsWith "#I")
                  |> Array.toList
                  |> fun n -> n.GetHashCode ()

                let! checkOptions =
                  match state.ScriptProjectOptions.TryFind file with
                  | Some (h, opts) when h = hash ->
                    async.Return opts
                  | _ ->
                    async {
                      let! checkOptions = checker.GetProjectOptionsFromScript(file, text, tmf)
                      state.ScriptProjectOptions.AddOrUpdate(file, (hash, checkOptions), (fun _ _ -> (hash, checkOptions))) |> ignore
                      return checkOptions
                    }

                scriptFileProjectOptions.Trigger checkOptions
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
        } |> x.AsCancellable file |> AsyncResult.recoverCancellation


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

    member __.Helptext sym = async {
        match KeywordList.keywordDescriptions.TryGetValue sym with
        | true, s ->
            return CoreResponse.Res (HelpText.Simple (sym, s))
        | _ ->
        match KeywordList.hashDirectives.TryGetValue sym with
        | true, s ->
            return CoreResponse.Res (HelpText.Simple (sym, s))
        | _ ->
        let sym = if sym.StartsWith "``" && sym.EndsWith "``" then sym.TrimStart([|'`'|]).TrimEnd([|'`'|]) else sym
        match state.Declarations.TryFind sym with
        | None -> //Isn't in sync filled cache, we don't have result
            return CoreResponse.ErrorRes (sprintf "No help text available for symbol '%s'" sym)
        | Some (decl, pos, fn) -> //Is in sync filled cache, try to get results from async filled cahces or calculate if it's not there
            let source =
                state.Files.TryFind fn
                |> Option.map (fun n -> n.Lines)
            match source with
            | None -> return CoreResponse.ErrorRes (sprintf "No help text available for symbol '%s'" sym)
            | Some source ->
                let getSource = fun i -> source.[i - 1]

                let! tip = async {
                  match state.HelpText.TryFind sym with
                  | Some tip -> return tip
                  | None ->
                    let! tip = decl.DescriptionTextAsync
                    state.HelpText.[sym] <- tip
                    return tip
                }

                let n =
                    match state.CompletionNamespaceInsert.TryFind sym with
                    | None -> calculateNamespaceInsert decl pos getSource
                    | Some s -> Some s
                return CoreResponse.Res (HelpText.Full (sym, tip, n))
    }

    member x.CompilerLocation () = CoreResponse.Res (Environment.fsc, Environment.fsi, Environment.msbuild, checker.GetDotnetRoot())
    member x.Colorization enabled = state.ColorizationOutput <- enabled
    member x.Error msg = [CoreResponse.ErrorRes msg]

    member x.Completion (tyRes : ParseAndCheckResults) (pos: pos) lineStr (lines : string[]) (fileName : SourceFilePath) filter includeKeywords includeExternal = async {
        let fileName = Path.GetFullPath fileName
        let getAllSymbols () =
            if includeExternal then tyRes.GetAllEntities true else []
        let! res = tyRes.TryGetCompletions pos lineStr filter getAllSymbols
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

            return CoreResponse.Res (decls, includeKeywords)

        | None -> return CoreResponse.ErrorRes "Timed out while fetching completions"
    }

    member x.ToolTip (tyRes : ParseAndCheckResults) (pos: pos) lineStr =
        tyRes.TryGetToolTipEnhanced pos lineStr
        |> AsyncResult.bimap CoreResponse.Res CoreResponse.ErrorRes

    member x.FormattedDocumentation (tyRes : ParseAndCheckResults) (pos: pos) lineStr =
        tyRes.TryGetFormattedDocumentation pos lineStr
        |> AsyncResult.bimap CoreResponse.Res CoreResponse.ErrorRes

    member x.FormattedDocumentationForSymbol (tyRes : ParseAndCheckResults) (xmlSig: string) (assembly: string) =
        tyRes.TryGetFormattedDocumentationForSymbol xmlSig assembly
        |> x.MapResult CoreResponse.Res

    member x.Typesig (tyRes : ParseAndCheckResults) (pos: pos) lineStr =
        tyRes.TryGetToolTip pos lineStr
        |> AsyncResult.bimap CoreResponse.Res CoreResponse.ErrorRes

    member x.SymbolUse (tyRes : ParseAndCheckResults) (pos: pos) lineStr =
        tyRes.TryGetSymbolUse pos lineStr
        |> AsyncResult.bimap CoreResponse.Res CoreResponse.ErrorRes

    member x.SignatureData (tyRes : ParseAndCheckResults) (pos: pos) lineStr =
        tyRes.TryGetSignatureData pos lineStr
        |> AsyncResult.bimap CoreResponse.Res CoreResponse.ErrorRes

    member x.Help (tyRes : ParseAndCheckResults) (pos: pos) lineStr =
        tyRes.TryGetF1Help pos lineStr
        |> AsyncResult.bimap CoreResponse.Res CoreResponse.ErrorRes

    member x.SymbolUseProject (tyRes : ParseAndCheckResults) (pos: pos) lineStr =
      let fn = tyRes.FileName
      async {
          match! tyRes.TryGetSymbolUse pos lineStr with
          | Ok (sym, usages) ->
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
          | Error x -> return CoreResponse.ErrorRes x
      }
      |> x.AsCancellable (Path.GetFullPath fn) |> AsyncResult.recoverCancellation

    member x.SymbolImplementationProject (tyRes : ParseAndCheckResults) (pos: pos) lineStr =
        let fn = tyRes.FileName
        let filterSymbols symbols =
            symbols
            |> Array.where (fun (su: FSharpSymbolUse) -> su.IsFromDispatchSlotImplementation || (su.IsFromType && not (UntypedAstUtils.isTypedBindingAtPosition tyRes.GetAST su.RangeAlternate )) )
        async {
          match! tyRes.TryGetSymbolUse pos lineStr with
          | Ok (sym, usages) ->
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
          | Error e -> return CoreResponse.ErrorRes e
        }
        |> x.AsCancellable (Path.GetFullPath fn)|> AsyncResult.recoverCancellation

    member x.FindDeclaration (tyRes : ParseAndCheckResults) (pos: pos) lineStr =
        tyRes.TryFindDeclaration pos lineStr
        |> x.MapResult (CoreResponse.Res, CoreResponse.ErrorRes)
        |> x.AsCancellable (Path.GetFullPath tyRes.FileName)|> AsyncResult.recoverCancellation

    member x.FindTypeDeclaration (tyRes : ParseAndCheckResults) (pos: pos) lineStr =
        tyRes.TryFindTypeDeclaration pos lineStr
        |> x.MapResult (CoreResponse.Res, CoreResponse.ErrorRes)
        |> x.AsCancellable (Path.GetFullPath tyRes.FileName) |> AsyncResult.recoverCancellation

    member x.Methods (tyRes : ParseAndCheckResults) (pos: pos) (lines: LineStr[]) =
        tyRes.TryGetMethodOverrides lines pos
        |> AsyncResult.bimap CoreResponse.Res CoreResponse.ErrorRes

    member x.Lint (file: SourceFilePath): Async<unit> =
        let file = Path.GetFullPath file
        asyncResult {
          let! (options, source) = state.TryGetFileCheckerOptionsWithSource file
          match checker.TryGetRecentCheckResultsForFile(file, options) with
          | None -> return ()
          | Some tyRes ->
            match tyRes.GetAST with
            | None -> return ()
            | Some tree ->
              try
                let! ctok = Async.CancellationToken
                let! enrichedWarnings = Lint.lintWithConfiguration linterConfiguration ctok tree source tyRes.GetCheckResults
                let res = CoreResponse.Res (file, enrichedWarnings)
                notify.Trigger (NotificationEvent.Lint (file, enrichedWarnings))
                return ()
              with ex ->
                commandsLogger.error (Log.setMessage "error while linting {file}: {message}"
                                      >> Log.addContextDestructured "file" file
                                      >> Log.addContextDestructured "message" ex.Message
                                      >> Log.addExn ex)
                return ()
        }
        |> Async.Ignore
        |> x.AsCancellable file

        |> AsyncResult.recoverCancellationIgnore

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
        }
        |> x.AsCancellable (Path.GetFullPath tyRes.FileName)
        |> AsyncResult.recoverCancellation

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
        }
        |> x.AsCancellable (Path.GetFullPath tyRes.FileName)
        |> AsyncResult.recoverCancellation

    member x.GetRecordStub (tyRes : ParseAndCheckResults) (pos: pos) (lines: LineStr[]) (line: LineStr) =
        async {
            let doc = docForText lines tyRes
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
        }
        |> x.AsCancellable (Path.GetFullPath tyRes.FileName)
        |> AsyncResult.recoverCancellation

    member x.GetInterfaceStub (tyRes : ParseAndCheckResults) (pos: pos) (lines: LineStr[]) (lineStr: LineStr) =
        async {
            let doc = docForText lines tyRes
            let! res = tryFindInterfaceExprInBufferAtPos codeGenServer pos doc
            match res with
            | None -> return CoreResponse.InfoRes "Interface at position not found"
            | Some interfaceData ->
                let! stubInfo = handleImplementInterface codeGenServer tyRes pos doc lines lineStr interfaceData
                match stubInfo with
                | Some (insertPosition, generatedCode) ->
                    return CoreResponse.Res (generatedCode, insertPosition)
                | None -> return CoreResponse.InfoRes "Interface at position not found"
        }
        |> x.AsCancellable (Path.GetFullPath tyRes.FileName)
        |> AsyncResult.recoverCancellation

    member x.GetAbstractClassStub (tyRes : ParseAndCheckResults) (objExprRange: range) (lines: LineStr[]) (lineStr: LineStr) =
        asyncResult {
            let doc = docForText lines tyRes
            let! abstractClass =
                AbstractClassStubGenerator.tryFindAbstractClassExprInBufferAtPos codeGenServer objExprRange.Start doc
                |> Async.map (Result.ofOption (fun _ -> CoreResponse.InfoRes "Abstract class at position not found"))
            let! (insertPosition, generatedCode) =
                AbstractClassStubGenerator.writeAbstractClassStub codeGenServer tyRes doc lines lineStr abstractClass
                |> Async.map (Result.ofOption (fun _ -> CoreResponse.InfoRes "Didn't need to write an abstract class"))
            return CoreResponse.Res (generatedCode, insertPosition)
        }
        |> AsyncResult.foldResult id id
        |> x.AsCancellable (Path.GetFullPath tyRes.FileName)
        |> AsyncResult.recoverCancellation

    member x.WorkspacePeek (dir: string) (deep: int) (excludedDirs: string list) = async {
        let d = state.ProjectController.PeekWorkspace dir deep excludedDirs
        return CoreResponse.Res d
    }

    member x.WorkspaceLoad (files: string list) (disableInMemoryProjectReferences: bool) tfmForScripts (generateBinlog: bool) = async {
        commandsLogger.info (Log.setMessage "Workspace loading started '{files}'" >> Log.addContextDestructured "files" files)
        checker.DisableInMemoryProjectReferences <- disableInMemoryProjectReferences
        let! res = state.ProjectController.LoadWorkspace files tfmForScripts onProjectLoaded generateBinlog
        commandsLogger.info (Log.setMessage "Workspace loading finished ")
        return CoreResponse.Res res
    }

    member x.Project projectFileName tfmForScripts (generateBinlog: bool)  = async {
        commandsLogger.info (Log.setMessage "Project loading '{file}'" >> Log.addContextDestructured "file" projectFileName)
        let! res = state.ProjectController.LoadProject projectFileName tfmForScripts onProjectLoaded generateBinlog
        return CoreResponse.Res res
    }

    member x.CheckUnusedDeclarations file: Async<unit> =
      asyncResult {
          let file = Path.GetFullPath file
          let isScript = file.EndsWith ".fsx"

          let! (opts, _) = state.TryGetFileCheckerOptionsWithSource file
          let tyResOpt = checker.TryGetRecentCheckResultsForFile(file, opts)
          match tyResOpt with
          | None -> ()
          | Some tyRes ->
              let! allUses = tyRes.GetCheckResults.GetAllUsesOfAllSymbolsInFile ()
              let unused = UnusedDeclarationsAnalyzer.getUnusedDeclarationRanges allUses isScript
              notify.Trigger (NotificationEvent.UnusedDeclarations (file, unused))
      }
      |> Async.Ignore<Result<unit, _>>

    member x.CheckSimplifiedNames file: Async<unit> =
        let file = Path.GetFullPath file
        asyncResult {
            let! (opts, source) =  state.TryGetFileCheckerOptionsWithLines file
            let tyResOpt = checker.TryGetRecentCheckResultsForFile(file, opts)
            match tyResOpt with
            | None -> ()
            | Some tyRes ->
                let getSourceLine lineNo = source.[lineNo - 1]
                let! simplified = SimplifyNames.getSimplifiableNames(tyRes.GetCheckResults, getSourceLine)
                let simplified = Array.ofList simplified
                notify.Trigger (NotificationEvent.SimplifyNames (file, simplified))
        }
        |> Async.Ignore<Result<unit, _>>
        |> x.AsCancellable file
        |> AsyncResult.recoverCancellationIgnore

    member x.CheckUnusedOpens file: Async<unit> =
        let file = Path.GetFullPath file
        asyncResult {
            let! (opts, source) =  state.TryGetFileCheckerOptionsWithLines file
            match checker.TryGetRecentCheckResultsForFile(file, opts) with
            | None ->
              return ()
            | Some tyRes ->
                let! unused = UnusedOpens.getUnusedOpens(tyRes.GetCheckResults, fun i -> source.[i - 1])
                notify.Trigger (NotificationEvent.UnusedOpens (file, (unused |> List.toArray)))

        }
        |> Async.Ignore<Result<unit, _>>
        |> x.AsCancellable file
        |> AsyncResult.recoverCancellationIgnore


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

    member x.ScopesForFile (file: string) = asyncResult {
        let file = Path.GetFullPath file
        let! (opts, sourceLines) = state.TryGetFileCheckerOptionsWithLines file
        let parseOpts = Utils.projectOptionsToParseOptions opts
        let allSource = sourceLines |> String.concat "\n"
        let! ast = checker.ParseFile(file, allSource, parseOpts)
        match ast.ParseTree with
        | None -> return! Error (ast.Errors |> Array.map string |> String.concat "\n")
        | Some ast' ->
            let ranges = Structure.getOutliningRanges sourceLines ast'
            return ranges
    }

    member __.SetDotnetSDKRoot(directory: System.IO.DirectoryInfo) = checker.SetDotnetRoot(directory)
    member __.SetFSIAdditionalArguments args = checker.SetFSIAdditionalArguments args

    member x.FormatDocument (file: SourceFilePath) =
      asyncResult {
          let filePath = Path.GetFullPath file
          let! (opts, lines) = x.TryGetFileCheckerOptionsWithLines filePath
          let source = String.concat "\n" lines
          let parsingOptions = Utils.projectOptionsToParseOptions opts
          let checker : FSharpChecker = checker.GetFSharpChecker()
          // ENHANCEMENT: consider caching the Fantomas configuration and reevaluate when the configuration file changes.
          let config =
              match Fantomas.Extras.EditorConfig.tryReadConfiguration filePath with
              | Some c -> c
              | None ->
                fantomasLogger.warn (Log.setMessage "No fantomas configuration found for file '{filePath}' or parent directories. Using the default configuration." >> Log.addContextDestructured "filePath" file)
                Fantomas.FormatConfig.FormatConfig.Default
          let! formatted =
              Fantomas.CodeFormatter.FormatDocumentAsync(filePath,
                                                         Fantomas.SourceOrigin.SourceString source,
                                                         config,
                                                         parsingOptions,
                                                         checker)
          return lines, formatted
      }
      |> AsyncResult.foldResult Some (fun _ -> None)

    member x.GetHighlighting (file: SourceFilePath) =
      let file = Path.GetFullPath file
      async {
        let! res = x.TryGetLatestTypeCheckResultsForFile file
        let res =
          match res with
          | Some res ->
            let r = res.GetCheckResults.GetSemanticClassification(None)
            Some r
          | None ->
            None
        return CoreResponse.Res res
      }

    member __.SetWorkspaceRoot (root: string option) =
      workspaceRoot <- root
      linterConfiguration <- Lint.loadConfiguration workspaceRoot linterConfigFileRelativePath

    member __.SetLinterConfigRelativePath (relativePath: string option) =
      linterConfigFileRelativePath <- relativePath
      linterConfiguration <- Lint.loadConfiguration workspaceRoot linterConfigFileRelativePath


    member __.FSharpLiterate (file: SourceFilePath) =
      let file = Path.GetFullPath file
      async {
        let cnt =
          match state.TryGetFileSource file with
          | Ok ctn -> String.concat "\n" ctn
          | _ ->  File.ReadAllText file
        let parsedFile =
          if Utils.isAScript file then
            FSharp.Formatting.Literate.Literate.ParseScriptString cnt
          else
             FSharp.Formatting.Literate.Literate.ParseMarkdownString cnt

        let html =  FSharp.Formatting.Literate.Literate.ToHtml parsedFile
        return CoreResponse.Res html
      }

    member __.PipelineHints (tyRes : ParseAndCheckResults) =
      let file = Path.GetFullPath tyRes.FileName
      asyncResult {
        let! contents = state.TryGetFileSource file
        let getGenerics line (token: FSharpTokenInfo) = async {
            let lineStr = contents.[line]
            let! res = tyRes.TryGetToolTip (Pos.fromZ line token.RightColumn) lineStr
            match res with
            | Ok tip ->
              return TipFormatter.extractGenerics tip
            | _ ->
              commandsLogger.info (Log.setMessage "ParameterHints - No tooltips for token: '{token}'\n Line: \n{line}" >> Log.addContextDestructured "token" token >> Log.addContextDestructured "line" lineStr)
              return []
        }

        let! hints =
          contents
          |> Array.map (Lexer.tokenizeLine [||])
          |> Array.pairwise
          |> Array.mapi (fun currentIndex (currentTokens, nextTokens) -> currentIndex, currentTokens, nextTokens)
          |> Array.choose (fun (id, tok, nextTok) ->
              match nextTok with
              | x::y::xs when x.TokenName.ToUpper() = "WHITESPACE" && y.TokenName.ToUpper() = "INFIX_BAR_OP" ->
                Some (async {
                  let! gens = getGenerics (id + 1) y
                  return id, gens
                })
              | y::xs when y.TokenName.ToUpper() = "INFIX_BAR_OP" ->
                Some (async {
                  let! gens = getGenerics (id + 1) y
                  return id, gens
                })
              | _ ->
                None
          )
          |> Async.Parallel

        return CoreResponse.Res hints
      }
      |> AsyncResult.foldResult id (fun _ -> CoreResponse.InfoRes "Couldn't find file content")

