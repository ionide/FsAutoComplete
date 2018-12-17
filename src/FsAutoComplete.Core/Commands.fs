namespace FsAutoComplete

open System
open System.IO
open FSharp.Compiler.SourceCodeServices
open FSharpLint.Application
open FsAutoComplete.UnionPatternMatchCaseGenerator
open FsAutoComplete.RecordStubGenerator
open FsAutoComplete.InterfaceStubGenerator
open System.Threading
open Utils
open System.Reflection
open FSharp.Compiler.Range
open FSharp.Analyzers

[<RequireQualifiedAccess>]
type CoreResponse =
    | InfoRes of text: string
    | ErrorRes of text: string
    | HelpText of name: string * tip: FSharpToolTipText * additionalEdit: (string * int * int * string) option
    | HelpTextSimple of name: string * tip: string
    | Project of projectFileName: ProjectFilePath * projectFiles: List<SourceFilePath> * outFileOpt : string option * references : ProjectFilePath list * logMap : Map<string,string> * extra: ExtraProjectInfoData * additionals : Map<string,string>
    | ProjectError of errorDetails: GetProjectOptionsErrors
    | ProjectLoading of projectFileName: ProjectFilePath
    | WorkspacePeek of found: WorkspacePeek.Interesting list
    | WorkspaceLoad of finished: bool
    | Completion of decls: FSharpDeclarationListItem[] * includeKeywords: bool
    | SymbolUse of symbol: FSharpSymbolUse * uses: FSharpSymbolUse[]
    | SignatureData of typ: string * parms: (string * string) list list
    | Help of data: string
    | Methods of meth: FSharpMethodGroup * commas: int
    | Errors of errors: FSharpErrorInfo[] * file: string
    | Colorizations of colorizations: (range * SemanticClassificationType) []
    | FindDeclaration of result: FindDeclarationResult
    | FindTypeDeclaration of range: range
    | Declarations of decls: (FSharpNavigationTopLevelDeclaration * string) []
    | ToolTip of tip: FSharpToolTipText<string> * signature: string * footer: string * typeDoc: string option
    | FormattedDocumentation of tip: FSharpToolTipText<string> * signature: (string * (string [] * string [] * string [])) * footer: string * cn: string
    | TypeSig of tip: FSharpToolTipText<string>
    | CompilerLocation of fcs: string option * fsi: string option * msbuild: string option
    | Lint of warnings: LintWarning.Warning list
    | ResolveNamespaces of word: string * opens: (string * string * InsertContext * bool) list * qualifies: (string * string) list
    | UnionCase of text: string * position: pos
    | RecordStub of text: string * position: pos
    | InterfaceStub of text: string * position: pos
    | UnusedDeclarations of decls: (range * bool)[]
    | UnusedOpens of opens: range[]
    | SimplifiedName of names: (range * string)[]
    | Compile of errors: Microsoft.FSharp.Compiler.SourceCodeServices.FSharpErrorInfo[] * code: int
    | Analyzer of messages: SDK.Message [] * file: string
    | SymbolUseRange of ranges: SymbolCache.SymbolUseRange[]

[<RequireQualifiedAccess>]
type NotificationEvent =
    | ParseError of CoreResponse
    | Workspace of CoreResponse
    | AnalyzerMessage of CoreResponse
    | UnusedOpens of CoreResponse
    | Lint of CoreResponse
    | Analyzer of CoreResponse
    | UnusedDeclarations of CoreResponse
    | SimplifyNames of CoreResponse

type Commands (serialize : Serializer) =

    let checker = FSharpCompilerServiceChecker()
    let state = State.Initial
    let fsharpLintConfig = ConfigurationManager.ConfigurationManager()
    let fileParsed = Event<FSharpParseFileResults>()
    let fileChecked = Event<ParseAndCheckResults * string * int>()
    let fileInProjectChecked = Event<SourceFilePath>()
    let mutable notifyErrorsInBackground = true
    let mutable useSymbolCache = false
    let mutable lastVersionChecked = -1
    let mutable isWorkspaceReady = false

    let notify = Event<NotificationEvent>()

    let workspaceReady = Event<unit>()

    let rec backgroundChecker () =
        async {
            try
                let opt = state.BackgroundProjects.First
                // printfn "1. BACKGROUND QUEUE: %A" (state.BackgroundProjects |> Seq.map (fun n -> n.ProjectFileName))
                // printfn "BACKGROUND CHECKER - PARSING STARTED: %s" (opt.ProjectFileName)
                checker.CheckProjectInBackground opt
            with
            | _ ->
                // printfn "BACKGROUND CHECKER - NO PROJECTS"
                ()
        } |> Async.Start

    let updateSymbolUsesCache (filename: FilePath) (check: FSharpCheckFileResults) =
        async {
            try
                let! symbols = check.GetAllUsesOfAllSymbolsInFile()
                SymbolCache.sendSymbols serialize filename symbols
            with
            | _ -> ()
        }

    do fileParsed.Publish.Add (fun parseRes ->
       let decls = parseRes.GetNavigationItems().Declarations
       state.NavigationDeclarations.[parseRes.FileName] <- decls
    )

    do checker.FileChecked.Add (fun (n,_) ->
        async {
            try
                let opts = state.FileCheckOptions.[n]
                let! res = checker.GetBackgroundCheckResultsForFileInProject(n, opts)
                fileChecked.Trigger (res, res.FileName, -1)
            with
            | _ -> ()
        } |> Async.Start
    )

    do fileChecked.Publish.Add (fun (parseAndCheck, file, version) ->
        async {
            try

                let checkErrors = parseAndCheck.GetParseResults.Errors
                let parseErrors = parseAndCheck.GetCheckResults.Errors
                let errors = Array.append checkErrors parseErrors

                CoreResponse.Errors (errors, file)
                |> NotificationEvent.ParseError
                |> notify.Trigger
            with
            | _ -> ()
        }
        |> if notifyErrorsInBackground then Async.Start else ignore

        async {
            try
                let analyzers = state.Analyzers.Values |> Seq.collect id
                if analyzers |> Seq.length > 0 then
                    match parseAndCheck.GetParseResults.ParseTree, parseAndCheck.GetCheckResults.ImplementationFile with
                    | Some pt, Some tast ->
                        let context : SDK.Context = {
                            FileName = file
                            Content = state.Files.[file].Lines
                            ParseTree = pt
                            TypedTree = tast
                            Symbols = parseAndCheck.GetCheckResults.PartialAssemblySignature.Entities |> Seq.toList
                        }
                        let result = analyzers |> Seq.collect (fun n -> n context)
                        CoreResponse.Analyzer (Seq.toArray result, file)
                        |> NotificationEvent.AnalyzerMessage
                        |> notify.Trigger
                    | _ -> ()

            with
            | _ -> ()
        } |> Async.Start

        updateSymbolUsesCache file parseAndCheck.GetCheckResults
        |> if useSymbolCache then Async.Start else ignore
    )


    do checker.ProjectChecked.Add(fun (o, _) ->
        if notifyErrorsInBackground then
            state.FileCheckOptions.ToArray()
            |> Array.tryPick(fun (KeyValue(_, v)) -> if v.ProjectFileName = Path.GetFullPath o then Some v else None )
            |> Option.iter ( state.BackgroundProjects.TryRemove >> ignore)
            backgroundChecker () ) //When one project finished, start new one

    let normalizeOptions (opts : FSharpProjectOptions) =
        { opts with
            SourceFiles = opts.SourceFiles |> Array.map (Path.GetFullPath)
            OtherOptions = opts.OtherOptions |> Array.map (fun n -> if FscArguments.isCompileFile(n) then Path.GetFullPath n else n)
        }

    let parseFilesInTheBackground files =
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
                            state.Files.[file] <- {Touched = DateTime.Now; Lines = ctn }
                            Some (ctn)
                        | None -> None
                    match sourceOpt with
                    | None -> ()
                    | Some source ->
                        let opts = state.FileCheckOptions.[file] |> Utils.projectOptionsToParseOptions
                        let parseRes = checker.ParseFile(file, source |> String.concat "\n", opts) |> Async.RunSynchronously
                        fileParsed.Trigger parseRes
                with
                | :? System.Threading.ThreadAbortException as ex ->
                    // on mono, if background parsing is aborted a ThreadAbortException
                    // is raised, who can be ignored
                    ()
                | ex ->
                    printfn "Failed to parse file '%s' exn %A" file ex
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

    let onProjectLoaded projectFileName (response: ProjectCrackerCache) =
        for file in response.Files do
            state.FileCheckOptions.[file] <- normalizeOptions response.Options

        response.Files
        |> parseFilesInTheBackground
        |> Async.Start

    let getGitHash () =
        Assembly.GetEntryAssembly().GetCustomAttributes(typeof<AssemblyMetadataAttribute>, true)
        |> Seq.cast<AssemblyMetadataAttribute>
        |> Seq.map (fun (m) -> m.Key,m.Value)
        |> Seq.tryPick (fun (x,y) -> if x = "githash" && not (String.IsNullOrWhiteSpace(y)) then Some y else None )

    member __.Notify = notify.Publish

    member __.WorkspaceReady = workspaceReady.Publish

    member __.FileChecked = fileChecked.Publish

    member __.IsWorkspaceReady
        with get() = isWorkspaceReady
        and set(value) = isWorkspaceReady <- value
    member __.NotifyErrorsInBackground
        with get() = notifyErrorsInBackground
        and set(value) = notifyErrorsInBackground <- value

    member __.UseSymbolCache
        with get() = useSymbolCache
        and set(value) = useSymbolCache <- value

    member __.LastVersionChecked
        with get() = lastVersionChecked

    member private x.MapResultAsync (successToString: 'a -> Async<CoreResponse>, ?failureToString: string -> CoreResponse) =
        Async.bind <| function
            // A failure is only info here, as this command is expected to be
            // used 'on idle', and frequent errors are expected.
            | ResultOrString.Error e -> async.Return [(defaultArg failureToString CoreResponse.InfoRes) e]
            | ResultOrString.Ok r -> successToString r |> Async.map List.singleton

    member private x.MapResult (successToString: 'a -> CoreResponse, ?failureToString: string -> CoreResponse) =
        x.MapResultAsync ((fun x -> successToString x |> async.Return), ?failureToString = failureToString)

    member private x.AsCancellable (filename : SourceFilePath) (action : Async<CoreResponse list>) =
        let cts = new CancellationTokenSource()
        state.AddCancellationToken(filename, cts)
        Async.StartCatchCancellation(action, cts.Token)
        |> Async.Catch
        |> Async.map (function Choice1Of2 res -> res | Choice2Of2 err -> [CoreResponse.InfoRes (sprintf "Request cancelled (exn was %A)" err)])

    member private x.CancelQueue (filename : SourceFilePath) =
        state.GetCancellationTokens filename |> List.iter (fun cts -> cts.Cancel() )

    member x.TryGetRecentTypeCheckResultsForFile = checker.TryGetRecentCheckResultsForFile

    member x.CheckFileInProject = checker.ParseAndCheckFileInProject'

    member x.TryGetFileCheckerOptionsWithLinesAndLineStr = state.TryGetFileCheckerOptionsWithLinesAndLineStr
    member x.TryGetFileCheckerOptionsWithLines = state.TryGetFileCheckerOptionsWithLines
    member x.Files = state.Files

    member x.Parse file lines version =
        let file = Path.GetFullPath file
        do x.CancelQueue file
        async {
            let colorizations = state.ColorizationOutput
            let parse' fileName text options =
                async {
                    let! result = checker.ParseAndCheckFileInProject(fileName, version, text, options)
                    return
                        match result with
                        | ResultOrString.Error e -> [CoreResponse.ErrorRes e]
                        | ResultOrString.Ok (parseAndCheck) ->
                            let parseResult = parseAndCheck.GetParseResults
                            let results = parseAndCheck.GetCheckResults
                            do fileParsed.Trigger parseResult
                            do fileInProjectChecked.Trigger file
                            do lastVersionChecked <- version
                            do fileChecked.Trigger (parseAndCheck, fileName, version)
                            let errors = Array.append results.Errors parseResult.Errors
                            if colorizations then
                                [   CoreResponse.Errors (errors, fileName)
                                    CoreResponse.Colorizations (results.GetSemanticClassification None) ]
                            else [ CoreResponse.Errors (errors, fileName) ]
                }
            let text = String.concat "\n" lines

            if Utils.isAScript file then
                let! checkOptions = checker.GetProjectOptionsFromScript(file, text)
                state.AddFileTextAndCheckerOptions(file, lines, normalizeOptions checkOptions)
                return! parse' file text checkOptions
            else
                let! checkOptions =
                    match state.GetCheckerOptions(file, lines) with
                    | Some c -> async.Return c
                    | None -> async {
                        let! checkOptions = checker.GetProjectOptionsFromScript(file, text)
                        state.AddFileTextAndCheckerOptions(file, lines, normalizeOptions checkOptions)
                        return checkOptions
                    }
                return! parse' file text checkOptions
        } |> x.AsCancellable file

    member x.ParseAndCheckProjectsInBackgroundForFile file = async {
        match checker.GetDependingProjects file (state.FileCheckOptions.ToArray() |> Array.map (fun (KeyValue(k, v)) -> k,v) |> Seq.ofArray) with
        | Some (p, projs) ->
            state.EnqueueProjectForBackgroundParsing(p, 0)
            projs |> Seq.iter (fun p -> state.EnqueueProjectForBackgroundParsing (p,1))
            // printfn "3. BACKGROUND QUEUE: %A" (state.BackgroundProjects |> Seq.map (fun n -> n.ProjectFileName))
        | None -> ()
        return [CoreResponse.Errors ([||], "") ]
    }

    member x.ParseProjectsForFile file = async {
        let! res = checker.ParseProjectsForFile(file, state.FileCheckOptions.ToArray() |> Array.map (fun (KeyValue(k, v)) -> k,v) |> Seq.ofArray)
        return
            match res with
            | ResultOrString.Error e -> [CoreResponse.ErrorRes e]
            | ResultOrString.Ok results ->
                let errors = results |> Array.collect (fun r -> r.Errors)
                [ CoreResponse.Errors (errors, file)]
    }

    member private __.ToProjectCache (opts, extraInfo, projectFiles, logMap) =
        let outFileOpt =
            match extraInfo.ProjectSdkType with
            | ProjectSdkType.Verbose v ->
                Some (v.TargetPath)
            | ProjectSdkType.ProjectJson ->
                FscArguments.outputFile (Path.GetDirectoryName(opts.ProjectFileName)) (opts.OtherOptions |> List.ofArray)
            | ProjectSdkType.DotnetSdk v ->
                Some (v.TargetPath)
        let references = FscArguments.references (opts.OtherOptions |> List.ofArray)
        let projectFiles = projectFiles |> List.map (Path.GetFullPath >> Utils.normalizePath)

        let cached = {
            ProjectCrackerCache.Options = opts
            Files = projectFiles
            OutFile = outFileOpt
            References = references
            Log = logMap
            ExtraInfo = extraInfo
        }

        (opts.ProjectFileName, cached)

    member x.Project projectFileName verbose onChange = async {
        let projectFileName = Path.GetFullPath projectFileName
        //printfn "project path: %s" projectFileName
        let project =
            match state.Projects.TryFind projectFileName with
            | Some prj -> prj
            | None ->
                let proj = new Project(projectFileName, onChange)
                state.Projects.[projectFileName] <- proj
                proj
        //printfn "project:\n%A" project
        let projResponse =
            match project.Response with
            | Some response ->
                Result.Ok (projectFileName, response)
            | None ->
                match projectFileName |> Workspace.parseProject verbose |> Result.map (x.ToProjectCache) with
                | Result.Ok (projectFileName, response) ->
                    project.Response <- Some response
                    Result.Ok (projectFileName, response)
                | Result.Error error ->
                    project.Response <- None
                    Result.Error error
        //printfn "Project response: %A" projResponse
        return
            match projResponse with
            | Result.Ok (projectFileName, response) ->
                onProjectLoaded projectFileName response
                [ CoreResponse.Project (projectFileName, response.Files, response.OutFile, response.References, response.Log, response.ExtraInfo, Map.empty) ]
            | Result.Error error ->
                [ CoreResponse.ProjectError error ]
    }

    member x.Declarations file lines version = async {
        let file = Path.GetFullPath file
        match state.TryGetFileCheckerOptionsWithSource file, lines with
        | ResultOrString.Error s, None -> return [CoreResponse.ErrorRes s]
        | ResultOrString.Error _, Some l ->
            let text = String.concat "\n" l
            let files = Array.singleton file
            let parseOptions = { FSharpParsingOptions.Default with SourceFiles = files}
            let! decls = checker.GetDeclarations(file, text, parseOptions, version)
            let decls = decls |> Array.map (fun a -> a,file)
            return [CoreResponse.Declarations decls]
        | ResultOrString.Ok (checkOptions, source), _ ->
            let text =
                match lines with
                | Some l -> String.concat "\n" l
                | None -> source

            let parseOptions = Utils.projectOptionsToParseOptions checkOptions
            let! decls = checker.GetDeclarations(file, text, parseOptions, version)

            state.NavigationDeclarations.[file] <- decls

            let decls = decls |> Array.map (fun a -> a,file)
            return [CoreResponse.Declarations decls]
    }

    member x.DeclarationsInProjects () = async {
        let decls =
            state.NavigationDeclarations.ToArray()
            |> Array.collect (fun (KeyValue(p, decls)) -> decls |> Array.map (fun d -> d,p))
        return [CoreResponse.Declarations decls]
    }

    member __.Helptext sym =
        match KeywordList.tryGetKeywordDescription sym with
        | Some s ->
            [CoreResponse.HelpTextSimple (sym, s)]
        | None ->
        match KeywordList.tryGetHashDescription sym with
        | Some s ->
            [CoreResponse.HelpTextSimple (sym, s)]
        | None ->
        match state.Declarations.TryFind sym with
        | None -> //Isn't in sync filled cache, we don't have result
            [CoreResponse.ErrorRes (sprintf "No help text available for symbol '%s'" sym)]
        | Some (decl, pos, fn) -> //Is in sync filled cache, try to get results from async filled cahces or calculate if it's not there
            let source =
                state.Files.TryFind fn
                |> Option.map (fun n -> n.Lines)
            match source with
            | None -> [CoreResponse.ErrorRes (sprintf "No help text available for symbol '%s'" sym)]
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
                [CoreResponse.HelpText (sym, tip, n)]

    member x.CompilerLocation () = [CoreResponse.CompilerLocation (Environment.fsc, Environment.fsi, Environment.msbuild)]
    member x.Colorization enabled = state.ColorizationOutput <- enabled
    member x.Error msg = [CoreResponse.ErrorRes msg]

    member x.Completion (tyRes : ParseAndCheckResults) (pos: pos) lineStr (lines : string[]) (fileName : SourceFilePath) filter includeKeywords includeExternal =
        async {
            let getAllSymbols () =
                if includeExternal then tyRes.GetAllEntities() else []
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

                    match firstMatchOpt with
                    | None -> [CoreResponse.Completion (decls, includeKeywords)]
                    | Some d ->
                        let insert = calculateNamespaceInser d pos getLine
                        [CoreResponse.HelpText (d.Name, d.DescriptionText, insert)
                         CoreResponse.Completion (decls, includeKeywords)]

                | None -> [CoreResponse.ErrorRes "Timed out while fetching completions"]
        }

    member x.ToolTip (tyRes : ParseAndCheckResults) (pos: pos) lineStr =
        tyRes.TryGetToolTipEnhanced pos lineStr
        |> x.MapResult CoreResponse.ToolTip
        |> x.AsCancellable (Path.GetFullPath tyRes.FileName)

    member x.FormattedDocumentation (tyRes : ParseAndCheckResults) (pos: pos) lineStr =
        tyRes.TryGetFormattedDocumentation pos lineStr
        |> x.MapResult CoreResponse.FormattedDocumentation
        |> x.AsCancellable (Path.GetFullPath tyRes.FileName)

    member x.Typesig (tyRes : ParseAndCheckResults) (pos: pos) lineStr =
        tyRes.TryGetToolTip pos lineStr
        |> x.MapResult CoreResponse.TypeSig
        |> x.AsCancellable (Path.GetFullPath tyRes.FileName)

    member x.SymbolUse (tyRes : ParseAndCheckResults) (pos: pos) lineStr =
        tyRes.TryGetSymbolUse pos lineStr
        |> x.MapResult CoreResponse.SymbolUse
        |> x.AsCancellable (Path.GetFullPath tyRes.FileName)

    member x.SignatureData (tyRes : ParseAndCheckResults) (pos: pos) lineStr =
        tyRes.TryGetSignatureData pos lineStr
        |> x.MapResult CoreResponse.SignatureData
        |> x.AsCancellable (Path.GetFullPath tyRes.FileName)

    member x.Help (tyRes : ParseAndCheckResults) (pos: pos) lineStr =
        tyRes.TryGetF1Help pos lineStr
        |> x.MapResult CoreResponse.Help
        |> x.AsCancellable (Path.GetFullPath tyRes.FileName)

    member x.SymbolUseProject (tyRes : ParseAndCheckResults) (pos: pos) lineStr =
        let fn = tyRes.FileName
        tyRes.TryGetSymbolUse pos lineStr |> x.MapResultAsync (fun (sym, usages) ->
            async {
                let fsym = sym.Symbol
                if fsym.IsPrivateToFile then
                    return CoreResponse.SymbolUse (sym, usages)
                elif useSymbolCache then
                    let! res =  SymbolCache.getSymbols fsym.FullName
                    match res with
                    | None ->
                        if fsym.IsInternalToProject then
                            let opts = state.FileCheckOptions.[tyRes.FileName]
                            let! symbols = checker.GetUsesOfSymbol (fn, [tyRes.FileName, opts] , sym.Symbol)
                            return CoreResponse.SymbolUse (sym, symbols)
                        else
                            let! symbols = checker.GetUsesOfSymbol (fn, state.FileCheckOptions.ToArray() |> Array.map (fun (KeyValue(k, v)) -> k,v) |> Seq.ofArray, sym.Symbol)
                            return CoreResponse.SymbolUse (sym, symbols)
                    | Some res ->
                        return CoreResponse.SymbolUseRange res
                elif fsym.IsInternalToProject then
                    let opts = state.FileCheckOptions.[tyRes.FileName]
                    let! symbols = checker.GetUsesOfSymbol (fn, [tyRes.FileName, opts] , sym.Symbol)
                    return CoreResponse.SymbolUse (sym, symbols)
                else
                    let! symbols = checker.GetUsesOfSymbol (fn, state.FileCheckOptions.ToArray() |> Array.map (fun (KeyValue(k, v)) -> k,v) |> Seq.ofArray, sym.Symbol)
                    return CoreResponse.SymbolUse (sym, symbols)
            })
        |> x.AsCancellable (Path.GetFullPath fn)

    member x.SymbolImplementationProject (tyRes : ParseAndCheckResults) (pos: pos) lineStr =
        let fn = tyRes.FileName
        let filterSymbols symbols =
            symbols
            |> Array.where (fun (su: FSharpSymbolUse) -> su.IsFromDispatchSlotImplementation || (su.IsFromType && not (UntypedAstUtils.isTypedBindingAtPosition tyRes.GetAST su.RangeAlternate )) )

        tyRes.TryGetSymbolUse pos lineStr |> x.SerializeResultAsync (fun _ (sym, usages) ->
            async {
                let fsym = sym.Symbol
                if fsym.IsPrivateToFile then
                    return Response.symbolImplementation serialize (sym, filterSymbols usages)
                elif useSymbolCache then
                    let! res =  SymbolCache.getImplementation fsym.FullName
                    if res = "ERROR" then
                        if fsym.IsInternalToProject then
                            let opts = state.FileCheckOptions.[tyRes.FileName]
                            let! symbols = checker.GetUsesOfSymbol (fn, [tyRes.FileName, opts] , sym.Symbol)
                            return Response.symbolUse serialize (sym, filterSymbols symbols )
                        else
                            let! symbols = checker.GetUsesOfSymbol (fn, state.FileCheckOptions.ToArray() |> Array.map (fun (KeyValue(k, v)) -> k,v) |> Seq.ofArray, sym.Symbol)
                            return Response.symbolUse serialize (sym, filterSymbols symbols)
                    else
                        return res
                elif fsym.IsInternalToProject then
                    let opts = state.FileCheckOptions.[tyRes.FileName]
                    let! symbols = checker.GetUsesOfSymbol (fn, [tyRes.FileName, opts] , sym.Symbol)
                    return Response.symbolUse serialize (sym, filterSymbols symbols )
                else
                    let! symbols = checker.GetUsesOfSymbol (fn, state.FileCheckOptions.ToArray() |> Array.map (fun (KeyValue(k, v)) -> k,v) |> Seq.ofArray, sym.Symbol)
                    let symbols = filterSymbols symbols
                    return Response.symbolUse serialize (sym, symbols )
            })
        |> x.AsCancellable (Path.GetFullPath fn)

    member x.FindDeclaration (tyRes : ParseAndCheckResults) (pos: pos) lineStr =
        tyRes.TryFindDeclaration pos lineStr
        |> x.MapResult (CoreResponse.FindDeclaration, CoreResponse.ErrorRes)
        |> x.AsCancellable (Path.GetFullPath tyRes.FileName)

    member x.FindTypeDeclaration (tyRes : ParseAndCheckResults) (pos: pos) lineStr =
        tyRes.TryFindTypeDeclaration pos lineStr
        |> x.MapResult (CoreResponse.FindTypeDeclaration, CoreResponse.ErrorRes)
        |> x.AsCancellable (Path.GetFullPath tyRes.FileName)

    member x.Methods (tyRes : ParseAndCheckResults) (pos: pos) (lines: LineStr[]) =
        tyRes.TryGetMethodOverrides lines pos
        |> x.MapResult (CoreResponse.Methods, CoreResponse.ErrorRes)
        |> x.AsCancellable (Path.GetFullPath tyRes.FileName)

    member x.Lint (file: SourceFilePath) =


        let file = Path.GetFullPath file
        async {
            let res =
                match state.TryGetFileCheckerOptionsWithSource file with
                | Error s -> [CoreResponse.ErrorRes s]
                | Ok (options, source) ->
                    let tyResOpt = checker.TryGetRecentCheckResultsForFile(file, options)

                    match tyResOpt with
                    | None -> [ CoreResponse.InfoRes "Cached typecheck results not yet available"]
                    | Some tyRes ->
                        match tyRes.GetAST with
                        | None -> [ CoreResponse.InfoRes "Something went wrong during parsing"]
                        | Some tree ->
                            try
                                fsharpLintConfig.LoadConfigurationForProject file
                                let opts = fsharpLintConfig.GetConfigurationForProject (file)
                                let res =
                                    Lint.lintParsedSource
                                        { Lint.OptionalLintParameters.Default with Configuration = Some opts}
                                        { Ast = tree
                                          Source = source
                                          TypeCheckResults = Some tyRes.GetCheckResults }
                                let res' =
                                    match res with
                                    | LintResult.Failure _ -> [ CoreResponse.InfoRes "Something went wrong, linter failed"]
                                    | LintResult.Success warnings ->
                                        let res = CoreResponse.Lint warnings
                                        notify.Trigger (NotificationEvent.Lint res)
                                        [ res ]

                                res'
                            with _ex ->
                                [ CoreResponse.InfoRes "Something went wrong during linter"]
            return res
        } |> x.AsCancellable file

    member x.GetNamespaceSuggestions (tyRes : ParseAndCheckResults) (pos: pos) (line: LineStr) =
        async {
            match tyRes.GetAST with
            | None -> return [CoreResponse.InfoRes "Parsed Tree not avaliable"]
            | Some parsedTree ->
            match Parsing.findLongIdents(pos.Column, line) with
            | None -> return [CoreResponse.InfoRes "Ident not found"]
            | Some (_,idents) ->
            match UntypedParseImpl.GetEntityKind(pos, parsedTree)  with
            | None -> return [CoreResponse.InfoRes "EntityKind not found"]
            | Some entityKind ->

            let symbol = Lexer.getSymbol pos.Line pos.Column line SymbolLookupKind.Fuzzy [||]
            match symbol with
            | None -> return [CoreResponse.InfoRes "Symbol at position not found"]
            | Some sym ->


            let entities = tyRes.GetAllEntities ()

            let isAttribute = entityKind = EntityKind.Attribute
            let entities =
                entities |> List.filter (fun e ->
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
                |> List.map (fun ident -> { Ident = ident; Resolved = false})
                |> List.toArray

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
                |> Seq.choose (fun (entity, ctx) -> entity.Namespace |> Option.map (fun ns -> ns, entity.Name, ctx))
                |> Seq.groupBy (fun (ns, _, _) -> ns)
                |> Seq.map (fun (ns, xs) ->
                    ns,
                    xs
                    |> Seq.map (fun (_, name, ctx) -> name, ctx)
                    |> Seq.distinctBy (fun (name, _) -> name)
                    |> Seq.sortBy fst
                    |> Seq.toArray)
                |> Seq.collect (fun (ns, names) ->
                    let multipleNames = names |> Array.length > 1
                    names |> Seq.map (fun (name, ctx) -> ns, name, ctx, multipleNames))
                |> Seq.toList

            let qualifySymbolActions =
                candidates
                |> Seq.map (fun (entity, _) -> entity.FullRelativeName, entity.Qualifier)
                |> Seq.distinct
                |> Seq.sort
                |> Seq.toList

            return [ CoreResponse.ResolveNamespaces (word, openNamespace, qualifySymbolActions) ]
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
            | None -> return [CoreResponse.InfoRes "Union at position not found"]
            | Some (patMatchExpr, unionTypeDefinition, insertionPos) ->

            if shouldGenerateUnionPatternMatchCases patMatchExpr unionTypeDefinition then
                let result = formatMatchExpr insertionPos "$1" patMatchExpr unionTypeDefinition
                let pos = mkPos insertionPos.InsertionPos.Line insertionPos.InsertionPos.Column

                return [CoreResponse.UnionCase (result, pos) ]
            else
                return [CoreResponse.InfoRes "Union at position not found"]
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
            | None -> return [CoreResponse.InfoRes "Record at position not found"]
            | Some(recordEpr, (Some recordDefinition), insertionPos) ->
                if shouldGenerateRecordStub recordEpr recordDefinition then
                    let result = formatRecord insertionPos "$1" recordDefinition recordEpr.FieldExprList
                    let pos = mkPos insertionPos.InsertionPos.Line insertionPos.InsertionPos.Column
                    return [CoreResponse.RecordStub (result, pos)]
                else
                    return [CoreResponse.InfoRes "Record at position not found"]
            | _ -> return [CoreResponse.InfoRes "Record at position not found"]
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
            | None -> return [CoreResponse.InfoRes "Interface at position not found"]
            | Some interfaceData ->
                let! stubInfo = handleImplementInterface codeGenServer pos doc lines lineStr interfaceData

                match stubInfo with
                | Some (insertPosition, generatedCode) ->
                    return [CoreResponse.InterfaceStub (generatedCode, insertPosition)]
                | None -> return [CoreResponse.InfoRes "Interface at position not found"]
        } |> x.AsCancellable (Path.GetFullPath tyRes.FileName)

    member x.WorkspacePeek (dir: string) (deep: int) (excludedDirs: string list) = async {
        let d = WorkspacePeek.peek dir deep excludedDirs
        state.WorkspaceRoot <- dir

        return [CoreResponse.WorkspacePeek d]
    }

    member x.WorkspaceLoad onChange (files: string list) (disableInMemoryProjectReferences: bool) = async {
        checker.DisableInMemoryProjectReferences <- disableInMemoryProjectReferences
        //TODO check full path
        let projectFileNames = files |> List.map Path.GetFullPath

        let projects =
            projectFileNames
            |> List.map (fun projectFileName -> projectFileName, new Project(projectFileName, onChange))

        for projectFileName, proj in projects do
            state.Projects.[projectFileName] <- proj

        let projectLoadedSuccessfully projectFileName response =
            let project =
                match state.Projects.TryFind projectFileName with
                | Some prj -> prj
                | None ->
                    let proj = new Project(projectFileName, onChange)
                    state.Projects.[projectFileName] <- proj
                    proj

            project.Response <- Some response

            onProjectLoaded projectFileName response

        let rec onLoaded p =
            match p with
            | WorkspaceProjectState.Loading projectFileName ->
                CoreResponse.ProjectLoading projectFileName
                |> NotificationEvent.Workspace
                |> notify.Trigger
            | WorkspaceProjectState.Loaded (opts, extraInfo, projectFiles, logMap) ->
                let projectFileName, response = x.ToProjectCache(opts, extraInfo, projectFiles, logMap)
                projectLoadedSuccessfully projectFileName response
                CoreResponse.Project (projectFileName, response.Files, response.OutFile, response.References, response.Log, response.ExtraInfo, Map.empty)
                |> NotificationEvent.Workspace
                |> notify.Trigger
            | WorkspaceProjectState.Failed (projectFileName, error) ->
                CoreResponse.ProjectError error
                |> NotificationEvent.Workspace
                |> notify.Trigger

        CoreResponse.WorkspaceLoad false
        |> NotificationEvent.Workspace
        |> notify.Trigger

        // this is to delay the project loading notification (of this thread)
        // after the workspaceload started response returned below in outer async
        // Make test output repeteable, and notification in correct order
        match Environment.workspaceLoadDelay() with
        | delay when delay > TimeSpan.Zero ->
            do! Async.Sleep(Environment.workspaceLoadDelay().TotalMilliseconds |> int)
        | _ -> ()

        do! Workspace.loadInBackground onLoaded false (projects |> List.map snd)

        CoreResponse.WorkspaceLoad true
        |> NotificationEvent.Workspace
        |> notify.Trigger

        x.IsWorkspaceReady <- true
        workspaceReady.Trigger ()


        return [CoreResponse.WorkspaceLoad true]
    }

    member x.GetUnusedDeclarations file =
        let file = Path.GetFullPath file
        let isScript = file.EndsWith ".fsx"

        async {
            match state.TryGetFileCheckerOptionsWithSource file with
            | Error s ->  return [CoreResponse.ErrorRes s]
            | Ok (opts, _) ->
                let tyResOpt = checker.TryGetRecentCheckResultsForFile(file, opts)
                match tyResOpt with
                | None -> return [ CoreResponse.InfoRes "Cached typecheck results not yet available"]
                | Some tyRes ->
                    let! allUses = tyRes.GetCheckResults.GetAllUsesOfAllSymbolsInFile ()
                    let unused = UnusedDeclarationsAnalyzer.getUnusedDeclarationRanges allUses isScript
                    let res = CoreResponse.UnusedDeclarations unused
                    notify.Trigger (NotificationEvent.UnusedDeclarations res)
                    return [ res ]
        } |> x.AsCancellable file

    member x.GetSimplifiedNames file =
        let file = Path.GetFullPath file

        async {
            match state.TryGetFileCheckerOptionsWithLines file with
            | Error s ->  return [CoreResponse.ErrorRes s]
            | Ok (opts, source) ->
                let tyResOpt = checker.TryGetRecentCheckResultsForFile(file, opts)
                match tyResOpt with
                | None -> return [ CoreResponse.InfoRes "Cached typecheck results not yet available"]
                | Some tyRes ->
                    let! allUses = tyRes.GetCheckResults.GetAllUsesOfAllSymbolsInFile ()
                    let! simplified = SimplifyNameDiagnosticAnalyzer.getSimplifyNameRanges tyRes.GetCheckResults source allUses
                    let res = CoreResponse.SimplifiedName (Seq.toArray simplified)
                    notify.Trigger (NotificationEvent.SimplifyNames res)
                    return [ res ]
        } |> x.AsCancellable file

    member x.GetUnusedOpens file =
        let file = Path.GetFullPath file
        async {
            match state.TryGetFileCheckerOptionsWithLines file with
            | Error s ->  return [CoreResponse.ErrorRes s]
            | Ok (opts, source) ->
                let tyResOpt = checker.TryGetRecentCheckResultsForFile(file, opts)
                match tyResOpt with
                | None -> return [ CoreResponse.InfoRes "Cached typecheck results not yet available"]
                | Some tyRes ->
                    let! unused = UnusedOpens.getUnusedOpens(tyRes.GetCheckResults, fun i -> source.[i - 1])
                    let res = CoreResponse.UnusedOpens (unused |> List.toArray)
                    notify.Trigger (NotificationEvent.UnusedOpens res)
                    return [ res ]
        } |> x.AsCancellable file

    member x.Compile projectFileName = async {
        let projectFileName = Path.GetFullPath projectFileName
        match state.Projects.TryFind projectFileName with
        | None -> return [ CoreResponse.InfoRes "Project not found" ]
        | Some proj ->
        match proj.Response with
        | None -> return [ CoreResponse.InfoRes "Project not found" ]
        | Some proj ->
            let! errors,code = checker.Compile(proj.Options.OtherOptions)
            return [ CoreResponse.Compile (errors,code)]
    }

    member __.BuildSymbolCacheForProject proj =
        match state.Projects.TryFind proj with
        | None -> ()
        | Some pr ->
            match pr.Response with
            | None -> ()
            | Some r ->
                SymbolCache.buildProjectCache serialize r.Options
                |> Async.Start

    member __.BuildBackgroundSymbolsCache () =
        async {
            let start = DateTime.Now
            SymbolCache.startCache state.WorkspaceRoot
            do! Async.Sleep 100
            let r =
                state.Projects.Values
                |> Seq.fold (fun acc n ->
                    let s =
                        match n.Response with
                        | None -> async.Return ()
                        | Some r ->
                            SymbolCache.buildProjectCache serialize r.Options
                    acc |> Async.bind (fun _ -> s )
                    ) (async.Return ())
            do! r
            let finish = DateTime.Now
            printfn "[Symbol Cache] Building background cache took %fms" (finish-start).TotalMilliseconds
        }

    member __.LoadAnalyzers (path: string) = async {
        let analyzers = Analyzers.loadAnalyzers path
        state.Analyzers.AddOrUpdate(path, (fun _ -> analyzers), (fun _ _ -> analyzers)) |> ignore
        return [CoreResponse.InfoRes (sprintf "%d Analyzers registered" analyzers.Length) ]
    }

    member x.EnableSymbolCache () =
        x.UseSymbolCache <- true

    member x.GetGitHash =
        let hash = getGitHash()
        match hash with
        | Some hash -> hash
        | None -> ""

    member __.Quit () =
        async {
            return [ CoreResponse.InfoRes "quitting..." ]
        }
