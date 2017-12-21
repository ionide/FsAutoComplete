namespace FsAutoComplete

open System
open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpLint.Application
open FsAutoComplete.UnionPatternMatchCaseGenerator
open System.Threading
open Utils
open System.Reflection
open Microsoft.FSharp.Compiler.Range



module Response = CommandResponse

[<RequireQualifiedAccess>]
type NotificationEvent =
    | ParseError of string
    | Workspace of string

type Commands (serialize : Serializer) =

    let checker = FSharpCompilerServiceChecker()
    let state = State.Initial
    let fsharpLintConfig = ConfigurationManager.ConfigurationManager()
    let fileParsed = Event<FSharpParseFileResults>()
    let fileInProjectChecked = Event<SourceFilePath>()
    let mutable notifyErrorsInBackground = true

    let notify = Event<NotificationEvent>()

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

    do fileParsed.Publish.Add (fun parseRes ->
       let decls = parseRes.GetNavigationItems().Declarations
       state.NavigationDeclarations.[parseRes.FileName] <- decls
    )

    do checker.FileChecked.Add (fun (n,_) ->
        async {
            try
                let opts = state.FileCheckOptions.[n]
                let! res = checker.GetBackgroundCheckResultsForFileInProject(n, opts)
                let checkErrors = res.GetCheckResults.Errors
                let parseErrors = res.GetParseResults.Errors
                let errors = Array.append checkErrors parseErrors

                Response.errors serialize (errors, n)
                |> NotificationEvent.ParseError
                |> notify.Trigger
            with
            | _ -> ()
        }
        |> if notifyErrorsInBackground then Async.Start else ignore
    )


    do checker.ProjectChecked.Add(fun (o, _) ->
        state.FileCheckOptions.ToArray()
        |> Array.tryPick(fun (KeyValue(k, v)) -> if v.ProjectFileName = Path.GetFullPath o then Some v else None )
        |> Option.iter (fun p ->
            state.BackgroundProjects.TryRemove(p) |> ignore
            // printfn "BACKGROUND CHECKER - PARSED: %s" o
            // printfn "2. BACKGROUND QUEUE: %A" (state.BackgroundProjects |> Seq.map (fun n -> n.ProjectFileName))

        )
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
            |> Option.bind (fun ast -> ParsedInput.tryFindNearestPointToInsertOpenDeclaration (pos.Line) ast idents TopLevel )
            |> Option.map (fun ic -> n, ic.Pos.Line, ic.Pos.Column))

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

    member __.NotifyErrorsInBackground
        with get() = notifyErrorsInBackground
        and set(value) = notifyErrorsInBackground <- value

    member private x.SerializeResultAsync (successToString: Serializer -> 'a -> Async<string>, ?failureToString: Serializer -> string -> string) =
        Async.bind <| function
            // A failure is only info here, as this command is expected to be
            // used 'on idle', and frequent errors are expected.
            | ResultOrString.Error e -> async.Return [(defaultArg failureToString Response.info) serialize e]
            | ResultOrString.Ok r -> successToString serialize r |> Async.map List.singleton

    member private x.SerializeResult (successToString: Serializer -> 'a -> string, ?failureToString: Serializer -> string -> string) =
        x.SerializeResultAsync ((fun s x -> successToString s x |> async.Return), ?failureToString = failureToString)

    member private x.AsCancellable (filename : SourceFilePath) (action : Async<string list>) =
        let cts = new CancellationTokenSource()
        state.AddCancellationToken(filename, cts)
        Async.StartCatchCancellation(action, cts.Token)
        |> Async.Catch
        |> Async.map (function Choice1Of2 res -> res | Choice2Of2 err -> [Response.info serialize (sprintf "Request cancelled (exn was %A)" err)])

    member private x.CancelQueue (filename : SourceFilePath) =
        state.GetCancellationTokens filename |> List.iter (fun cts -> cts.Cancel() )

    member x.TryGetRecentTypeCheckResultsForFile = checker.TryGetRecentCheckResultsForFile
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
                        | ResultOrString.Error e -> [Response.error serialize e]
                        | ResultOrString.Ok (parseResult, checkResults) ->
                            do fileParsed.Trigger parseResult
                            do fileInProjectChecked.Trigger file
                            match checkResults with
                            | FSharpCheckFileAnswer.Aborted -> [Response.info serialize "Parse aborted"]
                            | FSharpCheckFileAnswer.Succeeded results ->
                                let errors = Array.append results.Errors parseResult.Errors
                                if colorizations then
                                    [ Response.errors serialize (errors, fileName)
                                      Response.colorizations serialize (results.GetSemanticClassification None) ]
                                else [ Response.errors serialize (errors, fileName) ]
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
        return [Response.errors serialize ([||], "") ]
    }

    member x.ParseProjectsForFile file = async {
        let! res = checker.ParseProjectsForFile(file, state.FileCheckOptions.ToArray() |> Array.map (fun (KeyValue(k, v)) -> k,v) |> Seq.ofArray)
        return
            match res with
            | ResultOrString.Error e -> [Response.error serialize e]
            | ResultOrString.Ok results ->
                let errors = results |> Array.collect (fun r -> r.Errors)
                [ Response.errors serialize (errors, file)]
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

        let project =
            match state.Projects.TryFind projectFileName with
            | Some prj -> prj
            | None ->
                let proj = new Project(projectFileName, onChange)
                state.Projects.[projectFileName] <- proj
                proj

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

        return
            match projResponse with
            | Result.Ok (projectFileName, response) ->
                onProjectLoaded projectFileName response
                [ Response.project serialize (projectFileName, response.Files, response.OutFile, response.References, response.Log, response.ExtraInfo, Map.empty) ]
            | Result.Error error ->
                [ Response.projectError serialize error ]
    }

    member x.Declarations file lines version = async {
        let file = Path.GetFullPath file
        match state.TryGetFileCheckerOptionsWithSource file, lines with
        | ResultOrString.Error s, None -> return [Response.error serialize s]
        | ResultOrString.Error _, Some l ->
            let text = String.concat "\n" l
            let files = Array.singleton file
            let parseOptions = { FSharpParsingOptions.Default with SourceFiles = files}
            let! decls = checker.GetDeclarations(file, text, parseOptions, version)
            let decls = decls |> Array.map (fun a -> a,file)
            return [Response.declarations serialize decls]
        | ResultOrString.Ok (checkOptions, source), _ ->
            let text =
                match lines with
                | Some l -> String.concat "\n" l
                | None -> source

            let parseOptions = Utils.projectOptionsToParseOptions checkOptions
            let! decls = checker.GetDeclarations(file, text, parseOptions, version)

            state.NavigationDeclarations.[file] <- decls

            let decls = decls |> Array.map (fun a -> a,file)
            return [Response.declarations serialize decls]
    }

    member x.DeclarationsInProjects () = async {
        let decls =
            state.NavigationDeclarations.ToArray()
            |> Array.collect (fun (KeyValue(p, decls)) -> decls |> Array.map (fun d -> d,p))
        return [Response.declarations serialize decls]
    }

    member __.Helptext sym =
        match state.Declarations.TryFind sym with
        | None -> //Isn't in sync filled cache, we don't have result
            [Response.error serialize (sprintf "No help text available for symbol '%s'" sym)]
        | Some (decl, pos, fn) -> //Is in sync filled cache, try to get results from async filled cahces or calculate if it's not there
            let source =
                state.Files.TryFind fn
                |> Option.map (fun n -> n.Lines)
            match source with
            | None -> [Response.error serialize (sprintf "No help text available for symbol '%s'" sym)]
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
                [Response.helpText serialize (sym, tip, n)]

    member x.CompilerLocation () = [Response.compilerLocation serialize Environment.fsc Environment.fsi Environment.msbuild]
    member x.Colorization enabled = state.ColorizationOutput <- enabled
    member x.Error msg = [Response.error serialize msg]

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
                    | None -> [Response.completion serialize decls includeKeywords]
                    | Some d ->
                        let insert = calculateNamespaceInser d pos getLine
                        [Response.helpText serialize (d.Name, d.DescriptionText, insert)
                         Response.completion serialize decls includeKeywords]

                | None -> [Response.error serialize "Timed out while fetching completions"]
        }

    member x.ToolTip (tyRes : ParseAndCheckResults) (pos: pos) lineStr =
        tyRes.TryGetToolTipEnhanced pos lineStr
        |> x.SerializeResult Response.toolTip
        |> x.AsCancellable (Path.GetFullPath tyRes.FileName)

    member x.Typesig (tyRes : ParseAndCheckResults) (pos: pos) lineStr =
        tyRes.TryGetToolTip pos lineStr
        |> x.SerializeResult Response.typeSig
        |> x.AsCancellable (Path.GetFullPath tyRes.FileName)

    member x.SymbolUse (tyRes : ParseAndCheckResults) (pos: pos) lineStr =
        tyRes.TryGetSymbolUse pos lineStr
        |> x.SerializeResult Response.symbolUse
        |> x.AsCancellable (Path.GetFullPath tyRes.FileName)

    member x.SignatureData (tyRes : ParseAndCheckResults) (pos: pos) lineStr =
        tyRes.TryGetSignatureData pos lineStr
        |> x.SerializeResult Response.signatureData
        |> x.AsCancellable (Path.GetFullPath tyRes.FileName)

    member x.Help (tyRes : ParseAndCheckResults) (pos: pos) lineStr =
        tyRes.TryGetF1Help pos lineStr
        |> x.SerializeResult Response.help
        |> x.AsCancellable (Path.GetFullPath tyRes.FileName)

    member x.SymbolUseProject (tyRes : ParseAndCheckResults) (pos: pos) lineStr =
        let fn = tyRes.FileName
        tyRes.TryGetSymbolUse pos lineStr |> x.SerializeResultAsync (fun _ (sym, usages) ->
            async {
                let fsym = sym.Symbol
                if fsym.IsPrivateToFile then
                    return Response.symbolUse serialize (sym, usages)
                elif fsym.IsInternalToProject then
                    let opts = state.FileCheckOptions.[tyRes.FileName]
                    let! symbols = checker.GetUsesOfSymbol (fn, [tyRes.FileName, opts] , sym.Symbol)
                    return Response.symbolUse serialize (sym, symbols)
                else
                    let! symbols = checker.GetUsesOfSymbol (fn, state.FileCheckOptions.ToArray() |> Array.map (fun (KeyValue(k, v)) -> k,v) |> Seq.ofArray, sym.Symbol)
                    return Response.symbolUse serialize (sym, symbols)
            })
        |> x.AsCancellable (Path.GetFullPath fn)

    member x.FindDeclaration (tyRes : ParseAndCheckResults) (pos: pos) lineStr =
        tyRes.TryFindDeclaration pos lineStr
        |> x.SerializeResult (Response.findDeclaration, Response.error)
        |> x.AsCancellable (Path.GetFullPath tyRes.FileName)

    member x.FindTypeDeclaration (tyRes : ParseAndCheckResults) (pos: pos) lineStr =
        tyRes.TryFindTypeDeclaration pos lineStr
        |> x.SerializeResult (Response.findDeclaration, Response.error)
        |> x.AsCancellable (Path.GetFullPath tyRes.FileName)

    member x.Methods (tyRes : ParseAndCheckResults) (pos: pos) (lines: LineStr[]) =
        tyRes.TryGetMethodOverrides lines pos
        |> x.SerializeResult (Response.methods, Response.error)
        |> x.AsCancellable (Path.GetFullPath tyRes.FileName)

    member x.Lint (file: SourceFilePath) =


        let file = Path.GetFullPath file
        async {
            let res =
                match state.TryGetFileCheckerOptionsWithSource file with
                | Error s -> [Response.error serialize s]
                | Ok (options, source) ->
                    let tyResOpt = checker.TryGetRecentCheckResultsForFile(file, options)

                    match tyResOpt with
                    | None -> [ Response.info serialize "Cached typecheck results not yet available"]
                    | Some tyRes ->
                        match tyRes.GetAST with
                        | None -> [ Response.info serialize "Something went wrong during parsing"]
                        | Some tree ->
                            try
                                fsharpLintConfig.LoadConfigurationForProject file
                                let opts = fsharpLintConfig.GetConfigurationForProject (file)
                                let res =
                                    Lint.lintParsedSource
                                        { Lint.OptionalLintParameters.Default with Configuration = Some opts}
                                        { Ast = tree
                                          Source = source
                                          TypeCheckResults = Some tyRes.GetCheckResults
                                          FSharpVersion = Version() }
                                let res' =
                                    match res with
                                    | LintResult.Failure _ -> [ Response.info serialize "Something went wrong, linter failed"]
                                    | LintResult.Success warnings -> [ Response.lint serialize warnings ]

                                res'
                            with _ex ->
                                [ Response.info serialize "Something went wrong during linter"]
            return res
        } |> x.AsCancellable file

    member x.GetNamespaceSuggestions (tyRes : ParseAndCheckResults) (pos: pos) (line: LineStr) =
        async {
            match tyRes.GetAST with
            | None -> return [Response.info serialize "Parsed Tree not avaliable"]
            | Some parsedTree ->
            match Parsing.findLongIdents(pos.Column, line) with
            | None -> return [Response.info serialize "Ident not found"]
            | Some (_,idents) ->
            match UntypedParseImpl.GetEntityKind(pos, parsedTree)  with
            | None -> return [Response.info serialize "EntityKind not found"]
            | Some entityKind ->

            let symbol = Lexer.getSymbol pos.Line pos.Column line SymbolLookupKind.Fuzzy [||]
            match symbol with
            | None -> return [Response.info serialize "Symbol at position not found"]
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

            return [ Response.resolveNamespace serialize (word, openNamespace, qualifySymbolActions) ]
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
            | None -> return [Response.info serialize "Union at position not found"]
            | Some (patMatchExpr, unionTypeDefinition, insertionPos) ->

            if shouldGenerateUnionPatternMatchCases patMatchExpr unionTypeDefinition then
                let result = formatMatchExpr insertionPos "$1" patMatchExpr unionTypeDefinition
                let pos = mkPos insertionPos.InsertionPos.Line insertionPos.InsertionPos.Column

                return [Response.unionCase serialize result pos ]
            else
                return [Response.info serialize "Union at position not found"]
        } |> x.AsCancellable (Path.GetFullPath tyRes.FileName)

    member x.WorkspacePeek (dir: string) (deep: int) (excludedDirs: string list) = async {
        let d = WorkspacePeek.peek dir deep excludedDirs

        return [Response.workspacePeek serialize d]
    }

    member x.WorkspaceLoad onChange (files: string list) = async {

        //TODO check full path
        let projectFileNames = files |> List.map Path.GetFullPath

        let projects =
            projectFileNames
            |> List.map (fun projectFileName -> projectFileName, new Project(projectFileName, onChange))

        for projectFileName, proj in projects do
            state.Projects.[projectFileName] <- proj

        async {
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
                    Response.projectLoading serialize projectFileName
                    |> NotificationEvent.Workspace
                    |> notify.Trigger
                | WorkspaceProjectState.Loaded (opts, extraInfo, projectFiles, logMap) ->
                    let projectFileName, response = x.ToProjectCache(opts, extraInfo, projectFiles, logMap)
                    projectLoadedSuccessfully projectFileName response
                    Response.project serialize (projectFileName, response.Files, response.OutFile, response.References, response.Log, response.ExtraInfo, Map.empty)
                    |> NotificationEvent.Workspace
                    |> notify.Trigger
                | WorkspaceProjectState.Failed (projectFileName, error) ->
                    Response.projectError serialize error
                    |> NotificationEvent.Workspace
                    |> notify.Trigger

            Response.workspaceLoad serialize false
            |> NotificationEvent.Workspace
            |> notify.Trigger

            // this is to delay the project loading notification (of this thread)
            // after the workspaceload started response returned below in outer async
            // Make test output repeteable, and notification in correct order
            match Environment.workspaceLoadDelay() with
            | delay when delay > TimeSpan.Zero ->
                do! Async.Sleep(Environment.workspaceLoadDelay().TotalMilliseconds |> int)
            | _ -> ()

            do! Workspace.loadInBackground onLoaded false files

            Response.workspaceLoad serialize true
            |> NotificationEvent.Workspace
            |> notify.Trigger

        } |> Async.Start

        return [Response.workspaceLoad serialize false]
    }

    member x.GetUnusedDeclarations file =
        let file = Path.GetFullPath file
        let isScript = file.EndsWith ".fsx"

        async {
            match state.TryGetFileCheckerOptionsWithSource file with
            | Error s ->  return [Response.error serialize s]
            | Ok (opts, _) ->
                let tyResOpt = checker.TryGetRecentCheckResultsForFile(file, opts)
                match tyResOpt with
                | None -> return [ Response.info serialize "Cached typecheck results not yet available"]
                | Some tyRes ->
                    let! allUses = tyRes.GetCheckResults.GetAllUsesOfAllSymbolsInFile ()
                    let unused = UnusedDeclarationsAnalyzer.getUnusedDeclarationRanges allUses isScript
                    return [ Response.unusedDeclarations serialize unused ]
        } |> x.AsCancellable file

    member x.GetSimplifiedNames file =
        let file = Path.GetFullPath file

        async {
            match state.TryGetFileCheckerOptionsWithLines file with
            | Error s ->  return [Response.error serialize s]
            | Ok (opts, source) ->
                let tyResOpt = checker.TryGetRecentCheckResultsForFile(file, opts)
                match tyResOpt with
                | None -> return [ Response.info serialize "Cached typecheck results not yet available"]
                | Some tyRes ->
                    let! allUses = tyRes.GetCheckResults.GetAllUsesOfAllSymbolsInFile ()
                    let! simplified = SimplifyNameDiagnosticAnalyzer.getSimplifyNameRanges tyRes.GetCheckResults source allUses
                    return [ Response.simplifiedNames serialize simplified ]
        } |> x.AsCancellable file

    member x.GetUnusedOpens file =
        let file = Path.GetFullPath file
        async {
            match state.TryGetFileCheckerOptionsWithLines file with
            | Error s ->  return [Response.error serialize s]
            | Ok (opts, source) ->
                let tyResOpt = checker.TryGetRecentCheckResultsForFile(file, opts)
                match tyResOpt with
                | None -> return [ Response.info serialize "Cached typecheck results not yet available"]
                | Some tyRes ->
                    let! unused = UnusedOpens.getUnusedOpens(tyRes.GetCheckResults, fun i -> source.[i - 1])
                    return [ Response.unusedOpens serialize (unused |> List.toArray) ]
        } |> x.AsCancellable file

    member x.GetGitHash =
        let hash = getGitHash()
        match hash with 
        | Some hash -> hash
        | None -> ""

    member __.Quit () =
        async {
            return [ Response.info serialize "quitting..." ]
        }
