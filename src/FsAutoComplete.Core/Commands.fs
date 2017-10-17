namespace FsAutoComplete

open System
open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpLint.Application
open FsAutoComplete.UnopenedNamespacesResolver
open FsAutoComplete.UnionPatternMatchCaseGenerator
open Microsoft.FSharp.Compiler
open System.Threading


module Response = CommandResponse

type Commands (serialize : Serializer) =

    let checker = FSharpCompilerServiceChecker()
    let state = FsAutoComplete.State.Initial
    let fsharpLintConfig = ConfigurationManager.ConfigurationManager()
    let fileParsed = Event<FSharpParseFileResults>()

    do fileParsed.Publish.Add (fun parseRes ->
       let decls = parseRes.GetNavigationItems().Declarations
       state.NavigationDeclarations.[parseRes.FileName] <- decls
    )

    let parseFilesInTheBackground files =
        async {
            files
            |> List.toArray
            |> Array.Parallel.iter (fun file ->
                let source =
                    match state.Files.TryFind file with
                    | Some f -> f.Lines
                    | None ->
                        let ctn = File.ReadAllLines file
                        state.Files.[file] <- {Touched = DateTime.Now; Lines = ctn }
                        ctn
                let opts = state.FileCheckOptions.[file] |> Utils.projectOptionsToParseOptions
                let parseRes = checker.ParseFile(file, source |> String.concat "\n", opts) |> Async.RunSynchronously
                fileParsed.Trigger parseRes
            ) }

    member private x.SerializeResultAsync (successToString: Serializer -> 'a -> Async<string>, ?failureToString: Serializer -> string -> string) =
        Async.bind <| function
            // A failure is only info here, as this command is expected to be
            // used 'on idle', and frequent errors are expected.
            | Result.Failure e -> async.Return [(defaultArg failureToString Response.info) serialize e]
            | Result.Success r -> successToString serialize r |> Async.map List.singleton

    member private x.SerializeResult (successToString: Serializer -> 'a -> string, ?failureToString: Serializer -> string -> string) =
        x.SerializeResultAsync ((fun s x -> successToString s x |> async.Return), ?failureToString = failureToString)

    member private x.AsCancellable (filename : SourceFilePath) (action : Async<string list>) =
        let cts = new CancellationTokenSource()
        state.AddCancellationToken(filename, cts)
        Async.StartCatchCancellation(action, cts.Token)
        |> Async.Catch
        |> Async.map (function Choice1Of2 res -> res | Choice2Of2 _ -> [Response.info serialize "Request cancelled"])

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
                        | Failure e -> [Response.error serialize e]
                        | Success (parseResult, checkResults) ->
                            do fileParsed.Trigger parseResult
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
                state.AddFileTextAndCheckerOptions(file, lines, checkOptions)
                return! parse' file text checkOptions
            else
                let! checkOptions =
                    match state.GetCheckerOptions(file, lines) with
                    | Some c -> async.Return c
                    | None -> async {
                        let! checkOptions = checker.GetProjectOptionsFromScript(file, text)
                        state.AddFileTextAndCheckerOptions(file, lines, checkOptions)
                        return checkOptions
                    }
                return! parse' file text checkOptions
        } |> x.AsCancellable file

    member x.ParseAndCheckProjectsInBackgroundForFile file = async {
        do checker.CheckProjectsInBackgroundForFile (file, state.FileCheckOptions.ToSeq() )
        return [Response.errors serialize ([||], "") ]
    }

    member x.ParseProjectsForFile file = async {
        let! res = checker.ParseProjectsForFile(file, state.FileCheckOptions.ToSeq())
        return
            match res with
            | Failure e -> [Response.error serialize e]
            | Success results ->
                let errors = results |> Array.collect (fun r -> r.Errors)
                [ Response.errors serialize (errors, file)]
    }

    member x.Project projectFileName verbose onChange = async {
        let projectFileName = Path.GetFullPath projectFileName
        let project = state.Projects.TryFind projectFileName

        let project = project |> Option.getOrElseFun (fun _ ->
            let project = new Project(projectFileName, onChange)
            state.Projects.[projectFileName] <- project
            project)

        return
            match project.Response with
            | Some response ->
                for file in response.Files do
                    state.FileCheckOptions.[file] <- response.Options

                response.Files
                |> parseFilesInTheBackground
                |> Async.Start

                let r = Response.project serialize (projectFileName, response.Files, response.OutFile, response.References, response.Log, response.ExtraInfo, Map.empty)
                [r]
            | None ->
                let options = checker.GetProjectOptions verbose projectFileName

                match options with
                | Result.Err error ->
                    project.Response <- None
                    [Response.projectError serialize error]
                | Result.Ok (opts, projectFiles, logMap) ->
                    match opts.ExtraProjectInfo with
                    | None ->
                        project.Response <- None
                        [Response.projectError serialize (GenericError "expected ExtraProjectInfo after project parsing, was None")]
                    | Some x ->
                        match x with
                        | :? ExtraProjectInfoData as extraInfo ->
                            let outFileOpt = FscArguments.outputFile (Path.GetDirectoryName(opts.ProjectFileName)) (opts.OtherOptions |> List.ofArray)
                            let references = FscArguments.references (opts.OtherOptions |> List.ofArray)
                            let projectFiles = projectFiles |> List.map (Path.GetFullPath >> Utils.normalizePath)
                            let response = Response.project serialize (projectFileName, projectFiles, outFileOpt, references, logMap, extraInfo, Map.empty)
                            for file in projectFiles do
                                state.FileCheckOptions.[file] <- opts

                            projectFiles
                            |> parseFilesInTheBackground
                            |> Async.Start

                            let cached = {
                                Options = opts
                                Files = projectFiles
                                OutFile = outFileOpt
                                References = references
                                Log = logMap
                                ExtraInfo = extraInfo
                            }

                            project.Response <- Some cached
                            [response]
                        | x ->
                            project.Response <- None
                            [Response.projectError serialize (GenericError (sprintf "expected ExtraProjectInfo after project parsing, was %A" x))]
    }

    member x.Declarations file lines version = async {
        let file = Path.GetFullPath file
        match state.TryGetFileCheckerOptionsWithSource file, lines with
        | Failure s, None -> return [Response.error serialize s]
        | Failure _, Some l ->
            let text = String.concat "\n" l
            let files = Array.singleton file
            let parseOptions = { FSharpParsingOptions.Default with SourceFiles = files}
            let! decls = checker.GetDeclarations(file, text, parseOptions, version)
            let decls = decls |> Array.map (fun a -> a,file)
            return [Response.declarations serialize decls]
        | Success (checkOptions, source), _ ->
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
            state.NavigationDeclarations.ToSeq()
            |> Seq.collect (fun (p, decls) -> decls |> Seq.map (fun d -> d,p))
            |> Seq.toArray
        return [Response.declarations serialize decls]
    }

    member x.Helptext sym =
        match state.HelpText.TryFind sym with
        | None -> [Response.error serialize (sprintf "No help text available for symbol '%s'" sym)]
        | Some tip -> [Response.helpText serialize (sym, tip)]

    member x.CompilerLocation () = [Response.compilerLocation serialize Environment.fsc Environment.fsi Environment.msbuild]
    member x.Colorization enabled = state.ColorizationOutput <- enabled
    member x.Error msg = [Response.error serialize msg]

    member x.Completion (tyRes : ParseAndCheckResults) (pos: Pos) lineStr filter includeKeywords =
        async {
            let! res = tyRes.TryGetCompletions pos lineStr filter
            return match res with
                    | Some (decls, residue) ->
                        let declName (d: FSharpDeclarationListItem) = d.Name

                        // Send the first helptext without being requested.
                        // This allows it to be displayed immediately in the editor.
                        let firstMatchOpt =
                          Array.sortBy declName decls
                          |> Array.tryFind (fun d -> (declName d).StartsWith(residue, StringComparison.InvariantCultureIgnoreCase))
                        let res = match firstMatchOpt with
                                    | None -> [Response.completion serialize decls includeKeywords]
                                    | Some d ->
                                        [Response.helpText serialize (d.Name, d.DescriptionText)
                                         Response.completion serialize decls includeKeywords]

                        for decl in decls do
                            state.HelpText.[declName decl] <- decl.DescriptionText
                        res
                    | None -> [Response.error serialize "Timed out while fetching completions"]
        }

    member x.ToolTip (tyRes : ParseAndCheckResults) (pos: Pos) lineStr =
        tyRes.TryGetToolTip pos lineStr
        |> x.SerializeResult Response.toolTip
        |> x.AsCancellable (Path.GetFullPath tyRes.FileName)

    member x.Typesig (tyRes : ParseAndCheckResults) (pos: Pos) lineStr =
        tyRes.TryGetToolTip pos lineStr
        |> x.SerializeResult Response.typeSig
        |> x.AsCancellable (Path.GetFullPath tyRes.FileName)

    member x.SymbolUse (tyRes : ParseAndCheckResults) (pos: Pos) lineStr =
        tyRes.TryGetSymbolUse pos lineStr
        |> x.SerializeResult Response.symbolUse
        |> x.AsCancellable (Path.GetFullPath tyRes.FileName)

    member x.SignatureData (tyRes : ParseAndCheckResults) (pos: Pos) lineStr =
        tyRes.TryGetSignatureData pos lineStr
        |> x.SerializeResult Response.signatureData
        |> x.AsCancellable (Path.GetFullPath tyRes.FileName)

    member x.Help (tyRes : ParseAndCheckResults) (pos: Pos) lineStr =
        tyRes.TryGetF1Help pos lineStr
        |> x.SerializeResult Response.help
        |> x.AsCancellable (Path.GetFullPath tyRes.FileName)

    member x.SymbolUseProject (tyRes : ParseAndCheckResults) (pos: Pos) lineStr =
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
                    let! symbols = checker.GetUsesOfSymbol (fn, state.FileCheckOptions.ToSeq(), sym.Symbol)
                    return Response.symbolUse serialize (sym, symbols)
            })
        |> x.AsCancellable (Path.GetFullPath fn)

    member x.FindDeclaration (tyRes : ParseAndCheckResults) (pos: Pos) lineStr =
        tyRes.TryFindDeclaration pos lineStr
        |> x.SerializeResult (Response.findDeclaration, Response.error)
        |> x.AsCancellable (Path.GetFullPath tyRes.FileName)

    member x.Methods (tyRes : ParseAndCheckResults) (pos: Pos) (lines: LineStr[]) =
        tyRes.TryGetMethodOverrides lines pos
        |> x.SerializeResult (Response.methods, Response.error)
        |> x.AsCancellable (Path.GetFullPath tyRes.FileName)

    member x.Lint (file: SourceFilePath) =
        let file = Path.GetFullPath file
        async {
            let res =
                match state.TryGetFileCheckerOptionsWithSource file with
                | Failure s -> [Response.error serialize s]
                | Success (options, source) ->
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

    member x.GetNamespaceSuggestions (tyRes : ParseAndCheckResults) (pos: Pos) (line: LineStr) =
        async {
            match tyRes.GetAST with
            | None -> return [Response.info serialize "Parsed Tree not avaliable"]
            | Some parsedTree ->
            match Parsing.findLongIdents(pos.Col, line) with
            | None -> return [Response.info serialize "Ident not found"]
            | Some (_,idents) ->
            match ParsedInput.getEntityKind parsedTree pos with
            | None -> return [Response.info serialize "EntityKind not found"]
            | Some entityKind ->

            let symbol = Lexer.getSymbol pos.Line pos.Col line SymbolLookupKind.Fuzzy [||]
            match symbol with
            | None -> return [Response.info serialize "Symbol at position not found"]
            | Some sym ->

            let! entitiesRes = tyRes.GetAllEntities ()
            match entitiesRes with
            | None -> return [Response.info serialize "Something went wrong"]
            | Some entities ->
                let isAttribute = entityKind = FsAutoComplete.EntityKind.Attribute
                let entities =
                    entities |> List.filter (fun e ->
                        match entityKind, (e.Kind LookupType.Fuzzy) with
                        | FsAutoComplete.EntityKind.Attribute, EntityKind.Attribute
                        | FsAutoComplete.EntityKind.Type, (EntityKind.Type | EntityKind.Attribute)
                        | FsAutoComplete.EntityKind.FunctionOrValue _, _ -> true
                        | FsAutoComplete.EntityKind.Attribute, _
                        | _, EntityKind.Module _
                        | FsAutoComplete.EntityKind.Module _, _
                        | FsAutoComplete.EntityKind.Type, _ -> false)

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
                let createEntity = ParsedInput.tryFindInsertionContext pos.Line parsedTree (idents |> List.toArray)
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

    member x.GetUnionPatternMatchCases (tyRes : ParseAndCheckResults) (pos: Pos) (lines: LineStr[]) (line: LineStr) =
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
            | Some (symbolRange, patMatchExpr, unionTypeDefinition, insertionPos) ->

            if shouldGenerateUnionPatternMatchCases patMatchExpr unionTypeDefinition then
                let result = formatMatchExpr insertionPos "$1" patMatchExpr unionTypeDefinition
                let pos = {
                    Col = insertionPos.InsertionPos.Column
                    Line = insertionPos.InsertionPos.Line
                }
                return [Response.unionCase serialize result pos ]
            else
                return [Response.info serialize "Union at position not found"]
        } |> x.AsCancellable (Path.GetFullPath tyRes.FileName)

    member x.WorkspacePeek (dir: string) (deep: int) (excludedDirs: string list) = async {
        let d = WorkspacePeek.peek dir deep excludedDirs

        return [Response.workspacePeek serialize d]
    }
