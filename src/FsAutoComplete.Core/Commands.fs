namespace FsAutoComplete

open System
open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpLint.Application
open FsAutoComplete.UnopenedNamespacesResolver
open FsAutoComplete.UnionPatternMatchCaseGenerator
open Microsoft.FSharp.Compiler


module Response = CommandResponse

type Commands (serialize : Serializer) =
    let checker = FSharpCompilerServiceChecker()
    let state = FsAutoComplete.State.Initial
    let fsharpLintConfig = ConfigurationManager.ConfigurationManager()

    member private __.SerializeResultAsync (successToString: Serializer -> 'a -> Async<string>, ?failureToString: Serializer -> string -> string) =
        Async.bind <| function
            // A failure is only info here, as this command is expected to be
            // used 'on idle', and frequent errors are expected.
            | Result.Failure e -> async.Return [(defaultArg failureToString Response.info) serialize e]
            | Result.Success r -> successToString serialize r |> Async.map List.singleton

    member private x.SerializeResult (successToString: Serializer -> 'a -> string, ?failureToString: Serializer -> string -> string) =
        x.SerializeResultAsync ((fun s x -> successToString s x |> async.Return), ?failureToString = failureToString)

    member __.TryGetRecentTypeCheckResultsForFile = checker.TryGetRecentCheckResultsForFile
    member __.TryGetFileCheckerOptionsWithLinesAndLineStr = state.TryGetFileCheckerOptionsWithLinesAndLineStr
    member __.TryGetFileCheckerOptionsWithLines = state.TryGetFileCheckerOptionsWithLines
    member __.Files = state.Files

    member __.Parse file lines version = async {
        let colorizations = state.ColorizationOutput
        let parse' fileName text options =
            async {
                let! result = checker.ParseAndCheckFileInProject(fileName, version, text, options)
                return
                    match result with
                    | Failure e -> [Response.error serialize e]
                    | Success (parseResult, checkResults) ->
                        match checkResults with
                        | FSharpCheckFileAnswer.Aborted -> [Response.info serialize "Parse aborted"]
                        | FSharpCheckFileAnswer.Succeeded results ->
                            let errors = Array.append results.Errors parseResult.Errors
                            if colorizations then
                                [ Response.errors serialize (errors, fileName)
                                  Response.colorizations serialize (results.GetSemanticClassification None) ]
                            else [ Response.errors serialize (errors, fileName) ]
            }
        let file = Path.GetFullPath file
        let text = String.concat "\n" lines

        if Utils.isAScript file then
            let! checkOptions = checker.GetProjectOptionsFromScript(file, text)
            state.AddFileTextAndCheckerOptions(file, lines, checkOptions)
            return! parse' file text checkOptions
        else
            let checkOptions = state.GetCheckerOptions(file, lines)
            return! parse' file text checkOptions
    }

    member __.ParseAndCheckProjectsInBackgroundForFile file = async {
        do checker.CheckProjectsInBackgroundForFile (file, state.FileCheckOptions.ToSeq() )
        return [Response.errors serialize ([||], "") ]
    }

    member __.ParseProjectsForFile file = async {
        let! res = checker.ParseProjectsForFile(file, state.FileCheckOptions.ToSeq())
        return
            match res with
            | Failure e -> [Response.error serialize e]
            | Success results ->
                let errors = results |> Array.collect (fun r -> r.Errors)
                [ Response.errors serialize (errors, file)]
    }

    // member __.FileChecked =
    //     checker.FileChecked
    //     |> Event.map (fun fn ->
    //         let file = Path.GetFullPath fn
    //         let res = state.FileCheckOptions |> Seq.tryFind (fun kv -> Path.GetFullPath kv.Key = file)
    //         match res with
    //         | None  -> async { return [Response.info serialize ( sprintf "Project for file not found: %s" file) ]  }
    //         | Some kv ->
    //             async {
    //                 let result= checker.TryGetRecentCheckResultsForFile(fn, kv.Value)
    //                 return
    //                     match result with
    //                     | None -> [Response.info serialize "File not parsed"]
    //                     | Some res -> [ Response.errors serialize (res.GetCheckResults.Errors, file) ]
    //             })

    member __.Project projectFileName verbose onChange = async {
        let projectFileName = Path.GetFullPath projectFileName
        let project = state.Projects.TryFind projectFileName

        let project = project |> Option.getOrElseFun (fun _ ->
            let project = new Project(projectFileName, onChange)
            state.Projects.[projectFileName] <- project
            project)

        let (|NetCoreProjectJson|NetCoreSdk|Net45|Unsupported|) file =
            //.NET Core Sdk preview3+ replace project.json with fsproj
            //Easy way to detect new fsproj is to check the msbuild version of .fsproj
            //  MSBuild version 15 (`ToolsVersion="15.0"`) is the new project format
            //Post preview5 has (`Sdk="FSharp.NET.Sdk;Microsoft.NET.Sdk"`), use that
            //  for checking .NET Core fsproj. NB: casing of FSharp may be inconsistent.
            //The `dotnet-compile-fsc.rsp` are created also in `preview3+`, so we can
            //  reuse the same behaviour of `preview2`
            let rec getProjectType (sr:StreamReader) limit =
                // only preview3-5 uses ToolsVersion='15.0'
                // post preview5 dropped this, check Sdk field
                let isNetCore (line:string) = line.Contains("=\"15.0\"") || line.ToLower().Contains("sdk=")
                if limit = 0 then
                    Unsupported // unsupported project type
                else
                    let line = sr.ReadLine()
                    if not <| line.Contains("ToolsVersion") && not <| line.Contains("Sdk=") then
                        getProjectType sr (limit-1)
                    else // both net45 and preview3-5 have 'ToolsVersion', > 5 has 'Sdk'
                        if isNetCore line then NetCoreSdk else Net45
            if not <| File.Exists(projectFileName) then Net45 // no such file is handled downstream
            elif Path.GetExtension file = ".json" then NetCoreProjectJson // dotnet core preview 2 or earlier
            else
                use sr = File.OpenText(file)
                getProjectType sr 3


        return
            match project.Response with
            | Some response -> [response]
            | None ->
                let options =
                    match projectFileName with
                    | NetCoreProjectJson -> checker.TryGetProjectJsonProjectOptions projectFileName
                    | NetCoreSdk -> checker.TryGetCoreProjectOptions projectFileName
                    | Net45 -> checker.TryGetProjectOptions (projectFileName, verbose)
                    | Unsupported -> checker.TryGetProjectOptions (projectFileName, verbose)

                match options with
                | Result.Failure error ->
                    project.Response <- None
                    [Response.error serialize error]
                | Result.Success (opts, projectFiles, outFileOpt, references, logMap) ->
                    let projectFiles = projectFiles |> List.map (Path.GetFullPath >> Utils.normalizePath)
                    let response = Response.project serialize (projectFileName, projectFiles, outFileOpt, references, logMap)
                    for file in projectFiles do
                        state.FileCheckOptions.[file] <- opts
                    project.Response <- Some response
                    [response]
    }

    member __.Declarations file version = async {
        let file = Path.GetFullPath file
        match state.TryGetFileCheckerOptionsWithSource file with
        | Failure s -> return [Response.error serialize s]
        | Success (checkOptions, source) ->
            let! decls = checker.GetDeclarations(file, source, checkOptions, version)
            let decls = decls |> Array.map (fun a -> a,file)
            return [Response.declarations serialize decls]
    }

    member __.DeclarationsInProjects () = async {
        let! decls = checker.GetDeclarationsInProjects <| state.FileCheckOptions.ToSeq()
        return [Response.declarations serialize decls]
    }

    member __.Helptext sym =
        match state.HelpText.TryFind sym with
        | None -> [Response.error serialize (sprintf "No help text available for symbol '%s'" sym)]
        | Some tip -> [Response.helpText serialize (sym, tip)]

    member __.CompilerLocation () = [Response.compilerLocation serialize Environment.fsc Environment.fsi Environment.msbuild]
    member __.Colorization enabled = state.ColorizationOutput <- enabled
    member __.Error msg = [Response.error serialize msg]

    member __.Completion (tyRes : ParseAndCheckResults) (pos: Pos) lineStr filter includeKeywords = async {
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
        tyRes.TryGetToolTip pos lineStr |> x.SerializeResult Response.toolTip

    member x.Typesig (tyRes : ParseAndCheckResults) (pos: Pos) lineStr =
        tyRes.TryGetToolTip pos lineStr |> x.SerializeResult Response.typeSig

    member x.SymbolUse (tyRes : ParseAndCheckResults) (pos: Pos) lineStr =
        tyRes.TryGetSymbolUse pos lineStr |> x.SerializeResult Response.symbolUse

    member x.Help (tyRes : ParseAndCheckResults) (pos: Pos) lineStr =
        tyRes.TryGetF1Help pos lineStr |> x.SerializeResult Response.help

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

    member x.FindDeclarations (tyRes : ParseAndCheckResults) (pos: Pos) lineStr =
        tyRes.TryFindDeclaration pos lineStr |> x.SerializeResult (Response.findDeclaration, Response.error)

    member x.Methods (tyRes : ParseAndCheckResults) (pos: Pos) (lines: LineStr[]) =
        tyRes.TryGetMethodOverrides lines pos |> x.SerializeResult (Response.methods, Response.error)

    member __.Lint (file: SourceFilePath) = async {
        let file = Path.GetFullPath file
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
                            | LintResult.Failure _ -> [ Response.info serialize "Something went wrong during parsing"]
                            | LintResult.Success warnings -> [ Response.lint serialize warnings ]

                        res'
        return res
    }

    member __.GetNamespaceSuggestions (tyRes : ParseAndCheckResults) (pos: Pos) (line: LineStr) = async {
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
            let isAttribute = entityKind = EntityKind.Attribute
            let entities =
                entities |> List.filter (fun e ->
                    match entityKind, e.Kind with
                    | EntityKind.Attribute, EntityKind.Attribute
                    | EntityKind.Type, (EntityKind.Type | EntityKind.Attribute)
                    | EntityKind.FunctionOrValue _, _ -> true
                    | EntityKind.Attribute, _
                    | _, EntityKind.Module _
                    | EntityKind.Module _, _
                    | EntityKind.Type, _ -> false)

            let entities =
                entities
                |> List.collect (fun e ->
                      [ yield e.TopRequireQualifiedAccessParent, e.AutoOpenParent, e.Namespace, e.CleanedIdents
                        if isAttribute then
                            let lastIdent = e.CleanedIdents.[e.CleanedIdents.Length - 1]
                            if e.Kind = EntityKind.Attribute && lastIdent.EndsWith "Attribute" then
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
    }

    member __.GetUnionPatternMatchCases (tyRes : ParseAndCheckResults) (pos: Pos) (lines: LineStr[]) (line: LineStr) = async {
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
    }
