namespace FsAutoComplete

open System
open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpLint.Application

module Response = CommandResponse

type Commands (serialize : Serializer) =
    let checker = FSharpCompilerServiceChecker()
    let mutable state = FsAutoComplete.State.Initial

    member private __.SerializeResultAsync (successToString: Serializer -> 'a -> Async<string>, ?failureToString: Serializer -> string -> string) =
        Async.bind <| function
            // A failure is only info here, as this command is expected to be
            // used 'on idle', and frequent errors are expected.
            | Result.Failure e -> async.Return [(defaultArg failureToString Response.info) serialize e]
            | Result.Success r -> successToString serialize r |> Async.map List.singleton

    member private x.SerializeResult (successToString: Serializer -> 'a -> string, ?failureToString: Serializer -> string -> string) =
        x.SerializeResultAsync ((fun s x -> successToString s x |> async.Return), ?failureToString = failureToString)

    member __.TryGetRecentTypeCheckResultsForFile = checker.TryGetRecentTypeCheckResultsForFile
    member __.TryGetFileCheckerOptionsWithLinesAndLineStr = state.TryGetFileCheckerOptionsWithLinesAndLineStr
    member __.TryGetFileCheckerOptionsWithLines = state.TryGetFileCheckerOptionsWithLines
    member __.Files = state.Files

    member __.Parse file lines = async {
        let colorizations = state.ColorizationOutput
        let parse' fileName text options =
            async {
                let! _parseResults, checkResults = checker.ParseAndCheckFileInProject(fileName, 0, text, options)
                return match checkResults with
                        | FSharpCheckFileAnswer.Aborted -> [Response.info serialize "Parse aborted"]
                        | FSharpCheckFileAnswer.Succeeded results ->
                            if colorizations then
                                [ Response.errors serialize (results.Errors)
                                  Response.colorizations serialize (results.GetExtraColorizationsAlternate()) ]
                            else [ Response.errors serialize (results.Errors) ]
            }
        let file = Path.GetFullPath file
        let text = String.concat "\n" lines

        if Utils.isAScript file then
          let! checkOptions = checker.GetProjectOptionsFromScript(file, text)
          state <- state.WithFileTextAndCheckerOptions(file, lines, checkOptions)
          return! parse' file text checkOptions
        else
          let state', checkOptions = state.WithFileTextGetCheckerOptions(file, lines)
          state <- state'
          return! parse' file text checkOptions
    }

    member __.ParseAll () = async {
        let! errors = checker.ParseAndCheckAllProjects state.FileCheckOptions
        return [Response.errors serialize errors ]
    }

    member __.Project file time verbose = async {
        let file = Path.GetFullPath file

        // The FileSystemWatcher often triggers multiple times for
        // each event, as editors often modify files in several steps.
        // This 'debounces' the events, by only reloading a max of once
        // per second.
        return
            match state.ProjectLoadTimes.TryFind file with
            | Some oldtime when time - oldtime < TimeSpan.FromSeconds(1.0) -> []
            | _ ->
            let options =
              if file.EndsWith "fsproj" then
                checker.TryGetProjectOptions(file, verbose)
              else
                checker.TryGetCoreProjectOptions file

            match options with
            | Result.Failure s -> [Response.error serialize s]
            | Result.Success(po, projectFiles, outFileOpt, references, logMap) ->
                let pf = projectFiles |> List.map Path.GetFullPath |> List.map Utils.normalizePath
                let res = Response.project serialize (file, pf, outFileOpt, references, logMap)
                let checkOptions = pf |> List.fold (fun s f -> Map.add f po s) state.FileCheckOptions
                let loadTimes = Map.add file time state.ProjectLoadTimes
                state <- { state with FileCheckOptions = checkOptions; ProjectLoadTimes = loadTimes }
                [res]
    }

    member __.Declarations file = async {
        let file = Path.GetFullPath file
        match state.TryGetFileCheckerOptionsWithSource file with
        | Failure s -> return [Response.error serialize s]
        | Success (checkOptions, source) ->
            let! decls = checker.GetDeclarations(file, source, checkOptions)
            return [Response.declarations serialize decls]
    }

    member __.DeclarationsInProjects () = async {
        let! decls = checker.GetDeclarationsInProjects state.FileCheckOptions
        return [Response.declarations serialize decls]
    }

    member __.Helptext sym =
        match Map.tryFind sym state.HelpText with
        | None -> [Response.error serialize (sprintf "No help text available for symbol '%s'" sym)]
        | Some tip -> [Response.helpText serialize (sym, tip)]

    member __.CompilerLocation () = [Response.compilerLocation serialize Environment.fsc Environment.fsi Environment.msbuild]
    member __.Colorization enabled = state <- { state with ColorizationOutput = enabled }
    member __.Error msg = [Response.error serialize msg]

    member __.Completion (tyRes : ParseAndCheckResults) (pos: Pos) lineStr filter = async {
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
                                | None -> [Response.completion serialize decls]
                                | Some d ->
                                    [Response.helpText serialize (d.Name, d.DescriptionText)
                                     Response.completion serialize decls]

                    let helptext = Seq.fold (fun m d -> Map.add (declName d) d.DescriptionText m) Map.empty decls
                    state <- { state with HelpText = helptext }
                    res
                | None -> [Response.error serialize "Timed out while fetching completions"]
    }

    member x.ToolTip (tyRes : ParseAndCheckResults) (pos: Pos) lineStr =
        tyRes.TryGetToolTip pos.Line pos.Col lineStr |> x.SerializeResult Response.toolTip

    member x.Typesig (tyRes : ParseAndCheckResults) (pos: Pos) lineStr =
        tyRes.TryGetToolTip pos.Line pos.Col lineStr |> x.SerializeResult Response.typeSig

    member x.SymbolUse (tyRes : ParseAndCheckResults) (pos: Pos) lineStr =
        tyRes.TryGetSymbolUse pos.Line pos.Col lineStr |> x.SerializeResult Response.symbolUse

    member x.SymbolUseProject (tyRes : ParseAndCheckResults) (pos: Pos) lineStr =
        tyRes.TryGetSymbolUse pos.Line pos.Col lineStr |> x.SerializeResultAsync (fun _ (sym, _usages) ->
            async {
                let! symbols = checker.GetUsesOfSymbol (state.FileCheckOptions, sym.Symbol)
                return Response.symbolUse serialize (sym, symbols)
            })

    member x.FindDeclarations (tyRes : ParseAndCheckResults) (pos: Pos) lineStr =
        tyRes.TryFindDeclaration pos.Line pos.Col lineStr |> x.SerializeResult (Response.findDeclaration, Response.error)

    member x.Methods (tyRes : ParseAndCheckResults) (pos: Pos) lines =
        tyRes.TryGetMethodOverrides lines pos.Line pos.Col |> x.SerializeResult (Response.methods, Response.error)

    member __.Lint file = async {
        let file = Path.GetFullPath file
        let res =
            match state.TryGetFileCheckerOptionsWithSource file with
            | Failure s -> [Response.error serialize s]
            | Success (options, source) ->
                let tyResOpt = checker.TryGetRecentTypeCheckResultsForFile(file, options)

                match tyResOpt with
                | None -> [ Response.info serialize "Cached typecheck results not yet available"]
                | Some tyRes ->
                    match tyRes.GetAST with
                    | None -> [ Response.info serialize "Something went wrong during parsing"]
                    | Some tree ->
                        let res =
                            Lint.lintParsedSource
                                Lint.OptionalLintParameters.Default
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