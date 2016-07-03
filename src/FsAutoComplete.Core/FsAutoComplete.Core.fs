namespace FsAutoComplete

open System
open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpLint.Application

module Response = CommandResponse

type Commands (serialize : Serializer) =
    let checker = FSharpCompilerServiceChecker()

    let serializeResultAsync (successToString: Serializer -> 'a -> Async<string>) =
        Async.bind <| function
            // A failure is only info here, as this command is expected to be
            // used 'on idle', and frequent errors are expected.
            | Result.Failure e -> async.Return [Response.info serialize e]
            | Result.Success r -> successToString serialize r |> Async.map List.singleton

    let serializeResult (successToString: Serializer -> 'a -> string) = 
        serializeResultAsync (fun s x -> successToString s x |> async.Return)
    
    member __.TryGetRecentTypeCheckResultsForFile = checker.TryGetRecentTypeCheckResultsForFile

    member __.Parse (state : State) file lines = async {
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
          let state' = state.WithFileTextAndCheckerOptions(file, lines, checkOptions)
          let! res = (parse' file text checkOptions)
          return res , state'
        else
          let state', checkOptions = state.WithFileTextGetCheckerOptions(file, lines)
          let! res = (parse' file text checkOptions)
          return res, state'
    }

    member __.Project (state : State) file time verbose = async {
        let file = Path.GetFullPath file

        // The FileSystemWatcher often triggers multiple times for
        // each event, as editors often modify files in several steps.
        // This 'debounces' the events, by only reloading a max of once
        // per second.
        return match state.ProjectLoadTimes.TryFind file with
                | Some oldtime when time - oldtime < TimeSpan.FromSeconds(1.0) -> [],state
                | _ ->
                let options = 
                  if file.EndsWith "fsproj" then
                    checker.TryGetProjectOptions(file, verbose)
                  else
                    checker.TryGetCoreProjectOptions file

                match options with
                | Result.Failure s -> [Response.error serialize s],state
                | Result.Success(po, projectFiles, outFileOpt, references, logMap) ->
                    let pf = projectFiles |> List.map Path.GetFullPath |> List.map Utils.normalizePath
                    let res = Response.project serialize (file, pf, outFileOpt, references, logMap)
                    let checkOptions = pf |> List.fold (fun s f -> Map.add f po s) state.FileCheckOptions
                    let loadTimes = Map.add file time state.ProjectLoadTimes
                    let state' =  { state with FileCheckOptions = checkOptions; ProjectLoadTimes = loadTimes }
                    [res], state'
    }

    member __.Declarations (state : State) file = async {
        let file = Path.GetFullPath file
        return! match state.TryGetFileCheckerOptionsWithSource(file) with
                | Failure s -> async {return  [Response.error serialize (s)], state }
                | Success (checkOptions, source) -> async {
                    let! decls = checker.GetDeclarations(file, source, checkOptions)
                    return [Response.declarations serialize (decls)], state }
    }

    member __.Helptext (state : State) sym = async {
        return match Map.tryFind sym state.HelpText with
                | None -> [Response.error serialize (sprintf "No help text available for symbol '%s'" sym)], state
                | Some tip -> [Response.helpText serialize (sym, tip)], state
    }

    member __.CompilerLocation () =
        [Response.compilerLocation serialize Environment.fsc Environment.fsi Environment.msbuild]

    member __.Colorization (state : State) enabled =
        async.Return ([], { state with ColorizationOutput = enabled })

    member __.Error msg = async.Return [Response.error serialize msg]

    member __.Completion (state : State) (tyRes : ParseAndCheckResults) (pos: Pos) lineStr filter = async {
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

                    let helptext =
                      Seq.fold (fun m d -> Map.add (declName d) d.DescriptionText m) Map.empty decls
                    res, { state with HelpText = helptext }

                | None ->
                    [Response.error serialize "Timed out while fetching completions"], state
    }


    member __.ToolTip (tyRes : ParseAndCheckResults) (pos: Pos) lineStr =
        tyRes.TryGetToolTip pos.Line pos.Col lineStr |> serializeResult Response.toolTip

    member __.Typesig (tyRes : ParseAndCheckResults) (pos: Pos) lineStr =
        tyRes.TryGetToolTip pos.Line pos.Col lineStr |> serializeResult Response.typeSig

    member __.SymbolUse (tyRes : ParseAndCheckResults) (pos: Pos) lineStr =
        tyRes.TryGetSymbolUse pos.Line pos.Col lineStr |> serializeResult Response.symbolUse
         
    member __.SymbolUseProject (state : State) (tyRes : ParseAndCheckResults) (pos: Pos) lineStr =
        tyRes.TryGetSymbolUse pos.Line pos.Col lineStr |> serializeResultAsync (fun _ (sym, _usages) -> 
            async {
                let pChecker = checker.GetProjectChecker state.FileCheckOptions
                let! symbols = pChecker.GetUsesOfSymbol sym.Symbol
                return Response.symbolUse serialize (sym, symbols)
            })
    
    member __.FindDeclarations (tyRes : ParseAndCheckResults) (pos: Pos) lineStr =
        tyRes.TryFindDeclaration pos.Line pos.Col lineStr |> serializeResult Response.findDeclaration
    
    member __.Methods (tyRes : ParseAndCheckResults) (pos: Pos) lines =
        tyRes.TryGetMethodOverrides lines pos.Line pos.Col |> serializeResult Response.methods
    
    member __.Lint (state: State) file = async {
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