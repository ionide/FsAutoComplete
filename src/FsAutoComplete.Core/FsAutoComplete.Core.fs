namespace FsAutoComplete

open System
open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpLint.Application

module Response = CommandResponse

module Commands =
    type Serializer = obj -> string

    type Context = 
        { Serialize: Serializer
          State: State
          TyRes : ParseAndCheckResults 
          Checker : FSharpCompilerServiceChecker }

    let parse (serialize : Serializer) (state : State) (checker : FSharpCompilerServiceChecker) file lines = async {
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

    let project (serialize : Serializer) (state : State) (checker : FSharpCompilerServiceChecker) file time verbose = async {
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

    let declarations (serialize : Serializer) (state : State) (checker : FSharpCompilerServiceChecker) file = async {
        let file = Path.GetFullPath file
        return! match state.TryGetFileCheckerOptionsWithSource(file) with
                | Failure s -> async {return  [Response.error serialize (s)], state }
                | Success (checkOptions, source) -> async {
                    let! decls = checker.GetDeclarations(file, source, checkOptions)
                    return [Response.declarations serialize (decls)], state }
    }

    let helptext (serialize : Serializer) (state : State) sym = async {
        return match Map.tryFind sym state.HelpText with
                | None -> [Response.error serialize (sprintf "No help text available for symbol '%s'" sym)], state
                | Some tip -> [Response.helpText serialize (sym, tip)], state
    }

    let compilerLocation (serialize : Serializer) (state : State) = async {
        return [Response.compilerLocation serialize Environment.fsc Environment.fsi Environment.msbuild], state
    }

    let colorization (state : State) enabled = async {
        return [], { state with ColorizationOutput = enabled }
    }

    let error (serialize : Serializer) (state : State) msg = async {
        return [Response.error serialize msg], state
    }

    let completion (ctx: Context) (pos: Pos) lineStr filter = async {
        let! res = ctx.TyRes.TryGetCompletions pos lineStr filter
        return match res with
                | Some (decls, residue) ->
                    let declName (d: FSharpDeclarationListItem) = d.Name

                    // Send the first helptext without being requested.
                    // This allows it to be displayed immediately in the editor.
                    let firstMatchOpt =
                      Array.sortBy declName decls
                      |> Array.tryFind (fun d -> (declName d).StartsWith(residue, StringComparison.InvariantCultureIgnoreCase))
                    let res = match firstMatchOpt with
                                | None -> [Response.completion ctx.Serialize (decls)]
                                | Some d ->
                                    [Response.helpText ctx.Serialize (d.Name, d.DescriptionText)
                                     Response.completion ctx.Serialize (decls)]

                    let helptext =
                      Seq.fold (fun m d -> Map.add (declName d) d.DescriptionText m) Map.empty decls
                    res, { ctx.State with HelpText = helptext }

                | None ->
                    [Response.error ctx.Serialize "Timed out while fetching completions"], ctx.State
    }

    let toolTip (ctx: Context) (pos: Pos) lineStr = async {
        // A failure is only info here, as this command is expected to be
        // used 'on idle', and frequent errors are expected.
        let! res = ctx.TyRes.TryGetToolTip pos.Line pos.Col lineStr
        return match res with
                | Result.Failure s -> [Response.info ctx.Serialize (s)], ctx.State
                | Result.Success tip -> [Response.toolTip ctx.Serialize tip], ctx.State
    }

    let typesig (ctx: Context) (pos: Pos) lineStr = async {
        // A failure is only info here, as this command is expected to be
        // used 'on idle', and frequent errors are expected.
        let! res = ctx.TyRes.TryGetToolTip pos.Line pos.Col lineStr
        return match res with
                | Result.Failure s -> [Response.info ctx.Serialize (s)], ctx.State
                | Result.Success tip -> [Response.typeSig ctx.Serialize tip], ctx.State
    }

    let symbolUse (ctx: Context) (pos: Pos) lineStr = async {
        // A failure is only info here, as this command is expected to be
        // used 'on idle', and frequent errors are expected.
        let! res = ctx.TyRes.TryGetSymbolUse pos.Line pos.Col lineStr
        return match res with
                | Result.Failure s -> [Response.info ctx.Serialize (s)], ctx.State
                | Result.Success (sym,usages) -> [Response.symbolUse ctx.Serialize (sym,usages)], ctx.State
    } 
     
    let symbolUseProject (ctx: Context) (pos: Pos) lineStr = async {
        let! res = ctx.TyRes.TryGetSymbolUse pos.Line pos.Col lineStr
        match res with
            | Result.Failure s -> return [Response.info ctx.Serialize (s)], ctx.State
            | Result.Success (sym, _usages) -> 
                let pChecker = ctx.Checker.GetProjectChecker(ctx.State.FileCheckOptions)
                let! symbols = pChecker.GetUsesOfSymbol sym.Symbol
                return [Response.symbolUse ctx.Serialize (sym,symbols)], ctx.State
    }

    let findDeclarations (ctx: Context) (pos: Pos) lineStr = async {
        let! res = ctx.TyRes.TryFindDeclaration pos.Line pos.Col lineStr
        return match res with
                | Result.Failure s -> [Response.error ctx.Serialize (s)], ctx.State
                | Result.Success range -> [Response.findDeclaration ctx.Serialize range], ctx.State
    }

    let methods (ctx: Context) (pos: Pos) lines = async {
        let! res = ctx.TyRes.TryGetMethodOverrides lines pos.Line pos.Col
        return match res with
                | Result.Failure s -> [Response.error ctx.Serialize (s)], ctx.State
                | Result.Success (meth, commas) -> [Response.methods ctx.Serialize (meth, commas)], ctx.State
    }

    let lint (serialize: Serializer) (state: State) (checker : FSharpCompilerServiceChecker)  file = async {
        let file = Path.GetFullPath file
        let res =
            match state.TryGetFileCheckerOptionsWithSource file with
            | Failure s -> [Response.error serialize (s)], state
            | Success (options,source) ->
            
            let tyResOpt = checker.TryGetRecentTypeCheckResultsForFile(file, options)
            
            match tyResOpt with
            | None -> [ Response.info serialize "Cached typecheck results not yet available"], state
            | Some tyRes ->
                match tyRes.GetAST with
                | None -> [ Response.info serialize "Something went wrong during parsing"], state
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
                
                    res', state
        return res

    }