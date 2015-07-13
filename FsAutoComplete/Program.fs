namespace FsAutoComplete

open System
open System.IO

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices

open Newtonsoft.Json
open Newtonsoft.Json.Converters

type internal State =
  {
    Files : Map<string,VolatileFile>
    FileCheckOptions : Map<string,FSharpProjectOptions>
    HelpText : Map<String, FSharpToolTipText>
  }

  static member Initial =
    { Files = Map.empty
      FileCheckOptions = Map.empty
      HelpText = Map.empty }

  member x.WithFileTextGetCheckerOptions(file, lines) : State * FSharpProjectOptions =
    let opts =
      match x.FileCheckOptions.TryFind file with
      | None -> State.FileWithoutProjectOptions(file)
      | Some opts -> opts
    let fileState = { Lines = lines; Touched = DateTime.Now }
    { x with Files = Map.add file fileState x.Files
             FileCheckOptions = Map.add file opts x.FileCheckOptions }, opts

  member x.WithFileTextAndCheckerOptions(file, lines, opts) =
    let fileState = { Lines = lines; Touched = DateTime.Now }
    { x with Files = Map.add file fileState x.Files
             FileCheckOptions = Map.add file opts x.FileCheckOptions }

  static member private FileWithoutProjectOptions(file) =
    { ProjectFileName = file + ".fsproj"
      ProjectFileNames = [|file|]
      OtherOptions = [|"--noframework"|]
      ReferencedProjects = [| |]
      IsIncompleteTypeCheckEnvironment = true
      UseScriptResolutionRules = false
      LoadTime = DateTime.Now
      UnresolvedReferences = None }

  member private x.TryGetFileCheckerOptionsWithLines(file) : Result<FSharpProjectOptions * string[]> =
    match x.Files.TryFind(file) with
    | None -> Failure (sprintf "File '%s' not parsed" file)
    | Some (volFile) ->

      match x.FileCheckOptions.TryFind(file) with
      | None -> Success (State.FileWithoutProjectOptions(file), volFile.Lines)
      | Some opts -> Success (opts, volFile.Lines)

  member x.TryGetFileCheckerOptionsWithSource(file) : Result<FSharpProjectOptions * string> =
    match x.TryGetFileCheckerOptionsWithLines(file) with
    | Failure x -> Failure x
    | Success (opts, lines) -> Success (opts, String.concat "\n" lines)

  member x.TryGetFileCheckerOptionsWithLinesAndLineStr(file, line, col) : Result<FSharpProjectOptions * string[] * string> =
    match x.TryGetFileCheckerOptionsWithLines(file) with
    | Failure x -> Failure x
    | Success (opts, lines) ->
      let ok = line <= lines.Length && line >= 1 &&
               col <= lines.[line - 1].Length + 1 && col >= 1
      if not ok then Failure "Position is out of range"
      else Success (opts, lines, lines.[line - 1])

module internal Main =
  let respond = new CommandResponse.ResponseAgent()
  let checker = new FSharpCompilerServiceChecker()

  let mutable currentFiles = Map.empty
  let originalFs = AbstractIL.Internal.Library.Shim.FileSystem
  let fs = new FileSystem(originalFs, fun () -> currentFiles)
  AbstractIL.Internal.Library.Shim.FileSystem <- fs

  let rec main (state:State) : int =
    currentFiles <- state.Files

    match CommandInput.parseCommand(Console.ReadLine()) with
    | Parse(file,kind) ->
        let parse fileName text options =
          let task =
            async {
              let! _parseResults, checkResults = checker.ParseAndCheckFileInProject(fileName, 0, text, options)
              match checkResults with
              | FSharpCheckFileAnswer.Aborted -> ()
              | FSharpCheckFileAnswer.Succeeded results -> respond.Errors(results.Errors)
            }
          match kind with
          | Synchronous -> respond.Info "Synchronous parsing started"
                           Async.RunSynchronously task
          | Normal -> respond.Info "Background parsing started"
                      Async.StartImmediate task

        let file = Path.GetFullPath file
        let lines = CommandInput.readInput [] |> Array.ofList

        let text = String.concat "\n" lines

        if Utils.isAScript file then
          let checkOptions = checker.GetProjectOptionsFromScript(file, text)
          let state = state.WithFileTextAndCheckerOptions(file, lines, checkOptions)
          parse file text checkOptions
          main state
        else
          let state, checkOptions = state.WithFileTextGetCheckerOptions(file, lines)
          parse file text checkOptions
          main state

    | Project file ->
        let file = Path.GetFullPath file
        match checker.TryGetProjectOptions(file) with
        | Result.Failure s ->
            respond.Error(s)
            main state

        | Result.Success(po, projectFiles, outFileOpt, references, frameworkOpt) ->
            respond.Project(file, projectFiles, outFileOpt, references, frameworkOpt)

            let projects =
              projectFiles
              |> List.fold (fun s f -> Map.add f po s) state.FileCheckOptions
            main { state with FileCheckOptions = projects }

    | Declarations file ->
        let file = Path.GetFullPath file
        match state.TryGetFileCheckerOptionsWithSource(file) with
        | Failure s -> respond.Error(s)
        | Success (checkOptions, source) ->
            let decls = checker.GetDeclarations(file, source, checkOptions)
            respond.Declarations(decls)

        main state

    | HelpText sym ->
        match Map.tryFind sym state.HelpText with
        | None -> respond.Error (sprintf "No help text available for symbol '%s'" sym) 
        | Some tip -> respond.HelpText(sym, tip)

        main state

    | PosCommand(cmd, file, line, col, timeout, filter) ->
        let file = Path.GetFullPath file
        match state.TryGetFileCheckerOptionsWithLinesAndLineStr(file, line, col) with
        | Failure s -> respond.Error(s)
                       main state
        | Success (options, lines, lineStr) ->
          // TODO: Should sometimes pass options.Source in here to force a reparse
          //       for completions e.g. `(some typed expr).$`
          let tyResOpt = checker.TryGetRecentTypeCheckResultsForFile(file, options)
          match tyResOpt with
          | None -> respond.Error "Cached typecheck results not yet available"; main state
          | Some tyRes ->

          match cmd with
          | Completion ->
              match tyRes.TryGetCompletions line col lineStr timeout filter with
              | Some (decls, residue) ->
                  let declName (d: FSharpDeclarationListItem) = d.Name

                  // Send the first helptext without being requested.
                  // This allows it to be displayed immediately in the editor.
                  let firstMatchOpt =
                    Array.sortBy declName decls
                    |> Array.tryFind (fun d -> (declName d).StartsWith residue)
                  match firstMatchOpt with
                  | None -> ()
                  | Some d -> respond.HelpText(d.Name, d.DescriptionText)

                  respond.Completion(decls)
                  
                  let helptext =
                    Seq.fold (fun m d -> Map.add (declName d) d.DescriptionText m) Map.empty decls
                  main { state with HelpText = helptext }

              | None ->
                  respond.Error "Timed out while fetching completions"
                  main state

          | ToolTip ->
              // A failure is only info here, as this command is expected to be
              // used 'on idle', and frequent errors are expected.
              match tyRes.TryGetToolTip line col lineStr with
              | Result.Failure s -> respond.Info(s)
              | Result.Success tip -> respond.ToolTip(tip)
  
              main state

          | SymbolUse ->
              // A failure is only info here, as this command is expected to be
              // used 'on idle', and frequent errors are expected.
              match tyRes.TryGetSymbolUse line col lineStr with
              | Result.Failure s -> respond.Info(s)
              | Result.Success su -> respond.SymbolUse(su)

              main state

          | FindDeclaration ->
              match tyRes.TryFindDeclaration line col lineStr with
              | Result.Failure s -> respond.Error s
              | Result.Success range -> respond.FindDeclaration(range)

              main state

          | Methods ->
              match tyRes.TryGetMethodOverrides lines line col with
              | Result.Failure s -> respond.Error s
              | Result.Success (meth, commas) -> respond.Method(meth, commas)

              main state

    | CompilerLocation ->
        let locopt = FSharpEnvironment.BinFolderOfDefaultFSharpCompiler None
        match locopt with
        | None -> respond.Error "Could not find compiler"
        | Some loc -> respond.Message("compilerlocation", loc)

        main state

    | Error(msg) ->
        respond.Error msg
        main state

    | Quit ->
        respond.Quit()
        (!Debug.output).Close ()
        0

  [<EntryPoint>]
  let entry args =
    // System.Diagnostics.Debug.Listeners.Add(
    //   new System.Diagnostics.TextWriterTraceListener(Console.Out))
    // |> ignore
    let extra = Options.p.Parse args
    if extra.Count <> 0 then
      printfn "Unrecognised arguments: %s" (String.concat "," extra)
      1
    else
      try
        main State.Initial
      finally
        (!Debug.output).Close ()
