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
    ProjectLoadTimes : Map<string,DateTime>
    HelpText : Map<String, FSharpToolTipText>
    ColorizationOutput: bool
  }

  static member Initial =
    { Files = Map.empty
      FileCheckOptions = Map.empty
      ProjectLoadTimes = Map.empty
      HelpText = Map.empty
      ColorizationOutput = false }

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
  module Response = CommandResponse
  let checker = new FSharpCompilerServiceChecker()

  let mutable currentFiles = Map.empty
  let originalFs = AbstractIL.Internal.Library.Shim.FileSystem
  let fs = new FileSystem(originalFs, fun () -> currentFiles)
  AbstractIL.Internal.Library.Shim.FileSystem <- fs

  let commandQueue = new FSharpx.Control.BlockingQueueAgent<Command>(10)

  let rec main (state:State) : int =
    currentFiles <- state.Files

    try
      match commandQueue.Get() with
      | Parse(file,kind,lines) ->
          let parse fileName text options =
            let task =
              async {
                let! _parseResults, checkResults = checker.ParseAndCheckFileInProject(fileName, 0, text, options)
                match checkResults with
                | FSharpCheckFileAnswer.Aborted -> ()
                | FSharpCheckFileAnswer.Succeeded results ->
                     Response.errors(results.Errors)
                     if state.ColorizationOutput then
                       Response.colorizations(results.GetExtraColorizationsAlternate())
              }
            match kind with
            | Synchronous -> Response.info "Synchronous parsing started"
                             Async.RunSynchronously task
            | Normal -> Response.info "Background parsing started"
                        Async.StartImmediate task

          let file = Path.GetFullPath file
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

      | Project (file,time) ->
          let file = Path.GetFullPath file

          // The FileSystemWatcher often triggers multiple times for
          // each event, as editors often modify files in several steps.
          // This 'debounces' the events, by only reloading a max of once
          // per second.
          match state.ProjectLoadTimes.TryFind file with
          | Some oldtime when time - oldtime < TimeSpan.FromSeconds(1.0) -> main state
          | _ ->

          match checker.TryGetProjectOptions(file) with
          | Result.Failure s ->
              Response.error(s)
              main state

          | Result.Success(po, projectFiles, outFileOpt, references, frameworkOpt) ->
              Response.project(file, projectFiles, outFileOpt, references, frameworkOpt)

              let fsw = new FileSystemWatcher()
              fsw.Path <- Path.GetDirectoryName file
              fsw.Filter <- Path.GetFileName file
              fsw.Changed.Add(fun _ -> commandQueue.Add(Project (file, DateTime.Now)))
              fsw.EnableRaisingEvents <- true
              
              let checkOptions =
                projectFiles
                |> List.fold (fun s f -> Map.add f po s) state.FileCheckOptions
              let loadTimes = Map.add file time state.ProjectLoadTimes
              main { state with FileCheckOptions = checkOptions
                                ProjectLoadTimes = loadTimes }

      | Declarations file ->
          let file = Path.GetFullPath file
          match state.TryGetFileCheckerOptionsWithSource(file) with
          | Failure s -> Response.error(s)
          | Success (checkOptions, source) ->
              let decls = checker.GetDeclarations(file, source, checkOptions)
              Response.declarations(decls)

          main state

      | HelpText sym ->
          match Map.tryFind sym state.HelpText with
          | None -> Response.error (sprintf "No help text available for symbol '%s'" sym) 
          | Some tip -> Response.helpText(sym, tip)

          main state

      | PosCommand(cmd, file, line, col, timeout, filter) ->
          let file = Path.GetFullPath file
          match state.TryGetFileCheckerOptionsWithLinesAndLineStr(file, line, col) with
          | Failure s -> Response.error(s)
                         main state
          | Success (options, lines, lineStr) ->
            // TODO: Should sometimes pass options.Source in here to force a reparse
            //       for completions e.g. `(some typed expr).$`
            let tyResOpt = checker.TryGetRecentTypeCheckResultsForFile(file, options)
            match tyResOpt with
            | None -> Response.info "Cached typecheck results not yet available"; main state
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
                    | Some d -> Response.helpText(d.Name, d.DescriptionText)

                    Response.completion(decls)

                    let helptext =
                      Seq.fold (fun m d -> Map.add (declName d) d.DescriptionText m) Map.empty decls
                    main { state with HelpText = helptext }

                | None ->
                    Response.error "Timed out while fetching completions"
                    main state

            | ToolTip ->
                // A failure is only info here, as this command is expected to be
                // used 'on idle', and frequent errors are expected.
                match tyRes.TryGetToolTip line col lineStr with
                | Result.Failure s -> Response.info(s)
                | Result.Success tip -> Response.toolTip(tip)

                main state

            | SymbolUse ->
                // A failure is only info here, as this command is expected to be
                // used 'on idle', and frequent errors are expected.
                match tyRes.TryGetSymbolUse line col lineStr with
                | Result.Failure s -> Response.info(s)
                | Result.Success (sym,usages) -> Response.symbolUse(sym,usages)

                main state

            | FindDeclaration ->
                match tyRes.TryFindDeclaration line col lineStr with
                | Result.Failure s -> Response.error s
                | Result.Success range -> Response.findDeclaration(range)

                main state

            | Methods ->
                match tyRes.TryGetMethodOverrides lines line col with
                | Result.Failure s -> Response.error s
                | Result.Success (meth, commas) -> Response.methods(meth, commas)

                main state

      | CompilerLocation ->
          Response.compilerLocation Environment.fsc Environment.fsi Environment.msbuild
          main state

      | Colorization enabled ->
          main { state with ColorizationOutput = enabled }

      | Error(msg) ->
          Response.error msg
          main state

      | Quit ->
          0
          
    with e ->
      let msg = "Unexpected internal error. Please report at \
                 https://github.com/fsharp/FsAutoComplete/issues, \
                 attaching the exception information:\n"
                 + e.ToString()
      Response.error msg
      main state

  [<EntryPoint>]
  let entry args =
    let extra = Options.p.Parse args
    if extra.Count <> 0 then
      printfn "Unrecognised arguments: %s" (String.concat "," extra)
      1
    else
      try
        CompilerServiceInterface.addMSBuildv14BackupResolution()
        async {
          while true do
            let cmd = CommandInput.parseCommand(Console.ReadLine())
            commandQueue.Add(cmd)
        }
        |> Async.Start

        main State.Initial
      finally
        (!Debug.output).Close ()
