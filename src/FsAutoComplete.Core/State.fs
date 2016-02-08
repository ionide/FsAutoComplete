namespace FsAutoComplete

open System
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices

type State =
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
