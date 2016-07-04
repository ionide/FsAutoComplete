namespace FsAutoComplete

open System
open Microsoft.FSharp.Compiler.SourceCodeServices
open System.IO

type SerializedProjectResponse = string

type private ProjectMessage =
    | Changed of DateTime
    | GetResponse of AsyncReplyChannel<SerializedProjectResponse option>
    | SetResponse of SerializedProjectResponse option

type Project (projectFile, onChange) =
    let agent = MailboxProcessor.Start <| fun mb ->
        let rec loop (lastWriteTime, response) = async {
            let! msg = mb.Receive()
            match msg with
            | Changed lwt when lwt <> lastWriteTime ->
                onChange projectFile
                return! loop (lwt, None)
            | Changed _ -> return! loop (lastWriteTime, response)
            | GetResponse r ->
                r.Reply response
                return! loop (lastWriteTime, response)
            | SetResponse r ->
                return! loop (lastWriteTime, r)
        }    
        loop (File.GetLastWriteTimeUtc projectFile, None)

    let fullPath = Path.GetFullPath projectFile

    let fsw = 
        new FileSystemWatcher(
            Path = Path.GetDirectoryName fullPath, 
            Filter = Path.GetFileName fullPath,
            NotifyFilter = NotifyFilters.LastWrite)

    do fsw.Changed.Add (fun _ -> agent.Post (Changed (File.GetLastWriteTime projectFile)))
    do fsw.EnableRaisingEvents <- true

    member __.Response with get() = agent.PostAndReply GetResponse
                        and set r = agent.Post (SetResponse r)

    interface IDisposable with
        member __.Dispose() = fsw.Dispose()

type State =
  {
    Files : Map<string,VolatileFile>
    FileCheckOptions : Map<string,FSharpProjectOptions>
    Projects : Map<CommandResponse.ProjectFile, Project>
    HelpText : Map<String, FSharpToolTipText>
    ColorizationOutput: bool 
  }

  static member Initial =
    { Files = Map.empty
      FileCheckOptions = Map.empty
      Projects = Map.empty
      HelpText = Map.empty
      ColorizationOutput = false }

  member x.WithFileTextGetCheckerOptions(file, lines) : State * FSharpProjectOptions =
    let file = Utils.normalizePath file

    let opts =
      match x.FileCheckOptions.TryFind file with
      | None -> State.FileWithoutProjectOptions(file)
      | Some opts -> opts
    let fileState = { Lines = lines; Touched = DateTime.Now }
    { x with Files = Map.add file fileState x.Files
             FileCheckOptions = Map.add file opts x.FileCheckOptions }, opts

  member x.WithFileTextAndCheckerOptions(file, lines, opts) =
    let file = Utils.normalizePath file
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

  member x.TryGetFileCheckerOptionsWithLines(file) : Result<FSharpProjectOptions * string[]> =
    let file = Utils.normalizePath file
    match x.Files.TryFind(file) with
    | None -> Failure (sprintf "File '%s' not parsed" file)
    | Some (volFile) ->

      match x.FileCheckOptions.TryFind(file) with
      | None -> Success (State.FileWithoutProjectOptions(file), volFile.Lines)
      | Some opts -> Success (opts, volFile.Lines)

  member x.TryGetFileCheckerOptionsWithSource(file) : Result<FSharpProjectOptions * string> =
    let file = Utils.normalizePath file
    match x.TryGetFileCheckerOptionsWithLines(file) with
    | Failure x -> Failure x
    | Success (opts, lines) -> Success (opts, String.concat "\n" lines)

  member x.TryGetFileCheckerOptionsWithLinesAndLineStr(file, pos) : Result<FSharpProjectOptions * string[] * string> =
    let file = Utils.normalizePath file
    match x.TryGetFileCheckerOptionsWithLines(file) with
    | Failure x -> Failure x
    | Success (opts, lines) ->
      let ok = pos.Line <= lines.Length && pos.Line >= 1 &&
               pos.Col <= lines.[pos.Line - 1].Length + 1 && pos.Col >= 1
      if not ok then Failure "Position is out of range"
      else Success (opts, lines, lines.[pos.Line - 1])
