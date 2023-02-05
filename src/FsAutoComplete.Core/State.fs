namespace FsAutoComplete

open System
open System.Collections.Concurrent
open System.Threading
open FSharp.Compiler.Text
open Ionide.ProjInfo.ProjectSystem
open FSharp.UMX
open System.Diagnostics
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Syntax
open FSharp.Compiler.CodeAnalysis
open FsToolkit.ErrorHandling

[<AutoOpen>]
module ProjInfoExtensions =

  let private internalGetCSharpReferenceInfo =
    fun (r: FSharpReferencedProject) ->
      let rCase, fields =
        FSharp.Reflection.FSharpValue.GetUnionFields(
          r,
          typeof<FSharpReferencedProject>,
          System.Reflection.BindingFlags.Public
          ||| System.Reflection.BindingFlags.NonPublic
          ||| System.Reflection.BindingFlags.Instance
        )

      if rCase.Name = "PEReference" then
        let path: string = fields[0] :?> _
        let getStamp: unit -> DateTime = fields[1] :?> _
        let reader = fields[2]
        Some(path, getStamp, reader)
      else
        None

  let private internalGetProjectOptions =
    fun (r: FSharpReferencedProject) ->
      let rCase, fields =
        FSharp.Reflection.FSharpValue.GetUnionFields(
          r,
          typeof<FSharpReferencedProject>,
          System.Reflection.BindingFlags.Public
          ||| System.Reflection.BindingFlags.NonPublic
          ||| System.Reflection.BindingFlags.Instance
        )

      if rCase.Name = "FSharpReference" then
        let projOptions: FSharpProjectOptions = rCase.GetFields().[1].GetValue(box r) :?> _
        Some projOptions
      else
        None

  type FSharpReferencedProject with

    member x.ProjectFilePath =
      let rCase, fields =
        FSharp.Reflection.FSharpValue.GetUnionFields(
          x,
          typeof<FSharpReferencedProject>,
          System.Reflection.BindingFlags.Public
          ||| System.Reflection.BindingFlags.NonPublic
          ||| System.Reflection.BindingFlags.Instance
        )

      if rCase.Name = "FSharpReference" then
        (fields[1] :?> FSharpProjectOptions).ProjectFileName |> Some
      else
        None

  type FSharpProjectOptions with

    member x.OutputDll =
      x.OtherOptions |> Array.find (fun o -> o.StartsWith("-o:")) |> (fun s -> s[3..])

    member x.SourceFilesThatThisFileDependsOn(file: string<LocalPath>) =
      let untagged = UMX.untag file

      match Array.tryFindIndex ((=) untagged) x.SourceFiles with
      | None -> [||]
      | Some 0 -> [||] // at the start, so no possible dependents
      | Some index -> x.SourceFiles[0..index]


    member x.SourceFilesThatDependOnFile(file: string<LocalPath>) =
      let untagged = UMX.untag file

      match Array.tryFindIndex ((=) untagged) x.SourceFiles with
      | None -> [||]
      | Some index when index < x.SourceFiles.Length -> x.SourceFiles[index + 1 ..]
      | Some index -> [||] // at the end, so return empty list

  type ProjectController with

    /// returns all projects that depend on this one, transitively
    member x.GetDependentProjectsOfProjects(ps: FSharpProjectOptions list) : list<FSharpProjectOptions> =
      let projectSnapshot = x.ProjectOptions |> Seq.map snd
      let allDependents = System.Collections.Generic.HashSet<FSharpProjectOptions>()

      let currentPass = ResizeArray()
      currentPass.AddRange(ps |> List.map (fun p -> p.ProjectFileName))

      let mutable continueAlong = true

      while continueAlong do
        let dependents =
          projectSnapshot
          |> Seq.filter (fun p ->
            p.ReferencedProjects
            |> Seq.exists (fun r ->
              match r.ProjectFilePath with
              | None -> false
              | Some p -> currentPass.Contains(p)))

        if Seq.isEmpty dependents then
          continueAlong <- false
          currentPass.Clear()
        else
          for d in dependents do
            allDependents.Add d |> ignore<bool>

          currentPass.Clear()
          currentPass.AddRange(dependents |> Seq.map (fun p -> p.ProjectFileName))

      Seq.toList allDependents

    /// crawls the project set and returns projects that contain a given file
    member x.ProjectsThatContainFile(file: string<LocalPath>) =
      let untagged = UMX.untag file

      x.ProjectOptions
      |> Seq.choose (fun (_, p) ->
        if p.SourceFiles |> Array.contains untagged then
          Some p
        else
          None)
      |> Seq.distinct
      |> Seq.toList

type DeclName = string

type CompletionNamespaceInsert =
  { Namespace: string
    Position: Position
    Scope: ScopeKind }


[<DebuggerDisplay("{DebugString}")>]
type State =
  { Files: ConcurrentDictionary<string<LocalPath>, VolatileFile>
    LastCheckedVersion: ConcurrentDictionary<string<LocalPath>, int>
    ProjectController: ProjectController

    HelpText: ConcurrentDictionary<DeclName, ToolTipText>
    Declarations: ConcurrentDictionary<DeclName, DeclarationListItem * Position * string<LocalPath>>
    CompletionNamespaceInsert: ConcurrentDictionary<DeclName, CompletionNamespaceInsert>
    mutable CurrentAST: ParsedInput option

    NavigationDeclarations: ConcurrentDictionary<string<LocalPath>, NavigationTopLevelDeclaration[]>
    CancellationTokens: ConcurrentDictionary<string<LocalPath>, CancellationTokenSource list>

    ScriptProjectOptions: ConcurrentDictionary<string<LocalPath>, int * FSharpProjectOptions>

    mutable ColorizationOutput: bool

    WorkspaceStateDirectory: System.IO.DirectoryInfo }

  member x.DebugString =
    $"{x.Files.Count} Files, {x.ProjectController.ProjectOptions |> Seq.length} Projects"

  static member Initial toolsPath workspaceStateDir workspaceLoaderFactory =
    { Files = ConcurrentDictionary()
      LastCheckedVersion = ConcurrentDictionary()
      ProjectController = new ProjectController(toolsPath, workspaceLoaderFactory)
      HelpText = ConcurrentDictionary()
      Declarations = ConcurrentDictionary()
      CurrentAST = None
      CompletionNamespaceInsert = ConcurrentDictionary()
      CancellationTokens = ConcurrentDictionary()
      NavigationDeclarations = ConcurrentDictionary()
      ScriptProjectOptions = ConcurrentDictionary()
      ColorizationOutput = false
      WorkspaceStateDirectory = workspaceStateDir }

  member x.RefreshCheckerOptions(file: string<LocalPath>, text: NamedText) : FSharpProjectOptions option =
    x.ProjectController.GetProjectOptions(UMX.untag file)
    |> Option.map (fun opts ->
      x.Files.[file] <-
        { Lines = text
          Touched = DateTime.Now
          Version = None }

      opts)

  member x.GetProjectOptions(file: string<LocalPath>) : FSharpProjectOptions option =
    x.ProjectController.GetProjectOptions(UMX.untag file)

  member x.GetProjectOptions'(file: string<LocalPath>) : FSharpProjectOptions =
    (x.ProjectController.GetProjectOptions(UMX.untag file)).Value

  member x.RemoveProjectOptions(file: string<LocalPath>) : unit =
    x.ProjectController.RemoveProjectOptions(UMX.untag file)

  member x.FSharpProjectOptions = x.ProjectController.ProjectOptions

  member x.TryGetFileVersion(file: string<LocalPath>) : int option =
    x.Files.TryFind file |> Option.bind (fun f -> f.Version)

  member x.TryGetLastCheckedVersion(file: string<LocalPath>) : int option = x.LastCheckedVersion.TryFind file

  member x.SetFileVersion (file: string<LocalPath>) (version: int) =
    x.Files.TryFind file
    |> Option.iter (fun n ->
      let fileState = { n with Version = Some version }
      x.Files.[file] <- fileState)

  member x.SetLastCheckedVersion (file: string<LocalPath>) (version: int) = x.LastCheckedVersion.[file] <- version

  member x.AddFileTextAndCheckerOptions(file: string<LocalPath>, text: NamedText, opts, version) =
    let fileState =
      { Lines = text
        Touched = DateTime.Now
        Version = version }

    x.Files.[file] <- fileState
    x.ProjectController.SetProjectOptions(UMX.untag file, opts)

  member x.AddFileText(file: string<LocalPath>, text: NamedText, version) =
    let fileState =
      { Lines = text
        Touched = DateTime.Now
        Version = version }

    x.Files.[file] <- fileState

  member x.AddCancellationToken(file: string<LocalPath>, token: CancellationTokenSource) =
    x.CancellationTokens.AddOrUpdate(file, [ token ], (fun _ lst -> token :: lst))
    |> ignore

  member x.GetCancellationTokens(file: string<LocalPath>) =
    let lst = x.CancellationTokens.GetOrAdd(file, (fun _ -> []))
    x.CancellationTokens.TryRemove(file) |> ignore
    lst

  static member private FileWithoutProjectOptions(file: string<LocalPath>) =
    let opts = [| yield sprintf "-r:%s" Environment.fsharpCore; yield "--noframework" |]

    { ProjectId = Some((UMX.untag file) + ".fsproj")
      ProjectFileName = (UMX.untag file) + ".fsproj"
      SourceFiles = [| (UMX.untag file) |]
      OtherOptions = opts // "--noframework"
      ReferencedProjects = [||]
      IsIncompleteTypeCheckEnvironment = true
      UseScriptResolutionRules = false
      LoadTime = DateTime.Now
      UnresolvedReferences = None
      OriginalLoadReferences = []
      Stamp = None }

  member x.TryGetFileCheckerOptionsWithLines
    (file: string<LocalPath>)
    : ResultOrString<FSharpProjectOptions * NamedText> =
    match x.Files.TryFind(file) with
    | None -> ResultOrString.Error(sprintf "File '%s' not parsed" (UMX.untag file))
    | Some(volFile) ->

      match x.ProjectController.GetProjectOptions((UMX.untag file)) with
      | None -> Ok(State.FileWithoutProjectOptions(file), volFile.Lines)
      | Some opts -> Ok(opts, volFile.Lines)

  member x.TryGetFileCheckerOptionsWithSource
    (file: string<LocalPath>)
    : ResultOrString<FSharpProjectOptions * NamedText> =
    match x.TryGetFileCheckerOptionsWithLines(file) with
    | ResultOrString.Error x -> ResultOrString.Error x
    | Ok(opts, lines) -> Ok(opts, lines)

  member x.TryGetFileSource(file: string<LocalPath>) : ResultOrString<NamedText> =
    match x.Files.TryFind(file) with
    | None -> ResultOrString.Error(sprintf "File '%s' not parsed" (UMX.untag file))
    | Some f -> Ok f.Lines

  member x.TryGetFileCheckerOptionsWithLinesAndLineStr
    (
      file: string<LocalPath>,
      pos: Position
    ) : ResultOrString<FSharpProjectOptions * NamedText * LineStr> =
    result {
      let! (opts, text) = x.TryGetFileCheckerOptionsWithLines(file)

      let! line = text.GetLine pos |> Result.ofOption (fun _ -> "Position is out of range")

      return (opts, text, line)
    }

  /// Removes `file` from all caches
  member x.Forget(file: string<LocalPath>) =
    x.LastCheckedVersion.TryRemove(file) |> ignore
    x.Files.TryRemove(file) |> ignore
    x.ProjectController.RemoveProjectOptions(UMX.untag file) |> ignore
    x.ScriptProjectOptions.TryRemove(file) |> ignore
