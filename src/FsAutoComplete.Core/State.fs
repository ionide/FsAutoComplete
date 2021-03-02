namespace FsAutoComplete

open System
open FSharp.Compiler.SourceCodeServices
open System.Collections.Concurrent
open System.Threading
open FSharp.Compiler.Text
open Ionide.ProjInfo.ProjectSystem
open FSharp.UMX

type DeclName = string
type CompletionNamespaceInsert = { Namespace: string; Position: Pos; Scope : ScopeKind }

type State =
  {
    Files : ConcurrentDictionary<string<LocalPath>, VolatileFile>
    LastCheckedVersion: ConcurrentDictionary<string<LocalPath>, int>
    ProjectController: ProjectController

    HelpText : ConcurrentDictionary<DeclName, FSharpToolTipText>
    Declarations: ConcurrentDictionary<DeclName, FSharpDeclarationListItem * Pos * string<LocalPath>>
    CompletionNamespaceInsert : ConcurrentDictionary<DeclName, CompletionNamespaceInsert>
    mutable CurrentAST: FSharp.Compiler.SyntaxTree.ParsedInput option

    NavigationDeclarations : ConcurrentDictionary<string<LocalPath>, FSharpNavigationTopLevelDeclaration[]>
    CancellationTokens: ConcurrentDictionary<string<LocalPath>, CancellationTokenSource list>

    ScriptProjectOptions: ConcurrentDictionary<string<LocalPath>, int * FSharpProjectOptions>

    mutable ColorizationOutput: bool
  }

  static member Initial toolsPath =
    { Files = ConcurrentDictionary()
      LastCheckedVersion = ConcurrentDictionary()
      ProjectController = ProjectController(toolsPath)
      HelpText = ConcurrentDictionary()
      Declarations = ConcurrentDictionary()
      CurrentAST = None
      CompletionNamespaceInsert = ConcurrentDictionary()
      CancellationTokens = ConcurrentDictionary()
      NavigationDeclarations = ConcurrentDictionary()
      ScriptProjectOptions = ConcurrentDictionary()
      ColorizationOutput = false }

  member x.GetCheckerOptions(file: string<LocalPath>, lines: LineStr[]) : FSharpProjectOptions option =
    x.ProjectController.GetProjectOptions (UMX.untag file)
    |> Option.map (fun opts ->
        x.Files.[file] <- { Lines = lines; Touched = DateTime.Now; Version = None }
        opts
    )

  member x.GetProjectOptions(file: string<LocalPath>) : FSharpProjectOptions option =
    x.ProjectController.GetProjectOptions (UMX.untag file)

  member x.GetProjectOptions'(file: string<LocalPath>) : FSharpProjectOptions =
    (x.ProjectController.GetProjectOptions (UMX.untag file)).Value

  member x.RemoveProjectOptions(file: string<LocalPath>) : unit =
    x.ProjectController.RemoveProjectOptions (UMX.untag file)

  member x.FSharpProjectOptions = x.ProjectController.ProjectOptions

  member x.TryGetFileVersion (file: string<LocalPath>) : int option =
    x.Files.TryFind file
    |> Option.bind (fun f -> f.Version)

  member x.TryGetLastCheckedVersion (file: string<LocalPath>) : int option =
    x.LastCheckedVersion.TryFind file

  member x.SetFileVersion (file: string<LocalPath>) (version: int) =
    x.Files.TryFind file
    |> Option.iter (fun n ->
      let fileState = {n with Version = Some version}
      x.Files.[file] <- fileState
    )

  member x.SetLastCheckedVersion (file: string<LocalPath>) (version: int) =
    x.LastCheckedVersion.[file] <- version

  member x.AddFileTextAndCheckerOptions(file: string<LocalPath>, lines: LineStr[], opts, version) =
    let fileState = { Lines = lines; Touched = DateTime.Now; Version = version }
    x.Files.[file] <- fileState
    x.ProjectController.SetProjectOptions(UMX.untag file, opts)

  member x.AddFileText(file: string<LocalPath>, lines: LineStr[], version) =
    let fileState = { Lines = lines; Touched = DateTime.Now; Version = version }
    x.Files.[file] <- fileState

  member x.AddCancellationToken(file : string<LocalPath>, token: CancellationTokenSource) =
    x.CancellationTokens.AddOrUpdate(file, [token], fun _ lst -> token::lst)
    |> ignore

  member x.GetCancellationTokens(file : string<LocalPath>) =
    let lst = x.CancellationTokens.GetOrAdd(file, fun _ -> [])
    x.CancellationTokens.TryRemove(file) |> ignore
    lst

  static member private FileWithoutProjectOptions(file: string<LocalPath>) =
    let opts = [| yield sprintf "-r:%s" Environment.fsharpCore; yield "--noframework" |]

    { ProjectId = Some ((UMX.untag file) + ".fsproj")
      ProjectFileName = (UMX.untag file) + ".fsproj"
      SourceFiles = [|(UMX.untag file)|]
      OtherOptions = opts // "--noframework"
      ReferencedProjects = [| |]
      IsIncompleteTypeCheckEnvironment = true
      UseScriptResolutionRules = false
      LoadTime = DateTime.Now
      UnresolvedReferences = None
      OriginalLoadReferences = []
      ExtraProjectInfo = None
      Stamp = None}

  member x.TryGetFileCheckerOptionsWithLines(file: string<LocalPath>) : ResultOrString<FSharpProjectOptions * LineStr[]> =
    match x.Files.TryFind(file) with
    | None -> ResultOrString.Error (sprintf "File '%s' not parsed" (UMX.untag file))
    | Some (volFile) ->

      match x.ProjectController.GetProjectOptions((UMX.untag file)) with
      | None -> Ok (State.FileWithoutProjectOptions(file), volFile.Lines)
      | Some opts -> Ok (opts, volFile.Lines)

  member x.TryGetFileCheckerOptionsWithSource(file: string<LocalPath>) : ResultOrString<FSharpProjectOptions * string> =
    match x.TryGetFileCheckerOptionsWithLines(file) with
    | ResultOrString.Error x -> ResultOrString.Error x
    | Ok (opts, lines) -> Ok (opts, String.concat "\n" lines)

  member x.TryGetFileSource(file: string<LocalPath>) : ResultOrString<string[]> =
    match x.Files.TryFind(file) with
    | None -> ResultOrString.Error (sprintf "File '%s' not parsed" (UMX.untag file))
    | Some f -> Ok (f.Lines)

  member x.TryGetFileCheckerOptionsWithLinesAndLineStr(file: string<LocalPath>, pos : Pos) : ResultOrString<FSharpProjectOptions * LineStr[] * LineStr> =
    match x.TryGetFileCheckerOptionsWithLines(file) with
    | ResultOrString.Error x -> ResultOrString.Error x
    | Ok (opts, lines) ->
      let ok = pos.Line <= lines.Length && pos.Line >= 1 &&
               pos.Column <= lines.[pos.Line - 1].Length + 1 && pos.Column >= 1
      if not ok then ResultOrString.Error "Position is out of range"
      else Ok (opts, lines, lines.[pos.Line - 1])
