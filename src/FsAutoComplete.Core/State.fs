namespace FsAutoComplete

open System
open Microsoft.FSharp.Compiler.SourceCodeServices
open System.Collections.Concurrent
open System.Threading
open Priority_Queue
open Microsoft.FSharp.Compiler.Range

type DeclName = string
type CompletionNamespaceInsert = string * int * int

type State =
  {
    Files : ConcurrentDictionary<SourceFilePath, VolatileFile>
    FileCheckOptions : ConcurrentDictionary<SourceFilePath, FSharpProjectOptions>
    Projects : ConcurrentDictionary<ProjectFilePath, Project>

    HelpText : ConcurrentDictionary<DeclName, FSharpToolTipText>
    Declarations: ConcurrentDictionary<DeclName, FSharpDeclarationListItem * pos * SourceFilePath>
    CompletionNamespaceInsert : ConcurrentDictionary<DeclName, CompletionNamespaceInsert>
    mutable CurrentAST: Microsoft.FSharp.Compiler.Ast.ParsedInput option

    NavigationDeclarations : ConcurrentDictionary<SourceFilePath, FSharpNavigationTopLevelDeclaration[]>
    CancellationTokens: ConcurrentDictionary<SourceFilePath, CancellationTokenSource list>
    BackgroundProjects: SimplePriorityQueue<FSharpProjectOptions, int>
    mutable ColorizationOutput: bool
  }

  static member Initial =
    { Files = ConcurrentDictionary()
      FileCheckOptions = ConcurrentDictionary()
      Projects = ConcurrentDictionary()
      HelpText = ConcurrentDictionary()
      Declarations = ConcurrentDictionary()
      CurrentAST = None
      CompletionNamespaceInsert = ConcurrentDictionary()
      CancellationTokens = ConcurrentDictionary()
      NavigationDeclarations = ConcurrentDictionary()
      BackgroundProjects = SimplePriorityQueue<_, _>()
      ColorizationOutput = false }

  member x.GetCheckerOptions(file: SourceFilePath, lines: LineStr[]) : FSharpProjectOptions option =
    let file = Utils.normalizePath file

    x.FileCheckOptions.TryFind file
    |> Option.map (fun opts ->
        x.Files.[file] <- { Lines = lines; Touched = DateTime.Now }
        x.FileCheckOptions.[file] <- opts
        opts
    )

  member x.AddFileTextAndCheckerOptions(file: SourceFilePath, lines: LineStr[], opts) =
    let file = Utils.normalizePath file
    let fileState = { Lines = lines; Touched = DateTime.Now }
    x.Files.[file] <- fileState
    x.FileCheckOptions.[file] <- opts

  member x.AddCancellationToken(file : SourceFilePath, token: CancellationTokenSource) =
    x.CancellationTokens.AddOrUpdate(file, [token], fun _ lst -> token::lst)
    |> ignore

  member x.GetCancellationTokens(file : SourceFilePath) =
    let lst = x.CancellationTokens.GetOrAdd(file, fun _ -> [])
    x.CancellationTokens.TryRemove(file) |> ignore
    lst

  static member private FileWithoutProjectOptions(file) =
    let opts=
        defaultArg (Environment.fsharpCoreOpt  |> Option.map (fun path -> [| yield sprintf "-r:%s" path; yield "--noframework" |] )) [|"--noframework"|]

    { ProjectFileName = file + ".fsproj"
      SourceFiles = [|file|]
      OtherOptions = opts // "--noframework"
      ReferencedProjects = [| |]
      IsIncompleteTypeCheckEnvironment = true
      UseScriptResolutionRules = false
      LoadTime = DateTime.Now
      UnresolvedReferences = None
      OriginalLoadReferences = []
      ExtraProjectInfo = None
      Stamp = None}

  member x.TryGetFileCheckerOptionsWithLines(file: SourceFilePath) : ResultOrString<FSharpProjectOptions * LineStr[]> =
    let file = Utils.normalizePath file
    match x.Files.TryFind(file) with
    | None -> ResultOrString.Error (sprintf "File '%s' not parsed" file)
    | Some (volFile) ->

      match x.FileCheckOptions.TryFind(file) with
      | None -> Ok (State.FileWithoutProjectOptions(file), volFile.Lines)
      | Some opts -> Ok (opts, volFile.Lines)

  member x.TryGetFileCheckerOptionsWithSource(file: SourceFilePath) : ResultOrString<FSharpProjectOptions * string> =
    let file = Utils.normalizePath file
    match x.TryGetFileCheckerOptionsWithLines(file) with
    | ResultOrString.Error x -> ResultOrString.Error x
    | Ok (opts, lines) -> Ok (opts, String.concat "\n" lines)

  member x.TryGetFileCheckerOptionsWithLinesAndLineStr(file: SourceFilePath, pos : pos) : ResultOrString<FSharpProjectOptions * LineStr[] * LineStr> =
    let file = Utils.normalizePath file
    match x.TryGetFileCheckerOptionsWithLines(file) with
    | ResultOrString.Error x -> ResultOrString.Error x
    | Ok (opts, lines) ->
      let ok = pos.Line <= lines.Length && pos.Line >= 1 &&
               pos.Column <= lines.[pos.Line - 1].Length + 1 && pos.Column >= 1
      if not ok then ResultOrString.Error "Position is out of range"
      else Ok (opts, lines, lines.[pos.Line - 1])

  member x.EnqueueProjectForBackgroundParsing(opts: FSharpProjectOptions, priority: int) =
    if x.BackgroundProjects.Contains opts then
      match x.BackgroundProjects.TryGetPriority opts with
      | true, pr when pr > priority -> x.BackgroundProjects.TryUpdatePriority (opts, priority) |> ignore
      | false, _ -> x.BackgroundProjects.Enqueue(opts, priority)
      | _ -> ()
    else
      x.BackgroundProjects.Enqueue(opts, priority)
