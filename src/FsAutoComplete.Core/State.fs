namespace FsAutoComplete

open System
open FSharp.Compiler.SourceCodeServices
open System.Collections.Concurrent
open System.Threading
open FSharp.Compiler.Range
open FSharp.Analyzers.SDK
open ProjectSystem

type DeclName = string
type CompletionNamespaceInsert = string * int * int * string

type State =
  {
    Files : ConcurrentDictionary<SourceFilePath, VolatileFile>
    LastCheckedVersion: ConcurrentDictionary<SourceFilePath, int>
    FileCheckOptions : ConcurrentDictionary<SourceFilePath, FSharpProjectOptions>
    Projects : ConcurrentDictionary<ProjectFilePath, Project>

    HelpText : ConcurrentDictionary<DeclName, FSharpToolTipText>
    Declarations: ConcurrentDictionary<DeclName, FSharpDeclarationListItem * pos * SourceFilePath>
    CompletionNamespaceInsert : ConcurrentDictionary<DeclName, CompletionNamespaceInsert>
    mutable CurrentAST: FSharp.Compiler.Ast.ParsedInput option

    NavigationDeclarations : ConcurrentDictionary<SourceFilePath, FSharpNavigationTopLevelDeclaration[]>
    ParseResults: ConcurrentDictionary<SourceFilePath, FSharpParseFileResults>
    CancellationTokens: ConcurrentDictionary<SourceFilePath, CancellationTokenSource list>
    Analyzers: ConcurrentDictionary<FilePath, Analyzer list>

    mutable WorkspaceRoot: string

    mutable ColorizationOutput: bool
  }

  static member Initial =
    { Files = ConcurrentDictionary()
      LastCheckedVersion = ConcurrentDictionary()
      FileCheckOptions = ConcurrentDictionary()
      Projects = ConcurrentDictionary()
      HelpText = ConcurrentDictionary()
      Declarations = ConcurrentDictionary()
      CurrentAST = None
      CompletionNamespaceInsert = ConcurrentDictionary()
      CancellationTokens = ConcurrentDictionary()
      NavigationDeclarations = ConcurrentDictionary()
      ParseResults = ConcurrentDictionary()
      WorkspaceRoot = Environment.CurrentDirectory
      Analyzers = ConcurrentDictionary()
      ColorizationOutput = false }

  member x.GetCheckerOptions(file: SourceFilePath, lines: LineStr[]) : FSharpProjectOptions option =
    let file = Utils.normalizePath file

    x.FileCheckOptions.TryFind file
    |> Option.map (fun opts ->
        x.Files.[file] <- { Lines = lines; Touched = DateTime.Now; Version = None }
        x.FileCheckOptions.[file] <- opts
        opts
    )

  member x.TryGetFileVersion (file: SourceFilePath) : int option =
    let file = Utils.normalizePath file

    x.Files.TryFind file
    |> Option.bind (fun f -> f.Version)

  member x.TryGetLastCheckedVersion (file: SourceFilePath) : int option =
    let file = Utils.normalizePath file

    x.LastCheckedVersion.TryFind file

  member x.SetFileVersion (file: SourceFilePath) (version: int) =
    x.Files.TryFind file
    |> Option.iter (fun n ->
      let fileState = {n with Version = Some version}
      x.Files.[file] <- fileState
    )

  member x.SetLastCheckedVersion (file: SourceFilePath) (version: int) =
    x.LastCheckedVersion.[file] <- version

  member x.AddFileTextAndCheckerOptions(file: SourceFilePath, lines: LineStr[], opts, version) =
    let file = Utils.normalizePath file
    let fileState = { Lines = lines; Touched = DateTime.Now; Version = version }
    x.Files.[file] <- fileState
    x.FileCheckOptions.[file] <- opts

  member x.AddFileText(file: SourceFilePath, lines: LineStr[], version) =
    let file = Utils.normalizePath file
    let fileState = { Lines = lines; Touched = DateTime.Now; Version = version }
    x.Files.[file] <- fileState

  member x.AddCancellationToken(file : SourceFilePath, token: CancellationTokenSource) =
    x.CancellationTokens.AddOrUpdate(file, [token], fun _ lst -> token::lst)
    |> ignore

  member x.GetCancellationTokens(file : SourceFilePath) =
    let lst = x.CancellationTokens.GetOrAdd(file, fun _ -> [])
    x.CancellationTokens.TryRemove(file) |> ignore
    lst

  static member private FileWithoutProjectOptions(file) =
    let opts = [| yield sprintf "-r:%s" Environment.fsharpCore; yield "--noframework" |]

    { ProjectId = Some (file + ".fsproj")
      ProjectFileName = file + ".fsproj"
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

  member x.TryGetFileSource(file: SourceFilePath) : ResultOrString<string[]> =
    let file = Utils.normalizePath file
    match x.Files.TryFind(file) with
    | None -> ResultOrString.Error (sprintf "File '%s' not parsed" file)
    | Some f -> Ok (f.Lines)

  member x.TryGetFileCheckerOptionsWithLinesAndLineStr(file: SourceFilePath, pos : pos) : ResultOrString<FSharpProjectOptions * LineStr[] * LineStr> =
    let file = Utils.normalizePath file
    match x.TryGetFileCheckerOptionsWithLines(file) with
    | ResultOrString.Error x -> ResultOrString.Error x
    | Ok (opts, lines) ->
      let ok = pos.Line <= lines.Length && pos.Line >= 1 &&
               pos.Column <= lines.[pos.Line - 1].Length + 1 && pos.Column >= 1
      if not ok then ResultOrString.Error "Position is out of range"
      else Ok (opts, lines, lines.[pos.Line - 1])
