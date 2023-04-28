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
    type FSharpReferencedProject with

        member ProjectFilePath: string option

    type FSharpProjectOptions with

        member OutputDll: string
        member SourceFilesThatThisFileDependsOn: file: string<LocalPath> -> string array
        member SourceFilesThatDependOnFile: file: string<LocalPath> -> string array

    type ProjectController with

        /// returns all projects that depend on this one, transitively
        member GetDependentProjectsOfProjects: ps: FSharpProjectOptions list -> FSharpProjectOptions list
        /// crawls the project set and returns projects that contain a given file
        member ProjectsThatContainFile: file: string<LocalPath> -> FSharpProjectOptions list

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

    member DebugString: string

    static member Initial:
        toolsPath: Ionide.ProjInfo.Types.ToolsPath ->
        workspaceStateDir: IO.DirectoryInfo ->
        workspaceLoaderFactory: (Ionide.ProjInfo.Types.ToolsPath -> Ionide.ProjInfo.IWorkspaceLoader) ->
            State

    member RefreshCheckerOptions: file: string<LocalPath> * text: NamedText -> FSharpProjectOptions option
    member GetProjectOptions: file: string<LocalPath> -> FSharpProjectOptions option
    member GetProjectOptions': file: string<LocalPath> -> FSharpProjectOptions
    member RemoveProjectOptions: file: string<LocalPath> -> unit
    member FSharpProjectOptions: seq<string * FSharpProjectOptions>
    member TryGetFileVersion: file: string<LocalPath> -> int option
    member TryGetLastCheckedVersion: file: string<LocalPath> -> int option
    member SetFileVersion: file: string<LocalPath> -> version: int -> unit
    member SetLastCheckedVersion: file: string<LocalPath> -> version: int -> unit

    member AddFileTextAndCheckerOptions:
        file: string<LocalPath> * text: NamedText * opts: FSharpProjectOptions * version: int option -> unit

    member AddFileText: file: string<LocalPath> * text: NamedText * version: int option -> unit
    member AddCancellationToken: file: string<LocalPath> * token: CancellationTokenSource -> unit
    member GetCancellationTokens: file: string<LocalPath> -> CancellationTokenSource list

    member TryGetFileCheckerOptionsWithLines:
        file: string<LocalPath> -> ResultOrString<FSharpProjectOptions * NamedText>

    member TryGetFileCheckerOptionsWithSource:
        file: string<LocalPath> -> ResultOrString<FSharpProjectOptions * NamedText>

    member TryGetFileSource: file: string<LocalPath> -> ResultOrString<NamedText>

    member TryGetFileCheckerOptionsWithLinesAndLineStr:
        file: string<LocalPath> * pos: Position -> ResultOrString<FSharpProjectOptions * NamedText * LineStr>

    /// Removes `file` from all caches
    member Forget: file: string<LocalPath> -> unit
