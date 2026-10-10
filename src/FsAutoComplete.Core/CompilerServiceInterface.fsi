namespace FsAutoComplete

open System
open System.IO
open System.Collections.Generic
open FSharp.Compiler.CodeAnalysis
open Utils
open FSharp.Compiler.Text
open FsAutoComplete.Logging
open Ionide.ProjInfo.ProjectSystem
open FSharp.UMX
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Symbols
open FSharp.Compiler.Diagnostics

type Version = int

[<RequireQualifiedAccess>]
type CompilerProjectOption =
  | BackgroundCompiler of FSharpProjectOptions
  | TransparentCompiler of FSharpProjectSnapshot

  member ProjectFileName: string
  member ProjectId: string option
  member SourceFilesTagged: string<LocalPath> list
  member ReferencedProjectsPath: string list
  member LoadTime: DateTime
  member OtherOptions: string list

type FSharpCompilerServiceChecker =
  new:
    hasAnalyzers: bool *
    typecheckCacheSize: int64 *
    parallelReferenceResolution: bool *
    useTransparentCompiler: bool *
    ?transparentCompilerCacheSizes: int ->
      FSharpCompilerServiceChecker

  static member GetDependingProjects:
    file: string<LocalPath> ->
    snapshots: seq<string * CompilerProjectOption> ->
      option<CompilerProjectOption * list<CompilerProjectOption>>

  member GetProjectSnapshotsFromScript:
    file: string<LocalPath> * source: ISourceTextNew * tfm: FSIRefs.TFM ->
      Async<FSharpProjectSnapshot * FSharpDiagnostic list>

  member GetProjectOptionsFromScript:
    file: string<LocalPath> * source: ISourceText * tfm: FSIRefs.TFM ->
      Async<FSharpProjectOptions * list<FSharp.Compiler.Diagnostics.FSharpDiagnostic>>

  member ScriptTypecheckRequirementsChanged: IEvent<unit>

  member RemoveFileFromCache: file: string<LocalPath> -> unit

  member ClearCache: snap: seq<FSharpProjectSnapshot> -> unit

  /// This function is called when the entire environment is known to have changed for reasons not encoded in the ProjectOptions of any project/compilation.
  member ClearCaches: unit -> unit


  /// <summary>Gets the parsing options of a project, with its defines and language version.</summary>
  /// <param name="options">The project or script, for its source files and compiler options.</param>
  member GetParsingOptions: options: CompilerProjectOption -> FSharpParsingOptions

  /// <summary>Parses a source code file with the parsing options of its project. Returns an AST that can be traversed for various features.</summary>
  /// <remarks>Unlike a parse with a project snapshot, this does not import the references or check the referenced projects.</remarks>
  /// <param name="filePath">The path for the file. The file name is used as a module name for implicit top level modules (e.g. in scripts).</param>
  /// <param name="sourceText">The source of the file.</param>
  /// <param name="options">The project or script of the file.</param>
  /// <param name="cache">Whether the checker keeps the results in its cache. True by default.</param>
  member ParseFile:
    filePath: string<LocalPath> * sourceText: ISourceText * options: CompilerProjectOption * ?cache: bool ->
      Async<FSharpParseFileResults>

  /// <summary>Parses a source code file with the snapshot of its project. The type check of the file uses the same parse.</summary>
  /// <remarks>This imports the references and checks the referenced projects, if that was not done before.</remarks>
  /// <param name="filePath">The path for the file. The file name is used as a module name for implicit top level modules (e.g. in scripts).</param>
  /// <param name="snapshot">The snapshot of the project or script, with the source of the file.</param>
  member ParseFile: filePath: string<LocalPath> * snapshot: FSharpProjectSnapshot -> Async<FSharpParseFileResults>

  /// <summary>Parse and check a source code file, returning a handle to the results</summary>
  /// <param name="filePath">The name of the file in the project whose source is being checked.</param>
  /// <param name="snapshot">The options for the project or script.</param>
  /// <param name="shouldCache">Determines if the typecheck should be cached for autocompletions.</param>
  /// <remarks>Note: all files except the one being checked are read from the FileSystem API</remarks>
  /// <returns>Result of ParseAndCheckResults</returns>
  member ParseAndCheckFileInProject:
    filePath: string<LocalPath> * snapshot: FSharpProjectSnapshot * ?shouldCache: bool ->
      Async<Result<ParseAndCheckResults, string>>

  member ParseAndCheckFileInProject:
    filePath: string<LocalPath> *
    version: int *
    source: ISourceText *
    options: FSharpProjectOptions *
    ?shouldCache: bool ->
      Async<Result<ParseAndCheckResults, string>>

  /// <summary>
  /// This is use primary for Autocompletions. The problem with trying to use TryGetRecentCheckResultsForFile is that it will return None
  /// if there isn't a GetHashCode that matches the SourceText passed in.  This a problem particularly for Autocompletions because we'd have to wait for a typecheck
  /// on every keystroke which can prove slow.  For autocompletions, it's ok to rely on cached type-checks as files above generally don't change mid type.
  /// </summary>
  /// <param name="file">The path of the file to get cached type check results for.</param>
  /// <returns>Cached typecheck results</returns>
  member TryGetLastCheckResultForFile: file: string<LocalPath> -> ParseAndCheckResults option

  member TryGetRecentCheckResultsForFile:
    file: string<LocalPath> * snapshot: FSharpProjectSnapshot -> ParseAndCheckResults option

  member TryGetRecentCheckResultsForFile:
    file: string<LocalPath> * options: FSharpProjectOptions * source: ISourceText -> option<ParseAndCheckResults>

  member GetUsesOfSymbol:
    file: string<LocalPath> * snapshots: (string * CompilerProjectOption) seq * symbol: FSharpSymbol ->
      Async<FSharpSymbolUse array>

  member FindReferencesForSymbolInFile:
    file: string<LocalPath> * project: FSharpProjectSnapshot * symbol: FSharpSymbol -> Async<seq<range>>

  member FindReferencesForSymbolInFile:
    file: string<LocalPath> * project: FSharpProjectOptions * symbol: FSharpSymbol -> Async<seq<range>>

  // member GetDeclarations:
  //   fileName: string<LocalPath> * source: ISourceText * snapshot: FSharpProjectOptions * version: 'a ->
  //     Async<NavigationTopLevelDeclaration array>

  member SetDotnetRoot: dotnetBinary: FileInfo * cwd: DirectoryInfo -> unit
  member GetDotnetRoot: unit -> DirectoryInfo option
  member SetFSIAdditionalArguments: args: string array -> unit
