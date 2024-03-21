namespace FsAutoComplete

open System.IO
open System.Collections.Generic
open FSharp.Compiler.CodeAnalysis
open Utils
open FSharp.Compiler.Text
open Ionide.ProjInfo.ProjectSystem
open FSharp.UMX
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Symbols
open FSharp.Compiler.Diagnostics

type Version = int

type FSharpCompilerServiceChecker =
  new:
    hasAnalyzers: bool * typecheckCacheSize: int64 * parallelReferenceResolution: bool ->
      FSharpCompilerServiceChecker

  member DisableInMemoryProjectReferences: bool with get, set

  static member GetDependingProjects:
    file   : string<LocalPath> ->
    options: seq<string * FSharpProjectSnapshot>
         -> option<FSharpProjectSnapshot * list<FSharpProjectSnapshot>>

  member GetProjectOptionsFromScript:
    file: string<LocalPath> * source: ISourceText * tfm: FSIRefs.TFM ->
      Async<FSharpProjectOptions * FSharpDiagnostic list>

  member GetProjectSnapshotFromScript:
    file: string<LocalPath> * source: ISourceTextNew * tfm: FSIRefs.TFM -> Async<FSharpProjectSnapshot>

  member ScriptTypecheckRequirementsChanged: IEvent<unit>

  member RemoveFileFromCache: file: string<LocalPath> -> unit

  member ClearCache:
   snap: seq<FSharpProjectSnapshot>
      -> unit

  /// This function is called when the entire environment is known to have changed for reasons not encoded in the ProjectOptions of any project/compilation.
  member ClearCaches: unit -> unit

  member FromOptions:
    options: FSharpProjectOptions array * documentSource: DocumentSource ->
      Async<(FSharpProjectOptions * FSharpProjectSnapshot) array>

  member FromOption:
    options: FSharpProjectOptions *
    documentSource: DocumentSource *
    ?snapshotAccumulator: Dictionary<FSharpProjectOptions, FSharpProjectSnapshot> ->
      Async<FSharpProjectSnapshot>

  /// <summary>Parses a source code for a file and caches the results. Returns an AST that can be traversed for various features.</summary>
  /// <param name="filePath"> The path for the file. The file name is used as a module name for implicit top level modules (e.g. in scripts).</param>
  /// <param name="source">The source to be parsed.</param>
  /// <param name="options">Parsing options for the project or script.</param>
  /// <returns></returns>
  member ParseFile:
    filePath: string<LocalPath> * source: ISourceText * options: FSharpProjectSnapshot -> Async<FSharpParseFileResults>

  /// <summary>Parse and check a source code file, returning a handle to the results</summary>
  /// <param name="filePath">The name of the file in the project whose source is being checked.</param>
  /// <param name="version">An integer that can be used to indicate the version of the file. This will be returned by TryGetRecentCheckResultsForFile when looking up the file</param>
  /// <param name="source">The source for the file.</param>
  /// <param name="options">The options for the project or script.</param>
  /// <param name="shouldCache">Determines if the typecheck should be cached for autocompletions.</param>
  /// <param name="snapshotAccumulator">A dictionary of FSharpProjectOptions to FSharpProjectSnapshot that will be used to accumulate snapshots for the project. This is used to avoid re-reading the project file from disk for every file in the project.</param>
  /// <remarks>Note: all files except the one being checked are read from the FileSystem API</remarks>
  /// <returns>Result of ParseAndCheckResults</returns>
  member ParseAndCheckFileInProject:
    filePath: string<LocalPath> *
    version: int *
    source: ISourceText *
    options: FSharpProjectSnapshot *
    ?shouldCache: bool *
    ?snapshotAccumulator: Dictionary<FSharpProjectOptions, FSharpProjectSnapshot> ->
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
    file: string<LocalPath> * options: FSharpProjectSnapshot * source: ISourceText -> Async<ParseAndCheckResults option>

  member GetUsesOfSymbol:
    file: string<LocalPath> * options: (string * FSharpProjectSnapshot) seq * symbol: FSharpSymbol ->
      Async<FSharpSymbolUse array>

  member FindReferencesForSymbolInFile:
    file: string * project: FSharpProjectSnapshot * symbol: FSharpSymbol -> Async<seq<range>>

  // member GetDeclarations:
  //   fileName: string<LocalPath> * source: ISourceText * options: FSharpProjectOptions * version: 'a ->
  //     Async<NavigationTopLevelDeclaration array>

  member SetDotnetRoot: dotnetBinary: FileInfo * cwd: DirectoryInfo -> unit
  member GetDotnetRoot: unit -> DirectoryInfo option
  member SetFSIAdditionalArguments: args: string array -> unit
