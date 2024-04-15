namespace FsAutoComplete

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

type FSharpCompilerServiceChecker =
  new:
    hasAnalyzers: bool * typecheckCacheSize: int64 * parallelReferenceResolution: bool -> FSharpCompilerServiceChecker

  member DisableInMemoryProjectReferences: bool with get, set

  static member GetDependingProjects:
    file: string<LocalPath> ->
    snapshots: seq<string * FSharpProjectSnapshot> ->
      option<FSharpProjectSnapshot * list<FSharpProjectSnapshot>>

  member GetProjectOptionsFromScript:
    file: string<LocalPath> * source: ISourceTextNew * tfm: FSIRefs.TFM ->
      Async<FSharpProjectSnapshot * FSharpDiagnostic list>

  member ScriptTypecheckRequirementsChanged: IEvent<unit>

  member RemoveFileFromCache: file: string<LocalPath> -> unit

  member ClearCache: snap: seq<FSharpProjectSnapshot> -> unit

  /// This function is called when the entire environment is known to have changed for reasons not encoded in the ProjectOptions of any project/compilation.
  member ClearCaches: unit -> unit


  /// <summary>Parses a source code for a file and caches the results. Returns an AST that can be traversed for various features.</summary>
  /// <param name="filePath"> The path for the file. The file name is used as a module name for implicit top level modules (e.g. in scripts).</param>
  /// <param name="snapshot">Parsing options for the project or script.</param>
  /// <returns></returns>
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

  member GetUsesOfSymbol:
    file: string<LocalPath> * snapshots: (string * FSharpProjectSnapshot) seq * symbol: FSharpSymbol ->
      Async<FSharpSymbolUse array>

  member FindReferencesForSymbolInFile:
    file: string<LocalPath> * project: FSharpProjectSnapshot * symbol: FSharpSymbol -> Async<seq<range>>

  // member GetDeclarations:
  //   fileName: string<LocalPath> * source: ISourceText * snapshot: FSharpProjectOptions * version: 'a ->
  //     Async<NavigationTopLevelDeclaration array>

  member SetDotnetRoot: dotnetBinary: FileInfo * cwd: DirectoryInfo -> unit
  member GetDotnetRoot: unit -> DirectoryInfo option
  member SetFSIAdditionalArguments: args: string array -> unit
