namespace FsAutoComplete

open System.IO
open FSharp.Compiler.CodeAnalysis
open Utils
open FSharp.Compiler.Text
open FsAutoComplete.Logging
open Ionide.ProjInfo.ProjectSystem
open FSharp.UMX
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Symbols
open Microsoft.Extensions.Caching.Memory
open System
open FsToolkit.ErrorHandling
open FSharp.Compiler.CodeAnalysis.ProjectSnapshot
open System.Threading

type Version = int



[<AutoOpen>]
module Helpers3 =
  open FSharp.Compiler.CodeAnalysis.ProjectSnapshot

  type FSharpReferencedProjectSnapshot with

    member x.ProjectFilePath =
      match x with
      | FSharpReferencedProjectSnapshot.FSharpReference(snapshot = snapshot) -> snapshot.ProjectFileName |> Some
      | _ -> None


  type FSharpReferencedProject with

    member x.ProjectFilePath =
      match x with
      | FSharpReferencedProject.FSharpReference(options = options) -> options.ProjectFileName |> Some
      | _ -> None


[<RequireQualifiedAccess>]
type CompilerProjectOption =
  | BackgroundCompiler of FSharpProjectOptions
  | TransparentCompiler of FSharpProjectSnapshot

  member x.ProjectFileName =
    match x with
    | BackgroundCompiler(options) -> options.ProjectFileName
    | TransparentCompiler(snapshot) -> snapshot.ProjectFileName

  member x.ProjectId =
    match x with
    | BackgroundCompiler(options) -> options.ProjectId
    | TransparentCompiler(snapshot) -> snapshot.ProjectId

  member x.SourceFilesTagged =
    match x with
    | BackgroundCompiler(options) -> options.SourceFiles |> Array.toList
    | TransparentCompiler(snapshot) -> snapshot.SourceFiles |> List.map (fun f -> f.FileName)
    |> List.map Utils.normalizePath

  member x.ReferencedProjectsPath =
    match x with
    | BackgroundCompiler(options) ->
      options.ReferencedProjects
      |> Array.choose (fun p -> p.ProjectFilePath)
      |> Array.toList
    | TransparentCompiler(snapshot) -> snapshot.ReferencedProjects |> List.choose (fun p -> p.ProjectFilePath)

  member x.LoadTime =
    match x with
    | BackgroundCompiler(options) -> options.LoadTime
    | TransparentCompiler(snapshot) -> snapshot.LoadTime

  member x.OtherOptions =
    match x with
    | BackgroundCompiler(options) -> options.OtherOptions |> Array.toList
    | TransparentCompiler(snapshot) -> snapshot.OtherOptions

type FSharpCompilerServiceChecker(hasAnalyzers, typecheckCacheSize, parallelReferenceResolution, useTransparentCompiler)
  =
  let checker =
    FSharpChecker.Create(
      projectCacheSize = 200,
      keepAssemblyContents = hasAnalyzers,
      keepAllBackgroundResolutions = true,
      suggestNamesForErrors = true,
      keepAllBackgroundSymbolUses = true,
      enableBackgroundItemKeyStoreAndSemanticClassification = true,
      enablePartialTypeChecking = not hasAnalyzers,
      parallelReferenceResolution = parallelReferenceResolution,
      captureIdentifiersWhenParsing = true,
      useSyntaxTreeCache = true,
      useTransparentCompiler = useTransparentCompiler
    )

  let entityCache = EntityCache()

  // FCS can't seem to handle parallel project restores for script files
  // https://github.com/ionide/ionide-vscode-fsharp/issues/2005
  let scriptLocker = new SemaphoreSlim(1, 1)

  // This is used to hold previous check results for autocompletion.
  // We can't seem to rely on the checker for previous cached versions
  let memoryCache () =
    new MemoryCache(MemoryCacheOptions(SizeLimit = Nullable<_>(typecheckCacheSize)))

  let mutable lastCheckResults: IMemoryCache = memoryCache ()

  let checkerLogger = LogProvider.getLoggerByName "Checker"
  let optsLogger = LogProvider.getLoggerByName "Opts"

  /// the root path to the dotnet sdk installations, eg /usr/local/share/dotnet
  let mutable sdkRoot: DirectoryInfo option = None
  let mutable sdkFsharpCore: FileInfo option = None
  let mutable sdkFsiAuxLib: FileInfo option = None

  /// additional arguments that are added to typechecking of scripts
  let mutable fsiAdditionalArguments = Array.empty
  let mutable fsiAdditionalFiles: FSharpFileSnapshot list = List.empty

  /// This event is raised when any data that impacts script typechecking
  /// is changed. This can potentially invalidate existing project options
  /// so we must purge any typecheck results for scripts.
  let scriptTypecheckRequirementsChanged = Event<_>()

  let mutable disableInMemoryProjectReferences = false

  let fixupFsharpCoreAndFSIPathsForSnapshot (snapshot: FSharpProjectSnapshot) =
    match sdkFsharpCore, sdkFsiAuxLib with
    | None, _
    | _, None -> snapshot
    | Some fsc, Some fsi ->
      let _toReplace, otherOpts =
        snapshot.OtherOptions
        |> List.partition (fun opt ->
          opt.EndsWith("FSharp.Core.dll", StringComparison.Ordinal)
          || opt.EndsWith("FSharp.Compiler.Interactive.Settings.dll", StringComparison.Ordinal))

      FSharpProjectSnapshot.Create(
        snapshot.ProjectFileName,
        snapshot.ProjectId,
        snapshot.SourceFiles,
        snapshot.ReferencesOnDisk,
        List.append otherOpts [ $"-r:%s{fsc.FullName}"; $"-r:%s{fsi.FullName}" ],
        snapshot.ReferencedProjects,
        snapshot.IsIncompleteTypeCheckEnvironment,
        snapshot.UseScriptResolutionRules,
        snapshot.LoadTime,
        snapshot.UnresolvedReferences,
        snapshot.OriginalLoadReferences,
        snapshot.Stamp
      )

  let fixupFsharpCoreAndFSIPathsForOptions (p: FSharpProjectOptions) =
    match sdkFsharpCore, sdkFsiAuxLib with
    | None, _
    | _, None -> p
    | Some fsc, Some fsi ->
      let _toReplace, otherOpts =
        p.OtherOptions
        |> Array.partition (fun opt ->
          opt.EndsWith("FSharp.Core.dll", StringComparison.Ordinal)
          || opt.EndsWith("FSharp.Compiler.Interactive.Settings.dll", StringComparison.Ordinal))

      { p with
          OtherOptions = Array.append otherOpts [| $"-r:%s{fsc.FullName}"; $"-r:%s{fsi.FullName}" |] }


  let (|StartsWith|_|) (prefix: string) (s: string) =
    if s.StartsWith(prefix, StringComparison.Ordinal) then
      Some(s.[prefix.Length ..])
    else
      None

  let processFSIArgs args =
    (([||], [||]), args)
    ||> Array.fold (fun (args, files) arg ->
      match arg with
      | StartsWith "--use:" file
      | StartsWith "--load:" file -> args, Array.append files [| file |]
      | arg -> Array.append args [| arg |], files)

  let (|Reference|_|) (opt: string) =
    if opt.StartsWith("-r:", StringComparison.Ordinal) then
      Some(opt.[3..])
    else
      None


  /// ensures that all file paths are absolute before being sent to the compiler, because compilation of scripts fails with relative paths
  let resolveRelativeFilePaths (projectOptions: FSharpProjectOptions) =
    { projectOptions with
        SourceFiles = projectOptions.SourceFiles |> Array.map Path.GetFullPath
        OtherOptions =
          projectOptions.OtherOptions
          |> Array.map (fun opt ->
            match opt with
            | Reference r -> $"-r:{Path.GetFullPath r}"
            | opt -> opt) }


  /// ensures that any user-configured include/load files are added to the typechecking context
  let addLoadedFilesToSnapshot (snapshot: FSharpProjectSnapshot) =
    let files = List.append fsiAdditionalFiles snapshot.SourceFiles

    optsLogger.info (
      Log.setMessage "Source file list is {files}"
      >> Log.addContextDestructured "files" files
    )

    snapshot.Replace(files)

  let (|Reference|_|) (opt: string) =
    if opt.StartsWith("-r:", StringComparison.Ordinal) then
      Some(opt.[3..])
    else
      None


  /// ensures that any user-configured include/load files are added to the typechecking context
  let addLoadedFilesToProject (projectOptions: FSharpProjectOptions) =
    let additionalSourceFiles =
      (Array.ofList fsiAdditionalFiles) |> Array.map (fun s -> s.FileName)

    let files = Array.append additionalSourceFiles projectOptions.SourceFiles

    optsLogger.info (
      Log.setMessage "Source file list is {files}"
      >> Log.addContextDestructured "files" files
    )

    { projectOptions with
        SourceFiles = files }


  member __.DisableInMemoryProjectReferences
    with get () = disableInMemoryProjectReferences
    and set (value) = disableInMemoryProjectReferences <- value

  static member GetDependingProjects (file: string<LocalPath>) (snapshots: seq<string * CompilerProjectOption>) =
    let project =
      snapshots
      |> Seq.tryFind (fun (k, _) -> (UMX.untag k).ToUpperInvariant() = (UMX.untag file).ToUpperInvariant())

    project
    |> Option.map (fun (_, option) ->
      option,
      [ yield!
          snapshots
          |> Seq.map snd
          |> Seq.distinctBy (fun o -> o.ProjectFileName)
          |> Seq.filter (fun o ->
            o.ReferencedProjectsPath
            |> List.map (fun p -> Path.GetFullPath p)
            |> List.contains option.ProjectFileName) ])

  member private __.GetNetFxScriptSnapshot(file: string<LocalPath>, source) =
    async {
      optsLogger.info (
        Log.setMessage "Getting NetFX options for script file {file}"
        >> Log.addContextDestructured "file" file
      )

      let allFlags = Array.append [| "--targetprofile:mscorlib" |] fsiAdditionalArguments

      let! (opts, errors) =
        checker.GetProjectSnapshotFromScript(
          UMX.untag file,
          source,
          assumeDotNetFramework = true,
          useFsiAuxLib = true,
          otherFlags = allFlags,
          userOpName = "getNetFrameworkScriptOptions"
        )

      let allModifications = addLoadedFilesToSnapshot

      return allModifications opts, errors
    }

  member private __.GetNetCoreScriptSnapshot(file: string<LocalPath>, source) =
    async {
      optsLogger.info (
        Log.setMessage "Getting NetCore options for script file {file}"
        >> Log.addContextDestructured "file" file
      )

      let allFlags =
        Array.append [| "--targetprofile:netstandard" |] fsiAdditionalArguments

      let! (snapshot, errors) =
        checker.GetProjectSnapshotFromScript(
          UMX.untag file,
          source,
          assumeDotNetFramework = false,
          useSdkRefs = true,
          useFsiAuxLib = true,
          otherFlags = allFlags,
          userOpName = "getNetCoreScriptOptions"
        )

      optsLogger.trace (
        Log.setMessage "Got NetCore snapshot {snapshot} for file {file} with errors {errors}"
        >> Log.addContextDestructured "file" file
        >> Log.addContextDestructured "snapshot" snapshot
        >> Log.addContextDestructured "errors" errors
      )

      let allModifications =
        // filterBadRuntimeRefs >>
        addLoadedFilesToSnapshot >> fixupFsharpCoreAndFSIPathsForSnapshot

      let modified = allModifications snapshot

      optsLogger.trace (
        Log.setMessage "Replaced options to {opts}"
        >> Log.addContextDestructured "opts" modified
      )

      return modified, errors
    }

  member self.GetProjectSnapshotsFromScript(file: string<LocalPath>, source, tfm: FSIRefs.TFM) =
    async {
      try
        do! scriptLocker.WaitAsync() |> Async.AwaitTask

        match tfm with
        | FSIRefs.TFM.NetFx -> return! self.GetNetFxScriptSnapshot(file, source)
        | FSIRefs.TFM.NetCore -> return! self.GetNetCoreScriptSnapshot(file, source)
      finally
        scriptLocker.Release() |> ignore<int>
    }


  member private __.GetNetFxScriptOptions(file: string<LocalPath>, source) =
    async {
      optsLogger.info (
        Log.setMessage "Getting NetFX options for script file {file}"
        >> Log.addContextDestructured "file" file
      )

      let allFlags = Array.append [| "--targetprofile:mscorlib" |] fsiAdditionalArguments

      let! (opts, errors) =
        checker.GetProjectOptionsFromScript(
          UMX.untag file,
          source,
          assumeDotNetFramework = true,
          useFsiAuxLib = true,
          otherFlags = allFlags,
          userOpName = "getNetFrameworkScriptOptions"
        )

      let allModifications = addLoadedFilesToProject >> resolveRelativeFilePaths

      return allModifications opts, errors
    }

  member private __.GetNetCoreScriptOptions(file: string<LocalPath>, source) =
    async {
      optsLogger.info (
        Log.setMessage "Getting NetCore options for script file {file}"
        >> Log.addContextDestructured "file" file
      )

      let allFlags =
        Array.append [| "--targetprofile:netstandard"; "--checknulls+"; "--define:NULLABLE" |] fsiAdditionalArguments

      let! (opts, errors) =
        checker.GetProjectOptionsFromScript(
          UMX.untag file,
          source,
          assumeDotNetFramework = false,
          useSdkRefs = true,
          useFsiAuxLib = true,
          otherFlags = allFlags,
          userOpName = "getNetCoreScriptOptions"
        )

      optsLogger.trace (
        Log.setMessage "Got NetCore options {opts} for file {file} with errors {errors}"
        >> Log.addContextDestructured "file" file
        >> Log.addContextDestructured "opts" opts
        >> Log.addContextDestructured "errors" errors
      )

      let allModifications =
        // filterBadRuntimeRefs >>
        addLoadedFilesToProject
        >> resolveRelativeFilePaths
        >> fixupFsharpCoreAndFSIPathsForOptions

      let modified = allModifications opts

      optsLogger.trace (
        Log.setMessage "Replaced options to {opts}"
        >> Log.addContextDestructured "opts" modified
      )

      return modified, errors
    }

  member self.GetProjectOptionsFromScript(file: string<LocalPath>, source, tfm) =
    async {
      try
        do! scriptLocker.WaitAsync() |> Async.AwaitTask

        match tfm with
        | FSIRefs.TFM.NetFx -> return! self.GetNetFxScriptOptions(file, source)
        | FSIRefs.TFM.NetCore -> return! self.GetNetCoreScriptOptions(file, source)
      finally
        scriptLocker.Release() |> ignore<int>
    }


  member __.ScriptTypecheckRequirementsChanged =
    scriptTypecheckRequirementsChanged.Publish

  member _.RemoveFileFromCache(file: string<LocalPath>) = lastCheckResults.Remove(file)

  member _.ClearCache(snap: FSharpProjectSnapshot seq) = snap |> Seq.map (fun x -> x.Identifier) |> checker.ClearCache

  /// This function is called when the entire environment is known to have changed for reasons not encoded in the ProjectOptions of any project/compilation.
  member _.ClearCaches() =
    lastCheckResults.Dispose()
    lastCheckResults <- memoryCache ()
    checker.InvalidateAll()
    checker.ClearLanguageServiceRootCachesAndCollectAndFinalizeAllTransients()

  /// <summary>Parses a source code for a file and caches the results. Returns an AST that can be traversed for various features.</summary>
  /// <param name="filePath"> The path for the file. The file name is used as a module name for implicit top level modules (e.g. in scripts).</param>
  /// <param name="snapshot">Parsing options for the project or script.</param>
  /// <returns></returns>
  member x.ParseFile(filePath: string<LocalPath>, snapshot: FSharpProjectSnapshot) =
    async {
      checkerLogger.info (
        Log.setMessage "ParseFile - {file}"
        >> Log.addContextDestructured "file" filePath
      )

      let path = UMX.untag filePath
      return! checker.ParseFile(path, snapshot)
    }


  member x.ParseFile(filePath: string<LocalPath>, sourceText: ISourceText, project: FSharpProjectOptions) =
    async {
      checkerLogger.info (
        Log.setMessage "ParseFile - {file}"
        >> Log.addContextDestructured "file" filePath
      )

      let parseOpts = Utils.projectOptionsToParseOptions project

      let path = UMX.untag filePath
      return! checker.ParseFile(path, sourceText, parseOpts)
    }

  /// <summary>Parse and check a source code file, returning a handle to the results</summary>
  /// <param name="filePath">The name of the file in the project whose source is being checked.</param>
  /// <param name="snapshot">The snapshot for the project or script.</param>
  /// <param name="shouldCache">Determines if the typecheck should be cached for autocompletions.</param>
  /// <remarks>Note: all files except the one being checked are read from the FileSystem API</remarks>
  /// <returns>Result of ParseAndCheckResults</returns>
  member _.ParseAndCheckFileInProject
    (filePath: string<LocalPath>, snapshot: FSharpProjectSnapshot, ?shouldCache: bool)
    =
    asyncResult {
      let shouldCache = defaultArg shouldCache false
      let opName = sprintf "ParseAndCheckFileInProject - %A" filePath

      checkerLogger.info (Log.setMessage "{opName}" >> Log.addContextDestructured "opName" opName)

      let path = UMX.untag filePath

      try
        let! (p, c) = checker.ParseAndCheckFileInProject(path, snapshot, userOpName = opName)

        let parseErrors = p.Diagnostics |> Array.map (fun p -> p.Message)

        match c with
        | FSharpCheckFileAnswer.Aborted ->
          checkerLogger.info (
            Log.setMessage "{opName} completed with errors: {errors}"
            >> Log.addContextDestructured "opName" opName
            >> Log.addContextDestructured "errors" (List.ofArray p.Diagnostics)
          )

          return! ResultOrString.Error(sprintf "Check aborted (%A). Errors: %A" c parseErrors)
        | FSharpCheckFileAnswer.Succeeded(c) ->
          checkerLogger.info (
            Log.setMessage "{opName} completed successfully"
            >> Log.addContextDestructured "opName" opName
          )

          let r = ParseAndCheckResults(p, c, entityCache)

          if shouldCache then
            let ops =
              MemoryCacheEntryOptions().SetSize(1).SetSlidingExpiration(TimeSpan.FromMinutes(5.))

            return lastCheckResults.Set(filePath, r, ops)
          else
            return r
      with ex ->
        checkerLogger.error (
          Log.setMessage "{opName} completed with exception: {ex}"
          >> Log.addContextDestructured "opName" opName
          >> Log.addExn ex
        )

        return! ResultOrString.Error(ex.ToString())
    }

  member __.ParseAndCheckFileInProject
    (filePath: string<LocalPath>, version, source: ISourceText, options, ?shouldCache: bool)
    =
    asyncResult {
      let shouldCache = defaultArg shouldCache false
      let opName = sprintf "ParseAndCheckFileInProject - %A" filePath

      checkerLogger.info (Log.setMessage "{opName}" >> Log.addContextDestructured "opName" opName)

      // let options = clearProjectReferences options
      let path = UMX.untag filePath

      try
        let! (p, c) = checker.ParseAndCheckFileInProject(path, version, source, options, userOpName = opName)

        let parseErrors = p.Diagnostics |> Array.map (fun p -> p.Message)

        match c with
        | FSharpCheckFileAnswer.Aborted ->
          checkerLogger.info (
            Log.setMessage "{opName} completed with errors: {errors}"
            >> Log.addContextDestructured "opName" opName
            >> Log.addContextDestructured "errors" (List.ofArray p.Diagnostics)
          )

          return! ResultOrString.Error(sprintf "Check aborted (%A). Errors: %A" c parseErrors)
        | FSharpCheckFileAnswer.Succeeded(c) ->
          checkerLogger.info (
            Log.setMessage "{opName} completed successfully"
            >> Log.addContextDestructured "opName" opName
          )

          let r = ParseAndCheckResults(p, c, entityCache)

          if shouldCache then
            let ops =
              MemoryCacheEntryOptions().SetSize(1).SetSlidingExpiration(TimeSpan.FromMinutes(5.))

            return lastCheckResults.Set(filePath, r, ops)
          else
            return r
      with ex ->
        checkerLogger.error (
          Log.setMessage "{opName} completed with exception: {ex}"
          >> Log.addContextDestructured "opName" opName
          >> Log.addExn ex
        )

        return! ResultOrString.Error(ex.ToString())
    }

  /// <summary>
  /// This is use primary for Autocompletions. The problem with trying to use TryGetRecentCheckResultsForFile is that it will return None
  /// if there isn't a GetHashCode that matches the SourceText passed in.  This a problem particularly for Autocompletions because we'd have to wait for a typecheck
  /// on every keystroke which can prove slow.  For autocompletions, it's ok to rely on cached type-checks as files above generally don't change mid type.
  /// </summary>
  /// <param name="file">The path of the file to get cached type check results for.</param>
  /// <returns>Cached typecheck results</returns>
  member _.TryGetLastCheckResultForFile(file: string<LocalPath>) =
    let opName = sprintf "TryGetLastCheckResultForFile - %A" file

    checkerLogger.info (Log.setMessage "{opName}" >> Log.addContextDestructured "opName" opName)

    match lastCheckResults.TryGetValue<ParseAndCheckResults>(file) with
    | (true, v) -> Some v
    | _ -> None

  member _.TryGetRecentCheckResultsForFile(file: string<LocalPath>, snapshot: FSharpProjectSnapshot) =
    let opName = sprintf "TryGetRecentCheckResultsForFile - %A" file

    checkerLogger.info (Log.setMessage "{opName} - {hash}" >> Log.addContextDestructured "opName" opName)

    checker.TryGetRecentCheckResultsForFile(UMX.untag file, snapshot, opName)
    |> Option.map (fun (pr, cr) ->
      checkerLogger.info (
        Log.setMessage "{opName} - got results - {version}"
        >> Log.addContextDestructured "opName" opName
      )

      ParseAndCheckResults(pr, cr, entityCache))

  member __.TryGetRecentCheckResultsForFile(file: string<LocalPath>, options, source: ISourceText) =
    let opName = sprintf "TryGetRecentCheckResultsForFile - %A" file

    checkerLogger.info (
      Log.setMessage "{opName} - {hash}"
      >> Log.addContextDestructured "opName" opName
      >> Log.addContextDestructured "hash" (source.GetHashCode() |> int)

    )

    // let options = clearProjectReferences options

    let result =
      checker.TryGetRecentCheckResultsForFile(UMX.untag file, options, sourceText = source, userOpName = opName)
      |> Option.map (fun (pr, cr, version) ->
        checkerLogger.info (
          Log.setMessage "{opName} - got results - {version}"
          >> Log.addContextDestructured "opName" opName
          >> Log.addContextDestructured "version" version
        )

        ParseAndCheckResults(pr, cr, entityCache))

    checkerLogger.info (
      Log.setMessage "{opName} - {hash} - cacheHit {cacheHit}"
      >> Log.addContextDestructured "opName" opName
      >> Log.addContextDestructured "hash" (source.GetHashCode() |> int)
      >> Log.addContextDestructured "cacheHit" result.IsSome
    )

    result

  member _.ParseAndCheckProject(opts: CompilerProjectOption) =
    match opts with
    | CompilerProjectOption.BackgroundCompiler opts -> checker.ParseAndCheckProject(opts)
    | CompilerProjectOption.TransparentCompiler snapshot -> checker.ParseAndCheckProject(snapshot)

  member x.GetUsesOfSymbol
    (file: string<LocalPath>, snapshots: (string * CompilerProjectOption) seq, symbol: FSharpSymbol)
    =
    async {
      checkerLogger.info (
        Log.setMessage "GetUsesOfSymbol - {file}"
        >> Log.addContextDestructured "file" file
      )

      match FSharpCompilerServiceChecker.GetDependingProjects file snapshots with
      | None -> return [||]
      | Some(opts, []) ->

        let! res = x.ParseAndCheckProject(opts)
        return res.GetUsesOfSymbol symbol
      | Some(opts, dependentProjects) ->
        let! res =
          opts :: dependentProjects
          |> List.map (fun (opts) ->
            async {
              let! res = x.ParseAndCheckProject(opts)
              return res.GetUsesOfSymbol symbol
            })
          |> Async.parallel75

        return res |> Array.concat
    }

  member x.FindReferencesForSymbolInFile(file: string<LocalPath>, project: FSharpProjectSnapshot, symbol) =
    async {
      checkerLogger.info (
        Log.setMessage "FindReferencesForSymbolInFile - {file} - {projectFile}"
        >> Log.addContextDestructured "file" file
        >> Log.addContextDestructured "projectFile" project.ProjectFileName
      )

      let file = UMX.untag file

      try
        let! results = checker.FindBackgroundReferencesInFile(file, project, symbol, userOpName = "find references")

        checkerLogger.info (
          Log.setMessage "FindReferencesForSymbolInFile - {file} - {projectFile}  - {results}"
          >> Log.addContextDestructured "file" file
          >> Log.addContextDestructured "projectFile" project.ProjectFileName
          >> Log.addContextDestructured "results" results
        )

        return results
      with e ->
        checkerLogger.error (
          Log.setMessage "FindReferencesForSymbolInFile - {file} - {projectFile}"
          >> Log.addContextDestructured "projectFile" project.ProjectFileName
          >> Log.addContextDestructured "file" file
          >> Log.addExn e
        )

        return [||]
    }

  member _.FindReferencesForSymbolInFile(file: string<LocalPath>, project, symbol) =
    async {
      checkerLogger.info (
        Log.setMessage "FindReferencesForSymbolInFile - {file}"
        >> Log.addContextDestructured "file" file
      )

      return!
        checker.FindBackgroundReferencesInFile(
          UMX.untag file,
          project,
          symbol,
          canInvalidateProject = false,
          // fastCheck = true,
          userOpName = "find references"
        )
    }


  member __.SetDotnetRoot(dotnetBinary: FileInfo, cwd: DirectoryInfo) =
    match Ionide.ProjInfo.SdkDiscovery.versionAt cwd dotnetBinary with
    | Ok sdkVersion ->

      let sdks = Ionide.ProjInfo.SdkDiscovery.sdks dotnetBinary

      match sdks |> Array.tryFind (fun sdk -> sdk.Version = sdkVersion) with
      | Some sdk ->
        sdkRoot <- Some sdk.Path
        let fsharpDir = Path.Combine(sdk.Path.FullName, "FSharp")
        let dll = Path.Combine(fsharpDir, "FSharp.Core.dll")
        let fi = FileInfo(dll)

        if fi.Exists then
          sdkFsharpCore <- Some fi

        let dll = Path.Combine(fsharpDir, "FSharp.Compiler.Interactive.Settings.dll")
        let fi = FileInfo(dll)

        if fi.Exists then
          sdkFsiAuxLib <- Some fi

      | None -> ()

      scriptTypecheckRequirementsChanged.Trigger()
    | Error _ -> ()


  member __.GetDotnetRoot() = sdkRoot

  member __.SetFSIAdditionalArguments args =
    //TODO: UX - if preview-required features are set, then auto-add langversion:preview for the user.
    if fsiAdditionalArguments = args then
      ()
    else
      let additionalArgs, files = processFSIArgs args
      fsiAdditionalArguments <- additionalArgs

      fsiAdditionalFiles <-
        files
        |> Array.map (fun f -> FSharpFileSnapshot.CreateFromFileSystem(System.IO.Path.GetFullPath f))
        |> Array.toList

      scriptTypecheckRequirementsChanged.Trigger()
