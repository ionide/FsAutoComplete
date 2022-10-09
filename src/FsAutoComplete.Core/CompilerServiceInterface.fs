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

type Version = int

type FSharpCompilerServiceChecker(hasAnalyzers) =
  let checker =
    FSharpChecker.Create(
      projectCacheSize = 200,
      keepAllBackgroundResolutions = true,
      keepAssemblyContents = hasAnalyzers,
      suggestNamesForErrors = true,
      enablePartialTypeChecking = not hasAnalyzers,
      enableBackgroundItemKeyStoreAndSemanticClassification = true,
      keepAllBackgroundSymbolUses = true
    )

  let entityCache = EntityCache()

  let checkerLogger = LogProvider.getLoggerByName "Checker"
  let optsLogger = LogProvider.getLoggerByName "Opts"

  /// the root path to the dotnet sdk installations, eg /usr/local/share/dotnet
  let mutable sdkRoot: DirectoryInfo option = None
  let mutable sdkFsharpCore: FileInfo option = None
  let mutable sdkFsiAuxLib: FileInfo option = None

  /// additional arguments that are added to typechecking of scripts
  let mutable fsiAdditionalArguments = Array.empty
  let mutable fsiAdditionalFiles = Array.empty

  /// This event is raised when any data that impacts script typechecking
  /// is changed. This can potentially invalidate existing project options
  /// so we must purge any typecheck results for scripts.
  let scriptTypecheckRequirementsChanged = Event<_>()

  let mutable disableInMemoryProjectReferences = false

  let fixupFsharpCoreAndFSIPaths (p: FSharpProjectOptions) =
    match sdkFsharpCore, sdkFsiAuxLib with
    | None, _
    | _, None -> p
    | Some fsc, Some fsi ->
      let toReplace, otherOpts =
        p.OtherOptions
        |> Array.partition (fun opt ->
          opt.EndsWith "FSharp.Core.dll"
          || opt.EndsWith "FSharp.Compiler.Interactive.Settings.dll")

      { p with OtherOptions = Array.append otherOpts [| $"-r:%s{fsc.FullName}"; $"-r:%s{fsi.FullName}" |] }

  let (|StartsWith|_|) (prefix: string) (s: string) =
    if s.StartsWith(prefix) then
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

  let clearProjectReferences (opts: FSharpProjectOptions) =
    if disableInMemoryProjectReferences then
      { opts with ReferencedProjects = [||] }
    else
      opts

  let filterBadRuntimeRefs =
    let badRefs =
      [ "System.Private"
        "System.Runtime.WindowsRuntime"
        "System.Runtime.WindowsRuntime.UI.Xaml"
        "mscorlib" ]
      |> List.map (fun p -> p + ".dll")

    let containsBadRef (s: string) =
      badRefs |> List.exists (fun r -> s.EndsWith r)

    fun (projOptions: FSharpProjectOptions) ->
      { projOptions with OtherOptions = projOptions.OtherOptions |> Array.where (containsBadRef >> not) }

  /// ensures that any user-configured include/load files are added to the typechecking context
  let addLoadedFiles (projectOptions: FSharpProjectOptions) =
    let files = Array.append fsiAdditionalFiles projectOptions.SourceFiles

    optsLogger.info (
      Log.setMessage "Source file list is {files}"
      >> Log.addContextDestructured "files" files
    )

    { projectOptions with SourceFiles = files }

  let (|Reference|_|) (opt: string) =
    if opt.StartsWith "-r:" then Some(opt.[3..]) else None

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

  member __.DisableInMemoryProjectReferences
    with get () = disableInMemoryProjectReferences
    and set (value) = disableInMemoryProjectReferences <- value

  static member GetDependingProjects (file: string<LocalPath>) (options: seq<string * FSharpProjectOptions>) =
    let project =
      options
      |> Seq.tryFind (fun (k, _) -> (UMX.untag k).ToUpperInvariant() = (UMX.untag file).ToUpperInvariant())

    project
    |> Option.map (fun (_, option) ->
      option,
      [ yield!
          options
          |> Seq.map snd
          |> Seq.distinctBy (fun o -> o.ProjectFileName)
          |> Seq.filter (fun o ->
            o.ReferencedProjects
            |> Array.map (fun p -> Path.GetFullPath p.OutputFile)
            |> Array.contains option.ProjectFileName) ])

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

      let allModifications = addLoadedFiles >> resolveRelativeFilePaths

      return allModifications opts, errors
    }

  member private __.GetNetCoreScriptOptions(file: string<LocalPath>, source) =
    async {
      optsLogger.info (
        Log.setMessage "Getting NetCore options for script file {file}"
        >> Log.addContextDestructured "file" file
      )

      let allFlags =
        Array.append [| "--targetprofile:netstandard" |] fsiAdditionalArguments

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
        addLoadedFiles >> resolveRelativeFilePaths >> fixupFsharpCoreAndFSIPaths

      let modified = allModifications opts

      optsLogger.trace (
        Log.setMessage "Replaced options to {opts}"
        >> Log.addContextDestructured "opts" modified
      )

      return modified, errors
    }

  member self.GetProjectOptionsFromScript(file: string<LocalPath>, source, tfm) =
    async {
      let! (projOptions, errors) =
        match tfm with
        | FSIRefs.TFM.NetFx -> self.GetNetFxScriptOptions(file, source)
        | FSIRefs.TFM.NetCore -> self.GetNetCoreScriptOptions(file, source)

      match errors with
      | [] -> ()
      | errs ->
        optsLogger.info (
          Log.setLogLevel LogLevel.Error
          >> Log.setMessage "Resolved {opts} with {errors}"
          >> Log.addContextDestructured "opts" projOptions
          >> Log.addContextDestructured "errors" errs
        )

      return projOptions
    }

  member __.ScriptTypecheckRequirementsChanged =
    scriptTypecheckRequirementsChanged.Publish

  member __.ParseFile(fn: string<LocalPath>, source, fpo) =
    checkerLogger.info (Log.setMessage "ParseFile - {file}" >> Log.addContextDestructured "file" fn)

    let path = UMX.untag fn
    checker.ParseFile(path, source, fpo)

  member __.ParseAndCheckFileInProject(filePath: string<LocalPath>, version, source: ISourceText, options) =
    async {
      let opName = sprintf "ParseAndCheckFileInProject - %A" filePath

      checkerLogger.info (Log.setMessage "{opName}" >> Log.addContextDestructured "opName" opName)

      let options = clearProjectReferences options
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

          return ResultOrString.Error(sprintf "Check aborted (%A). Errors: %A" c parseErrors)
        | FSharpCheckFileAnswer.Succeeded (c) ->
          checkerLogger.info (
            Log.setMessage "{opName} completed successfully"
            >> Log.addContextDestructured "opName" opName
          )

          return Ok(ParseAndCheckResults(p, c, entityCache))
      with ex ->
        return ResultOrString.Error(ex.ToString())
    }

  member __.TryGetRecentCheckResultsForFile(file: string<LocalPath>, options, source: ISourceText) =
    let opName = sprintf "TryGetRecentCheckResultsForFile - %A" file

    checkerLogger.info (
      Log.setMessage "{opName} - {hash}"
      >> Log.addContextDestructured "opName" opName
      >> Log.addContextDestructured "hash" (source.GetHashCode() |> int)
    )

    let options = clearProjectReferences options

    let result =
      checker.TryGetRecentCheckResultsForFile(UMX.untag file, options, sourceText = source, userOpName = opName)
      |> Option.map (fun (pr, cr, _) -> ParseAndCheckResults(pr, cr, entityCache))

    checkerLogger.info (
      Log.setMessage "{opName} - {hash} - cacheHit {cacheHit}"
      >> Log.addContextDestructured "opName" opName
      >> Log.addContextDestructured "hash" (source.GetHashCode() |> int)
      >> Log.addContextDestructured "cacheHit" result.IsSome
    )

    result

  member x.GetUsesOfSymbol
    (
      file: string<LocalPath>,
      options: (string * FSharpProjectOptions) seq,
      symbol: FSharpSymbol
    ) =
    async {
      checkerLogger.info (
        Log.setMessage "GetUsesOfSymbol - {file}"
        >> Log.addContextDestructured "file" file
      )

      match FSharpCompilerServiceChecker.GetDependingProjects file options with
      | None -> return [||]
      | Some (opts, []) ->
        let opts = clearProjectReferences opts
        let! res = checker.ParseAndCheckProject opts
        return res.GetUsesOfSymbol symbol
      | Some (opts, dependentProjects) ->
        let! res =
          opts :: dependentProjects
          |> List.map (fun (opts) ->
            async {
              let opts = clearProjectReferences opts
              let! res = checker.ParseAndCheckProject opts
              return res.GetUsesOfSymbol symbol
            })
          |> Async.Parallel

        return res |> Array.concat
    }

  member _.FindReferencesForSymbolInFile(file, project, symbol) =
    checkerLogger.info (
      Log.setMessage "FindReferencesForSymbolInFile - {file}"
      >> Log.addContextDestructured "file" file
    )

    checker.FindBackgroundReferencesInFile(
      file,
      project,
      symbol,
      canInvalidateProject = false,
      userOpName = "find references"
    )

  member __.GetDeclarations(fileName: string<LocalPath>, source, options, version) =
    async {
      checkerLogger.info (
        Log.setMessage "GetDeclarations - {file}"
        >> Log.addContextDestructured "file" fileName
      )

      let! parseResult = checker.ParseFile(UMX.untag fileName, source, options)
      return parseResult.GetNavigationItems().Declarations
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
      fsiAdditionalFiles <- files
      scriptTypecheckRequirementsChanged.Trigger()
