namespace FsAutoComplete

open System.IO
open FSharp.Compiler.SourceCodeServices
open Utils
open FSharp.Compiler.Text
open FsAutoComplete.Logging
open Ionide.ProjInfo.ProjectSystem
open FSharp.UMX

type Version = int

type FSharpCompilerServiceChecker(backgroundServiceEnabled, hasAnalyzers) =
  let checker =
    FSharpChecker.Create(
      projectCacheSize = 200,
      keepAllBackgroundResolutions = not backgroundServiceEnabled,
      keepAssemblyContents = hasAnalyzers,
      suggestNamesForErrors = true,
      enablePartialTypeChecking = not hasAnalyzers,
      enableBackgroundItemKeyStoreAndSemanticClassification = true)

  do checker.ImplicitlyStartBackgroundWork <- not backgroundServiceEnabled
  do checker.BeforeBackgroundFileCheck.Add ignore

  // we only want to let people hook onto the underlying checker event if there's not a background service actually compiling things for us
  let safeFileCheckedEvent =
    if not backgroundServiceEnabled then checker.FileChecked else (new Event<string * obj option>()).Publish

  // /// FCS only accepts absolute file paths, so this ensures that by
  // /// rooting relative paths onto HOME on *nix and %HOMRDRIVE%%HOMEPATH% on windows
  // let ensureAbsolutePath path =
  //   if (try Path.GetFullPath path |> ignore; true with _ -> false) then path
  //   else
  //       match Environment.OSVersion.Platform with
  //       | PlatformID.Unix
  //       | PlatformID.MacOSX -> Environment.GetEnvironmentVariable "HOME"
  //       | _ -> Environment.ExpandEnvironmentVariables "%HOMEDRIVE%%HOMEPATH%"
  //       </> Path.GetFileName path

  let entityCache = EntityCache()

  let sdkRefsLogger = LogProvider.getLoggerByName "SdkRefs"
  let checkerLogger = LogProvider.getLoggerByName "Checker"
  let optsLogger =    LogProvider.getLoggerByName "Opts"

  /// the root path to the dotnet sdk installations, eg /usr/local/share/dotnet
  let mutable sdkRoot = None
  /// the chosen version of the dotnet sdk for deriving F# compiler FSI references, eg 3.0.100
  let mutable sdkVersion = lazy(None)
  /// the chosen version of the dotnet runtime for deriving BCL references, eg 3.0.0
  let mutable runtimeVersion = lazy(None)
  /// the map of assemblyNames and file paths derived from the sdkVersion and runtimeVersion
  let mutable discoveredAssembliesByName = lazy(Map.empty)
  /// additional arguments that are added to typechecking of scripts
  let mutable fsiAdditionalArguments = Array.empty
  let mutable fsiAdditionalFiles = Array.empty

  /// This event is raised when any data that impacts script typechecking
  /// is changed. This can potentially invalidate existing project options
  /// so we must purge any typecheck results for scripts.
  let scriptTypecheckRequirementsChanged = Event<_>()

  let mutable disableInMemoryProjectReferences = false

  /// evaluates the set of assemblies found given the current sdkRoot/sdkVersion/runtimeVersion
  let computeAssemblyMap () =
    match sdkRoot, sdkVersion.Force(), runtimeVersion.Force() with
    | None, _, _ ->
      sdkRefsLogger.info (Log.setMessage "No dotnet SDK root path found")
      Map.empty
    | Some root, None, None ->
      sdkRefsLogger.warn (Log.setMessage "Couldn't find matching sdk and runtime versions inside {root}" >> Log.addContextDestructured "root" root)
      Map.empty
    | Some root, None, _ ->
      sdkRefsLogger.warn (Log.setMessage "Couldn't find matching sdk version inside {root}" >> Log.addContextDestructured "root" root)
      Map.empty
    | Some root, _, None ->
      sdkRefsLogger.warn (Log.setMessage "Couldn't find matching runtime version inside {root}" >> Log.addContextDestructured "root" root)
      Map.empty
    | Some dotnetSdkRoot, Some sdkVersion, Some runtimeVersion ->
      let tfm = FSIRefs.tfmForRuntime sdkVersion
      let refs = FSIRefs.netCoreRefs dotnetSdkRoot (string sdkVersion) (string runtimeVersion) tfm true
      sdkRefsLogger.info (Log.setMessage "Found refs for {sdk} inside {root}"
                          >> Log.addContextDestructured "root" dotnetSdkRoot
                          >> Log.addContext "sdk" sdkVersion
                          >> Log.addContext "runtimeVersion" runtimeVersion
                          >> Log.addContextDestructured "tfm" tfm
                          >> Log.addContextDestructured "refs" refs)

      refs
      |> List.map (fun path -> Path.GetFileNameWithoutExtension path, path)
      |> Map.ofList

  let (|StartsWith|_|) (prefix: string) (s: string) =
    if s.StartsWith(prefix) then Some (s.[prefix.Length..]) else None

  let processFSIArgs args =
    (([||], [||]), args)
    ||> Array.fold (fun (args, files) arg ->
        match arg with
        | StartsWith "--use:" file | StartsWith "--load:" file -> args, Array.append files [| file |]
        | arg -> Array.append args [| arg |], files
    )

  let clearProjectReferences (opts: FSharpProjectOptions) =
    if disableInMemoryProjectReferences then {opts with ReferencedProjects = [||]} else opts

  let logQueueLength (logger: ILog) msg =
    checkerLogger.trace (Log.setMessage "Current Queue Length is {queueLength}" >> Log.addContextDestructured "queueLength" checker.CurrentQueueLength)
    logger.info msg

  /// replace any BCL/FSharp.Core/FSI refs that FCS gives us with our own set, which is more probe-able
  let replaceFrameworkRefs (projOptions: FSharpProjectOptions) =
    let refs, otherOptions = projOptions.OtherOptions |> Array.partition (fun r -> r.StartsWith "-r:")

    let fcsAndScriptReferences =
      refs
      |> Array.choose (fun r ->
          let path = r.[3..]
          let assemblyName = Path.GetFileNameWithoutExtension path
          // don't include the private imple assemblies that the compiler APIs want to give us
          if assemblyName.Contains "System.Private"
          then None
          // per https://github.com/fsharp/FSharp.Compiler.Service/blob/122520fa62edec7be5d00854989b282bf3ce7315/src/fsharp/DotNetFrameworkDependencies.fs#L262-L273
          // The Windows compatibility pack included in the runtime contains a reference to
          // System.Runtime.WindowsRuntime, but to properly use that type the runtime also needs a
          // reference to the Windows.md meta-package, which isn't referenced by default.  To avoid
          // a bug where types from `Windows, Version=255.255.255.255` can't be found we're going to
          // not default include this assembly.  It can still be manually referenced if it's needed
          // via the System.Runtime.WindowsRuntime NuGet package.
          //
          // In the future this branch can be removed because WinRT support is being removed from the
          // .NET 5 SDK (https://github.com/dotnet/runtime/pull/36715)
          else if assemblyName.Contains "System.Runtime.WindowsRuntime"
            || assemblyName.Contains "System.Runtime.WindowsRuntime.UI.Xaml"
          then None
          else if assemblyName.Contains "mscorlib" then None
          else Some (assemblyName, path)
      )
      |> Map.ofArray

    let mergedRefs =
      // we combine here taking our framework references first and throwing away theirs.
      // this is important because #r order influences typechecking
      Map.combineTakeFirst discoveredAssembliesByName.Value fcsAndScriptReferences
      |> Map.values
      |> Seq.map (fun r -> "-r:" + r)
      |> Array.ofSeq

    { projOptions with OtherOptions = Array.append otherOptions mergedRefs }

  /// ensures that any user-configured include/load files are added to the typechecking context
  let addLoadedFiles (projectOptions: FSharpProjectOptions) =
    let files = Array.append fsiAdditionalFiles projectOptions.SourceFiles
    logQueueLength optsLogger (Log.setMessage "Source file list is {files}" >> Log.addContextDestructured "files" files)
    { projectOptions with
        SourceFiles = files }

  let (|Reference|_|) (opt: string) =
    if opt.StartsWith "-r:" then Some (opt.[3..]) else None

  /// ensures that all file paths are absolute before being sent to the compiler, because compilation of scripts fails with relative paths
  let resolveRelativeFilePaths (projectOptions: FSharpProjectOptions) =
    { projectOptions with
        SourceFiles = projectOptions.SourceFiles |> Array.map Path.GetFullPath
        OtherOptions = projectOptions.OtherOptions |> Array.map (fun opt ->
          match opt with
          | Reference r -> $"-r:{Path.GetFullPath r}"
          | opt -> opt
        ) }

  let filterOutPlatformSpecificRefs (projectOptions: FSharpProjectOptions) =
    { projectOptions with
        OtherOptions = projectOptions.OtherOptions |> Array.filter (fun opt ->
          match opt with
          | Reference r ->
            let dllName = Path.GetFileNameWithoutExtension r
            not (dllName.StartsWith "Microsoft") && not (dllName.StartsWith "Windows") && not (dllName.StartsWith "System")
          | _ -> true
        ) }

  member __.DisableInMemoryProjectReferences
    with get() = disableInMemoryProjectReferences
    and set(value) = disableInMemoryProjectReferences <- value

  member __.GetDependingProjects (file: string<LocalPath>) (options : seq<string * FSharpProjectOptions>) =
    let project = options |> Seq.tryFind (fun (k,_) -> (UMX.untag k).ToUpperInvariant() = (UMX.untag file).ToUpperInvariant())
    project |> Option.map (fun (_, option) ->
      option, [
        yield! options
               |> Seq.map snd
               |> Seq.distinctBy (fun o -> o.ProjectFileName)
               |> Seq.filter (fun o -> o.ReferencedProjects |> Array.map (fun (_,v) -> Path.GetFullPath v.ProjectFileName) |> Array.contains option.ProjectFileName )
      ])

  member private __.GetNetFxScriptOptions(file: string<LocalPath>, source) = async {
    logQueueLength optsLogger (Log.setMessage "Getting NetFX options for script file {file}" >> Log.addContextDestructured "file" file)
    let allFlags = Array.append [| "--targetprofile:mscorlib" |] fsiAdditionalArguments
    let! (opts, errors) = checker.GetProjectOptionsFromScript(UMX.untag file, source, assumeDotNetFramework = true, useFsiAuxLib = true, otherFlags = allFlags, userOpName = "getNetFrameworkScriptOptions")
    let allModifications = addLoadedFiles >> resolveRelativeFilePaths
    return allModifications opts, errors
  }

  member private __.GetNetCoreScriptOptions(file: string<LocalPath>, source) = async {
    logQueueLength optsLogger (Log.setMessage "Getting NetCore options for script file {file}" >> Log.addContextDestructured "file" file)
    let allFlags = Array.append [| "--targetprofile:netstandard" |] fsiAdditionalArguments
    let! (opts, errors) = checker.GetProjectOptionsFromScript(UMX.untag file, source, assumeDotNetFramework = false, useSdkRefs = true, useFsiAuxLib = true, otherFlags = allFlags, userOpName = "getNetCoreScriptOptions")
    optsLogger.trace (Log.setMessage "Got NetCore options {opts} for file {file} with errors {errors}" >> Log.addContextDestructured "file" file >> Log.addContextDestructured "opts" opts >> Log.addContextDestructured "errors" errors)
    let allModifications =
      replaceFrameworkRefs >>
      // filterOutPlatformSpecificRefs >>
      addLoadedFiles >>
      resolveRelativeFilePaths
    let modified = allModifications opts
    optsLogger.trace (Log.setMessage "Replaced options to {opts}" >> Log.addContextDestructured "opts" modified)
    return modified, errors
  }

  member self.GetProjectOptionsFromScript(file: string<LocalPath>, source, tfm) = async {
    let! (projOptions, errors) =
      match tfm with
      | FSIRefs.TFM.NetFx ->
        self.GetNetFxScriptOptions(file, source)
      | FSIRefs.TFM.NetCore ->
        self.GetNetCoreScriptOptions(file, source)

    match errors with
    | [] -> ()
    | errs ->
      logQueueLength optsLogger (Log.setLogLevel LogLevel.Error >> Log.setMessage "Resolved {opts} with {errors}" >> Log.addContextDestructured "opts" projOptions >> Log.addContextDestructured "errors" errs)

    return projOptions

    // try
    //   match FakeSupport.detectFakeScript file with
    //   | None ->
    //     logQueueLength optsLogger (Log.setMessage "{file} is not a FAKE script" >> Log.addContextDestructured "file" file)
    //     return projOptions
    //   | Some (detectionInfo) ->
    //     logQueueLength optsLogger (Log.setMessage "{file} is a FAKE script" >> Log.addContextDestructured "file" file)
    //     try
    //       let otherOpts = FakeSupport.getProjectOptions detectionInfo
    //       logQueueLength optsLogger (Log.setMessage "Discovered FAKE options {otherOpts} " >> Log.addContextDestructured "file" file >> Log.addContextDestructured "otherOpts" otherOpts)
    //       return { projOptions with OtherOptions = otherOpts }
    //     with e ->
    //       logQueueLength optsLogger (Log.setLogLevel LogLevel.Error >> Log.setMessage "Error in FAKE script support" >> Log.addExn e)
    //       return projOptions
    // with
    // | e ->
    //   logQueueLength optsLogger (Log.setMessage "error while checking if {file} is a FAKE script" >> Log.addContextDestructured "file" file >> Log.addExn e)
    //   return projOptions
  }

  member __.GetBackgroundCheckResultsForFileInProject(fn: string<LocalPath>, opt) =
    logQueueLength checkerLogger (Log.setMessage "GetBackgroundCheckResultsForFileInProject - {file}" >> Log.addContextDestructured "file" fn)
    let opt = clearProjectReferences opt
    checker.GetBackgroundCheckResultsForFileInProject(UMX.untag fn, opt)
    |> Async.map (fun (pr,cr) ->  ParseAndCheckResults (pr, cr, entityCache))

  member __.FileChecked: IEvent<string<LocalPath> * obj option> =
    safeFileCheckedEvent
    |> Event.map (fun (fileName, blob) -> UMX.tag fileName, blob) //path comes from the compiler, so it's safe to assume the tag in this case

  member __.ScriptTypecheckRequirementsChanged =
    scriptTypecheckRequirementsChanged.Publish

  member __.ParseFile(fn: string<LocalPath>, source, fpo) =
    logQueueLength checkerLogger (Log.setMessage "ParseFile - {file}" >> Log.addContextDestructured "file" fn)
    checker.ParseFile(UMX.untag fn, source, fpo)

  member __.ParseAndCheckFileInProject(filePath: string<LocalPath>, version, source, options) =
    async {
      let opName = sprintf "ParseAndCheckFileInProject - %A" filePath
      logQueueLength checkerLogger (Log.setMessage "{opName}" >> Log.addContextDestructured "opName" opName)
      let options = clearProjectReferences options
      try
        let! (p, c) = checker.ParseAndCheckFileInProject (UMX.untag filePath, version, source, options, userOpName = opName)
        let parseErrors = p.Errors |> Array.map (fun p -> p.Message)
        match c with
        | FSharpCheckFileAnswer.Aborted ->
          logQueueLength checkerLogger (Log.setMessage "{opName} completed with errors: {errors}" >> Log.addContextDestructured "opName" opName >> Log.addContextDestructured "errors" (List.ofArray p.Errors))
          return ResultOrString.Error (sprintf "Check aborted (%A). Errors: %A" c parseErrors)
        | FSharpCheckFileAnswer.Succeeded(c) ->
          logQueueLength checkerLogger (Log.setMessage "{opName} completed successfully" >> Log.addContextDestructured "opName" opName)
          return Ok (ParseAndCheckResults(p, c, entityCache))
      with
      | ex ->
        return ResultOrString.Error (ex.ToString())
    }

  member __.TryGetRecentCheckResultsForFile(file: string<LocalPath>, options, ?source) =
    let opName = sprintf "TryGetRecentCheckResultsForFile - %A" file
    logQueueLength checkerLogger (Log.setMessage "{opName}" >> Log.addContextDestructured "opName" opName)
    let options = clearProjectReferences options
    checker.TryGetRecentCheckResultsForFile(UMX.untag file, options, ?sourceText=source, userOpName=opName)
    |> Option.map (fun (pr, cr, _) -> ParseAndCheckResults (pr, cr, entityCache))

  member x.GetUsesOfSymbol (file: string<LocalPath>, options : (string * FSharpProjectOptions) seq, symbol : FSharpSymbol) = async {
    logQueueLength checkerLogger (Log.setMessage "GetUsesOfSymbol - {file}" >> Log.addContextDestructured "file" file)
    let projects = x.GetDependingProjects file options
    return!
      match projects with
      | None -> async { return [||] }
      | Some (p, projects) -> async {
        let! res =
          p :: projects
          |> Seq.map (fun (opts) -> async {
              let opts = clearProjectReferences opts
              let! res = checker.ParseAndCheckProject opts
              return res.GetUsesOfSymbol symbol
            })
          |> Async.Parallel
        return res |> Array.concat }
  }

  member __.GetDeclarations (fileName: string<LocalPath>, source, options, version) = async {
    logQueueLength checkerLogger (Log.setMessage "GetDeclarations - {file}" >> Log.addContextDestructured "file" fileName)
    let! parseResult = checker.ParseFile(UMX.untag fileName, source, options)
    return parseResult.GetNavigationItems().Declarations
  }

  member __.Compile = checker.Compile

  member internal __.GetFSharpChecker() = checker

  member __.SetDotnetRoot(dotnetBinary: FileInfo) =
    if sdkRoot = Some dotnetBinary.Directory
    then ()
    else
      sdkRoot <- Some dotnetBinary.Directory
      //TODO(CH): should this range be user-assignable somehow?
      let allowedVersionRange =
        let maxVersion = System.Environment.Version.Major + 1
        SemanticVersioning.Range.Parse $"< %d{maxVersion}"
      let sdk = lazy (
          Ionide.ProjInfo.SdkDiscovery.sdks dotnetBinary
          |> Array.map (fun sdk -> sdk.Version)
          |> Environment.maxVersionWithThreshold (Some allowedVersionRange) true
      )
      let runtime = lazy (
          Ionide.ProjInfo.SdkDiscovery.runtimes dotnetBinary
          |> Array.map (fun runtime -> runtime.Version)
          |> Environment.maxVersionWithThreshold (Some allowedVersionRange) true
          |> Option.map (fun v -> SemanticVersioning.Version(v.Major, v.Minor, 0))
      )
      sdkVersion <- sdk
      runtimeVersion <- runtime
      discoveredAssembliesByName <- lazy(computeAssemblyMap ())
      scriptTypecheckRequirementsChanged.Trigger ()

  member __.GetDotnetRoot () = sdkRoot

  member __.SetFSIAdditionalArguments args =
    //TODO: UX - if preview-required features are set, then auto-add langversion:preview for the user.
    if fsiAdditionalArguments = args
    then ()
    else
      let additionalArgs, files = processFSIArgs args
      fsiAdditionalArguments <- additionalArgs
      fsiAdditionalFiles <- files
      scriptTypecheckRequirementsChanged.Trigger ()
