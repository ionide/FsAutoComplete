namespace FsAutoComplete.ProjectWorkspace

open System
open FsAutoComplete.Telemetry
open FsAutoComplete.Utils.Tracing

module Snapshots =
  open System
  open FsAutoComplete
  open System.Threading
  open FSharp.UMX
  open System.Threading.Tasks
  open Ionide.ProjInfo.Types
  open FSharp.Compiler.CodeAnalysis.ProjectSnapshot
  open System.IO
  open FSharp.Compiler.CodeAnalysis
  open FSharp.Data.Adaptive
  open FSharp.Compiler.Text
  open FsAutoComplete.Adaptive
  open Ionide.ProjInfo.Logging
  open System.Collections.Generic

  let rec logger = LogProvider.getLoggerByQuotation <@ logger @>

  let private loadFromDotnetDll (p: ProjectOptions) : FSharpReferencedProjectSnapshot =
    /// because only a successful compilation will be written to a DLL, we can rely on
    /// the file metadata for things like write times
    let projectFile = FileInfo p.TargetPath

    let getStamp () =
      projectFile.Refresh()
      projectFile.LastWriteTimeUtc

    let getStream (_ctok: System.Threading.CancellationToken) =
      try
        File.openFileStreamForReadingAsync (normalizePath p.TargetPath) :> Stream |> Some
      with _ ->
        None

    let delayedReader = DelayedILModuleReader(p.TargetPath, getStream)

    ProjectSnapshot.FSharpReferencedProjectSnapshot.PEReference(getStamp, delayedReader)

  let makeAdaptiveFCSSnapshot
    projectFileName
    projectId
    sourceFiles
    referencePaths
    otherOptions
    referencedProjects
    isIncompleteTypeCheckEnvironment
    useScriptResolutionRules
    loadTime
    unresolvedReferences
    originalLoadReferences
    =
    aval {
      let! projectFileName = projectFileName
      and! projectId = projectId
      and! sourceFiles = sourceFiles
      and! referencePaths = referencePaths
      and! otherOptions = otherOptions
      and! referencedProjects = referencedProjects
      and! isIncompleteTypeCheckEnvironment = isIncompleteTypeCheckEnvironment
      and! useScriptResolutionRules = useScriptResolutionRules
      and! loadTime = loadTime
      and! unresolvedReferences = unresolvedReferences
      and! originalLoadReferences = originalLoadReferences
      // Always use a new stamp for a new snapshot
      let stamp = DateTime.UtcNow.Ticks

      logger.debug (
        Log.setMessage "Creating FCS snapshot {projectFileName} {stamp}"
        >> Log.addContextDestructured "projectFileName" projectFileName
        >> Log.addContextDestructured "stamp" stamp
      )

      return
        FSharpProjectSnapshot.Create(
          projectFileName,
          projectId,
          sourceFiles,
          referencePaths,
          otherOptions,
          referencedProjects,
          isIncompleteTypeCheckEnvironment,
          useScriptResolutionRules,
          loadTime,
          unresolvedReferences,
          originalLoadReferences,
          Some stamp
        )
    }

  let makeAdaptiveFCSSnapshot2
    projectFileName
    projectId
    (sourceFiles: alist<aval<FSharpFileSnapshot>>)
    (referencePaths: aset<aval<ReferenceOnDisk>>)
    (otherOptions: aset<aval<string>>)
    (referencedProjects: aset<aval<FSharpReferencedProjectSnapshot>>)
    isIncompleteTypeCheckEnvironment
    useScriptResolutionRules
    loadTime
    unresolvedReferences
    originalLoadReferences
    =
    let flattenASet (s: aset<aval<'a>>) = s |> ASet.mapA id |> ASet.toAVal |> AVal.map HashSet.toList
    let flattenAList (s: alist<aval<'a>>) = s |> AList.mapA id |> AList.toAVal |> AVal.map IndexList.toList

    makeAdaptiveFCSSnapshot
      projectFileName
      projectId
      (flattenAList sourceFiles)
      (flattenASet referencePaths)
      (flattenASet otherOptions)
      (flattenASet referencedProjects)
      isIncompleteTypeCheckEnvironment
      useScriptResolutionRules
      loadTime
      unresolvedReferences
      originalLoadReferences

  let private createFSharpFileSnapshotOnDisk
    (sourceTextFactory: aval<ISourceTextFactory>)
    (sourceFilePath: string<LocalPath>)
    =
    aval {
      let file = UMX.untag sourceFilePath
      let! writeTime = AdaptiveFile.GetLastWriteTimeUtc file
      and! sourceTextFactory = sourceTextFactory

      let getSource () =
        task {
          let! sourceText = SourceTextFactory.readFile sourceFilePath sourceTextFactory CancellationToken.None
          return sourceText :> ISourceTextNew
        }

      return ProjectSnapshot.FSharpFileSnapshot.Create(file, string writeTime.Ticks, getSource)
    }

  let private createFSharpFileSnapshotInMemory (v: VolatileFile) =
    let file = UMX.untag v.FileName
    // Use LastTouched instead of Version because we're using that in the onDisk version
    // it's useful for keeping the cache consistent in FCS so when someone opens a file we don't need to re-issue type-checks
    let version = v.LastTouched.Ticks
    let getSource () = v.Source :> ISourceTextNew |> Task.FromResult

    ProjectSnapshot.FSharpFileSnapshot.Create(file, string version, getSource)

  let private createReferenceOnDisk path : aval<ProjectSnapshot.ReferenceOnDisk> =
    aval {
      let! lastModified = AdaptiveFile.GetLastWriteTimeUtc path

      return
        { LastModified = lastModified
          Path = path }
    }

  let private createReferencedProjectsFSharpReference projectOutputFile (snapshot: aval<FSharpProjectSnapshot>) =
    aval {
      let! projectOutputFile = projectOutputFile
      and! snapshot = snapshot
      return FSharpReferencedProjectSnapshot.FSharpReference(projectOutputFile, snapshot)
    }

  let rec private createReferences
    (cachedSnapshots)
    (inMemorySourceFiles: amap<string<LocalPath>, aval<VolatileFile>>)
    (sourceTextFactory: aval<ISourceTextFactory>)
    (loadedProjectsA: amap<string<LocalPath>, ProjectOptions>)
    (p: ProjectOptions)
    =
    let tags = seq {
      "projectFileName", box p.ProjectFileName
    }
    use _span = fsacActivitySource.StartActivityForFunc(tags = tags)
    logger.debug (
      Log.setMessage "Creating references for {projectFileName}"
      >> Log.addContextDestructured "projectFileName" p.ProjectFileName
    )

    loadedProjectsA
    |> AMap.filter (fun k _ ->
      p.ReferencedProjects
      |> List.exists (fun x -> normalizePath x.ProjectFileName = k))
    |> AMap.map (fun _ proj ->
      if proj.ProjectFileName.EndsWith ".fsproj" then

        let resolvedTargetPath =
          aval {
            // TODO: Find if this needs to be adaptive, unsure if we need to check if the file has changed on disk if we need a new snapshot
            let! _ = AdaptiveFile.GetLastWriteTimeUtc proj.ResolvedTargetPath
            return proj.ResolvedTargetPath
          }

        proj
        |> optionsToSnapshot
          cachedSnapshots
          inMemorySourceFiles
          sourceTextFactory
          (createReferences cachedSnapshots inMemorySourceFiles sourceTextFactory loadedProjectsA)
        |> createReferencedProjectsFSharpReference resolvedTargetPath

      else
        // TODO: Find if this needs to be adaptive or if `getStamp` in a PEReference will be enough to break thru the caching in FCS
        loadFromDotnetDll proj |> AVal.constant)
    |> AMap.toASetValues

  and private optionsToSnapshot
    (cachedSnapshots: Dictionary<_, _>)
    (inMemorySourceFiles: amap<_, aval<VolatileFile>>)
    (sourceTextFactory: aval<ISourceTextFactory>)
    (mapReferences: ProjectOptions -> aset<aval<FSharpReferencedProjectSnapshot>>)
    (p: ProjectOptions)
    =
    let normPath = Utils.normalizePath p.ProjectFileName
    let tags = seq {
      "projectFileName", box p.ProjectFileName
    }
    use span = fsacActivitySource.StartActivityForFunc(tags = tags)

    match cachedSnapshots.TryGetValue normPath with
    | true, snapshot ->
      span.SetTagSafe("cachehit", true) |> ignore
      logger.debug (
        Log.setMessage "optionsToSnapshot - Cache hit - {projectFileName}"
        >> Log.addContextDestructured "projectFileName" p.ProjectFileName
      )

      snapshot
    | _ ->
      logger.debug (
        Log.setMessage "optionsToSnapshot - Cache miss - {projectFileName}"
        >> Log.addContextDestructured "projectFileName" p.ProjectFileName
      )

      let projectName = AVal.constant p.ProjectFileName
      let projectId = AVal.constant p.ProjectId


      let sourceFiles = // alist because order matters for the F# Compiler
        p.SourceFiles
        |> AList.ofList
        |> AList.map Utils.normalizePath
        |> AList.map (fun sourcePath ->

          aval {
            match! inMemorySourceFiles |> AMap.tryFind sourcePath with
            | Some volatileFile -> return! volatileFile |> AVal.map createFSharpFileSnapshotInMemory
            | None -> return! createFSharpFileSnapshotOnDisk sourceTextFactory sourcePath
          })

      let references = p.OtherOptions |> List.filter (fun x -> x.StartsWith("-r:"))

      let otherOptions = p.OtherOptions |> ASet.ofList |> ASet.map (AVal.constant)

      let referencePaths =
        references
        |> ASet.ofList
        |> ASet.map (fun referencePath ->
          referencePath.Substring(3) // remove "-r:"
          |> createReferenceOnDisk)

      let referencedProjects = mapReferences p
      let isIncompleteTypeCheckEnvironment = AVal.constant false
      let useScriptResolutionRules = AVal.constant false
      let loadTime = AVal.constant p.LoadTime
      let unresolvedReferences = AVal.constant None
      let originalLoadReferences = AVal.constant []

      let snap =
        makeAdaptiveFCSSnapshot2
          projectName
          projectId
          sourceFiles
          referencePaths
          otherOptions
          referencedProjects
          isIncompleteTypeCheckEnvironment
          useScriptResolutionRules
          loadTime
          unresolvedReferences
          originalLoadReferences

      cachedSnapshots.Add(normPath, snap)

      snap

  let createSnapshots
    (inMemorySourceFiles: amap<string<LocalPath>, aval<VolatileFile>>)
    (sourceTextFactory: aval<ISourceTextFactory>)
    (loadedProjectsA: amap<string<LocalPath>, ProjectOptions>)
    =
    loadedProjectsA
    |> AMap.filter (fun k _ -> (UMX.untag k).EndsWith ".fsproj")
    |> AMap.toAVal
    |> AVal.map (fun ps ->
      let cachedSnapshots = Dictionary<_, _>()

      let mapReferences =
        createReferences cachedSnapshots inMemorySourceFiles sourceTextFactory loadedProjectsA

      let optionsToSnapshot =
        optionsToSnapshot cachedSnapshots inMemorySourceFiles sourceTextFactory mapReferences

      ps |> HashMap.map (fun _ v -> (v, optionsToSnapshot v)))
    |> AMap.ofAVal
