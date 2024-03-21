namespace FsAutoComplete.ProjectWorkspace



module AMap =
  open FSharp.Data.Adaptive
  let rec findAllDependenciesOfAndIncluding key findNextKeys items =
      amap {
          // printfn "findAllDependenciesOfAndIncluding %A" key
          let! item = AMap.tryFind key items
          match item with
          | None ->
              ()
          | Some item ->
              yield key, item
              let! dependencies =
                  item
                  |> AVal.map(
                      findNextKeys
                      >> Seq.map (fun newKey -> findAllDependenciesOfAndIncluding newKey findNextKeys items)
                      >> Seq.fold(fun s v -> AMap.union v s) AMap.empty
                  )
              yield! dependencies
      }

  let findAllDependenciesOfAndIncluding2 key (findNextKeys : _ -> seq<_>) items =
    let rec inner key =
      aset {
          // printfn "findAllDependenciesOfAndIncluding %A" key
          let! item = AMap.tryFind key items
          match item with
          | None ->
              ()
          | Some item ->
              yield key, item
              let! itemV = item
              for newKey in findNextKeys itemV do
                yield! inner newKey
      }
    inner key
    |> AMap.ofASet
    |> AMap.choose' Seq.tryHead


  let rec findAllDependentsOfAndIncluding key findNextKeys items =
      amap {
          // printfn "findAllDependentsOfAndIncluding %A" key
          let immediateDependents =
              items
              |> AMap.filterA (fun _ v -> v |> AVal.map (findNextKeys >> Seq.exists ((=) key)))
          yield! immediateDependents
          let! dependentOfDependents =
              immediateDependents
              |> AMap.map (fun nextKey _ -> findAllDependentsOfAndIncluding nextKey findNextKeys items)
              |> AMap.fold(fun acc _ x -> AMap.union acc x) AMap.empty
          yield! dependentOfDependents
      }


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
      projectFile.Refresh ()
      projectFile.LastWriteTimeUtc

    let getStream (_ctok: System.Threading.CancellationToken) =
      try
        projectFile.OpenRead() :> Stream |> Some
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
        let stamp = DateTime.UtcNow.Ticks
        logger.info(
          Log.setMessage "Creating FCS snapshot {projectFileName} {stamp}"
          >> Log.addContextDestructured "projectFileName" projectFileName
          >> Log.addContextDestructured "stamp" stamp
        )

        // printfn "Snapshot %A" projectFileName
        return FSharpProjectSnapshot.Create(
          projectFileName,
          projectId,
          sourceFiles,
          referencePaths,
          otherOptions,
          referencedProjects  ,
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


  let private createFSharpFileSnapshotOnDisk (sourceTextFactory : aval<ISourceTextFactory>) fileName  =
    aval {
      let! writeTime = AdaptiveFile.GetLastWriteTimeUtc fileName
      and! sourceTextFactory = sourceTextFactory
      let getSource () = task {

        let file = Utils.normalizePath fileName
        // use s = File.openFileStreamForReadingAsync file
        // let! source = sourceTextFactory.Create(file, s) CancellationToken.None
        let! text = File.ReadAllTextAsync fileName
        let source = sourceTextFactory.Create(file, text)
        return source :> ISourceTextNew
      }
      // printfn "Creating source text for %s" fileName
      return ProjectSnapshot.FSharpFileSnapshot.Create(fileName, string writeTime.Ticks, getSource)
    }
  let private createFSharpFileSnapshotInMemory (v : VolatileFile) =
    let file = UMX.untag v.FileName
    let version = v.LastTouched.Ticks
    let getSource () =
      v.Source
      :> ISourceTextNew
      |> Task.FromResult
    ProjectSnapshot.FSharpFileSnapshot.Create(file, string version, getSource)

  let private createReferenceOnDisk path : aval<ProjectSnapshot.ReferenceOnDisk> =
    aval {
      let! lastModified = AdaptiveFile.GetLastWriteTimeUtc path
      return { LastModified = lastModified; Path = path }
    }

  let private createReferencedProjectsFSharpReference projectOutputFile (snapshot: aval<FSharpProjectSnapshot>) =
    aval {
      let! projectOutputFile = projectOutputFile
      and! snapshot = snapshot
      return FSharpReferencedProjectSnapshot.FSharpReference(projectOutputFile, snapshot)
    }

  let rec private createReferences
    (cachedSnapshots)
    (inMemorySourceFiles : amap<string<LocalPath>, aval<VolatileFile>>)
    (sourceTextFactory: aval<ISourceTextFactory>)
    (loadedProjectsA: amap<string<LocalPath>,ProjectOptions>)
    (p : ProjectOptions) =
    logger.info(
      Log.setMessage "Creating references for {projectFileName}"
      >> Log.addContextDestructured "projectFileName" p.ProjectFileName
    )
    let normPath = Utils.normalizePath p.ProjectFileName
    // let deps =
    //   loadedProjectsA
    //   |> AMap.findAllDependenciesOfAndIncluding2
    //     normPath
    //     (fun p -> p.ReferencedProjects |> Seq.map(_.ProjectFileName >> Utils.normalizePath))
    let deps =
      loadedProjectsA
      |> AMap.filter(fun k _ -> p.ReferencedProjects |> List.exists(fun x -> x.ProjectFileName = UMX.untag k))
    deps
    |> AMap.filter(fun k _ -> k <> normPath)
    |> AMap.map(fun _ p -> aval {
      if p.ProjectFileName.EndsWith ".fsproj" then
        let snapshot = optionsToSnapshot cachedSnapshots inMemorySourceFiles sourceTextFactory (createReferences cachedSnapshots inMemorySourceFiles sourceTextFactory loadedProjectsA) p
        return! createReferencedProjectsFSharpReference (AVal.constant p.ResolvedTargetPath) snapshot
      else
        // TODO: Find if this needs to be adaptive or if `getStamp` in a PEReference will be enough to break thru the caching in FCS
        return loadFromDotnetDll p
    })
    |> AMap.toASetValues

  and optionsToSnapshot
    (cachedSnapshots : Dictionary<_,_>)
    (inMemorySourceFiles : amap<_, aval<VolatileFile>>)
    (sourceTextFactory: aval<ISourceTextFactory>)
    (mapReferences: ProjectOptions -> aset<aval<FSharpReferencedProjectSnapshot>>)
    (p : ProjectOptions) =

    // printfn "optionsToSnapshot - enter %A" p.ProjectFileName
    aval {
      let normPath = Utils.normalizePath p.ProjectFileName
      match cachedSnapshots.TryGetValue normPath with
      | (true, x) ->
        logger.info(
          Log.setMessage "optionsToSnapshot - Cache hit - {projectFileName}"
          >> Log.addContextDestructured "projectFileName" p.ProjectFileName
        )
        // printfn "optionsToSnapshot - Cache hit  %A" p.ProjectFileName
        return! x
      | _ ->
        logger.info(
          Log.setMessage "optionsToSnapshot - Cache miss - {projectFileName}"
          >> Log.addContextDestructured "projectFileName" p.ProjectFileName
        )
        // printfn "optionsToSnapshot - Cache miss %A" p.ProjectFileName
        let projectName = p.ProjectFileName
        let projectId = p.ProjectId |> AVal.constant

        let sourceFiles =
          p.SourceFiles
          |> AList.ofList
          |> AList.map(fun sourcePath ->
              let normPath = Utils.normalizePath sourcePath
              aval {
                match! inMemorySourceFiles |> AMap.tryFind normPath with
                | Some volatileFile ->
                  return! volatileFile |> AVal.map createFSharpFileSnapshotInMemory
                | None -> return! createFSharpFileSnapshotOnDisk sourceTextFactory sourcePath
              }

              )

        let references, otherOptions = p.OtherOptions |> List.partition (fun x -> x.StartsWith("-r:"))
        let otherOptions = otherOptions |> ASet.ofList |> ASet.map(AVal.constant)
        let referencePaths =
          references
          |> ASet.ofList
          |> ASet.map(fun referencePath ->
            referencePath.Substring(3) // remove "-r:"
            |> createReferenceOnDisk
          )
        let referencedProjects = mapReferences p
        let isIncompleteTypeCheckEnvironment = AVal.constant false
        let useScriptResolutionRules = AVal.constant false
        let loadTime = AVal.constant p.LoadTime
        let unresolvedReferences = AVal.constant None
        let originalLoadReferences = AVal.constant []

        let snap =
          makeAdaptiveFCSSnapshot2
            (AVal.constant projectName)
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

        return! snap
    }

  let createSnapshots
    (inMemorySourceFiles: amap<string<LocalPath>,aval<VolatileFile>>)
    (sourceTextFactory: aval<ISourceTextFactory>)
    (loadedProjectsA: amap<string<LocalPath>,ProjectOptions>) =
    let cachedSnapshots = Dictionary<_,_>()
    let mapReferences = createReferences cachedSnapshots inMemorySourceFiles sourceTextFactory loadedProjectsA
    let optionsToSnapshot =  optionsToSnapshot cachedSnapshots inMemorySourceFiles sourceTextFactory mapReferences

    loadedProjectsA
    |> AMap.filter(fun k _ -> (UMX.untag k).EndsWith ".fsproj")
    |> AMap.map (fun _ v -> v, optionsToSnapshot v )
