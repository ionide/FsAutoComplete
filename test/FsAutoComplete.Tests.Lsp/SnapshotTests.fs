module FsAutoComplete.Tests.SnapshotTests
open Expecto
open System.IO
open Ionide.ProjInfo
open FsAutoComplete.Utils
open FSharp.Data.Adaptive
open  FsAutoComplete.Adaptive
open System
open FSharp.Compiler.Text
open Ionide.ProjInfo.ProjectLoader
open Ionide.ProjInfo.Types
open FSharp.Compiler.CodeAnalysis.ProjectSnapshot
open FSharp.Compiler.CodeAnalysis
open IcedTasks

module FcsRange = FSharp.Compiler.Text.Range
type FcsRange = FSharp.Compiler.Text.Range
type FcsPos = FSharp.Compiler.Text.Position

// type ProjectSnapshotLike = {
//   ProjectFileName: string
//   ProjectId : string option
//   SourceFiles: string list
//   ReferencesOnDisk : string list
//   OtherOptions : string list
//   ReferencedProjects : string list
//   IsIncompleteTypeCheckEnvironment : bool
//   UseScriptResolutionRules : bool
//   LoadTime : DateTime
//   UnresolvedReferences : string list
//   OriginalLoadReferences: (FcsRange * string * string) list
//   Stamp: int64 option
// }
//   with
//     static member Create(p : ProjectOptions) =

//       {
//         ProjectFileName = p.ProjectFileName
//         ProjectId = p.ProjectId
//         SourceFiles = p.SourceFiles
//         ReferencesOnDisk = p.PackageReferences |> List.map (fun x -> x.FullPath)
//         OtherOptions = p.OtherOptions
//         ReferencedProjects = p.ReferencedProjects |> List.map (fun x -> x.ProjectFileName)
//         IsIncompleteTypeCheckEnvironment = false
//         UseScriptResolutionRules = false
//         LoadTime = p.LoadTime
//         UnresolvedReferences = []
//         OriginalLoadReferences = []
//         Stamp = None
//       }



let rec findAllDependenciesOfAndIncluding key findNextKeys items =
    amap {
        printfn "findAllDependenciesOfAndIncluding %A" key
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
                    >> Seq.fold(AMap.union) AMap.empty
                )
            yield! dependencies
    }

let rec findAllDependentsOfAndIncluding key findNextKeys items =
    amap {
        printfn "findAllDependentsOfAndIncluding %A" key
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

module Dotnet =
  let restore (projectPath : FileInfo) = async {
    let! cmd = Helpers.runProcess projectPath.Directory.FullName "dotnet" "restore"
    Helpers.expectExitCodeZero cmd
  }

  let restoreAll (projectPaths : FileInfo seq) = async {
    do!
      projectPaths
      |> Seq.map(fun p -> restore p)
      |> Async.Sequential
      |> Async.Ignore
  }

  let addPackage (projectPath : FileInfo) (packageName : string) (version : string) = async {
    let! cmd = Helpers.runProcess projectPath.Directory.FullName "dotnet" $"add package {packageName} --version {version}"
    Helpers.expectExitCodeZero cmd
  }

  let removePackage (projectPath : FileInfo) (packageName : string) = async {
    let! cmd = Helpers.runProcess projectPath.Directory.FullName "dotnet" $"remove package {packageName}"
    Helpers.expectExitCodeZero cmd
  }

module Projects =
  module Simple =
    let simpleProjectDir = DirectoryInfo(__SOURCE_DIRECTORY__ </> "TestCases/ProjectSnapshot/SimpleProject")
    let simpleProject = "SimpleProject.fsproj"
    let projects (srcDir : DirectoryInfo) =
      [
          FileInfo(srcDir.FullName </> simpleProject)
      ]

  module MultiProjectScenario1 =
    let multiProjectScenario1Dir = DirectoryInfo(__SOURCE_DIRECTORY__ </> "TestCases/ProjectSnapshot/MultiProjectScenario1")

    module Console1 =
      let dir = "Console1"
      let project = "Console1.fsproj"
      let projectIn (srcDir : DirectoryInfo) = FileInfo(srcDir.FullName </> dir </> project)
      let programFile = "Program.fs"
      let programFileIn (srcDir : DirectoryInfo) = FileInfo(srcDir.FullName </> dir </> programFile)
    module Library1 =
      let dir = "Library1"
      let project = "Library1.fsproj"
      let projectIn (srcDir : DirectoryInfo) = FileInfo(srcDir.FullName </> dir </> project)
      let LibraryFile = "Library.fs"
      let libraryFileIn (srcDir : DirectoryInfo) = FileInfo(srcDir.FullName </> dir </> LibraryFile)

    let projects (srcDir : DirectoryInfo) =
      [
          Console1.projectIn srcDir
          Library1.projectIn srcDir
      ]

let createProjectA (projects : FileInfo seq) (loader : IWorkspaceLoader) onLoadCallback =
  let projectsA =
    projects
    |> ASet.ofSeq
    |> ASet.mapAtoAMap (fun x -> AdaptiveFile.GetLastWriteTimeUtc x.FullName)

  let loadedProjectsA =
    projectsA
    |> AMap.toAVal
    |> AVal.map(fun kvp ->
      let projects = kvp.ToKeyList()
      let loaded = projects |> List.map (fun p -> p.FullName) |> loader.LoadProjects |> Seq.cache
      onLoadCallback ()
      projects
      |> List.map(fun p -> p.FullName, AVal.constant(p, loaded |> Seq.find(fun l -> l.ProjectFileName = p.FullName)))
    )
    |> AMap.ofAVal

  loadedProjectsA



module Snapshots =
  open FsAutoComplete

  let loadFromDotnetDll (p: Types.ProjectOptions) : ProjectSnapshot.FSharpReferencedProjectSnapshot =
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

        printfn "Snapshot %A" projectFileName
        let snap = FSharpProjectSnapshot.Create(
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
          Some (DateTime.UtcNow.Ticks)
        )

        return snap
      }

  let makeAdaptiveFCSSnapshot2
      projectFileName
      projectId
      (sourceFiles: aset<aval<FSharpFileSnapshot>>)
      (referencePaths: aset<aval<ReferenceOnDisk>>)
      (otherOptions: aset<aval<string>>)
      (referencedProjects: aset<aval<FSharpReferencedProjectSnapshot>>)
      isIncompleteTypeCheckEnvironment
      useScriptResolutionRules
      loadTime
      unresolvedReferences
      originalLoadReferences
        =
      makeAdaptiveFCSSnapshot
        projectFileName
        projectId
        (sourceFiles |> ASet.mapA id |> ASet.toAVal |> AVal.map HashSet.toList)
        (referencePaths |> ASet.mapA id |> ASet.toAVal |> AVal.map HashSet.toList)
        (otherOptions |> ASet.mapA id |> ASet.toAVal |> AVal.map HashSet.toList)
        (referencedProjects |> ASet.mapA id |> ASet.toAVal |> AVal.map HashSet.toList)
        isIncompleteTypeCheckEnvironment
        useScriptResolutionRules
        loadTime
        unresolvedReferences
        originalLoadReferences


  let createFSharpFileSnapshotOnDisk (sourceTextFactory : ISourceTextFactory) fileName  =
    aval {
      let! writeTime = AdaptiveFile.GetLastWriteTimeUtc fileName
      let getSource () = task {
        let! text = File.ReadAllTextAsync fileName
        return sourceTextFactory.Create((normalizePath fileName), text) :> ISourceTextNew
      }
      printfn "Creating source text for %s" fileName
      return ProjectSnapshot.FSharpFileSnapshot.Create(fileName, string writeTime.Ticks, getSource)
    }
  let createReferenceOnDisk path : aval<ProjectSnapshot.ReferenceOnDisk> =
    aval {
      let! lastModified = AdaptiveFile.GetLastWriteTimeUtc path
      return { LastModified = lastModified; Path = path }
    }

  let createReferencedProjectsFSharpReference projectOutputFile (snapshot: aval<FSharpProjectSnapshot>) =
    aval {
      let! projectOutputFile = projectOutputFile
      and! snapshot = snapshot
      return FSharpReferencedProjectSnapshot.FSharpReference(projectOutputFile, snapshot)
    }

  let rec createReferences cached sourceTextFactory loadedProjectsA (p : ProjectOptions) =
    findAllDependenciesOfAndIncluding
      p.ProjectFileName
        (fun (_, p : ProjectOptions) -> p.ReferencedProjects |> List.map(fun x -> x.ProjectFileName)) loadedProjectsA
    |> AMap.filter (fun k _ -> k <> p.ProjectFileName)
    |> AMap.mapAVal(fun _ (_,p) -> aval {
      if p.ProjectFileName.EndsWith ".fsproj" then
        let opt = aval {
          match! cached |> AMap.tryFind p.ProjectFileName with
          | Some x ->
            printfn "mapReferences - Cache hit  %A" p.ProjectFileName
            return! x
          | None ->
            printfn "mapReferences - Cache miss mapReferences %A" p.ProjectFileName
            return! (optionsToSnapshot cached sourceTextFactory (createReferences cached sourceTextFactory loadedProjectsA) p)
        }
        return! createReferencedProjectsFSharpReference (AVal.constant p.ResolvedTargetPath) opt
      else
        return loadFromDotnetDll p
    })
    |> AMap.toASetValues


  and optionsToSnapshot (cache : ChangeableHashMap<_,_>) (sourceTextFactory : ISourceTextFactory) (mapReferences: ProjectOptions -> aset<aval<FSharpReferencedProjectSnapshot>>) (p : ProjectOptions) =

    printfn "optionsToSnapshot - enter %A" p.ProjectFileName
    aval {
      match! cache |> AMap.tryFind p.ProjectFileName with
      | Some x ->
        printfn "optionsToSnapshot - Cache hit  %A" p.ProjectFileName
        return! x
      | None ->
        printfn "optionsToSnapshot - Cache miss %A" p.ProjectFileName
        let projectName = p.ProjectFileName |> AVal.constant
        let projectId = p.ProjectId |> AVal.constant

        let sourceFiles =
          // TODO Make it use "open files" and file system files
          p.SourceFiles
          |> ASet.ofList
          |> ASet.map(createFSharpFileSnapshotOnDisk sourceTextFactory)

        let references, otherOptions = p.OtherOptions |> List.partition (fun x -> x.StartsWith("-r:"))
        let otherOptions = otherOptions |> ASet.ofList |> ASet.map(AVal.constant)
        let referencePaths =
          references
          |> ASet.ofList
          |> ASet.map(fun referencePath ->
            let path = referencePath.Substring(3) // remove "-r:"
            createReferenceOnDisk path
          )
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

        let! snap =
          aval {
            let! projectName = projectName
            transact <| fun () ->
              cache.Add(projectName, snap) |> ignore<_>
            return! snap
          }

        return snap
    }


  let private makeFCSSnapshot
    makeFileSnapshot
    makeFileOnDisk
    mapProjectToReference
    (project: Types.ProjectOptions)
    : FSharpProjectSnapshot =
    let references, otherOptions =
      project.OtherOptions |> List.partition (fun x -> x.StartsWith("-r:"))

    let referencePaths = references |> List.map (fun x -> x.Substring(3))

    FSharpProjectSnapshot.Create(
      project.ProjectFileName,
      project.ProjectId,
      project.SourceFiles |> List.map makeFileSnapshot,
      referencePaths |> List.map makeFileOnDisk,
      otherOptions,
      project.ReferencedProjects |> List.choose mapProjectToReference,
      isIncompleteTypeCheckEnvironment = false,
      useScriptResolutionRules = false,
      loadTime = project.LoadTime,
      unresolvedReferences = None,
      originalLoadReferences = [],
      stamp = Some DateTime.UtcNow.Ticks
    )


  let private makeProjectReference
    isKnownProject
    makeFSharpProjectReference
    (p: Types.ProjectReference)
    : ProjectSnapshot.FSharpReferencedProjectSnapshot option =
    let knownProject = isKnownProject p

    let isDotnetProject (knownProject: Types.ProjectOptions option) =
      match knownProject with
      | Some p ->
        (p.ProjectFileName.EndsWith(".csproj") || p.ProjectFileName.EndsWith(".vbproj"))
        && File.Exists p.ResolvedTargetPath
      | None -> false

    if p.ProjectFileName.EndsWith ".fsproj" then
      knownProject
      |> Option.map (fun (p: Types.ProjectOptions) ->
        let theseOptions = makeFSharpProjectReference p
        ProjectSnapshot.FSharpReferencedProjectSnapshot.FSharpReference(p.ResolvedTargetPath, theseOptions))
    elif isDotnetProject knownProject then
      knownProject |> Option.map loadFromDotnetDll
    else
      None

  let mapManySnapshots
    makeFile
    makeDiskReference
    (allKnownProjects: Types.ProjectOptions seq)
    : FSharpProjectSnapshot seq =
    seq {
      let dict =
        System.Collections.Concurrent.ConcurrentDictionary<Types.ProjectOptions, FSharpProjectSnapshot>()

      let isKnownProject (p: Types.ProjectReference) =
        allKnownProjects
        |> Seq.tryFind (fun kp -> kp.ProjectFileName = p.ProjectFileName)

      let rec makeFSharpProjectReference (p: Types.ProjectOptions) =
        let factory = makeProjectReference isKnownProject makeFSharpProjectReference
        let makeSnapshot = makeFCSSnapshot makeFile makeDiskReference factory
        dict.GetOrAdd(p, makeSnapshot)

      for project in allKnownProjects do
        let thisProject = dict.GetOrAdd(project, makeFSharpProjectReference)

        yield thisProject
    }



let createsnapshot mapProjectToReference documentSource (project : ProjectOptions) =


  let makeDiskReference referencePath : ProjectSnapshot.ReferenceOnDisk =
    { Path = referencePath
      LastModified = FSharp.Compiler.IO.FileSystemAutoOpens.FileSystem.GetLastWriteTimeShim(referencePath) }
  let makeFileSnapshot filePath : ProjectSnapshot.FSharpFileSnapshot =
    ProjectSnapshot.FSharpFileSnapshot.CreateFromDocumentSource(filePath, documentSource)

  let references, otherOptions =
    project.OtherOptions |> List.partition (fun x -> x.StartsWith("-r:"))
  let referencePaths = references |> List.map (fun x -> x.Substring(3))
  let referenceSnapshots = project.ReferencedProjects |> List.choose mapProjectToReference
  FSharpProjectSnapshot.Create(
    project.ProjectFileName,
    project.ProjectId,
    project.SourceFiles |> List.map makeFileSnapshot,
    referencePaths |> List.map makeDiskReference,
    otherOptions = otherOptions,
    referencedProjects  = referenceSnapshots,
    isIncompleteTypeCheckEnvironment = false,
    useScriptResolutionRules = false,
    loadTime = project.LoadTime,
    unresolvedReferences = None,
    originalLoadReferences = [],
    stamp = Some DateTime.UtcNow.Ticks

  )

let snapshotTests loaders toolsPath =
  testSequenced <|
  testList "SnapshotTests" [
  for (loaderName, workspaceLoaderFactory) in loaders do

    testList $"{loaderName}" [
      ptestCaseAsync "Simple Project Load" <| async {
        let (loader : IWorkspaceLoader) = workspaceLoaderFactory toolsPath
        let srcDir = Projects.Simple.simpleProjectDir
        use dDir = Helpers.DisposableDirectory.From srcDir
        let projects =  Projects.Simple.projects dDir.DirectoryInfo
        do! Dotnet.restoreAll projects

        let mutable loadedCalls = 0

        let loadedProjectsA = createProjectA projects loader (fun () -> loadedCalls <- loadedCalls + 1)

        let _loadedProjects = loadedProjectsA |> AMap.force
        Expect.equal 1 loadedCalls "Load Projects should only get called once"

        // No interaction with fsproj should not cause a reload
        let _loadedProjects = loadedProjectsA |> AMap.force
        Expect.equal 1 loadedCalls "Load Projects should only get called once after doing nothing that should trigger a reload"
      }

      ptestCaseAsync "Adding nuget package should cause project load" <| async {
        let (loader : IWorkspaceLoader) = workspaceLoaderFactory toolsPath
        let srcDir = Projects.Simple.simpleProjectDir
        use dDir = Helpers.DisposableDirectory.From srcDir
        let projects =  Projects.Simple.projects dDir.DirectoryInfo
        do! Dotnet.restoreAll projects

        let mutable loadedCalls = 0
        let loadedProjectsA = createProjectA projects loader (fun () -> loadedCalls <- loadedCalls + 1)

        let _loadedProjects = loadedProjectsA |> AMap.force
        Expect.equal 1 loadedCalls "Loaded Projects should only get called 1 time"

        do! Dotnet.addPackage (projects |> List.head) "Newtonsoft.Json" "12.0.3"

        let _loadedProjects = loadedProjectsA |> AMap.force
        Expect.equal 2 loadedCalls "Load Projects should have gotten called again after adding a nuget package"
      }

      ptestCaseAsync "Create snapshot" <| async {
        let (loader : IWorkspaceLoader) = workspaceLoaderFactory toolsPath
        let srcDir = Projects.Simple.simpleProjectDir
        use dDir = Helpers.DisposableDirectory.From srcDir
        let projects =  Projects.Simple.projects dDir.DirectoryInfo
        do! Dotnet.restoreAll projects

        let mutable loadedCalls = 0


        let loadedProjectsA =
          createProjectA projects loader (fun () -> loadedCalls <- loadedCalls + 1)
          |> AMap.map (fun _ (project) -> project |> AVal.map(snd >> createsnapshot (fun _ -> None) (DocumentSource.FileSystem)))

        let snapshots = loadedProjectsA |> AMap.force

        let (project, snapshotA) = snapshots |> Seq.head
        let snapshot = snapshotA |> AVal.force
        Expect.equal 1 loadedCalls "Loaded Projects should only get called 1 time"
        Expect.equal snapshot.ProjectFileName project "Snapshot should have the same project file name as the project"
        Expect.equal (Seq.length snapshot.SourceFiles) 3 "Snapshot should have the same number of source files as the project"
        Expect.equal (Seq.length snapshot.ReferencedProjects) 0 "Snapshot should have the same number of referenced projects as the project"
      }

      ptestCaseAsync "Create snapshot from multiple projects" <| async {
        let (loader : IWorkspaceLoader) = workspaceLoaderFactory toolsPath
        let srcDir = Projects.MultiProjectScenario1.multiProjectScenario1Dir
        use dDir = Helpers.DisposableDirectory.From srcDir
        let projects =  Projects.MultiProjectScenario1.projects dDir.DirectoryInfo
        do! Dotnet.restoreAll projects

        let mutable loadedCalls = 0

        let loadedProjectsA =
          createProjectA projects loader (fun () -> loadedCalls <- loadedCalls + 1)
        let snapsA =
          loadedProjectsA
          |> AMap.map (fun k (project) ->
            let references =
              findAllDependenciesOfAndIncluding k (fun (_, p : ProjectOptions) -> p.ReferencedProjects |> List.map(_.ProjectFileName)) loadedProjectsA
            aval {

              let! (_, _) = project
              and! references = references |> AMap.mapA (fun _ v -> v) |> AMap.toAVal
              let makeFile filePath : ProjectSnapshot.FSharpFileSnapshot =
                ProjectSnapshot.FSharpFileSnapshot.CreateFromDocumentSource(filePath, DocumentSource.FileSystem)

              let makeDiskReference referencePath : ProjectSnapshot.ReferenceOnDisk =
                { Path = referencePath
                  LastModified = FSharp.Compiler.IO.FileSystemAutoOpens.FileSystem.GetLastWriteTimeShim(referencePath) }

              let lol =
                references
                |> HashMap.toValueList
                |> Seq.map snd
                |> Snapshots.mapManySnapshots makeFile makeDiskReference
                |> Seq.tryFind (fun x -> x.ProjectFileName = k)
              return lol
            }

          )

        let snapshots = snapsA |> AMap.force |> HashMap.map (fun k v -> k, v |> AVal.force)

        let (project, snapshot) = snapshots |> HashMap.choose (fun _ (_,v) -> v) |> Seq.find(fun (_, s) -> s.ProjectFileName.EndsWith Projects.MultiProjectScenario1.Console1.project)
        Expect.equal 1 loadedCalls "Loaded Projects should only get called 1 time"
        Expect.equal snapshot.ProjectFileName project "Snapshot should have the same project file name as the project"
        Expect.equal (Seq.length snapshot.SourceFiles) 3 "Snapshot should have the same number of source files as the project"
        Expect.equal (Seq.length snapshot.ReferencedProjects) 1 "Snapshot should have the same number of referenced projects as the project"
      }

      testCaseAsync "Cached Adaptive Snapshot - MultiProject - Updating nothing shouldn't cause recalculation" <| asyncEx {
        let (loader : IWorkspaceLoader) = workspaceLoaderFactory toolsPath
        let sourceTextFactory : FsAutoComplete.ISourceTextFactory = FsAutoComplete.RoslynSourceTextFactory()
        let srcDir = Projects.MultiProjectScenario1.multiProjectScenario1Dir
        use dDir = Helpers.DisposableDirectory.From srcDir
        let projects =  Projects.MultiProjectScenario1.projects dDir.DirectoryInfo
        do! Dotnet.restoreAll projects

        let mutable loadedCalls = 0

        let loadedProjectsA = createProjectA projects loader (fun () -> loadedCalls <- loadedCalls + 1)

        let cache = ChangeableHashMap()

        let snapsA =
          loadedProjectsA
          |> AMap.mapAVal (fun _ (_,v) -> Snapshots.optionsToSnapshot  cache sourceTextFactory (Snapshots.createReferences cache sourceTextFactory loadedProjectsA) v)

        let snapshots = snapsA |> AMap.mapA (fun _ v -> v) |> AMap.force

        let snapshots2 = snapsA |> AMap.mapA (fun _ v -> v) |> AMap.force

        let ls1 = snapshots |> HashMap.find ((Projects.MultiProjectScenario1.Library1.projectIn dDir.DirectoryInfo).FullName)
        let ls2 = snapshots2 |> HashMap.find ((Projects.MultiProjectScenario1.Library1.projectIn dDir.DirectoryInfo).FullName)

        Expect.equal ls1.ProjectFileName ls2.ProjectFileName "Project file name should be the same"
        Expect.equal ls1.ProjectId ls2.ProjectId "Project Id name should be the same"
        Expect.equal ls1.SourceFiles.Length 3 "Source files length should be 3"
        Expect.equal ls1.SourceFiles.Length ls2.SourceFiles.Length "Source files length should be the same"
        Expect.equal ls1.ReferencedProjects.Length ls2.ReferencedProjects.Length "Referenced projects length should be the same"
        Expect.equal ls1.ReferencedProjects.Length 0 "Referenced projects length should be 0"
        Expect.equal ls1.Stamp ls2.Stamp "Stamp should be the same"

        let cs1 = snapshots |> HashMap.find ((Projects.MultiProjectScenario1.Console1.projectIn dDir.DirectoryInfo).FullName)
        let cs2 = snapshots2 |> HashMap.find ((Projects.MultiProjectScenario1.Console1.projectIn dDir.DirectoryInfo).FullName)

        Expect.equal cs1.ProjectFileName cs2.ProjectFileName "Project file name should be the same"
        Expect.equal cs1.ProjectId cs2.ProjectId "Project Id name should be the same"
        Expect.equal cs1.SourceFiles.Length 3 "Source files length should be 3"
        Expect.equal cs1.SourceFiles.Length cs2.SourceFiles.Length "Source files length should be the same"
        Expect.equal cs1.ReferencedProjects.Length cs2.ReferencedProjects.Length "Referenced projects length should be the same"
        Expect.equal cs1.ReferencedProjects.Length 1 "Referenced projects length should be 1"
        Expect.equal cs1.Stamp cs2.Stamp "Stamp should be the same"

      }


      ftestCaseAsync "Cached Adaptive Snapshot - MultiProject - Updating Source file in Console recreates Console snapshot" <| asyncEx {
        let (loader : IWorkspaceLoader) = workspaceLoaderFactory toolsPath
        let sourceTextFactory : FsAutoComplete.ISourceTextFactory = FsAutoComplete.RoslynSourceTextFactory()
        let srcDir = Projects.MultiProjectScenario1.multiProjectScenario1Dir
        use dDir = Helpers.DisposableDirectory.From srcDir
        let projects =  Projects.MultiProjectScenario1.projects dDir.DirectoryInfo
        do! Dotnet.restoreAll projects

        let mutable loadedCalls = 0

        let loadedProjectsA = createProjectA projects loader (fun () -> loadedCalls <- loadedCalls + 1)

        let cache = ChangeableHashMap()

        let snapsA =
          loadedProjectsA
          |> AMap.mapAVal (fun _ (_,v) -> Snapshots.optionsToSnapshot  cache sourceTextFactory (Snapshots.createReferences cache sourceTextFactory loadedProjectsA) v)

        let snapshots = snapsA |> AMap.mapA (fun _ v -> v) |> AMap.force

        let libraryFile = Projects.MultiProjectScenario1.Console1.programFileIn dDir.DirectoryInfo
        printfn "Setting last write time for %s %A" libraryFile.FullName libraryFile.LastWriteTime

        do! File.WriteAllTextAsync(libraryFile.FullName, "let x = 1")
        libraryFile.Refresh()
        printfn "last write time for %s %A" libraryFile.FullName libraryFile.LastWriteTime


        let snapshots2 = snapsA |> AMap.mapA (fun _ v -> v) |> AMap.force

        let ls1 = snapshots |> HashMap.find ((Projects.MultiProjectScenario1.Library1.projectIn dDir.DirectoryInfo).FullName)
        let ls2 = snapshots2 |> HashMap.find ((Projects.MultiProjectScenario1.Library1.projectIn dDir.DirectoryInfo).FullName)

        Expect.equal ls1.ProjectFileName ls2.ProjectFileName "Project file name should be the same"
        Expect.equal ls1.ProjectId ls2.ProjectId "Project Id name should be the same"
        Expect.equal ls1.SourceFiles.Length 3 "Source files length should be 3"
        Expect.equal ls1.SourceFiles.Length ls2.SourceFiles.Length "Source files length should be the same"
        Expect.equal ls1.ReferencedProjects.Length ls2.ReferencedProjects.Length "Referenced projects length should be the same"
        Expect.equal ls1.ReferencedProjects.Length 0 "Referenced projects length should be 0"
        Expect.equal ls1.Stamp ls2.Stamp "Stamp should be the same"


        let cs1 = snapshots |> HashMap.find ((Projects.MultiProjectScenario1.Console1.projectIn dDir.DirectoryInfo).FullName)
        let cs2 = snapshots2 |> HashMap.find ((Projects.MultiProjectScenario1.Console1.projectIn dDir.DirectoryInfo).FullName)

        Expect.equal cs1.ProjectFileName cs2.ProjectFileName "Project file name should be the same"
        Expect.equal cs1.ProjectId cs2.ProjectId "Project Id name should be the same"
        Expect.equal cs1.SourceFiles.Length 3 "Source files length should be 3"
        Expect.equal cs1.SourceFiles.Length cs2.SourceFiles.Length "Source files length should be the same"
        Expect.equal cs1.ReferencedProjects.Length cs2.ReferencedProjects.Length "Referenced projects length should be the same"
        Expect.equal cs1.ReferencedProjects.Length 1 "Referenced projects length should be 1"
        Expect.notEqual cs1.Stamp cs2.Stamp "Stamp should not be the same"

      }

      testCaseAsync "Cached Adaptive Snapshot - MultiProject - Updating Source file in Library recreates Library and Console snapshot" <| asyncEx {
        let (loader : IWorkspaceLoader) = workspaceLoaderFactory toolsPath
        let sourceTextFactory : FsAutoComplete.ISourceTextFactory = FsAutoComplete.RoslynSourceTextFactory()
        let srcDir = Projects.MultiProjectScenario1.multiProjectScenario1Dir
        use dDir = Helpers.DisposableDirectory.From srcDir
        let projects =  Projects.MultiProjectScenario1.projects dDir.DirectoryInfo
        do! Dotnet.restoreAll projects

        let mutable loadedCalls = 0

        let loadedProjectsA = createProjectA projects loader (fun () -> loadedCalls <- loadedCalls + 1)

        let cache = ChangeableHashMap()

        let snapsA =
          loadedProjectsA
          |> AMap.mapAVal (fun _ (_,v) -> Snapshots.optionsToSnapshot  cache sourceTextFactory (Snapshots.createReferences cache sourceTextFactory loadedProjectsA) v)

        let snapshots = snapsA |> AMap.mapA (fun _ v -> v) |> AMap.force

        let libraryFile = Projects.MultiProjectScenario1.Library1.libraryFileIn dDir.DirectoryInfo
        printfn "Setting last write time for %s %A" libraryFile.FullName libraryFile.LastWriteTime

        do! File.WriteAllTextAsync(libraryFile.FullName, "let x = 1")
        libraryFile.Refresh()
        printfn "last write time for %s %A" libraryFile.FullName libraryFile.LastWriteTime


        let snapshots2 = snapsA |> AMap.mapA (fun _ v -> v) |> AMap.force

        let ls1 = snapshots |> HashMap.find ((Projects.MultiProjectScenario1.Library1.projectIn dDir.DirectoryInfo).FullName)
        let ls2 = snapshots2 |> HashMap.find ((Projects.MultiProjectScenario1.Library1.projectIn dDir.DirectoryInfo).FullName)

        Expect.equal ls1.ProjectFileName ls2.ProjectFileName "Project file name should be the same"
        Expect.equal ls1.ProjectId ls2.ProjectId "Project Id name should be the same"
        Expect.equal ls1.SourceFiles.Length 3 "Source files length should be 3"
        Expect.equal ls1.SourceFiles.Length ls2.SourceFiles.Length "Source files length should be the same"
        Expect.equal ls1.ReferencedProjects.Length ls2.ReferencedProjects.Length "Referenced projects length should be the same"
        Expect.equal ls1.ReferencedProjects.Length 0 "Referenced projects length should be 0"
        Expect.notEqual ls1.Stamp ls2.Stamp "Stamp should not be the same"


        let cs1 = snapshots |> HashMap.find ((Projects.MultiProjectScenario1.Console1.projectIn dDir.DirectoryInfo).FullName)
        let cs2 = snapshots2 |> HashMap.find ((Projects.MultiProjectScenario1.Console1.projectIn dDir.DirectoryInfo).FullName)

        Expect.equal cs1.ProjectFileName cs2.ProjectFileName "Project file name should be the same"
        Expect.equal cs1.ProjectId cs2.ProjectId "Project Id name should be the same"
        Expect.equal cs1.SourceFiles.Length 3 "Source files length should be 3"
        Expect.equal cs1.SourceFiles.Length cs2.SourceFiles.Length "Source files length should be the same"
        Expect.equal cs1.ReferencedProjects.Length cs2.ReferencedProjects.Length "Referenced projects length should be the same"
        Expect.equal cs1.ReferencedProjects.Length 1 "Referenced projects length should be 1"
        Expect.notEqual cs1.Stamp cs2.Stamp "Stamp should not be the same"

      }
    ]
]
