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
                    >> Seq.fold(AMap.union) AMap.empty
                )
            yield! dependencies
    }

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
      loaded
      |> Seq.map(fun l -> l.ProjectFileName, AVal.constant l)
      // projects
      // |> List.map(fun p -> p.FullName, AVal.constant(p, loaded |> Seq.find(fun l -> l.ProjectFileName = p.FullName)))
    )
    |> AMap.ofAVal

  loadedProjectsA



module Snapshots =
  open FsAutoComplete
  open System.Threading

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

        // printfn "Snapshot %A" projectFileName
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
      let flattenASet (s: aset<aval<'a>>) = s |> ASet.mapA id |> ASet.toAVal |> AVal.map HashSet.toList
      makeAdaptiveFCSSnapshot
        projectFileName
        projectId
        (flattenASet sourceFiles)
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
        // let! text = File.ReadAllTextAsync fileName
        let fileName = normalizePath fileName
        use s = File.openFileStreamForReadingAsync fileName

        let! source = sourceTextFactory.Create(fileName, s) CancellationToken.None
        return source :> ISourceTextNew
      }
      // printfn "Creating source text for %s" fileName
      return ProjectSnapshot.FSharpFileSnapshot.Create(fileName, string writeTime.Ticks, getSource)
    }

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
    (cachedSnapshots: ChangeableHashMap<string,aval<FSharpProjectSnapshot>>)
    (sourceTextFactory: aval<ISourceTextFactory>)
    (loadedProjectsA: amap<string,aval<ProjectOptions>>)
    (p : ProjectOptions) =
    let deps =
      loadedProjectsA
      |> findAllDependenciesOfAndIncluding
        p.ProjectFileName
        (fun p -> p.ReferencedProjects |> List.map(_.ProjectFileName))
    deps
    |> AMap.filter(fun k _ -> k <> p.ProjectFileName)
    |> AMap.mapAVal(fun _ p -> aval {
      if p.ProjectFileName.EndsWith ".fsproj" then
        let snapshot = optionsToSnapshot cachedSnapshots sourceTextFactory (createReferences cachedSnapshots sourceTextFactory deps) p
        return! createReferencedProjectsFSharpReference (AVal.constant p.ResolvedTargetPath) snapshot
      else
        // TODO: Find if this needs to be adaptive or if `getStamp` in a PEReference will be enough
        return loadFromDotnetDll p
    })
    |> AMap.toASetValues

  and optionsToSnapshot
    (cachedSnapshots : ChangeableHashMap<_,_>)
    (sourceTextFactory: aval<ISourceTextFactory>)
    (mapReferences: ProjectOptions -> aset<aval<FSharpReferencedProjectSnapshot>>)
    (p : ProjectOptions) =

    // printfn "optionsToSnapshot - enter %A" p.ProjectFileName
    aval {
      match! cachedSnapshots |> AMap.tryFind p.ProjectFileName with
      | Some x ->
        // printfn "optionsToSnapshot - Cache hit  %A" p.ProjectFileName
        return! x
      | None ->
        // printfn "optionsToSnapshot - Cache miss %A" p.ProjectFileName
        let projectName = p.ProjectFileName
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

        transact <| fun () ->
          cachedSnapshots.Add(projectName, snap) |> ignore<_>

        return! snap
    }

  let createSnapshot
    (cachedSnapshots: ChangeableHashMap<string,aval<FSharpProjectSnapshot>>)
    (sourceTextFactory: aval<ISourceTextFactory>)
    (loadedProjectsA: amap<string,aval<ProjectOptions>>)
    (projectOptions: ProjectOptions) =
    let mapReferences = createReferences cachedSnapshots sourceTextFactory loadedProjectsA
    optionsToSnapshot cachedSnapshots sourceTextFactory mapReferences projectOptions


let snapshotTests loaders toolsPath =

  testList "SnapshotTests" [
  for (loaderName, workspaceLoaderFactory) in loaders do
    testSequencedGroup loaderName <|
    testList $"{loaderName}" [
      testCaseAsync "Simple Project Load" <| async {
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

      testCaseAsync "Adding nuget package should cause project load" <| async {
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

      testCaseAsync "Create snapshot" <| async {
        let (loader : IWorkspaceLoader) = workspaceLoaderFactory toolsPath
        let srcDir = Projects.Simple.simpleProjectDir
        let sourceTextFactory : FsAutoComplete.ISourceTextFactory = FsAutoComplete.RoslynSourceTextFactory()
        use dDir = Helpers.DisposableDirectory.From srcDir
        let projects =  Projects.Simple.projects dDir.DirectoryInfo
        do! Dotnet.restoreAll projects

        let mutable loadedCalls = 0

        let loadedProjectsA =
          createProjectA projects loader (fun () -> loadedCalls <- loadedCalls + 1)


        let snaps =
          let cache = ChangeableHashMap()
          loadedProjectsA |> AMap.mapAVal (fun _ v -> Snapshots.createSnapshot cache (AVal.constant sourceTextFactory) loadedProjectsA v)

        let snapshots = snaps |> AMap.force

        let (project, snapshotA) = snapshots |> Seq.head
        let snapshot = snapshotA |> AVal.force
        Expect.equal 1 loadedCalls "Loaded Projects should only get called 1 time"
        Expect.equal snapshot.ProjectFileName project "Snapshot should have the same project file name as the project"
        Expect.equal (Seq.length snapshot.SourceFiles) 3 "Snapshot should have the same number of source files as the project"
        Expect.equal (Seq.length snapshot.ReferencedProjects) 0 "Snapshot should have the same number of referenced projects as the project"
      }


      testCaseAsync "Cached Adaptive Snapshot - MultiProject - Updating nothing shouldn't cause recalculation" <| asyncEx {
        let (loader : IWorkspaceLoader) = workspaceLoaderFactory toolsPath
        let sourceTextFactory : FsAutoComplete.ISourceTextFactory = FsAutoComplete.RoslynSourceTextFactory()
        use dDir = Helpers.DisposableDirectory.From Projects.MultiProjectScenario1.multiProjectScenario1Dir
        let projects =  Projects.MultiProjectScenario1.projects dDir.DirectoryInfo
        do! Dotnet.restoreAll projects

        let mutable loadedCalls = 0

        let loadedProjectsA = createProjectA projects loader (fun () -> loadedCalls <- loadedCalls + 1)

        let cache = ChangeableHashMap()

        let snapsA =
          loadedProjectsA
          |> AMap.mapAVal (fun _ v -> Snapshots.createSnapshot cache (AVal.constant sourceTextFactory) loadedProjectsA v)

        let snapshots = snapsA |> AMap.mapA (fun _ v -> v) |> AMap.force

        let snapshots2 = snapsA |> AMap.mapA (fun _ v -> v) |> AMap.force

        let ls1 = snapshots |> HashMap.find ((Projects.MultiProjectScenario1.Library1.projectIn dDir.DirectoryInfo).FullName)
        let ls2 = snapshots2 |> HashMap.find ((Projects.MultiProjectScenario1.Library1.projectIn dDir.DirectoryInfo).FullName)

        Expect.equal ls1 ls2 "library should be the same"

        let cs1 = snapshots |> HashMap.find ((Projects.MultiProjectScenario1.Console1.projectIn dDir.DirectoryInfo).FullName)
        let cs2 = snapshots2 |> HashMap.find ((Projects.MultiProjectScenario1.Console1.projectIn dDir.DirectoryInfo).FullName)

        Expect.equal cs1 cs2 "console should be the same"
      }

      testCaseAsync "Cached Adaptive Snapshot - MultiProject - Updating Source file in Console recreates Console snapshot" <| asyncEx {
        let (loader : IWorkspaceLoader) = workspaceLoaderFactory toolsPath
        let sourceTextFactory : FsAutoComplete.ISourceTextFactory = FsAutoComplete.RoslynSourceTextFactory()
        use dDir = Helpers.DisposableDirectory.From Projects.MultiProjectScenario1.multiProjectScenario1Dir
        let projects =  Projects.MultiProjectScenario1.projects dDir.DirectoryInfo
        do! Dotnet.restoreAll projects

        let mutable loadedCalls = 0

        let loadedProjectsA = createProjectA projects loader (fun () -> loadedCalls <- loadedCalls + 1)

        let cache = ChangeableHashMap()

        let snapsA =
          loadedProjectsA
          |> AMap.mapAVal (fun _ v -> Snapshots.createSnapshot cache (AVal.constant sourceTextFactory) loadedProjectsA v)

        let snapshots = snapsA |> AMap.mapA (fun _ v -> v) |> AMap.force

        let libraryFile = Projects.MultiProjectScenario1.Console1.programFileIn dDir.DirectoryInfo
        // printfn "Setting last write time for %s %A" libraryFile.FullName libraryFile.LastWriteTime

        do! File.WriteAllTextAsync(libraryFile.FullName, "let x = 1")
        libraryFile.Refresh()
        // printfn "last write time for %s %A" libraryFile.FullName libraryFile.LastWriteTime


        let snapshots2 = snapsA |> AMap.mapA (fun _ v -> v) |> AMap.force

        let ls1 = snapshots |> HashMap.find ((Projects.MultiProjectScenario1.Library1.projectIn dDir.DirectoryInfo).FullName)
        let ls2 = snapshots2 |> HashMap.find ((Projects.MultiProjectScenario1.Library1.projectIn dDir.DirectoryInfo).FullName)

        Expect.equal ls1 ls2 "library should be the same"

        let cs1 = snapshots |> HashMap.find ((Projects.MultiProjectScenario1.Console1.projectIn dDir.DirectoryInfo).FullName)
        let cs2 = snapshots2 |> HashMap.find ((Projects.MultiProjectScenario1.Console1.projectIn dDir.DirectoryInfo).FullName)

        Expect.equal cs1.ProjectFileName cs2.ProjectFileName "Project file name should be the same"
        Expect.equal cs1.ProjectId cs2.ProjectId "Project Id name should be the same"
        Expect.equal cs1.SourceFiles.Length 3 "Source files length should be 3"
        Expect.equal cs1.SourceFiles.Length cs2.SourceFiles.Length "Source files length should be the same"
        Expect.equal cs1.ReferencedProjects.Length cs2.ReferencedProjects.Length "Referenced projects length should be the same"
        Expect.equal cs1.ReferencedProjects.Length 1 "Referenced projects length should be 1"
        let refLib1 = cs1.ReferencedProjects |> Seq.tryPick (fun x -> match x with | FSharpReferencedProjectSnapshot.FSharpReference(_, x) -> Some x | _ -> None) |> Option.get
        Expect.equal refLib1 ls1 "Referenced library should be the same as library snapshot"
        let refLib2 = cs2.ReferencedProjects |> Seq.tryPick (fun x -> match x with | FSharpReferencedProjectSnapshot.FSharpReference(_, x) -> Some x | _ -> None) |> Option.get
        Expect.equal refLib2 ls2 "Referenced library should be the same as library snapshot"
        Expect.equal refLib1 refLib2 "Referenced library in both snapshots should be the same as library did not change in this test"
        Expect.notEqual cs1.Stamp cs2.Stamp "Stamp should not be the same"

      }

      testCaseAsync "Cached Adaptive Snapshot - MultiProject - Updating Source file in Library recreates Library and Console snapshot" <| asyncEx {
        let (loader : IWorkspaceLoader) = workspaceLoaderFactory toolsPath
        let sourceTextFactory : FsAutoComplete.ISourceTextFactory = FsAutoComplete.RoslynSourceTextFactory()
        use dDir = Helpers.DisposableDirectory.From Projects.MultiProjectScenario1.multiProjectScenario1Dir
        let projects = Projects.MultiProjectScenario1.projects dDir.DirectoryInfo
        do! Dotnet.restoreAll projects

        let mutable loadedCalls = 0

        let loadedProjectsA = createProjectA projects loader (fun () -> loadedCalls <- loadedCalls + 1)

        let cache = ChangeableHashMap()

        let snapsA =
          loadedProjectsA
          |> AMap.mapAVal (fun _ v -> Snapshots.createSnapshot cache (AVal.constant sourceTextFactory) loadedProjectsA v)

        let snapshots = snapsA |> AMap.mapA (fun _ v -> v) |> AMap.force

        let libraryFile = Projects.MultiProjectScenario1.Library1.libraryFileIn dDir.DirectoryInfo
        // printfn "Setting last write time for %s %A" libraryFile.FullName libraryFile.LastWriteTime

        do! File.WriteAllTextAsync(libraryFile.FullName, "let x = 1")
        libraryFile.Refresh()
        // printfn "last write time for %s %A" libraryFile.FullName libraryFile.LastWriteTime

        let snapshots2 = snapsA |> AMap.mapA (fun _ v -> v) |> AMap.force

        let ls1 = snapshots |> HashMap.find ((Projects.MultiProjectScenario1.Library1.projectIn dDir.DirectoryInfo).FullName)
        let ls2 = snapshots2 |> HashMap.find ((Projects.MultiProjectScenario1.Library1.projectIn dDir.DirectoryInfo).FullName)

        Expect.notEqual ls1 ls2 "library should not be the same"
        Expect.equal ls1.ProjectFileName ls2.ProjectFileName "Project file name should be the same"
        Expect.equal ls1.ProjectId ls2.ProjectId "Project Id name should be the same"
        Expect.equal ls1.SourceFiles.Length 3 "Source files length should be 3"
        Expect.equal ls1.SourceFiles.Length ls2.SourceFiles.Length "Source files length should be the same"
        let ls1File = ls1.SourceFiles |> Seq.find (fun x -> x.FileName = libraryFile.FullName)
        let ls2File = ls2.SourceFiles |> Seq.find (fun x -> x.FileName = libraryFile.FullName)
        Expect.notEqual ls1File.Version ls2File.Version "Library source file version should not be the same"
        Expect.equal ls1.ReferencedProjects.Length ls2.ReferencedProjects.Length "Referenced projects length should be the same"
        Expect.equal ls1.ReferencedProjects.Length 0 "Referenced projects length should be 0"
        Expect.notEqual ls1.Stamp ls2.Stamp "Stamp should not be the same"

        let cs1 = snapshots |> HashMap.find ((Projects.MultiProjectScenario1.Console1.projectIn dDir.DirectoryInfo).FullName)
        let cs2 = snapshots2 |> HashMap.find ((Projects.MultiProjectScenario1.Console1.projectIn dDir.DirectoryInfo).FullName)

        Expect.equal cs1.ProjectFileName cs2.ProjectFileName "Project file name should be the same"
        Expect.equal cs1.ProjectId cs2.ProjectId "Project Id name should be the same"
        Expect.equal cs1.SourceFiles.Length 3 "Source files length should be 3"
        Expect.equal cs1.SourceFiles.Length cs2.SourceFiles.Length "Source files length should be the same"
        let consoleFile = Projects.MultiProjectScenario1.Console1.programFileIn dDir.DirectoryInfo
        let cs1File = cs1.SourceFiles |> Seq.find (fun x -> x.FileName = consoleFile.FullName)
        let cs2File = cs2.SourceFiles |> Seq.find (fun x -> x.FileName = consoleFile.FullName)
        Expect.equal cs1File.Version cs2File.Version "Console source file version should be the same"

        Expect.equal cs1.ReferencedProjects.Length cs2.ReferencedProjects.Length "Referenced projects length should be the same"
        Expect.equal cs1.ReferencedProjects.Length 1 "Referenced projects length should be 1"
        let refLib1 = cs1.ReferencedProjects |> Seq.tryPick (fun x -> match x with | FSharpReferencedProjectSnapshot.FSharpReference(_, x) -> Some x | _ -> None) |> Option.get
        Expect.equal refLib1 ls1 "Referenced library should be the same as library snapshot"
        let refLib2 = cs2.ReferencedProjects |> Seq.tryPick (fun x -> match x with | FSharpReferencedProjectSnapshot.FSharpReference(_, x) -> Some x | _ -> None) |> Option.get
        Expect.equal refLib2 ls2 "Referenced library should be the same as library snapshot"
        Expect.notEqual refLib1 refLib2 "Referenced library from different snapshot should not be the same as library source file changed"
        Expect.notEqual cs1.Stamp cs2.Stamp "Stamp should not be the same"

      }
    ]
]
