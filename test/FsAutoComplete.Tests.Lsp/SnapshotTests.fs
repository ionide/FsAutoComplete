module FsAutoComplete.Tests.SnapshotTests
open Expecto
open System.IO
open Ionide.ProjInfo
open FsAutoComplete.Utils
open FSharp.Data.Adaptive
open FsAutoComplete.Adaptive
open FsAutoComplete.ProjectWorkspace
open System
open FSharp.Compiler.Text
open Ionide.ProjInfo.ProjectLoader
open Ionide.ProjInfo.Types
open FSharp.Compiler.CodeAnalysis.ProjectSnapshot
open FSharp.Compiler.CodeAnalysis
open IcedTasks
open FSharp.UMX

module FcsRange = FSharp.Compiler.Text.Range
type FcsRange = FSharp.Compiler.Text.Range
type FcsPos = FSharp.Compiler.Text.Position



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
      |> Seq.map(fun l -> normalizePath l.ProjectFileName, l)
      // projects
      // |> List.map(fun p -> p.FullName, AVal.constant(p, loaded |> Seq.find(fun l -> l.ProjectFileName = p.FullName)))
    )
    |> AMap.ofAVal

  loadedProjectsA

let normalizeUntag = normalizePath >> UMX.untag


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
          Snapshots.createSnapshots AMap.empty (AVal.constant sourceTextFactory) loadedProjectsA

        let snapshots = snaps |> AMap.force

        let (project, (_, snapshotA)) = snapshots |> Seq.head
        let snapshot = snapshotA |> AVal.force
        Expect.equal 1 loadedCalls "Loaded Projects should only get called 1 time"
        Expect.equal snapshot.ProjectFileName (UMX.untag project) "Snapshot should have the same project file name as the project"
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

        let snapsA =
          Snapshots.createSnapshots AMap.empty (AVal.constant sourceTextFactory) loadedProjectsA

        let snapshots = snapsA |> AMap.mapA (fun _ (_,v) -> v) |> AMap.force

        let snapshots2 = snapsA |> AMap.mapA (fun _ (_,v) -> v) |> AMap.force

        let ls1 = snapshots |> HashMap.find (normalizePath (Projects.MultiProjectScenario1.Library1.projectIn dDir.DirectoryInfo).FullName)
        let ls2 = snapshots2 |> HashMap.find (normalizePath (Projects.MultiProjectScenario1.Library1.projectIn dDir.DirectoryInfo).FullName)

        Expect.equal ls1 ls2 "library should be the same"

        let cs1 = snapshots |> HashMap.find (normalizePath (Projects.MultiProjectScenario1.Console1.projectIn dDir.DirectoryInfo).FullName)
        let cs2 = snapshots2 |> HashMap.find (normalizePath (Projects.MultiProjectScenario1.Console1.projectIn dDir.DirectoryInfo).FullName)

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

        let snapsA =
          Snapshots.createSnapshots AMap.empty (AVal.constant sourceTextFactory) loadedProjectsA
        let snapshots = snapsA |> AMap.mapA (fun _ (_,v) -> v) |> AMap.force

        let consoleFile = Projects.MultiProjectScenario1.Console1.programFileIn dDir.DirectoryInfo
        // printfn "Setting last write time for %s %A" libraryFile.FullName libraryFile.LastWriteTime

        do! File.WriteAllTextAsync(consoleFile.FullName, "let x = 1")
        consoleFile.Refresh()
        // printfn "last write time for %s %A" libraryFile.FullName libraryFile.LastWriteTime


        let snapshots2 = snapsA |> AMap.mapA (fun _ (_,v) -> v) |> AMap.force

        let ls1 = snapshots |> HashMap.find (normalizePath (Projects.MultiProjectScenario1.Library1.projectIn dDir.DirectoryInfo).FullName)
        let ls2 = snapshots2 |> HashMap.find (normalizePath (Projects.MultiProjectScenario1.Library1.projectIn dDir.DirectoryInfo).FullName)

        Expect.equal ls1 ls2 "library should be the same"

        let cs1 = snapshots |> HashMap.find (normalizePath (Projects.MultiProjectScenario1.Console1.projectIn dDir.DirectoryInfo).FullName)
        let cs2 = snapshots2 |> HashMap.find (normalizePath (Projects.MultiProjectScenario1.Console1.projectIn dDir.DirectoryInfo).FullName)

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


        let snapsA =
          Snapshots.createSnapshots AMap.empty (AVal.constant sourceTextFactory) loadedProjectsA

        let snapshots = snapsA |> AMap.mapA (fun _ (_,v) -> v) |> AMap.force

        let libraryFile = Projects.MultiProjectScenario1.Library1.libraryFileIn dDir.DirectoryInfo
        // printfn "Setting last write time for %s %A" libraryFile.FullName libraryFile.LastWriteTime

        do! File.WriteAllTextAsync(libraryFile.FullName, "let x = 1")
        libraryFile.Refresh()
        // printfn "last write time for %s %A" libraryFile.FullName libraryFile.LastWriteTime

        let snapshots2 = snapsA |> AMap.mapA (fun _ (_,v) -> v) |> AMap.force

        let ls1 = snapshots |> HashMap.find (normalizePath (Projects.MultiProjectScenario1.Library1.projectIn dDir.DirectoryInfo).FullName)
        let ls2 = snapshots2 |> HashMap.find (normalizePath (Projects.MultiProjectScenario1.Library1.projectIn dDir.DirectoryInfo).FullName)

        Expect.notEqual ls1 ls2 "library should not be the same"
        Expect.equal ls1.ProjectFileName ls2.ProjectFileName "Project file name should be the same"
        Expect.equal ls1.ProjectId ls2.ProjectId "Project Id name should be the same"
        Expect.equal ls1.SourceFiles.Length 3 "Source files length should be 3"
        Expect.equal ls1.SourceFiles.Length ls2.SourceFiles.Length "Source files length should be the same"
        let ls1File = ls1.SourceFiles |> Seq.find (fun x -> x.FileName = normalizeUntag libraryFile.FullName)
        let ls2File = ls2.SourceFiles |> Seq.find (fun x -> x.FileName = normalizeUntag libraryFile.FullName)
        Expect.notEqual ls1File.Version ls2File.Version "Library source file version should not be the same"
        Expect.equal ls1.ReferencedProjects.Length ls2.ReferencedProjects.Length "Referenced projects length should be the same"
        Expect.equal ls1.ReferencedProjects.Length 0 "Referenced projects length should be 0"
        Expect.notEqual ls1.Stamp ls2.Stamp "Stamp should not be the same"

        let cs1 = snapshots |> HashMap.find (normalizePath (Projects.MultiProjectScenario1.Console1.projectIn dDir.DirectoryInfo).FullName)
        let cs2 = snapshots2 |> HashMap.find (normalizePath (Projects.MultiProjectScenario1.Console1.projectIn dDir.DirectoryInfo).FullName)

        Expect.equal cs1.ProjectFileName cs2.ProjectFileName "Project file name should be the same"
        Expect.equal cs1.ProjectId cs2.ProjectId "Project Id name should be the same"
        Expect.equal cs1.SourceFiles.Length 3 "Source files length should be 3"
        Expect.equal cs1.SourceFiles.Length cs2.SourceFiles.Length "Source files length should be the same"
        let consoleFile = Projects.MultiProjectScenario1.Console1.programFileIn dDir.DirectoryInfo
        let cs1File = cs1.SourceFiles |> Seq.find (fun x -> x.FileName = normalizeUntag consoleFile.FullName)
        let cs2File = cs2.SourceFiles |> Seq.find (fun x -> x.FileName = normalizeUntag consoleFile.FullName)
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

      (*
        Depending on the tree structure of the project, certain things will cause a reload of the project
        and certain certain things will only cause a snapshot to be updated.

        Also depending on the structure and update only subgraphs should change and not the entire graph.
        We need to reason about each scenario below and create multiple tests for them based on different structures.

        Also performance of bigger project graphs should be tested.
      *)

      // Add Project
      // - Should cause a project reload
      // Delete project
      // - Should cause a project reload
      // Rename Project
      // - Should practically be "Delete" then "Add"
      // - Unsure how this works with regards to updating all references to the project in other projects or solution files
      // Move project
      // - Should practically be "Delete" then "Add"
      // - Unsure how this works with regards to updating all references to the project in other projects or solution files

      // Add file
      // - Should cause a project reload as this is a project file change
      // Delete file
      // - Should cause a project reload as this is a project file change
      // Rename file
      // - Should practically be "Delete" then "Add"
      // Move file order
      // - Should practically be "Delete" then "Add"
      // Update file
      // - Should cause a snapshot update and all depending snapshots but not a project reload

      // Add package
      // - Should cause a project reload
      // Remove package
      // - Should cause a project reload
      // Update package
      // - Should cause a project reload

      // Add reference
      // - Should cause a project reload
      // Remove reference
      // - Should cause a project reload
      // Build referenced project that isn't fsproj
      // - Probably should only cause a snapshot update but might depend on what was done the csproj as well
    ]
]
