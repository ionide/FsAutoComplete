// include Fake lib
#r @"packages/FAKE/tools/FakeLib.dll"
open Fake
open Fake.Git
open Fake.ReleaseNotesHelper
open Fake.UserInputHelper
open Fake.ZipHelper
open Fake.AssemblyInfoFile
open Fake.Testing
open System
open System.IO
open System.Text.RegularExpressions

let githubOrg = "fsharp"
let project = "FsAutoComplete"
let summary = "A command line tool for interfacing with FSharp.Compiler.Service over a pipe."

// Read additional information from the release notes document
let releaseNotesData =
    File.ReadAllLines "RELEASE_NOTES.md"
    |> parseAllReleaseNotes

let release = List.head releaseNotesData


let buildDir = "src" </> project </> "bin" </> "Debug"
let buildReleaseDir = "src" </> project </>  "bin" </> "Release"
let integrationTestDir = "test" </> "FsAutoComplete.IntegrationTests"
let releaseArchive = "fsautocomplete.zip"

let suaveSummary = "A Suave web server for interfacing with FSharp.Compiler.Service over a HTTP."
let suaveProject = "FsAutoComplete.Suave"
let suaveBuildDebugDir = "src" </> suaveProject </>  "bin" </> "Debug"
let suaveBuildReleaseDir = "src" </> suaveProject </> "bin" </> "Release"
let suaveReleaseArchive = "fsautocomplete.suave.zip"

// Pattern specifying assemblies to be tested using NUnit
let testAssemblies = "**/bin/*/*Tests*.dll"

Target "BuildDebug" (fun _ ->
  MSBuildDebug "" "Build" ["./FsAutoComplete.sln"]
  |> Log "Build-Output: "
)

Target "BuildRelease" (fun _ ->
  MSBuildRelease "" "Build" ["./FsAutoComplete.sln"]
  |> Log "Build-Output: "
)

let integrationTests =
  !! (integrationTestDir + "/**/*Runner.fsx")

let runIntegrationTest (num:int) (fsx: string) : bool =
  let dir = Path.GetDirectoryName fsx

  tracefn "Running FSIHelper - %i\n    '%s'\n    '%s'\n    '%s'\n" num FSIHelper.fsiPath dir fsx
  let success, msgs = FSIHelper.executeFSIWithScriptArgsAndReturnMessages fsx [|"--shadowcopyreferences"|]
  if not success then
    for msg in msgs do
      traceError msg.Message
  success

Target "IntegrationTest" (fun _ ->
  let runOk =
    integrationTests |> Array.ofSeq
    |> Array.Parallel.mapi (fun idx fsx -> 
        let dir = Path.GetDirectoryName fsx   
        System.Environment.CurrentDirectory <- dir
        runIntegrationTest idx fsx
    )
    |> Seq.forall id

  if not runOk then
    failwith "Integration tests did not run successfully"
  else
    let ok, out, err =
      Git.CommandHelper.runGitCommand
                        __SOURCE_DIRECTORY__
                        ("-c core.fileMode=false diff --exit-code " + integrationTestDir)
    if not ok then
      trace (toLines out)
      failwithf "Integration tests failed:\n%s" err
)

Target "UnitTest" (fun _ ->
    !! testAssemblies
    |> NUnit3 (fun p ->
        { p with
            ShadowCopy = true
            TimeOut = TimeSpan.FromMinutes 20.
            OutputDir = "TestResults.xml" })
)



Target "AssemblyInfo" (fun _ ->
  let fileName = "src" </> project </> "AssemblyInfo.fs"
  CreateFSharpAssemblyInfo fileName
    [ Attribute.Title project
      Attribute.Product project
      Attribute.Description summary
      Attribute.Version release.AssemblyVersion
      Attribute.FileVersion release.AssemblyVersion ]
)

Target "SuaveAssemblyInfo" (fun _ ->
  let fileName = "src" </> suaveProject </> "AssemblyInfo.fs"
  CreateFSharpAssemblyInfo fileName
    [ Attribute.Title suaveProject
      Attribute.Product suaveProject
      Attribute.Description suaveSummary
      Attribute.Version release.AssemblyVersion
      Attribute.FileVersion release.AssemblyVersion ]
)

Target "ReleaseArchive" (fun _ ->
  Zip buildReleaseDir
      releaseArchive
      ( !! (buildReleaseDir + "/*.dll")
        ++ (buildReleaseDir + "/*.exe")
        ++ (buildReleaseDir + "/*.exe.config"))
)

Target "SuaveReleaseArchive" (fun _ ->
  Zip suaveBuildReleaseDir
      suaveReleaseArchive
      ( !! (suaveBuildReleaseDir + "/*.dll")
        ++ (suaveBuildReleaseDir + "/*.exe")
        ++ (suaveBuildReleaseDir + "/*.exe.config"))
)

Target "LocalRelease" (fun _ ->
    ensureDirectory "bin/release"
    CopyFiles "bin/release"(   
        !! (buildReleaseDir      + "/*.dll")
        ++ (buildReleaseDir      + "/*.exe")
        ++ (buildReleaseDir      + "/*.exe.config")
        ++ (suaveBuildReleaseDir + "/*.dll")
        ++ (suaveBuildReleaseDir + "/*.exe")
        ++ (suaveBuildReleaseDir + "/*.exe.config")
    )
)


#load "paket-files/fsharp/FAKE/modules/Octokit/Octokit.fsx"
open Octokit

Target "Release" (fun _ ->
    let user = getUserInput "Username: "
    let pw = getUserPassword "Password: "
    let remote =
      Git.CommandHelper.getGitResult "" "remote -v"
      |> Seq.filter (fun (s: string) -> s.EndsWith("(push)"))
      |> Seq.tryFind (fun (s: string) -> s.Contains(githubOrg + "/" + project))
      |> function None -> "https://github.com/" + githubOrg + "/" + project
                | Some (s: string) ->  s.Split().[0]

    StageAll ""
    Git.Commit.Commit "" (sprintf "Bump version to %s" release.NugetVersion)
    Branches.pushBranch "" remote (Information.getBranchName "")

    Branches.tag "" release.NugetVersion
    Branches.pushTag "" remote release.NugetVersion

    // release on github
    createClient user pw
    |> createDraft githubOrg project release.NugetVersion (release.SemVer.PreRelease <> None) release.Notes
    |> uploadFile releaseArchive
    |> uploadFile suaveReleaseArchive
    |> releaseDraft
    |> Async.RunSynchronously
)

Target "Clean" (fun _ ->
  CleanDirs [ buildDir; buildReleaseDir; suaveBuildDebugDir; suaveBuildReleaseDir ]
  DeleteFiles [releaseArchive; suaveReleaseArchive ]
)

Target "Build" id
Target "Test" id
Target "All" id

"BuildDebug"
  ==> "Build"
  ==> "IntegrationTest"

"BuildDebug"
  ==> "Build"
  ==> "UnitTest"

"IntegrationTest" ==> "Test"
"UnitTest" ==> "Test"

"BuildDebug" ==> "All"
"Test" ==> "All"

"BuildRelease"
    ==> "LocalRelease"

"AssemblyInfo"
  ==> "BuildRelease"
  ==> "ReleaseArchive"
  ==> "Release"

"SuaveAssemblyInfo"
  ==> "BuildRelease"
  ==> "SuaveReleaseArchive"
  ==> "Release"

RunTargetOrDefault "BuildDebug"
