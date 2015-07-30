// include Fake lib
#r @"packages/FAKE/tools/FakeLib.dll"
open Fake
open Fake.Git
open Fake.ReleaseNotesHelper
open Fake.UserInputHelper
open Fake.ZipHelper
open Fake.AssemblyInfoFile
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
    

let buildDir = project + "/bin/Debug/"
let buildReleaseDir = project + "/bin/Release/"
let integrationTestDir = project + "/test/integration/"
let releaseArchive = "fsautocomplete.zip"

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

let runIntegrationTest (fn: string) : bool =
  let dir = Path.GetDirectoryName fn

  tracefn "Running FSIHelper '%s', '%s', '%s'"  FSIHelper.fsiPath dir fn
  let b, msgs = FSIHelper.executeFSI dir fn []
  if not b then
    for msg in msgs do
      traceError msg.Message
  b

Target "IntegrationTest" (fun _ ->
  let runOk =
   [ for i in integrationTests do
       yield runIntegrationTest i ]
   |> Seq.forall id
  if not runOk then
    failwith "Integration tests did not run successfully"
  else

    let ok, out, err =
      Git.CommandHelper.runGitCommand
                        "."
                        ("-c core.fileMode=false diff --exit-code " + integrationTestDir)
    if not ok then
      trace (toLines out)
      failwithf "Integration tests failed:\n%s" err
)

Target "UnitTest" (fun _ ->
    !! testAssemblies
    |> NUnit (fun p ->
        { p with
            DisableShadowCopy = true
            TimeOut = TimeSpan.FromMinutes 20.
            OutputFile = "TestResults.xml" })
)

Target "AssemblyInfo" (fun _ ->
  let fileName = project + "/AssemblyInfo.fs"
  CreateFSharpAssemblyInfo fileName
    [ Attribute.Title project
      Attribute.Product project
      Attribute.Description summary
      Attribute.Version release.AssemblyVersion
      Attribute.FileVersion release.AssemblyVersion ]
)

Target "ReleaseArchive" (fun _ ->
  Zip buildReleaseDir
      releaseArchive
      ( !! (buildReleaseDir + "/*.dll")
        ++ (buildReleaseDir + "/*.exe"))
)

#load "lib/Octokit.fsx"
open Octokit

Target "Release" (fun _ ->
    let user = getUserInput "Username: "
    let pw = getUserPassword "Password: "

    StageAll ""
    Git.Commit.Commit "" (sprintf "Bump version to %s" release.NugetVersion)
    Branches.push ""

    Branches.tag "" release.NugetVersion
    Branches.pushTag "" "origin" release.NugetVersion
    
    // release on github
    createClient user pw
    |> createDraft githubOrg project release.NugetVersion (release.SemVer.PreRelease <> None) release.Notes 
    |> uploadFile releaseArchive
    |> releaseDraft
    |> Async.RunSynchronously
)

Target "Clean" (fun _ ->
  CleanDirs [ buildDir; buildReleaseDir ]
  DeleteFile releaseArchive
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

"AssemblyInfo"
  ==> "BuildRelease"
  ==> "ReleaseArchive"
  ==> "Release"

RunTargetOrDefault "BuildDebug"

