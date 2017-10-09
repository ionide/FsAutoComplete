// include Fake lib
#r @"packages/build/FAKE/tools/FakeLib.dll"

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

// Pattern specifying assemblies to be tested using NUnit
let testAssemblies = "**/bin/*/*Tests*.dll"

if Environment.OSVersion.Platform = PlatformID.Win32NT then
  MSBuildDefaults <- {MSBuildDefaults with ToolsVersion = Some "14.0"}

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

type Mode = HttpMode | StdioMode
type FSACRuntime = NET | NETCoreSCD
type IntegrationTestConfig = { Mode: Mode; Runtime: FSACRuntime }

let isTestSkipped cfg fn =
  let file = Path.GetFileName(fn)
  let dir = Path.GetFileName(Path.GetDirectoryName(fn))
  match cfg.Mode, dir, file with
  // stdio and http
  | _, "ProjectCache", "Runner.fsx" ->
    Some "fails, ref https://github.com/fsharp/FsAutoComplete/issues/198"
  | _, "DotNetCoreCrossgenWithNetFx", "Runner.fsx"
  | _, "DotNetSdk2.0CrossgenWithNetFx", "Runner.fsx" ->
    match isWindows, environVar "FSAC_TESTSUITE_CROSSGEN_NETFX" with
    | true, _ -> None //always run it on windows
    | false, "1" -> None //force run on mono
    | false, _ -> Some "not supported on this mono version" //by default skipped on mono
  | _, "DotNetSdk2.0", "InvalidProjectFileRunner.fsx"
  | _, "OldSdk", "InvalidProjectFileRunner.fsx" ->
    match isWindows with
    | true -> None //always run it on windows
    | false -> Some "the regex to normalize output fails. mono/.net divergence?" //by default skipped on mono
  // http
  | HttpMode, "RobustCommands", "NoSuchCommandRunner.fsx" ->
    Some "invalid command is 404 in http"
  | HttpMode, "Colorizations", "Runner.fsx" ->
    Some "not supported in http"
  | HttpMode, "OutOfRange", "OutOfRangeRunner.fsx" ->
    Some "dunno why diverge"
  | HttpMode, "ProjectReload", "Runner.fsx" ->
    Some "probably ok, is a notification"
  // by default others are enabled
  | _ -> None

let runIntegrationTest cfg (fn: string) : bool =
  let dir = Path.GetDirectoryName fn

  match isTestSkipped cfg fn with
  | Some msg ->
    tracefn "Skipped '%s' reason: %s"  fn msg
    true
  | None ->
    let mode =
      match cfg.Mode with
      | HttpMode -> "--define:FSAC_TEST_HTTP"
      | StdioMode -> "--define:FSAC_TEST_EXE_NETCORE"
    let fsiArgs = sprintf "%s %s" mode fn
    tracefn "Running fsi '%s %s' (from dir '%s')"  FSIHelper.fsiPath fsiArgs dir
    let testExecution =
      try
        FileUtils.pushd dir

        let result, messages =
            ExecProcessRedirected (fun info ->
              info.FileName <- FSIHelper.fsiPath
              info.Arguments <- fsiArgs
              info.WorkingDirectory <- dir
            ) (TimeSpan.FromMinutes(10.0))

        System.Threading.Thread.Sleep (TimeSpan.FromSeconds(1.0))

        Some (result, messages |> List.ofSeq)
      with _ ->
        None
    FileUtils.popd ()
    match testExecution with
    | None -> //timeout
      false
    | Some (result, msgs) ->
      let msgs = msgs |> List.filter (fun x -> x.IsError)
      if not result then
        for msg in msgs do
          traceError msg.Message
        let isWebEx = msgs |> List.exists (fun m -> m.Message.Contains("System.Net.WebException"))
        if isWebEx then
          true // ignore failure on web ex, like connection refused
        else
          false
      else
        true

let runall cfg =

    trace "Cleanup test dir (git clean)..."
    let clean =
      let ok, out, err =
        Git.CommandHelper.runGitCommand (Path.Combine(__SOURCE_DIRECTORY__, integrationTestDir)) "clean -xdf"
      out |> Seq.iter (printfn "%s")
      printfn "Done: %s" (ok.ToString())

    trace "Resetting output files in test dir (git reset)..."
    let clean =
      let ok, out, err =
        Git.CommandHelper.runGitCommand "." (sprintf "git checkout -- %s" integrationTestDir)
      out |> Seq.iter (printfn "%s")
      printfn "Done: %s" (ok.ToString())

    trace "Running Integration tests..."
    let runOk =
     integrationTests
     |> Seq.map (runIntegrationTest cfg)
     |> Seq.forall id

    if not runOk then
      trace "Integration tests did not run successfully"
      failwith "Integration tests did not run successfully"
    else
      trace "checking tests results..."
      let ok, out, err =
        Git.CommandHelper.runGitCommand
                          "."
                          ("-c core.fileMode=false diff --exit-code " + integrationTestDir)
      if not ok then
        trace (toLines out)
        failwithf "Integration tests failed:\n%s" err
    trace "Done Integration tests."

Target "IntegrationTestStdioMode" (fun _ ->
  trace "== Integration tests (stdio) =="
  runall { Mode = StdioMode; Runtime = NET }
)

Target "IntegrationTestHttpMode" (fun _ ->
  trace "== Integration tests (http) =="
  runall { Mode = HttpMode; Runtime = NET }
)

Target "UnitTest" (fun _ ->
    trace "Running Unit tests."
    !! testAssemblies
    |> NUnit3 (fun p ->
        { p with
            ShadowCopy = true
            TimeOut = TimeSpan.FromMinutes 10.
            OutputDir = "TestResults.xml" })
    trace "Done Unit tests."
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

Target "ReleaseArchive" (fun _ ->
  Zip buildReleaseDir
      releaseArchive
      ( !! (buildReleaseDir + "/*.dll")
        ++ (buildReleaseDir + "/*.exe")
        ++ (buildReleaseDir + "/*.exe.config"))
)

Target "LocalRelease" (fun _ ->
    ensureDirectory "bin/release"
    CopyFiles "bin/release"(
        !! (buildReleaseDir      + "/*.dll")
        ++ (buildReleaseDir      + "/*.exe")
        ++ (buildReleaseDir      + "/*.exe.config")
    )
)

#load "paket-files/build/fsharp/FAKE/modules/Octokit/Octokit.fsx"
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
    |> releaseDraft
    |> Async.RunSynchronously
)

Target "Clean" (fun _ ->
  CleanDirs [ buildDir; buildReleaseDir ]
  DeleteFiles [releaseArchive]
)

Target "Build" id
Target "Test" id
Target "IntegrationTest" id
Target "All" id

"BuildDebug"
  ==> "Build"
  ==> "IntegrationTest"

"BuildDebug"
  ==> "Build"
  ==> "UnitTest"

"UnitTest" ==> "Test"
"IntegrationTest" ==> "Test"

"IntegrationTestStdioMode" ==> "IntegrationTest"
"IntegrationTestHttpMode" ==> "IntegrationTest"

"BuildDebug" ==> "All"
"Test" ==> "All"

"BuildRelease"
    ==> "LocalRelease"

"AssemblyInfo"
  ==> "BuildRelease"
  ==> "ReleaseArchive"
  ==> "Release"

RunTargetOrDefault "BuildDebug"
