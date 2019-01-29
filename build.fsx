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
let releaseArchive = "bin" </> "pkgs" </> "fsautocomplete.zip"
let releaseArchiveNetCore = "bin" </> "pkgs" </> "fsautocomplete.netcore.zip"

// Pattern specifying assemblies to be tested using NUnit
let testAssemblies = "**/bin/*/*Tests*.dll"

Target "BuildDebug" (fun _ ->
  DotNetCli.Build (fun p ->
     { p with
         Project = "FsAutoComplete.sln"
         Configuration = "Debug"
         AdditionalArgs = [ "/p:SourceLinkCreate=true" ] })
)

Target "BuildRelease" (fun _ ->
  DotNetCli.Build (fun p ->
     { p with
         Project = "FsAutoComplete.sln"
         AdditionalArgs = [ "/p:SourceLinkCreate=true" ] })
)

let integrationTests =
  !! (integrationTestDir + "/**/*Runner.fsx")
  -- (integrationTestDir + "/DotNetCore*/*.*")
  -- (integrationTestDir + "/DotNetSdk*/*.*")

type Mode = HttpMode | StdioMode
type FSACRuntime = NET | NETCoreSCD | NETCoreFDD
type IntegrationTestConfig = { Mode: Mode; Runtime: FSACRuntime }

let (|AnyNetcoreRuntime|_|) r =
  match r with
  | FSACRuntime.NETCoreSCD
  | FSACRuntime.NETCoreFDD -> Some ()
  | FSACRuntime.NET -> None

let isTestSkipped cfg (fn: string) =
  let file = Path.GetFileName(fn)
  let dir = Path.GetFileName(Path.GetDirectoryName(fn))

  let msbuildToolsVersion4Installed = (environVar "FSAC_TESTSUITE_MSBUILD_TOOLSVERSION_4_INSTALLED") = "1"

  match cfg.Runtime, cfg.Mode, dir, file with
  // known failure. lint fails because a binding redirect over FParsec initializing FSharpLint
  | FSACRuntime.NET, _, "LinterWithOptions", "Runner.fsx"
  | FSACRuntime.NET, _, "Linter", "Runner.fsx" ->
    Some "known failure. lint fails because a binding redirect over FParsec initializing FSharpLint "
  // stdio and http
  | _, _, "ProjectCache", "Runner.fsx" ->
    Some "fails, ref https://github.com/fsharp/FsAutoComplete/issues/198"
  | AnyNetcoreRuntime, _, "DotNetCoreCrossgenWithNetFx", "Runner.fsx" ->
    Some "DotnetCore (sdk 1.0) tests cannot specify the dotnet sdk to use (1.0), and wrongly fallback to 2.0 in tests because is the one running FSAC. related to https://github.com/fsharp/FsAutoComplete/issues/213"
  | _, _, "DotNetCoreCrossgenWithNetFx", "Runner.fsx"
  | _, _, "DotNetSdk2.0CrossgenWithNetFx", "Runner.fsx" ->
    match isWindows, environVar "FSAC_TESTSUITE_CROSSGEN_NETFX" with
    | true, _ -> None //always run it on windows
    | false, "1" -> None //force run on mono
    | false, _ -> Some "not supported on this mono version" //by default skipped on mono
//  | _, _, "DotNetSdk2.0", "InvalidProjectFileRunner.fsx"
  | AnyNetcoreRuntime, _, "OldSdk", "InvalidProjectFileRunner.fsx" when not(isWindows) ->
    Some "the regex to normalize output fails. mono/.net divergence?" //by default skipped on mono
  // http
  | _, HttpMode, "RobustCommands", "NoSuchCommandRunner.fsx" ->
    Some "invalid command is 404 in http"
  | _, HttpMode, "Colorizations", "Runner.fsx" ->
    Some "not supported in http"
  | _, HttpMode, "OutOfRange", "OutOfRangeRunner.fsx" ->
    Some "dunno why diverge"
  | _, HttpMode, "ProjectReload", "Runner.fsx" ->
    Some "probably ok, is a notification"
  // .net core based fsac
  | AnyNetcoreRuntime, _, "DotNetCore", "AppAndLibRunner.fsx"
  | AnyNetcoreRuntime, _, "DotNetCoreCrossgen", "Runner.fsx"
  | AnyNetcoreRuntime, _, "DotNetCoreWithOtherDotnetLang", "FSharpOuterRunner.fsx" ->
    Some "DotnetCore (sdk 1.0) tests cannot specify the dotnet sdk to use (1.0), and wrongly fallback to 2.0 in tests because is the one running FSAC. related to https://github.com/fsharp/FsAutoComplete/issues/213"
  | AnyNetcoreRuntime, _, "NoFSharpCoreReference", "Runner.fsx" ->
    Some "know failure, the FSharp.Core is not added if not in the fsc args list"
  // by default others are enabled
  | _ -> None

let runIntegrationTest cfg (fn: string) : bool =
  let dir = Path.GetDirectoryName fn

  match isTestSkipped cfg fn with
  | Some msg ->
    tracefn "Skipped '%s' reason: %s"  fn msg
    true
  | None ->
    let runtime =
      match cfg.Runtime with
      | FSACRuntime.NET -> ""
      | FSACRuntime.NETCoreSCD -> "--define:FSAC_TEST_EXE_NETCORE_SCD"
      | FSACRuntime.NETCoreFDD -> "--define:FSAC_TEST_EXE_NETCORE"
    let mode =
      match cfg.Mode with
      | HttpMode -> "--define:FSAC_TEST_HTTP"
      | StdioMode -> ""
    let fsiArgs = sprintf "%s %s %s" mode runtime fn
    let fsiPath = FSIHelper.fsiPath
    tracefn "Running fsi '%s %s' (from dir '%s')"  fsiPath fsiArgs dir
    let testExecution =
      try
        FileUtils.pushd dir

        let result, messages =
            ExecProcessRedirected (fun info ->
              info.FileName <- fsiPath
              info.Arguments <- fsiArgs
              info.WorkingDirectory <- dir
            ) (TimeSpan.FromMinutes(1.0))

        System.Threading.Thread.Sleep (TimeSpan.FromSeconds(1.0))

        Some (result, messages |> List.ofSeq)
      with ex ->
        tracefn "fsi failed with ex %A" ex
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

let applyPaketLoadScriptWorkaround paketLoadScript =
    trace "apply workaround for bug https://github.com/fsprojects/Paket/issues/2868"
    let includeFile = Path.Combine(__SOURCE_DIRECTORY__ , paketLoadScript)
    trace (sprintf "File '%s' contents:" includeFile)
    File.ReadAllLines(includeFile)
    |> Array.iter (trace)
    trace "apply fix"
    File.ReadAllLines(includeFile)
    |> Array.map (fun s -> s.Replace("../../../../src/FsAutoComplete.Core.VerboseSdkHelper/.paket/load/net461/IntegrationTests/", ""))
    |> fun lines -> File.WriteAllLines(includeFile, lines)
    trace (sprintf "File '%s' contents:" includeFile)
    File.ReadAllLines(includeFile)
    |> Array.iter (trace)
    trace "applied workaround"

let listAll cfg =
  let willRun, willSkip =
    integrationTests
    |> Seq.map (fun test -> test, isTestSkipped cfg test)
    |> List.ofSeq
    |> List.partition (fun (test, skipped) -> match skipped with
                                              | Some txt -> false
                                              | None -> true)

  printfn "=== Tests to Run ==="
  for (testName, _msg) in willRun do
    printfn "\t%s" testName

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

    [ @".paket/load/net471/IntegrationTests/Http.fs.fsx"
      @".paket/load/net471/IntegrationTests/System.Net.WebSockets.Client.fsx"
      @".paket/load/net471/IntegrationTests/System.Security.Cryptography.X509Certificates.fsx"
      @".paket/load/net471/IntegrationTests/System.Security.Cryptography.Algorithms.fsx"
      @".paket/load/net471/IntegrationTests/System.Security.Cryptography.Encoding.fsx"
      @".paket/load/net471/IntegrationTests/System.Security.Cryptography.Primitives.fsx" ]
    |> List.iter applyPaketLoadScriptWorkaround

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
  trace "== Integration tests (stdio/net) =="
  let cfg = { Mode = StdioMode; Runtime = NET }
  listAll cfg
  runall cfg
)

Target "IntegrationTestHttpMode" (fun _ ->
  trace "== Integration tests (http/net) =="
  let cfg = { Mode = HttpMode; Runtime = NET }
  listAll cfg
  runall cfg
)

Target "IntegrationTestStdioModeNetCore" (fun _ ->
  trace "== Integration tests (stdio/netcore) =="
  let cfg = { Mode = StdioMode; Runtime = NETCoreFDD }
  listAll cfg
  runall cfg
)

Target "IntegrationTestHttpModeNetCore" (fun _ ->
  trace "== Integration tests (http/netcore) =="
  let cfg = { Mode = HttpMode; Runtime = NETCoreFDD }
  listAll cfg
  runall cfg
)

// Target "UnitTest" (fun _ ->
//     trace "Running Unit tests."
//     !! testAssemblies
//     |> NUnit3 (fun p ->
//         { p with
//             ShadowCopy = true
//             TimeOut = TimeSpan.FromMinutes 10.
//             OutputDir = "TestResults.xml" })
//     trace "Done Unit tests."
// )



Target "AssemblyInfo" (fun _ ->
  let fileName = "src" </> project </> "AssemblyInfo.fs"
  let githash = Information.getCurrentSHA1 ""

  CreateFSharpAssemblyInfo fileName
    [ Attribute.Title project
      Attribute.Product project
      Attribute.Description summary
      Attribute.Version release.AssemblyVersion
      Attribute.FileVersion release.AssemblyVersion
      Attribute.Metadata("githash", githash) ]
)

Target "ReleaseArchive" (fun _ ->
    CleanDirs [ "bin/pkgs" ]
    ensureDirectory "bin/pkgs"

    !! "bin/release/*.*"
    |> Zip "bin/release" releaseArchive

    !! "bin/release_netcore/*.*"
    |> Zip "bin/release_netcore" releaseArchiveNetCore
)

Target "LocalRelease" (fun _ ->
    ensureDirectory "bin/release"
    CleanDirs [ "bin/release"; "bin/release_netcore" ]

    DotNetCli.Publish (fun p ->
       { p with
           Output = __SOURCE_DIRECTORY__ </> "bin/release"
           Framework = "net461"
           Project = "src/FsAutoComplete"
           AdditionalArgs = [ "/p:SourceLinkCreate=true" ]  })

    !! "packages/FSharp.Compiler.Service.ProjectCracker/utilities/net45/*.*"
    |> CopyFiles "bin/release"

    CleanDirs [ "bin/release_netcore" ]
    DotNetCli.Publish (fun p ->
       { p with
           Output = __SOURCE_DIRECTORY__ </> "bin/release_netcore"
           Framework = "netcoreapp2.1"
           Project = "src/FsAutoComplete"
           AdditionalArgs = [ "/p:SourceLinkCreate=true" ]  })
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
    |> uploadFiles [ releaseArchive; releaseArchiveNetCore ]
    |> releaseDraft
    |> Async.RunSynchronously
)

Target "Clean" (fun _ ->
  CleanDirs [ buildDir; buildReleaseDir ]
  DeleteFiles [ releaseArchive; releaseArchiveNetCore ]
)

Target "Build" id
Target "Test" id
Target "IntegrationTest" id
Target "All" id

"AssemblyInfo" ==> "BuildDebug"

"BuildDebug"
  ==> "Build"
  ==> "IntegrationTest"

"BuildDebug"
  ==> "Build"
  // ==> "UnitTest"

// "UnitTest" ==> "Test"
"IntegrationTest" ==> "Test"

"IntegrationTestStdioMode" ==> "IntegrationTest"
"IntegrationTestHttpMode" ==> "IntegrationTest"
"IntegrationTestStdioModeNetCore" ==> "IntegrationTest"
"IntegrationTestHttpModeNetCore" ==> "IntegrationTest"

"BuildDebug" ==> "All"
"Test" ==> "All"

"BuildRelease" ==> "LocalRelease"
"LocalRelease" ==> "ReleaseArchive"

"AssemblyInfo"
  ==> "BuildRelease"
  ==> "ReleaseArchive"
  ==> "Release"

"ReleaseArchive" ==> "All"

RunTargetOrDefault "BuildDebug"
