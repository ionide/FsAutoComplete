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

if Environment.OSVersion.Platform = PlatformID.Win32NT then
  MSBuildDefaults <- {MSBuildDefaults with ToolsVersion = Some "14.0"}

Target "BuildDebug" (fun _ ->
  MSBuildDebug "" "Build" ["./FsAutoComplete.sln"]
  |> Log "Build-Output: "

  DotNetCli.Build (fun p ->
     { p with
         Configuration = "Debug"
         Project = "FsAutoComplete.netcore.sln" })
)

Target "BuildRelease" (fun _ ->
  MSBuildRelease "" "Rebuild" ["./FsAutoComplete.sln"]
  |> Log "Build-Output: "

  DotNetCli.Build (fun p ->
     { p with
         Project = "FsAutoComplete.netcore.sln"
         AdditionalArgs = [ "/p:SourceLinkCreate=true" ] })
)

let integrationTests =
  !! (integrationTestDir + "/**/*Runner.fsx")

type Mode = HttpMode | StdioMode
type FSACRuntime = NET | NETCoreSCD | NETCoreFDD
type IntegrationTestConfig = { Mode: Mode; Runtime: FSACRuntime }

let (|AnyNetcoreRuntime|_|) r =
  match r with
  | FSACRuntime.NETCoreSCD
  | FSACRuntime.NETCoreFDD -> Some ()
  | FSACRuntime.NET -> None

let isTestSkipped cfg fn =
  let file = Path.GetFileName(fn)
  let dir = Path.GetFileName(Path.GetDirectoryName(fn))

  let msbuildToolsVersion4Installed = (environVar "FSAC_TESTSUITE_MSBUILD_TOOLSVERSION_4_INSTALLED") <> "0"

  match cfg.Runtime, cfg.Mode, dir, file with
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
  | _, _, "DotNetSdk2.0", "InvalidProjectFileRunner.fsx"
  | _, _, "OldSdk", "InvalidProjectFileRunner.fsx" when not(isWindows) ->
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
  // fsproj in test suite use ToolsVersion 4 (VS2010) and is not always installed
  | AnyNetcoreRuntime, _, "ErrorTestsJson", "ErrorsRunner.fsx"
  | AnyNetcoreRuntime, _, "FindDeclarations", "FindDeclRunner.fsx"
  | AnyNetcoreRuntime, _, "MultiProj", "MultiProjRunner.fsx"
  | AnyNetcoreRuntime, _, "MultipleUnsavedFiles", "multunsavedRunner.fsx"
  | AnyNetcoreRuntime, _, "ParamCompletion", "ParamCompletionRunner.fsx"
  | AnyNetcoreRuntime, _, "ProjectReload", "Runner.fsx"
  | AnyNetcoreRuntime, _, "RobustCommands", "CompleteBadPositionRunner.fsx"
  | AnyNetcoreRuntime, _, "RobustCommands", "CompleteNoSuchFileRunner.fsx"
  | AnyNetcoreRuntime, _, "RobustCommands", "ParseNoSuchFileRunner.fsx"
  | AnyNetcoreRuntime, _, "SymbolUse", "SymbolUseRunner.fsx"
  | AnyNetcoreRuntime, _, "Test1Json", "Test1JsonRunner.fsx"
  | AnyNetcoreRuntime, _, "UncompiledReferencedProjects", "Runner.fsx" when not(msbuildToolsVersion4Installed) ->
    Some "The test use old fsproj, and msbuild tools version 4 is not installed"
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

let applyPaketLoadScriptWorkaround paketLoadScript =
    trace "apply workaround for bug https://github.com/fsprojects/Paket/issues/2868"
    let includeFile = Path.Combine(__SOURCE_DIRECTORY__ , paketLoadScript)
    trace (sprintf "File '%s' contents:" includeFile)
    File.ReadAllLines(includeFile)
    |> Array.iter (trace)
    trace "apply fix"
    File.ReadAllLines(includeFile)
    |> Array.map (fun s -> s.Replace("../../../../src/FsAutoComplete.Core.VerboseSdkHelper.netcore/.paket/load/net461/IntegrationTests/", ""))
    |> fun lines -> File.WriteAllLines(includeFile, lines)
    trace (sprintf "File '%s' contents:" includeFile)
    File.ReadAllLines(includeFile)
    |> Array.iter (trace)
    trace "applied workaround"

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

    [ @".paket/load/net461/IntegrationTests/Http.fs.fsx"
      @".paket/load/net461/IntegrationTests/System.Net.WebSockets.Client.fsx"
      @".paket/load/net461/IntegrationTests/System.Security.Cryptography.X509Certificates.fsx"
      @".paket/load/net461/IntegrationTests/System.Security.Cryptography.Algorithms.fsx"
      @".paket/load/net461/IntegrationTests/System.Security.Cryptography.Encoding.fsx"
      @".paket/load/net461/IntegrationTests/System.Security.Cryptography.Primitives.fsx" ]
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
  runall { Mode = StdioMode; Runtime = NET }
)

Target "IntegrationTestHttpMode" (fun _ ->
  trace "== Integration tests (http/net) =="
  runall { Mode = HttpMode; Runtime = NET }
)

Target "IntegrationTestStdioModeNetCore" (fun _ ->
  trace "== Integration tests (stdio/netcore) =="
  runall { Mode = StdioMode; Runtime = NETCoreFDD }
)

Target "IntegrationTestHttpModeNetCore" (fun _ ->
  trace "== Integration tests (http/netcore) =="
  runall { Mode = HttpMode; Runtime = NETCoreFDD }
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

    CopyFiles "bin/release"(
        !! (buildReleaseDir      + "/*.dll")
        ++ (buildReleaseDir      + "/*.exe")
        ++ (buildReleaseDir      + "/*.exe.config")
    )

    CleanDirs [ "bin/release_netcore" ]
    DotNetCli.Publish (fun p ->
       { p with
           Output = __SOURCE_DIRECTORY__ </> "bin/release_netcore"
           Project = "src/FsAutoComplete.netcore" })
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
  ==> "UnitTest"

"UnitTest" ==> "Test"
"IntegrationTest" ==> "Test"

"IntegrationTestStdioMode" ==> "IntegrationTest"
"IntegrationTestHttpMode" ==> "IntegrationTest"
"IntegrationTestStdioModeNetCore" =?> ("IntegrationTest", ((environVar "FSAC_TESTSUITE_NETCORE_MODE_STDIO") <> "0"))
"IntegrationTestHttpModeNetCore" =?> ("IntegrationTest", ((environVar "FSAC_TESTSUITE_NETCORE_MODE_HTTP") <> "0"))

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
