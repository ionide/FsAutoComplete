#r "paket: groupref build //"
#load ".fake/build.fsx/intellisense.fsx"


// open Fake
open Fake.Core
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.DotNet
open Fake.Core.TargetOperators
open Fake.Api
open Fake.Tools

let project = "FsAutoComplete"

// Read additional information from the release notes document
let release = ReleaseNotes.load "RELEASE_NOTES.md"

let configuration = Environment.environVarOrDefault "configuration" "Release"

let buildDir = "src" </> project </> "bin" </> "Debug"
let buildReleaseDir = "src" </> project </>  "bin" </> "Release"
let pkgsDir = "bin" </> "pkgs"
let releaseArchiveNetCore = pkgsDir </> "fsautocomplete.netcore.zip"

let gitOwner = "fsharp"
let gitName = project
let gitHome = "https://github.com/" + gitOwner

Target.initEnvironment ()

let fsacAssemblies =
  "FsAutoComplete|FsAutoComplete.Core|FsAutoComplete.BackgroundServices|LanguageServerProtocol"

let versionProp = "Version", release.AssemblyVersion
let packAsToolProp = "PackAsTool", "true"
let latestReleaseNotesProp = "PackageReleaseNotes", release.Notes |> String.concat "\n"

Target.create "LspTest" (fun _ ->

  let msbuildCli : Fake.DotNet.MSBuild.CliArguments =
    { MSBuild.CliArguments.Create() with
          Properties =
            [ "AltCover", "true"
              // "AltCoverAssemblyFilter", fsacAssemblies
              "AltCoverAssemblyExcludeFilter", "System.Reactive|FSharp.Compiler.Service|Ionide.ProjInfo|FSharp.Analyzers|Analyzer|Humanizer|FSharp.Core|Dapper|FSharp.DependencyManager|FsAutoComplete.Tests.Lsp"
              versionProp
              latestReleaseNotesProp
            ]
          }
  let testOpts (opts: DotNet.TestOptions) =
    { opts with Configuration = DotNet.BuildConfiguration.Release
                RunSettingsArguments = Some ("Expecto.fail-on-focused-tests=true")
                MSBuildParams = msbuildCli }
  DotNet.test testOpts "./test/FsAutoComplete.Tests.Lsp/FsAutoComplete.Tests.Lsp.fsproj"
)

Target.create "Coverage" (fun _ ->
  DotNet.exec id "reportgenerator" "-reports:test/FsAutoComplete.Tests.Lsp/coverage.xml -reporttypes:Html;HtmlSummary -targetdir:./coverage"
  |> fun r -> if not r.OK then failwithf "Errors while generating coverage report: %A" r.Errors
)

Target.create "ReleaseArchive" (fun _ ->
    Shell.cleanDirs [ "bin/pkgs" ]
    Directory.ensure "bin/pkgs"

    !! "bin/release_netcore/**/*"
    |> Zip.zip "bin/release_netcore" releaseArchiveNetCore

    !! (sprintf "bin/release_as_tool/fsautocomplete.%s.nupkg" release.AssemblyVersion)
    |> Shell.copy "bin/pkgs"
)

Target.create "LocalRelease" (fun _ ->
    Directory.ensure "bin/release_netcore"
    Shell.cleanDirs [ "bin/release_netcore" ]

    Shell.cleanDirs [ "bin/release_netcore" ]
    DotNet.publish (fun p ->
       { p with
           OutputPath = Some (__SOURCE_DIRECTORY__ </> "bin/release_netcore")
           Framework = Some "net5.0"
           Configuration = DotNet.BuildConfiguration.fromString configuration
           MSBuildParams = { MSBuild.CliArguments.Create () with Properties =  [ versionProp ] } }) "src/FsAutoComplete"

    Directory.ensure "bin/release_as_tool"
    Shell.cleanDirs [ "bin/release_as_tool" ]
    DotNet.pack (fun p ->
       { p with
           OutputPath = Some (__SOURCE_DIRECTORY__ </> "bin/release_as_tool")
           Configuration = DotNet.BuildConfiguration.fromString configuration
           MSBuildParams = { MSBuild.CliArguments.Create () with Properties =  [ versionProp; packAsToolProp; latestReleaseNotesProp ] } }) "src/FsAutoComplete"
)

Target.create "Clean" (fun _ ->
  Shell.cleanDirs [ buildDir; buildReleaseDir; pkgsDir ]
)

Target.create "Restore" (fun _ ->
    DotNet.restore id ""
)

Target.create "Build" (fun _ ->
  DotNet.build (fun p ->
     { p with
         Configuration = DotNet.BuildConfiguration.fromString configuration
         MSBuildParams = { MSBuild.CliArguments.Create () with Properties = [versionProp ] } }) "FsAutoComplete.sln"
)

let ensureGitUser user email =
    match Fake.Tools.Git.CommandHelper.runGitCommand "." "config user.name" with
    | true, [username], _ when username = user -> ()
    | _, _, _ ->
        Fake.Tools.Git.CommandHelper.directRunGitCommandAndFail "." (sprintf "config user.name %s" user)
        Fake.Tools.Git.CommandHelper.directRunGitCommandAndFail "." (sprintf "config user.email %s" email)

Target.create "ReplaceFsLibLogNamespaces" <| fun _ ->
  let replacements =
    [ "FsLibLog\\n", "FsAutoComplete.Logging\n"
      "FsLibLog\\.", "FsAutoComplete.Logging" ]
  replacements
  |> List.iter (fun (``match``, replace) ->
    (!! "paket-files/TheAngryByrd/FsLibLog/**/FsLibLog*.fs")
    |> Shell.regexReplaceInFilesWithEncoding ``match`` replace System.Text.Encoding.UTF8
  )

Target.create "ReleaseGitHub" (fun _ ->
    let remote =
        Git.CommandHelper.getGitResult "" "remote -v"
        |> Seq.filter (fun (s: string) -> s.EndsWith("(push)"))
        |> Seq.tryFind (fun (s: string) -> s.Contains(gitOwner + "/" + gitName))
        |> function None -> gitHome + "/" + gitName | Some (s: string) -> s.Split().[0]

    let user =
        match Environment.environVarOrDefault "github-user" "" with
        | s when not (String.isNullOrWhiteSpace s) -> s
        | _ -> UserInput.getUserInput "Username: "

    let email =
        match Environment.environVarOrDefault "user-email" "" with
        | s when not (String.isNullOrWhiteSpace s) -> s
        | _ -> UserInput.getUserInput "Email: "

    ensureGitUser user email

    Git.Staging.stageAll ""
    Git.Commit.exec "" (sprintf "Bump version to %s" release.NugetVersion)
    Git.Branches.pushBranch "" remote (Git.Information.getBranchName "")

    Git.Branches.tag "" release.NugetVersion
    Git.Branches.pushTag "" remote release.NugetVersion

    let client =
        let token =
            match Environment.environVarOrNone "github-token" with
            | Some s when not (String.isNullOrWhiteSpace s) -> s
            | _ -> UserInput.getUserInput "Token: "

        GitHub.createClientWithToken token

    let notes =
      release.Notes
      |> List.map (fun s -> "* " + s)

    let files = !! (pkgsDir </> "*.*")
    // release on github
    let cl =
        client
        |> GitHub.draftNewRelease gitOwner gitName release.NugetVersion (release.SemVer.PreRelease <> None) notes
    (cl,files)
    ||> Seq.fold (fun acc e -> GitHub.uploadFile e acc)
    |> GitHub.publishDraft
    |> Async.RunSynchronously
)

Target.create "PublishTool" (fun _ ->
  let apikey =
    match Environment.environVarOrNone "nuget-key" with
    | Some s when not (String.isNullOrWhiteSpace s) -> s
    | _ -> UserInput.getUserInput "Token: "

  let configurePush (p: DotNet.NuGetPushOptions) =
    { p with
        PushParams = {
          p.PushParams with
            ApiKey = Some apikey
            PushTrials = 3
            Source = Some "https://api.nuget.org/v3/index.json"
        } }

  !! (pkgsDir </> "*.nupkg")
  |> Seq.iter (DotNet.nugetPush configurePush)
)

Target.create "NoOp" ignore
Target.create "Test" ignore
Target.create "All" ignore
Target.create "Release" ignore

"Restore"
  ==> "ReplaceFsLibLogNamespaces"
  ==> "Build"


"Build"
  ==> "LspTest"
  ==> "Coverage"
  ==> "Test"
  ==> "All"

"ReplaceFsLibLogNamespaces"
  ==> "LocalRelease"
  ==> "ReleaseArchive"
  ==> "ReleaseGitHub"
  ==> "PublishTool"
  ==> "Release"

"ReleaseArchive"
  ==> "All"

Target.runOrDefaultWithArguments "Build"
