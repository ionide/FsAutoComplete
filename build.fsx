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
let releaseArchive = pkgsDir </> "fsautocomplete.zip"
let releaseArchiveNetCore = pkgsDir </> "fsautocomplete.netcore.zip"

let gitOwner = "fsharp"
let gitName = project
let gitHome = "https://github.com/" + gitOwner


Target.initEnvironment ()

Target.create "LspTest" (fun _ ->
  DotNet.exec
      id
      "run"
      """-c Release --no-build -p "./test/FsAutoComplete.Tests.Lsp/FsAutoComplete.Tests.Lsp.fsproj" -- --fail-on-focused-tests --summary"""
  |> fun r -> if not r.OK then failwithf "Errors while running LSP tests:\n%s" (r.Errors |> String.concat "\n\t")
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
    Directory.ensure "bin/release"
    Shell.cleanDirs [ "bin/release"; "bin/release_netcore" ]

    Shell.cleanDirs [ "bin/release_netcore" ]
    DotNet.publish (fun p ->
       { p with
           OutputPath = Some (__SOURCE_DIRECTORY__ </> "bin/release_netcore")
           Framework = Some "net5.0"
           Configuration = DotNet.BuildConfiguration.fromString configuration
           MSBuildParams = { MSBuild.CliArguments.Create () with Properties =  [ "SourceLinkCreate","true"; "Version", release.AssemblyVersion ] } }) "src/FsAutoComplete"


    Shell.cleanDirs [ "bin/release_as_tool" ]
    DotNet.publish (fun p ->
       { p with
           OutputPath = Some (__SOURCE_DIRECTORY__ </> "bin/release_as_tool")
           Configuration = DotNet.BuildConfiguration.fromString configuration
           MSBuildParams = { MSBuild.CliArguments.Create () with Properties =  [ "SourceLinkCreate","true"; "Version", release.AssemblyVersion; "PackAsTool", "true" ] } }) "src/FsAutoComplete"
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
         MSBuildParams = { MSBuild.CliArguments.Create () with Properties =  [ "SourceLinkCreate","true"; "Version", release.AssemblyVersion ] } }) "FsAutoComplete.sln"
)

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

    let files = !! (pkgsDir </> "*.*")

    let notes =
      release.Notes
      |> List.map (fun s -> "* " + s)

    // release on github
    let cl =
        client
        |> GitHub.draftNewRelease gitOwner gitName release.NugetVersion (release.SemVer.PreRelease <> None) notes
    (cl,files)
    ||> Seq.fold (fun acc e -> acc |> GitHub.uploadFile e)
    |> GitHub.publishDraft//releaseDraft
    |> Async.RunSynchronously
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
  ==> "Test"
  ==> "All"

"ReplaceFsLibLogNamespaces"
  ==> "LocalRelease"
  ==> "ReleaseArchive"
  ==> "ReleaseGitHub"
  ==> "Release"

"ReleaseArchive"
  ==> "All"

Target.runOrDefaultWithArguments "Build"
