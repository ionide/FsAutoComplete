#r "paket: groupref build //"
#load ".fake/build.fsx/intellisense.fsx"


// open Fake
open Fake.Core
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.DotNet
open Fake.Core.TargetOperators
open Fake.Tools

let project = "FsAutoComplete"

let changeLogFile = "CHANGELOG.md"
let mutable changelogs = Changelog.load changeLogFile
let mutable currentRelease = changelogs.LatestEntry

let configuration = Environment.environVarOrDefault "configuration" "Release"

let buildDir = "src" </> project </> "bin" </> "Debug"
let buildReleaseDir = "src" </> project </>  "bin" </> "Release"
let pkgsDir = "bin" </> "pkgs"
let releaseArchiveNetCore = pkgsDir </> "fsautocomplete.netcore.zip"


Target.initEnvironment ()

let fsacAssemblies =
  "FsAutoComplete|FsAutoComplete.Core|FsAutoComplete.BackgroundServices|LanguageServerProtocol"

let packAsToolProp = "PackAsTool", "true"

Target.create "LspTest" (fun _ ->

  let msbuildCli : Fake.DotNet.MSBuild.CliArguments =
    { MSBuild.CliArguments.Create() with
          Properties =
            [ "AltCover", "true"
              // "AltCoverAssemblyFilter", fsacAssemblies
              "AltCoverAssemblyExcludeFilter", "System.Reactive|FSharp.Compiler.Service|Ionide.ProjInfo|FSharp.Analyzers|Analyzer|Humanizer|FSharp.Core|Dapper|FSharp.DependencyManager|FsAutoComplete.Tests.Lsp"
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

    !! (sprintf "bin/release_as_tool/fsautocomplete.%s.nupkg" currentRelease.AssemblyVersion)
    |> Shell.copy "bin/pkgs"
)

Target.create "LocalRelease" (fun _ ->
    Directory.ensure "bin/release_netcore"
    Shell.cleanDirs [ "bin/release_netcore" ]

    Shell.cleanDirs [ "bin/release_netcore" ]
    DotNet.publish (fun p ->
       { p with
           OutputPath = Some (__SOURCE_DIRECTORY__ </> "bin/release_netcore")
           Framework = Some "net6.0"
           Configuration = DotNet.BuildConfiguration.fromString configuration }) "src/FsAutoComplete"

    Directory.ensure "bin/release_as_tool"
    Shell.cleanDirs [ "bin/release_as_tool" ]
    DotNet.pack (fun p ->
       { p with
           OutputPath = Some (__SOURCE_DIRECTORY__ </> "bin/release_as_tool")
           Configuration = DotNet.BuildConfiguration.fromString configuration
           MSBuildParams = { MSBuild.CliArguments.Create () with Properties =  [ packAsToolProp ] } }) "src/FsAutoComplete"
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
         Configuration = DotNet.BuildConfiguration.fromString configuration }) "FsAutoComplete.sln"
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

Target.create "NoOp" ignore
Target.create "Test" ignore
Target.create "All" ignore
Target.create "Release" ignore

type SemverBump =
  | Major | Minor | Patch
  static member Combine l r =
    match l, r with
    | Major, _ | _, Major -> Major
    | Minor, _ | _, Minor -> Minor
    | _ -> Patch

let determineBump (currentBump: SemverBump) (c: Changelog.Change) =
  let thisChange =
    match c with
    | Changelog.Change.Added _ -> Minor
    | Changelog.Change.Removed _ -> Major
    | Changelog.Change.Changed _
    | Changelog.Change.Custom _ // TODO: handle?
    | Changelog.Change.Deprecated _
    | Changelog.Change.Fixed _
    | Changelog.Change.Security _ -> Patch
  SemverBump.Combine currentBump thisChange

let bumpVersion (ver: SemVerInfo) bump  =
  match bump with
  | Major -> { ver with Major = ver.Major + 1u; Minor = 0u; Patch = 0u; PreRelease = None; Original = None }
  | Minor -> { ver with Minor = ver.Minor + 1u; Patch = 0u; PreRelease = None; Original = None }
  | Patch -> { ver with Patch = ver.Patch + 1u; PreRelease = None; Original = None }

Target.create "PromoteUnreleasedToVersion" (fun _ ->
  match changelogs.Unreleased with
  | None -> failwith "No unreleased changes to be promoted"
  | Some unreleased ->
    let nextReleaseNumber =
      Trace.tracefn $"Determining bump for version %O{currentRelease.SemVer}"
      let bump =
        (Minor, unreleased.Changes)
        ||> List.fold determineBump
      Trace.tracefn $"Bump type is %O{bump}"
      bumpVersion changelogs.LatestEntry.SemVer bump
    Trace.tracefn $"Promoting unreleased changes to version {nextReleaseNumber}"
    changelogs <- Changelog.promoteUnreleased (string nextReleaseNumber) changelogs
    changelogs |> Changelog.save changeLogFile
    currentRelease <- changelogs.LatestEntry
)

Target.create "CreateVersionTag" (fun _ ->
  Git.Staging.stageFile "." changeLogFile |> ignore<_>
  Git.Commit.exec "." $"Promote changelog entry for %O{currentRelease.SemVer}"
  Git.CommandHelper.gitCommand "." $"tag v%O{currentRelease.SemVer}"
)

Target.create "Promote" ignore

"PromoteUnreleasedToVersion"
==> "CreateVersionTag"
==> "Promote"


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
  ==> "Release"

"ReleaseArchive"
  ==> "All"

Target.runOrDefaultWithArguments "Build"
