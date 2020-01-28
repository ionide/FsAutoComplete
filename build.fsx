#r "paket: groupref build //"
#load ".fake/build.fsx/intellisense.fsx"


// open Fake
open Fake.Core
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.DotNet
open Fake.Core.TargetOperators

let project = "FsAutoComplete"

// Read additional information from the release notes document
let release = ReleaseNotes.load "RELEASE_NOTES.md"

let configuration = Environment.environVarOrDefault "configuration" "Release"

let buildDir = "src" </> project </> "bin" </> "Debug"
let buildReleaseDir = "src" </> project </>  "bin" </> "Release"
let pkgsDir = "bin" </> "pkgs"
let releaseArchive = pkgsDir </> "fsautocomplete.zip"
let releaseArchiveNetCore = pkgsDir </> "fsautocomplete.netcore.zip"

Target.initEnvironment ()

Target.create "LspTest" (fun _ ->
  DotNet.exec
      // (fun p ->
      //     { p with
      //         Timeout = TimeSpan.FromMinutes 15. })
      id
      "run"
      """-c Release --no-build -p "./test/FsAutoComplete.Tests.Lsp/FsAutoComplete.Tests.Lsp.fsproj" -- --fail-on-focused-tests --debug"""
  |> ignore
)

Target.create "ReleaseArchive" (fun _ ->
    Shell.cleanDirs [ "bin/pkgs" ]
    Directory.ensure "bin/pkgs"

    !! "bin/release/**/*"
    |> Zip.zip "bin/release" releaseArchive

    !! "bin/release_netcore/**/*"
    |> Zip.zip "bin/release_netcore" releaseArchiveNetCore

    !! (sprintf "bin/release_as_tool/fsautocomplete.%s.nupkg" release.AssemblyVersion)
    |> Shell.copy "bin/pkgs"

    !! (sprintf "bin/project_system/ProjectSystem.%s.nupkg" release.AssemblyVersion)
    |> Shell.copy "bin/pkgs"
)

Target.create "LocalRelease" (fun _ ->
    Directory.ensure "bin/release"
    Shell.cleanDirs [ "bin/release"; "bin/release_netcore" ]

    DotNet.publish (fun p ->
       { p with
           OutputPath = Some (__SOURCE_DIRECTORY__ </> "bin/release")
           Framework = Some "net461"
           Configuration = DotNet.BuildConfiguration.fromString configuration
           MSBuildParams = { MSBuild.CliArguments.Create () with Properties =  [ "SourceLinkCreate","true"; "Version", release.AssemblyVersion ] } }) "src/FsAutoComplete"

    Shell.cleanDirs [ "bin/release_netcore" ]
    DotNet.publish (fun p ->
       { p with
           OutputPath = Some (__SOURCE_DIRECTORY__ </> "bin/release_netcore")
           Framework = Some "netcoreapp2.1"
           Configuration = DotNet.BuildConfiguration.fromString configuration
           MSBuildParams = { MSBuild.CliArguments.Create () with Properties =  [ "SourceLinkCreate","true"; "Version", release.AssemblyVersion ] } }) "src/FsAutoComplete"


    Shell.cleanDirs [ "bin/release_as_tool" ]
    DotNet.publish (fun p ->
       { p with
           OutputPath = Some (__SOURCE_DIRECTORY__ </> "bin/release_as_tool")
           Configuration = DotNet.BuildConfiguration.fromString configuration
           MSBuildParams = { MSBuild.CliArguments.Create () with Properties =  [ "SourceLinkCreate","true"; "Version", release.AssemblyVersion; "PackAsTool", "true" ] } }) "src/FsAutoComplete"


    Shell.cleanDirs [ "bin/project_system" ]
    DotNet.pack (fun p ->
       { p with
           OutputPath = Some ( __SOURCE_DIRECTORY__ </> "bin/project_system")
           Configuration = DotNet.BuildConfiguration.fromString configuration
           MSBuildParams = { MSBuild.CliArguments.Create () with Properties =  [ "SourceLinkCreate","true"; "Version", release.AssemblyVersion ] } }) "src/ProjectSystem"
)

Target.create "Clean" (fun _ ->
  Shell.cleanDirs [ buildDir; buildReleaseDir; pkgsDir ]
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

Target.create "NoOp" ignore
Target.create "Test" ignore
Target.create "All" ignore
Target.create "Release" ignore


"ReplaceFsLibLogNamespaces"
  ==> "Build"


"Build"
  ==> "LspTest"
  ==> "Test"
  ==> "All"

"Build"
  ==> "LocalRelease"
  ==> "ReleaseArchive"
  ==> "Release"

"ReleaseArchive"
  ==> "All"

Target.runOrDefaultWithArguments "Build"
