// include Fake lib
#r @"packages/FAKE/tools/FakeLib.dll"
open Fake
open Fake.Git
open Fake.ReleaseNotesHelper
open Fake.ZipHelper
open Fake.AssemblyInfoFile
open System
open System.IO
open System.Text.RegularExpressions

let project = "FSharp.AutoComplete"
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



Target "BuildDebug" (fun _ ->
  MSBuildDebug buildDir "Build" ["./FSharp.AutoComplete.sln"]
  |> Log "Build-Output: "
)

Target "BuildRelease" (fun _ ->
  MSBuildRelease buildReleaseDir "Build" ["./FSharp.AutoComplete.sln"]
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

  // Normalize output files so that a simple
  // `git diff` will be clean if the tests passed.
  for fn in !! (dir + "/*.txt") ++ (dir + "/*.json") do
    let lines = File.ReadAllLines fn
    for i in [ 0 .. lines.Length - 1 ] do
      if Path.DirectorySeparatorChar = '/' then
        lines.[i] <- Regex.Replace(lines.[i],
                                   "/.*?FSharp.AutoComplete/test/(.*?(\"|$))",
                                   "<absolute path removed>/test/$1")
        lines.[i] <- Regex.Replace(lines.[i],
                                   "\"/[^\"]*?/([^\"/]*?\.dll\")",
                                    "\"<absolute path removed>/$1")
      else
        if Path.GetExtension fn = ".json" then
          lines.[i] <- Regex.Replace(lines.[i].Replace(@"\\", "/"),
                                     "[a-zA-Z]:/.*?FSharp.AutoComplete/test/(.*?(\"|$))",
                                     "<absolute path removed>/test/$1")
          lines.[i] <- Regex.Replace(lines.[i],
                                     "\"[a-zA-Z]:/[^\"]*?/([^\"/]*?\.dll\")",
                                     "\"<absolute path removed>/$1")
        else
          lines.[i] <- Regex.Replace(lines.[i].Replace('\\','/'),
                                     "[a-zA-Z]:/.*?FSharp.AutoComplete/test/(.*?(\"|$))",
                                     "<absolute path removed>/test/$1")

    // Write manually to ensure \n line endings on all platforms
    using (new StreamWriter(fn))
    <| fun f ->
        for line in lines do
          f.Write(line)
          f.Write('\n')
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
                        ("diff --exit-code " + integrationTestDir)
    if not ok then
      trace (toLines out)
      failwithf "Integration tests failed:\n%s" err
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

Target "ReleaseInstructions"
  (fun _ ->
   printfn "Go to https://github.com/fsharp/FSharp.AutoComplete/releases/new"
   printfn "Enter the following information:\n"
   printfn "\tTag version: %s" release.AssemblyVersion
   printfn "\tRelease title: %s" release.AssemblyVersion
   printfn "\tNotes:\n"
   for note in release.Notes do
     printfn "%s" note
   printfn "\n\nAttach the archive '%s'" releaseArchive
)

Target "Clean" (fun _ ->
  CleanDirs [ buildDir; buildReleaseDir ]
  DeleteFile releaseArchive
)

Target "Build" id
Target "Test" id
Target "All" id
Target "Release" id

"BuildDebug"
  ==> "Build"
  ==> "IntegrationTest"

"IntegrationTest" ==> "Test"

"BuildDebug" ==> "All"
"Test" ==> "All"

"AssemblyInfo"
  ==> "BuildRelease"
  ==> "ReleaseArchive"
  ==> "ReleaseInstructions"
  ==> "Release"

RunTargetOrDefault "BuildDebug"

