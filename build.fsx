// include Fake lib
#r @"packages/FAKE/tools/FakeLib.dll"
open Fake
open System
open System.IO
open System.Text.RegularExpressions

let buildDir = "./FSharp.AutoComplete/bin/Debug/"
let buildReleaseDir = "./FSharp.AutoComplete/bin/Release/"
let integrationTestDir = "./FSharp.AutoComplete/test/integration/"

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

Target "Clean" (fun _ ->
  CleanDirs [ buildDir; buildReleaseDir ]
)

Target "Build" id
Target "Test" id
Target "All" id

"BuildDebug"
  ==> "Build"
  ==> "IntegrationTest"

"IntegrationTest" ==> "Test"

"BuildDebug" ==> "All"
"Test" ==> "All"

RunTargetOrDefault "BuildDebug"

