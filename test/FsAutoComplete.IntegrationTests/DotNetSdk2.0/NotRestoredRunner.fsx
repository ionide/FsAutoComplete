#load "../TestHelpers.fsx"
open TestHelpers
open System.IO
open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let outputJson = "notrestored.json"
File.Delete outputJson

let doIt () =
  let sdkDir = DotnetCli.sdk2Dir ()
  
  use _sdk2 = DotnetCli.useSdk sdkDir

  runProcess __SOURCE_DIRECTORY__ "dotnet" "--info" |> ignore

  [ for c in ["c1";"l1"] do
      for outDir in ["obj";"bin"] do
        yield System.IO.Path.Combine("sample1", c, outDir) ]
  |> List.iter deleteDir

  let p = new FsAutoCompleteWrapper()

  p.project "sample1/c1/c1.fsproj"

  p.quit()
  p.finalOutput ()
  |> writeNormalizedOutput outputJson

doIt ()
