#load "../TestHelpers.fsx"
open TestHelpers
open System.IO
open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let outputJson = "invalidprojectfile.json"
File.Delete outputJson

let doIt () =
  let sdkDir = DotnetCli.sdk2Dir ()
  
  use _sdk2 = DotnetCli.useSdk sdkDir

  runProcess __SOURCE_DIRECTORY__ "dotnet" "--info" |> ignore

  let p = new FsAutoCompleteWrapper()

  //create the project.assets.json, otherwise the project error out as not restored
  Directory.CreateDirectory(Path.Combine(__SOURCE_DIRECTORY__, "sample3", "l1", "obj")) |> ignore
  File.Create(Path.Combine(__SOURCE_DIRECTORY__, "sample3", "l1", "obj", @"project.assets.json")) |> ignore

  p.project "sample3/l1/l1.fsproj"

  p.quit()
  p.finalOutput ()
  |> writeNormalizedOutput outputJson

doIt ()
