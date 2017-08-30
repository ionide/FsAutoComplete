#load "../TestHelpers.fsx"
open TestHelpers
open System.IO
open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let outputJson = "workspacepeek.json"
File.Delete outputJson

let doIt () =
  let sdkDir = DotnetCli.sdk1Dir ()

  use _sdk1 = DotnetCli.useSdk sdkDir

  runProcess __SOURCE_DIRECTORY__ "dotnet" "--info" |> ignore

  match runProcessCaptureOut __SOURCE_DIRECTORY__ "dotnet" "restore sample1/c1" with
  | NonExitCodeResult data ->
    data |> processResultLog "failed 'dotnet restore sample1/c1'" |> writeNormalizedOutput outputJson
  | _ ->
    let p = new FsAutoCompleteWrapper()

    p.workspacepeek (Path.Combine(__SOURCE_DIRECTORY__, "sample1")) 1

    p.send "quit\n"
    p.finalOutput ()
    |> writeNormalizedOutput outputJson

doIt ()
