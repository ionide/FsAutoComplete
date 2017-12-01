#load "../TestHelpers.fsx"
open TestHelpers
open System.IO
open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let outputJson = "workspaceload.json"
File.Delete outputJson

let doIt () =
  let sdkDir = DotnetCli.sdk2Dir ()

  use _sdk1 = DotnetCli.useSdk sdkDir

  runProcess __SOURCE_DIRECTORY__ "dotnet" "--info" |> ignore

  match runProcessCaptureOut __SOURCE_DIRECTORY__ "dotnet" "restore sample1/c1" with
  | NonExitCodeResult data ->
    data |> processResultLog "failed 'dotnet restore sample1/c1'" |> writeNormalizedOutput outputJson
  | _ ->
    let p = new FsAutoCompleteWrapper()

    p.notify ()

    p.workspaceload [ (Path.Combine(__SOURCE_DIRECTORY__, "sample1/c1/c1.fsproj")); (Path.Combine(__SOURCE_DIRECTORY__, "sample1/l1/l1.fsproj")) ]

    let expectedNotificationCount = 7
    p.awaitNotification (fun (o) -> o.Contains("""{"Status":"finished"}""")) expectedNotificationCount

    p.quit()
    p.finalOutput ()
    |> writeNormalizedOutput outputJson

doIt ()
