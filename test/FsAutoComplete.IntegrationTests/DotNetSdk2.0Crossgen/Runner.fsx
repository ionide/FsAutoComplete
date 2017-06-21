#load "../TestHelpers.fsx"
open TestHelpers
open System.IO
open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
File.Delete "output.json"

let doIt () =
  let sdkDir = DotnetCli.sdk2Dir ()
  
  use _sdk2 = DotnetCli.useSdk sdkDir

  runProcess __SOURCE_DIRECTORY__ "dotnet" "--info" |> ignore

  match runProcessCaptureOut __SOURCE_DIRECTORY__ "dotnet" "restore sample1/c1" with
  | NonExitCodeResult data ->
    data |> processResultLog "failed 'dotnet restore sample1/c1'" |> writeNormalizedOutput "output.json"
  | _ ->
    let p = new FsAutoCompleteWrapper()

    p.project "sample1/c1/c1.fsproj"
    p.parse "sample1/c1/Program.fs"

    p.send "quit\n"
    p.finalOutput ()
    |> writeNormalizedOutput "output.json"

doIt ()
