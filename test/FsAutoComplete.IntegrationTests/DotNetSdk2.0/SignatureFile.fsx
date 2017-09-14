#load "../TestHelpers.fsx"
open TestHelpers
open System.IO
open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
File.Delete "signfile.json"

let doIt () =
  let sdkDir = DotnetCli.sdk2Dir ()
  
  use _sdk2 = DotnetCli.useSdk sdkDir

  runProcess __SOURCE_DIRECTORY__ "dotnet" "--info" |> ignore

  match runProcessCaptureOut __SOURCE_DIRECTORY__ "dotnet" "restore sample2/l1" with
  | NonExitCodeResult data ->
    data |> processResultLog "failed 'dotnet restore sample2/l1'" |> writeNormalizedOutput "signfile.json"
  | _ ->
    let p = new FsAutoCompleteWrapper()

    p.project "sample2/l1/l1.fsproj"
    p.parse "sample2/l1/Module1.fsi"
    p.parse "sample2/l1/Module1.fs"

    p.send "quit\n"
    p.finalOutput ()
    |> writeNormalizedOutput "signfile.json"

doIt ()
