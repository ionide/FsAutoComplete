#load "../TestHelpers.fsx"
open TestHelpers
open System.IO
open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
File.Delete "fsharpouter.json"

let doIt () =
  let sdkDir = DotnetCli.sdk2Dir ()
  
  use _sdk2 = DotnetCli.useSdk sdkDir

  runProcess __SOURCE_DIRECTORY__ "dotnet" "--info" |> ignore

  match runProcessCaptureOut __SOURCE_DIRECTORY__ "dotnet" "restore sampleo/c1" with
  | NonExitCodeResult data ->
    data |> processResultLog "failed 'dotnet restore sampleo/c1'" |> writeNormalizedOutput "output.json"
  | _ ->
    match runProcessCaptureOut __SOURCE_DIRECTORY__ "dotnet" "build sampleo/l1" with
    | NonExitCodeResult data ->
        data |> processResultLog "failed 'dotnet build sampleo/l1'" |> writeNormalizedOutput "output.json"
    | _ ->
      let p = new FsAutoCompleteWrapper()

      p.project "sampleo/c1/c1.fsproj"
      p.parse "sampleo/c1/Program.fs"

      p.send "quit\n"
      p.finalOutput ()
      |> writeNormalizedOutput "fsharpouter.json"

doIt ()
