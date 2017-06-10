#load "../TestHelpers.fsx"
open TestHelpers
open System.IO
open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
File.Delete "output.json"

let sdkDir =
  let isWin = true
  let file = if isWin then "dotnet-install.ps1" else "dotnet-install.sh"
  let repoDir = Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "..")
  let sdkDir = Path.Combine(repoDir, ".dotnetsdk2_0") |> Path.GetFullPath

  if not <| Directory.Exists(sdkDir) then
    printfn ".net core sdk not found in '%s'" sdkDir

    Directory.CreateDirectory(sdkDir) |> ignore
  
    printfn "downloading .net core sdk install script"
    use client = new System.Net.WebClient()
    let installScriptPath = Path.Combine( __SOURCE_DIRECTORY__, ".dotnetsdk", file)
    client.DownloadFile("https://raw.githubusercontent.com/dotnet/cli/release/2.0.0/scripts/obtain/" + file, installScriptPath)

    printfn "installing .net core sdk to '%s'" sdkDir

    let powershell script args = runProcess __SOURCE_DIRECTORY__ "powershell" (sprintf """-NoProfile -ExecutionPolicy unrestricted -File "%s" %s """ script args) |> ignore
    powershell installScriptPath  (sprintf "-InstallDir %s -Channel release/2.0.0" sdkDir)

  sdkDir

let withPath dir f =
  let pathvar = System.Environment.GetEnvironmentVariable("PATH");
  try
    System.Environment.SetEnvironmentVariable("PATH", dir + Path.PathSeparator.ToString() + pathvar)
    f ()
  with _ ->
    System.Environment.SetEnvironmentVariable("PATH", pathvar)

let doIt () =
  runProcess __SOURCE_DIRECTORY__ "dotnet" "restore sample1/c1"
  |> ignore

  let p = new FsAutoCompleteWrapper()

  p.project "sample1/c1/c1.fsproj"
  p.parse "sample1/c1/Program.fs"

  p.send "quit\n"
  p.finalOutput ()
  |> writeNormalizedOutput "output.json"

withPath sdkDir doIt
