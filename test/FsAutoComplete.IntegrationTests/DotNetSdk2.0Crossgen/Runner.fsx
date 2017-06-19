#load "../TestHelpers.fsx"
open TestHelpers
open System.IO
open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
File.Delete "output.json"

let sdkDir =
  let isWindows = Environment.OSVersion.Platform = PlatformID.Win32NT
  let file = if isWindows then "dotnet-install.ps1" else "dotnet-install.sh"
  let repoDir = Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "..")
  let sdkDir = Path.Combine(repoDir, ".dotnetsdk2_0") |> Path.GetFullPath

  if not <| Directory.Exists(sdkDir) then
    printfn ".net core sdk not found in '%s'" sdkDir

    Directory.CreateDirectory(sdkDir) |> ignore
  
    printfn "downloading .net core sdk install script"
    use client = new System.Net.WebClient()
    let installScriptPath = Path.Combine(sdkDir, file)
    client.DownloadFile("https://raw.githubusercontent.com/dotnet/cli/release/2.0.0/scripts/obtain/" + file, installScriptPath)

    printfn "installing .net core sdk to '%s'" sdkDir

    if isWindows then
      let powershell script args = runProcess __SOURCE_DIRECTORY__ "powershell" (sprintf """-NoProfile -ExecutionPolicy unrestricted -File "%s" %s """ script args) |> ignore
      powershell installScriptPath  (sprintf "-InstallDir %s -Channel release/2.0.0" sdkDir)
    else
      let bash script args = runProcess __SOURCE_DIRECTORY__ "bash" (sprintf """ "%s" %s """ script args) |> ignore
      bash installScriptPath  (sprintf "--install-dir %s -channel release/2.0.0" sdkDir)

  sdkDir

let withPath dir f =
  let pathvar = System.Environment.GetEnvironmentVariable("PATH");
  try
    System.Environment.SetEnvironmentVariable("PATH", dir + Path.PathSeparator.ToString() + pathvar)
    f ()
  with _ ->
    System.Environment.SetEnvironmentVariable("PATH", pathvar)

let doIt () =
  runProcess __SOURCE_DIRECTORY__ "dotnet" "--info" |> ignore

  match runProcess __SOURCE_DIRECTORY__ "dotnet" "restore sample1/c1" with
  | 0 -> ()
  | err ->
    let msg = sprintf "failure during 'dotnet restore sample1/c1' with error %i" err
    msg |> writeNormalizedOutput "output.json"
    failwith msg

  let p = new FsAutoCompleteWrapper()

  p.project "sample1/c1/c1.fsproj"
  p.parse "sample1/c1/Program.fs"

  p.send "quit\n"
  p.finalOutput ()
  |> writeNormalizedOutput "output.json"

withPath sdkDir doIt
