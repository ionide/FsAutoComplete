open System
open System.IO
open System.Diagnostics
open System.Text.RegularExpressions

#load "../../.paket/load/net471/IntegrationTests/Argu.fsx"
#load "../../.paket/load/net471/IntegrationTests/Newtonsoft.Json.fsx"

open Argu

type CLIArguments =
    | [<AltCommandLine("-c")>] Configuration of name:string
    | [<AltCommandLine("-f")>] Framework of tfm:string
    | [<AltCommandLine("-r")>] Runtime of rid:string
    | [<AltCommandLine("-pub")>] Published
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Configuration _ -> "The configuration. The default is 'Debug'."
            | Framework _ -> "The target framework. The default is 'netcoreapp2.1'"
            | Published -> "Test the exe published in '~/bin/' dirs instead"
            | Runtime _ -> "The target runtime. If set, it will test a self-contained deployment."

let fsxArgs =
  let rec getArgs args =
    match args with
    | "--" :: rest -> rest
    | _ :: tail -> getArgs tail
    | [] -> []

  fsi.CommandLineArgs |> List.ofArray |> getArgs

open Newtonsoft.Json

let (</>) a b = Path.Combine(a,b)

type IntegrationTestConfig =
  { Runtime: FSACRuntime;
    Configuration: string;
    Framework: string;
    Rid: string option;
    Published: bool;
    FsacExePath: string }
and FSACRuntime = NET | NETCoreSCD | NETCoreFDD

let testConfig =
    let parser = ArgumentParser.Create<CLIArguments>(programName = "fsac_test")

    let results = parser.Parse (Array.ofList fsxArgs)

    let configuration = results.GetResult (<@ Configuration @>, defaultValue = "Debug")
    let framework = results.GetResult (<@ Framework @>, defaultValue = "netcoreapp2.1")
    let rid = results.TryGetResult <@ Runtime @>
    let published = results.Contains <@ Published @>

    let fsacRuntime =
      if framework.StartsWith("netcoreapp") then
        match rid with
        | Some _ -> NETCoreSCD
        | None -> NETCoreFDD
      else
        NET // TODO support SCD too

    let fromRoot format =
        let makeAbs relativePath =
          IO.Path.Combine(__SOURCE_DIRECTORY__, relativePath)
          |> Path.GetFullPath
        Printf.ksprintf makeAbs format

    let fsacExePath =
      if published then
        match fsacRuntime with
        | FSACRuntime.NETCoreFDD ->
          fromRoot "../../bin/release_netcore/fsautocomplete.dll"
        | FSACRuntime.NETCoreSCD ->
          fromRoot "../../bin/release_netcore/fsautocomplete"
        | FSACRuntime.NET ->
          fromRoot "../../bin/release/fsautocomplete.exe"
      else
        match fsacRuntime with
        | FSACRuntime.NETCoreFDD ->
          fromRoot "../../src/FsAutoComplete/bin/%s/netcoreapp2.1/fsautocomplete.dll" configuration
        | FSACRuntime.NETCoreSCD ->
          fromRoot "../../src/FsAutoComplete/bin/%s/netcoreapp2.1/publish/fsautocomplete" configuration
        | FSACRuntime.NET ->
          fromRoot "../../src/FsAutoComplete/bin/%s/%s/fsautocomplete.exe" configuration framework

    { Runtime = fsacRuntime
      Configuration = configuration
      Framework = framework
      Rid = rid
      Published = published
      FsacExePath = fsacExePath }


let outputJsonForRuntime path =
  match testConfig.Runtime with
  | FSACRuntime.NETCoreFDD | FSACRuntime.NETCoreSCD ->
    System.IO.Path.ChangeExtension(path, ".netcore.json")
  | FSACRuntime.NET ->
    path

let fsacExePath () =
  testConfig.FsacExePath

let configureFSACArgs (startInfo: ProcessStartInfo) =
    let fileName, args =
      match testConfig.Runtime with
      | FSACRuntime.NETCoreFDD ->
          "dotnet", fsacExePath () + " --use-sdk-scripts"
      | FSACRuntime.NETCoreSCD ->
          fsacExePath (), "--use-sdk-scripts"
      | FSACRuntime.NET ->
          fsacExePath (), ""

    startInfo.FileName <- fileName
    startInfo.RedirectStandardOutput <- true
    startInfo.RedirectStandardError  <- true
    startInfo.RedirectStandardInput  <- true
    startInfo.UseShellExecute <- false
    startInfo.EnvironmentVariables.Add("FCS_ToolTipSpinWaitTime", "10000")
    startInfo.EnvironmentVariables.Add("FSAC_WORKSPACELOAD_DELAY", "2000")
    startInfo.Arguments <- args
    if Environment.GetEnvironmentVariable("FSAC_TESTSUITE_WAITDEBUGGER") = "1" then
      startInfo.Arguments <- sprintf "%s --wait-for-debugger" startInfo.Arguments

type FsAutoCompleteWrapperStdio() =

  let p = new System.Diagnostics.Process()
  let cachedOutput = new Text.StringBuilder()

  do
    configureFSACArgs p.StartInfo
    printfn "Starting %s %s" p.StartInfo.FileName p.StartInfo.Arguments
    p.Start () |> ignore

  static member ExePath () =
    fsacExePath ()

  member x.project (s: string) : unit =
    fprintf p.StandardInput "project \"%s\"\n" s

  member x.parse (s: string) : unit =
    let text = if IO.File.Exists s then IO.File.ReadAllText(s) else ""
    fprintf p.StandardInput "parse \"%s\" sync\n%s\n<<EOF>>\n" s text

  member x.parseContent (filename: string) (content: string) : unit =
    fprintf p.StandardInput "parse \"%s\" sync\n%s\n<<EOF>>\n" filename content

  member x.completion (fn: string) (lineStr:string)(line: int) (col: int) : unit =
    fprintf p.StandardInput "completion \"%s\" \"%s\" %d %d\n" fn lineStr line col

  member x.methods (fn: string) (lineStr: string)(line: int) (col: int) : unit =
    fprintf p.StandardInput "methods \"%s\" \"%s\" %d %d\n" fn lineStr line col

  member x.completionFilter (fn: string) (lineStr: string)(line: int) (col: int) (filter: string) : unit =
    fprintf p.StandardInput "completion \"%s\" \"%s\" %d %d filter=%s\n" fn lineStr line col filter

  member x.tooltip (fn: string) (lineStr: string) (line: int) (col: int) : unit =
    fprintf p.StandardInput "tooltip \"%s\" \"%s\" %d %d\n" fn lineStr line col

  member x.typesig (fn: string) (lineStr: string) (line: int) (col: int) : unit =
    fprintf p.StandardInput "typesig \"%s\" \"%s\" %d %d\n" fn lineStr line col

  member x.finddeclaration (fn: string) (lineStr: string) (line: int) (col: int) : unit =
    fprintf p.StandardInput "finddecl \"%s\" \"%s\" %d %d\n" fn lineStr line col

  member x.symboluse (fn: string) (lineStr: string) (line: int) (col: int) : unit =
    fprintf p.StandardInput "symboluse \"%s\" \"%s\" %d %d\n" fn lineStr line col

  member x.declarations (fn: string) : unit =
    fprintf p.StandardInput "declarations \"%s\"\n" fn

  member x.lint (fn: string) : unit =
    fprintf p.StandardInput "lint \"%s\"\n" fn

  member x.send (s: string) : unit =
    fprintf p.StandardInput "%s" s

  member x.workspacepeek (dir: string) (deep: int): unit =
    fprintf p.StandardInput "workspacepeek \"%s\" %i\n" dir deep

  member x.workspaceload (projects: string list): unit =
    fprintf p.StandardInput "workspaceload %s\n" (projects |> List.map (sprintf "\"%s\"") |> String.concat " ")

  member x.quit (): unit =
    x.send "quit\n"

  member x.notify () : unit =
    ()

  /// Wait for a single line to be output (one JSON message)
  /// Note that this line will appear at the *start* of output.json,
  /// so use carefully, and preferably only at the beginning.
  member x.waitForLine () : unit =
    cachedOutput.AppendLine(p.StandardOutput.ReadLine()) |> ignore

  member x.finalOutput () : string =
    let s = p.StandardOutput.ReadToEnd()
    let t = p.StandardError.ReadToEnd()
    p.WaitForExit()
    cachedOutput.ToString() + s + t

  member x.awaitNotification f times =
    let fiveSec = TimeSpan.FromSeconds(5.0)
    let rec waitOut times =
      printfn "waiting next notification"
      let s = p.StandardOutput.ReadLine()
      printfn "got: %s" s
      cachedOutput.AppendLine(s) |> ignore
      if f s then
        ()
      else
        if times > 0 then
          waitOut (times - 1)
        else
          ()
    waitOut times


let formatJson json =
    try
      let parsedJson = JsonConvert.DeserializeObject(json)
      JsonConvert.SerializeObject(parsedJson, Formatting.Indented)
    with _ ->
      json

type FsAutoCompleteWrapper = FsAutoCompleteWrapperStdio

let stripPackagesDir =
  let r = Regex("(..\/)*packages\/integrationtests\/.*\/(?<dllname>.*)\.dll")
  fun (s: string) -> r.Replace(s, "<absolute path removed>/$2.dll")

let writeNormalizedOutputWith additionalFn (fn: string) (s: string) =

  let driveLetterRegex = if Path.DirectorySeparatorChar  = '/' then "" else "[a-zA-Z]:"
  let normalizeDirSeparators (s: string) =
    if Path.DirectorySeparatorChar  = '/' then
      s
    else
       if Path.GetExtension fn = ".json"
       then s.Replace(@"\\", "/")
       else s.Replace('\\','/')

  let lines = s.TrimEnd().Split('\n')

  for i in [ 0 .. lines.Length - 1 ] do

    // re-serialize json so is indented
    if Path.GetExtension fn = ".json" then
      lines.[i] <- formatJson lines.[i]

    // replace paths with <absolute path removed>
    lines.[i] <- Regex.Replace(normalizeDirSeparators lines.[i],
                               sprintf "%s/.*?test/FsAutoComplete\.IntegrationTests/(.*?(\"|$))" driveLetterRegex,
                               "<absolute path removed>/FsAutoComplete.IntegrationTests/$1")

    // replace paths ending with whitespace with <absolute path removed>
    lines.[i] <- Regex.Replace(lines.[i],
                               sprintf "%s/.*?test/FsAutoComplete\.IntegrationTests/(.*?)\s" driveLetterRegex,
                               "<absolute path removed>/FsAutoComplete.IntegrationTests/$1 ")

    // replace dll paths that come from the integrationtests packages directory
    lines.[i] <- stripPackagesDir lines.[i]

    // replace paths ending with ( with <absolute path removed>
    lines.[i] <- Regex.Replace(lines.[i],
                               sprintf "%s/.*?test/FsAutoComplete\.IntegrationTests/(.*?)\(" driveLetterRegex,
                               "<absolute path removed>/FsAutoComplete.IntegrationTests/$1(")

    // replace quoted paths "<path>" with <absolute path removed>
    lines.[i] <- Regex.Replace(lines.[i],
                               sprintf "\"%s/[^\"]*?/([^\"/]*?\.dll\")" driveLetterRegex,
                               "\"<absolute path removed>/$1")

    // replace quoted paths '<path>' with <absolute path removed>
    lines.[i] <- Regex.Replace(lines.[i],
                               sprintf "'%s/[^']*?/([^'/]*?\.[a-zA-Z]*)'" driveLetterRegex,
                               "'<absolute path removed>/$1'")

    // replace temp directory with <tempdir path removed>
    lines.[i] <- Regex.Replace(lines.[i],
                               Path.GetTempPath().Replace("\\","[/|\\\\]"),
                               "<tempdir path removed>/", RegexOptions.IgnoreCase)

    // replace temp filename with <tempfile name removed>
    lines.[i] <- Regex.Replace(lines.[i],
                               "tmp.*?\.tmp",
                               "<tempfile name removed>")

    // normalize newline char
    lines.[i] <- lines.[i].Replace("\r", "").Replace(@"\r", "")

    lines.[i] <- additionalFn lines.[i]

  //workaround for https://github.com/fsharp/fsharp/issues/774
  let lines = lines |> Array.filter ((<>) "non-IL or abstract method with non-zero RVA")

  // Write manually to ensure \n line endings on all platforms
  use f = new StreamWriter(fn)
  for line in lines do
    f.Write(line)
    f.Write('\n')

let writeNormalizedOutput (fn: string) (s: string) =
  writeNormalizedOutputWith id fn s

let runProcess (workingDir: string) (exePath: string) (args: string) =
    printfn "Running '%s %s' in working dir '%s'" exePath args workingDir
    let psi = System.Diagnostics.ProcessStartInfo()
    psi.FileName <- exePath
    psi.WorkingDirectory <- workingDir
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    psi.Arguments <- args
    psi.CreateNoWindow <- true
    psi.UseShellExecute <- false

    use p = new System.Diagnostics.Process()
    p.StartInfo <- psi

    p.OutputDataReceived.Add(fun ea -> printfn "%s" (ea.Data))

    p.ErrorDataReceived.Add(fun ea -> printfn "%s" (ea.Data))

    p.Start() |> ignore
    p.BeginOutputReadLine()
    p.BeginErrorReadLine()
    p.WaitForExit()

    let exitCode = p.ExitCode
    exitCode

let runProcessCaptureOut (workingDir: string) (exePath: string) (args: string) =
    printfn "Running '%s %s' in working dir '%s'" exePath args workingDir
    let psi = System.Diagnostics.ProcessStartInfo()
    psi.FileName <- exePath
    psi.WorkingDirectory <- workingDir
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    psi.Arguments <- args
    psi.CreateNoWindow <- true
    psi.UseShellExecute <- false

    use p = new System.Diagnostics.Process()
    p.StartInfo <- psi

    let sbOut = System.Collections.Generic.List<string>()
    p.OutputDataReceived.Add(fun ea ->
        printfn "%s" (ea.Data)
        sbOut.Add(ea.Data) |> ignore)

    let sbErr = System.Collections.Generic.List<string>()
    p.ErrorDataReceived.Add(fun ea ->
        printfn "%s" (ea.Data)
        sbErr.Add(ea.Data) |> ignore)

    p.Start() |> ignore
    p.BeginOutputReadLine()
    p.BeginErrorReadLine()
    p.WaitForExit()

    let exitCode = p.ExitCode
    (exitCode, sbOut |> List.ofSeq, sbErr |> List.ofSeq)

let processResultLog msg (err, outData, errData) =
    let sb = System.Text.StringBuilder()
    sb.Append(sprintf "%s with exit code %i" msg err) |> ignore
    sb.Append("Output:") |> ignore
    outData |> List.iter (fun (s: string) -> sb.Append(s) |> ignore)
    sb.Append("Error:") |> ignore
    errData |> List.iter (fun (s: string) -> sb.Append(s) |> ignore)
    sb.ToString()

let (|NonExitCodeResult|_|) processResult =
  match processResult with
  | (0,_,_) -> None
  | data -> Some data

let deleteDir d =
  if Directory.Exists(d) then
    printfn "Deleting dir '%s'" d
    Directory.Delete(d, true)

let setEnvVar envVar f =
  let oldValue = System.Environment.GetEnvironmentVariable(envVar)
  let newValue = f oldValue
  System.Environment.SetEnvironmentVariable(envVar, newValue)

  { new IDisposable with
    member x.Dispose() =
      System.Environment.SetEnvironmentVariable(envVar, oldValue) }

let withPath dir =
  setEnvVar "PATH" (fun pathvar -> dir + Path.PathSeparator.ToString() + pathvar)

module DotnetCli =

  // see https://github.com/dotnet/core/blob/master/release-notes/download-archive.md for released version
  // the channel and version are passed to Channel and Version argument of install script, see that for
  // more help
  let private dotnetSdkInstallScript channel version toDir =
    let isWindows = Environment.OSVersion.Platform = PlatformID.Win32NT
    let file = if isWindows then "dotnet-install.ps1" else "dotnet-install.sh"
    let repoDir = Path.Combine(__SOURCE_DIRECTORY__, "..", "..")
    let sdkDir = Path.Combine(repoDir, ".dotnetsdk", toDir) |> Path.GetFullPath

    if Directory.Exists(sdkDir) then
      printfn ".net core sdk found in '%s'" sdkDir
      sdkDir
    else
      printfn ".net core sdk not found in '%s'" sdkDir

      Directory.CreateDirectory(sdkDir) |> ignore

      use client = new System.Net.WebClient()
      let installScriptPath = Path.Combine(sdkDir, file)
      let installScriptUrl = sprintf "https://dot.net/v1/%s" file
      printfn "downloading .net core sdk install script %s" installScriptUrl
      try
        client.DownloadFile(installScriptUrl, installScriptPath)
      with _ when not(isWindows) ->
        //DownloadFile fails in WLS (https://github.com/Microsoft/BashOnWindows/issues/1639), fallback to curl
        printfn "download failed, retry with curl"
        match runProcess __SOURCE_DIRECTORY__  "curl" (sprintf "%s -o %s" installScriptUrl installScriptPath) with
        | 0 -> ()
        | _ -> failwithf "Failed to download script '%s' from curl" installScriptUrl

      printfn "installing .net core sdk (channel %s, version %s) to '%s'" channel version sdkDir

      if isWindows then
        let powershell script args = runProcess __SOURCE_DIRECTORY__ "powershell" (sprintf """-NoProfile -ExecutionPolicy unrestricted -File "%s" %s """ script args) |> ignore
        powershell installScriptPath  (sprintf "-InstallDir %s -Channel %s -Version %s" sdkDir channel version)
      else
        let bash script args = runProcess __SOURCE_DIRECTORY__ "bash" (sprintf """ "%s" %s """ script args) |> ignore
        bash installScriptPath  (sprintf "--install-dir %s -channel %s -version %s" sdkDir channel version)

      sdkDir

  let sdk1Dir () = dotnetSdkInstallScript "1.0" "1.1.4" "v1.1.4"
  let sdk2Dir () = dotnetSdkInstallScript "2.1" "2.1.500" "v2.1.500"

  let useSdk sdkDir =
    let p = withPath sdkDir
    let e = setEnvVar "DOTNET_SKIP_FIRST_TIME_EXPERIENCE" (fun _ -> "1")
    { new IDisposable with
      member x.Dispose() = p.Dispose(); e.Dispose() }

  let withNetFxBclAvaiable version =
    let isWindows = Environment.OSVersion.Platform = PlatformID.Win32NT
    if isWindows then
      //on windows is not needed
      { new IDisposable with member x.Dispose() = () }
    else
      let monoLibPath = "/usr/lib/mono/"
      setEnvVar "FrameworkPathOverride" (fun _ -> sprintf "%s%s-api/" monoLibPath version)
