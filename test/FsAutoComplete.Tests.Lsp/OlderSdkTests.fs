/// FSAC runs on .NET 10, but the projects it loads can target older frameworks and pin an older SDK in global.json.
/// FSAC then loads the MSBuild of that SDK into its .NET 10 process. The test host can register MSBuild only once, for
/// its own SDK, so these tests start FSAC as a separate process, the way an editor does, and talk LSP to it.
module FsAutoComplete.Tests.Lsp.OlderSdkTests

open System
open System.Collections.Concurrent
open System.Diagnostics
open System.IO
open System.Text
open System.Text.Json.Nodes
open System.Threading.Tasks
open Expecto
open Helpers
open Ionide.ProjInfo
open FsAutoComplete

/// Ionide.ProjInfo sets these in the test host for the SDK of the tests. A child `dotnet` that inherits them uses that
/// MSBuild instead of the one its global.json selects.
let private msbuildVariables =
  [ "MSBUILD_EXE_PATH"; "MSBuildExtensionsPath"; "MSBuildSDKsPath" ]

let private startInfo (dotnet: FileInfo) (directory: string) (arguments: string list) =
  let psi = ProcessStartInfo(dotnet.FullName)
  arguments |> List.iter psi.ArgumentList.Add
  psi.WorkingDirectory <- directory
  psi.UseShellExecute <- false
  psi.RedirectStandardInput <- true
  psi.RedirectStandardOutput <- true
  psi.RedirectStandardError <- true

  for name in msbuildVariables do
    psi.Environment.Remove name |> ignore

  psi

let private run dotnet directory arguments =
  task {
    use proc = Process.Start(startInfo dotnet directory arguments)
    proc.StandardInput.Close()
    let stdout = proc.StandardOutput.ReadToEndAsync()
    let stderr = proc.StandardError.ReadToEndAsync()
    do! proc.WaitForExitAsync()
    let! stdout = stdout
    let! stderr = stderr
    return proc.ExitCode, stdout + stderr
  }

let private send (input: Stream) (message: JsonObject) =
  let body = Encoding.UTF8.GetBytes(message.ToJsonString())
  input.Write(Encoding.ASCII.GetBytes $"Content-Length: %i{body.Length}\r\n\r\n")
  input.Write(body)
  input.Flush()

let private readMessage (output: Stream) =
  let readLine () =
    let line = StringBuilder()
    let mutable finished = false

    while not finished do
      match output.ReadByte() with
      | -1 -> raise (EndOfStreamException "FSAC closed its output")
      | 10 -> finished <- true
      | 13 -> ()
      | b -> line.Append(char b) |> ignore

    line.ToString()

  let mutable length = 0
  let mutable header = readLine ()

  while header <> "" do
    let separator = header.IndexOf ':'

    if header.Substring(0, separator).Trim().Equals("Content-Length", StringComparison.OrdinalIgnoreCase) then
      length <- int (header.Substring(separator + 1).Trim())

    header <- readLine ()

  let body = Array.zeroCreate<byte> length
  output.ReadExactly(body)
  JsonNode.Parse(body).AsObject()

let private message (fields: (string * JsonNode) list) =
  let message = JsonObject()
  message["jsonrpc"] <- JsonValue.Create "2.0"

  for name, value in fields do
    message[name] <- value

  message

let private request (id: int) (method: string) (parameters: JsonNode) =
  message
    [
      "id", JsonValue.Create id
      "method", JsonValue.Create method
      "params", parameters
    ]

let private notification (method: string) (parameters: JsonNode) =
  message [ "method", JsonValue.Create method; "params", parameters ]

/// Answers the server until it publishes diagnostics for `file` that are not empty, and returns them. Outside the
/// `task` of `firstDiagnostics`: a `let rec` there cannot compile to a state machine (FS3511).
let rec private handle (input: Stream) (output: Stream) (file: string) (fileUri: string) =
  let received = readMessage output
  let method = Option.ofObj received["method"] |> Option.map _.GetValue<string>()
  let id = Option.ofObj received["id"]

  match method, id with
  | None, Some id when id.GetValue<int>() = 1 ->
    send input (notification "initialized" (JsonObject()))

    send
      input
      (notification
        "textDocument/didOpen"
        (JsonNode.Parse
          $"""{{ "textDocument": {{ "uri": %s{JsonValue.Create(fileUri).ToJsonString()}, "languageId": "fsharp", "version": 1, "text": %s{JsonValue.Create(File.ReadAllText file).ToJsonString()} }} }}"""))

    handle input output file fileUri
  | Some "textDocument/publishDiagnostics", _ when received["params"].["uri"].GetValue<string>() = fileUri ->
    let diagnostics = received["params"].["diagnostics"].AsArray()

    if diagnostics.Count = 0 then
      handle input output file fileUri
    else
      diagnostics
  | Some _, Some id ->
    // A request from the server, such as client/registerCapability.
    send input (message [ "id", id.DeepClone(); "result", null ])
    handle input output file fileUri
  | _ -> handle input output file fileUri

/// Starts FSAC in `directory`, opens `file` and returns the diagnostics of the first publish for it that is not empty.
let private firstDiagnostics (dotnet: FileInfo) (directory: string) (file: string) =
  task {
    let fsac = Path.Combine(AppContext.BaseDirectory, "fsautocomplete.dll")
    use proc = Process.Start(startInfo dotnet directory [ fsac ])
    let log = ConcurrentQueue<string>()

    proc.ErrorDataReceived.Add(fun args ->
      if not (isNull args.Data) then
        log.Enqueue args.Data)

    proc.BeginErrorReadLine()
    let input = proc.StandardInput.BaseStream
    let output = proc.StandardOutput.BaseStream
    let fileUri = Path.FilePathToUri file

    try
      try
        send
          input
          (request
            1
            "initialize"
            (JsonNode.Parse
              $"""{{ "processId": %i{Environment.ProcessId}, "rootUri": %s{JsonValue.Create(Path.FilePathToUri directory).ToJsonString()}, "capabilities": {{ "textDocument": {{ "publishDiagnostics": {{}} }} }}, "initializationOptions": {{ "AutomaticWorkspaceInit": true }} }}"""))

        let! diagnostics =
          Task.Run(fun () -> handle input output file fileUri).WaitAsync(Helpers.Expecto.DEFAULT_TIMEOUT)

        send input (request 2 "shutdown" null)
        send input (notification "exit" null)
        return diagnostics
      with e ->
        return
          failtestf
            "FSAC did not report diagnostics for %s: %s\nLast lines of its log:\n%s"
            file
            e.Message
            (String.Join("\n", Seq.truncate 40 (Seq.rev log) |> Seq.rev))
    finally
      if not (proc.WaitForExit(TimeSpan.FromSeconds 10.)) then
        proc.Kill(entireProcessTree = true)
  }

let tests =
  testList
    "older SDK"
    [
      testCaseAsync "loads a net8.0 project whose global.json pins SDK 8"
      <| async {
        let dotnet =
          Paths.dotnetRoot.Value
          |> Option.defaultWith (fun () -> failtest "No dotnet binary found")

        let sdk8 =
          SdkDiscovery.sdks dotnet
          |> Array.filter (fun sdk -> sdk.Version.Major = 8)
          |> Array.sortBy _.Version
          |> Array.tryLast

        match sdk8 with
        | None -> skiptest "No .NET 8 SDK is installed."
        | Some sdk ->
          let directory = Directory.CreateTempSubdirectory "fsac-older-sdk"

          try
            let root = directory.FullName

            File.WriteAllText(
              Path.Combine(root, "global.json"),
              $"""{{ "sdk": {{ "version": "%O{sdk.Version}", "rollForward": "disable" }} }}"""
            )

            File.WriteAllText(
              Path.Combine(root, "App.fsproj"),
              """<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net8.0</TargetFramework>
  </PropertyGroup>
  <ItemGroup>
    <Compile Include="Program.fs" />
  </ItemGroup>
</Project>
"""
            )

            let program = Path.Combine(root, "Program.fs")
            File.WriteAllText(program, "module Program\n\nlet broken: int = \"not an int\"\n")

            let! exitCode, version = run dotnet root [ "--version" ] |> Async.AwaitTask
            Expect.equal exitCode 0 $"`dotnet --version` failed: %s{version}"
            Expect.equal (version.Trim()) (string sdk.Version) "The project directory resolves the pinned SDK"

            let! exitCode, restore = run dotnet root [ "restore" ] |> Async.AwaitTask
            Expect.equal exitCode 0 $"`dotnet restore` with SDK %O{sdk.Version} failed: %s{restore}"

            let! diagnostics = firstDiagnostics dotnet root program |> Async.AwaitTask

            let typeError =
              diagnostics
              |> Seq.tryFind (fun diagnostic -> diagnostic["code"].ToString() = "1")

            Expect.isSome
              typeError
              $"Expected the type error FS0001 from checking Program.fs in the project, got %s{diagnostics.ToJsonString()}"
          finally
            try
              directory.Delete(true)
            with _ ->
              ()
      }
    ]
