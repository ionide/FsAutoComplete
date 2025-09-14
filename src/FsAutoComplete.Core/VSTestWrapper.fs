namespace FsAutoComplete.TestServer

open System

module VSTestWrapper =
  open Microsoft.TestPlatform.VsTestConsole.TranslationLayer
  open Microsoft.VisualStudio.TestPlatform.ObjectModel
  open Microsoft.VisualStudio.TestPlatform.ObjectModel.Client
  open Microsoft.VisualStudio.TestPlatform.ObjectModel.Logging
  open Microsoft.VisualStudio.TestPlatform.ObjectModel.Client.Interfaces

  type TestProjectDll = string

  type TestDiscoveryUpdate =
    | Progress of TestCase list
    | LogMessage of TestMessageLevel * string

  type private TestDiscoveryHandler(notifyDiscoveryProgress: TestDiscoveryUpdate -> unit) =

    member val DiscoveredTests: TestCase ResizeArray = ResizeArray() with get, set

    interface ITestDiscoveryEventsHandler with
      member this.HandleDiscoveredTests(discoveredTestCases: System.Collections.Generic.IEnumerable<TestCase>) : unit =
        if (not << isNull) discoveredTestCases then
          this.DiscoveredTests.AddRange(discoveredTestCases)
          notifyDiscoveryProgress (discoveredTestCases |> List.ofSeq |> Progress)

      member this.HandleDiscoveryComplete
        (_totalTests: int64, lastChunk: System.Collections.Generic.IEnumerable<TestCase>, _isAborted: bool)
        : unit =
        if (not << isNull) lastChunk then
          this.DiscoveredTests.AddRange(lastChunk)
          notifyDiscoveryProgress (lastChunk |> List.ofSeq |> Progress)

      member this.HandleLogMessage(level: TestMessageLevel, message: string) : unit =
        notifyDiscoveryProgress (LogMessage(level, message))

      member this.HandleRawMessage(_rawMessage: string) : unit = ()

  type ProcessId = int
  type DidDebuggerAttach = bool

  type TestRunUpdate =
    | Progress of TestRunChangedEventArgs
    | LogMessage of TestMessageLevel * string

  type private TestRunHandler(notifyTestRunProgress: TestRunUpdate -> unit) =

    member val TestResults: TestResult ResizeArray = ResizeArray() with get, set

    interface ITestRunEventsHandler with
      member _.HandleLogMessage(level: TestMessageLevel, message: string) : unit =
        notifyTestRunProgress (LogMessage(level, message))

      member _.HandleRawMessage(_rawMessage: string) : unit = ()

      member this.HandleTestRunComplete
        (
          _testRunCompleteArgs: TestRunCompleteEventArgs,
          lastChunkArgs: TestRunChangedEventArgs,
          _runContextAttachments: System.Collections.Generic.ICollection<AttachmentSet>,
          _executorUris: System.Collections.Generic.ICollection<string>
        ) : unit =
        if ((not << isNull) lastChunkArgs && (not << isNull) lastChunkArgs.NewTestResults) then
          this.TestResults.AddRange(lastChunkArgs.NewTestResults)
          notifyTestRunProgress (Progress lastChunkArgs)

      member this.HandleTestRunStatsChange(testRunChangedArgs: TestRunChangedEventArgs) : unit =
        if
          ((not << isNull) testRunChangedArgs
           && (not << isNull) testRunChangedArgs.NewTestResults)
        then
          this.TestResults.AddRange(testRunChangedArgs.NewTestResults)
          notifyTestRunProgress (Progress testRunChangedArgs)

      member _.LaunchProcessWithDebuggerAttached(_testProcessStartInfo: TestProcessStartInfo) : int =
        raise (System.NotImplementedException())

  type private TestHostLauncher(isDebug: bool, onAttachDebugger: ProcessId -> DidDebuggerAttach) =
    // IMPORTANT: RunTestsWithCustomTestHost says it takes an ITestHostLauncher, but it actually calls a method that is only available on ITestHostLauncher3

    interface ITestHostLauncher3 with
      member _.IsDebug: bool = isDebug

      member _.LaunchTestHost(_defaultTestHostStartInfo: TestProcessStartInfo) : int = raise (NotImplementedException())

      member _.LaunchTestHost
        (_defaultTestHostStartInfo: TestProcessStartInfo, _cancellationToken: Threading.CancellationToken)
        : int =
        raise (NotImplementedException())

      member _.AttachDebuggerToProcess
        (attachDebuggerInfo: AttachDebuggerInfo, _cancellationToken: Threading.CancellationToken)
        : bool =
        onAttachDebugger attachDebuggerInfo.ProcessId

      member _.AttachDebuggerToProcess(pid: int) : bool = onAttachDebugger pid

      member _.AttachDebuggerToProcess(pid: int, _cancellationToken: Threading.CancellationToken) : bool =
        onAttachDebugger pid


  module TestPlatformOptions =
    let withTestCaseFilter (options: TestPlatformOptions) filterExpression = options.TestCaseFilter <- filterExpression

  module private RunSettings =
    let defaultRunSettings =
      "<RunSettings>
    <RunConfiguration>
        <DesignMode>False</DesignMode>
    </RunConfiguration>
</RunSettings>"

  let discoverTestsAsync
    (vstestPath: string)
    (onDiscoveryProgress: TestDiscoveryUpdate -> unit)
    (sources: TestProjectDll list)
    : Async<TestCase list> =
    async {
      let consoleParams = ConsoleParameters()
      let vstest = new VsTestConsoleWrapper(vstestPath, consoleParams)
      let discoveryHandler = TestDiscoveryHandler(onDiscoveryProgress)

      use! _onCancel = Async.OnCancel(fun () -> vstest.CancelDiscovery())

      vstest.DiscoverTests(sources, null, discoveryHandler)
      return discoveryHandler.DiscoveredTests |> List.ofSeq
    }

  /// onAttachDebugger assumes that the debugger is attached when the method returns. The test project will continue execution as soon as attachDebugger returns
  let runTestsAsync
    (vstestPath: string)
    (onTestRunProgress: TestRunUpdate -> unit)
    (onAttachDebugger: ProcessId -> DidDebuggerAttach)
    (sources: TestProjectDll list)
    (testCaseFilter: string option)
    (shouldDebug: bool)
    : Async<TestResult list> =
    async {
      let consoleParams = ConsoleParameters()
      let vstest = new VsTestConsoleWrapper(vstestPath, consoleParams)
      let runHandler = TestRunHandler(onTestRunProgress)
      let runSettings = RunSettings.defaultRunSettings

      let options = new TestPlatformOptions()
      testCaseFilter |> Option.iter (TestPlatformOptions.withTestCaseFilter options)

      use! _cancel =
        Async.OnCancel(fun () ->
          printfn "Cancelling test run"
          vstest.CancelTestRun()
          printfn "Test Run Cancelled")

      if shouldDebug then
        let hostLauncher = TestHostLauncher(shouldDebug, onAttachDebugger)
        vstest.RunTestsWithCustomTestHost(sources, runSettings, options, runHandler, hostLauncher)
      else
        vstest.RunTests(sources, runSettings, options, runHandler)

      return runHandler.TestResults |> List.ofSeq
    }

  open System.IO

  let tryFindVsTestFromDotnetRoot (dotnetRoot: string) (workspaceRoot: string option) : Result<FileInfo, string> =
    let cwd =
      defaultArg workspaceRoot System.Environment.CurrentDirectory |> DirectoryInfo

    let dotnetBinary =
      if dotnetRoot |> Directory.Exists then
        if
          System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(
            System.Runtime.InteropServices.OSPlatform.Windows
          )
        then
          FileInfo(Path.Combine(dotnetRoot, "dotnet.exe"))
        else
          FileInfo(Path.Combine(dotnetRoot, "dotnet"))
      else
        dotnetRoot |> FileInfo

    match Ionide.ProjInfo.SdkDiscovery.versionAt cwd dotnetBinary with
    | Ok sdkVersion ->
      let sdks = Ionide.ProjInfo.SdkDiscovery.sdks dotnetBinary

      match sdks |> Array.tryFind (fun sdk -> sdk.Version = sdkVersion) with
      | Some sdk ->
        let vstestBinary = Path.Combine(sdk.Path.FullName, "vstest.console.dll") |> FileInfo

        if vstestBinary.Exists then
          Ok vstestBinary
        else
          Error $"Found the correct dotnet sdk, but vstest was not at the expected sub-path: {vstestBinary.FullName}"
      | None -> Error $"Couldn't find the install location for dotnet sdk version: {sdkVersion}"
    | Error _ -> Error $"Couldn't identify the dotnet version for working directory: {cwd.FullName}"
