namespace FsAutoComplete.VSTestAdapter

open Microsoft.TestPlatform.VsTestConsole.TranslationLayer;
open Microsoft.VisualStudio.TestPlatform.ObjectModel;
open Microsoft.VisualStudio.TestPlatform.ObjectModel.Client;
open Microsoft.VisualStudio.TestPlatform.ObjectModel.Client.Interfaces;
open Microsoft.VisualStudio.TestPlatform.ObjectModel.Logging;

module VSTestWrapper = 

    type TestProjectDll = string 

    type private TestDiscoveryHandler (notifyIncrementalUpdate: TestCase list -> unit) =

        member val DiscoveredTests : TestCase ResizeArray = ResizeArray() with get,set

        interface ITestDiscoveryEventsHandler with 
            member this.HandleDiscoveredTests (discoveredTestCases: System.Collections.Generic.IEnumerable<TestCase>): unit = 
                if (not << isNull) discoveredTestCases then 
                    this.DiscoveredTests.AddRange(discoveredTestCases)
                    notifyIncrementalUpdate (discoveredTestCases |> List.ofSeq) 
               
            member this.HandleDiscoveryComplete (_totalTests: int64, lastChunk: System.Collections.Generic.IEnumerable<TestCase>, _isAborted: bool): unit = 
                if (not << isNull) lastChunk then 
                    this.DiscoveredTests.AddRange(lastChunk)
                    notifyIncrementalUpdate (lastChunk |> List.ofSeq)
            
            member this.HandleLogMessage (_level: TestMessageLevel, _message: string): unit = 
                ()

            member this.HandleRawMessage (_rawMessage: string): unit = 
                ()
        
    let discoverTests (vstestPath: string) (incrementalUpdateHandler: TestCase list -> unit)  (sources: TestProjectDll list) : TestCase list = 
        let consoleParams = ConsoleParameters()
        let vstest = new VsTestConsoleWrapper(vstestPath, consoleParams)
        let discoveryHandler = TestDiscoveryHandler(incrementalUpdateHandler)
        
        vstest.DiscoverTests(sources, null, discoveryHandler)
        discoveryHandler.DiscoveredTests |> List.ofSeq


    type TestRunHandler() = 
        member val TestResults : TestResult ResizeArray = ResizeArray() with get,set

        interface ITestRunEventsHandler with
            member _.HandleLogMessage (_level: TestMessageLevel, _message: string): unit = 
                ()

            member _.HandleRawMessage (_rawMessage: string): unit = 
                ()

            member this.HandleTestRunComplete (_testRunCompleteArgs: TestRunCompleteEventArgs, lastChunkArgs: TestRunChangedEventArgs, _runContextAttachments: System.Collections.Generic.ICollection<AttachmentSet>, _executorUris: System.Collections.Generic.ICollection<string>): unit = 
                if((not << isNull) lastChunkArgs && (not << isNull) lastChunkArgs.NewTestResults) then
                    this.TestResults.AddRange(lastChunkArgs.NewTestResults)

            member this.HandleTestRunStatsChange (testRunChangedArgs: TestRunChangedEventArgs): unit = 
                if((not << isNull) testRunChangedArgs && (not << isNull) testRunChangedArgs.NewTestResults) then
                    this.TestResults.AddRange(testRunChangedArgs.NewTestResults)

            member _.LaunchProcessWithDebuggerAttached (_testProcessStartInfo: TestProcessStartInfo): int = 
                raise (System.NotImplementedException())

    let runTests (vstestPath: string) (sources: TestProjectDll list) = 
        let consoleParams = ConsoleParameters()
        let vstest = new VsTestConsoleWrapper(vstestPath, consoleParams)
        let runHandler = TestRunHandler()
        
        vstest.RunTests(sources, null, runHandler)
        runHandler.TestResults |> List.ofSeq

    open System.IO
    let tryFindVsTestFromDotnetRoot (dotnetRoot: string) (workspaceRoot: string option) : Result<FileInfo, string> =
        let cwd = defaultArg workspaceRoot System.Environment.CurrentDirectory |> DirectoryInfo
        let dotnetBinary = 
            if dotnetRoot |> Directory.Exists then
                if System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(System.Runtime.InteropServices.OSPlatform.Windows)
                then
                    FileInfo(Path.Combine(dotnetRoot, "dotnet.exe"))
                else
                    FileInfo(Path.Combine(dotnetRoot, "dotnet")) 
            else dotnetRoot |> FileInfo

        match Ionide.ProjInfo.SdkDiscovery.versionAt cwd dotnetBinary with
        | Ok sdkVersion ->
            let sdks = Ionide.ProjInfo.SdkDiscovery.sdks dotnetBinary

            match sdks |> Array.tryFind (fun sdk -> sdk.Version = sdkVersion) with
            | Some sdk ->
                let vstestBinary = Path.Combine(sdk.Path.FullName, "vstest.console.dll") |> FileInfo 
                if vstestBinary.Exists 
                then Ok vstestBinary
                else Error $"Found the correct dotnet sdk, but vstest was not at the expected sub-path: {vstestBinary.FullName}"
            | None -> Error $"Couldn't find the install location for dotnet sdk version: {sdkVersion}"
        | Error _ -> Error $"Couldn't identify the dotnet version for working directory: {cwd.FullName}"
    

type TestFileRange = {
    StartLine: int
    EndLine: int
  }
type TestItem = {
    FullName : string
    DisplayName : string
    /// Identifies the test adapter that ran the tests
    /// Example: executor://xunit/VsTestRunner2/netcoreapp 
    /// Used for determining the test library, which effects how tests names are broken down
    ExecutorUri : string
    ProjectFilePath : string
    TargetFramework : string
    CodeFilePath : string option
    CodeLocationRange : TestFileRange option  
}
    

        
