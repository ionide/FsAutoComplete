namespace FsAutoComplete.VSTestAdapter

open Microsoft.TestPlatform.VsTestConsole.TranslationLayer;
open Microsoft.VisualStudio.TestPlatform.ObjectModel;
open Microsoft.VisualStudio.TestPlatform.ObjectModel.Client;
open Microsoft.VisualStudio.TestPlatform.ObjectModel.Client.Interfaces;
open Microsoft.VisualStudio.TestPlatform.ObjectModel.Logging;

module VSTestWrapper = 

    type TestProjectDll = string 

    type private TestDiscoveryHandler () =

        member val DiscoveredTests : TestCase ResizeArray = ResizeArray() with get,set

        interface ITestDiscoveryEventsHandler with 
            member this.HandleDiscoveredTests (discoveredTestCases: System.Collections.Generic.IEnumerable<TestCase>): unit = 
                if (not << isNull) discoveredTestCases then 
                    this.DiscoveredTests.AddRange(discoveredTestCases) 
               
            member this.HandleDiscoveryComplete (_totalTests: int64, lastChunk: System.Collections.Generic.IEnumerable<TestCase>, _isAborted: bool): unit = 
                if (not << isNull) lastChunk then 
                    this.DiscoveredTests.AddRange(lastChunk)
            
            member this.HandleLogMessage (_level: TestMessageLevel, _message: string): unit = 
                ()

            member this.HandleRawMessage (_rawMessage: string): unit = 
                ()
        
    open System.Linq
    let discoverTests (vstestPath: string) (sources: TestProjectDll list) : TestCase list = 
        let consoleParams = ConsoleParameters()

        let vstest = new VsTestConsoleWrapper(vstestPath, consoleParams)
        let discoveryHandler = TestDiscoveryHandler()
        
        vstest.DiscoverTests(sources, null, discoveryHandler)
        discoveryHandler.DiscoveredTests |> List.ofSeq

    open System.IO
    open FsToolkit.ErrorHandling
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
    
        

        
