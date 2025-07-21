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
        

        
