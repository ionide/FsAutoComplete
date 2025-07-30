module TestRunTests

open Expecto
open FsAutoComplete.TestServer
open Microsoft.VisualStudio.TestPlatform.ObjectModel;
open System.IO


let tryFindVsTest () : string =
  let dotnetBinary =  
    Ionide.ProjInfo.Paths.dotnetRoot.Value
    |> Option.defaultWith (fun () -> failwith "Couldn't find dotnet root. The dotnet sdk must be installed to run these tests")

  let cwd = System.Environment.CurrentDirectory |> Some
  
  VSTestWrapper.tryFindVsTestFromDotnetRoot dotnetBinary.FullName cwd
  |> Result.defaultWith failwith
  |> _.FullName
  

let vstestPath = tryFindVsTest ()

[<Tests>]
let tests =
  testList "VSTestWrapper Test Run" [
    testCase "should return an empty list if given no projects" <| fun () ->
      let expected = []
      let actual = VSTestWrapper.discoverTests vstestPath ignore [] 
      Expect.equal actual expected ""
    
    testCase "should be able to report basic test run outcomes" <| fun () -> 
      let expected = [
        ("Tests.My test", TestOutcome.Passed)
        ("Tests.Fails", TestOutcome.Failed)
        ("Tests.Skipped", TestOutcome.Skipped)
        ("Tests.Exception", TestOutcome.Failed)
      ]
      
      let sourceDir = __SOURCE_DIRECTORY__
      let sources = [
        Path.Combine(sourceDir, "SampleTestProjects/VSTest.XUnit.RunResults/bin/Debug/net8.0/VSTest.XUnit.RunResults.dll")
      ]

      let runResults = VSTestWrapper.runTests vstestPath sources

      let likenessOfTestResult (result: TestResult) = (result.TestCase.FullyQualifiedName, result.Outcome) 
      let actual = runResults |> List.map likenessOfTestResult

      Expect.equal (set actual) (set expected) ""
  ]
