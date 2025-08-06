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
      let actual = VSTestWrapper.runTests vstestPath ignore [] None false
      Expect.equal actual expected ""
    
    testCase "should be able to report basic test run outcomes" <| fun () -> 
      let expected = [
        ("Tests.My test", TestOutcome.Passed)
        ("Tests.Fails", TestOutcome.Failed)
        ("Tests.Skipped", TestOutcome.Skipped)
        ("Tests.Exception", TestOutcome.Failed)
        ("Tests+Nested.Test 1", TestOutcome.Passed)
        ("Tests+Nested.Test 2", TestOutcome.Passed)
      ]
      
      let sources = [
        Path.Combine(ResourceLocators.sampleProjectsRootDir, "VSTest.XUnit.RunResults/bin/Debug/net8.0/VSTest.XUnit.RunResults.dll")
      ]

      let runResults = VSTestWrapper.runTests vstestPath ignore sources None false

      let likenessOfTestResult (result: TestResult) = (result.TestCase.FullyQualifiedName, result.Outcome)
      let actual = runResults |> List.map likenessOfTestResult

      Expect.equal (set actual) (set expected) ""

    testCase "should run only tests that match the case filter" <| fun () -> 
      let expected = [
        ("Tests+Nested.Test 1", TestOutcome.Passed)
        ("Tests+Nested.Test 2", TestOutcome.Passed)
      ]
      
      let sources = [
        Path.Combine(ResourceLocators.sampleProjectsRootDir, "VSTest.XUnit.RunResults/bin/Debug/net8.0/VSTest.XUnit.RunResults.dll")
      ]

      let runResults = VSTestWrapper.runTests vstestPath ignore sources (Some "FullyQualifiedName~Tests+Nested") false

      let likenessOfTestResult (result: TestResult) = (result.TestCase.FullyQualifiedName, result.Outcome) 
      let actual = runResults |> List.map likenessOfTestResult

      Expect.equal (set actual) (set expected) ""

    testCaseAsync "should report processIds when debugging is on" <| async {
      use tokenSource = new System.Threading.CancellationTokenSource(2000)
      
      let mutable actualProcessId : string option = None
      let updateSpy (update: VSTestWrapper.TestRunUpdate) =
        match update with
        | VSTestWrapper.TestRunUpdate.AttachDebugProcess processId -> 
          actualProcessId <- Some processId
          tokenSource.Cancel()
        | _ -> ()

      use! _c = Async.OnCancel(fun _ -> 
        tokenSource.Cancel())

      Expect.throws (fun () ->
        let sources = [
          Path.Combine(ResourceLocators.sampleProjectsRootDir, "VSTest.XUnit.RunResults/bin/Debug/net8.0/VSTest.XUnit.RunResults.dll")
        ]
        Async.RunSynchronously (VSTestWrapper.runTestsAsync vstestPath updateSpy sources None true, 2000, tokenSource.Token) |> ignore
      ) ""
      
      Expect.isSome actualProcessId "Expected runTest to report a processId"
    }
  ]
