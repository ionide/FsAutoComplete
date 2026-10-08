module TestRunTests

open Expecto
open FsAutoComplete.TestServer
open Microsoft.VisualStudio.TestPlatform.ObjectModel
open System.IO

let vstestPath = ResourceLocators.tryFindVsTest ()

let nullAttachDebugger _ = false

let private sampleDll relativePath = Path.Combine(ResourceLocators.sampleProjectsRootDir, relativePath)

/// Discovers in one wrapper call and runs the chosen case in another, as an id run does:
/// the case handed to the run comes from a separate vstest.console session.
let private discoverThenRun (dll: string) (select: TestCase -> bool) =
  async {
    let! discovered = VSTestWrapper.discoverTestsAsync vstestPath ignore [ dll ]
    let selected = discovered |> List.filter select
    Expect.hasLength selected 1 "Expected the selection to match exactly one discovered case"
    let! results = VSTestWrapper.runTestCasesAsync vstestPath ignore nullAttachDebugger selected false
    return selected.Head, results
  }

let private expectOnlyResultFor (selected: TestCase) (outcome: TestOutcome) (results: TestResult list) =
  Expect.hasLength results 1 "Expected exactly one result"
  Expect.equal results.Head.TestCase.Id selected.Id "Expected the result for the selected case"
  Expect.equal results.Head.Outcome outcome ""

[<Tests>]
let testCaseRunTests =
  testList
    "VSTestWrapper Test Case Run"
    [ testCaseAsync "it should return an empty list without starting vstest when given no test cases"
      <| async {
        let! actual = VSTestWrapper.runTestCasesAsync "does-not-exist.dll" ignore nullAttachDebugger [] false
        Expect.isEmpty actual ""
      }

      testCaseAsync "it should run only the selected xUnit theory row"
      <| async {
        let! selected, results =
          discoverThenRun (sampleDll "VSTest.XUnit.Theories/bin/Debug/net8.0/VSTest.XUnit.Theories.dll") (fun tc ->
            tc.DisplayName.Contains "x: 2")

        expectOnlyResultFor selected TestOutcome.Failed results
      }

      testCaseAsync "it should run only the selected MSTest data row"
      <| async {
        let! selected, results =
          discoverThenRun (sampleDll "VSTest.MSTest.DataRows/bin/Debug/net8.0/VSTest.MSTest.DataRows.dll") (fun tc ->
            tc.DisplayName.Contains "(2)")

        expectOnlyResultFor selected TestOutcome.Failed results
      }

      testCaseAsync "it should run a selected NUnit test"
      <| async {
        let! selected, results =
          discoverThenRun (sampleDll "VSTest.NUnit/bin/Debug/net8.0/VSTest.NUnit.dll") (fun tc ->
            tc.FullyQualifiedName = "VSTest.NUnit.Test1")

        expectOnlyResultFor selected TestOutcome.Passed results
      }

      testCaseAsync "it should run a selected xUnit fact"
      <| async {
        let! selected, results =
          discoverThenRun (sampleDll "VSTest.XUnit.RunResults/bin/Debug/net8.0/VSTest.XUnit.RunResults.dll") (fun tc ->
            tc.FullyQualifiedName = "Tests+Nested.Test 1")

        expectOnlyResultFor selected TestOutcome.Passed results
      }

      testCaseAsync "it should report a processId and complete the run when debugging is on"
      <| async {
        let dll =
          sampleDll "VSTest.XUnit.Theories/bin/Debug/net8.0/VSTest.XUnit.Theories.dll"

        let! discovered = VSTestWrapper.discoverTestsAsync vstestPath ignore [ dll ]
        let selected = discovered |> List.filter (fun tc -> tc.DisplayName.Contains "x: 2")

        let mutable reportedProcessIds: int list = []

        let attachSpy (processId: int) =
          reportedProcessIds <- processId :: reportedProcessIds
          true

        let! results = VSTestWrapper.runTestCasesAsync vstestPath ignore attachSpy selected true

        Expect.hasLength reportedProcessIds 1 "Expected the run to report the test host's processId once"
        expectOnlyResultFor selected.Head TestOutcome.Failed results
      } ]

[<Tests>]
let tests =
  testList
    "VSTestWrapper Test Run"
    [ testCaseAsync "it should return an empty list if given no projects"
      <| async {
        let expected = []
        let! actual = VSTestWrapper.runTestsAsync vstestPath ignore nullAttachDebugger [] None false
        Expect.equal actual expected ""
      }

      testCaseAsync "it should be able to report basic test run outcomes"
      <| async {
        let expected =
          [ "Tests.My test", TestOutcome.Passed
            "Tests.Fails", TestOutcome.Failed
            "Tests.Skipped", TestOutcome.Skipped
            "Tests.Exception", TestOutcome.Failed
            "Tests+Nested.Test 1", TestOutcome.Passed
            "Tests+Nested.Test 2", TestOutcome.Passed
            "Tests.Expects environment variable", TestOutcome.Failed ]

        let sources =
          [ Path.Combine(
              ResourceLocators.sampleProjectsRootDir,
              "VSTest.XUnit.RunResults/bin/Debug/net8.0/VSTest.XUnit.RunResults.dll"
            ) ]

        let! runResults = VSTestWrapper.runTestsAsync vstestPath ignore nullAttachDebugger sources None false

        let likenessOfTestResult (result: TestResult) = (result.TestCase.FullyQualifiedName, result.Outcome)
        let actual = runResults |> List.map likenessOfTestResult

        Expect.equal (set actual) (set expected) ""
      }

      testCaseAsync "it should run only tests that match the case filter"
      <| async {
        let expected =
          [ ("Tests+Nested.Test 1", TestOutcome.Passed)
            ("Tests+Nested.Test 2", TestOutcome.Passed) ]

        let sources =
          [ Path.Combine(
              ResourceLocators.sampleProjectsRootDir,
              "VSTest.XUnit.RunResults/bin/Debug/net8.0/VSTest.XUnit.RunResults.dll"
            ) ]

        let! runResults =
          VSTestWrapper.runTestsAsync
            vstestPath
            ignore
            nullAttachDebugger
            sources
            (Some "FullyQualifiedName~Tests+Nested")
            false

        let likenessOfTestResult (result: TestResult) = (result.TestCase.FullyQualifiedName, result.Outcome)
        let actual = runResults |> List.map likenessOfTestResult

        Expect.equal (set actual) (set expected) ""
      }

      testCaseAsync "it should respect test filters on NUnit projects"
      <| async {
        // NOTE: This is an NUnit bug. NUnit doesn't respect filters when VSTest is in Design Mode, which VsTestConsoleWrapper is by default
        //       https://github.com/ionide/FsAutoComplete/pull/1383#issuecomment-3245590606
        let expected = [ "VSTest.NUnit.Test1", TestOutcome.Passed ]

        let sources =
          [ Path.Combine(ResourceLocators.sampleProjectsRootDir, "VSTest.NUnit/bin/Debug/net8.0/VSTest.NUnit.dll") ]

        let! runResults =
          VSTestWrapper.runTestsAsync
            vstestPath
            ignore
            nullAttachDebugger
            sources
            (Some "FullyQualifiedName~Test1")
            false

        let likenessOfTestResult (result: TestResult) = (result.TestCase.FullyQualifiedName, result.Outcome)
        let actual = runResults |> List.map likenessOfTestResult

        Expect.equal (set actual) (set expected) ""
      }

      testCaseAsync "it should report processIds when debugging is on"
      <| async {
        use tokenSource = new System.Threading.CancellationTokenSource(2000)

        let mutable actualProcessId: int option = None

        let updateSpy (processId: int) =
          actualProcessId <- Some processId
          tokenSource.Cancel()
          false

        use! _c = Async.OnCancel(fun _ -> tokenSource.Cancel())

        Expect.throwsT<System.OperationCanceledException>
          (fun () ->
            let sources =
              [ Path.Combine(
                  ResourceLocators.sampleProjectsRootDir,
                  "VSTest.XUnit.RunResults/bin/Debug/net8.0/VSTest.XUnit.RunResults.dll"
                ) ]

            Async.RunSynchronously(
              VSTestWrapper.runTestsAsync vstestPath ignore updateSpy sources None true,
              cancellationToken = tokenSource.Token
            )
            |> ignore)
          ""

        Expect.isSome actualProcessId "Expected runTest to report a processId"
      }

      testCaseAsync "it should report a processId only once per process"
      <| async {
        use tokenSource = new System.Threading.CancellationTokenSource(1000)

        let mutable reportedProcessIds: int list = []

        let updateSpy (processId: int) =
          reportedProcessIds <- processId :: reportedProcessIds
          tokenSource.Cancel()
          false

        use! _c = Async.OnCancel(fun _ -> tokenSource.Cancel())

        Expect.throwsT<System.OperationCanceledException>
          (fun () ->
            let sources =
              [ Path.Combine(
                  ResourceLocators.sampleProjectsRootDir,
                  "VSTest.XUnit.RunResults/bin/Debug/net8.0/VSTest.XUnit.RunResults.dll"
                ) ]

            Async.RunSynchronously(
              VSTestWrapper.runTestsAsync vstestPath ignore updateSpy sources None true,
              cancellationToken = tokenSource.Token
            )
            |> ignore)
          ""

        Expect.hasLength reportedProcessIds 1 "Expected runTest to report a processId"
      } ]
