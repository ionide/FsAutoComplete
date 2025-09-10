module FsAutoComplete.Tests.TestExplorer

open Expecto
open Helpers
open System.IO
open FsAutoComplete.LspHelpers
open System.Threading
open Helpers.Expecto.ShadowedTimeouts
open FsAutoComplete.Tests.Lsp.Helpers

module TestRunResult =
  open Ionide.LanguageServerProtocol.JsonRpc

  let tryUnwrapTestRunResult (res: LspResult<PlainNotification option>) =
    match res with
    | Ok plainNotification ->
      plainNotification
      |> Option.get
      |> _.Content
      |> FsAutoComplete.JsonSerializer.readJson<
        FsAutoComplete.CommandResponse.ResponseMsg<FsAutoComplete.TestServer.TestResult list>
          >
      |> _.Data
    | Error err -> failwith $"TestRunTests returned error: {err.Message}"

module ExpectedTests =
  let XUnitRunResults =
    [ "Tests.My test", FsAutoComplete.TestServer.TestOutcome.Passed
      "Tests.Fails", FsAutoComplete.TestServer.TestOutcome.Failed
      "Tests.Skipped", FsAutoComplete.TestServer.TestOutcome.Skipped
      "Tests.Exception", FsAutoComplete.TestServer.TestOutcome.Failed
      "Tests+Nested.Test 1", FsAutoComplete.TestServer.TestOutcome.Passed
      "Tests+Nested.Test 2", FsAutoComplete.TestServer.TestOutcome.Passed
      "Tests.Expects environment variable", FsAutoComplete.TestServer.TestOutcome.Failed ]

module Workspace =

  let build workspaceRoot =
    let dir = DirectoryInfo workspaceRoot

    if not dir.Exists then
      failwith $"Target workspace doesn't exist: {workspaceRoot}"

    let projects = dir.GetFiles("*.?sproj", SearchOption.AllDirectories)

    for project in projects do
      let buildResult = DotnetCli.build project.FullName

      Expect.equal
        0
        buildResult.ExitCode
        $"Workspace build failed with: {buildResult.StdErr} \nProject: {project.FullName}"

let tests createServer =
  let initializeServer workspaceRoot =
    async {
      let! (server, event) = serverInitialize workspaceRoot defaultConfigDto createServer
      do! waitForWorkspaceFinishedParsing event

      return (server, event)
    }
    |> Async.Cache

  testSequenced
  <| testList
    "TestExplorerTests"
    [ testCaseAsync "it should error if the workspace hasn't been built"
      <| async {
        let workspaceRoot =
          Path.Combine(__SOURCE_DIRECTORY__, "SampleTestProjects", "VSTest.XUnit.RunResults")

        let! server, _ = initializeServer workspaceRoot
        use server = server

        let! res = server.TestDiscoverTests()

        Expect.isTrue res.IsError ""
      }
      testCaseAsync "it should report tests of all basic outcomes"
      <| async {
        let workspaceRoot =
          Path.Combine(__SOURCE_DIRECTORY__, "SampleTestProjects", "VSTest.XUnit.RunResults")

        let! server, _ = initializeServer workspaceRoot
        use server = server

        let buildResult = DotnetCli.build workspaceRoot
        Expect.equal 0 buildResult.ExitCode $"Build failed with: {buildResult.StdErr}"

        let runRequest: TestRunRequest =
          { LimitToProjects = None
            TestCaseFilter = None
            AttachDebugger = false }

        let! res = server.TestRunTests(runRequest)

        let actual =
          TestRunResult.tryUnwrapTestRunResult res
          |> List.map (fun tr -> tr.TestItem.FullName, tr.Outcome)

        let expected = ExpectedTests.XUnitRunResults

        Expect.equal (set actual) (set expected) ""
      }

      testCaseAsync "it should report a processId when debugging a test project"
      <| async {
        let workspaceRoot =
          Path.Combine(__SOURCE_DIRECTORY__, "SampleTestProjects", "VSTest.XUnit.RunResults")

        let! server, clientNotifications = initializeServer workspaceRoot

        use server = server

        let buildResult = DotnetCli.build workspaceRoot
        Expect.equal 0 buildResult.ExitCode $"Build failed with: {buildResult.StdErr}"

        use tokenSource = new CancellationTokenSource()
        let mutable processIdSpy: int option = None
        use! _onCancel = Async.OnCancel(fun () -> tokenSource.Cancel())

        use _ =
          clientNotifications.Subscribe(fun (msgType: string, data: obj) ->
            if msgType = "test/processWaitingForDebugger" then
              let processId: int =
                data :?> PlainNotification
                |> _.Content
                |> FsAutoComplete.JsonSerializer.readJson

              processIdSpy <- Some processId
              tokenSource.Cancel())

        Expect.throwsT<System.OperationCanceledException>
          (fun () ->
            let runRequest: TestRunRequest =
              { LimitToProjects = None
                TestCaseFilter = None
                AttachDebugger = true }

            Async.RunSynchronously(
              server.TestRunTests(runRequest) |> Async.Ignore,
              cancellationToken = tokenSource.Token
            ))
          ""

        Expect.isSome processIdSpy ""

        let maybeHangingTestProcess =
          System.Diagnostics.Process.GetProcesses()
          |> Array.tryFind (fun p -> Some p.Id = processIdSpy)

        Expect.isNone maybeHangingTestProcess "All test processes should be canceled with the test run"
      }

      testCaseAsync
        "it should inherit environment variables from it's parent, allowing tests to depend on environment variables"
      <| async {
        let workspaceRoot =
          Path.Combine(__SOURCE_DIRECTORY__, "SampleTestProjects", "VSTest.XUnit.RunResults")

        let! server, _ = initializeServer workspaceRoot

        use server = server

        let buildResult = DotnetCli.build workspaceRoot
        Expect.equal 0 buildResult.ExitCode $"Build failed with: {buildResult.StdErr}"

        System.Environment.SetEnvironmentVariable("dd586685-08f6-410c-a9f1-84530af117ab", "Set me")

        let! response =
          server.TestRunTests(
            { LimitToProjects = None
              TestCaseFilter = Some "FullyQualifiedName~Tests.Expects environment variable"
              AttachDebugger = false }
          )

        let expected =
          [ "Tests.Expects environment variable", FsAutoComplete.TestServer.TestOutcome.Passed ]

        let actual =
          TestRunResult.tryUnwrapTestRunResult response
          |> List.map (fun tr -> tr.TestItem.FullName, tr.Outcome)

        Expect.equal (set actual) (set expected) ""

        System.Environment.SetEnvironmentVariable("dd586685-08f6-410c-a9f1-84530af117ab", "")
      }

      testCaseAsync "it should ignore test project filters that aren't projects in the workspace"
      <| async {
        let workspaceRoot =
          Path.Combine(__SOURCE_DIRECTORY__, "SampleTestProjects", "VSTest.XUnit.RunResults")

        let! server, _ = initializeServer workspaceRoot

        use server = server

        let buildResult = DotnetCli.build workspaceRoot
        Expect.equal 0 buildResult.ExitCode $"Build failed with: {buildResult.StdErr}"

        let! response =
          server.TestRunTests(
            { LimitToProjects = Some [ Path.Combine(__SOURCE_DIRECTORY__, "SampleTestProjects", "Nope", "Nope.fsproj") ]
              TestCaseFilter = None
              AttachDebugger = false }
          )

        let expected = []

        let actual =
          TestRunResult.tryUnwrapTestRunResult response
          |> List.map (fun tr -> tr.TestItem.FullName, tr.Outcome)

        Expect.equal (set actual) (set expected) ""
      }

      testCaseAsync "it should run only test projects in the project filter when specified"
      <| async {
        let workspaceRoot = Path.Combine(__SOURCE_DIRECTORY__, "SampleTestProjects")

        let! server, _ = initializeServer workspaceRoot

        use server = server

        Workspace.build workspaceRoot

        let! response =
          server.TestRunTests(
            { LimitToProjects =
                Some
                  [ Path.Combine(
                      __SOURCE_DIRECTORY__,
                      "SampleTestProjects",
                      "VSTest.XUnit.RunResults",
                      "VSTest.XUnit.RunResults.fsproj"
                    ) ]
              TestCaseFilter = None
              AttachDebugger = false }
          )

        let expected = ExpectedTests.XUnitRunResults

        let actual =
          TestRunResult.tryUnwrapTestRunResult response
          |> List.map (fun tr -> tr.TestItem.FullName, tr.Outcome)

        Expect.equal (set actual) (set expected) ""
      }
      testCaseAsync "it should only attach the debugger for projects in the project filter if filter is specified"
      <| async {
        let workspaceRoot = Path.Combine(__SOURCE_DIRECTORY__, "SampleTestProjects")

        let! server, clientNotifications = initializeServer workspaceRoot

        use server = server

        Workspace.build workspaceRoot

        use tokenSource = new CancellationTokenSource()
        let mutable processIdSpy: int list = []
        use! _onCancel = Async.OnCancel(fun () -> tokenSource.Cancel())

        use _ =
          clientNotifications.Subscribe(fun (msgType: string, data: obj) ->
            if msgType = "test/processWaitingForDebugger" then
              let processId: int =
                data :?> PlainNotification
                |> _.Content
                |> FsAutoComplete.JsonSerializer.readJson

              processIdSpy <- processId :: processIdSpy
              tokenSource.Cancel())

        Expect.throwsT<System.OperationCanceledException>
          (fun () ->
            let runRequest: TestRunRequest =
              { LimitToProjects =
                  Some
                    [ Path.Combine(
                        __SOURCE_DIRECTORY__,
                        "SampleTestProjects",
                        "VSTest.XUnit.RunResults",
                        "VSTest.XUnit.RunResults.fsproj"
                      ) ]
                TestCaseFilter = None
                AttachDebugger = true }

            Async.RunSynchronously(
              server.TestRunTests(runRequest) |> Async.Ignore,
              cancellationToken = tokenSource.Token
            ))
          ""

        Expect.hasLength processIdSpy 1 "Should only launch one process to debug"
      } ]
