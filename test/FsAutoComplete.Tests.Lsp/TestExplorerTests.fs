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
    [ testCaseAsync "it should report tests of all basic outcomes"
      <| async {
        let workspaceRoot =
          Path.Combine(__SOURCE_DIRECTORY__, "..", "SampleTestProjects", "VSTest.XUnit.RunResults")

        let! server, _ = initializeServer workspaceRoot
        use server = server

        let buildResult = DotnetCli.build workspaceRoot
        Expect.equal 0 buildResult.ExitCode $"Build failed with: {buildResult.StdErr}"

        let runRequest: TestRunRequest =
          { TestCaseFilter = None
            AttachDebugger = false }

        let! res = server.TestRunTests(runRequest)

        let actual =
          TestRunResult.tryUnwrapTestRunResult res
          |> List.map (fun tr -> tr.TestItem.FullName, tr.Outcome)

        let expected =
          [ "Tests.My test", FsAutoComplete.TestServer.TestOutcome.Passed
            "Tests.Fails", FsAutoComplete.TestServer.TestOutcome.Failed
            "Tests.Skipped", FsAutoComplete.TestServer.TestOutcome.Skipped
            "Tests.Exception", FsAutoComplete.TestServer.TestOutcome.Failed
            "Tests+Nested.Test 1", FsAutoComplete.TestServer.TestOutcome.Passed
            "Tests+Nested.Test 2", FsAutoComplete.TestServer.TestOutcome.Passed
            "Tests.Expects environment variable", FsAutoComplete.TestServer.TestOutcome.Failed ]

        Expect.equal (set actual) (set expected) ""
      }

      testCaseAsync "it should report a processId when debugging a test project"
      <| async {
        let workspaceRoot =
          Path.Combine(__SOURCE_DIRECTORY__, "..", "SampleTestProjects", "VSTest.XUnit.RunResults")

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
              { TestCaseFilter = None
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
          Path.Combine(__SOURCE_DIRECTORY__, "..", "SampleTestProjects", "VSTest.XUnit.RunResults")

        let! server, _ = initializeServer workspaceRoot

        use server = server

        let buildResult = DotnetCli.build workspaceRoot
        Expect.equal 0 buildResult.ExitCode $"Build failed with: {buildResult.StdErr}"

        System.Environment.SetEnvironmentVariable("dd586685-08f6-410c-a9f1-84530af117ab", "Set me")

        let! response =
          server.TestRunTests(
            { TestCaseFilter = Some "FullyQualifiedName~Tests.Expects environment variable"
              AttachDebugger = false }
          )

        let expected =
          [ "Tests.Expects environment variable", FsAutoComplete.TestServer.TestOutcome.Passed ]

        let actual =
          TestRunResult.tryUnwrapTestRunResult response
          |> List.map (fun tr -> tr.TestItem.FullName, tr.Outcome)

        Expect.equal (set actual) (set expected) ""

        System.Environment.SetEnvironmentVariable("dd586685-08f6-410c-a9f1-84530af117ab", "")
      } ]
