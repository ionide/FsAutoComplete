module MtpRunTests

open Expecto
open System
open System.Diagnostics
open System.IO
open System.Threading
open System.Threading.Tasks
open FsAutoComplete.TestServer
open FsAutoComplete.TestingPlatform.Client

let private sampleApp =
  Path.Combine(ResourceLocators.sampleProjectsRootDir, "Mtp.XUnit/bin/Debug/net8.0/Mtp.XUnit.dll")

let private nameOf (node: TestNodeUpdate) = node.DisplayName |> Option.defaultValue node.Uid

let private runAll () = MtpWrapper.runTestsAsync ignore [ sampleApp, MtpWrapper.TestSelection.All ]

let private byName name (results: MtpWrapper.RunNode list) =
  results |> List.find (fun (_, node) -> nameOf node = name) |> snd

/// Collects the messages logged at error level.
let private errorsInto (errors: ResizeArray<string>) =
  function
  | MtpWrapper.TestRunUpdate.LogMessage(ClientLogLevel.Error, message) -> errors.Add message
  | _ -> ()

[<Tests>]
let tests =
  testList
    "MtpWrapper Test Runs"
    [ testCaseAsync "runs nothing when no application is named"
      <| async {
        let! actual = MtpWrapper.runTestsAsync ignore []
        Expect.isEmpty actual "no application runs no tests"
      }

      testCaseAsync "reports a passing test as passed"
      <| async {
        let! results = runAll ()

        Expect.equal (byName "Tests.My test" results).ExecutionState (Some ExecutionState.Passed) "the test passed"
      }

      testCaseAsync "reports a failing test as failed"
      <| async {
        let! results = runAll ()

        Expect.equal (byName "Tests.Fails" results).ExecutionState (Some ExecutionState.Failed) "the test failed"
      }

      testCaseAsync "explains a failure"
      <| async {
        let! results = runAll ()
        let failed = byName "Tests.Fails" results

        Expect.isSome failed.Error "the server says why the test failed"
      }

      testCaseAsync "carries what a test printed through to its result"
      <| async {
        let! results = runAll ()
        let printed = byName "Tests.Writes to stdout" results
        let actual = TestResult.ofMtpNode "Mtp.XUnit.fsproj" "net8.0" printed

        Expect.stringContains
          (actual.AdditionalOutput |> Option.defaultValue "")
          "Where do I show up in the results"
          "the result keeps the test's console output"
      }

      testCaseAsync "reports a skipped test as skipped"
      <| async {
        let! results = runAll ()

        Expect.equal
          (byName "Tests.Skipped" results).ExecutionState
          (Some ExecutionState.Skipped)
          "the test was skipped"
      }

      testCaseAsync "pairs every result with the application it came from"
      <| async {
        let! results = runAll ()

        Expect.all results (fun (source, _) -> source = sampleApp) "each result names its application"
      }

      testCaseAsync "reports progress as the server runs"
      <| async {
        let seen = ResizeArray()

        let! results =
          MtpWrapper.runTestsAsync
            (function
            | MtpWrapper.TestRunUpdate.Progress nodes -> seen.AddRange nodes
            | MtpWrapper.TestRunUpdate.LogMessage _ -> ())
            [ sampleApp, MtpWrapper.TestSelection.All ]

        Expect.equal (List.ofSeq seen) results "progress reports the same nodes as the result"
      }

      testCaseAsync "requests debugger attachment before running the test application"
      <| async {
        let mutable processId = None

        let! results =
          MtpWrapper.runTestsWithDebuggerAsync
            ignore
            (Some(fun pid ->
              use testProcess = System.Diagnostics.Process.GetProcessById(pid)
              Expect.isFalse testProcess.HasExited "the named process is alive before tests execute"
              processId <- Some pid
              false))
            [ sampleApp, MtpWrapper.TestSelection.All ]

        Expect.isSome processId "the debugger was asked to attach to the test application"
        Expect.isNonEmpty results "the run still completes when attachment is declined"
      }

      testCaseAsync "cancelling an executing test stops the run and its host"
      <| async {
        let! discovered = MtpWrapper.discoverTestsAsync ignore [ sampleApp ]

        let uid =
          discovered
          |> List.find (fun (_, node) -> nameOf node = "Tests.Waits for cancellation")
          |> snd
          |> _.Uid

        use cancellation = new CancellationTokenSource()

        let launched =
          TaskCompletionSource<Process>(TaskCreationOptions.RunContinuationsAsynchronously)

        let mutable gate = None

        let run =
          MtpWrapper.runTestsWithDebuggerAsync
            ignore
            (Some(fun pid ->
              let path = Path.Combine(Path.GetTempPath(), $"fsac-mtp-cancellation-{pid}")
              gate <- Some path
              File.WriteAllText(path, "wait")
              launched.SetResult(Process.GetProcessById pid)
              false))
            [ sampleApp, MtpWrapper.TestSelection.Uids [ uid ] ]
          |> fun operation -> Async.StartAsTask(operation, cancellationToken = cancellation.Token)

        try
          let! testProcess = launched.Task.WaitAsync(TimeSpan.FromSeconds 30.0) |> Async.AwaitTask
          let started = gate.Value + ".started"
          let elapsed = Stopwatch.StartNew()

          while not (File.Exists started) && elapsed.Elapsed < TimeSpan.FromSeconds 30.0 do
            do! Async.Sleep 25

          Expect.isTrue (File.Exists started) "the test body started before cancellation"
          Expect.isFalse run.IsCompleted "the test is still executing when cancellation is requested"
          cancellation.Cancel()

          let! completed = Task.WhenAny(run :> Task, Task.Delay 5000) |> Async.AwaitTask

          Expect.isTrue
            (obj.ReferenceEquals(completed, run))
            "cancelling an executing MTP test must complete the run within five seconds"

          Expect.isTrue run.IsCanceled $"the run completes as cancelled; status: {run.Status}; error: {run.Exception}"
          Expect.isTrue (testProcess.WaitForExit 5000) "cancellation terminates the test host"
        finally
          cancellation.Cancel()

          if launched.Task.IsCompletedSuccessfully then
            use testProcess = launched.Task.Result

            if not testProcess.HasExited then
              testProcess.Kill(true)
              testProcess.WaitForExit(5000) |> ignore

          gate
          |> Option.iter (fun path ->
            File.Delete path
            File.Delete(path + ".started"))
      }

      testCaseAsync "a failing debugger attachment surfaces its exception and stops the host"
      <| async {
        let failure = InvalidOperationException "attach failed"
        let mutable host = None

        try
          let! outcome =
            MtpWrapper.runTestsWithDebuggerAsync
              ignore
              (Some(fun pid ->
                // The host sits idle in server mode after this throws, so only teardown ends it.
                host <- Some(Process.GetProcessById pid)
                raise failure))
              [ sampleApp, MtpWrapper.TestSelection.All ]
            |> Async.Catch

          match outcome with
          | Choice2Of2 error ->
            Expect.isTrue (obj.ReferenceEquals(error, failure)) $"not the original exception: {error}"
          | Choice1Of2 results -> failtest $"expected the attach failure, got {results}"

          Expect.isSome host "the debugger was asked to attach"
          Expect.isTrue (host.Value.WaitForExit 5000) "the failed run terminates the test host"
        finally
          host
          |> Option.iter (fun testProcess ->
            if not testProcess.HasExited then
              testProcess.Kill(true)
              testProcess.WaitForExit(5000) |> ignore

            testProcess.Dispose())
      }

      testCaseAsync "runs only the tests it is asked to run"
      <| async {
        let! discovered = MtpWrapper.discoverTestsAsync ignore [ sampleApp ]

        let uid =
          discovered
          |> List.find (fun (_, node) -> nameOf node = "Tests.Fails")
          |> snd
          |> _.Uid

        let! results = MtpWrapper.runTestsAsync ignore [ sampleApp, MtpWrapper.TestSelection.Uids [ uid ] ]

        let names =
          results
          |> List.filter (fun (_, node) -> node.ExecutionState <> Some ExecutionState.InProgress)
          |> List.map (snd >> nameOf)
          |> List.distinct

        Expect.equal names [ "Tests.Fails" ] "only the named test ran"
      }

      testCaseAsync "a uid the application does not know runs nothing"
      <| async {
        let! results = MtpWrapper.runTestsAsync ignore [ sampleApp, MtpWrapper.TestSelection.Uids [ "deadbeef" ] ]

        Expect.isEmpty results "an application runs only the tests it recognises"
      }

      testCaseAsync "an explicitly empty uid selection runs no tests"
      <| async {
        let! results = MtpWrapper.runTestsAsync ignore [ sampleApp, MtpWrapper.TestSelection.Uids [] ]
        Expect.isEmpty results "an empty selection does not turn into a request to run all tests"
      }

      testCaseAsync "an application that does not exist is reported and the others still run"
      <| async {
        let errors = ResizeArray()

        let! results =
          MtpWrapper.runTestsAsync
            (errorsInto errors)
            [ ResourceLocators.missingApp, MtpWrapper.TestSelection.All
              sampleApp, MtpWrapper.TestSelection.All ]

        Expect.equal
          (byName "Tests.My test" results).ExecutionState
          (Some ExecutionState.Passed)
          "the built application still ran"

        Expect.all results (fun (source, _) -> source = sampleApp) "the missing application reports no results"

        Expect.exists
          errors
          (fun message -> message.Contains ResourceLocators.missingApp)
          $"an error names the missing application; errors: {List.ofSeq errors}"
      }

      testCaseAsync "an application that fails to start is reported and the others still run"
      <| ResourceLocators.withBrokenApp (fun brokenApp ->
        async {
          let errors = ResizeArray()

          let! results =
            MtpWrapper.runTestsAsync
              (errorsInto errors)
              [ brokenApp, MtpWrapper.TestSelection.All
                sampleApp, MtpWrapper.TestSelection.All ]

          Expect.equal
            (byName "Tests.My test" results).ExecutionState
            (Some ExecutionState.Passed)
            "the working application still ran"

          Expect.all results (fun (source, _) -> source = sampleApp) "the broken application reports no results"

          Expect.exists
            errors
            (fun message -> message.Contains brokenApp)
            $"an error names the broken application; errors: {List.ofSeq errors}"
        })

      testCaseAsync "a run in which no application could run fails, after reporting each"
      <| ResourceLocators.withBrokenApp (fun brokenApp ->
        async {
          let errors = ResizeArray()

          let! outcome =
            MtpWrapper.runTestsAsync
              (errorsInto errors)
              [ ResourceLocators.missingApp, MtpWrapper.TestSelection.All
                brokenApp, MtpWrapper.TestSelection.All ]
            |> Async.Catch

          match outcome with
          | Choice1Of2 results -> failtest $"expected the run to fail, got {results}"
          | Choice2Of2 _ -> ()

          Expect.exists
            errors
            (fun message -> message.Contains ResourceLocators.missingApp)
            "the missing application is reported"

          Expect.exists errors (fun message -> message.Contains brokenApp) "the broken application is reported"
        }) ]
