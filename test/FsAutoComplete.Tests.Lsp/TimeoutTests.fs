module FsAutoComplete.Tests.Lsp.TimeoutTests

open System
open System.Threading
open Expecto

let private run (code: TestCode) =
  match code with
  | TestCode.Async test -> test
  | _ -> failwith "cancelOnTimeout should keep an asynchronous test asynchronous"

let tests =
  testList
    "cancelOnTimeout"
    [ testCaseAsync "a test that runs too long fails as a timeout and stops"
      <| async {
        let steps = ref 0

        let slow =
          TestCode.Async(
            async {
              while true do
                do! Async.Sleep 20
                Interlocked.Increment(&steps.contents) |> ignore
            }
          )

        let! result =
          slow
          |> Helpers.Expecto.cancelOnTimeout (TimeSpan.FromMilliseconds 200.)
          |> run
          |> Async.Catch

        match result with
        | Choice2Of2(:? AssertException as e) ->
          Expect.stringContains e.Message "Timeout" "The test fails as a timeout"
          // Expecto fails the whole run on an AssertException without a stack trace.
          Expect.isNotNull e.StackTrace "The timeout is raised, not only created"
        | other -> failtestf "Expected a timeout, got %A" other

        let stepsAtTimeout = steps.Value
        do! Async.Sleep 200
        Expect.equal steps.Value stepsAtTimeout "The test does not run on after the timeout"
      }

      testCaseAsync "a test that finishes in time keeps its own outcome"
      <| async {
        let failing = TestCode.Async(async { failwith "the test's own failure" })

        let! result =
          failing
          |> Helpers.Expecto.cancelOnTimeout (TimeSpan.FromSeconds 10.)
          |> run
          |> Async.Catch

        match result with
        | Choice2Of2 e -> Expect.equal e.Message "the test's own failure" "The test's exception is not replaced"
        | Choice1Of2 () -> failtest "The test should have failed"
      }

      testCaseAsync "a test with two timeouts still gets the time to stop"
      <| async {
        let stopped = ref false

        let blocking =
          TestCode.Async(
            async {
              try
                do! Async.Sleep 20
                // Synchronous, so the cancellation cannot stop it before it ends.
                Thread.Sleep 1000
                do! Async.Sleep 20
              finally
                stopped.Value <- true
            }
          )

        let! result =
          blocking
          |> Helpers.Expecto.cancelOnTimeout (TimeSpan.FromMilliseconds 200.)
          |> Helpers.Expecto.cancelOnTimeout (TimeSpan.FromMilliseconds 200.)
          |> run
          |> Async.Catch

        match result with
        | Choice2Of2(:? AssertException as e) -> Expect.stringContains e.Message "Timeout" "The test fails as a timeout"
        | other -> failtestf "Expected a timeout, got %A" other

        Expect.isTrue stopped.Value "The timeout is reported once the test has stopped"
      } ]
