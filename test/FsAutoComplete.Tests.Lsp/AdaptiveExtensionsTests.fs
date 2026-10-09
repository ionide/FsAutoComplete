module FsAutoComplete.Tests.Lsp.AdaptiveExtensionsTests

open System
open System.Threading.Tasks
open Expecto
open FSharp.Data.Adaptive
open FsAutoComplete.Adaptive

let tests =
  testList
    "asyncaval.AddCallback"
    [ testCaseAsync "a value that fails to compute does not stop later values"
      <| async {
        let source = cval 0
        let firstFailed = TaskCompletionSource()

        let value =
          source
          |> AsyncAVal.ofAVal
          |> AsyncAVal.mapAsync (fun v ->
            async {
              do! Async.Sleep 10

              if v = 0 then
                firstFailed.TrySetResult() |> ignore
                failwith "the first value fails"

              return v
            })

        let nextValue = TaskCompletionSource<int>()

        use _ =
          value.AddCallback(false, fun v -> async { nextValue.TrySetResult v |> ignore })

        do! firstFailed.Task.WaitAsync(TimeSpan.FromSeconds 5.) |> Async.AwaitTask
        transact (fun () -> source.Value <- 1)

        let! next = nextValue.Task.WaitAsync(TimeSpan.FromSeconds 5.) |> Async.AwaitTask
        Expect.equal next 1 "The callback receives the next value"
      } ]
