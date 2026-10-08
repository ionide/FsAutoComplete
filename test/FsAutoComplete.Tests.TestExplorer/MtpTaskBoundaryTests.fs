module MtpTaskBoundaryTests

open Expecto
open System
open System.Collections.Concurrent
open System.Threading
open System.Threading.Tasks
open FsAutoComplete.TestServer

type private Outcome<'T> =
  | Completed of 'T
  | Failed of exn
  | Cancelled

/// Runs <c>computation</c> under a token it may cancel through the source it is given, and reports
/// which continuation the async took. Fails rather than hangs if no continuation is ever called.
let private runCancellable (computation: CancellationTokenSource -> Async<'T>) =
  use cancellation = new CancellationTokenSource()
  let outcome = TaskCompletionSource<Outcome<'T>>()

  Async.StartWithContinuations(
    computation cancellation,
    (fun result -> outcome.SetResult(Completed result)),
    (fun error -> outcome.SetResult(Failed error)),
    (fun _ -> outcome.SetResult Cancelled),
    cancellation.Token
  )

  if not (outcome.Task.Wait(TimeSpan.FromSeconds 10.0)) then
    failtest "the async reported no outcome"

  outcome.Task.Result

/// Cancels the token as soon as <c>start</c> has started its task, before the async side could
/// observe it.
let private cancelOnceStarted (cancellation: CancellationTokenSource) (start: CancellationToken -> Task<'T>) =
  fun token ->
    let started = start token
    cancellation.Cancel()
    started

let private startThenCancel (start: CancellationToken -> Task<'T>) =
  runCancellable (fun cancellation -> MtpWrapper.startTaskAsync (cancelOnceStarted cancellation start))

/// Stands in for a launched client, recording what happens to it in order.
type private ResourceSpy() =
  let events = ConcurrentQueue<string>()
  member _.Record event = events.Enqueue event
  member _.Events = List.ofSeq events

  interface IDisposable with
    member _.Dispose() = events.Enqueue "disposed"

let private acquiredAfterDelay (spy: ResourceSpy) =
  fun (_: CancellationToken) ->
    task {
      do! Task.Delay 100
      return spy
    }

let private release (spy: ResourceSpy) =
  spy.Record "released"
  Task.CompletedTask

[<Tests>]
let tests =
  testList
    "MtpWrapper.startTaskAsync"
    [ testCase "a result the task produces after a racing cancellation is not dropped"
      <| fun _ ->
        let outcome =
          startThenCancel (fun _ ->
            task {
              do! Task.Delay 100
              return 42
            })

        Expect.equal outcome (Completed 42) "the finished task's result reaches the caller"

      testCase "request cancellation is reported as cancellation once the task has finished"
      <| fun _ ->
        let mutable finished = false

        let outcome =
          startThenCancel (fun token ->
            task {
              try
                do! Task.Delay 100
                token.ThrowIfCancellationRequested()
              finally
                finished <- true
            })

        Expect.equal outcome Cancelled "cancellation of the request is async cancellation"
        Expect.isTrue finished "the task finished before cancellation was reported"

      testCase "a failure surfaces as the exception the task raised"
      <| fun _ ->
        let failure = InvalidOperationException "launch failed"

        let outcome =
          MtpWrapper.startTaskAsync (fun _ -> Task.FromException<int> failure)
          |> Async.Catch
          |> Async.RunSynchronously

        match outcome with
        | Choice2Of2 error -> Expect.isTrue (obj.ReferenceEquals(error, failure)) $"not wrapped: {error}"
        | Choice1Of2 result -> failtest $"expected the failure, got {result}"

      testCase "a cancellation the request did not ask for is a failure"
      <| fun _ ->
        let outcome =
          MtpWrapper.startTaskAsync (fun _ -> Task.FromCanceled<int>(CancellationToken(true)))
          |> Async.Catch
          |> Async.RunSynchronously

        match outcome with
        | Choice2Of2(:? OperationCanceledException) -> ()
        | other -> failtest $"expected an OperationCanceledException failure, got {other}" ]

[<Tests>]
let ownershipTests =
  testList
    "MtpWrapper.ownedAsync"
    [ testCase "a resource acquired under a racing cancellation is released and disposed"
      <| fun _ ->
        let spy = new ResourceSpy()

        let outcome =
          runCancellable (fun cancellation ->
            MtpWrapper.ownedAsync
              (cancelOnceStarted cancellation (acquiredAfterDelay spy))
              release
              (fun token resource ->
                task {
                  token.ThrowIfCancellationRequested()
                  resource.Record "worked"
                  return 42
                }))

        Expect.equal outcome Cancelled "cancellation of the request is async cancellation"
        Expect.equal spy.Events [ "released"; "disposed" ] "the resource was released, then disposed, once"

      testCase "the work's result is reported once the resource is released and disposed"
      <| fun _ ->
        let spy = new ResourceSpy()

        let outcome =
          runCancellable (fun _ ->
            MtpWrapper.ownedAsync (acquiredAfterDelay spy) release (fun _ resource ->
              task {
                resource.Record "worked"
                return 42
              }))

        Expect.equal outcome (Completed 42) "the work's result reaches the caller"
        Expect.equal spy.Events [ "worked"; "released"; "disposed" ] "the resource outlives the work only"

      testCase "a failing release leaves the work's failure as the outcome"
      <| fun _ ->
        let spy = new ResourceSpy()
        let failure = InvalidOperationException "work failed"

        let outcome =
          runCancellable (fun _ ->
            MtpWrapper.ownedAsync
              (acquiredAfterDelay spy)
              (fun _ -> Task.FromException(InvalidOperationException "release failed"))
              (fun _ _ -> Task.FromException<int> failure))

        match outcome with
        | Failed error -> Expect.isTrue (obj.ReferenceEquals(error, failure)) $"not the work's failure: {error}"
        | other -> failtest $"expected the work's failure, got {other}"

        Expect.equal spy.Events [ "disposed" ] "the resource was disposed although its release failed" ]
