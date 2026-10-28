module FsAutoComplete.Tests.SharedProgressReporterTests

open Expecto
open System
open System.Collections.Concurrent
open FsAutoComplete.Lsp
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc
open Newtonsoft.Json.Linq

/// Captured progress call from the mock client
type ProgressCall =
  | Begin of title: string * message: string option * percentage: uint32 option
  | Report of message: string option * percentage: uint32 option
  | End

/// Creates a mock FSharpLspClient that captures all progress notifications
let private createMockClient () =
  let progressCalls = ConcurrentQueue<ProgressCall>()

  let notificationSender: ClientNotificationSender =
    fun methodName payload ->
      if methodName = "$/progress" then
        let value = (payload :?> ProgressParams).Value

        let tryGet (name: string) =
          match value.[name] with
          | null -> None
          | token -> Some token

        let message = tryGet "message" |> Option.map (fun t -> t.Value<string>())
        let percentage = tryGet "percentage" |> Option.map (fun t -> t.Value<uint32>())

        match value.Value<string>("kind") with
        | "begin" -> progressCalls.Enqueue(Begin(value.Value<string>("title"), message, percentage))
        | "report" -> progressCalls.Enqueue(Report(message, percentage))
        | "end" -> progressCalls.Enqueue End
        | kind -> failwith $"Unexpected progress kind {kind}"

      AsyncLspResult.success ()

  let requestSender =
    { new ClientRequestSender with
        member _.Send _name _payload = AsyncLspResult.success Unchecked.defaultof<_>
    }

  let client = new FSharpLspClient(notificationSender, requestSender)

  client.ClientCapabilities <-
    Some
      {
        Workspace = None
        TextDocument = None
        Experimental = None
        Window =
          Some
            {
              WorkDoneProgress = Some true
              ShowMessage = None
              ShowDocument = None
            }
        General = None
        NotebookDocument = None
      }

  client, progressCalls

type private TestReporter =
  {
    Reporter: SharedProgressReporter
    Calls: ConcurrentQueue<ProgressCall>
    Reports: ConcurrentQueue<ServerProgressReport>
  }

  /// Waits until the client has been sent the current state, then returns all calls so far.
  member x.Flush() =
    async {
      do! x.Reporter.Flush() |> Async.AwaitTask
      return x.Calls.ToArray() |> Array.toList
    }

let private createReporter (isEnabled: bool) =
  let client, calls = createMockClient ()
  let reports = ConcurrentQueue<ServerProgressReport>()

  let reporter =
    new SharedProgressReporter(
      "Typechecking",
      (fun () ->
        let report = new ServerProgressReport(client, cancellableDefault = true)
        reports.Enqueue report
        report),
      isEnabled = fun () -> isEnabled
    )

  {
    Reporter = reporter
    Calls = calls
    Reports = reports
  }

let private dispose (d: IDisposable) = d.Dispose()

/// The message and percentage the client shows after the given calls.
let private lastState calls =
  match List.last calls with
  | Begin(_, message, percentage)
  | Report(message, percentage) -> Some(message, percentage)
  | End -> None

let operationTests =
  testList
    "Operations"
    [
      testCaseAsync "One operation begins and ends a report"
      <| async {
        let t = createReporter true
        let scope = t.Reporter.Begin "Foo.fs"
        let! calls = t.Flush()
        Expect.equal calls [ Begin("Typechecking", Some "Foo.fs", None) ] "The report should begin with the label"

        dispose scope
        let! calls = t.Flush()
        Expect.equal (List.last calls) End "The report should end with the operation"
        Expect.equal t.Reports.Count 1 "One report should be created"
      }

      testCaseAsync "Operations that start after the previous one ended use a new report"
      <| async {
        let t = createReporter true
        let first = t.Reporter.Begin "A.fs"
        let! _ = t.Flush()
        dispose first
        let! _ = t.Flush()

        let second = t.Reporter.Begin "B.fs"
        let! _ = t.Flush()
        dispose second
        let! calls = t.Flush()

        Expect.equal
          calls
          [
            Begin("Typechecking", Some "A.fs", None)
            End
            Begin("Typechecking", Some "B.fs", None)
            End
          ]
          "Each operation should have its own report cycle"

        Expect.equal t.Reports.Count 2 "Two reports should be created"
      }

      testCaseAsync "Concurrent operations share one report until the last ends"
      <| async {
        let t = createReporter true
        let a = t.Reporter.Begin "A.fs"
        let b = t.Reporter.Begin "B.fs"
        let! calls = t.Flush()
        Expect.equal (lastState calls) (Some(Some "A.fs, B.fs", None)) "Both labels should show"

        dispose a
        let! calls = t.Flush()
        Expect.equal (List.last calls) (Report(Some "B.fs", None)) "Only the running operation should show"

        dispose b
        let! calls = t.Flush()
        Expect.equal (List.last calls) End "The report should end with the last operation"
        Expect.equal t.Reports.Count 1 "One report should be shared"
      }

      testCaseAsync "Operations with the same label keep the report open until both end"
      <| async {
        let t = createReporter true
        let first = t.Reporter.Begin "Foo.fs"
        let second = t.Reporter.Begin "Foo.fs"
        let! calls = t.Flush()
        Expect.equal (lastState calls) (Some(Some "Foo.fs", None)) "The label should show once"

        dispose first
        let! calls = t.Flush()
        Expect.isFalse (List.contains End calls) "The report should stay open for the second operation"

        dispose second
        let! calls = t.Flush()
        Expect.equal (List.last calls) End "The report should end with the second operation"
      }

      testCaseAsync "Disposing a scope twice ends the operation once"
      <| async {
        let t = createReporter true
        let first = t.Reporter.Begin "Foo.fs"
        let second = t.Reporter.Begin "Foo.fs"
        dispose first
        dispose first
        let! calls = t.Flush()
        Expect.isFalse (List.contains End calls) "The second operation still runs"
        dispose second
      }

      testCaseAsync "More than three operations show the first three and a count"
      <| async {
        let t = createReporter true
        let scopes = [ "A.fs"; "B.fs"; "C.fs"; "D.fs"; "E.fs" ] |> List.map t.Reporter.Begin
        let! calls = t.Flush()

        Expect.equal (lastState calls) (Some(Some "A.fs, B.fs, C.fs (+2 more)", None)) "The message should be truncated"

        scopes |> List.iter dispose
      }

      testCaseAsync "Disposing the reporter ends the shown report"
      <| async {
        let t = createReporter true
        let _scope = t.Reporter.Begin "Foo.fs"
        let! _ = t.Flush()
        dispose t.Reporter
        let! calls = t.Flush()
        Expect.equal (List.last calls) End "The report should end"

        let _afterDispose = t.Reporter.Begin "Bar.fs"
        let _batchAfterDispose = t.Reporter.BeginBatch 2
        let! callsAfter = t.Flush()
        Expect.equal callsAfter calls "A disposed reporter should start no new report"
        Expect.equal t.Reports.Count 1 "No report should be created after dispose"
      }
    ]

let batchTests =
  testList
    "Batches"
    [
      testCaseAsync "A batch shows how many of its operations completed"
      <| async {
        let t = createReporter true
        let batch = t.Reporter.BeginBatch 4
        let! calls = t.Flush()
        Expect.equal calls [ Begin("Typechecking", Some "0/4 completed", Some 0u) ] "The batch should begin at 0%"

        let first = batch.Begin()
        let! calls = t.Flush()
        Expect.equal calls.Length 1 "A batch operation has no label of its own"

        let work = t.Reporter.Begin "A.fs"
        let! calls = t.Flush()

        Expect.equal (List.last calls) (Report(Some "A.fs (0/4 completed)", Some 0u)) "The running work should show"

        dispose work
        dispose first
        let! calls = t.Flush()
        Expect.equal (List.last calls) (Report(Some "1/4 completed", Some 25u)) "One of four operations completed"

        dispose batch
        let! calls = t.Flush()
        Expect.equal (List.last calls) End "The report should end with the batch"
      }

      testCaseAsync "Operations outside the batch do not count towards it"
      <| async {
        let t = createReporter true
        let batch = t.Reporter.BeginBatch 2
        let outside = t.Reporter.Begin "Outside.fs"
        dispose outside
        let! calls = t.Flush()
        Expect.equal (lastState calls) (Some(Some "0/2 completed", Some 0u)) "Only batch operations should count"

        dispose batch
      }

      testCaseAsync "Batch operations count separately"
      <| async {
        let t = createReporter true
        let batch = t.Reporter.BeginBatch 2
        let first = batch.Begin()
        let second = batch.Begin()
        dispose first
        let! calls = t.Flush()
        Expect.equal (lastState calls) (Some(Some "1/2 completed", Some 50u)) "One operation completed"

        dispose second
        let! calls = t.Flush()
        Expect.equal (List.last calls) (Report(Some "2/2 completed", Some 100u)) "Both operations completed"
        dispose batch
      }

      testCaseAsync "Overlapping batches add up"
      <| async {
        let t = createReporter true
        let first = t.Reporter.BeginBatch 2
        let second = t.Reporter.BeginBatch 2
        first.Begin() |> dispose
        second.Begin() |> dispose
        let! calls = t.Flush()
        Expect.equal (lastState calls) (Some(Some "2/4 completed", Some 50u)) "Both batches should count"

        dispose first
        dispose second
        let! calls = t.Flush()
        Expect.equal (List.last calls) End "The report should end with the last batch"
      }

      testCaseAsync "A new batch starts from zero when the previous one ended"
      <| async {
        let t = createReporter true
        // Keeps the report open between the two batches.
        let keepOpen = t.Reporter.Begin "Open.fs"
        let first = t.Reporter.BeginBatch 2
        first.Begin() |> dispose
        dispose first
        let! callsBefore = t.Flush()

        let second = t.Reporter.BeginBatch 3
        let! calls = t.Flush()
        let newCalls = calls |> List.skip callsBefore.Length

        Expect.equal
          newCalls
          [ Report(Some "Open.fs (0/3 completed)", Some 0u) ]
          "The second batch should not keep the counts of the first"

        dispose second
        dispose keepOpen
      }

      testCaseAsync "A batch that ends early stops counting its operations that did not complete"
      <| async {
        let t = createReporter true
        let aborted = t.Reporter.BeginBatch 3
        let running = t.Reporter.BeginBatch 2
        aborted.Begin() |> dispose
        let stillRunning = aborted.Begin()
        dispose aborted
        let! calls = t.Flush()
        Expect.equal (lastState calls) (Some(Some "1/3 completed", Some 33u)) "Only the completed operation should stay"

        dispose stillRunning
        let! calls = t.Flush()
        Expect.equal (lastState calls) (Some(Some "1/3 completed", Some 33u)) "A late operation should not count"

        running.Begin() |> dispose
        let! calls = t.Flush()
        Expect.equal (lastState calls) (Some(Some "2/3 completed", Some 66u)) "The running batch should count"
        dispose running
      }

      testCaseAsync "An empty batch shows nothing"
      <| async {
        let t = createReporter true
        let batch = t.Reporter.BeginBatch 0
        let! calls = t.Flush()
        Expect.isEmpty calls "No progress should be sent"
        Expect.equal t.Reports.Count 0 "No report should be created"
        dispose batch
      }
    ]

let cancellationTests =
  testList
    "Cancellation"
    [
      testCaseAsync "The scope token is cancelled when the user cancels the report"
      <| async {
        let t = createReporter true
        let scope = t.Reporter.Begin "Foo.fs"
        Expect.isFalse scope.CancellationToken.IsCancellationRequested "The token should not start cancelled"
        t.Reports.ToArray().[0].Cancel()
        Expect.isTrue scope.CancellationToken.IsCancellationRequested "The token should follow the report"
        dispose scope
      }

      testCaseAsync "An operation that starts after a cancel gets a new report and token"
      <| async {
        let t = createReporter true
        let cancelled = t.Reporter.Begin "A.fs"
        let! _ = t.Flush()
        t.Reports.ToArray().[0].Cancel()

        let fresh = t.Reporter.Begin "B.fs"
        Expect.isFalse fresh.CancellationToken.IsCancellationRequested "The new operation should not be cancelled"
        Expect.equal t.Reports.Count 2 "A new report should be created"

        let! calls = t.Flush()

        Expect.equal
          calls
          [
            Begin("Typechecking", Some "A.fs", None)
            End
            Begin("Typechecking", Some "B.fs", None)
          ]
          "The cancelled report should end and the new one begin"

        dispose cancelled
        let! calls = t.Flush()
        Expect.equal (List.last calls) (Begin("Typechecking", Some "B.fs", None)) "The old operation is not shown"

        dispose fresh
        let! calls = t.Flush()
        Expect.equal (List.last calls) End "The new report should end"
      }

      testCaseAsync "Batch operations that start after a cancel get the cancelled token"
      <| async {
        let t = createReporter true
        let batch = t.Reporter.BeginBatch 2
        let! _ = t.Flush()
        t.Reports.ToArray().[0].Cancel()

        let operation = batch.Begin()
        Expect.isTrue operation.CancellationToken.IsCancellationRequested "The batch was cancelled"
        Expect.equal t.Reports.Count 1 "No new report should be created for the batch"

        dispose operation
        dispose batch
        let! calls = t.Flush()
        Expect.equal (List.last calls) End "The report should end with the batch"
      }
    ]

let disabledTests =
  testList
    "Disabled"
    [
      testCaseAsync "A disabled reporter sends nothing and never cancels"
      <| async {
        let t = createReporter false
        let scope = t.Reporter.Begin "Foo.fs"
        let batch = t.Reporter.BeginBatch 2
        let operation = batch.Begin()

        Expect.equal scope.CancellationToken Threading.CancellationToken.None "No token for an operation"
        Expect.equal batch.CancellationToken Threading.CancellationToken.None "No token for a batch"
        Expect.equal operation.CancellationToken Threading.CancellationToken.None "No token for a batch operation"

        dispose operation
        dispose batch
        dispose scope
        let! calls = t.Flush()
        Expect.isEmpty calls "No progress should be sent"
        Expect.equal t.Reports.Count 0 "No report should be created"
      }
    ]

let tests =
  testList "SharedProgressReporter" [ operationTests; batchTests; cancellationTests; disabledTests ]
