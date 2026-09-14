module FsAutoComplete.Tests.SharedTypecheckProgressReporterTests

open Expecto
open System
open System.Threading
open System.Threading.Tasks
open System.Collections.Concurrent
open FsAutoComplete.Lsp
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc

/// Captured progress call from the mock client
type ProgressCall =
  | Begin of title: string * message: string option * percentage: uint32 option
  | Report of message: string option * percentage: uint32 option
  | End of message: string option

/// Creates a mock FSharpLspClient that captures all progress notifications
let private createMockClient () =
  let progressCalls = ConcurrentQueue<ProgressCall>()

  let notificationSender: ClientNotificationSender =
    fun methodName payload ->
      match methodName with
      | "$/progress" ->
        let p = payload :?> ProgressParams
        // Parse the progress value to determine the call type
        let json = p.Value.ToString()

        if json.Contains("\"kind\":\"begin\"") || json.Contains("\"kind\": \"begin\"") then
          // Extract title from JSON
          let titleStart = json.IndexOf("\"title\"") + "\"title\"".Length
          let colonAfterTitle = json.IndexOf(":", titleStart) + 1
          let titleQuoteStart = json.IndexOf("\"", colonAfterTitle) + 1
          let titleQuoteEnd = json.IndexOf("\"", titleQuoteStart)
          let title = json.Substring(titleQuoteStart, titleQuoteEnd - titleQuoteStart)

          let message =
            if json.Contains("\"message\"") then
              let msgStart = json.IndexOf("\"message\"") + "\"message\"".Length
              let colonAfterMsg = json.IndexOf(":", msgStart) + 1
              let msgQuoteStart = json.IndexOf("\"", colonAfterMsg) + 1
              let msgQuoteEnd = json.IndexOf("\"", msgQuoteStart)
              Some(json.Substring(msgQuoteStart, msgQuoteEnd - msgQuoteStart))
            else
              None

          let percentage =
            if json.Contains("\"percentage\"") then
              let pctStart = json.IndexOf("\"percentage\"") + "\"percentage\"".Length
              let colonAfterPct = json.IndexOf(":", pctStart) + 1
              // Find the number value after the colon
              let numStr =
                json.Substring(colonAfterPct).TrimStart()
                |> fun s ->
                  let endIdx = s.IndexOfAny([| ','; '}'; ' ' |])

                  if endIdx > 0 then s.Substring(0, endIdx) else s

              match UInt32.TryParse(numStr) with
              | true, v -> Some v
              | _ -> None
            else
              None

          progressCalls.Enqueue(Begin(title, message, percentage))
        elif json.Contains("\"kind\":\"report\"") || json.Contains("\"kind\": \"report\"") then
          let message =
            if json.Contains("\"message\"") then
              let msgStart = json.IndexOf("\"message\"") + "\"message\"".Length
              let colonAfterMsg = json.IndexOf(":", msgStart) + 1
              let msgQuoteStart = json.IndexOf("\"", colonAfterMsg) + 1
              let msgQuoteEnd = json.IndexOf("\"", msgQuoteStart)
              Some(json.Substring(msgQuoteStart, msgQuoteEnd - msgQuoteStart))
            else
              None

          let percentage =
            if json.Contains("\"percentage\"") then
              let pctStart = json.IndexOf("\"percentage\"") + "\"percentage\"".Length
              let colonAfterPct = json.IndexOf(":", pctStart) + 1

              let numStr =
                json.Substring(colonAfterPct).TrimStart()
                |> fun s ->
                  let endIdx = s.IndexOfAny([| ','; '}'; ' ' |])

                  if endIdx > 0 then s.Substring(0, endIdx) else s

              match UInt32.TryParse(numStr) with
              | true, v -> Some v
              | _ -> None
            else
              None

          progressCalls.Enqueue(Report(message, percentage))
        elif json.Contains("\"kind\":\"end\"") || json.Contains("\"kind\": \"end\"") then
          let message =
            if json.Contains("\"message\"") then
              let msgStart = json.IndexOf("\"message\"") + "\"message\"".Length
              let colonAfterMsg = json.IndexOf(":", msgStart) + 1
              let msgQuoteStart = json.IndexOf("\"", colonAfterMsg) + 1
              let msgQuoteEnd = json.IndexOf("\"", msgQuoteStart)
              Some(json.Substring(msgQuoteStart, msgQuoteEnd - msgQuoteStart))
            else
              None

          progressCalls.Enqueue(End message)
      | _ -> ()

      AsyncLspResult.success ()

  let requestSender =
    { new ClientRequestSender with
        member _.Send _name _payload = AsyncLspResult.success Unchecked.defaultof<_> }

  let client = new FSharpLspClient(notificationSender, requestSender)

  client.ClientCapabilities <-
    Some
      { Workspace = None
        TextDocument = None
        Experimental = None
        Window =
          Some
            { WorkDoneProgress = Some true
              ShowMessage = None
              ShowDocument = None }
        General = None
        NotebookDocument = None }

  client, progressCalls

/// Helper: create a reporter with a mock client, returning (reporter, capturedCalls, reportCount)
let private createReporter (title: string) =
  let client, calls = createMockClient ()
  let reportCount = ref 0

  let reporter =
    new SharedTypecheckProgressReporter(
      title,
      fun () ->
        Interlocked.Increment(reportCount) |> ignore
        new ServerProgressReport(client)
    )

  reporter, calls, reportCount

/// Wait briefly for fire-and-forget tasks to settle
let private settle () = Task.Delay(50) |> Async.AwaitTask

let private lastPercentage (calls: ConcurrentQueue<ProgressCall>) =
  calls.ToArray()
  |> Array.rev
  |> Array.tryPick (function
    | Report(_, Some percentage) -> Some percentage
    | _ -> None)


let singleFileTests =
  testList
    "Single File"
    [ testCaseAsync "Begin creates report and End disposes it"
      <| async {
        let reporter, calls, reportCount = createReporter "Typechecking"

        use reporter = reporter

        let! disp = reporter.Begin ("C:/src/Foo.fs") CancellationToken.None |> Async.AwaitTask

        do! settle ()

        Expect.equal reportCount.Value 1 "Should create one report"
        Expect.isGreaterThanOrEqual (calls.Count) 1 "Should have at least a Begin call"

        let! ct = reporter.GetCancellationToken() |> Async.AwaitTask
        Expect.isFalse (ct = CancellationToken.None) "CancellationToken should be active during file tracking"

        do! disp.DisposeAsync().AsTask() |> Async.AwaitTask
        do! settle ()

        let! ct = reporter.GetCancellationToken() |> Async.AwaitTask
        Expect.isTrue (ct = CancellationToken.None) "CancellationToken should be None after all files end"
      }

      testCaseAsync "Sequential files create separate report cycles"
      <| async {
        let reporter, _calls, reportCount = createReporter "Typechecking"

        use reporter = reporter

        // File 1
        let! disp1 = reporter.Begin ("C:/src/A.fs") CancellationToken.None |> Async.AwaitTask
        do! settle ()
        do! disp1.DisposeAsync().AsTask() |> Async.AwaitTask
        do! settle ()

        Expect.equal reportCount.Value 1 "First file creates one report"

        // File 2 — new report cycle
        let! disp2 = reporter.Begin ("C:/src/B.fs") CancellationToken.None |> Async.AwaitTask
        do! settle ()
        do! disp2.DisposeAsync().AsTask() |> Async.AwaitTask
        do! settle ()

        Expect.equal reportCount.Value 2 "Second file after first ended creates new report"
      }

      testCaseAsync "Concurrent files keep report alive until all end"
      <| async {
        let reporter, _calls, reportCount = createReporter "Typechecking"

        use reporter = reporter

        let! disp1 = reporter.Begin ("C:/src/A.fs") CancellationToken.None |> Async.AwaitTask
        let! disp2 = reporter.Begin ("C:/src/B.fs") CancellationToken.None |> Async.AwaitTask
        do! settle ()

        Expect.equal reportCount.Value 1 "Concurrent files share one report"

        // End first file — report should still be active
        do! disp1.DisposeAsync().AsTask() |> Async.AwaitTask
        do! settle ()

        let! ct = reporter.GetCancellationToken() |> Async.AwaitTask
        Expect.isFalse (ct = CancellationToken.None) "Report should still be active with one file remaining"

        // End second file — report should end
        do! disp2.DisposeAsync().AsTask() |> Async.AwaitTask
        do! settle ()

        let! ct = reporter.GetCancellationToken() |> Async.AwaitTask
        Expect.isTrue (ct = CancellationToken.None) "Report should end after all files complete"
      }

      testCaseAsync "Concurrent scopes for the same path keep the report active"
      <| async {
        let reporter, _calls, _reportCount = createReporter "Typechecking"
        use reporter = reporter

        let! disp1 = reporter.Begin ("C:/src/A.fs") CancellationToken.None |> Async.AwaitTask
        let! disp2 = reporter.Begin ("C:/src/A.fs") CancellationToken.None |> Async.AwaitTask

        do! disp1.DisposeAsync().AsTask() |> Async.AwaitTask
        do! settle ()

        let! ct = reporter.GetCancellationToken() |> Async.AwaitTask
        Expect.notEqual ct CancellationToken.None "The second scope must keep the report active"

        do! disp2.DisposeAsync().AsTask() |> Async.AwaitTask
        do! settle ()

        let! ct = reporter.GetCancellationToken() |> Async.AwaitTask
        Expect.equal ct CancellationToken.None "The report must end after both scopes end"
      }

      testCaseAsync "Files with the same basename keep independent scopes"
      <| async {
        let reporter, _calls, _reportCount = createReporter "Typechecking"
        use reporter = reporter

        let! disp1 = reporter.Begin ("C:/src/A/Foo.fs") CancellationToken.None |> Async.AwaitTask
        let! disp2 = reporter.Begin ("C:/src/B/Foo.fs") CancellationToken.None |> Async.AwaitTask

        do! disp1.DisposeAsync().AsTask() |> Async.AwaitTask
        do! settle ()

        let! ct = reporter.GetCancellationToken() |> Async.AwaitTask
        Expect.notEqual ct CancellationToken.None "The second path must keep the report active"

        do! disp2.DisposeAsync().AsTask() |> Async.AwaitTask
        do! settle ()

        let! ct = reporter.GetCancellationToken() |> Async.AwaitTask
        Expect.equal ct CancellationToken.None "The report must end after both paths complete"
      } ]


let batchTests =
  testList
    "Batch"
    [ testCaseAsync "BeginBatch creates report eagerly for CancellationToken access"
      <| async {
        let reporter, _calls, reportCount = createReporter "Typechecking"

        use reporter = reporter

        let! batchDisp =
          reporter.BeginBatch ([| "C:/src/A.fs"; "C:/src/B.fs" |]) CancellationToken.None
          |> Async.AwaitTask

        do! settle ()

        Expect.equal reportCount.Value 1 "BeginBatch should eagerly create a report"

        let! ct = reporter.GetCancellationToken() |> Async.AwaitTask
        Expect.isFalse (ct = CancellationToken.None) "CancellationToken should be available after BeginBatch"

        do! batchDisp.DisposeAsync().AsTask() |> Async.AwaitTask
        do! settle ()
      }

      testCaseAsync "Batch tracks completion percentage for batch files only"
      <| async {
        let reporter, calls, _reportCount = createReporter "Typechecking"

        use reporter = reporter

        let! batchDisp =
          reporter.BeginBatch ([| "C:/src/A.fs"; "C:/src/B.fs"; "C:/src/C.fs" |]) CancellationToken.None
          |> Async.AwaitTask

        do! settle ()

        // Process batch file A
        let! dispA = reporter.Begin ("C:/src/A.fs") CancellationToken.None |> Async.AwaitTask
        do! settle ()
        do! dispA.DisposeAsync().AsTask() |> Async.AwaitTask
        do! settle ()

        // After A completes: 1/3 = 33%
        let lastReport =
          calls.ToArray()
          |> Array.tryFindBack (fun c ->
            match c with
            | Report(_, Some _) -> true
            | _ -> false)

        match lastReport with
        | Some(Report(_, Some pct)) -> Expect.equal pct 33u "After 1/3 batch files complete, percentage should be 33%"
        | _ -> failtest "Expected a Report call with percentage after first batch file completes"

        // Process non-batch file — should NOT affect batch percentage
        let! dispX = reporter.Begin ("C:/src/X.fs") CancellationToken.None |> Async.AwaitTask
        do! settle ()
        do! dispX.DisposeAsync().AsTask() |> Async.AwaitTask
        do! settle ()

        // Process batch file B
        let! dispB = reporter.Begin ("C:/src/B.fs") CancellationToken.None |> Async.AwaitTask
        do! settle ()
        do! dispB.DisposeAsync().AsTask() |> Async.AwaitTask
        do! settle ()

        // After B completes: 2/3 = 66%
        let lastReport2 =
          calls.ToArray()
          |> Array.tryFindBack (fun c ->
            match c with
            | Report(_, Some pct) when pct >= 60u -> true
            | _ -> false)

        match lastReport2 with
        | Some(Report(_, Some pct)) -> Expect.equal pct 66u "After 2/3 batch files complete, percentage should be 66%"
        | _ -> failtest "Expected a Report call with ~66% after second batch file completes"

        do! batchDisp.DisposeAsync().AsTask() |> Async.AwaitTask
        do! settle ()
      }

      testCaseAsync "Batch counts duplicate paths independently"
      <| async {
        let reporter, calls, _reportCount = createReporter "Typechecking"
        use reporter = reporter

        let! batchDisp =
          reporter.BeginBatch ([| "C:/src/A.fs"; "C:/src/A.fs" |]) CancellationToken.None
          |> Async.AwaitTask

        let! disp1 = reporter.Begin ("C:/src/A.fs") CancellationToken.None |> Async.AwaitTask
        let! disp2 = reporter.Begin ("C:/src/A.fs") CancellationToken.None |> Async.AwaitTask

        do! disp1.DisposeAsync().AsTask() |> Async.AwaitTask
        do! settle ()

        let firstPercentage = lastPercentage calls

        Expect.equal firstPercentage (Some 50u) "The first duplicate completion must report 50 percent"

        do! disp2.DisposeAsync().AsTask() |> Async.AwaitTask
        do! settle ()

        let secondPercentage = lastPercentage calls

        Expect.equal secondPercentage (Some 100u) "The second duplicate completion must report 100 percent"

        do! batchDisp.DisposeAsync().AsTask() |> Async.AwaitTask
      }

      testCaseAsync "Overlapping batches count the same path independently"
      <| async {
        let reporter, calls, _reportCount = createReporter "Typechecking"
        use reporter = reporter

        let! batchDisp1 =
          reporter.BeginBatch ([| "C:/src/A.fs" |]) CancellationToken.None
          |> Async.AwaitTask

        let! batchDisp2 =
          reporter.BeginBatch ([| "C:/src/A.fs" |]) CancellationToken.None
          |> Async.AwaitTask

        let! disp1 = reporter.Begin ("C:/src/A.fs") CancellationToken.None |> Async.AwaitTask
        let! disp2 = reporter.Begin ("C:/src/A.fs") CancellationToken.None |> Async.AwaitTask

        do! disp1.DisposeAsync().AsTask() |> Async.AwaitTask
        do! settle ()

        let firstPercentage = lastPercentage calls

        Expect.equal firstPercentage (Some 50u) "The first overlapping completion must report 50 percent"

        do! disp2.DisposeAsync().AsTask() |> Async.AwaitTask
        do! settle ()

        let secondPercentage = lastPercentage calls

        Expect.equal secondPercentage (Some 100u) "The second overlapping completion must report 100 percent"

        do! batchDisp2.DisposeAsync().AsTask() |> Async.AwaitTask
        do! batchDisp1.DisposeAsync().AsTask() |> Async.AwaitTask
      }

      testCaseAsync "Batch with active file scopes keeps report alive"
      <| async {
        let reporter, _calls, _reportCount = createReporter "Typechecking"

        use reporter = reporter

        let! batchDisp =
          reporter.BeginBatch ([| "C:/src/A.fs" |]) CancellationToken.None
          |> Async.AwaitTask

        let! fileDisp = reporter.Begin ("C:/src/A.fs") CancellationToken.None |> Async.AwaitTask

        do! settle ()

        // End batch scope but file is still active
        do! batchDisp.DisposeAsync().AsTask() |> Async.AwaitTask
        do! settle ()

        let! ct = reporter.GetCancellationToken() |> Async.AwaitTask
        Expect.isFalse (ct = CancellationToken.None) "Report should remain active while file scope is open"

        // End file scope
        do! fileDisp.DisposeAsync().AsTask() |> Async.AwaitTask
        do! settle ()

        let! ct = reporter.GetCancellationToken() |> Async.AwaitTask
        Expect.isTrue (ct = CancellationToken.None) "Report should end after file and batch both complete"
      }

      testCaseAsync "EndBatch resets batch counters"
      <| async {
        let reporter, calls, _reportCount = createReporter "Typechecking"

        use reporter = reporter

        // First batch
        let! batchDisp1 =
          reporter.BeginBatch ([| "C:/src/A.fs"; "C:/src/B.fs" |]) CancellationToken.None
          |> Async.AwaitTask

        let! dispA = reporter.Begin ("C:/src/A.fs") CancellationToken.None |> Async.AwaitTask
        do! settle ()
        do! dispA.DisposeAsync().AsTask() |> Async.AwaitTask
        do! settle ()
        do! batchDisp1.DisposeAsync().AsTask() |> Async.AwaitTask
        do! settle ()

        // Second batch — counters should have been reset
        let callCountBeforeSecondBatch = calls.Count

        let! batchDisp2 =
          reporter.BeginBatch ([| "C:/src/C.fs"; "C:/src/D.fs" |]) CancellationToken.None
          |> Async.AwaitTask

        do! settle ()

        // The Begin for second batch should show 0% (0/2)
        let callsArr = calls.ToArray() |> Array.skip callCountBeforeSecondBatch

        let hasZeroPercentage =
          callsArr
          |> Array.exists (fun c ->
            match c with
            | Begin(_, _, Some 0u) -> true
            | Report(_, Some 0u) -> true
            | _ -> false)

        Expect.isTrue hasZeroPercentage "Second batch should start at 0% after first batch ended"

        do! batchDisp2.DisposeAsync().AsTask() |> Async.AwaitTask
        do! settle ()
      } ]


let batchSyncTests =
  testList
    "BeginBatchSync"
    [ testCaseAsync "BeginBatchSync adds files to active tracking and sets up batch"
      <| async {
        let reporter, calls, reportCount = createReporter "Loading Projects"
        use reporter = reporter

        let disp =
          reporter.BeginBatchSync([| "C:/src/Proj1.fsproj"; "C:/src/Proj2.fsproj"; "C:/src/Proj3.fsproj" |])

        // Give fire-and-forget tasks time to complete
        do! settle ()
        do! settle ()

        Expect.equal reportCount.Value 1 "Should create one report"

        let! ct = reporter.GetCancellationToken() |> Async.AwaitTask
        Expect.isFalse (ct = CancellationToken.None) "Report should be active after BeginBatchSync"

        // Check that a Begin call was made with the title
        let callsArr = calls.ToArray()

        let hasBegin =
          callsArr
          |> Array.exists (fun c ->
            match c with
            | Begin("Loading Projects", _, _) -> true
            | _ -> false)

        Expect.isTrue hasBegin "Should have a Begin call with the title"

        // Check that file names appear in report messages
        let hasFileNames =
          callsArr
          |> Array.exists (fun c ->
            match c with
            | Report(Some msg, _) ->
              msg.Contains("Proj1.fsproj")
              || msg.Contains("Proj2.fsproj")
              || msg.Contains("Proj3.fsproj")
            | _ -> false)

        Expect.isTrue hasFileNames "Report messages should contain project file names"

        disp.Dispose()
        do! settle ()
        do! settle ()

        let! ct = reporter.GetCancellationToken() |> Async.AwaitTask
        Expect.isTrue (ct = CancellationToken.None) "Report should end after Dispose"
      }

      testCaseAsync "BeginBatchSync Dispose ends all file tracking and batch"
      <| async {
        let reporter, calls, _reportCount = createReporter "Loading Projects"
        use reporter = reporter

        let disp = reporter.BeginBatchSync([| "C:/src/A.fsproj"; "C:/src/B.fsproj" |])
        do! settle ()
        do! settle ()

        disp.Dispose()
        do! settle ()
        do! settle ()

        // After dispose, an End call should have been made
        let callsArr = calls.ToArray()

        let hasEnd =
          callsArr
          |> Array.exists (fun c ->
            match c with
            | End _ -> true
            | _ -> false)

        Expect.isTrue hasEnd "Should have an End call after Dispose"
      } ]


let messageFormattingTests =
  testList
    "Message Formatting"
    [ testCaseAsync "Single file shows just the filename"
      <| async {
        let reporter, calls, _reportCount = createReporter "Typechecking"
        use reporter = reporter

        let! disp = reporter.Begin ("C:/src/Foo.fs") CancellationToken.None |> Async.AwaitTask

        do! settle ()

        let callsArr = calls.ToArray()

        let beginMsg =
          callsArr
          |> Array.tryPick (fun c ->
            match c with
            | Begin(_, msg, _) -> msg
            | _ -> None)

        Expect.equal beginMsg (Some "Foo.fs") "Single file message should be just the filename"

        do! disp.DisposeAsync().AsTask() |> Async.AwaitTask
        do! settle ()
      }

      testCaseAsync "Two concurrent files shows both filenames"
      <| async {
        let reporter, calls, _reportCount = createReporter "Typechecking"
        use reporter = reporter

        let! disp1 = reporter.Begin ("C:/src/A.fs") CancellationToken.None |> Async.AwaitTask
        do! settle ()
        let! disp2 = reporter.Begin ("C:/src/B.fs") CancellationToken.None |> Async.AwaitTask
        do! settle ()

        let callsArr = calls.ToArray()

        let hasMultipleFiles =
          callsArr
          |> Array.exists (fun c ->
            match c with
            | Report(Some msg, _) -> msg.Contains("A.fs") && msg.Contains("B.fs")
            | _ -> false)

        Expect.isTrue hasMultipleFiles "Message should contain both file names"

        do! disp1.DisposeAsync().AsTask() |> Async.AwaitTask
        do! disp2.DisposeAsync().AsTask() |> Async.AwaitTask
        do! settle ()
      }

      testCaseAsync "More than 3 concurrent files truncates with (+N more)"
      <| async {
        let reporter, calls, _reportCount = createReporter "Typechecking"
        use reporter = reporter

        let! disp1 = reporter.Begin ("C:/src/A.fs") CancellationToken.None |> Async.AwaitTask
        let! disp2 = reporter.Begin ("C:/src/B.fs") CancellationToken.None |> Async.AwaitTask
        let! disp3 = reporter.Begin ("C:/src/C.fs") CancellationToken.None |> Async.AwaitTask
        let! disp4 = reporter.Begin ("C:/src/D.fs") CancellationToken.None |> Async.AwaitTask
        do! settle ()

        let callsArr = calls.ToArray()

        let hasTruncation =
          callsArr
          |> Array.exists (fun c ->
            match c with
            | Report(Some msg, _) -> msg.Contains("(+1 more)")
            | _ -> false)

        Expect.isTrue hasTruncation "4 files should show (+1 more) truncation"

        do! disp1.DisposeAsync().AsTask() |> Async.AwaitTask
        do! disp2.DisposeAsync().AsTask() |> Async.AwaitTask
        do! disp3.DisposeAsync().AsTask() |> Async.AwaitTask
        do! disp4.DisposeAsync().AsTask() |> Async.AwaitTask
        do! settle ()
      }

      testCaseAsync "Batch file with active file shows combined message"
      <| async {
        let reporter, calls, _reportCount = createReporter "Typechecking"
        use reporter = reporter

        let! batchDisp =
          reporter.BeginBatch ([| "C:/src/A.fs"; "C:/src/B.fs" |]) CancellationToken.None
          |> Async.AwaitTask

        let! fileDisp = reporter.Begin ("C:/src/A.fs") CancellationToken.None |> Async.AwaitTask

        do! settle ()

        let callsArr = calls.ToArray()

        // Should have a message with both file name and batch progress
        let hasCombined =
          callsArr
          |> Array.exists (fun c ->
            match c with
            | Report(Some msg, _) -> msg.Contains("A.fs") && msg.Contains("0/2 completed")
            | _ -> false)

        Expect.isTrue hasCombined "Message should contain filename and batch progress"

        do! fileDisp.DisposeAsync().AsTask() |> Async.AwaitTask
        do! batchDisp.DisposeAsync().AsTask() |> Async.AwaitTask
        do! settle ()
      } ]


let cancellationTokenTests =
  testList
    "CancellationToken"
    [ testCaseAsync "GetCancellationToken returns None when no report is active"
      <| async {
        let reporter, _calls, _reportCount = createReporter "Typechecking"
        use reporter = reporter

        let! ct = reporter.GetCancellationToken() |> Async.AwaitTask
        Expect.isTrue (ct = CancellationToken.None) "Should return None when no report is active"
      }

      testCaseAsync "GetCancellationToken returns token from active report"
      <| async {
        let reporter, _calls, _reportCount = createReporter "Typechecking"
        use reporter = reporter

        let! disp = reporter.Begin ("C:/src/A.fs") CancellationToken.None |> Async.AwaitTask
        do! settle ()

        let! ct = reporter.GetCancellationToken() |> Async.AwaitTask
        Expect.isFalse (ct = CancellationToken.None) "Should return active token"
        Expect.isFalse ct.IsCancellationRequested "Token should not be cancelled"

        do! disp.DisposeAsync().AsTask() |> Async.AwaitTask
        do! settle ()
      }

      testCaseAsync "GetCancellationToken returns token from batch-created report"
      <| async {
        let reporter, _calls, _reportCount = createReporter "Typechecking"
        use reporter = reporter

        let! batchDisp =
          reporter.BeginBatch ([| "C:/src/A.fs" |]) CancellationToken.None
          |> Async.AwaitTask

        do! settle ()

        let! ct = reporter.GetCancellationToken() |> Async.AwaitTask
        Expect.isFalse (ct = CancellationToken.None) "BeginBatch should eagerly create report with token"

        do! batchDisp.DisposeAsync().AsTask() |> Async.AwaitTask
        do! settle ()
      } ]


let disabledReporterTests =
  testList
    "Disabled reporter"
    [ testCaseAsync "Does not create progress reports"
      <| async {
        let client, calls = createMockClient ()
        let enabled = ref false
        let reportCount = ref 0

        use reporter =
          new SharedTypecheckProgressReporter(
            "Typechecking",
            (fun () ->
              Interlocked.Increment(reportCount) |> ignore
              new ServerProgressReport(client)),
            isEnabled = (fun () -> enabled.Value)
          )

        let! fileDisp = reporter.Begin ("C:/src/A.fs") CancellationToken.None |> Async.AwaitTask

        let! batchDisp =
          reporter.BeginBatch ([| "C:/src/A.fs" |]) CancellationToken.None
          |> Async.AwaitTask

        use _batchSyncDisp = reporter.BeginBatchSync([| "C:/src/A.fs" |])

        do! fileDisp.DisposeAsync().AsTask() |> Async.AwaitTask
        do! batchDisp.DisposeAsync().AsTask() |> Async.AwaitTask
        do! settle ()

        Expect.equal reportCount.Value 0 "The disabled reporter must not create a report"
        Expect.equal calls.Count 0 "The disabled reporter must not send progress"

        let! ct = reporter.GetCancellationToken() |> Async.AwaitTask
        Expect.equal ct CancellationToken.None "The disabled reporter must not create a cancellation token"

        enabled.Value <- true

        let! enabledDisp = reporter.Begin ("C:/src/A.fs") CancellationToken.None |> Async.AwaitTask
        do! settle ()

        Expect.equal reportCount.Value 1 "The enabled reporter must create a report"

        do! enabledDisp.DisposeAsync().AsTask() |> Async.AwaitTask
      } ]


let tests =
  testList
    "SharedTypecheckProgressReporter"
    [ singleFileTests
      batchTests
      batchSyncTests
      messageFormattingTests
      cancellationTokenTests
      disabledReporterTests ]
