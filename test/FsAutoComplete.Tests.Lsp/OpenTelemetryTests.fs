module FsAutoComplete.Tests.OpenTelemetryTests

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Text.Json
open Expecto
open Expecto.Impl
open OpenTelemetry
open OpenTelemetry.Exporter.OtlpFile
open OpenTelemetry.Trace

type private CollectingProcessor(spans: ResizeArray<Activity>) =
  inherit BaseProcessor<Activity>()

  override _.OnEnd(activity: Activity) =
    spans.Add activity
    base.OnEnd activity

let private withTempDirectory f =
  let directory =
    Path.Combine(Path.GetTempPath(), "FsAutoComplete.Tests", Guid.NewGuid().ToString())

  Directory.CreateDirectory directory |> ignore

  try
    f directory
  finally
    Directory.Delete(directory, true)

let private withoutCurrentActivity f =
  let previousActivity = Activity.Current
  Activity.Current <- null

  try
    f ()
  finally
    Activity.Current <- previousActivity

let private spanNames (path: string) =
  use json = JsonDocument.Parse(File.ReadAllText path)

  [| for resourceSpans in json.RootElement.GetProperty("resourceSpans").EnumerateArray() do
       for scopeSpans in resourceSpans.GetProperty("scopeSpans").EnumerateArray() do
         for span in scopeSpans.GetProperty("spans").EnumerateArray() do
           span.GetProperty("name").GetString() |]

let private spanKinds (path: string) =
  use json = JsonDocument.Parse(File.ReadAllText path)

  [| for resourceSpans in json.RootElement.GetProperty("resourceSpans").EnumerateArray() do
       for scopeSpans in resourceSpans.GetProperty("scopeSpans").EnumerateArray() do
         for span in scopeSpans.GetProperty("spans").EnumerateArray() do
           span.GetProperty("kind").GetInt32() |]

let tests =
  testList
    "OpenTelemetry"
    [ testCase "test spans are independent roots with exception details" (fun () ->
        use source = new ActivitySource($"FsAutoComplete.Tests.%O{Guid.NewGuid()}")
        let stoppedSpans = ResizeArray<Activity>()

        use provider =
          Sdk
            .CreateTracerProviderBuilder()
            .AddSource(source.Name)
            .AddProcessor(new CollectingProcessor(stoppedSpans))
            .Build()

        use outer = source.StartActivity("outer")

        let testCode =
          Expecto.OpenTelemetry.wrapCodeWithLazySpan
            source
            "failed test"
            SourceLocation.empty
            (TestCode.Sync(fun () -> raise (InvalidOperationException "boom")))

        match testCode with
        | TestCode.Sync run ->
          Expect.throwsT<InvalidOperationException> run "The wrapper must preserve the test exception"
        | _ -> failtest "Expected synchronous test code"

        let testSpan =
          stoppedSpans |> Seq.find (fun span -> span.DisplayName = "failed test")

        Expect.notEqual testSpan.TraceId outer.TraceId "Each test must start a separate trace"
        Expect.equal testSpan.ParentSpanId (ActivitySpanId()) "A test span must be a trace root"
        Expect.equal testSpan.Status ActivityStatusCode.Error "A failed test must set error status"
        Expect.equal testSpan.StatusDescription "boom" "A failed test must include the exception message"

        Expect.exists
          testSpan.Events
          (fun event -> event.Name = "exception")
          "A failed test must include an exception event")

      testCase "failed test export includes its child spans only" (fun () ->
        withTempDirectory (fun directory ->
          withoutCurrentActivity (fun () ->
            use source = new ActivitySource($"FsAutoComplete.Tests.%O{Guid.NewGuid()}")

            let builder, exporter =
              Sdk
                .CreateTracerProviderBuilder()
                .AddSource(source.Name)
                .AddFailedTestOtlpFileExporter(fun options ->
                  options.OutputDirectory <- directory
                  options.ServiceName <- "tests"
                  options.ServiceVersion <- "1.0.0")

            use provider = builder.Build()

            use failedTest =
              source.StartActivity("failed test", ActivityKind.Internal, ActivityContext())

            do
              use child = source.StartActivity("failed child")
              ()

            failedTest.SetTag("test.result.status", "Failed") |> ignore
            failedTest.SetStatus(ActivityStatusCode.Error) |> ignore
            failedTest.Stop()

            use passedTest =
              source.StartActivity("passed test", ActivityKind.Internal, ActivityContext())

            do
              use child = source.StartActivity("passed child")
              ()

            passedTest.SetTag("test.result.status", "Passed") |> ignore
            passedTest.SetStatus(ActivityStatusCode.Ok) |> ignore
            passedTest.Stop()

            provider.ForceFlush() |> ignore

            let path, failedTestCount =
              exporter.WriteToFile()
              |> Option.defaultWith (fun () -> failtest "Expected a failed test trace file")

            let names = spanNames path
            let kinds = spanKinds path

            Expect.equal failedTestCount 1 "The exporter must report the failed test count"

            Expect.stringContains
              (Path.GetFileName path)
              $"_{Environment.ProcessId}.otlp.json"
              "The trace filename must identify its test process"

            Expect.contains names "failed test" "The export must contain the failed test span"
            Expect.contains names "failed child" "The export must contain child spans from the failed test"
            Expect.isFalse (names |> Array.contains "passed test") "The export must exclude passed test spans"

            Expect.isFalse
              (names |> Array.contains "passed child")
              "The export must exclude child spans from passed tests"

            Expect.all kinds ((=) 1) "Internal activities must use the OTLP internal span kind"))) ]
