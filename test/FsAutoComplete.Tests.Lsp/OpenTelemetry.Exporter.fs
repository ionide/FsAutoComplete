/// OpenTelemetry File Exporter for OTLP JSON format
/// Exports traces to files in OTLP JSON format compatible with Jaeger, Aspire, and other OTLP-compatible backends
module OpenTelemetry.Exporter.OtlpFile

open System
open System.Diagnostics
open System.IO
open System.Text.Json
open System.Text.Json.Serialization
open OpenTelemetry

/// OTLP JSON format types following the OTLP specification
/// See: https://opentelemetry.io/docs/specs/otlp/#json-protobuf-encoding
[<RequireQualifiedAccess>]
module OtlpJson =

  /// Convert nanoseconds since Unix epoch to string (OTLP JSON format)
  let toUnixNano (dt: DateTime) : string =
    let unixEpoch = DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)
    let ticks = (dt.ToUniversalTime() - unixEpoch).Ticks
    let nanos = ticks * 100L // 1 tick = 100 nanoseconds
    string nanos

  /// Convert ActivityTraceId to lowercase hex string (OTLP JSON format)
  /// Per OTLP spec: traceId and spanId are hex-encoded, NOT base64
  let traceIdToHex (traceId: ActivityTraceId) : string = traceId.ToHexString().ToLowerInvariant()

  /// Convert ActivitySpanId to lowercase hex string (OTLP JSON format)
  let spanIdToHex (spanId: ActivitySpanId) : string = spanId.ToHexString().ToLowerInvariant()

  type KeyValue =
    { [<JsonPropertyName("key")>]
      Key: string
      [<JsonPropertyName("value")>]
      Value: AnyValue }

  and AnyValue =
    { [<JsonPropertyName("stringValue")>]
      [<JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)>]
      StringValue: string
      [<JsonPropertyName("intValue")>]
      [<JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)>]
      IntValue: string
      [<JsonPropertyName("boolValue")>]
      [<JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingDefault)>]
      BoolValue: bool
      [<JsonPropertyName("doubleValue")>]
      [<JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingDefault)>]
      DoubleValue: float }

  let toAnyValue (value: obj) : AnyValue =
    match value with
    | :? string as s ->
      { StringValue = s
        IntValue = null
        BoolValue = false
        DoubleValue = 0.0 }
    | :? int as i ->
      { StringValue = null
        IntValue = string i
        BoolValue = false
        DoubleValue = 0.0 }
    | :? int64 as i ->
      { StringValue = null
        IntValue = string i
        BoolValue = false
        DoubleValue = 0.0 }
    | :? bool as b ->
      { StringValue = null
        IntValue = null
        BoolValue = b
        DoubleValue = 0.0 }
    | :? float as f ->
      { StringValue = null
        IntValue = null
        BoolValue = false
        DoubleValue = f }
    | _ ->
      { StringValue = (if isNull value then "" else value.ToString())
        IntValue = null
        BoolValue = false
        DoubleValue = 0.0 }

  type SpanEvent =
    { [<JsonPropertyName("timeUnixNano")>]
      TimeUnixNano: string
      [<JsonPropertyName("name")>]
      Name: string
      [<JsonPropertyName("attributes")>]
      Attributes: KeyValue[] }

  type SpanStatus =
    { [<JsonPropertyName("code")>]
      Code: int
      [<JsonPropertyName("message")>]
      [<JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)>]
      Message: string }

  type OtlpSpan =
    { [<JsonPropertyName("traceId")>]
      TraceId: string
      [<JsonPropertyName("spanId")>]
      SpanId: string
      [<JsonPropertyName("parentSpanId")>]
      [<JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)>]
      ParentSpanId: string
      [<JsonPropertyName("name")>]
      Name: string
      [<JsonPropertyName("kind")>]
      Kind: int
      [<JsonPropertyName("startTimeUnixNano")>]
      StartTimeUnixNano: string
      [<JsonPropertyName("endTimeUnixNano")>]
      EndTimeUnixNano: string
      [<JsonPropertyName("attributes")>]
      Attributes: KeyValue[]
      [<JsonPropertyName("events")>]
      Events: SpanEvent[]
      [<JsonPropertyName("status")>]
      Status: SpanStatus }

  type InstrumentationScope =
    { [<JsonPropertyName("name")>]
      Name: string
      [<JsonPropertyName("version")>]
      [<JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)>]
      Version: string }

  type ScopeSpans =
    { [<JsonPropertyName("scope")>]
      Scope: InstrumentationScope
      [<JsonPropertyName("spans")>]
      Spans: OtlpSpan[] }

  type ResourceAttributes =
    { [<JsonPropertyName("attributes")>]
      Attributes: KeyValue[] }

  type ResourceSpans =
    { [<JsonPropertyName("resource")>]
      Resource: ResourceAttributes
      [<JsonPropertyName("scopeSpans")>]
      ScopeSpans: ScopeSpans[] }

  type TracesData =
    { [<JsonPropertyName("resourceSpans")>]
      ResourceSpans: ResourceSpans[] }

  /// Convert an Activity to an OTLP JSON span
  let fromActivity (activity: Activity) : OtlpSpan =
    let attributes =
      activity.Tags
      |> Seq.map (fun kvp ->
        { Key = kvp.Key
          Value = toAnyValue kvp.Value })
      |> Seq.toArray

    let events =
      activity.Events
      |> Seq.map (fun evt ->
        { TimeUnixNano = toUnixNano evt.Timestamp.UtcDateTime
          Name = evt.Name
          Attributes =
            evt.Tags
            |> Seq.map (fun kvp ->
              { Key = kvp.Key
                Value = toAnyValue kvp.Value })
            |> Seq.toArray })
      |> Seq.toArray

    let statusCode =
      match activity.Status with
      | ActivityStatusCode.Error -> 2
      | ActivityStatusCode.Ok -> 1
      | _ -> 0

    let parentSpanId =
      if activity.ParentSpanId.ToHexString() = "0000000000000000" then
        null
      else
        spanIdToHex activity.ParentSpanId

    { TraceId = traceIdToHex activity.TraceId
      SpanId = spanIdToHex activity.SpanId
      ParentSpanId = parentSpanId
      Name = activity.DisplayName
      Kind = int activity.Kind
      StartTimeUnixNano = toUnixNano activity.StartTimeUtc
      EndTimeUnixNano = toUnixNano (activity.StartTimeUtc + activity.Duration)
      Attributes = attributes
      Events = events
      Status =
        { Code = statusCode
          Message = activity.StatusDescription } }

  /// Convert a collection of spans to OTLP TracesData format
  let toTracesData (serviceName: string) (serviceVersion: string) (spans: OtlpSpan seq) : TracesData =
    { ResourceSpans =
        [| { Resource =
               { Attributes =
                   [| { Key = "service.name"
                        Value = toAnyValue serviceName }
                      { Key = "service.version"
                        Value = toAnyValue serviceVersion } |] }
             ScopeSpans =
               [| { Scope =
                      { Name = serviceName
                        Version = serviceVersion }
                    Spans = spans |> Seq.toArray } |] } |] }


/// Configuration options for the OTLP File Exporter
type OtlpFileExporterOptions() =
  /// Directory to write trace files to
  member val OutputDirectory: string = "traces" with get, set

  /// Service name to include in resource attributes
  member val ServiceName: string = "unknown" with get, set

  /// Service version to include in resource attributes
  member val ServiceVersion: string = "0.0.0" with get, set

  /// Optional filter predicate - only export activities that match this filter
  /// If None, all activities are exported
  member val Filter: (Activity -> bool) option = None with get, set

  /// File name prefix for trace files
  member val FilePrefix: string = "traces" with get, set

  /// Whether to pretty-print the JSON output
  member val PrettyPrint: bool = true with get, set


/// An OpenTelemetry processor that collects spans and writes them to files in OTLP JSON format.
/// This is compatible with Jaeger, Aspire Dashboard, and other OTLP-compatible backends.
type OtlpFileExportProcessor(options: OtlpFileExporterOptions) =
  inherit BaseProcessor<Activity>()

  let collectedSpans = Collections.Concurrent.ConcurrentBag<OtlpJson.OtlpSpan>()
  let mutable exportedFileCount = 0

  let shouldExport (activity: Activity) =
    match options.Filter with
    | Some filter -> filter activity
    | None -> true

  let writeToFile () =
    if not (collectedSpans.IsEmpty) then
      if not (Directory.Exists(options.OutputDirectory)) then
        Directory.CreateDirectory(options.OutputDirectory) |> ignore

      let timestamp = DateTime.UtcNow.ToString("yyyyMMdd_HHmmss_fff")

      let filename =
        Path.Combine(options.OutputDirectory, $"{options.FilePrefix}_{timestamp}.otlp.json")

      let jsonOptions = JsonSerializerOptions(WriteIndented = options.PrettyPrint)

      let spans = collectedSpans.ToArray()

      let tracesData =
        OtlpJson.toTracesData options.ServiceName options.ServiceVersion spans

      let json = JsonSerializer.Serialize(tracesData, jsonOptions)
      File.WriteAllText(filename, json)
      exportedFileCount <- exportedFileCount + 1

      // Clear the bag after writing
      while not (collectedSpans.IsEmpty) do
        collectedSpans.TryTake() |> ignore

      Some(filename, spans.Length)
    else
      None

  /// Called when a span ends - collect spans that match the filter
  override _.OnEnd(activity: Activity) =
    if shouldExport activity then
      collectedSpans.Add(OtlpJson.fromActivity activity)

    base.OnEnd(activity)

  /// Get the number of spans currently collected (not yet written)
  member _.CollectedSpanCount = collectedSpans.Count

  /// Get the number of files exported so far
  member _.ExportedFileCount = exportedFileCount

  /// Manually trigger writing collected spans to a file
  /// Returns the filename and span count if successful, None if no spans to write
  member _.WriteToFile() = writeToFile ()

  interface IDisposable with
    member this.Dispose() =
      this.WriteToFile() |> ignore
      base.Dispose()


/// A specialized processor that only exports failed test spans to OTLP JSON files
type FailedTestOtlpFileExportProcessor(options: OtlpFileExporterOptions) =
  inherit
    OtlpFileExportProcessor(
      OtlpFileExporterOptions(
        OutputDirectory = options.OutputDirectory,
        ServiceName = options.ServiceName,
        ServiceVersion = options.ServiceVersion,
        FilePrefix =
          (if String.IsNullOrEmpty(options.FilePrefix) || options.FilePrefix = "traces" then
             "failed_tests"
           else
             options.FilePrefix),
        PrettyPrint = options.PrettyPrint,
        Filter =
          Some(fun activity ->
            let status = activity.GetTagItem("test.result.status")

            match status with
            | :? string as s -> s = "Failed" || s = "Error"
            | _ -> activity.Status = ActivityStatusCode.Error)
      )
    )


/// Extension methods for TracerProviderBuilder to add the OTLP File Export Processor
[<AutoOpen>]
module TracerProviderBuilderExtensions =
  open OpenTelemetry.Trace

  type TracerProviderBuilder with

    /// Adds the OTLP File Export Processor with default options
    member this.AddOtlpFileExporter() =
      let options = OtlpFileExporterOptions()
      let processor = new OtlpFileExportProcessor(options)
      this.AddProcessor(processor), processor

    /// Adds the OTLP File Export Processor with custom options
    member this.AddOtlpFileExporter(configure: OtlpFileExporterOptions -> unit) =
      let options = OtlpFileExporterOptions()
      configure options
      let processor = new OtlpFileExportProcessor(options)
      this.AddProcessor(processor), processor

    /// Adds the Failed Test OTLP File Export Processor (only exports failed tests)
    member this.AddFailedTestOtlpFileExporter(configure: OtlpFileExporterOptions -> unit) =
      let options = OtlpFileExporterOptions()
      configure options
      let processor = new FailedTestOtlpFileExportProcessor(options)
      this.AddProcessor(processor), processor
