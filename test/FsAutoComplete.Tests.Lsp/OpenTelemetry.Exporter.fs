module OpenTelemetry.Exporter.OtlpFile

open System
open System.Diagnostics
open System.IO
open System.Text.Json
open System.Text.Json.Serialization
open OpenTelemetry

[<RequireQualifiedAccess>]
module OtlpJson =

  let toUnixNano (dt: DateTime) : string =
    let unixEpoch = DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)
    let ticks = (dt.ToUniversalTime() - unixEpoch).Ticks
    let nanos = ticks * 100L // 1 tick = 100 nanoseconds
    string nanos

  // OTLP JSON requires hexadecimal trace and span identifiers.
  let traceIdToHex (traceId: ActivityTraceId) : string = traceId.ToHexString().ToLowerInvariant()

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

    let spanKind =
      match activity.Kind with
      | ActivityKind.Internal -> 1
      | ActivityKind.Server -> 2
      | ActivityKind.Client -> 3
      | ActivityKind.Producer -> 4
      | ActivityKind.Consumer -> 5
      | _ -> 0

    { TraceId = traceIdToHex activity.TraceId
      SpanId = spanIdToHex activity.SpanId
      ParentSpanId = parentSpanId
      Name = activity.DisplayName
      Kind = spanKind
      StartTimeUnixNano = toUnixNano activity.StartTimeUtc
      EndTimeUnixNano = toUnixNano (activity.StartTimeUtc + activity.Duration)
      Attributes = attributes
      Events = events
      Status =
        { Code = statusCode
          Message = activity.StatusDescription } }

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


type OtlpFileExporterOptions() =
  member val OutputDirectory: string = "traces" with get, set

  member val ServiceName: string = "unknown" with get, set

  member val ServiceVersion: string = "0.0.0" with get, set

  member val Filter: (Activity -> bool) option = None with get, set

  member val FilePrefix: string = "traces" with get, set

  member val PrettyPrint: bool = true with get, set

let private writeSpansToFile (options: OtlpFileExporterOptions) (spans: OtlpJson.OtlpSpan[]) =
  if Array.isEmpty spans then
    None
  else
    Directory.CreateDirectory(options.OutputDirectory) |> ignore

    let timestamp = DateTime.UtcNow.ToString("yyyyMMdd_HHmmss_fff")

    let filename =
      Path.Combine(options.OutputDirectory, $"{options.FilePrefix}_{timestamp}_{Environment.ProcessId}.otlp.json")

    let jsonOptions = JsonSerializerOptions(WriteIndented = options.PrettyPrint)

    let tracesData =
      OtlpJson.toTracesData options.ServiceName options.ServiceVersion spans

    let json = JsonSerializer.Serialize(tracesData, jsonOptions)
    File.WriteAllText(filename, json)
    Some(filename, spans.Length)


type OtlpFileExportProcessor(options: OtlpFileExporterOptions) =
  inherit BaseProcessor<Activity>()

  let collectedSpans = Collections.Concurrent.ConcurrentBag<OtlpJson.OtlpSpan>()
  let mutable exportedFileCount = 0

  let shouldExport (activity: Activity) =
    match options.Filter with
    | Some filter -> filter activity
    | None -> true

  let writeToFile () =
    let result = collectedSpans.ToArray() |> writeSpansToFile options

    if Option.isSome result then
      exportedFileCount <- exportedFileCount + 1

      while not (collectedSpans.IsEmpty) do
        collectedSpans.TryTake() |> ignore

    result

  override _.OnEnd(activity: Activity) =
    if shouldExport activity then
      collectedSpans.Add(OtlpJson.fromActivity activity)

    base.OnEnd(activity)

  member _.CollectedSpanCount = collectedSpans.Count

  member _.ExportedFileCount = exportedFileCount

  member _.WriteToFile() = writeToFile ()

  interface IDisposable with
    member this.Dispose() =
      this.WriteToFile() |> ignore
      base.Dispose()


type FailedTestOtlpFileExportProcessor(options: OtlpFileExporterOptions) =
  inherit BaseProcessor<Activity>()

  let options =
    OtlpFileExporterOptions(
      OutputDirectory = options.OutputDirectory,
      ServiceName = options.ServiceName,
      ServiceVersion = options.ServiceVersion,
      FilePrefix =
        (if String.IsNullOrEmpty(options.FilePrefix) || options.FilePrefix = "traces" then
           "failed_tests"
         else
           options.FilePrefix),
      PrettyPrint = options.PrettyPrint
    )

  let traces =
    Collections.Concurrent.ConcurrentDictionary<ActivityTraceId, Collections.Concurrent.ConcurrentBag<OtlpJson.OtlpSpan>>()

  let failedTraceIds =
    Collections.Concurrent.ConcurrentDictionary<ActivityTraceId, unit>()

  let completedSuccessfulTraceIds =
    Collections.Concurrent.ConcurrentDictionary<ActivityTraceId, unit>()

  let mutable exportedFileCount = 0

  let testStatus (activity: Activity) =
    match activity.GetTagItem("test.result.status") with
    | :? string as status -> Some status
    | _ -> None

  let isRecorded (activity: Activity) =
    activity.ActivityTraceFlags &&& ActivityTraceFlags.Recorded = ActivityTraceFlags.Recorded

  let writeToFile () =
    let failedIds = failedTraceIds.Keys |> Seq.toArray

    let spans =
      failedIds
      |> Array.collect (fun traceId ->
        match traces.TryGetValue traceId with
        | true, spans -> spans.ToArray()
        | false, _ -> [||])

    let result = spans |> writeSpansToFile options

    if Option.isSome result then
      exportedFileCount <- exportedFileCount + 1

      for traceId in failedIds do
        traces.TryRemove traceId |> ignore
        failedTraceIds.TryRemove traceId |> ignore

    result |> Option.map (fun (filename, _) -> filename, failedIds.Length)

  override _.OnEnd(activity: Activity) =
    if isRecorded activity then
      match testStatus activity with
      | Some("Passed" | "Ignored") ->
        completedSuccessfulTraceIds.TryAdd(activity.TraceId, ()) |> ignore
        traces.TryRemove activity.TraceId |> ignore
      | status when not (completedSuccessfulTraceIds.ContainsKey activity.TraceId) ->
        traces
          .GetOrAdd(activity.TraceId, fun _ -> Collections.Concurrent.ConcurrentBag<OtlpJson.OtlpSpan>())
          .Add(OtlpJson.fromActivity activity)

        match status with
        | Some("Failed" | "Error") -> failedTraceIds.TryAdd(activity.TraceId, ()) |> ignore
        | _ -> ()
      | _ -> ()

    base.OnEnd activity

  member _.CollectedSpanCount = traces.Values |> Seq.sumBy _.Count
  member _.ExportedFileCount = exportedFileCount
  member _.WriteToFile() = writeToFile ()

  interface IDisposable with
    member this.Dispose() =
      this.WriteToFile() |> ignore
      base.Dispose()


[<AutoOpen>]
module TracerProviderBuilderExtensions =
  open OpenTelemetry.Trace

  type TracerProviderBuilder with

    member this.AddOtlpFileExporter() =
      let options = OtlpFileExporterOptions()
      let processor = new OtlpFileExportProcessor(options)
      this.AddProcessor(processor), processor

    member this.AddOtlpFileExporter(configure: OtlpFileExporterOptions -> unit) =
      let options = OtlpFileExporterOptions()
      configure options
      let processor = new OtlpFileExportProcessor(options)
      this.AddProcessor(processor), processor

    member this.AddFailedTestOtlpFileExporter(configure: OtlpFileExporterOptions -> unit) =
      let options = OtlpFileExporterOptions()
      configure options
      let processor = new FailedTestOtlpFileExportProcessor(options)
      this.AddProcessor(processor), processor
