# Analyzing Failed Test Traces

When tests fail in CI, FsAutoComplete exports OpenTelemetry traces for the failed tests in OTLP JSON format. These traces can be imported into observability tools like Jaeger, Aspire Dashboard, or Zipkin for detailed analysis.

## Finding the Trace Files

### GitHub Actions

1. Navigate to the failed workflow run
2. Scroll to the **Artifacts** section at the bottom
3. Download the artifact named `failed-test-traces-{os}-{dotnet}-{loader}-{compiler}`
4. Extract the ZIP file to find `failed_tests_{timestamp}.otlp.json`

### Local Development

When running tests locally with `CI=true`:

```bash
CI=true FAILED_TRACES_DIR=./traces dotnet run -c Release -f net8.0
```

Trace files will be written to the specified directory (default: `failed_traces/`).

## Viewing Traces

### Option 1: Jaeger

[Jaeger](https://www.jaegertracing.io/) is a popular open-source distributed tracing platform.

#### Quick Start with Docker

```bash
# Start Jaeger with OTLP support
docker run -d --name jaeger \
  -p 16686:16686 \
  -p 4317:4317 \
  -p 4318:4318 \
  jaegertracing/all-in-one:latest
```

#### Import Traces via OTLP HTTP

```bash
# Send the trace file to Jaeger's OTLP HTTP endpoint
curl -X POST http://localhost:4318/v1/traces \
  -H "Content-Type: application/json" \
  -d @failed_tests_20251218_100000.otlp.json
```

#### View in Jaeger UI

1. Open http://localhost:16686
2. Select service `FsAutoComplete.Tests.Lsp`
3. Click "Find Traces"
4. Click on a trace to see the span details, including:
   - Test name
   - Duration
   - Error status and message
   - Exception details (type, message, stack trace)
   - Source code location (file path, line number)

### Option 2: Aspire Dashboard

The [.NET Aspire Dashboard](https://learn.microsoft.com/en-us/dotnet/aspire/fundamentals/dashboard/overview) provides a standalone trace viewer.

#### Quick Start with Docker

```bash
# Start Aspire Dashboard
docker run -d --name aspire-dashboard \
  -p 18888:18888 \
  -p 4317:18889 \
  -p 4318:18890 \
  mcr.microsoft.com/dotnet/aspire-dashboard:latest
```

#### Import Traces

```bash
# Send traces to Aspire's OTLP HTTP endpoint
curl -X POST http://localhost:18890/v1/traces \
  -H "Content-Type: application/json" \
  -d @failed_tests_20251218_100000.otlp.json
```

#### View in Aspire Dashboard

1. Open http://localhost:18888
2. Navigate to the "Traces" tab
3. Browse and filter traces by service name or status

### Option 3: OpenTelemetry Collector

For more flexibility, use the [OpenTelemetry Collector](https://opentelemetry.io/docs/collector/) to route traces to multiple backends.

#### Collector Configuration

Create `otel-collector-config.yaml`:

```yaml
receivers:
  otlp:
    protocols:
      grpc:
        endpoint: 0.0.0.0:4317
      http:
        endpoint: 0.0.0.0:4318

exporters:
  # Export to Jaeger
  otlp/jaeger:
    endpoint: jaeger:4317
    tls:
      insecure: true
  
  # Or export to console for debugging
  debug:
    verbosity: detailed

service:
  pipelines:
    traces:
      receivers: [otlp]
      exporters: [otlp/jaeger, debug]
```

#### Run the Collector

```bash
docker run -d --name otel-collector \
  -p 4317:4317 \
  -p 4318:4318 \
  -v $(pwd)/otel-collector-config.yaml:/etc/otelcol/config.yaml \
  otel/opentelemetry-collector:latest
```

### Option 4: Programmatic Analysis

You can also analyze the JSON file programmatically:

#### F# Example

```fsharp
open System.Text.Json

type AnyValue = { stringValue: string option; intValue: string option }
type KeyValue = { key: string; value: AnyValue }
type SpanStatus = { code: int; message: string option }
type OtlpSpan = {
    traceId: string
    spanId: string
    name: string
    startTimeUnixNano: string
    endTimeUnixNano: string
    attributes: KeyValue[]
    status: SpanStatus
}
type ScopeSpans = { spans: OtlpSpan[] }
type ResourceSpans = { scopeSpans: ScopeSpans[] }
type TracesData = { resourceSpans: ResourceSpans[] }

let traces = JsonSerializer.Deserialize<TracesData>(File.ReadAllText("failed_tests.otlp.json"))

for rs in traces.resourceSpans do
    for ss in rs.scopeSpans do
        for span in ss.spans do
            printfn "Test: %s" span.name
            printfn "Status: %s" (if span.status.code = 2 then "FAILED" else "OK")
            for attr in span.attributes do
                match attr.value.stringValue with
                | Some v -> printfn "  %s: %s" attr.key v
                | None -> ()
```

#### PowerShell Example

```powershell
$traces = Get-Content "failed_tests.otlp.json" | ConvertFrom-Json

foreach ($rs in $traces.resourceSpans) {
    foreach ($ss in $rs.scopeSpans) {
        foreach ($span in $ss.spans) {
            Write-Host "Test: $($span.name)"
            Write-Host "Status: $(if ($span.status.code -eq 2) { 'FAILED' } else { 'OK' })"
            foreach ($attr in $span.attributes) {
                if ($attr.value.stringValue) {
                    Write-Host "  $($attr.key): $($attr.value.stringValue)"
                }
            }
            Write-Host ""
        }
    }
}
```

## Trace Structure

The trace files follow the [OTLP JSON Protobuf Encoding](https://opentelemetry.io/docs/specs/otlp/#json-protobuf-encoding) specification.

Each failed test span contains:

| Field | Description |
|-------|-------------|
| `traceId` | Unique trace identifier (32-char lowercase hex string) |
| `spanId` | Unique span identifier (16-char lowercase hex string) |
| `name` | Full test name (e.g., `FSAC.lsp.CodeFix.TestName`) |
| `startTimeUnixNano` | Test start time in nanoseconds since Unix epoch |
| `endTimeUnixNano` | Test end time in nanoseconds since Unix epoch |
| `status.code` | `0` = Unset, `1` = OK, `2` = Error |
| `status.message` | Error description |

### Common Attributes

| Attribute | Description |
|-----------|-------------|
| `test.result.status` | `Passed`, `Failed`, `Error`, or `Ignored` |
| `test.result.message` | Detailed failure message |
| `code.filepath` | Source file path |
| `code.lineno` | Line number in source |
| `exception.type` | Exception type name |
| `exception.message` | Exception message |
| `exception.stacktrace` | Full stack trace |

### Events

Exception details are recorded as span events with the name `exception`, containing:
- `exception.type`
- `exception.message`
- `exception.stacktrace`

## Troubleshooting

### No trace file generated

- Ensure `CI=true` environment variable is set
- Check that tests actually failed (traces are only generated for failures)
- Verify `FAILED_TRACES_DIR` points to a writable directory

### Traces not appearing in Jaeger/Aspire

- Verify the OTLP endpoint is accessible
- Check that the JSON file is valid: `jq . failed_tests.otlp.json`
- Ensure the Content-Type header is set to `application/json`

### Large trace files

If many tests fail, the trace file may be large. Consider:
- Filtering to specific test categories
- Using `--filter` to run a subset of tests
- Compressing before upload: `gzip failed_tests.otlp.json`

## Implementation Notes

The OTLP JSON serialization is implemented in `test/FsAutoComplete.Tests.Lsp/Expecto.OpenTelemetry.fs`.

While the `OpenTelemetry.Exporter.OpenTelemetryProtocol` NuGet package contains protobuf types (`OpenTelemetry.Proto.Trace.V1.Span`, etc.), these types are marked as **internal** and cannot be used directly by consuming code. Therefore, we implement our own JSON serialization following the OTLP specification.

Key implementation details:
- Trace and span IDs use **lowercase hex encoding** (not base64) per OTLP JSON spec
- Timestamps are nanoseconds since Unix epoch as strings
- Enum values are serialized as integers (e.g., `status.code: 2` for Error)
- The `FailedTestFileExporter` processor filters spans by `test.result.status` tag
