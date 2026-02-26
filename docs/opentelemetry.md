
# OpenTelemetry

FsAutoComplete uses [System.Diagnostics.Activity](https://learn.microsoft.com/en-us/dotnet/core/diagnostics/distributed-tracing-instrumentation-walkthroughs) to create traces.

## Exporting Traces with Jaeger

To export traces, run [Jaeger](https://www.jaegertracing.io/):

```bash
docker run -d --name jaeger \
  -e COLLECTOR_ZIPKIN_HOST_PORT=9411 \
  -e COLLECTOR_OTLP_ENABLED=true \
  -p 6831:6831/udp \
  -p 6832:6832/udp \
  -p 5778:5778 \
  -p 16686:16686 \
  -p 4317:4317 \
  -p 4318:4318 \
  -p 14250:14250 \
  -p 14268:14268 \
  -p 14269:14269 \
  -p 9411:9411 \
  jaegertracing/all-in-one:latest
```

Then configure your [environment](https://opentelemetry.io/docs/concepts/sdk-configuration/otlp-exporter-configuration/):

```bash
OTEL_EXPORTER_OTLP_ENDPOINT = "http://localhost:4317"
```

Start FsAutoComplete with:

```bash
dotnet fsautocomplete --otel-exporter-enabled
```

Or via VS Code with the setting:

```json
"FSharp.fsac.fsacArgs": ["--otel-exporter-enabled"]
```

> **Tip:** If you also want to observe `fsc` traces, use the `FSharp.notifications` settings.

After performing actions like opening documents, saving, or getting tooltips, open <http://localhost:16686/> to inspect traces.
