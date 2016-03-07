namespace FsAutoComplete

open System
open Newtonsoft.Json
open Newtonsoft.Json.Converters
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices

module JsonSerializer =

  type private FSharpErrorSeverityConverter() =
    inherit JsonConverter()

    override x.CanConvert(t:System.Type) = t = typeof<FSharpErrorSeverity>

    override x.WriteJson(writer, value, serializer) =
      match value :?> FSharpErrorSeverity with
      | FSharpErrorSeverity.Error -> serializer.Serialize(writer, "Error")
      | FSharpErrorSeverity.Warning -> serializer.Serialize(writer, "Warning")

    override x.ReadJson(_reader, _t, _, _serializer) =
      raise (System.NotSupportedException())

    override x.CanRead = false
    override x.CanWrite = true

  type private RangeConverter() =
    inherit JsonConverter()

    override x.CanConvert(t:System.Type) = t = typeof<Range.range>

    override x.WriteJson(writer, value, _serializer) =
      let range = value :?> Range.range
      writer.WriteStartObject()
      writer.WritePropertyName("StartColumn")
      writer.WriteValue(range.StartColumn + 1)
      writer.WritePropertyName("StartLine")
      writer.WriteValue(range.StartLine)
      writer.WritePropertyName("EndColumn")
      writer.WriteValue(range.EndColumn + 1)
      writer.WritePropertyName("EndLine")
      writer.WriteValue(range.EndLine)
      writer.WriteEndObject()

    override x.ReadJson(_reader, _t, _, _serializer) =
      raise (System.NotSupportedException())

    override x.CanRead = false
    override x.CanWrite = true

  let private jsonConverters =
    [|
     new FSharpErrorSeverityConverter() :> JsonConverter;
     new RangeConverter() :> JsonConverter
    |]

  let internal writeJson(o: obj) = JsonConvert.SerializeObject(o, jsonConverters)
