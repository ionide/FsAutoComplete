namespace FsAutoComplete

module JsonSerializer =

    open System
    open Microsoft.FSharp.Reflection
    open Newtonsoft.Json
    open Newtonsoft.Json.Converters
    open Microsoft.FSharp.Compiler
    open Microsoft.FSharp.Compiler.SourceCodeServices


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

    type OptionConverter() =
      inherit JsonConverter()

      override x.CanConvert(t) =
        t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>>

      override x.WriteJson(writer, value, serializer) =
        let value =
          if isNull value then null
          else
            let _,fields = FSharpValue.GetUnionFields(value, value.GetType())
            fields.[0]
        serializer.Serialize(writer, value)

      override x.ReadJson(reader, t, existingValue, serializer) =
          let innerType = t.GetGenericArguments().[0]
          let innerType =
            if innerType.IsValueType then (typedefof<Nullable<_>>).MakeGenericType([|innerType|])
            else innerType
          let value = serializer.Deserialize(reader, innerType)
          let cases = FSharpType.GetUnionCases(t)
          if isNull value then FSharpValue.MakeUnion(cases.[0], [||])
          else FSharpValue.MakeUnion(cases.[1], [|value|])

    let private jsonConverters =
      [|
       new FSharpErrorSeverityConverter() :> JsonConverter
       new RangeConverter() :> JsonConverter
       new OptionConverter() :> JsonConverter
      |]

    let internal writeJson(o: obj) = JsonConvert.SerializeObject(o, jsonConverters)
