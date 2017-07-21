namespace FsAutoComplete

open System
open Newtonsoft.Json
open Newtonsoft.Json.Converters
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices

module private JsonSerializerConverters =

  let fsharpErrorSeverityWriter (writer: JsonWriter) value (serializer : JsonSerializer) =
    let s =
        match value with
        | FSharpErrorSeverity.Error -> "Error"
        | FSharpErrorSeverity.Warning -> "Warning"
    serializer.Serialize(writer, s)

  let rangeWriter (writer: JsonWriter) (range: Range.range) (_serializer : JsonSerializer) =
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

  let projectSdkTypeWriter (writer: JsonWriter) value (serializer : JsonSerializer) =
    let t, v =
        match value with
        | CommandResponse.ProjectResponseInfo.Verbose ->
            "verbose", (Object())
        | CommandResponse.ProjectResponseInfo.ProjectJson ->
            "project.json", (Object())
        | CommandResponse.ProjectResponseInfo.DotnetSdk d->
            "dotnet/sdk", box d
    writer.WriteStartObject()
    writer.WritePropertyName("SdkType")
    writer.WriteValue(t)
    writer.WritePropertyName("Data")
    serializer.Serialize(writer, v)
    writer.WriteEndObject()

  let projectOutputTypeWriter (writer: JsonWriter) value (serializer : JsonSerializer) =
    let s =
        match value with
        | ProjectOutputType.Library -> "lib"
        | ProjectOutputType.Exe -> "exe"
        | ProjectOutputType.Custom(x) -> x.ToLower()
    serializer.Serialize(writer, s)

  type OptionConverter() =
    inherit JsonConverter()

    override x.CanConvert(t) =
      t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>>

    override x.WriteJson(writer, value, serializer) =
      let value =
        if isNull value then null
        else
          let _,fields = Microsoft.FSharp.Reflection.FSharpValue.GetUnionFields(value, value.GetType())
          fields.[0]
      serializer.Serialize(writer, value)

    override x.ReadJson(reader, t, existingValue, serializer) =
        let innerType = t.GetGenericArguments().[0]
        let innerType =
          if innerType.IsValueType then (typedefof<Nullable<_>>).MakeGenericType([|innerType|])
          else innerType
        let value = serializer.Deserialize(reader, innerType)
        let cases = Microsoft.FSharp.Reflection.FSharpType.GetUnionCases(t)
        if isNull value then Microsoft.FSharp.Reflection.FSharpValue.MakeUnion(cases.[0], [||])
        else Microsoft.FSharp.Reflection.FSharpValue.MakeUnion(cases.[1], [|value|])

  let internal jsonConverters =

    let writeOnlyConverter (f: JsonWriter -> 'T -> JsonSerializer -> unit) (canConvert: Type -> Type -> bool) =
        { new JsonConverter() with
            member x.CanConvert(t:System.Type) = canConvert typeof<'T> t

            member x.WriteJson(writer, value, serializer) = f writer (value :?> 'T) serializer

            member x.ReadJson(_reader, _t, _, _serializer) =
              raise (System.NotSupportedException())

            member x.CanRead = false
            member x.CanWrite = true }

    let sameDU ty t = Microsoft.FSharp.Reflection.FSharpType.IsUnion(t) && t.BaseType = ty
    [| writeOnlyConverter fsharpErrorSeverityWriter (=)
       writeOnlyConverter rangeWriter (=)
       writeOnlyConverter projectSdkTypeWriter sameDU
       writeOnlyConverter projectOutputTypeWriter sameDU
       OptionConverter() :> JsonConverter |]

module JsonSerializer =

  let internal writeJson(o: obj) = JsonConvert.SerializeObject(o, JsonSerializerConverters.jsonConverters)
