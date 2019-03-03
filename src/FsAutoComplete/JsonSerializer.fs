namespace FsAutoComplete

open System
open Newtonsoft.Json
open Newtonsoft.Json.Converters
open FSharp.Compiler
open FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Reflection

module private JsonSerializerConverters =

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

  let fsharpErrorSeverityWriter (writer: JsonWriter) value (serializer : JsonSerializer) =
    let s =
        match value with
        | FSharpErrorSeverity.Error -> "Error"
        | FSharpErrorSeverity.Warning -> "Warning"
    serializer.Serialize(writer, s)

  let workspacePeekFoundWriter (writer: JsonWriter) value (serializer : JsonSerializer) =
    let t, v =
        match value with
        | CommandResponse.WorkspacePeekFound.Directory d ->
            "directory", box d
        | CommandResponse.WorkspacePeekFound.Solution sln ->
            "solution", box sln
    writer.WriteStartObject()
    writer.WritePropertyName("Type")
    writer.WriteValue(t)
    writer.WritePropertyName("Data")
    serializer.Serialize(writer, v)
    writer.WriteEndObject()

  let workspacePeekFoundSolutionItemKindWriter (writer: JsonWriter) value (serializer : JsonSerializer) =
    let t, v =
        match value with
        | CommandResponse.WorkspacePeekFoundSolutionItemKind.Folder d ->
            "folder", box d
        | CommandResponse.WorkspacePeekFoundSolutionItemKind.MsbuildFormat msbuildProj ->
            "msbuildFormat", box msbuildProj
    writer.WriteStartObject()
    writer.WritePropertyName("Kind")
    writer.WriteValue(t)
    writer.WritePropertyName("Data")
    serializer.Serialize(writer, v)
    writer.WriteEndObject()

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
        | CommandResponse.ProjectOutputType.Library -> "lib"
        | CommandResponse.ProjectOutputType.Exe -> "exe"
        | CommandResponse.ProjectOutputType.Custom(x) -> x.ToLower()
    serializer.Serialize(writer, s)


  let internal jsonConverters =

    let writeOnlyConverter (f: JsonWriter -> 'T -> JsonSerializer -> unit) (canConvert: Type -> Type -> bool) =
        { new JsonConverter() with
            member x.CanConvert(t:System.Type) = canConvert typeof<'T> t

            member x.WriteJson(writer, value, serializer) = f writer (value :?> 'T) serializer

            member x.ReadJson(_reader, _t, _, _serializer) =
              raise (System.NotSupportedException())

            member x.CanRead = false
            member x.CanWrite = true }

    let memoizeTest f =
      let cache = ref Map.empty
      fun (x : Type) (y : Type) ->
        let input = (x.GetHashCode(),y.GetHashCode())
        match (!cache).TryFind(input) with
        | Some res -> res
        | None ->
            let res = f x y
            cache := (!cache).Add(input,res)
            res

    let sameDU = memoizeTest (fun ty t -> Microsoft.FSharp.Reflection.FSharpType.IsUnion(t) && t.BaseType = ty)

    [| writeOnlyConverter fsharpErrorSeverityWriter (=)
       writeOnlyConverter rangeWriter (=)
       OptionConverter() :> JsonConverter
       writeOnlyConverter workspacePeekFoundWriter sameDU
       writeOnlyConverter workspacePeekFoundSolutionItemKindWriter sameDU
       writeOnlyConverter projectSdkTypeWriter sameDU
       writeOnlyConverter projectOutputTypeWriter sameDU |]

module JsonSerializer =

  let writeJson(o: obj) = JsonConvert.SerializeObject(o, JsonSerializerConverters.jsonConverters)
