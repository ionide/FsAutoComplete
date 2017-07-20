namespace FsAutoComplete

open System
open Newtonsoft.Json
open Newtonsoft.Json.Converters
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices

module JsonSerializer =

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
    let s =
        match value with
        | ProjectSdkType.Verbose -> "verbose"
        | ProjectSdkType.ProjectJson -> "project.json"
        | ProjectSdkType.DotnetSdk -> "dotnet/sdk"
    serializer.Serialize(writer, s)

  let projectOutputTypeWriter (writer: JsonWriter) value (serializer : JsonSerializer) =
    let s =
        match value with
        | ProjectOutputType.Library -> "lib"
        | ProjectOutputType.Exe -> "exe"
        | ProjectOutputType.Custom(x) -> x.ToLower()
    serializer.Serialize(writer, s)

  let private jsonConverters =

    let jsonWriter (f: JsonWriter -> 'T -> JsonSerializer -> unit) =
        let boxedF (writer: JsonWriter) (v: obj) (serializer : JsonSerializer) =
            let typed = v :?> 'T
            f writer typed serializer
        typeof<'T>, boxedF

    let writeOnlyConverter (converterOfType, f) = 
        { new JsonConverter() with
            member x.CanConvert(t:System.Type) = t = converterOfType

            member x.WriteJson(writer, value, serializer) = f writer value serializer

            member x.ReadJson(_reader, _t, _, _serializer) =
              raise (System.NotSupportedException())

            member x.CanRead = false
            member x.CanWrite = true }

    let writers =
        [| jsonWriter fsharpErrorSeverityWriter
           jsonWriter rangeWriter
           jsonWriter projectSdkTypeWriter
           jsonWriter projectOutputTypeWriter |]

    writers
    |> Array.map writeOnlyConverter

  let internal writeJson(o: obj) = JsonConvert.SerializeObject(o, jsonConverters)
