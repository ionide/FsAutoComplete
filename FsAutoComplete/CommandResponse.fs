namespace FsAutoComplete

open System
open Newtonsoft.Json
open Newtonsoft.Json.Converters
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices

module internal CompletionUtils =
  let map =
    [ 0x0000,  ("Class", "C")
      0x0003,  ("Enum", "E")
      0x00012, ("Struct", "S")
      0x00018, ("Struct", "S") (* value type *)
      0x0002,  ("Delegate", "D")
      0x0008,  ("Interface", "I")
      0x000e,  ("Module", "N") (* module *)
      0x000f,  ("Namespace", "N")
      0x000c,  ("Method", "M")
      0x000d,  ("Extension Method", "M") (* method2 ? *)
      0x00011, ("Property", "P")
      0x0005,  ("Event", "e")
      0x0007,  ("Field", "F") (* fieldblue ? *)
      0x0020,  ("Field", "F") (* fieldyellow ? *)
      0x0001,  ("Field", "F") (* const *)
      0x0004,  ("Property", "P") (* enummember *)
      0x0006,  ("Exception", "X") (* exception *)
      0x0009,  ("Text File Icon", "t") (* TextLine *)
      0x000a,  ("Regular File", "R") (* Script *)
      0x000b,  ("Script", "s") (* Script2 *)
      0x0010,  ("Tip of the day", "t") (* Formula *);
      0x00013, ("Class", "C") (* Template *)
      0x00014, ("Class", "C") (* Typedef *)
      0x00015, ("Type", "T") (* Type *)
      0x00016, ("Type", "T") (* Union *)
      0x00017, ("Field", "F") (* Variable *)
      0x00019, ("Class", "C") (* Intrinsic *)
      0x0001f, ("Other", "o") (* error *)
      0x00021, ("Other", "o") (* Misc1 *)
      0x0022,  ("Other", "o") (* Misc2 *)
      0x00023, ("Other", "o") (* Misc3 *) ] |> Map.ofSeq

  let getIcon glyph =
    match map.TryFind (glyph / 6), map.TryFind (glyph % 6) with
    | Some(s), _ -> s // Is the second number good for anything?
    | _, _ -> ("", "")

module CommandResponse =

  type ResponseMsg<'T> =
    {
      Kind: string
      Data: 'T
    }

  type Location =
    {
      File: string
      Line: int
      Column: int
    }

  type CompletionResponse =
    {
      Name: string
      Glyph: string
      GlyphChar: string
    }

  type ProjectResponse =
    {
      Project: string
      Files: List<string>
      Output: string
      References: List<string>
      Framework: string
    }

  type OverloadParameter =
    {
      Name : string
      CanonicalTypeTextForSorting : string
      Display : string
      Description : string
    }
  type Overload =
    {
      Tip : string
      TypeText : string
      Parameters : OverloadParameter list
      IsStaticArguments : bool
    }
  type MethodResponse =
    {
      Name : string
      CurrentParameter : int
      Overloads : Overload list
    }

  type SymbolUseRange =
    {
      FileName: string
      StartLine: int
      StartColumn: int
      EndLine: int
      EndColumn: int
      IsFromDefinition: bool
      IsFromAttribute : bool
      IsFromComputationExpression : bool
      IsFromDispatchSlotImplementation : bool
      IsFromPattern : bool
      IsFromType : bool
    }

  type SymbolUseResponse =
    {
      Name: string
      Uses: SymbolUseRange list
    }

  type HelpTextResponse =
    {
      Name: string
      Text: string
    }

  type CompilerLocationResponse =
    {
      Fsc: string
      Fsi: string
      MSBuild: string
    }

  type FSharpErrorInfo =
    {
      FileName: string
      StartLine:int
      EndLine:int
      StartColumn:int
      EndColumn:int
      Severity:FSharpErrorSeverity
      Message:string
      Subcategory:string
    }
    static member OfFSharpError(e:Microsoft.FSharp.Compiler.FSharpErrorInfo) =
      {
        FileName = e.FileName
        StartLine = e.StartLineAlternate
        EndLine = e.EndLineAlternate
        StartColumn = e.StartColumn + 1
        EndColumn = e.EndColumn + 1
        Severity = e.Severity
        Message = e.Message
        Subcategory = e.Subcategory
      }

  type Colorization =
    {
      Range: Range.range
      Kind: string
    }

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

  let private writeJson(o: obj) = Console.WriteLine (JsonConvert.SerializeObject(o, jsonConverters))

  let info(s: string) = writeJson { Kind = "info"; Data = s }
  let error(s: string) = writeJson { Kind = "error"; Data = s }

  let helpText(name: string, tip: FSharpToolTipText) =
    let text = TipFormatter.formatTip tip
    writeJson { Kind = "helptext"; Data = { Name = name; Text = text } }

  let project(projectFileName, projectFiles, outFileOpt, references, frameworkOpt) =
    let projectData =
      { Project = projectFileName
        Files = projectFiles
        Output = match outFileOpt with Some x -> x | None -> "null"
        References = List.sortBy IO.Path.GetFileName references
        Framework = match frameworkOpt with Some x -> x | None -> "null" }
    writeJson { Kind = "project"; Data = projectData }

  let completion(decls: FSharpDeclarationListItem[]) =
    writeJson
      { Kind = "completion"
        Data = [ for d in decls do
                   let (glyph, glyphChar) = CompletionUtils.getIcon d.Glyph
                   yield { Name = d.Name; Glyph = glyph; GlyphChar = glyphChar } ] }

  let symbolUse(symbol: FSharpSymbolUse, uses: FSharpSymbolUse[]) =
    let su =
      { Name = symbol.Symbol.DisplayName
        Uses =
          [ for su in uses do
              yield { StartLine = su.RangeAlternate.StartLine
                      StartColumn = su.RangeAlternate.StartColumn + 1
                      EndLine = su.RangeAlternate.EndLine
                      EndColumn = su.RangeAlternate.EndColumn + 1
                      FileName = su.FileName
                      IsFromDefinition = su.IsFromDefinition
                      IsFromAttribute = su.IsFromAttribute
                      IsFromComputationExpression = su.IsFromComputationExpression
                      IsFromDispatchSlotImplementation = su.IsFromDispatchSlotImplementation
                      IsFromPattern = su.IsFromPattern
                      IsFromType = su.IsFromType } ] }
    writeJson { Kind = "symboluse"; Data = su }

  let methods(meth: FSharpMethodGroup, commas: int) =
    writeJson
      { Kind = "method"
        Data = { Name = meth.MethodName
                 CurrentParameter = commas
                 Overloads =
                  [ for o in meth.Methods do
                     let tip = TipFormatter.formatTip o.Description
                     yield {
                       Tip = tip
                       TypeText = o.TypeText
                       Parameters =
                         [ for p in o.Parameters do
                            yield {
                              Name = p.ParameterName
                              CanonicalTypeTextForSorting = p.CanonicalTypeTextForSorting
                              Display = p.Display
                              Description = p.Description
                            }
                       ]
                       IsStaticArguments = o.IsStaticArguments
                     }
                  ] }
        }

  let errors(errors: Microsoft.FSharp.Compiler.FSharpErrorInfo[]) =
    writeJson { Kind = "errors"
                Data = Seq.map FSharpErrorInfo.OfFSharpError errors }

  let colorizations(colorizations: (Range.range * FSharpTokenColorKind)[]) =
    let data = [ for r, k in colorizations do
                   yield { Range = r; Kind = Enum.GetName(typeof<FSharpTokenColorKind>, k) } ]
    writeJson { Kind = "colorizations"; Data = data }

  let findDeclaration(range: Range.range) =
    let data = { Line = range.StartLine; Column = range.StartColumn + 1; File = range.FileName }
    writeJson { Kind = "finddecl"; Data = data }

  let declarations(decls) =
    writeJson { Kind = "declarations"; Data = decls }

  let toolTip(tip) =
    writeJson { Kind = "tooltip"; Data = TipFormatter.formatTip tip }

  let compilerLocation fsc fsi msbuild =
    let data = { Fsi = fsi; Fsc = fsc; MSBuild = msbuild }
    writeJson { Kind = "compilerlocation"; Data = data }

  let message(kind: string, data: 'a) =
    writeJson { Kind = kind; Data = data }

