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
      Filename: string
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

  type FSharpErrorSeverityConverter() =
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

  type RangeConverter() =
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

  type internal ResponseAgent() =
    let agent = MailboxProcessor.Start(fun agent ->
      let rec loop () = async {
          let! (msg: Choice<string,AsyncReplyChannel<unit>>) = agent.Receive()
          match msg with
          | Choice1Of2 (s: string) -> Console.WriteLine s; return! loop ()
          | Choice2Of2 ch -> ch.Reply ()
        }
      loop ()
      )

    let jsonConverters =
      [|
       new FSharpErrorSeverityConverter() :> JsonConverter;
       new RangeConverter() :> JsonConverter
      |]

    member private x.Write(s) = agent.Post (Choice1Of2 s)

    member x.WriteJson(o: obj) = x.Write (JsonConvert.SerializeObject(o, jsonConverters))

    member x.Info(s) = x.WriteJson { Kind = "info"; Data = s }
    member x.Error(s) = x.WriteJson { Kind = "error"; Data = s }

    member x.Quit() = agent.PostAndReply(fun ch -> Choice2Of2 ch)

    member x.HelpText(name: string, tip: FSharpToolTipText) =
      let text = TipFormatter.formatTip tip
      x.WriteJson { Kind = "helptext"; Data = { Name = name; Text = text } }

    member x.Project(projectFileName, projectFiles, outFileOpt, references, frameworkOpt) =
      let projectData =
        { Project = projectFileName
          Files = projectFiles
          Output = match outFileOpt with Some x -> x | None -> "null"
          References = List.sortBy IO.Path.GetFileName references
          Framework = match frameworkOpt with Some x -> x | None -> "null" }
      x.WriteJson { Kind = "project"; Data = projectData }

    member x.Completion(decls: FSharpDeclarationListItem[]) =
      x.WriteJson
        { Kind = "completion"
          Data = [ for d in decls do
                     let (glyph, glyphChar) = CompletionUtils.getIcon d.Glyph
                     yield { Name = d.Name; Glyph = glyph; GlyphChar = glyphChar } ] }

    member x.SymbolUse(symboluses: FSharpSymbolUse[]) =
      let su =
        { Name = symboluses.[0].Symbol.DisplayName
          Uses =
            [ for su in symboluses do
                yield { StartLine = su.RangeAlternate.StartLine
                        StartColumn = su.RangeAlternate.StartColumn + 1
                        EndLine = su.RangeAlternate.EndLine
                        EndColumn = su.RangeAlternate.EndColumn + 1
                        Filename = su.FileName
                        IsFromDefinition = su.IsFromDefinition
                        IsFromAttribute = su.IsFromAttribute
                        IsFromComputationExpression = su.IsFromComputationExpression
                        IsFromDispatchSlotImplementation = su.IsFromDispatchSlotImplementation
                        IsFromPattern = su.IsFromPattern
                        IsFromType = su.IsFromType } ] }
      x.WriteJson { Kind = "symboluse"; Data = su }

    member x.Method(meth: FSharpMethodGroup, commas: int) =
      x.WriteJson
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

    member x.Errors(errors: Microsoft.FSharp.Compiler.FSharpErrorInfo[]) =
      x.WriteJson { Kind = "errors"
                    Data = Seq.map FSharpErrorInfo.OfFSharpError errors }

    member x.FindDeclaration(range: Range.range) =
      let data = { Line = range.StartLine; Column = range.StartColumn + 1; File = range.FileName }
      x.WriteJson { Kind = "finddecl"; Data = data }

    member x.Declarations(decls) =
      x.WriteJson { Kind = "declarations"; Data = decls }

    member x.ToolTip(tip) =
      x.WriteJson { Kind = "tooltip"; Data = TipFormatter.formatTip tip }

    member x.Message(kind: string, data: 'a) =
      x.WriteJson { Kind = kind; Data = data }

