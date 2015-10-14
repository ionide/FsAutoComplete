namespace FsAutoComplete

open System

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

  type OverloadSignature =
    {
      Signature: string
      Comment: string
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
      Tip : OverloadSignature list list
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
      Overloads: OverloadSignature list list
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



  type Declaration =
    {
      UniqueName: string
      Name: string
      Glyph: string
      GlyphChar: string
      IsTopLevel: bool
      Range: Range.range
      BodyRange : Range.range
    }
    static member OfDeclarationItem(e:FSharpNavigationDeclarationItem) =
      let (glyph, glyphChar) = CompletionUtils.getIcon e.Glyph
      {
        UniqueName = e.UniqueName
        Name = e.Name
        Glyph = glyph
        GlyphChar = glyphChar
        IsTopLevel = e.IsSingleTopLevel
        Range = e.Range
        BodyRange = e.BodyRange
      }
  type DeclarationResponse = {
      Declaration : Declaration;
      Nested : Declaration []
  }



  let info (serialize : obj -> string) (s: string) = serialize { Kind = "info"; Data = s }
  let error (serialize : obj -> string) (s: string) = serialize { Kind = "error"; Data = s }

  let helpText (serialize : obj -> string) (name: string, tip: FSharpToolTipText) =
    let data = TipFormatter.formatTip tip |> List.map(List.map(fun (n,m) -> {Signature = n; Comment = m} ))
    serialize { Kind = "helptext"; Data = { HelpTextResponse.Name = name; Overloads = data } }

  let project (serialize : obj -> string) (projectFileName, projectFiles, outFileOpt, references, frameworkOpt) =
    let projectData =
      { Project = projectFileName
        Files = projectFiles
        Output = match outFileOpt with Some x -> x | None -> "null"
        References = List.sortBy IO.Path.GetFileName references
        Framework = match frameworkOpt with Some x -> x | None -> "null" }
    serialize { Kind = "project"; Data = projectData }

  let completion (serialize : obj -> string) (decls: FSharpDeclarationListItem[]) =
      serialize {  Kind = "completion"
                   Data = [ for d in decls do
                               let (glyph, glyphChar) = CompletionUtils.getIcon d.Glyph
                               yield { CompletionResponse.Name = d.Name; Glyph = glyph; GlyphChar = glyphChar } ] }

  let symbolUse (serialize : obj -> string) (symbol: FSharpSymbolUse, uses: FSharpSymbolUse[]) =
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
    serialize { Kind = "symboluse"; Data = su }

  let methods (serialize : obj -> string) (meth: FSharpMethodGroup, commas: int) =
      serialize {  Kind = "method"
                   Data = {  Name = meth.MethodName
                             CurrentParameter = commas
                             Overloads =
                              [ for o in meth.Methods do
                                 let tip = TipFormatter.formatTip o.Description |> List.map(List.map(fun (n,m) -> {Signature = n; Comment = m} ))
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

  let errors (serialize : obj -> string) (errors: Microsoft.FSharp.Compiler.FSharpErrorInfo[]) =
    serialize { Kind = "errors";  Data = Seq.map FSharpErrorInfo.OfFSharpError errors }

  let colorizations (serialize : obj -> string) (colorizations: (Range.range * FSharpTokenColorKind)[]) =
    let data = [ for r, k in colorizations do
                   yield { Range = r; Kind = Enum.GetName(typeof<FSharpTokenColorKind>, k) } ]
    serialize { Kind = "colorizations"; Data = data }

  let findDeclaration (serialize : obj -> string) (range: Range.range) =
    let data = { Line = range.StartLine; Column = range.StartColumn + 1; File = range.FileName }
    serialize { Kind = "finddecl"; Data = data }

  let declarations (serialize : obj -> string) (decls : FSharpNavigationTopLevelDeclaration[]) =
     let decls' =
      decls |> Array.map (fun d ->
        { Declaration = Declaration.OfDeclarationItem d.Declaration;
          Nested = d.Nested |> Array.map Declaration.OfDeclarationItem
        })
     serialize { Kind = "declarations"; Data = decls' }

  let toolTip (serialize : obj -> string) (tip) =
    let data = TipFormatter.formatTip tip |> List.map(List.map(fun (n,m) -> {Signature = n; Comment = m} ))
    serialize { Kind = "tooltip"; Data = data }

  let compilerLocation (serialize : obj -> string) fsc fsi msbuild =
    let data = { Fsi = fsi; Fsc = fsc; MSBuild = msbuild }
    serialize { Kind = "compilerlocation"; Data = data }

  let message (serialize : obj -> string) (kind: string, data: 'a) =
    serialize { Kind = kind; Data = data }
