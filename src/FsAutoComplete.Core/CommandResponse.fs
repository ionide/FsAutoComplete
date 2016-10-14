namespace FsAutoComplete

open System

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpLint.Application

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
      0x0020,  ("Field", "Fy") (* fieldyellow ? *)
      0x0001,  ("Function", "Fc") (* const *)
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
      0x00017, ("Field", "V") (* Variable *)
      0x00019, ("Class", "C") (* Intrinsic *)
      0x0001f, ("Other", "o") (* error *)
      0x00021, ("Other", "o") (* Misc1 *)
      0x0022,  ("Other", "o") (* Misc2 *)
      0x00023, ("Other", "o") (* Misc3 *) ] |> Map.ofSeq

  let getIcon glyph =
    match map.TryFind (glyph / 6), map.TryFind (glyph % 6) with
    | Some(s), _ -> s // Is the second number good for anything?
    | _, _ -> ("", "")

  let getEnclosingEntityChar = function
    | FSharpEnclosingEntityKind.Namespace -> "N"
    | FSharpEnclosingEntityKind.Module -> "M"
    | FSharpEnclosingEntityKind.Class -> "C"
    | FSharpEnclosingEntityKind.Exception -> "E"
    | FSharpEnclosingEntityKind.Interface -> "I"
    | FSharpEnclosingEntityKind.Record -> "R"
    | FSharpEnclosingEntityKind.Enum -> "En"
    | FSharpEnclosingEntityKind.DU -> "D"

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
      ReplacementText: string
      Glyph: string
      GlyphChar: string
    }

  type ProjectResponse =
    {
      Project: ProjectFilePath
      Files: List<SourceFilePath>
      Output: string
      References: List<ProjectFilePath>
      Logs: Map<string, string>
    }

  type OverloadDescription =
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
      Tip : OverloadDescription list list
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
      Overloads: OverloadDescription list list
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
      File : string
      EnclosingEntity: string
      IsAbstract: bool
    }
    static member OfDeclarationItem(e:FSharpNavigationDeclarationItem, fn) =
      let (glyph, glyphChar) = CompletionUtils.getIcon e.Glyph
      {
        UniqueName = e.UniqueName
        Name = e.Name
        Glyph = glyph
        GlyphChar = glyphChar
        IsTopLevel = e.IsSingleTopLevel
        Range = e.Range
        BodyRange = e.BodyRange
        File = fn
        EnclosingEntity = CompletionUtils.getEnclosingEntityChar e.EnclosingEntityKind
        IsAbstract = e.IsAbstract
      }

  type DeclarationResponse = {
      Declaration : Declaration;
      Nested : Declaration []
  }

  let info (serialize : Serializer) (s: string) = serialize { Kind = "info"; Data = s }
  let error (serialize : Serializer) (s: string) = serialize { Kind = "error"; Data = s }

  let helpText (serialize : Serializer) (name: string, tip: FSharpToolTipText) =
    let data = TipFormatter.formatTip tip |> List.map(List.map(fun (n,m) -> {Signature = n; Comment = m} ))
    serialize { Kind = "helptext"; Data = { HelpTextResponse.Name = name; Overloads = data } }

  let project (serialize : Serializer) (projectFileName, projectFiles, outFileOpt, references, logMap) =
    let projectData =
      { Project = projectFileName
        Files = projectFiles
        Output = match outFileOpt with Some x -> x | None -> "null"
        References = List.sortBy IO.Path.GetFileName references
        Logs = logMap }
    serialize { Kind = "project"; Data = projectData }

  let completion (serialize : Serializer) (decls: FSharpDeclarationListItem[]) =
      let keywords = ["abstract"; "and"; "as"; "assert"; "base"; "begin"; "class"; "default"; "delegate"; "do";
          "done"; "downcast"; "downto"; "elif"; "else"; "end"; "exception"; "extern"; "false"; "finally"; "for";
          "fun"; "function"; "global"; "if"; "in"; "inherit"; "inline"; "interface"; "internal"; "lazy"; "let";
          "match"; "member"; "module"; "mutable"; "namespace"; "new"; "null"; "of"; "open"; "or"; "override";
          "private"; "public"; "rec"; "return"; "sig"; "static"; "struct"; "then"; "to"; "true"; "try"; "type";
          "upcast"; "use"; "val"; "void"; "when"; "while"; "with"; "yield"
      ]

      serialize {  Kind = "completion"
                   Data = [ for d in decls do
                               let code = Microsoft.FSharp.Compiler.SourceCodeServices.PrettyNaming.QuoteIdentifierIfNeeded d.Name
                               let (glyph, glyphChar) = CompletionUtils.getIcon d.Glyph
                               yield {CompletionResponse.Name = d.Name; ReplacementText = code; Glyph = glyph; GlyphChar = glyphChar }
                            for k in keywords do
                              yield {CompletionResponse.Name = k; ReplacementText = k; Glyph = "Keyword"; GlyphChar = "K"}
                          ] }

  let symbolUse (serialize : Serializer) (symbol: FSharpSymbolUse, uses: FSharpSymbolUse[]) =
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
                      IsFromType = su.IsFromType } ] |> Seq.distinct |> Seq.toList }
    serialize { Kind = "symboluse"; Data = su }

  let methods (serialize : Serializer) (meth: FSharpMethodGroup, commas: int) =
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
                                   IsStaticArguments = not o.HasParameters
                                 }
                              ] }
                }

  let errors (serialize : Serializer) (errors: Microsoft.FSharp.Compiler.FSharpErrorInfo[]) =
    serialize { Kind = "errors";  Data = Seq.map FSharpErrorInfo.OfFSharpError errors }

  let colorizations (serialize : Serializer) (colorizations: (Range.range * FSharpTokenColorKind)[]) =
    let data = [ for r, k in colorizations do
                   yield { Range = r; Kind = Enum.GetName(typeof<FSharpTokenColorKind>, k) } ]
    serialize { Kind = "colorizations"; Data = data }

  let findDeclaration (serialize : Serializer) (range: Range.range) =
    let data = { Line = range.StartLine; Column = range.StartColumn + 1; File = range.FileName }
    serialize { Kind = "finddecl"; Data = data }

  let declarations (serialize : Serializer) (decls : (FSharpNavigationTopLevelDeclaration * string) []) =
     let decls' =
      decls |> Array.map (fun (d, fn) ->
        { Declaration = Declaration.OfDeclarationItem (d.Declaration, fn);
          Nested = d.Nested |> Array.map ( fun a -> Declaration.OfDeclarationItem(a,fn))
        })
     serialize { Kind = "declarations"; Data = decls' }

  let toolTip (serialize : Serializer) (tip) =
    let data = TipFormatter.formatTip tip |> List.map(List.map(fun (n,m) -> {Signature = n; Comment = m} ))
    serialize { Kind = "tooltip"; Data = data }

  let typeSig (serialize : Serializer) (tip) =
    let data = TipFormatter.extractSignature tip
    serialize { Kind = "typesig"; Data = data }

  let compilerLocation (serialize : Serializer) fsc fsi msbuild =
    let data = { Fsi = fsi; Fsc = fsc; MSBuild = msbuild }
    serialize { Kind = "compilerlocation"; Data = data }

  let message (serialize : Serializer) (kind: string, data: 'a) =
    serialize { Kind = kind; Data = data }

  let lint (serialize : Serializer) (warnings : LintWarning.Warning list) =
    let data = warnings |> List.toArray

    serialize { Kind = "lint"; Data = data }
