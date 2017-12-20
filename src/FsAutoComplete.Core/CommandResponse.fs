namespace FsAutoComplete

open System

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpLint.Application

module internal CompletionUtils =
  let getIcon (glyph : FSharpGlyph) =
    match glyph with
    | FSharpGlyph.Class -> ("Class", "C")
    | FSharpGlyph.Constant -> ("Constant", "Cn")
    | FSharpGlyph.Delegate -> ("Delegate", "D")
    | FSharpGlyph.Enum -> ("Enum", "E")
    | FSharpGlyph.EnumMember -> ("Property", "P")
    | FSharpGlyph.Event -> ("Event", "e")
    | FSharpGlyph.Exception -> ("Exception", "X")
    | FSharpGlyph.Field -> ("Field", "F")
    | FSharpGlyph.Interface -> ("Interface", "I")
    | FSharpGlyph.Method -> ("Method", "M")
    | FSharpGlyph.OverridenMethod -> ("Method", "M")
    | FSharpGlyph.Module -> ("Module", "N")
    | FSharpGlyph.NameSpace -> ("Namespace", "N")
    | FSharpGlyph.Property -> ("Property", "P")
    | FSharpGlyph.Struct -> ("Struct", "S")
    | FSharpGlyph.Typedef -> ("Class", "C")
    | FSharpGlyph.Type -> ("Type", "T")
    | FSharpGlyph.Union -> ("Type", "T")
    | FSharpGlyph.Variable -> ("Variable", "V")
    | FSharpGlyph.ExtensionMethod -> ("Extension Method", "M")
    | FSharpGlyph.Error -> ("Error", "E")


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
  open Microsoft.FSharp.Compiler.Range

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
      NamespaceToOpen: string option
    }

  type ResponseError<'T> =
    {
      Code: int
      Message: string
      AdditionalData: 'T
    }

  [<RequireQualifiedAccess>]
  type ErrorCodes =
    | GenericError = 1
    | ProjectNotRestored = 100
    | ProjectParsingFailed = 101

  [<RequireQualifiedAccess>]
  type ErrorData =
    | GenericError
    | ProjectNotRestored of ProjectNotRestoredData
    | ProjectParsingFailed of ProjectParsingFailedData
  and ProjectNotRestoredData = { Project: ProjectFilePath }
  and ProjectParsingFailedData = { Project: ProjectFilePath }

  [<RequireQualifiedAccess>]
  type ProjectResponseInfo =
    | DotnetSdk of ProjectResponseInfoDotnetSdk
    | Verbose
    | ProjectJson
  and ProjectResponseInfoDotnetSdk =
    {
      IsTestProject: bool
      Configuration: string
      IsPackable: bool
      TargetFramework: string
      TargetFrameworkIdentifier: string
      TargetFrameworkVersion: string
      RestoreSuccess: bool
      TargetFrameworks: string list
      RunCmd: RunCmd option
      IsPublishable: bool option
    }
  and [<RequireQualifiedAccess>] RunCmd = { Command: string; Arguments: string }

  type ProjectLoadingResponse =
    {
      Project: ProjectFilePath
    }

  type ProjectResponse =
    {
      Project: ProjectFilePath
      Files: List<SourceFilePath>
      Output: string
      References: List<ProjectFilePath>
      Logs: Map<string, string>
      OutputType: ProjectOutputType
      Info: ProjectResponseInfo
      AdditionalInfo: Map<string, string>
    }

  type OverloadDescription =
    {
      Signature: string
      Comment: string
    }

  type TooltipDescription =
    {
      Signature: string
      Comment: string
      Footer: string
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

  type AdditionalEdit =
    {
      Text: string
      Line: int
      Column: int
    }


  type HelpTextResponse =
    {
      Name: string
      Overloads: OverloadDescription list list
      AdditionalEdit: AdditionalEdit option
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
    static member OfFSharpError(e:Microsoft.FSharp.Compiler.SourceCodeServices.FSharpErrorInfo) =
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

  type ErrorResponse =
    {
      File: string
      Errors: FSharpErrorInfo []
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

  type OpenNamespace = {
    Namespace : string
    Name : string
    Type : string
    Line : int
    Column : int
    MultipleNames : bool
  }

  type QualifySymbol = {
    Name : string
    Qualifier : string
  }

  type ResolveNamespaceResponse = {
    Opens : OpenNamespace []
    Qualifies: QualifySymbol []
    Word : string
  }

  type UnionCaseResponse = {
    Text : string
    Position : pos
  }

  type Parameter = {
    Name : string
    Type : string
  }

  type SignatureData = {
    OutputType : string
    Parameters : Parameter list list
  }

  type UnusedDeclaration = {
    Range: Range.range
    IsThisMember: bool
  }

  type UnusedDeclarations = {
    Declarations : UnusedDeclaration []
  }

  type UnusedOpens = {
    Declarations : Range.range []
  }

  type SimplifiedNameData = {
    RelativeName : string
    UnnecessaryRange: Range.range
  }

  type SimplifiedName = {
    Names: SimplifiedNameData []
  }

  type WorkspacePeekResponse = {
    Found: WorkspacePeekFound list
  }
  and WorkspacePeekFound =
    | Directory of WorkspacePeekFoundDirectory
    | Solution of WorkspacePeekFoundSolution
  and WorkspacePeekFoundDirectory = {
    Directory: string
    Fsprojs: string list
  }
  and WorkspacePeekFoundSolution = {
    Path: string
    Items: WorkspacePeekFoundSolutionItem list
    Configurations: WorkspacePeekFoundSolutionConfiguration list
  }
  and [<RequireQualifiedAccess>] WorkspacePeekFoundSolutionItem = {
    Guid: Guid
    Name: string
    Kind: WorkspacePeekFoundSolutionItemKind
  }
  and WorkspacePeekFoundSolutionItemKind =
    | MsbuildFormat of WorkspacePeekFoundSolutionItemKindMsbuildFormat
    | Folder of WorkspacePeekFoundSolutionItemKindFolder
  and [<RequireQualifiedAccess>] WorkspacePeekFoundSolutionItemKindMsbuildFormat = {
    Configurations: WorkspacePeekFoundSolutionConfiguration list
  }
  and [<RequireQualifiedAccess>] WorkspacePeekFoundSolutionItemKindFolder = {
    Items: WorkspacePeekFoundSolutionItem list
    Files: FilePath list
  }
  and [<RequireQualifiedAccess>] WorkspacePeekFoundSolutionConfiguration = {
    Id: string
    ConfigurationName: string
    PlatformName: string
  }

  type WorkspaceLoadResponse = {
    Status: string
    }

  let info (serialize : Serializer) (s: string) = serialize { Kind = "info"; Data = s }

  let errorG (serialize : Serializer) (errorData: ErrorData) message =
    let inline ser code data =
        serialize { Kind = "error"; Data = { Code = (int code); Message = message; AdditionalData = data }  }
    match errorData with
    | ErrorData.GenericError -> ser (ErrorCodes.GenericError) (obj())
    | ErrorData.ProjectNotRestored d -> ser (ErrorCodes.ProjectNotRestored) d
    | ErrorData.ProjectParsingFailed d -> ser (ErrorCodes.ProjectParsingFailed) d

  let error (serialize : Serializer) (s: string) = errorG serialize ErrorData.GenericError s

  let helpText (serialize : Serializer) (name: string, tip: FSharpToolTipText, additionalEdit ) =
    let data = TipFormatter.formatTip tip |> List.map(List.map(fun (n,m) -> {OverloadDescription.Signature = n; Comment = m} ))
    let additionalEdit = additionalEdit |> Option.map (fun (n, l, c) -> {AdditionalEdit.Text = n; Line = l; Column = c})
    serialize { Kind = "helptext"; Data = { HelpTextResponse.Name = name; Overloads = data; AdditionalEdit = additionalEdit  } }

  let project (serialize : Serializer) (projectFileName, projectFiles, outFileOpt, references, logMap, (extra: ExtraProjectInfoData), additionals) =
    let projectInfo =
      match extra.ProjectSdkType with
      | ProjectSdkType.Verbose _ ->
        ProjectResponseInfo.Verbose
      | ProjectSdkType.ProjectJson ->
        ProjectResponseInfo.ProjectJson
      | ProjectSdkType.DotnetSdk info ->
        ProjectResponseInfo.DotnetSdk {
          IsTestProject = info.IsTestProject
          Configuration = info.Configuration
          IsPackable = info.IsPackable
          TargetFramework = info.TargetFramework
          TargetFrameworkIdentifier = info.TargetFrameworkIdentifier
          TargetFrameworkVersion = info.TargetFrameworkVersion
          RestoreSuccess = info.RestoreSuccess
          TargetFrameworks = info.TargetFrameworks
          RunCmd =
            match info.RunCommand, info.RunArguments with
            | Some cmd, Some args -> Some { RunCmd.Command = cmd; Arguments = args }
            | Some cmd, None -> Some { RunCmd.Command = cmd; Arguments = "" }
            | _ -> None
          IsPublishable = info.IsPublishable
        }
    let projectData =
      { Project = projectFileName
        Files = projectFiles
        Output = match outFileOpt with Some x -> x | None -> "null"
        References = List.sortBy IO.Path.GetFileName references
        Logs = logMap
        OutputType = extra.ProjectOutputType
        Info = projectInfo
        AdditionalInfo = additionals }
    serialize { Kind = "project"; Data = projectData }

  let projectError (serialize : Serializer) errorDetails =
    match errorDetails with
    | GenericError (project, errorMessage) -> errorG serialize (ErrorData.ProjectParsingFailed { Project = project }) errorMessage
    | ProjectNotRestored project -> errorG serialize (ErrorData.ProjectNotRestored { Project = project }) "Project not restored"

  let projectLoading (serialize : Serializer) projectFileName =
    serialize { Kind = "projectLoading"; Data = { ProjectLoadingResponse.Project = projectFileName } }

  let workspacePeek (serialize : Serializer) (found: FsAutoComplete.WorkspacePeek.Interesting list) =
    let mapInt i =
        match i with
        | FsAutoComplete.WorkspacePeek.Interesting.Directory (p, fsprojs) ->
            WorkspacePeekFound.Directory { WorkspacePeekFoundDirectory.Directory = p; Fsprojs = fsprojs }
        | FsAutoComplete.WorkspacePeek.Interesting.Solution (p, sd) ->
            let rec item (x: FsAutoComplete.WorkspacePeek.SolutionItem) =
                let kind =
                    match x.Kind with
                    | FsAutoComplete.WorkspacePeek.SolutionItemKind.Unknown
                    | FsAutoComplete.WorkspacePeek.SolutionItemKind.Unsupported ->
                        None
                    | FsAutoComplete.WorkspacePeek.SolutionItemKind.MsbuildFormat msbuildProj ->
                        Some (WorkspacePeekFoundSolutionItemKind.MsbuildFormat {
                            WorkspacePeekFoundSolutionItemKindMsbuildFormat.Configurations = []
                        })
                    | FsAutoComplete.WorkspacePeek.SolutionItemKind.Folder(children, files) ->
                        let c = children |> List.choose item
                        Some (WorkspacePeekFoundSolutionItemKind.Folder {
                            WorkspacePeekFoundSolutionItemKindFolder.Items = c
                            Files = files
                        })
                kind
                |> Option.map (fun k -> { WorkspacePeekFoundSolutionItem.Guid = x.Guid; Name = x.Name; Kind = k })
            let items = sd.Items |> List.choose item
            WorkspacePeekFound.Solution { WorkspacePeekFoundSolution.Path = p; Items = items; Configurations = [] }

    let data = { WorkspacePeekResponse.Found = found |> List.map mapInt }
    serialize { Kind = "workspacePeek"; Data = data }

  let workspaceLoad (serialize : Serializer) finished =
    let data =
        if finished then "finished" else "started"
    serialize { Kind = "workspaceLoad"; Data = { WorkspaceLoadResponse.Status = data } }

  let completion (serialize : Serializer) (decls: FSharpDeclarationListItem[]) includeKeywords =
      serialize {  Kind = "completion"
                   Data = [ for d in decls do
                               let code = PrettyNaming.QuoteIdentifierIfNeeded d.Name
                               let (glyph, glyphChar) = CompletionUtils.getIcon d.Glyph
                               yield {CompletionResponse.Name = d.Name; ReplacementText = code; Glyph = glyph; GlyphChar = glyphChar; NamespaceToOpen = d.NamespaceToOpen }
                            if includeKeywords then
                              for k in KeywordList.allKeywords do
                                yield {CompletionResponse.Name = k; ReplacementText = k; Glyph = "Keyword"; GlyphChar = "K"; NamespaceToOpen = None}
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

  let signatureData (serialize : Serializer) ((typ, parms) : string * ((string * string) list list) ) =
    let pms =
      parms
      |> List.map (List.map (fun (n, t) -> { Name= n; Type = t }))
    serialize { Kind = "signatureData"; Data = { Parameters = pms; OutputType = typ } }

  let help (serialize : Serializer) (data : string) =
    serialize { Kind = "help"; Data = data }

  let methods (serialize : Serializer) (meth: FSharpMethodGroup, commas: int) =
      serialize {  Kind = "method"
                   Data = {  Name = meth.MethodName
                             CurrentParameter = commas
                             Overloads =
                              [ for o in meth.Methods do
                                 let tip = TipFormatter.formatTip o.Description |> List.map(List.map(fun (n,m) -> {OverloadDescription.Signature = n; Comment = m} ))
                                 yield {
                                   Tip = tip
                                   TypeText = o.ReturnTypeText
                                   Parameters =
                                     [ for p in o.Parameters do
                                        yield {
                                          Name = p.ParameterName
                                          CanonicalTypeTextForSorting = p.CanonicalTypeTextForSorting
                                          Display = p.Display
                                          Description = "" //TODO: Investigate if Description is needed at all - not used in Ionide, check in Emacs and vim.
                                        }
                                   ]
                                   IsStaticArguments = not o.HasParameters
                                 }
                              ] }
                }

  let errors (serialize : Serializer) (errors: Microsoft.FSharp.Compiler.SourceCodeServices.FSharpErrorInfo[], file: string) =
    serialize { Kind = "errors";
                Data = { File = file
                         Errors = Array.map FSharpErrorInfo.OfFSharpError errors }}

  let colorizations (serialize : Serializer) (colorizations: (Range.range * SemanticClassificationType)[]) =
    // let data = [ for r, k in colorizations do
    //                yield { Range = r; Kind = Enum.GetName(typeof<SemanticClassificationType>, k) } ]
    serialize { Kind = "colorizations"; Data = [] } //TODO: Fix colorization

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

  let toolTip (serialize : Serializer) (tip, signature, footer) =
    let data = TipFormatter.formatTipEnhanced tip signature footer |> List.map(List.map(fun (n,m,f) -> {Footer =f; Signature = n; Comment = m} ))
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


  let resolveNamespace (serialize : Serializer) (word: string, opens : (string * string * InsertContext * bool) list, qualfies : (string * string) list) =
    let ops =
      opens
      |> List.map (fun (ns, name, ctx, multiple) ->
        {
          Namespace = ns
          Name = name
          Type = ctx.ScopeKind.ToString()
          Line = ctx.Pos.Line
          Column = ctx.Pos.Column
          MultipleNames = multiple
        })
      |> List.toArray

    let quals =
      qualfies
      |> List.map (fun (name, q) ->
        {
          QualifySymbol.Name = name
          QualifySymbol.Qualifier = q
        })
      |> List.toArray

    let data = {
      Opens = ops
      Qualifies = quals
      Word = word
    }

    serialize { Kind = "namespaces"; Data = data}

  let unionCase (serialize : Serializer) (text : string) position =
    let data = {
      Text = text
      Position = position
    }
    serialize { Kind = "unionCase"; Data = data}

  let unusedDeclarations (serialize : Serializer) data =
    let data =
      data |> Array.map (fun (r, t) ->
        {
          UnusedDeclaration.Range = r
          IsThisMember = t
        })
    let data = {UnusedDeclarations.Declarations = data}
    serialize { Kind = "unusedDeclarations"; Data = data}

  let unusedOpens (serialize : Serializer) data =
    let data = {UnusedOpens.Declarations = data}
    serialize { Kind = "unusedOpens"; Data = data}

  let simplifiedNames (serialize : Serializer) data =
    let data = {
      SimplifiedName.Names = data |> Seq.map (fun (r,n) -> { SimplifiedNameData.RelativeName = n; SimplifiedNameData.UnnecessaryRange =r }) |> Seq.toArray
    }
    serialize { Kind = "simpifiedNames"; Data = data}

