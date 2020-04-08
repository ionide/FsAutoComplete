namespace FsAutoComplete

open System

open FSharp.Compiler
open FSharp.Compiler.SourceCodeServices
open ProjectSystem
open ProjectSystem.WorkspacePeek

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
  open FSharp.Compiler.Range

  type ResponseMsg<'T> =
    {
      Kind: string
      Data: 'T
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
    | GenericProjectError = 102

  [<RequireQualifiedAccess>]
  type ErrorData =
    | GenericError
    | ProjectNotRestored of ProjectNotRestoredData
    | ProjectParsingFailed of ProjectParsingFailedData
    | GenericProjectError of ProjectData
  and ProjectNotRestoredData = { Project: ProjectFilePath }
  and ProjectParsingFailedData = { Project: ProjectFilePath }
  and ProjectData = { Project: ProjectFilePath }

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
      Items: List<ProjectResponseItem>
      AdditionalInfo: Map<string, string>
    }
  and ProjectOutputType =
    | Library
    | Exe
    | Custom of string
  and ProjectResponseItem =
    {
      Name: string
      FilePath: string
      VirtualPath: string
      Metadata: Map<string, string>
    }

  type DocumentationDescription =
    {
      XmlKey: string
      Constructors: string list
      Fields: string list
      Functions: string list
      Interfaces: string list
      Attributes: string list
      Types: string list
      Signature: string
      Comment: string
      Footer: string
    }

  type CompilerLocationResponse =
    {
      Fsc: string option
      Fsi: string option
      MSBuild: string option
      SdkRoot: string option
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
    static member IsIgnored(e:FSharp.Compiler.SourceCodeServices.FSharpErrorInfo) =
        // FST-1027 support in Fake 5
        e.ErrorNumber = 213 && e.Message.StartsWith "'paket:"
    static member OfFSharpError(e:FSharp.Compiler.SourceCodeServices.FSharpErrorInfo) =
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

  type Parameter = {
    Name : string
    Type : string
  }

  type SignatureData = {
    OutputType : string
    Parameters : Parameter list list
    Generics : string list
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

  type CompileResponse = {
    Code: int
    Errors: FSharpErrorInfo []
  }

  type FsdnResponse = {
    Functions: string list
  }

  type DotnetNewListResponse = {
    Installed : DotnetNewTemplate.Template list
  }

  type DotnetNewGetDetailsResponse = {
    Detailed : DotnetNewTemplate.DetailedTemplate
  }

  type DotnetNewCreateCliResponse = {
    CommandName : string
    ParameterStr : string
  }

  type HighlightingRange = {Range: range; TokenType: string}

  type HighlightingResponse = {
    Highlights: HighlightingRange []
  }


  let private errorG (serialize : Serializer) (errorData: ErrorData) message =
    let inline ser code data =
        serialize { Kind = "error"; Data = { Code = (int code); Message = message; AdditionalData = data }  }
    match errorData with
    | ErrorData.GenericError -> ser (ErrorCodes.GenericError) (obj())
    | ErrorData.GenericProjectError d -> ser (ErrorCodes.GenericProjectError) d
    | ErrorData.ProjectNotRestored d -> ser (ErrorCodes.ProjectNotRestored) d
    | ErrorData.ProjectParsingFailed d -> ser (ErrorCodes.ProjectParsingFailed) d

  let project (serialize : Serializer) (projectResult: ProjectResult) =
    let projectInfo =
      match projectResult.extra.ProjectSdkType with
      | Dotnet.ProjInfo.Workspace.ProjectSdkType.Verbose _ ->
        ProjectResponseInfo.Verbose
      | Dotnet.ProjInfo.Workspace.ProjectSdkType.DotnetSdk info ->
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
    let mapItemResponse (p: Dotnet.ProjInfo.Workspace.ProjectViewerItem) : ProjectResponseItem =
      match p with
      | Dotnet.ProjInfo.Workspace.ProjectViewerItem.Compile (fullpath, extraInfo) ->
        { ProjectResponseItem.Name = "Compile"
          ProjectResponseItem.FilePath = fullpath
          ProjectResponseItem.VirtualPath = extraInfo.Link
          ProjectResponseItem.Metadata = Map.empty }

    let projectData =
      { Project = projectResult.projectFileName
        Files = projectResult.projectFiles
        Output = match projectResult.outFileOpt with Some x -> x | None -> "null"
        References = List.sortBy IO.Path.GetFileName projectResult.references
        Logs = projectResult.logMap
        OutputType =
          match projectResult.extra.ProjectOutputType with
          | Dotnet.ProjInfo.Workspace.ProjectOutputType.Library -> Library
          | Dotnet.ProjInfo.Workspace.ProjectOutputType.Exe -> Exe
          | Dotnet.ProjInfo.Workspace.ProjectOutputType.Custom outType -> Custom outType
        Info = projectInfo
        Items = projectResult.projectItems |> List.map mapItemResponse
        AdditionalInfo = projectResult.additionals }
    serialize { Kind = "project"; Data = projectData }

  let projectError (serialize : Serializer) errorDetails =
    let rec getMessageLines errorDetails =
      match errorDetails with
      | Dotnet.ProjInfo.Workspace.LanguageNotSupported (_) -> [sprintf "this project is not supported, only fsproj"]
      | Dotnet.ProjInfo.Workspace.ProjectNotLoaded (_) -> [sprintf "this project was not loaded due to some internal error"]
      | Dotnet.ProjInfo.Workspace.MissingExtraProjectInfos _ -> [sprintf "this project was not loaded because ExtraProjectInfos were missing"]
      | Dotnet.ProjInfo.Workspace.InvalidExtraProjectInfos (_, err) ->  [sprintf "this project was not loaded because ExtraProjectInfos were invalid: %s" err]
      | Dotnet.ProjInfo.Workspace.ReferencesNotLoaded (_, referenceErrors) ->

        [ yield sprintf "this project was not loaded because some references could not be loaded:"
          yield!
            referenceErrors
              |> Seq.collect (fun (projPath, er) ->
                [ yield sprintf "  - %s:" projPath
                  yield! getMessageLines er |> Seq.map (fun line -> sprintf "    - %s" line)]) ]
      | Dotnet.ProjInfo.Workspace.GenericError (_, errorMessage) -> [errorMessage]
      | Dotnet.ProjInfo.Workspace.ProjectNotRestored _ -> ["Project not restored"]
    let msg = getMessageLines errorDetails |> fun s -> String.Join("\n", s)
    match errorDetails with
    | Dotnet.ProjInfo.Workspace.LanguageNotSupported (project) -> errorG serialize (ErrorData.GenericProjectError { Project = project }) msg
    | Dotnet.ProjInfo.Workspace.ProjectNotLoaded project -> errorG serialize (ErrorData.GenericProjectError { Project = project }) msg
    | Dotnet.ProjInfo.Workspace.MissingExtraProjectInfos project -> errorG serialize (ErrorData.GenericProjectError { Project = project }) msg
    | Dotnet.ProjInfo.Workspace.InvalidExtraProjectInfos (project, _) -> errorG serialize (ErrorData.GenericProjectError { Project = project }) msg
    | Dotnet.ProjInfo.Workspace.ReferencesNotLoaded (project, _) -> errorG serialize (ErrorData.GenericProjectError { Project = project }) msg
    | Dotnet.ProjInfo.Workspace.GenericError (project, _) -> errorG serialize (ErrorData.ProjectParsingFailed { Project = project }) msg
    | Dotnet.ProjInfo.Workspace.ProjectNotRestored project -> errorG serialize (ErrorData.ProjectNotRestored { Project = project }) msg

  let projectLoading (serialize : Serializer) projectFileName =
    serialize { Kind = "projectLoading"; Data = { ProjectLoadingResponse.Project = projectFileName } }

  let workspacePeek (serialize : Serializer) (found: WorkspacePeek.Interesting list) =
    let mapInt i =
        match i with
        | WorkspacePeek.Interesting.Directory (p, fsprojs) ->
            WorkspacePeekFound.Directory { WorkspacePeekFoundDirectory.Directory = p; Fsprojs = fsprojs }
        | WorkspacePeek.Interesting.Solution (p, sd) ->
            let rec item (x: ProjectSystem.WorkspacePeek.SolutionItem) =
                let kind =
                    match x.Kind with
                    | SolutionItemKind.Unknown
                    | SolutionItemKind.Unsupported ->
                        None
                    | SolutionItemKind.MsbuildFormat msbuildProj ->
                        Some (WorkspacePeekFoundSolutionItemKind.MsbuildFormat {
                            WorkspacePeekFoundSolutionItemKindMsbuildFormat.Configurations = []
                        })
                    | SolutionItemKind.Folder(children, files) ->
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

  let signatureData (serialize : Serializer) ((typ, parms, generics) : string * ((string * string) list list) * string list) =
    let pms =
      parms
      |> List.map (List.map (fun (n, t) -> { Name= n; Type = t }))
    serialize { Kind = "signatureData"; Data = { Parameters = pms; OutputType = typ; Generics = generics } }

  let help (serialize : Serializer) (data : string) =
    serialize { Kind = "help"; Data = data }

  let fsdn (serialize : Serializer) (functions : string list) =
    let data = { FsdnResponse.Functions = functions }
    serialize { Kind = "fsdn"; Data = data }

  let dotnetnewlist (serialize : Serializer) (installedTemplate : DotnetNewTemplate.Template list) =
    serialize { Kind = "dotnetnewlist"; Data = installedTemplate }

  let dotnetnewgetDetails (serialize : Serializer) (detailedTemplate : DotnetNewTemplate.DetailedTemplate) =
    let data = { DotnetNewGetDetailsResponse.Detailed = detailedTemplate }
    serialize { Kind = "dotnetnewgetDetails"; Data = data }

  let dotnetnewCreateCli (serialize : Serializer) (commandName : string, parameterStr : string) =
    serialize { Kind = "dotnetnewCreateCli";
                Data = { CommandName = commandName
                         ParameterStr = parameterStr} }

  let declarations (serialize : Serializer) (decls : (FSharpNavigationTopLevelDeclaration * string) []) =
     let decls' =
      decls |> Array.map (fun (d, fn) ->
        { Declaration = Declaration.OfDeclarationItem (d.Declaration, fn);
          Nested = d.Nested |> Array.map ( fun a -> Declaration.OfDeclarationItem(a,fn))
        })
     serialize { Kind = "declarations"; Data = decls' }

  let formattedDocumentation (serialize : Serializer) (tip, xmlSig, signature, footer, cn) =
    let data =
      match tip, xmlSig  with
      | Some tip, _ ->
        TipFormatter.formatDocumentation tip signature footer cn |> List.map(List.map(fun (n,cns, fds, funcs, intf, attrs,ts, m,f, cn) -> {XmlKey = cn; Constructors = cns |> Seq.toList; Fields = fds |> Seq.toList; Functions = funcs |> Seq.toList; Interfaces = intf |> Seq.toList; Attributes = attrs |> Seq.toList; Types = ts |> Seq.toList; Footer =f; Signature = n; Comment = m} ))
      | _, Some (xml, assembly) ->
        TipFormatter.formatDocumentationFromXmlSig xml assembly signature footer cn |> List.map(List.map(fun (n,cns, fds, funcs, intf, attrs,ts, m,f, cn) -> {XmlKey = cn; Constructors = cns |> Seq.toList; Fields = fds |> Seq.toList; Functions = funcs |> Seq.toList; Interfaces = intf |> Seq.toList; Attributes = attrs |> Seq.toList; Types = ts |> Seq.toList; Footer =f; Signature = n; Comment = m} ))
      | _ -> failwith "Shouldn't happen"
    serialize { Kind = "formattedDocumentation"; Data = data }

  let formattedDocumentationForSymbol (serialize : Serializer) xml assembly (xmlDoc : string list) (signature, footer, cn) =
    let data = TipFormatter.formatDocumentationFromXmlSig xml assembly signature footer cn |> List.map(List.map(fun (n,cns, fds, funcs, intf, attrs, ts, m,f, cn) ->
      let m = if String.IsNullOrWhiteSpace m then xmlDoc |> String.concat "\n" else m
      {XmlKey = cn; Constructors = cns |> Seq.toList; Fields = fds |> Seq.toList; Functions = funcs |> Seq.toList; Interfaces = intf |> Seq.toList; Attributes = attrs |> Seq.toList; Types = ts |> Seq.toList; Footer =f; Signature = n; Comment = m} ))
    serialize { Kind = "formattedDocumentation"; Data = data }

  let typeSig (serialize : Serializer) (tip) =
    let data = TipFormatter.extractSignature tip
    serialize { Kind = "typesig"; Data = data }

  let compilerLocation (serialize : Serializer) fsc fsi msbuild sdkRoot =
    let data = { Fsi = fsi; Fsc = fsc; MSBuild = msbuild; SdkRoot = sdkRoot }
    serialize { Kind = "compilerlocation"; Data = data }

  let compile (serialize : Serializer) (errors,code) =
    serialize { Kind = "compile"; Data = {Code = code; Errors = Array.map FSharpErrorInfo.OfFSharpError errors}}

  let fakeTargets (serialize : Serializer) (targets : FakeSupport.GetTargetsResult) =
     serialize targets

  let fakeRuntime (serialize : Serializer) (runtimePath : string) =
     serialize { Kind = "fakeRuntime"; Data = runtimePath }

  let highlighting (serialize: Serializer) ranges =
    let map (t: SemanticClassificationType) : string =
      match t with
      | SemanticClassificationType.Operator -> "operator"
      | SemanticClassificationType.ReferenceType -> "type"
      | SemanticClassificationType.ValueType -> "struct"
      | SemanticClassificationType.UnionCase -> "enumMember"
      | SemanticClassificationType.Function -> "function"
      | SemanticClassificationType.Property -> "property"
      | SemanticClassificationType.MutableVar -> "mutable"
      | SemanticClassificationType.Module -> "namespace"
      | SemanticClassificationType.Printf -> "printf"
      | SemanticClassificationType.ComputationExpression -> "macro"
      | SemanticClassificationType.IntrinsicFunction -> "function"
      | SemanticClassificationType.Enumeration -> "enum"
      | SemanticClassificationType.Interface -> "interface"
      | SemanticClassificationType.TypeArgument -> "typeParameter"
      | SemanticClassificationType.Disposable -> "disposable"


    serialize {
      Kind = "highlighting"
      Data = {
        Highlights =
          ranges |> Array.map (fun struct (r, tk) ->
            {Range = r; TokenType = map tk }
          )
      }
    }
