namespace FsAutoComplete

open System
open FSharp.Compiler
open Ionide.ProjInfo.ProjectSystem
open FSharp.UMX
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Xml
open FSharp.Compiler.Symbols

module internal CompletionUtils =
  val getIcon: glyph: FSharpGlyph -> string * string
  val getEnclosingEntityChar: (NavigationEntityKind -> string)

module internal ClassificationUtils =
  val map: t: SemanticClassificationType -> string

module CommandResponse =
  open FSharp.Compiler.Text
  type ResponseMsg<'T> = { Kind: string; Data: 'T }

  type ResponseError<'T> =
    { Code: int
      Message: string
      AdditionalData: 'T }

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

  type ProjectResponseInfoDotnetSdk =
    { IsTestProject: bool
      Configuration: string
      IsPackable: bool
      TargetFramework: string
      TargetFrameworkIdentifier: string
      TargetFrameworkVersion: string
      RestoreSuccess: bool
      TargetFrameworks: string list
      RunCmd: RunCmd option
      IsPublishable: bool option }

  and [<RequireQualifiedAccess>] RunCmd = { Command: string; Arguments: string }
  type ProjectLoadingResponse = { Project: ProjectFilePath }

  type ProjectReference =
    { RelativePath: string
      ProjectFileName: string }

  type PackageReference =
    { Name: string
      Version: string
      FullPath: string }

  type ProjectResponse =
    { Project: ProjectFilePath
      Files: List<SourceFilePath>
      Output: string
      ProjectReferences: List<ProjectReference>
      PackageReferences: List<PackageReference>
      References: List<ProjectFilePath>
      OutputType: ProjectOutputType
      Info: ProjectResponseInfoDotnetSdk
      Items: List<ProjectResponseItem>
      AdditionalInfo: Map<string, string> }

  and [<Struct>] ProjectOutputType =
    | Library
    | Exe
    | Custom of string

  and ProjectResponseItem =
    { Name: string
      FilePath: string
      VirtualPath: string
      Metadata: Map<string, string> }

  type DocumentationDescription =
    { XmlKey: string
      Constructors: string array
      Fields: string array
      Functions: string array
      Interfaces: string array
      Attributes: string array
      DeclaredTypes: string array
      Signature: string
      Comment: string
      FooterLines: string array }

    static member CreateFromError: error: string -> DocumentationDescription

  type CompilerLocationResponse =
    { Fsc: string option
      Fsi: string option
      MSBuild: string option
      SdkRoot: string option }

  type FSharpErrorInfo =
    { FileName: string
      StartLine: int
      EndLine: int
      StartColumn: int
      EndColumn: int
      Severity: FSharpDiagnosticSeverity
      Message: string
      Subcategory: string }

    static member IsIgnored: e: FSharpDiagnostic -> bool
    static member OfFSharpError: e: FSharpDiagnostic -> FSharpErrorInfo

  type Declaration =
    { UniqueName: string
      Name: string
      Glyph: string
      GlyphChar: string
      IsTopLevel: bool
      Range: Range
      BodyRange: Range
      File: string
      EnclosingEntity: string
      IsAbstract: bool }

    static member OfDeclarationItem: e: NavigationItem * fn: string -> Declaration

  type DeclarationResponse =
    { Declaration: Declaration
      Nested: Declaration[] }

  type Parameter = { Name: string; Type: string }

  type SignatureData =
    { OutputType: string
      Parameters: Parameter list list
      Generics: string list }

  type FsprojPeek = { Path: string; CompileItems: string list }

  type WorkspacePeekResponse = { Found: WorkspacePeekFound list }

  and WorkspacePeekFound =
    | Directory of WorkspacePeekFoundDirectory
    | Solution of WorkspacePeekFoundSolution

  and WorkspacePeekFoundDirectory =
    { Directory: string
      Fsprojs: FsprojPeek list }

  and WorkspacePeekFoundSolution =
    { Path: string
      Items: WorkspacePeekFoundSolutionItem list
      Configurations: WorkspacePeekFoundSolutionConfiguration list }

  and [<RequireQualifiedAccess>] WorkspacePeekFoundSolutionItem =
    { Guid: Guid
      Name: string
      Kind: WorkspacePeekFoundSolutionItemKind }

  and WorkspacePeekFoundSolutionItemKind =
    | MsbuildFormat of WorkspacePeekFoundSolutionItemKindMsbuildFormat
    | Folder of WorkspacePeekFoundSolutionItemKindFolder

  and [<RequireQualifiedAccess>] WorkspacePeekFoundSolutionItemKindMsbuildFormat =
    { Configurations: WorkspacePeekFoundSolutionConfiguration list }

  and [<RequireQualifiedAccess>] WorkspacePeekFoundSolutionItemKindFolder =
    { Items: WorkspacePeekFoundSolutionItem list
      Files: FilePath list }

  and [<RequireQualifiedAccess>] WorkspacePeekFoundSolutionConfiguration =
    { Id: string
      ConfigurationName: string
      PlatformName: string }

  type WorkspaceLoadResponse = { Status: string }

  type CompileResponse =
    { Code: int; Errors: FSharpErrorInfo[] }

  type FsdnResponse = { Functions: string list }

  type DotnetNewListResponse =
    { Installed: DotnetNewTemplate.Template list }

  type DotnetNewGetDetailsResponse =
    { Detailed: DotnetNewTemplate.DetailedTemplate }

  type DotnetNewCreateCliResponse =
    { CommandName: string
      ParameterStr: string }

  type HighlightingRange =
    { Range: Ionide.LanguageServerProtocol.Types.Range
      TokenType: string }

  type HighlightingResponse = { Highlights: HighlightingRange[] }

  type PipelineHint =
    { Line: int
      Types: string[]
      PrecedingNonPipeExprLine: int option }

  val project: serialize: Serializer -> projectResult: ProjectResult -> string
  val projectError: serialize: Serializer -> errorDetails: Ionide.ProjInfo.Types.GetProjectOptionsErrors -> string
  val projectLoading: serialize: Serializer -> projectFileName: ProjectFilePath -> string
  val workspacePeek: serialize: Serializer -> found: WorkspacePeek.Interesting list -> string
  val workspaceLoad: serialize: Serializer -> finished: bool -> string
  val projectLoad: serialize: Serializer -> finished: bool -> string
  val projectChanged: serialize: Serializer -> projectChanged: ProjectFilePath -> string
  val signatureData: serialize: Serializer -> string * (string * string) list list * string list -> string
  val help: serialize: Serializer -> data: string -> string
  val fsdn: serialize: Serializer -> functions: string list -> string
  val dotnetnewlist: serialize: Serializer -> installedTemplate: DotnetNewTemplate.Template list -> string
  val dotnetnewgetDetails: serialize: Serializer -> detailedTemplate: DotnetNewTemplate.DetailedTemplate -> string
  val dotnetnewCreateCli: serialize: Serializer -> commandName: string * parameterStr: string -> string
  val declarations: serialize: Serializer -> decls: (NavigationTopLevelDeclaration * string<LocalPath>)[] -> string

  val formattedDocumentation:
    serialize: Serializer ->
    param:
      {| Tip: ToolTipText option
         XmlSig: (string * string) option
         Signature: (string * DocumentationFormatter.EntityInfo)
         Footer: string
         XmlKey: string |} ->
      string

  val formattedDocumentationForSymbol:
    serialize: Serializer ->
    param:
      {| Xml: string
         Assembly: string
         XmlDoc: FSharpXmlDoc
         Signature: (string * DocumentationFormatter.EntityInfo)
         Footer: string
         XmlKey: string |} ->
      string

  val typeSig: serialize: Serializer -> tip: ToolTipText -> string

  val compilerLocation:
    serialize: Serializer ->
    fsc: string option ->
    fsi: string option ->
    msbuild: string option ->
    sdkRoot: string option ->
      string

  val compile: serialize: Serializer -> errors: #FSharpDiagnostic array * code: int -> string
  val fsharpLiterate: serialize: Serializer -> content: string -> string
  val pipelineHint: serialize: Serializer -> content: (int * int option * string list)[] -> string
