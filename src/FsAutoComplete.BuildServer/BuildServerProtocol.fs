namespace FsAutoComplete.BuildServer

open System
open System.IO
open Newtonsoft.Json
open Newtonsoft.Json.Linq

/// Build Server Protocol types based on https://build-server-protocol.github.io/docs/specification.html
module BuildServerProtocol =

  /// Base types for BSP
  type BuildClientCapabilities = 
    { LanguageIds: string[] }

  type BuildServerCapabilities = 
    { CompileProvider: bool option
      TestProvider: bool option
      RunProvider: bool option
      DebugProvider: bool option
      InverseSourcesProvider: bool option
      DependencySourcesProvider: bool option
      DependencyModulesProvider: bool option
      ResourcesProvider: bool option
      OutputPathsProvider: bool option
      BuildTargetChangedProvider: bool option
      JvmRunEnvironmentProvider: bool option
      JvmTestEnvironmentProvider: bool option
      CanReload: bool option }

  /// Workspace/project discovery and loading

  type WorkspacePeekRequest = 
    { Directory: string
      Deep: int
      ExcludedDirs: string[] }

  type WorkspacePeekResponse = 
    { Found: WorkspaceProjectState[] }

  and WorkspaceProjectState = 
    { Project: ProjectDescription
      Crosswalk: Crosswalk[] option
      Sdk: ProjectSdkInfo option }

  and ProjectDescription = 
    { Project: string
      Name: string
      Virtual: bool option
      Dependencies: string[] option }

  and Crosswalk = 
    { MSBuildProject: string
      ProjectFile: string }

  and ProjectSdkInfo = 
    { Type: string
      Path: string option }

  type WorkspaceLoadRequest = 
    { TextDocuments: string[] }

  type WorkspaceLoadResponse = 
    { WorkspaceRoot: string
      Projects: ProjectDetails[] }

  and ProjectDetails = 
    { Project: string
      Name: string
      SourceFiles: string[]
      ProjectReferences: string[]
      PackageReferences: PackageReference[]
      FrameworkVersion: string
      TargetFramework: string
      OutputType: string
      OutputFile: string
      IsTestProject: bool option
      Properties: Map<string, string> option }

  and PackageReference = 
    { Name: string
      Version: string
      FullPath: string option }

  /// Build target related types

  type BuildTargetIdentifier = 
    { Uri: string }

  type BuildTarget = 
    { Id: BuildTargetIdentifier
      DisplayName: string option
      BaseDirectory: string option
      Tags: string[]
      Capabilities: BuildTargetCapabilities
      LanguageIds: string[]
      Dependencies: BuildTargetIdentifier[]
      DataKind: string option
      Data: JObject option }

  and BuildTargetCapabilities = 
    { CanCompile: bool
      CanTest: bool
      CanRun: bool
      CanDebug: bool }

  /// Build/compile related types

  type CompileParams = 
    { Targets: BuildTargetIdentifier[]
      OriginId: string option
      Arguments: string[] option }

  type CompileResult = 
    { OriginId: string option
      StatusCode: int
      DataKind: string option
      Data: JObject option }

  /// Diagnostics and notifications

  type Diagnostic = 
    { Range: Range
      Severity: DiagnosticSeverity option
      Code: string option
      CodeDescription: CodeDescription option
      Source: string option
      Message: string
      Tags: DiagnosticTag[] option
      RelatedInformation: DiagnosticRelatedInformation[] option
      Data: JObject option }

  and Range = 
    { Start: Position
      End: Position }

  and Position = 
    { Line: int
      Character: int }

  and DiagnosticSeverity = Error = 1 | Warning = 2 | Information = 3 | Hint = 4

  and CodeDescription = 
    { Href: string }

  and DiagnosticTag = Unnecessary = 1 | Deprecated = 2

  and DiagnosticRelatedInformation = 
    { Location: Location
      Message: string }

  and Location = 
    { Uri: string
      Range: Range }

  type PublishDiagnosticsParams = 
    { TextDocument: TextDocumentIdentifier
      BuildTarget: BuildTargetIdentifier
      OriginId: string option
      Diagnostics: Diagnostic[]
      Reset: bool }

  and TextDocumentIdentifier = 
    { Uri: string }

  /// Custom FSAC extensions for F# specific functionality

  type FSharpWorkspacePeekRequest = WorkspacePeekRequest

  type FSharpWorkspaceLoadRequest = 
    { TextDocuments: string[]
      ExcludeProjectDirectories: string[] option }

  type FSharpProjectRequest = 
    { Project: string }

  type FSharpProjectResponse = 
    { Project: ProjectDetails }

  /// JSON RPC message types

  type JsonRpcRequest = 
    { Id: JToken
      Method: string
      Params: JToken option }

  type JsonRpcResponse = 
    { Id: JToken option
      Result: JToken option
      Error: JsonRpcError option }

  and JsonRpcError = 
    { Code: int
      Message: string
      Data: JToken option }

  type JsonRpcNotification = 
    { Method: string
      Params: JToken option }