module FsAutoComplete.LspHelpers

open System
open System.IO
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete.Utils
open FSharp.Reflection
open System.Collections.Generic
open Ionide.ProjInfo.ProjectSystem
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.EditorServices

type FcsRange = FSharp.Compiler.Text.Range
module FcsRange = FSharp.Compiler.Text.Range
type FcsPos = FSharp.Compiler.Text.Position
module FcsPos = FSharp.Compiler.Text.Position

module Lsp = Ionide.LanguageServerProtocol.Types

module FcsPos =
  val subtractColumn: pos: FcsPos -> column: int -> FcsPos

[<AutoOpen>]
module Conversions =
  module Lsp = Ionide.LanguageServerProtocol.Types
  /// convert an LSP position to a compiler position
  val protocolPosToPos: pos: Lsp.Position -> FcsPos
  val protocolPosToRange: pos: Lsp.Position -> Range
  /// convert a compiler position to an LSP position
  val fcsPosToLsp: pos: FcsPos -> Position
  /// convert a compiler range to an LSP range
  val fcsRangeToLsp: range: FcsRange -> Range
  val protocolRangeToRange: fn: string -> range: Lsp.Range -> FcsRange
  /// convert an FCS position to a single-character range in LSP
  val fcsPosToProtocolRange: pos: FcsPos -> Range
  val fcsRangeToLspLocation: range: FcsRange -> Location
  val findDeclToLspLocation: decl: FsAutoComplete.FindDeclarationResult -> Location

  type TextDocumentIdentifier with

    member GetFilePath: unit -> string

  type VersionedTextDocumentIdentifier with

    member GetFilePath: unit -> string

  type TextDocumentItem with

    member GetFilePath: unit -> string

  type ITextDocumentPositionParams with

    member GetFilePath: unit -> string
    member GetFcsPos: unit -> FcsPos

  val fcsSeverityToDiagnostic: (FSharpDiagnosticSeverity -> DiagnosticSeverity option)
  val urlForCompilerCode: number: int -> string
  val fcsErrorToDiagnostic: error: FSharpDiagnostic -> Diagnostic

  val getWorkspaceSymbols:
    uri: DocumentUri ->
    glyphToSymbolKind: (FSharpGlyph -> SymbolKind option) ->
    topLevel: NavigationTopLevelDeclaration ->
    symbolFilter: (WorkspaceSymbol -> bool) ->
      WorkspaceSymbol array

  val getDocumentSymbol:
    glyphToSymbolKind: (FSharpGlyph -> SymbolKind option) ->
    topLevelItem: NavigationTopLevelDeclaration ->
      DocumentSymbol

  val applyQuery: query: string -> info: WorkspaceSymbol -> bool

  val getCodeLensInformation:
    uri: DocumentUri -> typ: string -> topLevel: NavigationTopLevelDeclaration -> CodeLens array

  val getLine: lines: string[] -> pos: Lsp.Position -> string
  val getText: lines: string[] -> r: Lsp.Range -> string

[<AutoOpen>]
module internal GlyphConversions =
  val internal glyphToKindGenerator:
    clientCapabilities: ClientCapabilities option ->
    setFromCapabilities: (ClientCapabilities -> 'kind[] option) ->
    defaultSet: 'kind[] ->
    getUncached: (FSharpGlyph -> 'kind[]) ->
      (FSharpGlyph -> 'kind option)
      when 'kind: equality

  type CompletionItemKind = Ionide.LanguageServerProtocol.Types.CompletionItemKind

  /// Compute the best possible CompletionItemKind for each FSharpGlyph according
  /// to the client capabilities
  val glyphToCompletionKindGenerator:
    clientCapabilities: ClientCapabilities option -> (FSharpGlyph -> CompletionItemKind option)

  /// Compute the best possible SymbolKind for each FSharpGlyph according
  /// to the client capabilities
  val glyphToSymbolKindGenerator: clientCapabilities: ClientCapabilities option -> (FSharpGlyph -> SymbolKind option)

module Workspace =
  open Ionide.ProjInfo.ProjectSystem.WorkspacePeek
  open FsAutoComplete.CommandResponse

  val mapInteresting: i: Interesting -> WorkspacePeekFound
  val getProjectsFromWorkspacePeek: loadedWorkspace: WorkspacePeekFound -> string list

  val foldFsproj:
    item: WorkspacePeekFoundSolutionItem -> (string * WorkspacePeekFoundSolutionItemKindMsbuildFormat) list

  val countProjectsInSln: sln: WorkspacePeekFoundSolution -> int

module SignatureData =
  val formatSignature: typ: string -> parms: ('a * string) list list -> string

module Structure =
  /// convert structure scopes to known kinds of folding range.
  /// this lets commands like 'fold all comments' work sensibly.
  /// impl note: implemented as an exhaustive match here so that
  /// if new structure kinds appear we have to handle them.
  val scopeToKind: scope: Structure.Scope -> string option
  val toFoldingRange: item: Structure.ScopeRange -> FoldingRange

module ClassificationUtils =
  [<RequireQualifiedAccess>]
  type SemanticTokenTypes =
    | Namespace = 0
    /// Represents a generic type. Acts as a fallback for types which
    /// can't be mapped to a specific type like class or enum.
    | Type = 1
    | Class = 2
    | Enum = 3
    | Interface = 4
    | Struct = 5
    | TypeParameter = 6
    | Parameter = 7
    | Variable = 8
    | Property = 9
    | EnumMember = 10
    | Event = 11
    | Function = 12
    | Method = 13
    | Macro = 14
    | Keyword = 15
    | Modifier = 16
    | Comment = 17
    | String = 18
    | Number = 19
    | Regexp = 20
    | Operator = 21
    | Member = 22
    /// computation expressions
    | Cexpr = 23
    | Text = 24
    | Module = 25

  [<RequireQualifiedAccess; Flags>]
  type SemanticTokenModifier =
    | Declaration = 0b1
    | Definition = 0b10
    | Readonly = 0b100
    | Static = 0b1000
    | Deprecated = 0b1_0000
    | Abstract = 0b10_0000
    | Async = 0b100_0000
    | Modification = 0b1000_0000
    | Documentation = 0b1_0000_0000
    | DefaultLibrary = 0b10_0000_0000
    | Mutable = 0b100_0000_0000
    | Disposable = 0b1000_0000_0000

  val map: t: SemanticClassificationType -> SemanticTokenTypes * SemanticTokenModifier list

type PlainNotification = { Content: string }

/// Notification when a `TextDocument` is completely analyzed:
/// F# Compiler checked file & all Analyzers (like `UnusedOpensAnalyzer`) are done.
///
/// Used to signal all Diagnostics for this `TextDocument` are collected and sent.
/// -> For tests to get all Diagnostics of `TextDocument`
type DocumentAnalyzedNotification =
  { TextDocument: VersionedTextDocumentIdentifier }

type TestDetectedNotification =
  { File: string
    Tests: TestAdapter.TestAdapterEntry<Range> array }

type ProjectParms =
  {
    /// Project file to compile
    Project: TextDocumentIdentifier
  }

type WorkspaceLoadParms =
  {
    /// Project files to load
    TextDocuments: TextDocumentIdentifier[]
  }

type WorkspacePeekRequest =
  { Directory: string
    Deep: int
    ExcludedDirs: string array }

type DocumentationForSymbolRequest = { XmlSig: string; Assembly: string }

type HighlightingRequest =
  { TextDocument: TextDocumentIdentifier }

type LineLensConfig = { Enabled: string; Prefix: string }
type FsdnRequest = { Query: string }
type DotnetNewListRequest = { Query: string }

type DotnetNewRunRequest =
  { Template: string
    Output: string option
    Name: string option }

type DotnetProjectRequest = { Target: string; Reference: string }

type DotnetFileRequest =
  { FsProj: string
    FileVirtualPath: string }

type DotnetFile2Request =
  { FsProj: string
    FileVirtualPath: string
    NewFile: string }

type DotnetRenameFileRequest =
  { FsProj: string
    OldFileVirtualPath: string
    NewFileName: string }

type FSharpLiterateRequest =
  { TextDocument: TextDocumentIdentifier }

type FSharpPipelineHintRequest =
  { TextDocument: TextDocumentIdentifier }

type CodeLensConfigDto =
  { Signature: {| Enabled: bool option |} option
    References: {| Enabled: bool option |} option }

type InlayHintDto =
  { typeAnnotations: bool option
    parameterNames: bool option
    disableLongTooltip: bool option }

type InlineValueDto =
  { Enabled: bool option
    Prefix: string option }

type NotificationsDto =
  { Trace: bool option
    TraceNamespaces: string array option }

type DebugDto =
  { DontCheckRelatedFiles: bool option
    CheckFileDebouncerTimeout: int option
    LogDurationBetweenCheckFiles: bool option
    LogCheckFileDuration: bool option }

type FSACDto =
  {
    /// <summary>The <see cref='F:Microsoft.Extensions.Caching.Memory.MemoryCacheOptions.SizeLimit '/> for typecheck cache. </summary>
    CachedTypeCheckCount: int64 option
    ParallelReferenceResolution: bool option
  }

type FSharpConfigDto =
  { AutomaticWorkspaceInit: bool option
    WorkspaceModePeekDeepLevel: int option
    ExcludeProjectDirectories: string array option
    KeywordsAutocomplete: bool option
    ExternalAutocomplete: bool option
    FullNameExternalAutocomplete: bool option
    Linter: bool option
    LinterConfig: string option
    IndentationSize: int option
    UnionCaseStubGeneration: bool option
    UnionCaseStubGenerationBody: string option
    RecordStubGeneration: bool option
    RecordStubGenerationBody: string option
    InterfaceStubGeneration: bool option
    InterfaceStubGenerationObjectIdentifier: string option
    InterfaceStubGenerationMethodBody: string option
    AddPrivateAccessModifier: bool option
    UnusedOpensAnalyzer: bool option
    UnusedOpensAnalyzerExclusions: string array option
    UnusedDeclarationsAnalyzer: bool option
    UnusedDeclarationsAnalyzerExclusions: string array option
    SimplifyNameAnalyzer: bool option
    SimplifyNameAnalyzerExclusions: string array option
    ResolveNamespaces: bool option
    EnableReferenceCodeLens: bool option
    EnableAnalyzers: bool option
    AnalyzersPath: string array option
    ExcludeAnalyzers: string array option
    IncludeAnalyzers: string array option
    DisableInMemoryProjectReferences: bool option
    LineLens: LineLensConfig option
    UseSdkScripts: bool option
    DotNetRoot: string option
    FSIExtraParameters: string array option
    FSICompilerToolLocations: string array option
    TooltipMode: string option
    GenerateBinlog: bool option
    AbstractClassStubGeneration: bool option
    AbstractClassStubGenerationObjectIdentifier: string option
    AbstractClassStubGenerationMethodBody: string option
    CodeLenses: CodeLensConfigDto option
    PipelineHints: InlineValueDto option
    InlayHints: InlayHintDto option
    Fsac: FSACDto option
    Notifications: NotificationsDto option
    Debug: DebugDto option }

type FSharpConfigRequest = { FSharp: FSharpConfigDto option }

type CodeLensConfig =
  { Signature: {| Enabled: bool |}
    References: {| Enabled: bool |} }

  static member Default: CodeLensConfig

type InlayHintsConfig =
  { typeAnnotations: bool
    parameterNames: bool
    disableLongTooltip: bool }

  static member Default: InlayHintsConfig

type InlineValuesConfig =
  { Enabled: bool option
    Prefix: string option }

  static member Default: InlineValuesConfig

type NotificationsConfig =
  { Trace: bool
    TraceNamespaces: string array }

  static member Default: NotificationsConfig
  static member FromDto: dto: NotificationsDto -> NotificationsConfig
  member AddDto: dto: NotificationsDto -> NotificationsConfig

type FSACConfig =
  {
    /// <summary>The <see cref='F:Microsoft.Extensions.Caching.Memory.MemoryCacheOptions.SizeLimit '/> for typecheck cache. </summary>
    CachedTypeCheckCount: int64
    /// <summary>Whether to use parallel reference resolution in the compiler. See <see href="https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-codeanalysis-fsharpchecker.html#Create">the docs</see> for details.</summary>
    ParallelReferenceResolution: bool
  }

  static member Default: FSACConfig
  static member FromDto: dto: FSACDto -> FSACConfig
  member AddDto: dto: FSACDto -> FSACConfig

type DebugConfig =
  { DontCheckRelatedFiles: bool
    CheckFileDebouncerTimeout: int
    LogDurationBetweenCheckFiles: bool
    LogCheckFileDuration: bool }

  static member Default: DebugConfig

type FSharpConfig =
  { AutomaticWorkspaceInit: bool
    WorkspaceModePeekDeepLevel: int
    ExcludeProjectDirectories: string array
    KeywordsAutocomplete: bool
    ExternalAutocomplete: bool
    FullNameExternalAutocomplete: bool
    Linter: bool
    LinterConfig: string option
    IndentationSize: int
    UnionCaseStubGeneration: bool
    UnionCaseStubGenerationBody: string
    RecordStubGeneration: bool
    RecordStubGenerationBody: string
    AbstractClassStubGeneration: bool
    AbstractClassStubGenerationObjectIdentifier: string
    AbstractClassStubGenerationMethodBody: string
    InterfaceStubGeneration: bool
    InterfaceStubGenerationObjectIdentifier: string
    InterfaceStubGenerationMethodBody: string
    AddPrivateAccessModifier: bool
    UnusedOpensAnalyzer: bool
    UnusedOpensAnalyzerExclusions: System.Text.RegularExpressions.Regex array
    UnusedDeclarationsAnalyzer: bool
    UnusedDeclarationsAnalyzerExclusions: System.Text.RegularExpressions.Regex array
    SimplifyNameAnalyzer: bool
    SimplifyNameAnalyzerExclusions: System.Text.RegularExpressions.Regex array
    ResolveNamespaces: bool
    EnableReferenceCodeLens: bool
    EnableAnalyzers: bool
    AnalyzersPath: string array
    ExcludeAnalyzers: string array
    IncludeAnalyzers: string array
    DisableInMemoryProjectReferences: bool
    LineLens: LineLensConfig
    UseSdkScripts: bool
    DotNetRoot: string
    FSIExtraParameters: string array
    FSICompilerToolLocations: string array
    TooltipMode: string
    GenerateBinlog: bool
    CodeLenses: CodeLensConfig
    InlayHints: InlayHintsConfig
    InlineValues: InlineValuesConfig
    Notifications: NotificationsConfig
    Fsac: FSACConfig
    Debug: DebugConfig }

  static member Default: FSharpConfig
  static member FromDto: dto: FSharpConfigDto -> FSharpConfig
  /// called when a configuration change takes effect, so None-valued members here should revert options
  /// back to their defaults
  member AddDto: dto: FSharpConfigDto -> FSharpConfig
  member ScriptTFM: FSIRefs.TFM

/// generate a TokenLegend from an enum representing the token types and the
/// token modifiers.
///
/// since the token types and modifiers are int-backed names, we follow the
/// following logic to create the backing string arrays for the legends:
///   * iterate the enum values
///   * get the enum name
///   * lowercase the first char because of .net naming conventions
val createTokenLegend<'types, 'modifiers
  when 'types: enum<int>
  and 'types: (new: unit -> 'types)
  and 'types: struct
  and 'types :> Enum
  and 'modifiers: enum<int>
  and 'modifiers: (new: unit -> 'modifiers)
  and 'modifiers: struct
  and 'modifiers :> Enum> : SemanticTokensLegend

/// <summary>
/// Encodes an array of ranges + token types/mods into the LSP SemanticTokens' data format.
/// Each item in our range array is turned into 5 integers:
///   * line number delta relative to the previous entry
///   * start column delta relative to the previous entry
///   * length of the token
///   * token type int
///   * token modifiers encoded as bit flags
/// </summary>
/// <param name="rangesAndHighlights"></param>
/// <returns></returns>
val encodeSemanticHighlightRanges:
  rangesAndHighlights:
    (struct (Ionide.LanguageServerProtocol.Types.Range *
    ClassificationUtils.SemanticTokenTypes *
    ClassificationUtils.SemanticTokenModifier list)) array ->
    uint32 array option

type FSharpInlayHintsRequest =
  { TextDocument: TextDocumentIdentifier
    Range: Range }
