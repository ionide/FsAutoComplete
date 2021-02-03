module FsAutoComplete.LspHelpers

open System
open System.IO
open LanguageServerProtocol.Types
open FsAutoComplete.Utils
open FSharp.Compiler.SourceCodeServices
open FSharp.Reflection
open System.Collections.Generic
open Ionide.ProjInfo.ProjectSystem

module FcsRange = FSharp.Compiler.Range

[<AutoOpen>]
module Conversions =
    module Lsp = LanguageServerProtocol.Types

    /// convert an LSP position to a compiler position
    let protocolPosToPos (pos: Lsp.Position): FcsRange.pos =
        FcsRange.mkPos (pos.Line + 1) (pos.Character + 1)

    /// convert a compiler position to an LSP position
    let fcsPosToLsp (pos: FcsRange.pos): Lsp.Position =
        { Line = pos.Line - 1; Character = pos.Column }

    /// convert a compiler range to an LSP range
    let fcsRangeToLsp(range: FcsRange.range): Lsp.Range =
        {
            Start = fcsPosToLsp range.Start
            End = fcsPosToLsp range.End
        }

    let protocolRangeToRange fn (range: Lsp.Range): FcsRange.range =
        FcsRange.mkRange fn (protocolPosToPos range.Start) (protocolPosToPos range.End)

    /// convert an FCS position to a single-character range in LSP
    let fcsPosToProtocolRange (pos: FcsRange.pos): Lsp.Range =
      {
        Start = fcsPosToLsp pos
        End = fcsPosToLsp pos
      }


    let symbolUseRangeToLsp (range: SymbolCache.SymbolUseRange): Lsp.Range =
        {
            Start = { Line = range.StartLine - 1; Character = range.StartColumn - 1 }
            End = { Line = range.EndLine - 1; Character = range.EndColumn - 1 }
        }

    let fcsRangeToLspLocation(range: FSharp.Compiler.Range.range): Lsp.Location =
        let fileUri = Path.FilePathToUri range.FileName
        let lspRange = fcsRangeToLsp range
        {
            Uri = fileUri
            Range = lspRange
        }

    let symbolUseRangeToLspLocation (range: SymbolCache.SymbolUseRange): Lsp.Location =
        let fileUri = Path.FilePathToUri range.FileName
        let lspRange = symbolUseRangeToLsp range
        {
            Uri = fileUri
            Range = lspRange
        }

    let findDeclToLspLocation(decl: FsAutoComplete.FindDeclarationResult): Lsp.Location =
        match decl with
        | FsAutoComplete.FindDeclarationResult.ExternalDeclaration ex ->
            let fileUri = Path.FilePathToUri ex.File
            {
                Uri = fileUri
                Range = {
                    Start = { Line = ex.Line - 1; Character = ex.Column - 1 }
                    End = { Line = ex.Line - 1; Character = ex.Column - 1 }
                }
            }
        | FsAutoComplete.FindDeclarationResult.Range r -> fcsRangeToLspLocation r
        | FsAutoComplete.FindDeclarationResult.File file ->
            let fileUri = Path.FilePathToUri file
            {
                Uri = fileUri
                Range = {
                    Start = { Line = 0; Character = 0 }
                    End = { Line = 0; Character = 0 }
                }
            }

    type TextDocumentIdentifier with
        member doc.GetFilePath() = Path.FileUriToLocalPath doc.Uri

    type VersionedTextDocumentIdentifier with
        member doc.GetFilePath() = Path.FileUriToLocalPath doc.Uri

    type TextDocumentItem with
        member doc.GetFilePath() = Path.FileUriToLocalPath doc.Uri

    type ITextDocumentPositionParams with
        member p.GetFilePath() = p.TextDocument.GetFilePath()
        member p.GetFcsPos() = protocolPosToPos p.Position

    let fcsSeverityToDiagnostic = function
        | FSharpErrorSeverity.Error -> DiagnosticSeverity.Error
        | FSharpErrorSeverity.Warning -> DiagnosticSeverity.Warning

    let fcsErrorToDiagnostic (error: FSharpErrorInfo) =
        {
            Range =
                {
                    Start = { Line = error.StartLineAlternate - 1; Character = error.StartColumn }
                    End = { Line = error.EndLineAlternate - 1; Character = error.EndColumn }
                }
            Severity = Some (fcsSeverityToDiagnostic error.Severity)
            Source = "F# Compiler"
            Message = error.Message
            Code = Some (string error.ErrorNumber)
            RelatedInformation = Some [||]
            Tags = None
        }

    let getSymbolInformations (uri: DocumentUri) (glyphToSymbolKind: FSharpGlyph -> SymbolKind option) (topLevel: FSharpNavigationTopLevelDeclaration) (symbolFilter: SymbolInformation -> bool): SymbolInformation [] =
        let inner (container: string option) (decl: FSharpNavigationDeclarationItem): SymbolInformation option =
            // We should nearly always have a kind, if the client doesn't send weird capabilities,
            // if we don't why not assume module...
            let kind = defaultArg (glyphToSymbolKind decl.Glyph) SymbolKind.Module
            let location = { Uri = uri; Range = fcsRangeToLsp decl.Range }
            let sym =
              {
                  SymbolInformation.Name = decl.Name
                  Kind = kind
                  Location = location
                  ContainerName = container
              }
            if symbolFilter sym then Some sym else None
        [|
          yield! inner None topLevel.Declaration |> Option.toArray
          yield! topLevel.Nested |> Array.choose (inner (Some topLevel.Declaration.Name))
        |]

    let applyQuery (query: string) (info: SymbolInformation) =
      match query.Split([| '.' |], StringSplitOptions.RemoveEmptyEntries) with
      | [|  |] -> false
      | [| fullName |] -> info.Name.StartsWith fullName
      | [| moduleName; fieldName |] ->
        info.Name.StartsWith fieldName && info.ContainerName = Some moduleName
      | parts ->
        let containerName =
          parts.[0..(parts.Length - 2)] |> String.concat "."
        let fieldName =
          Array.last parts
        info.Name.StartsWith fieldName && info.ContainerName = Some containerName

    let getCodeLensInformation (uri: DocumentUri) (typ: string) (topLevel: FSharpNavigationTopLevelDeclaration): CodeLens [] =
        let map (decl: FSharpNavigationDeclarationItem): CodeLens =
            {
                Command = None
                Data = Some (Newtonsoft.Json.Linq.JToken.FromObject [|uri; typ |] )
                Range = fcsRangeToLsp decl.Range
            }
        topLevel.Nested
        |> Array.filter(fun n ->
            not (n.Glyph <> FSharpGlyph.Method
              && n.Glyph <> FSharpGlyph.OverridenMethod
              && n.Glyph <> FSharpGlyph.ExtensionMethod
              && n.Glyph <> FSharpGlyph.Field
              && n.Glyph <> FSharpGlyph.EnumMember
              && n.Glyph <> FSharpGlyph.Property
              || n.IsAbstract
              || n.EnclosingEntityKind = FSharpEnclosingEntityKind.Interface
              || n.EnclosingEntityKind = FSharpEnclosingEntityKind.Record
              || n.EnclosingEntityKind = FSharpEnclosingEntityKind.DU
              || n.EnclosingEntityKind = FSharpEnclosingEntityKind.Enum
              || n.EnclosingEntityKind = FSharpEnclosingEntityKind.Exception)
        )
        |> Array.map map

    let getLine (lines: string[]) (pos: Lsp.Position) =
      lines.[pos.Line]

    let getText (lines: string []) (r: Lsp.Range) =
        lines.[r.Start.Line].Substring(r.Start.Character, r.End.Character - r.Start.Character)

[<AutoOpen>]
module internal GlyphConversions =
    let internal glyphToKindGenerator<'kind when 'kind : equality>
        (clientCapabilities: ClientCapabilities option)
        (setFromCapabilities: ClientCapabilities -> 'kind [] option)
        (defaultSet: 'kind [])
        (getUncached: FSharpGlyph -> 'kind[]) =

        let completionItemSet = clientCapabilities |> Option.bind(setFromCapabilities)
        let completionItemSet = defaultArg completionItemSet defaultSet

        let bestAvailable (possible: 'kind[]) =
            possible
            |> Array.tryFind (fun x -> Array.contains x completionItemSet)

        let unionCases = FSharpType.GetUnionCases(typeof<FSharpGlyph>)
        let cache = Dictionary<FSharpGlyph, 'kind option>(unionCases.Length)
        for info in unionCases do
            let glyph = FSharpValue.MakeUnion(info, [||]) :?> FSharpGlyph
            let completionItem = getUncached glyph |> bestAvailable
            cache.Add(glyph, completionItem)

        fun glyph ->
            cache.[glyph]

    type CompletionItemKind = LanguageServerProtocol.Types.CompletionItemKind

    /// Compute the best possible CompletionItemKind for each FSharpGlyph according
    /// to the client capabilities
    let glyphToCompletionKindGenerator (clientCapabilities: ClientCapabilities option) =
        glyphToKindGenerator
            clientCapabilities
            (fun clientCapabilities ->
                clientCapabilities.TextDocument
                |> Option.bind(fun x -> x.Completion)
                |> Option.bind(fun x -> x.CompletionItemKind)
                |> Option.bind(fun x -> x.ValueSet))
            CompletionItemKindCapabilities.DefaultValueSet
            (fun code ->
                match code with
                | FSharpGlyph.Class -> [| CompletionItemKind.Class |]
                | FSharpGlyph.Constant -> [| CompletionItemKind.Constant |]
                | FSharpGlyph.Delegate -> [| CompletionItemKind.Function |]
                | FSharpGlyph.Enum -> [| CompletionItemKind.Enum |]
                | FSharpGlyph.EnumMember -> [| CompletionItemKind.EnumMember; CompletionItemKind.Enum |]
                | FSharpGlyph.Event -> [| CompletionItemKind.Event |]
                | FSharpGlyph.Exception -> [| CompletionItemKind.Class |]
                | FSharpGlyph.Field -> [| CompletionItemKind.Field |]
                | FSharpGlyph.Interface -> [| CompletionItemKind.Interface; CompletionItemKind.Class |]
                | FSharpGlyph.Method -> [| CompletionItemKind.Method |]
                | FSharpGlyph.OverridenMethod-> [| CompletionItemKind.Method |]
                | FSharpGlyph.Module -> [| CompletionItemKind.Module; CompletionItemKind.Class |]
                | FSharpGlyph.NameSpace -> [| CompletionItemKind.Module |]
                | FSharpGlyph.Property -> [| CompletionItemKind.Property |]
                | FSharpGlyph.Struct -> [| CompletionItemKind.Struct; CompletionItemKind.Class |]
                | FSharpGlyph.Typedef -> [| CompletionItemKind.Class |]
                | FSharpGlyph.Type -> [| CompletionItemKind.Class |]
                | FSharpGlyph.Union -> [| CompletionItemKind.Class |]
                | FSharpGlyph.Variable -> [| CompletionItemKind.Variable |]
                | FSharpGlyph.ExtensionMethod -> [| CompletionItemKind.Method |]
                | FSharpGlyph.Error
                | _ -> [||])

    /// Compute the best possible SymbolKind for each FSharpGlyph according
    /// to the client capabilities
    let glyphToSymbolKindGenerator (clientCapabilities: ClientCapabilities option) =
        glyphToKindGenerator
            clientCapabilities
            (fun clientCapabilities ->
                clientCapabilities.TextDocument
                |> Option.bind(fun x -> x.DocumentSymbol)
                |> Option.bind(fun x -> x.SymbolKind)
                |> Option.bind(fun x -> x.ValueSet))
            SymbolKindCapabilities.DefaultValueSet
            (fun code ->
                match code with
                | FSharpGlyph.Class -> [| SymbolKind.Class |]
                | FSharpGlyph.Constant -> [| SymbolKind.Constant |]
                | FSharpGlyph.Delegate -> [| SymbolKind.Function |]
                | FSharpGlyph.Enum -> [| SymbolKind.Enum |]
                | FSharpGlyph.EnumMember -> [| SymbolKind.EnumMember; SymbolKind.Enum |]
                | FSharpGlyph.Event -> [| SymbolKind.Event |]
                | FSharpGlyph.Exception -> [| SymbolKind.Class |]
                | FSharpGlyph.Field -> [| SymbolKind.Field |]
                | FSharpGlyph.Interface -> [| SymbolKind.Interface; SymbolKind.Class |]
                | FSharpGlyph.Method -> [| SymbolKind.Method |]
                | FSharpGlyph.OverridenMethod-> [| SymbolKind.Method |]
                | FSharpGlyph.Module -> [| SymbolKind.Module; SymbolKind.Class |]
                | FSharpGlyph.NameSpace -> [| SymbolKind.Module |]
                | FSharpGlyph.Property -> [| SymbolKind.Property |]
                | FSharpGlyph.Struct -> [| SymbolKind.Struct; SymbolKind.Class |]
                | FSharpGlyph.Typedef -> [| SymbolKind.Class |]
                | FSharpGlyph.Type -> [| SymbolKind.Class |]
                | FSharpGlyph.Union -> [| SymbolKind.Class |]
                | FSharpGlyph.Variable -> [| SymbolKind.Variable |]
                | FSharpGlyph.ExtensionMethod -> [| SymbolKind.Method |]
                | FSharpGlyph.Error
                | _ -> [||])

module Workspace =
    open Ionide.ProjInfo.ProjectSystem.WorkspacePeek
    open FsAutoComplete.CommandResponse

    let mapInteresting i =
        match i with
        | Interesting.Directory (p, fsprojs) ->
            WorkspacePeekFound.Directory { WorkspacePeekFoundDirectory.Directory = p; Fsprojs = fsprojs }
        | Interesting.Solution (p, sd) ->
            let rec item (x: Ionide.ProjInfo.InspectSln.SolutionItem) =
                let kind =
                    match x.Kind with
                    | Ionide.ProjInfo.InspectSln.SolutionItemKind.Unknown
                    | Ionide.ProjInfo.InspectSln.SolutionItemKind.Unsupported ->
                        None
                    | Ionide.ProjInfo.InspectSln.SolutionItemKind.MsbuildFormat msbuildProj ->
                        Some (WorkspacePeekFoundSolutionItemKind.MsbuildFormat {
                            WorkspacePeekFoundSolutionItemKindMsbuildFormat.Configurations = []
                        })
                    | Ionide.ProjInfo.InspectSln.SolutionItemKind.Folder(children, files) ->
                        let c = children |> List.choose item
                        Some (WorkspacePeekFoundSolutionItemKind.Folder {
                            WorkspacePeekFoundSolutionItemKindFolder.Items = c
                            Files = files
                        })
                kind
                |> Option.map (fun k -> { WorkspacePeekFoundSolutionItem.Guid = x.Guid; Name = x.Name; Kind = k })
            let items = sd.Items |> List.choose item
            WorkspacePeekFound.Solution { WorkspacePeekFoundSolution.Path = p; Items = items; Configurations = [] }

    let getProjectsFromWorkspacePeek loadedWorkspace =
        match loadedWorkspace with
        | WorkspacePeekFound.Solution sln ->
            let rec getProjs (item : WorkspacePeekFoundSolutionItem) =
                match item.Kind with
                | MsbuildFormat _proj ->
                    [ item.Name ]
                | Folder folder ->
                    folder.Items |> List.collect getProjs
            sln.Items
            |> List.collect getProjs
        | WorkspacePeekFound.Directory dir ->
            dir.Fsprojs

    let rec foldFsproj (item : WorkspacePeekFoundSolutionItem) =
        match item.Kind with
        | WorkspacePeekFoundSolutionItemKind.Folder folder ->
            folder.Items |> List.collect foldFsproj
        | WorkspacePeekFoundSolutionItemKind.MsbuildFormat msbuild ->
            [ item.Name, msbuild ]

    let countProjectsInSln (sln : WorkspacePeekFoundSolution) =
        sln.Items |> List.map foldFsproj |> List.sumBy List.length

module SigantureData =
    let formatSignature typ parms : string =
        let formatType =
            function
            | Contains "->" t -> sprintf "(%s)" t
            | t -> t

        let args =
            parms
            |> List.map (fun group ->
                group
                |> List.map (fun (n,t) -> formatType t)
                |> String.concat " * "
            )
            |> String.concat " -> "

        if String.IsNullOrEmpty args then typ else args + " -> " + formatType typ

module Structure =
      /// convert structure scopes to known kinds of folding range.
    /// this lets commands like 'fold all comments' work sensibly.
    /// impl note: implemented as an exhaustive match here so that
    /// if new structure kinds appear we have to handle them.
    let scopeToKind (scope: Structure.Scope): string option =
        match scope with
        | Structure.Scope.Open -> Some FoldingRangeKind.Imports
        | Structure.Scope.Comment
        | Structure.Scope.XmlDocComment -> Some FoldingRangeKind.Comment
        | Structure.Scope.Namespace
        | Structure.Scope.Module
        | Structure.Scope.Type
        | Structure.Scope.Member
        | Structure.Scope.LetOrUse
        | Structure.Scope.Val
        | Structure.Scope.CompExpr
        | Structure.Scope.IfThenElse
        | Structure.Scope.ThenInIfThenElse
        | Structure.Scope.ElseInIfThenElse
        | Structure.Scope.TryWith
        | Structure.Scope.TryInTryWith
        | Structure.Scope.WithInTryWith
        | Structure.Scope.TryFinally
        | Structure.Scope.TryInTryFinally
        | Structure.Scope.FinallyInTryFinally
        | Structure.Scope.ArrayOrList
        | Structure.Scope.ObjExpr
        | Structure.Scope.For
        | Structure.Scope.While
        | Structure.Scope.Match
        | Structure.Scope.MatchBang
        | Structure.Scope.MatchLambda
        | Structure.Scope.MatchClause
        | Structure.Scope.Lambda
        | Structure.Scope.CompExprInternal
        | Structure.Scope.Quote
        | Structure.Scope.Record
        | Structure.Scope.SpecialFunc
        | Structure.Scope.Do
        | Structure.Scope.New
        | Structure.Scope.Attribute
        | Structure.Scope.Interface
        | Structure.Scope.HashDirective
        | Structure.Scope.LetOrUseBang
        | Structure.Scope.TypeExtension
        | Structure.Scope.YieldOrReturn
        | Structure.Scope.YieldOrReturnBang
        | Structure.Scope.Tuple
        | Structure.Scope.UnionCase
        | Structure.Scope.EnumCase
        | Structure.Scope.RecordField
        | Structure.Scope.RecordDefn
        | Structure.Scope.UnionDefn -> None

    let toFoldingRange (item: Structure.ScopeRange): FoldingRange =
        let kind = scopeToKind item.Scope
        // map the collapserange to the foldingRange
        let lsp = fcsRangeToLsp item.CollapseRange
        { StartCharacter   = Some lsp.Start.Character
          StartLine        = lsp.Start.Line
          EndCharacter     = Some lsp.End.Character
          EndLine          = lsp.End.Line
          Kind             = kind }

module ClassificationUtils =
  [<RequireQualifiedAccess>]
  type SemanticTokenTypes =
  (* implementation note: these indexes map to array indexes *)
  (* LSP-provided types *)

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
  (* our custom token types *)
  | Member = 22
  /// computation expressions
  | Cexpr = 23
  | Text = 24

  [<RequireQualifiedAccess; Flags>]
  type SemanticTokenModifier =
  (* implementation note: these are defined as bitflags to make it easy to calculate them *)
  (* LSP-defined modifiers *)
  | Declaration    =              0b1
  | Definition     =             0b10
  | Readonly       =            0b100
  | Static         =           0b1000
  | Deprecated     =         0b1_0000
  | Abstract       =        0b10_0000
  | Async          =       0b100_0000
  | Modification   =      0b1000_0000
  | Documentation  =    0b1_0000_0000
  | DefaultLibrary =   0b10_0000_0000
  (* custom modifiers *)
  | Mutable        =  0b100_0000_0000
  | Disposable     = 0b1000_0000_0000


  let map (t: SemanticClassificationType) : SemanticTokenTypes * SemanticTokenModifier list =
      match t with
      | SemanticClassificationType.Operator -> SemanticTokenTypes.Operator, []
      | SemanticClassificationType.ReferenceType
      | SemanticClassificationType.Type
      | SemanticClassificationType.TypeDef
      | SemanticClassificationType.ConstructorForReferenceType -> SemanticTokenTypes.Type, []
      | SemanticClassificationType.ValueType
      | SemanticClassificationType.ConstructorForValueType -> SemanticTokenTypes.Struct, []
      | SemanticClassificationType.UnionCase
      | SemanticClassificationType.UnionCaseField -> SemanticTokenTypes.EnumMember, []
      | SemanticClassificationType.Function
      | SemanticClassificationType.Method
      | SemanticClassificationType.ExtensionMethod -> SemanticTokenTypes.Function, []
      | SemanticClassificationType.Property -> SemanticTokenTypes.Property, []
      | SemanticClassificationType.MutableVar
      | SemanticClassificationType.MutableRecordField -> SemanticTokenTypes.Member, [SemanticTokenModifier.Mutable]
      | SemanticClassificationType.Module
      | SemanticClassificationType.Namespace -> SemanticTokenTypes.Namespace, []
      | SemanticClassificationType.Printf -> SemanticTokenTypes.Regexp, []
      | SemanticClassificationType.ComputationExpression -> SemanticTokenTypes.Cexpr, []
      | SemanticClassificationType.IntrinsicFunction -> SemanticTokenTypes.Function, []
      | SemanticClassificationType.Enumeration -> SemanticTokenTypes.Enum, []
      | SemanticClassificationType.Interface -> SemanticTokenTypes.Interface, []
      | SemanticClassificationType.TypeArgument -> SemanticTokenTypes.TypeParameter, []
      | SemanticClassificationType.DisposableTopLevelValue
      | SemanticClassificationType.DisposableLocalValue -> SemanticTokenTypes.Variable, [ SemanticTokenModifier.Disposable ]
      | SemanticClassificationType.DisposableType -> SemanticTokenTypes.Type, [ SemanticTokenModifier.Disposable ]
      | SemanticClassificationType.Literal -> SemanticTokenTypes.Variable, [SemanticTokenModifier.Readonly; SemanticTokenModifier.DefaultLibrary]
      | SemanticClassificationType.RecordField
      | SemanticClassificationType.RecordFieldAsFunction -> SemanticTokenTypes.Property, [SemanticTokenModifier.Readonly]
      | SemanticClassificationType.Exception
      | SemanticClassificationType.Field
      | SemanticClassificationType.Event
      | SemanticClassificationType.Delegate
      | SemanticClassificationType.NamedArgument -> SemanticTokenTypes.Member, []
      | SemanticClassificationType.Value
      | SemanticClassificationType.LocalValue -> SemanticTokenTypes.Variable, []
      | SemanticClassificationType.Plaintext -> SemanticTokenTypes.Text, []

type PlainNotification= { Content: string }

type ProjectParms = {
    /// Project file to compile
    Project: TextDocumentIdentifier
}

type WorkspaceLoadParms = {
    /// Project files to load
    TextDocuments: TextDocumentIdentifier []
}

type WorkspacePeekRequest = {Directory : string; Deep: int; ExcludedDirs: string array}
type DocumentationForSymbolReuqest = {XmlSig: string; Assembly: string}

type FakeTargetsRequest = {FileName : string; FakeContext : FakeSupport.FakeContext; }

type HighlightingRequest = {FileName : string; }

type LineLensConfig = {
    Enabled: string
    Prefix: string
}

type FsdnRequest = { Query: string }

type DotnetNewListRequest = { Query: string }

type DotnetNewRunRequest = { Template: string; Output: string option; Name: string option }

type DotnetProjectRequest = { Target: string; Reference: string }

type DotnetFileRequest = { FsProj: string; FileVirtualPath: string }

type DotnetFile2Request = { FsProj: string; FileVirtualPath: string; NewFile: string }

type FSharpLiterateRequest = {FileName : string; }

type FSharpPipelineHintRequest = {FileName : string; }

type FSharpConfigDto = {
    AutomaticWorkspaceInit: bool option
    WorkspaceModePeekDeepLevel: int option
    ExcludeProjectDirectories: string [] option
    KeywordsAutocomplete: bool option
    ExternalAutocomplete: bool option
    Linter: bool option
    LinterConfig: string option
    UnionCaseStubGeneration: bool option
    UnionCaseStubGenerationBody: string option
    RecordStubGeneration: bool option
    RecordStubGenerationBody: string option
    InterfaceStubGeneration: bool option
    InterfaceStubGenerationObjectIdentifier: string option
    InterfaceStubGenerationMethodBody: string option
    UnusedOpensAnalyzer: bool option
    UnusedDeclarationsAnalyzer: bool option
    SimplifyNameAnalyzer: bool option
    ResolveNamespaces: bool option
    EnableReferenceCodeLens: bool option
    EnableAnalyzers: bool option
    AnalyzersPath: string [] option
    DisableInMemoryProjectReferences: bool option
    LineLens: LineLensConfig option
    UseSdkScripts: bool option
    DotNetRoot: string option
    FSIExtraParameters: string [] option
    FSICompilerToolLocations: string [] option
    TooltipMode : string option
    GenerateBinlog: bool option
    AbstractClassStubGeneration: bool option
    AbstractClassStubGenerationObjectIdentifier: string option
    AbstractClassStubGenerationMethodBody: string option

}

type FSharpConfigRequest = {
    FSharp: FSharpConfigDto
}

type FSharpConfig = {
    AutomaticWorkspaceInit: bool
    WorkspaceModePeekDeepLevel: int
    ExcludeProjectDirectories: string []
    KeywordsAutocomplete: bool
    ExternalAutocomplete: bool
    Linter: bool
    LinterConfig: string option
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
    UnusedOpensAnalyzer: bool
    UnusedDeclarationsAnalyzer: bool
    SimplifyNameAnalyzer: bool
    ResolveNamespaces: bool
    EnableReferenceCodeLens: bool
    EnableAnalyzers: bool
    AnalyzersPath: string []
    DisableInMemoryProjectReferences: bool
    LineLens: LineLensConfig
    UseSdkScripts: bool
    DotNetRoot: string
    FSIExtraParameters: string []
    FSICompilerToolLocations: string []
    TooltipMode : string
    GenerateBinlog: bool
}
with
    static member Default : FSharpConfig =
        {
            AutomaticWorkspaceInit = false
            WorkspaceModePeekDeepLevel = 2
            ExcludeProjectDirectories = [||]
            KeywordsAutocomplete = false
            ExternalAutocomplete = false
            Linter = false
            LinterConfig = None
            UnionCaseStubGeneration = false
            UnionCaseStubGenerationBody = "failwith \"Not Implemented\""
            RecordStubGeneration = false
            RecordStubGenerationBody = "failwith \"Not Implemented\""
            AbstractClassStubGeneration = true
            AbstractClassStubGenerationObjectIdentifier = "this"
            AbstractClassStubGenerationMethodBody = "failwith \"Not Implemented\""
            InterfaceStubGeneration = false
            InterfaceStubGenerationObjectIdentifier = "this"
            InterfaceStubGenerationMethodBody = "failwith \"Not Implemented\""
            UnusedOpensAnalyzer = false
            UnusedDeclarationsAnalyzer = false
            SimplifyNameAnalyzer = false
            ResolveNamespaces = false
            EnableReferenceCodeLens = false
            EnableAnalyzers = false
            AnalyzersPath = [||]
            DisableInMemoryProjectReferences = false
            LineLens = {
                Enabled = "never"
                Prefix =""
            }
            UseSdkScripts = true
            DotNetRoot = Environment.dotnetSDKRoot.Value.FullName
            FSIExtraParameters = [||]
            FSICompilerToolLocations = [||]
            TooltipMode = "full"
            GenerateBinlog = false
        }

    static member FromDto(dto: FSharpConfigDto): FSharpConfig =
        {
            AutomaticWorkspaceInit = defaultArg dto.AutomaticWorkspaceInit false
            WorkspaceModePeekDeepLevel = defaultArg dto.WorkspaceModePeekDeepLevel 2
            ExcludeProjectDirectories = defaultArg dto.ExcludeProjectDirectories [||]
            KeywordsAutocomplete = defaultArg dto.KeywordsAutocomplete false
            ExternalAutocomplete = defaultArg dto.ExternalAutocomplete false
            Linter = defaultArg dto.Linter false
            LinterConfig = dto.LinterConfig
            UnionCaseStubGeneration = defaultArg dto.UnionCaseStubGeneration false
            UnionCaseStubGenerationBody = defaultArg dto.UnionCaseStubGenerationBody "failwith \"Not Implemented\""
            RecordStubGeneration = defaultArg dto.RecordStubGeneration false
            RecordStubGenerationBody = defaultArg dto.RecordStubGenerationBody "failwith \"Not Implemented\""
            InterfaceStubGeneration = defaultArg dto.InterfaceStubGeneration false
            InterfaceStubGenerationObjectIdentifier = defaultArg dto.InterfaceStubGenerationObjectIdentifier "this"
            InterfaceStubGenerationMethodBody = defaultArg dto.InterfaceStubGenerationMethodBody "failwith \"Not Implemented\""
            UnusedOpensAnalyzer = defaultArg dto.UnusedOpensAnalyzer false
            UnusedDeclarationsAnalyzer = defaultArg dto.UnusedDeclarationsAnalyzer false
            SimplifyNameAnalyzer = defaultArg dto.SimplifyNameAnalyzer false
            ResolveNamespaces = defaultArg dto.ResolveNamespaces false
            EnableReferenceCodeLens = defaultArg dto.EnableReferenceCodeLens false
            EnableAnalyzers = defaultArg dto.EnableAnalyzers false
            AnalyzersPath = defaultArg dto.AnalyzersPath [||]
            DisableInMemoryProjectReferences = defaultArg dto.DisableInMemoryProjectReferences false
            LineLens = {
                Enabled = defaultArg (dto.LineLens |> Option.map (fun n -> n.Enabled)) "never"
                Prefix = defaultArg (dto.LineLens |> Option.map (fun n -> n.Prefix)) ""
            }
            UseSdkScripts = defaultArg dto.UseSdkScripts true
            DotNetRoot =
                dto.DotNetRoot
                |> Option.bind (fun s -> if String.IsNullOrEmpty s then None else Some s)
                |> Option.defaultValue Environment.dotnetSDKRoot.Value.FullName
            FSIExtraParameters = defaultArg dto.FSIExtraParameters FSharpConfig.Default.FSIExtraParameters
            FSICompilerToolLocations = defaultArg dto.FSICompilerToolLocations FSharpConfig.Default.FSICompilerToolLocations
            TooltipMode = defaultArg dto.TooltipMode "full"
            GenerateBinlog = defaultArg dto.GenerateBinlog false
            AbstractClassStubGeneration = defaultArg dto.AbstractClassStubGeneration false
            AbstractClassStubGenerationObjectIdentifier = defaultArg dto.AbstractClassStubGenerationObjectIdentifier "this"
            AbstractClassStubGenerationMethodBody = defaultArg dto.AbstractClassStubGenerationMethodBody "failwith \Not Implemented\""
        }

    /// called when a configuration change takes effect, so None-valued members here should revert options
    /// back to their defaults
    member x.AddDto(dto: FSharpConfigDto) =
        {
            AutomaticWorkspaceInit = defaultArg dto.AutomaticWorkspaceInit x.AutomaticWorkspaceInit
            AbstractClassStubGeneration = defaultArg dto.AbstractClassStubGeneration x.AbstractClassStubGeneration
            AbstractClassStubGenerationObjectIdentifier = defaultArg dto.AbstractClassStubGenerationObjectIdentifier x.AbstractClassStubGenerationObjectIdentifier
            AbstractClassStubGenerationMethodBody = defaultArg dto.AbstractClassStubGenerationMethodBody x.AbstractClassStubGenerationMethodBody
            WorkspaceModePeekDeepLevel = defaultArg dto.WorkspaceModePeekDeepLevel x.WorkspaceModePeekDeepLevel
            ExcludeProjectDirectories = defaultArg dto.ExcludeProjectDirectories x.ExcludeProjectDirectories
            KeywordsAutocomplete = defaultArg dto.KeywordsAutocomplete x.KeywordsAutocomplete
            ExternalAutocomplete = defaultArg dto.ExternalAutocomplete x.ExternalAutocomplete
            Linter = defaultArg dto.Linter x.Linter
            LinterConfig = dto.LinterConfig
            UnionCaseStubGeneration = defaultArg dto.UnionCaseStubGeneration x.UnionCaseStubGeneration
            UnionCaseStubGenerationBody = defaultArg dto.UnionCaseStubGenerationBody x.UnionCaseStubGenerationBody
            RecordStubGeneration = defaultArg dto.RecordStubGeneration x.RecordStubGeneration
            RecordStubGenerationBody = defaultArg dto.RecordStubGenerationBody x.RecordStubGenerationBody
            InterfaceStubGeneration = defaultArg dto.InterfaceStubGeneration x.InterfaceStubGeneration
            InterfaceStubGenerationObjectIdentifier = defaultArg dto.InterfaceStubGenerationObjectIdentifier x.InterfaceStubGenerationObjectIdentifier
            InterfaceStubGenerationMethodBody = defaultArg dto.InterfaceStubGenerationMethodBody x.InterfaceStubGenerationMethodBody
            UnusedOpensAnalyzer = defaultArg dto.UnusedOpensAnalyzer x.UnusedOpensAnalyzer
            UnusedDeclarationsAnalyzer = defaultArg dto.UnusedDeclarationsAnalyzer x.UnusedDeclarationsAnalyzer
            SimplifyNameAnalyzer = defaultArg dto.SimplifyNameAnalyzer x.SimplifyNameAnalyzer
            ResolveNamespaces = defaultArg dto.ResolveNamespaces x.ResolveNamespaces
            EnableReferenceCodeLens = defaultArg dto.EnableReferenceCodeLens x.EnableReferenceCodeLens
            EnableAnalyzers = defaultArg dto.EnableAnalyzers x.EnableAnalyzers
            AnalyzersPath = defaultArg dto.AnalyzersPath x.AnalyzersPath
            DisableInMemoryProjectReferences = defaultArg dto.DisableInMemoryProjectReferences x.DisableInMemoryProjectReferences
            LineLens = {
                Enabled = defaultArg (dto.LineLens |> Option.map (fun n -> n.Enabled)) x.LineLens.Enabled
                Prefix = defaultArg (dto.LineLens |> Option.map (fun n -> n.Prefix)) x.LineLens.Prefix
            }
            UseSdkScripts = defaultArg dto.UseSdkScripts x.UseSdkScripts
            DotNetRoot =
                dto.DotNetRoot
                |> Option.bind (fun s -> if String.IsNullOrEmpty s then None else Some s)
                |> Option.defaultValue FSharpConfig.Default.DotNetRoot
            FSIExtraParameters = defaultArg dto.FSIExtraParameters FSharpConfig.Default.FSIExtraParameters
            FSICompilerToolLocations = defaultArg dto.FSICompilerToolLocations FSharpConfig.Default.FSICompilerToolLocations
            TooltipMode = defaultArg dto.TooltipMode x.TooltipMode
            GenerateBinlog = defaultArg dto.GenerateBinlog x.GenerateBinlog
        }

    member x.ScriptTFM =
        match x.UseSdkScripts with
        | false -> FSIRefs.NetFx
        | true -> FSIRefs.NetCore

/// generate a TokenLegend from an enum representing the token types and the
/// token modifiers.
///
/// since the token types and modifiers are int-backed names, we follow the
/// following logic to create the backing string arrays for the legends:
///   * iterate the enum values
///   * get the enum name
///   * lowercase the first char because of .net naming conventions
let createTokenLegend<'types, 'modifiers when 'types : enum<int> and
                                              'types: (new : unit -> 'types) and
                                              'types: struct and
                                              'types :> Enum and
                                              'modifiers: enum<int> and
                                              'modifiers: (new : unit -> 'modifiers) and
                                              'modifiers: struct and
                                              'modifiers :> Enum> : SemanticTokensLegend =
  let tokenTypes = Enum.GetNames<'types>() |> Array.map String.lowerCaseFirstChar
  let tokenModifiers = Enum.GetNames<'modifiers>() |> Array.map String.lowerCaseFirstChar
  {
    TokenModifiers = tokenModifiers
    TokenTypes = tokenTypes
  }


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
let encodeSemanticHighlightRanges (rangesAndHighlights: (struct(LanguageServerProtocol.Types.Range * ClassificationUtils.SemanticTokenTypes * ClassificationUtils.SemanticTokenModifier list)) array) =
  let fileStart = { Start = { Line = 0; Character = 0}; End = { Line = 0; Character = 0 } }
  let computeLine (prev: LanguageServerProtocol.Types.Range) ((range, ty, mods): struct(LanguageServerProtocol.Types.Range * ClassificationUtils.SemanticTokenTypes * ClassificationUtils.SemanticTokenModifier list)): uint32 [] =
    let lineDelta =
      if prev.Start.Line = range.Start.Line then 0u
      else uint32 (range.Start.Line - prev.Start.Line)
    let charDelta =
      if lineDelta = 0u
      then uint32 (range.Start.Character - prev.Start.Character)
      else uint32 range.Start.Character
    let tokenLen = uint32 (range.End.Character - range.Start.Character)
    let tokenTy = uint32 ty
    let tokenMods =
      match mods with
      | [] -> 0u
      | [ single ] -> uint32 single
      | mods ->
        // because the mods are all bit flags, we can just OR them together
        let flags = mods |> List.reduce (( ||| ))
        uint32 flags
    [| lineDelta; charDelta; tokenLen; tokenTy; tokenMods |]

  match rangesAndHighlights.Length with
  | 0 -> None
  /// only 1 entry, so compute the line from the 0 position
  | 1 ->
    Some (
      computeLine fileStart rangesAndHighlights.[0]
    )
  | _ ->
    let finalArray = Array.zeroCreate (rangesAndHighlights.Length * 5) // 5 elements per entry
    let mutable prev = fileStart
    let mutable idx = 0
    // trying to fill the `finalArray` in a single pass here, since its size is known
    for (currentRange, _, _) as item in rangesAndHighlights do
      let result = computeLine prev item
      // copy the 5-array of results into the final array
      finalArray.[idx] <- result.[0]
      finalArray.[idx+1] <- result.[1]
      finalArray.[idx+2] <- result.[2]
      finalArray.[idx+3] <- result.[3]
      finalArray.[idx+4] <- result.[4]
      prev <- currentRange
      idx <- idx + 5
    Some finalArray
