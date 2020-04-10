module FsAutoComplete.LspHelpers

open System
open System.IO
open LanguageServerProtocol.Types
open FsAutoComplete.Utils
open FSharp.Compiler.SourceCodeServices
open FSharp.Reflection
open System.Collections.Generic
open System.Text
open ProjectSystem

module FcsRange = FSharp.Compiler.Range

[<AutoOpen>]
module Conversions =
    module Lsp = LanguageServerProtocol.Types

    let protocolPosToPos (pos: Lsp.Position): FcsRange.pos =
        FcsRange.mkPos (pos.Line + 1) (pos.Character + 1)

    let posToProtocolPos (pos: FcsRange.pos): Lsp.Position =
        { Line = pos.Line - 1; Character = pos.Column - 1 }

    let fcsPosToLsp (pos: FcsRange.pos): Lsp.Position =
        { Line = pos.Line - 1; Character = pos.Column }

    let fcsRangeToLsp(range: FcsRange.range): Lsp.Range =
        {
            Start = fcsPosToLsp range.Start
            End = fcsPosToLsp range.End
        }

    let protocolRangeToRange fn (range: Lsp.Range): FcsRange.range =
        FcsRange.mkRange fn (protocolPosToPos range.Start) (protocolPosToPos range.End)

    let symbolUseRangeToLsp (range: SymbolCache.SymbolUseRange): Lsp.Range =
        {
            Start = { Line = range.StartLine - 1; Character = range.StartColumn - 1 }
            End = { Line = range.EndLine - 1; Character = range.EndColumn - 1 }
        }

    /// Algorithm from https://stackoverflow.com/a/35734486/433393 for converting file paths to uris,
    /// modified slightly to not rely on the System.Path members because they vary per-platform
    let filePathToUri (filePath: string): DocumentUri =
        let filePath, finished =
            if filePath.Contains "Untitled-" then
                let rg = System.Text.RegularExpressions.Regex.Match(filePath, @"(Untitled-\d+).fsx")
                if rg.Success then
                    rg.Groups.[1].Value, true
                else
                    filePath, false
            else
                filePath, false

        if not finished then
            let uri = StringBuilder(filePath.Length)
            for c in filePath do
                if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') ||
                    c = '+' || c = '/' || c = '.' || c = '-' || c = '_' || c = '~' ||
                    c > '\xFF' then
                    uri.Append(c) |> ignore
                // handle windows path separator chars.
                // we _would_ use Path.DirectorySeparator/AltDirectorySeparator, but those vary per-platform and we want this
                // logic to work cross-platform (for tests)
                else if c = '\\' then
                    uri.Append('/') |> ignore
                else
                    uri.Append('%') |> ignore
                    uri.Append((int c).ToString("X2")) |> ignore

            if uri.Length >= 2 && uri.[0] = '/' && uri.[1] = '/' then // UNC path
                "file:" + uri.ToString()
            else
                "file:///" + (uri.ToString()).TrimStart('/')
        else
            "untitled:" + filePath

    let fcsRangeToLspLocation(range: FSharp.Compiler.Range.range): Lsp.Location =
        let fileUri = filePathToUri range.FileName
        let lspRange = fcsRangeToLsp range
        {
            Uri = fileUri
            Range = lspRange
        }

    let symbolUseRangeToLspLocation (range: SymbolCache.SymbolUseRange): Lsp.Location =
        let fileUri = filePathToUri range.FileName
        let lspRange = symbolUseRangeToLsp range
        {
            Uri = fileUri
            Range = lspRange
        }

    let findDeclToLspLocation(decl: FsAutoComplete.FindDeclarationResult): Lsp.Location =
        match decl with
        | FsAutoComplete.FindDeclarationResult.ExternalDeclaration ex ->
            let fileUri = filePathToUri ex.File
            {
                Uri = fileUri
                Range = {
                    Start = { Line = ex.Line - 1; Character = ex.Column - 1 }
                    End = { Line = ex.Line - 1; Character = ex.Column - 1 }
                }
            }
        | FsAutoComplete.FindDeclarationResult.Range r -> fcsRangeToLspLocation r

    /// a test that checks if the start of the line is a windows-style drive string, for example
    /// /d:, /c:, /z:, etc.
    let isWindowsStyleDriveLetterMatch (s: string) =
        match s.[0..2].ToCharArray() with
        | [| |]
        | [| _ |]
        | [| _; _ |] -> false
        // 26 windows drive letters allowed, only
        | [| '/'; c; ':' |] when Char.IsLetter c -> true
        | _ -> false

    /// handles unifying the local-path logic for windows and non-windows paths,
    /// without doing a check based on what the current system's OS is.
    let fileUriToLocalPath (u: DocumentUri) =
        let initialLocalPath = Uri(u).LocalPath
        let fn =
            if isWindowsStyleDriveLetterMatch initialLocalPath
            then initialLocalPath.TrimStart('/')
            else initialLocalPath
        if u.StartsWith "untitled:" then (fn + ".fsx") else fn

    type TextDocumentIdentifier with
        member doc.GetFilePath() = fileUriToLocalPath doc.Uri

    type VersionedTextDocumentIdentifier with
        member doc.GetFilePath() = fileUriToLocalPath doc.Uri

    type TextDocumentItem with
        member doc.GetFilePath() = fileUriToLocalPath doc.Uri

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

    let getSymbolInformations (uri: DocumentUri) (glyphToSymbolKind: FSharpGlyph -> SymbolKind option) (topLevel: FSharpNavigationTopLevelDeclaration): SymbolInformation seq =
        let inner (container: string option) (decl: FSharpNavigationDeclarationItem): SymbolInformation =
            // We should nearly always have a kind, if the client doesn't send weird capabilites,
            // if we don't why not assume module...
            let kind = defaultArg (glyphToSymbolKind decl.Glyph) SymbolKind.Module
            let location = { Uri = uri; Range = fcsRangeToLsp decl.Range }
            {
                SymbolInformation.Name = decl.Name
                Kind = kind
                Location = location
                ContainerName = container
            }
        seq {
            yield (inner None topLevel.Declaration)
            yield! topLevel.Nested |> Seq.map (inner (Some topLevel.Declaration.Name))
        }

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
    open ProjectSystem.WorkspacePeek
    open FsAutoComplete.CommandResponse

    let mapInteresting i =
        match i with
        | Interesting.Directory (p, fsprojs) ->
            WorkspacePeekFound.Directory { WorkspacePeekFoundDirectory.Directory = p; Fsprojs = fsprojs }
        | Interesting.Solution (p, sd) ->
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
}
with
    static member Default =
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
            UseSdkScripts = false
            DotNetRoot = Environment.dotnetSDKRoot.Value
            FSIExtraParameters = [||]
            FSICompilerToolLocations = [||]
            TooltipMode = "full"
        }

    static member FromDto(dto: FSharpConfigDto) =
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
            UseSdkScripts = defaultArg dto.UseSdkScripts false
            DotNetRoot =
                dto.DotNetRoot
                |> Option.bind (fun s -> if String.IsNullOrEmpty s then None else Some s)
                |> Option.defaultValue Environment.dotnetSDKRoot.Value
            FSIExtraParameters = defaultArg dto.FSIExtraParameters FSharpConfig.Default.FSIExtraParameters
            FSICompilerToolLocations = defaultArg dto.FSICompilerToolLocations FSharpConfig.Default.FSICompilerToolLocations
            TooltipMode = defaultArg dto.TooltipMode "full"
        }

    /// called when a configuration change takes effect, so None-valued members here should revert options
    /// back to their defaults
    member x.AddDto(dto: FSharpConfigDto) =
        {
            AutomaticWorkspaceInit = defaultArg dto.AutomaticWorkspaceInit x.AutomaticWorkspaceInit
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
        }

    member x.ScriptTFM =
        match x.UseSdkScripts with
        | false -> FSIRefs.NetFx
        | true -> FSIRefs.NetCore
