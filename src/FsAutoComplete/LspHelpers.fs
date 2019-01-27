module FsAutoComplete.LspHelpers

open System
open System.IO
open LanguageServerProtocol.Types
open FsAutoComplete.Utils
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Reflection
open System.Collections.Generic
open System.Text


module FcsRange = Microsoft.FSharp.Compiler.Range

[<AutoOpen>]
module internal Conversions =
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

    let filePathToUri (filePath: string): DocumentUri =
        let uri = StringBuilder(filePath.Length)
        for c in filePath do
            if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') ||
                c = '+' || c = '/' || c = ':' || c = '.' || c = '-' || c = '_' || c = '~' ||
                c > '\xFF' then
                uri.Append(c) |> ignore
            else if c = Path.DirectorySeparatorChar || c = Path.AltDirectorySeparatorChar then
                uri.Append('/') |> ignore
            else
                uri.Append('%') |> ignore
                uri.Append((int c).ToString("X2")) |> ignore

        if uri.Length >= 2 && uri.[0] = '/' && uri.[1] = '/' then // UNC path
            "file:" + uri.ToString()
        else
            "file:///" + (uri.ToString()).TrimStart('/')

    let fcsRangeToLspLocation(range: Microsoft.FSharp.Compiler.Range.range): Lsp.Location =
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

    type TextDocumentIdentifier with
        member doc.GetFilePath() = Uri(doc.Uri).LocalPath

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
            RelatedInformation = [||]
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
            yield! topLevel.Nested |> Seq.ofArray |> Seq.map (inner (Some topLevel.Declaration.Name))
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
            let mutable found: 'kind option = None
            let mutable i = 0
            let possibleCount = possible.Length
            while found.IsNone && i < possibleCount do
                if Array.contains possible.[i] completionItemSet then
                    found <- Some possible.[i]
                i <- i + 1
            found

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

module Markdown =

    open System.Text.RegularExpressions

    let internal stringReplacePatterns =
        [ "&lt;", "<"
          "&gt;", ">"
          "&quot;", "\""
          "&apos;", "'"
          "&amp;", "&"
          "<summary>", "**Description**\n\n"
          "</summary>", "\n"
          "<para>", "\n"
          "</para>", "\n"
          "<remarks>", ""
          "</remarks>", "\n" ]

    let internal regexReplacePatterns =
        let r pat = Regex(pat, RegexOptions.IgnoreCase)

        let code (strings : string array) =
            let str = strings.[0]
            if str.Contains("\n") then
                "```forceNoHighlight" + str + "```"
            else
                "`" + str + "`"
        let returns = Array.item 0 >> sprintf "\n**Returns**\n\n%s"

        let param (s : string[]) = sprintf "* `%s`: %s"(s.[0].Substring(1, s.[0].Length - 2)) s.[1]

        [ r"<c>((?:(?!<c>)(?!<\/c>)[\s\S])*)<\/c>", code
          r"""<see\s+cref=(?:'[^']*'|"[^"]*")>((?:(?!<\/see>)[\s\S])*)<\/see>""", code
          r"""<param\s+name=('[^']*'|"[^"]*")>((?:(?!<\/param>)[\s\S])*)<\/param>""", param
          r"""<typeparam\s+name=('[^']*'|"[^"]*")>((?:(?!<\/typeparam>)[\s\S])*)<\/typeparam>""", param
          r"""<exception\s+cref=('[^']*'|"[^"]*")>((?:(?!<\/exception>)[\s\S])*)<\/exception>""", param
          r"""<a\s+href=('[^']*'|"[^"]*")>((?:(?!<\/a>)[\s\S])*)<\/a>""", fun s -> (s.[0].Substring(1, s.[0].Length - 2))
          r"<returns>((?:(?!<\/returns>)[\s\S])*)<\/returns>", returns ]

    /// Helpers to create a new section in the markdown comment
    let internal suffixXmlKey (tag : string) (value : string) (str : string) =
        match str.IndexOf(tag) with
        | x when x <> -1 ->
            let insertAt =
                if str.Chars(x - 1) = ' ' then
                    x - 1
                else
                    x
            str.Insert(insertAt, value)
        | _ -> str

    let internal suffixTypeparam = suffixXmlKey "<typeparam" "\n**Type parameters**\n\n"

    let internal suffixException = suffixXmlKey "<exception" "\n**Exceptions**\n\n"

    let internal suffixParam = suffixXmlKey "<param" "\n**Parameters**\n\n"

    /// Replaces XML tags with Markdown equivalents.
    /// List of standard tags: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/xml-documentation
    let internal replaceXml (str : string) : string =
        let str =
            str
            |> suffixTypeparam
            |> suffixException
            |> suffixParam

        let res =
            regexReplacePatterns
            |> List.fold (fun res (regex : Regex, formatter : string[] -> string) ->
                // repeat replacing with same pattern to handle nested tags, like `<c>..<c>..</c>..</c>`
                let rec loop res : string =
                    match regex.Match res with
                    | m when m.Success ->
                        let [| firstGroup |], otherGroups =
                            m.Groups
                            |> Seq.cast<Group>
                            |> Seq.map (fun g -> g.Value)
                            |> Seq.toArray
                            |> Array.splitAt 1
                        loop <| res.Replace(firstGroup, formatter otherGroups)
                    | _ -> res
                loop res
            ) str

        stringReplacePatterns
        |> List.fold (fun (res : string) (oldValue, newValue) ->
            res.Replace(oldValue, newValue)
        ) res

    let internal normalizeLeadingSpace (content : string) =
        content
            .Replace("\r\n", "\n")
            .Split('\n')
        |> Array.map(fun line ->
            if line.Length > 1 && line.[0] = ' ' then
                line.[1..]
            else
                line
        )
        |> String.concat "\n"

    let createCommentBlock (comment : string) : string =
        comment
        |> replaceXml
        |> normalizeLeadingSpace

module Workspace =
    open FsAutoComplete.WorkspacePeek
    open FsAutoComplete.CommandResponse

    let mapInteresting i =
        match i with
        | Interesting.Directory (p, fsprojs) ->
            WorkspacePeekFound.Directory { WorkspacePeekFoundDirectory.Directory = p; Fsprojs = fsprojs }
        | Interesting.Solution (p, sd) ->
            let rec item (x: FsAutoComplete.WorkspacePeek.SolutionItem) =
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

type FSharpConfigDto = {
    WorkspaceModePeekDeepLevel: int option
    WorkspaceExcludedDirs: string [] option
    KeywordsAutocomplete: bool option
    ExternalAutocomplete: bool option
    Linter: bool option
    RecordStubGeneration: bool option
    UnusedOpensAnalyzer: bool option
    UnusedDeclarationsAnalyzer: bool option
    SimplifyNameAnalyzer: bool option
    ResolveNamespaces: bool option
    MinimizeBackgroundParsing: bool option
    EnableBackgroundSymbolCache: bool option
    EnableReferenceCodeLens: bool option
    EnableAnalyzers: bool option
    AnalyzersPath: string [] option
    DisableInMemoryProjectReferences: bool option
}

type FSharpConfig = {
    WorkspaceModePeekDeepLevel: int
    WorkspaceExcludedDirs: string []
    KeywordsAutocomplete: bool
    ExternalAutocomplete: bool
    Linter: bool
    RecordStubGeneration: bool
    UnusedOpensAnalyzer: bool
    UnusedDeclarationsAnalyzer: bool
    SimplifyNameAnalyzer: bool
    ResolveNamespaces: bool
    MinimizeBackgroundParsing: bool
    EnableBackgroundSymbolCache: bool
    EnableReferenceCodeLens: bool
    EnableAnalyzers: bool
    AnalyzersPath: string []
    DisableInMemoryProjectReferences: bool
}
with
    static member Default =
        {
            WorkspaceModePeekDeepLevel = 2
            WorkspaceExcludedDirs = [||]
            KeywordsAutocomplete = false
            ExternalAutocomplete = false
            Linter = false
            RecordStubGeneration = false
            UnusedOpensAnalyzer = false
            UnusedDeclarationsAnalyzer = false
            SimplifyNameAnalyzer = false
            ResolveNamespaces = false
            MinimizeBackgroundParsing = false
            EnableBackgroundSymbolCache = false
            EnableReferenceCodeLens = false
            EnableAnalyzers = false
            AnalyzersPath = [||]
            DisableInMemoryProjectReferences = false
        }

    static member FromDto(dto: FSharpConfigDto) =
        {
            WorkspaceModePeekDeepLevel = defaultArg dto.WorkspaceModePeekDeepLevel 2
            WorkspaceExcludedDirs = defaultArg dto.WorkspaceExcludedDirs [||]
            KeywordsAutocomplete = defaultArg dto.KeywordsAutocomplete false
            ExternalAutocomplete = defaultArg dto.ExternalAutocomplete false
            Linter = defaultArg dto.Linter false
            RecordStubGeneration = defaultArg dto.RecordStubGeneration false
            UnusedOpensAnalyzer = defaultArg dto.UnusedOpensAnalyzer false
            UnusedDeclarationsAnalyzer = defaultArg dto.UnusedDeclarationsAnalyzer false
            SimplifyNameAnalyzer = defaultArg dto.SimplifyNameAnalyzer false
            ResolveNamespaces = defaultArg dto.ResolveNamespaces false
            MinimizeBackgroundParsing = defaultArg dto.MinimizeBackgroundParsing false
            EnableBackgroundSymbolCache = defaultArg dto.EnableBackgroundSymbolCache false
            EnableReferenceCodeLens = defaultArg dto.EnableReferenceCodeLens false
            EnableAnalyzers = defaultArg dto.EnableAnalyzers false
            AnalyzersPath = defaultArg dto.AnalyzersPath [||]
            DisableInMemoryProjectReferences = defaultArg dto.DisableInMemoryProjectReferences false
        }

    member x.AddDto(dto: FSharpConfigDto) =
        {
            WorkspaceModePeekDeepLevel = defaultArg dto.WorkspaceModePeekDeepLevel x.WorkspaceModePeekDeepLevel
            WorkspaceExcludedDirs = defaultArg dto.WorkspaceExcludedDirs x.WorkspaceExcludedDirs
            KeywordsAutocomplete = defaultArg dto.KeywordsAutocomplete x.KeywordsAutocomplete
            ExternalAutocomplete = defaultArg dto.ExternalAutocomplete x.ExternalAutocomplete
            Linter = defaultArg dto.Linter x.Linter
            RecordStubGeneration = defaultArg dto.RecordStubGeneration x.RecordStubGeneration
            UnusedOpensAnalyzer = defaultArg dto.UnusedOpensAnalyzer x.UnusedOpensAnalyzer
            UnusedDeclarationsAnalyzer = defaultArg dto.UnusedDeclarationsAnalyzer x.UnusedDeclarationsAnalyzer
            SimplifyNameAnalyzer = defaultArg dto.SimplifyNameAnalyzer x.SimplifyNameAnalyzer
            ResolveNamespaces = defaultArg dto.ResolveNamespaces x.ResolveNamespaces
            MinimizeBackgroundParsing = defaultArg dto.MinimizeBackgroundParsing x.MinimizeBackgroundParsing
            EnableBackgroundSymbolCache = defaultArg dto.EnableBackgroundSymbolCache x.EnableBackgroundSymbolCache
            EnableReferenceCodeLens = defaultArg dto.EnableReferenceCodeLens x.EnableReferenceCodeLens
            EnableAnalyzers = defaultArg dto.EnableAnalyzers x.EnableAnalyzers
            AnalyzersPath = defaultArg dto.AnalyzersPath x.AnalyzersPath
            DisableInMemoryProjectReferences = defaultArg dto.DisableInMemoryProjectReferences x.DisableInMemoryProjectReferences
        }

//source: https://nbevans.wordpress.com/2014/08/09/a-simple-stereotypical-javascript-like-debounce-service-for-f/
type Debounce<'a>(timeout, fn) =
    let debounce fn timeout = MailboxProcessor<'a>.Start(fun agent ->
        let rec loop ida idb arg = async {
            let! r = agent.TryReceive(timeout)
            match r with
            | Some arg -> return! loop ida (idb + 1) (Some arg)
            | None when ida <> idb -> fn arg.Value; return! loop idb idb None
            | None -> return! loop ida idb arg
        }
        loop 0 0 None)

    let mailbox = debounce fn timeout

    /// Calls the function, after debouncing has been applied.
    member __.Bounce(arg) = mailbox.Post(arg)