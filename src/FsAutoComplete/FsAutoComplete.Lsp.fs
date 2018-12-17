module FsAutoComplete.Lsp

open Argu
open System
open System.IO
open LanguageServerProtocol.Server
open LanguageServerProtocol.Types
open FsAutoComplete.Utils
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Reflection
open System.Collections.Generic
open System.Text

module FcsRange = Microsoft.FSharp.Compiler.Range

[<AutoOpen>]
module private Conversions =
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
        | FindDeclarationResult.ExternalDeclaration ex ->
            let fileUri = filePathToUri ex.File
            {
                Uri = fileUri
                Range = {
                    Start = { Line = ex.Line - 1; Character = ex.Column - 1 }
                    End = { Line = ex.Line - 1; Character = ex.Column - 1 }
                }
            }
        | FindDeclarationResult.Range r -> fcsRangeToLspLocation r

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
            Source = Some ("F# Compiler")
            Message = error.Message
            Code = Some (DiagnosticCode.Number error.ErrorNumber)
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

[<AutoOpen>]
module private GlyphConversions =
    let private glyphToKindGenerator<'kind when 'kind : equality>
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

    let private stringReplacePatterns =
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

    let private regexReplacePatterns =
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
    let private suffixXmlKey (tag : string) (value : string) (str : string) =
        match str.IndexOf(tag) with
        | x when x <> -1 ->
            let insertAt =
                if str.Chars(x - 1) = ' ' then
                    x - 1
                else
                    x
            str.Insert(insertAt, value)
        | _ -> str

    let private suffixTypeparam = suffixXmlKey "<typeparam" "\n**Type parameters**\n\n"

    let private suffixException = suffixXmlKey "<exception" "\n**Exceptions**\n\n"

    let private suffixParam = suffixXmlKey "<param" "\n**Parameters**\n\n"

    /// Replaces XML tags with Markdown equivalents.
    /// List of standard tags: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/xml-documentation
    let private replaceXml (str : string) : string =
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

    let private normalizeLeadingSpace (content : string) =
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

open LanguageServerProtocol
open LanguageServerProtocol.LspResult
open FsAutoComplete
open FSharpLint.Application.LintWarning
open System.Diagnostics

type PlainNotification= { Content: string }

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

type FSharpLspClient(sendServerRequest: ClientNotificationSender) =
    inherit LspClient ()

    override __.WindowShowMessage(p) =
        sendServerRequest "window/showMessage" (box p) |> Async.Ignore

    override __.WindowLogMessage(p) =
        sendServerRequest "window/logMessage" (box p) |> Async.Ignore

    override __.TextDocumentPublishDiagnostics(p) =
        sendServerRequest "textDocument/publishDiagnostics" (box p) |> Async.Ignore

    ///Custom notification for workspace/solution/project loading events
    member __.NotifyWorkspace (p: PlainNotification) =
        sendServerRequest "fsharp/notifyWorkspace" (box p) |> Async.Ignore

    ///Custom notification for initial workspace peek
    member __.NotifyWorkspacePeek (p: PlainNotification) =
        sendServerRequest "fsharp/notifyWorkspacePeek" (box p) |> Async.Ignore

    // TODO: Add the missing notifications
    // TODO: Implement requests

type FsharpLspServer(commands: Commands, lspClient: FSharpLspClient) =
    inherit LspServer()

    let mutable clientCapabilities: ClientCapabilities option = None
    let mutable glyphToCompletionKind = glyphToCompletionKindGenerator None
    let mutable glyphToSymbolKind = glyphToSymbolKindGenerator None

    let subscriptions = ResizeArray<IDisposable>()

    let mutable config = FSharpConfig.Default

    let workspaceReady = Event<unit>()
    let WorkspaceReady = workspaceReady.Publish

    let getRecentTypeCheckResultsForFile file =
        match commands.TryGetFileCheckerOptionsWithLines file with
        | ResultOrString.Error s ->
            Result.Error (sprintf "Can't get filecheck options with lines: %s" s)
        | ResultOrString.Ok (options, lines) ->
            let tyResOpt = commands.TryGetRecentTypeCheckResultsForFile(file, options)
            match tyResOpt with
            | None ->
                Result.Error "Cached typecheck results not yet available"
            | Some tyRes ->
                Ok (options, lines, tyRes)

    let diagnosticCollections = System.Collections.Concurrent.ConcurrentDictionary<DocumentUri * string,Diagnostic[]>()

    let sendDiagnostics (uri: DocumentUri) =
        let diags =
            diagnosticCollections
            |> Seq.collect (fun kv ->
                let (u, _) = kv.Key
                if u = uri then kv.Value else [||])
            |> Seq.sortBy (fun n ->
                n.Range.Start.Line
            )
            |> Seq.toArray
        {Uri = uri; Diagnostics = diags}
        |> lspClient.TextDocumentPublishDiagnostics
        |> Async.Start

    do
        commands.Notify.Subscribe(fun n ->
            try
                match n with
                | NotificationEvent.Workspace ws ->
                    let ws = CommandResponse.serialize JsonSerializer.writeJson ws

                    {Content = ws}
                    |> lspClient.NotifyWorkspace
                    |> Async.Start

                | NotificationEvent.ParseError (CoreResponse.Errors (errors, file)) ->
                    let diags = errors |> Array.map (fcsErrorToDiagnostic)
                    let uri = filePathToUri file
                    diagnosticCollections.AddOrUpdate((uri, "F# Compiler"), diags, fun _ _ -> diags) |> ignore
                    sendDiagnostics uri

                // | NotificationEvent.UnusedOpens (CoreResponse.UnusedOpens opens) ->
                //     let diags = opens |> Array.map(fun n ->
                //         {Diagnostic.Range = fcsRangeToLsp n; Code = None; Severity = Some DiagnosticSeverity.Hint; Source = Some "FSAC"; Message = "Unused open statement" }
                //     )
                //     let uri = filePathToUri opens.[0].FileName
                //     diagnosticCollections.AddOrUpdate((uri, "F# Unused opens"), diags, fun _ _ -> diags) |> ignore
                //     sendDiagnostics uri

                // | NotificationEvent.UnusedDeclarations (CoreResponse.UnusedDeclarations decls) ->
                //     let diags = decls |> Array.map(fun (n, _) ->
                //         {Diagnostic.Range = fcsRangeToLsp n; Code = None; Severity = Some DiagnosticSeverity.Hint; Source = Some "FSAC"; Message = "This value is unused" }
                //     )
                //     let (r,_) = decls.[0]
                //     let uri = filePathToUri r.FileName
                //     diagnosticCollections.AddOrUpdate((uri, "F# Unused declarations"), diags, fun _ _ -> diags) |> ignore
                //     sendDiagnostics uri

                // | NotificationEvent.SimplifyNames (CoreResponse.SimplifiedName decls) ->
                //     let diags = decls |> Array.map(fun (n, _) ->
                //         {Diagnostic.Range = fcsRangeToLsp n; Code = None; Severity = Some DiagnosticSeverity.Information; Source = Some "FSAC"; Message = "This qualifier is redundant" }
                //     )
                //     let (r,_) = decls.[0]
                //     let uri = filePathToUri r.FileName
                //     diagnosticCollections.AddOrUpdate((uri, "F# simplify names"), diags, fun _ _ -> diags) |> ignore
                //     sendDiagnostics uri

                // | NotificationEvent.Lint (CoreResponse.Lint warnings) ->
                //     let diags =
                //         warnings |> List.map(fun (n: Warning) ->
                //             {Diagnostic.Range = fcsRangeToLsp n.Range; Code = None; Severity = Some DiagnosticSeverity.Information; Source = Some "F# Linter"; Message = "Lint: " + n.Info })
                //         |> List.toArray
                //     let uri = filePathToUri warnings.[0].Range.FileName

                //     diagnosticCollections.AddOrUpdate((uri, "F# Linter"), diags, fun _ _ -> diags) |> ignore
                //     sendDiagnostics uri
                | _ ->
                    //TODO: Add analyzer support
                    ()
            with
            | _ -> ()
        ) |> subscriptions.Add

    member __.positionHandler<'a, 'b when 'b :> ITextDocumentPositionParams> (f: 'b -> FcsRange.pos -> ParseAndCheckResults -> string -> string [] ->  AsyncLspResult<'a>) (arg: 'b) : AsyncLspResult<'a> =
        async {
            let pos = arg.GetFcsPos()
            let file = arg.GetFilePath()
            Debug.print "Position request: %s at %A" file pos

            return!
                match commands.TryGetFileCheckerOptionsWithLinesAndLineStr(file, pos) with
                | ResultOrString.Error s ->
                    Debug.print "Getting file checker options failed: %s" s
                    AsyncLspResult.internalError s
                | ResultOrString.Ok (options, lines, lineStr) ->
                    try
                        let tyResOpt = commands.TryGetRecentTypeCheckResultsForFile(file, options)
                        match tyResOpt with
                        | None ->
                            Debug.print "Cached typecheck results not yet available"
                            AsyncLspResult.internalError "Cached typecheck results not yet available"
                        | Some tyRes ->
                            async {
                                let! r = Async.Catch (f arg pos tyRes lineStr lines)
                                match r with
                                | Choice1Of2 r -> return r
                                | Choice2Of2 e ->
                                    Debug.print "Operation failed: %s" e.Message
                                    return LspResult.internalError e.Message
                            }
                    with e ->
                        Debug.print "Operation failed: %s" e.Message
                        AsyncLspResult.internalError e.Message
        }


    override __.Initialize(p) = async {
        // Debug.print "Initialize"
        clientCapabilities <- p.Capabilities
        glyphToCompletionKind <- glyphToCompletionKindGenerator clientCapabilities
        glyphToSymbolKind <- glyphToSymbolKindGenerator clientCapabilities

        let c =
            p.InitializationOptions
            |> Option.map Server.deserialize<FSharpConfigDto>
            |> Option.map FSharpConfig.FromDto
            |> Option.getOrElse FSharpConfig.Default

        config <- c
        // Debug.print "Config: %A" c

        match p.RootPath with
        | None -> ()
        | Some p ->
            async {
                let! peek = commands.WorkspacePeek p config.WorkspaceModePeekDeepLevel (List.ofArray config.WorkspaceExcludedDirs)
                return
                    match peek.[0] with
                    | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                        ()
                    | CoreResponse.WorkspacePeek ints ->

                        let serialized = CommandResponse.workspacePeek JsonSerializer.writeJson ints
                        lspClient.NotifyWorkspacePeek {Content = serialized} |> Async.Start

                        let peeks =
                            ints
                            |> List.map Workspace.mapInteresting
                            |> List.sortByDescending (fun x ->
                                match x with
                                | CommandResponse.WorkspacePeekFound.Solution sln -> Workspace.countProjectsInSln sln
                                | CommandResponse.WorkspacePeekFound.Directory _ -> -1)

                        match peeks with
                        | [] -> ()
                        | [CommandResponse.WorkspacePeekFound.Directory projs] ->
                            commands.WorkspaceLoad ignore projs.Fsprojs false
                            |> Async.Ignore
                            |> Async.Start
                        | CommandResponse.WorkspacePeekFound.Solution sln::_ ->
                            let projs =
                                sln.Items
                                |> List.collect Workspace.foldFsproj
                                |> List.map fst
                            commands.WorkspaceLoad ignore projs false
                            |> Async.Ignore
                            |> Async.Start
                        | _ ->
                            //TODO: Above case always picks solution with most projects, should be changed
                            ()
                    | _ -> ()
            } |> Async.Start
            ()
        // Debug.print "INIT RETURN"
        return
            { InitializeResult.Default with
                Capabilities =
                    { ServerCapabilities.Default with
                        HoverProvider = Some true
                        RenameProvider = Some true
                        DefinitionProvider = Some true
                        TypeDefinitionProvider = Some true
                        ReferencesProvider = Some true
                        DocumentHighlightProvider = Some true
                        DocumentSymbolProvider = Some true
                        WorkspaceSymbolProvider = Some true
                        CodeActionProvider = Some true
                        TextDocumentSync =
                            Some { TextDocumentSyncOptions.Default with
                                     OpenClose = Some true
                                     Change = Some TextDocumentSyncKind.Full
                                     Save = Some { IncludeText = Some true }
                                 }
                        CompletionProvider =
                            Some {
                                ResolveProvider = Some true
                                TriggerCharacters = Some ([| "."; "'"; "," |])
                            }
                    }
            }
            |> success
    }

    override __.TextDocumentDidOpen(p) = async {
        if not commands.IsWorkspaceReady then
            do! commands.WorkspaceReady |> Async.AwaitEvent
            workspaceReady.Trigger ()


        let doc = p.TextDocument
        let filePath = Uri(doc.Uri).LocalPath
        let content = doc.Text.Split('\n')

        do! (commands.Parse filePath content doc.Version |> Async.Ignore)
        if config.Linter then do! (commands.Lint filePath |> Async.Ignore)
        if config.UnusedOpensAnalyzer then do! (commands.GetUnusedOpens filePath |> Async.Ignore)
        if config.UnusedDeclarationsAnalyzer then do! (commands.GetUnusedDeclarations filePath |> Async.Ignore)
        if config.SimplifyNameAnalyzer then do! (commands.GetSimplifiedNames filePath |> Async.Ignore)
    }

    override __.TextDocumentDidChange(p) = async {
        if not commands.IsWorkspaceReady then
            Debug.print "Workspace not ready"
            ()
        else
            let doc = p.TextDocument
            let filePath = Uri(doc.Uri).LocalPath
            let contentChange = p.ContentChanges |> Seq.tryLast
            match contentChange, doc.Version with
            | Some contentChange, Some version ->
                if contentChange.Range.IsNone && contentChange.RangeLength.IsNone then
                    let content = contentChange.Text.Split('\n')
                    do! (commands.Parse filePath content version |> Async.Ignore)
                    if config.Linter then do! (commands.Lint filePath |> Async.Ignore)
                    if config.UnusedOpensAnalyzer then do! (commands.GetUnusedOpens filePath |> Async.Ignore)
                    if config.UnusedDeclarationsAnalyzer then do! (commands.GetUnusedDeclarations filePath |> Async.Ignore)
                    if config.SimplifyNameAnalyzer then do! (commands.GetSimplifiedNames filePath |> Async.Ignore)
                else
                    Debug.print "Parse not started, received partial change"
            | _ ->
                Debug.print "Found no change for %s" filePath
                ()
    }

    override __.TextDocumentCompletion(p) = async {

        // Sublime-lsp doesn't like when we answer null so we answer an empty list instead
        let noCompletion = success (Some { IsIncomplete = true; Items = [||] })
        let doc = p.TextDocument
        let file = Uri(doc.Uri).LocalPath
        let pos = p.GetFcsPos()
        let! res =
            match commands.TryGetFileCheckerOptionsWithLines file with
            | ResultOrString.Error s -> AsyncLspResult.internalError s
            | ResultOrString.Ok (options, lines) ->
                let line = p.Position.Line
                let col = p.Position.Character
                let lineStr = lines.[line]
                let word = lineStr.Substring(0, col)
                let ok = line <= lines.Length && line >= 1 && col <= lineStr.Length + 1 && col >= 1
                if not ok then
                    AsyncLspResult.internalError "not ok"
                elif (lineStr.StartsWith "#" && (FsAutoComplete.KeywordList.hashDirectives |> List.exists (fun (n,_) -> n.StartsWith word ) || word.Contains "\n" )) then
                    let its =
                        FsAutoComplete.KeywordList.hashDirectives
                        |> List.map (fun (k, d) ->
                            { CompletionItem.Create(k) with
                                Kind = Some CompletionItemKind.Keyword
                                InsertText = Some k
                                FilterText = Some k
                                SortText = Some k
                                Documentation = Some (Documentation.String d)
                                Label = "#" + k
                            })
                        |> List.toArray
                    let completionList = { IsIncomplete = false; Items = its}
                    async.Return (success (Some completionList))
                else
                    async {
                        let! tyResOpt =
                            match p.Context with
                            | None -> commands.TryGetRecentTypeCheckResultsForFile(file, options) |> async.Return
                            | Some ctx ->
                                if ctx.triggerKind = CompletionTriggerKind.Invoked || (ctx.triggerCharacter = Some ".") then
                                    let f = String.concat "\n" lines
                                    commands.CheckFileInProject(file, commands.LastVersionChecked + 1, f, options)
                                else
                                    commands.TryGetRecentTypeCheckResultsForFile(file, options) |> async.Return

                        match tyResOpt with
                        | None -> return LspResult.internalError "no type check results"
                        | Some tyRes ->
                            let! res = commands.Completion tyRes pos lineStr lines file None (config.KeywordsAutocomplete) (config.ExternalAutocomplete)
                            let x = if res.Length = 1 then res.[0] else res.[1]
                            let res =
                                match x with
                                | CoreResponse.Completion(decls, keywords) ->
                                    let items =
                                        decls
                                        |> Array.mapi (fun id d ->
                                            let code =
                                                if System.Text.RegularExpressions.Regex.IsMatch(d.Name, """^[a-zA-Z][a-zA-Z0-9']+$""") then d.Name else
                                                PrettyNaming.QuoteIdentifierIfNeeded d.Name
                                            let label =
                                                match d.NamespaceToOpen with
                                                | Some no -> sprintf "%s (open %s)" d.Name no
                                                | None -> d.Name

                                            { CompletionItem.Create(d.Name) with
                                                Kind = glyphToCompletionKind d.Glyph
                                                InsertText = Some code
                                                SortText = Some (sprintf "%06d" id)
                                                FilterText = Some d.Name
                                                Label = label
                                            }
                                        )
                                    let kwds =
                                        if not keywords
                                        then []
                                        else
                                            FsAutoComplete.KeywordList.allKeywords
                                            |> List.mapi (fun id k ->
                                                { CompletionItem.Create(k) with
                                                    Kind = Some CompletionItemKind.Keyword
                                                    InsertText = Some k
                                                    SortText = Some (sprintf "1000000%d" id)
                                                    FilterText = Some k
                                                    Label = k })
                                    let its = Array.append items (List.toArray kwds)
                                    let completionList = { IsIncomplete = false; Items = its}
                                    success (Some completionList)
                                | _ -> noCompletion
                            return res
                    }
        return res
    }

    override __.CompletionItemResolve(ci) = async {
        let res = commands.Helptext ci.InsertText.Value
        let res =
            match res.[0] with
            | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                ci
            | CoreResponse.HelpTextSimple(name, str) ->
                let d = Documentation.Markup (markdown str)
                {ci with Detail = Some name; Documentation = Some d  }
            | CoreResponse.HelpText (name, tip, additionalEdit) ->
                let (si, comment) = (TipFormatter.formatTip tip) |> List.collect id |> List.head
                //TODO: Add insert namespace
                let d = Documentation.Markup (markdown comment)
                {ci with Detail = Some name; Documentation = Some d  }
            | _ -> ci
        return success res
    }

    override x.TextDocumentHover(p) =
        p |> x.positionHandler (fun p pos tyRes lineStr lines ->
            async {
                let! res = commands.ToolTip tyRes pos lineStr
                let res =
                    match res.[0] with
                    | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                        LspResult.internalError msg
                    | CoreResponse.ToolTip(tip, signature, footer, typeDoc) ->
                        let (signature, comment, footer) = TipFormatter.formatTipEnhanced tip signature footer typeDoc |> List.head |> List.head //I wonder why we do that

                        let markStr lang (value:string) = MarkedString.WithLanguage { Language = lang ; Value = value }
                        let fsharpBlock (lines: string[]) = lines |> String.concat "\n" |> markStr "fsharp"

                        let sigContent =
                            let lines =
                                signature.Split '\n'
                                |> Array.filter (not << String.IsNullOrWhiteSpace)

                            match lines |> Array.splitAt (lines.Length - 1) with
                            | (h, [| StartsWith "Full name:" fullName |]) ->
                                [| yield fsharpBlock h
                                   yield markStr "plaintext"  ("*" + fullName + "*") |]
                            | _ -> [| fsharpBlock lines |]

                        let commentContent =
                            comment
                            |> Markdown.createCommentBlock
                            |> markStr "plaintext"

                        let footerContent =
                            footer.Split '\n'
                            |> Array.filter (not << String.IsNullOrWhiteSpace)
                            |> Array.map (fun n -> markStr "plaintext" ("*" + n + "*"))


                        let response =
                            {
                                Contents =
                                    MarkedStrings
                                        [|
                                            yield! sigContent
                                            yield commentContent
                                            yield! footerContent
                                        |]
                                Range = None
                            }
                        success (Some response)
                    | _ -> LspResult.notImplemented
                return res
            })

    override x.TextDocumentRename(p) =
        p |> x.positionHandler (fun p pos tyRes lineStr lines ->
            async {
                let! res = commands.SymbolUseProject tyRes pos lineStr
                let res =
                    match res.[0] with
                    | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                        LspResult.internalError msg
                    | CoreResponse.SymbolUse (_, uses) ->
                        let documentChanges =
                            uses
                            |> Array.groupBy (fun sym -> sym.FileName)
                            |> Array.map(fun (fileName, symbols) ->
                                let edits =
                                    symbols |> Array.map (fun sym ->
                                        {
                                            Range = fcsRangeToLsp sym.RangeAlternate
                                            NewText = p.NewName
                                        }
                                    )
                                {
                                    TextDocument =
                                        {
                                            Uri = filePathToUri fileName
                                            // TODO: Maintain the version of all "in flight" documents for each TypeCheck
                                            Version = None
                                        }
                                    Edits = edits
                                }
                            )
                        WorkspaceEdit.Create(documentChanges, clientCapabilities.Value) |> Some |> success
                    | CoreResponse.SymbolUseRange uses ->
                        let documentChanges =
                            uses
                            |> Array.groupBy (fun sym -> sym.FileName)
                            |> Array.map(fun (fileName, symbols) ->
                                let edits =
                                    symbols |> Array.map (fun sym ->
                                        {
                                            Range = symbolUseRangeToLsp sym
                                            NewText = p.NewName
                                        }
                                    )
                                {
                                    TextDocument =
                                        {
                                            Uri = filePathToUri fileName
                                            // TODO: Maintain the version of all "in flight" documents for each TypeCheck
                                            Version = None
                                        }
                                    Edits = edits
                                }
                            )
                        WorkspaceEdit.Create(documentChanges, clientCapabilities.Value) |> Some |> success
                    | _ -> LspResult.notImplemented
                return res
            })

    override x.TextDocumentDefinition(p) =
        p |> x.positionHandler (fun p pos tyRes lineStr lines ->
            async {
                //TODO: Add #load reference
                let! res = commands.FindDeclaration tyRes pos lineStr
                let res =
                    match res.[0] with
                    | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                        LspResult.internalError msg
                    | CoreResponse.FindDeclaration r ->
                        findDeclToLspLocation r
                        |> GotoResult.Single
                        |> Some
                        |> success
                    | _ -> LspResult.notImplemented
                return res
            })

    override x.TextDocumentTypeDefinition(p) =
        p |> x.positionHandler (fun p pos tyRes lineStr lines ->
            async {
                let! res = commands.FindTypeDeclaration tyRes pos lineStr
                let res =
                    match res.[0] with
                    | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                        LspResult.internalError msg
                    | CoreResponse.FindTypeDeclaration r ->
                        fcsRangeToLspLocation r
                        |> GotoResult.Single
                        |> Some
                        |> success
                    | _ -> LspResult.notImplemented
                return res
            })

    override x.TextDocumentReferences(p) =
        p |> x.positionHandler (fun p pos tyRes lineStr lines ->
            async {
                let! res = commands.SymbolUseProject tyRes pos lineStr
                let res =
                    match res.[0] with
                    | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                        LspResult.internalError msg
                    | CoreResponse.SymbolUse (_, uses) ->
                        uses
                        |> Array.map (fun n -> fcsRangeToLspLocation n.RangeAlternate)
                        |> Some
                        |> success
                    | CoreResponse.SymbolUseRange uses ->
                        uses
                        |> Array.map symbolUseRangeToLspLocation
                        |> Some
                        |> success
                    | _ -> LspResult.notImplemented
                return res
            })

    override x.TextDocumentDocumentHighlight(p) =
        p |> x.positionHandler (fun p pos tyRes lineStr lines ->
            async {
                let! res = commands.SymbolUse tyRes pos lineStr
                let res =
                    match res.[0] with
                    | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                        LspResult.internalError msg
                    | CoreResponse.SymbolUse (symbol, uses) ->
                        uses
                        |> Array.map (fun s ->
                        {
                            DocumentHighlight.Range = fcsRangeToLsp s.RangeAlternate
                            Kind = None
                        })
                        |> Some
                        |> success
                    | _ -> LspResult.notImplemented
                return res
            })


    override __.TextDocumentDocumentSymbol(p) = async {
        if not commands.IsWorkspaceReady then
            do! WorkspaceReady |> Async.AwaitEvent
        let! res = commands.Declarations (p.TextDocument.GetFilePath()) None None
        let res =
            match res.[0] with
            | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                LspResult.internalError msg
            | CoreResponse.Declarations (decls) ->
                decls
                |> Array.map (fst >> getSymbolInformations p.TextDocument.Uri glyphToSymbolKind)
                |> Seq.collect id
                |> Seq.toArray
                |> Some
                |> success
            | _ -> LspResult.notImplemented
        return res
    }

    override __.WorkspaceSymbol(p) = async {
        let! res = commands.DeclarationsInProjects ()
        let res =
            match res.[0] with
            | CoreResponse.InfoRes msg | CoreResponse.ErrorRes msg ->
                LspResult.internalError msg
            | CoreResponse.Declarations (decls) ->
                decls
                |> Array.map (fun (n,p) ->
                    let uri = filePathToUri p
                    getSymbolInformations uri glyphToSymbolKind n)
                |> Seq.collect id
                |> Seq.filter(fun symbolInfo -> symbolInfo.Name.StartsWith(p.Query))
                |> Seq.toArray
                |> Some
                |> success
            | _ -> LspResult.notImplemented
        return res
    }

    override __.TextDocumentCodeAction(p) = async {
        let unusedOpenActions =
            if config.UnusedOpensAnalyzer then
                let diag =
                    p.Context.Diagnostics |> Seq.tryFind (fun n ->
                        n.Message = "Unused open statement"
                    )
                match diag with
                | None -> []
                | Some d ->
                    let e =
                        {
                            Range = {
                                Start = {Line = d.Range.Start.Line - 1; Character = 1000}
                                End = {Line = d.Range.End.Line; Character = d.Range.End.Character}
                            }
                            NewText = ""
                        }
                    let edit =
                        {
                            TextDocument =
                                {
                                    Uri = p.TextDocument.Uri
                                    // TODO: Maintain the version of all "in flight" documents for each TypeCheck
                                    Version = None
                                }
                            Edits = [|e|]
                        }
                    let we = WorkspaceEdit.Create([|edit|], clientCapabilities.Value)
                    let action =
                        { CodeAction.Title = "Remove unused open"
                          Kind = Some "quickfix"
                          Diagnostics = Some [| d |]
                          Edit = we
                          Command = None}

                    [action]
            else
                []

        let res =
            [|
                yield! unusedOpenActions
            |] |> TextDocumentCodeActionResult.CodeActions |> Some

        return success res
    }

let startCore (commands: Commands) =
    use input = Console.OpenStandardInput()
    use output = Console.OpenStandardOutput()

    LanguageServerProtocol.Server.start defaultRequestHandlings input output FSharpLspClient (fun lspClient -> FsharpLspServer(commands, lspClient))

let start (commands: Commands) (_args: ParseResults<Options.CLIArguments>) =
    // stdout is used for commands
    if Debug.output = stdout then
        Debug.output <- stderr

    // Debug.print "Starting LSP mode"

    try
        let result = startCore commands
        Debug.print "Ending LSP mode with %A" result
        int result
    with
    | ex ->
        Debug.print "LSP mode crashed with %A" ex
        3