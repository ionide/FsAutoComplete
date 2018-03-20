module FsAutoComplete.Lsp

/// Write a trace to System.Diagnostics.Trace
let private tracefn format =
    Debug.print format

open Argu
open System
open System.IO
open LanguageServerProtocol.Server
open LanguageServerProtocol.Protocol
open System.Diagnostics
open FsAutoComplete.Utils
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Reflection
open System.Collections.Generic
open System.Text

[<AutoOpen>]
module private Conversions =
    module Lsp = LanguageServerProtocol.Protocol

    let protocolPosToPos (pos: Lsp.Position): Pos =
        { Line = pos.Line + 1; Col = pos.Character + 1 }

    let posToProtocolPos (pos: Pos): Lsp.Position =
        { Line = pos.Line - 1; Character = pos.Col - 1 }

    let fcsPosToLsp (pos: Microsoft.FSharp.Compiler.Range.pos): Lsp.Position =
        { Line = pos.Line - 1; Character = pos.Column }

    let fcsRangeToLsp(range: Microsoft.FSharp.Compiler.Range.range): Lsp.Range =
        {
            Start = fcsPosToLsp range.Start
            End = fcsPosToLsp range.End
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
            "file:///" + uri.ToString()

    let fcsRangeToLspLocation(range: Microsoft.FSharp.Compiler.Range.range): Lsp.Location =
        let fileUri = filePathToUri range.FileName
        let lspRange = fcsRangeToLsp range
        {
            Uri = fileUri
            Range = lspRange
        }

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
            Source = Some (error.Subcategory)
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

    type CompletionItemKind = LanguageServerProtocol.Protocol.CompletionItemKind

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

let tooltipElementToMarkdown (tip: FSharpToolTipElement<string>) =
    match tip with
    | FSharpToolTipElement.None -> ""
    | FSharpToolTipElement.Group lst ->
        match lst with
        | data :: _ ->
            data.MainDescription
        | [] -> ""
    | FSharpToolTipElement.CompositionError err -> "ERR " + err

let tooltipToMarkdown (tip: FSharpToolTipText<string>) =
    let (FSharpToolTipText elements) = tip
    let elementsLines = elements |> List.map tooltipElementToMarkdown
    System.String.Join("\n", elementsLines)

open LanguageServerProtocol
open LanguageServerProtocol.LspResult
open FsAutoComplete.CommandResponse

type FsharpLspServer(commands: Commands, lspClient: LspClient) =
    inherit LspServer()

    let mutable clientCapabilities: ClientCapabilities option = None
    let mutable glyphToCompletionKind = glyphToCompletionKindGenerator None
    let mutable glyphToSymbolKind = glyphToSymbolKindGenerator None

    let parseAsync filePath (text: string) version = async {
        tracefn "[%s] Parse started" filePath
        let! resp = commands.ParseNoSerialize filePath (text.Split('\n')) version

        match resp with
        | ResultOrString.Error msg -> tracefn "[%s] Parse failed with %s" filePath msg
        | ResultOrString.Ok errors ->
            tracefn "[%s] Parse finished with success, reporting %d errors" filePath errors.Length
            let diagnostics = errors |> Array.map fcsErrorToDiagnostic
            do! lspClient.TextDocumentPublishDiagnostics({ Uri = filePathToUri filePath; Diagnostics = diagnostics })
    }

    let getRecentTypeCheckResultsForFile file =
        match commands.TryGetFileCheckerOptionsWithLines file with
        | ResultOrString.Error s ->
            Result.Error (sprintf "Can't get filecheck options with lines: %s" s)
        | ResultOrString.Ok (options, lines) ->
            let tyResOpt = commands.Checker.TryGetRecentCheckResultsForFile(file, options)
            match tyResOpt with
            | None ->
                Result.Error "Cached typecheck results not yet available"
            | Some tyRes ->
                Ok (options, lines, tyRes)

    override __.Initialize(p) = async {
        clientCapabilities <- p.Capabilities
        glyphToCompletionKind <- glyphToCompletionKindGenerator clientCapabilities
        glyphToSymbolKind <- glyphToSymbolKindGenerator clientCapabilities

        match p.RootPath with
        | None -> ()
        | Some rootPath ->
            let projects = Directory.EnumerateFiles(rootPath, "*.fsproj", SearchOption.AllDirectories)
            tracefn "Loading projects: %A" projects
            let! response = commands.WorkspaceLoad ignore (List.ofSeq projects)
            tracefn "WorkspaceLoad result = %A" response
            ()

            // TODO
(*
            commands.Checker.FileChecked.Add (fun (filePath, _) ->
                dbgf "File checked A %s" filePath
                dbgf "KEYS = %A" (commands.FileCheckOptions.Keys |> List.ofSeq)
                let filePath = filePath.Replace("G:\\", "g:\\")
                async {
                    try
                        let opts = commands.GetFileCheckOptions(filePath)
                        let! res = commands.Checker.GetBackgroundCheckResultsForFileInProject(filePath, opts)
                        let checkErrors = res.GetCheckResults.Errors
                        let parseErrors = res.GetParseResults.Errors
                        let errors = Array.append checkErrors parseErrors

                        let diagnostics = errors |> Array.map errorToDiagnostic
                        sendToClient (PublishDiagnostics { Uri = filePathToUri filePath; Diagnostics = diagnostics })
                    with
                    | ex ->
                        dbgf "Error: %A" ex
                        ()
                }
                |> Async.Start
            )
            *)
            (*
            commands.FileInProjectChecked
                |> Observable.add(fun filePath ->
                    dbgf "File checked B %s" filePath
                    async {
                        try
                            let opts = commands.GetFileCheckOptions(filePath)
                            let! res = commands.Checker.GetBackgroundCheckResultsForFileInProject(filePath, opts)
                            let checkErrors = res.GetCheckResults.Errors
                            let parseErrors = res.GetParseResults.Errors
                            let errors = Array.append checkErrors parseErrors

                            let diagnostics = errors |> Array.map errorToDiagnostic
                            sendToClient (PublishDiagnostics { Uri = filePathToUri filePath; Diagnostics = diagnostics })
                        with
                        | ex ->
                            dbgf "Error: %A" ex
                            ()
                    }
                    |> Async.Start
                )
*)

        return
            { InitializeResult.Default with
                Capabilities =
                    { ServerCapabilities.Default with
                        HoverProvider = Some true
                        RenameProvider = Some true
                        DefinitionProvider = Some true
                        ReferencesProvider = Some true
                        DocumentHighlightProvider = Some true
                        DocumentSymbolProvider = Some true
                        WorkspaceSymbolProvider = Some true
                        TextDocumentSync =
                            Some { TextDocumentSyncOptions.Default with
                                     OpenClose = Some true
                                     Change = Some TextDocumentSyncKind.Full
                                     Save = Some { IncludeText = Some true }
                                 }
                        CompletionProvider =
                            Some {
                                ResolveProvider = Some false
                                TriggerCharacters = Some ([| "."; "'"; "," |])
                            }
                    }
            }
            |> success
    }

    override __.TextDocumentDidOpen(p) = async {
        let doc = p.TextDocument
        let filePath = Uri(doc.Uri).LocalPath

        do! parseAsync filePath doc.Text doc.Version
    }

    override __.TextDocumentDidChange(p) = async {
        let doc = p.TextDocument
        let filePath = Uri(doc.Uri).LocalPath
        let contentChange = p.ContentChanges |> Seq.tryLast
        match contentChange, doc.Version with
        | Some contentChange, Some version ->
            if contentChange.Range.IsNone && contentChange.RangeLength.IsNone then
                do! parseAsync filePath contentChange.Text version
            else
                tracefn "Parse not started, received partial change"
        | _ ->
            tracefn "Found no change for %s" filePath
            ()
    }

    override __.TextDocumentCompletion(p) = async {
        // Sublime-lsp doesn't like when we answer null so we answer an empty list instead
        let noCompletion = success (Some { IsIncomplete = true; Items = [||] })
        let doc = p.TextDocument
        let file = Uri(doc.Uri).LocalPath
        match commands.TryGetFileCheckerOptionsWithLines file with
        | ResultOrString.Error s ->
            tracefn "Can't get filecheck options with lines: %s" s
            return noCompletion
        | ResultOrString.Ok (options, lines) ->
            let pos = protocolPosToPos p.Position
            let line = pos.Line
            let col = pos.Col
            let lineStr = lines.[line]
            let ok = line <= lines.Length && line >= 1 && col <= lineStr.Length + 1 && col >= 1
            if not ok then
                tracefn "Out of range"
                return noCompletion
            else
                let tyResOpt = commands.TryGetRecentTypeCheckResultsForFile(file, options)
                match tyResOpt with
                | None ->
                    tracefn "Cached typecheck results not yet available"
                    return noCompletion
                | Some tyRes ->
                    let getAllSymbols () = tyRes.GetAllEntities()
                    let! res = tyRes.TryGetCompletions pos lineStr (Some "StartsWith") getAllSymbols
                    match res with
                    | Some (decls, _residue) ->
                        let items =
                            decls
                            |> Array.map (fun d ->
                                { CompletionItem.Create(d.Name) with
                                    Kind = glyphToCompletionKind d.Glyph
                                }
                            )
                        let x = { IsIncomplete = false; Items = items}
                        return success (Some x)
                    | None ->
                        return noCompletion
    }

    override __.TextDocumentHover(p) = async {
        let pos = p.GetFcsPos()
        let filePath = p.GetFilePath()

        tracefn "Hovering %s at %A" filePath pos

        match commands.TryGetFileCheckerOptionsWithLinesAndLineStr(filePath, pos) with
        | ResultOrString.Error s ->
            tracefn "TypeCheck error: %s" s
            return success None
        | ResultOrString.Ok (options, _lines, lineStr) ->
            // TODO: Should sometimes pass options.Source in here to force a reparse
            //       for completions e.g. `(some typed expr).$`
            let tyResOpt = commands.TryGetRecentTypeCheckResultsForFile(filePath, options)
            match tyResOpt with
            | None ->
                tracefn "No recent typecheck"
                return success None
            | Some tyRes ->
                let! tipResult = tyRes.TryGetToolTipEnhanced pos lineStr
                let! helpResult = commands.Help tyRes pos lineStr
                match tipResult with
                | Result.Error err ->
                    tracefn "Tooltip error: %s" err
                    return success None
                | Result.Ok (tip, _signature, _footer) ->
                    tracefn "Tootlip: %A" tip
                    let s = tooltipToMarkdown tip
                    tracefn "Tootlip: %A" s
                    let response =
                        {
                            Contents =
                                MarkedStrings (Array.append [| MarkedString.WithLanguage { Language = "fsharp"; Value = s } |] (helpResult |> Array.ofList |> Array.map MarkedString.String))
                            Range = None
                        }
                    return success (Some response)
    }

    override __.TextDocumentRename(p) = async {
        let pos = p.GetFcsPos()
        let filePath = p.GetFilePath()
        tracefn "Rename for pos=%A" pos

        // TODO: How do we get a versioned answer ??? as we have the version
        match getRecentTypeCheckResultsForFile filePath with
        | Ok (_options, lines, tyRes) ->
            let lineStr = lines.[pos.Line-1]
            tracefn "Rename is for line=%s" lineStr
            let! symbolUse = commands.SymbolUseProjectNotSerialized tyRes pos lineStr
            match symbolUse with
            | Ok (_sym, symbols) ->
                let documentChanges =
                    symbols
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

                return
                    WorkspaceEdit.Create(documentChanges, clientCapabilities.Value) |> Some |> success
            | Result.Error s ->
                return invalidParams (s.ToString())
        | Result.Error s ->
            return invalidParams (s.ToString())
    }

    override __.TextDocumentDefinition(p) = async {
        let pos = p.GetFcsPos()
        let filePath = p.GetFilePath()

        match getRecentTypeCheckResultsForFile filePath with
        | Ok (_options, lines, tyRes) ->
            let lineStr = lines.[pos.Line-1]
            let! declarationResult = tyRes.TryFindDeclaration pos lineStr
            match declarationResult with
            | Ok range ->
                let location = fcsRangeToLspLocation range
                return success (Some (GotoResult.Single location))
            | Result.Error s ->
                return invalidParams s
        | Result.Error s ->
            return invalidParams s
    }

    override __.TextDocumentReferences(p) = async {
        let pos = p.GetFcsPos()
        let filePath = p.GetFilePath()

        match getRecentTypeCheckResultsForFile filePath with
        | Ok (_options, lines, tyRes) ->
            let lineStr = lines.[pos.Line-1]
            // Copy-paste of Commands.SymbolUseProject
            let! symbolUseResult = tyRes.TryGetSymbolUse pos lineStr
            match symbolUseResult with
            | Ok (symbolUse, usages) ->
                let! symbols = async {
                    if symbolUse.Symbol.IsPrivateToFile then
                        return usages
                    elif symbolUse.Symbol.IsInternalToProject then
                        let opts = commands.FileCheckOptions.[tyRes.FileName]
                        return! commands.Checker.GetUsesOfSymbol (tyRes.FileName, [tyRes.FileName, opts] , symbolUse.Symbol)
                    else
                        return! commands.Checker.GetUsesOfSymbol (tyRes.FileName, commands.FileCheckOptions.ToArray() |> Array.map (fun (KeyValue(k, v)) -> k,v) |> Seq.ofArray, symbolUse.Symbol)
                }

                let finalSymbols =
                    if p.Context.IncludeDeclaration then
                        Array.append [| symbolUse |] symbols
                    else
                        symbols

                return
                    finalSymbols
                    |> Array.map(fun s -> fcsRangeToLspLocation s.RangeAlternate)
                    |> Some
                    |> success
            | Result.Error s ->
                return invalidParams s
        | Result.Error s ->
            return invalidParams s
    }

    override __.TextDocumentDocumentHighlight(p) = async {
        let pos = p.GetFcsPos()
        let filePath = p.GetFilePath()

        match getRecentTypeCheckResultsForFile filePath with
        | Ok (_options, lines, tyRes) ->
            let lineStr = lines.[pos.Line-1]
            let! symbolUseResult = tyRes.TryGetSymbolUse pos lineStr
            match symbolUseResult with
            | Ok (symbolUse, usages) ->
                let finalSymbols = Array.append [| symbolUse |] usages

                // TODO: Add fuzzy matches
                // TODO: Get if it's a read or a write usage
                return
                    finalSymbols
                    |> Array.map(fun s -> { DocumentHighlight.Range = fcsRangeToLsp s.RangeAlternate; Kind = None })
                    |> Some
                    |> success

            | Result.Error s ->
                return invalidParams s
        | Result.Error s ->
            return invalidParams s
    }

    override __.TextDocumentDocumentSymbol(p) = async {
        let filePath = p.TextDocument.GetFilePath()
        match getRecentTypeCheckResultsForFile filePath with
        | Ok (_options, _lines, tyRes) ->
            let declarations = tyRes.GetParseResults.GetNavigationItems().Declarations

            let symbols =
                declarations
                |> Seq.ofArray
                |> Seq.collect(getSymbolInformations p.TextDocument.Uri glyphToSymbolKind)
                |> Array.ofSeq

            return symbols |> Some |> success
        | Result.Error s ->
            return invalidParams s
    }

    override __.WorkspaceSymbol(p) = async {
        let symbols =
            commands.NavigationDeclarations.ToArray()
            |> Seq.ofArray
            |> Seq.collect (fun (KeyValue(filePath, decls)) ->
                let uri = filePathToUri filePath
                decls
                |> Seq.ofArray
                |> Seq.collect(getSymbolInformations uri glyphToSymbolKind)
                |> Seq.filter(fun symbolInfo -> symbolInfo.Name.StartsWith(p.Query))
                )
            |> Array.ofSeq

        return symbols |> Some |> success
    }

    override __.Exit() = async {
        Environment.Exit(0)
    }

let startCore (commands: Commands) =
    use input = Console.OpenStandardInput()
    use output = Console.OpenStandardOutput()

    LanguageServerProtocol.Server.start input output (fun lspClient -> FsharpLspServer(commands, lspClient))
    ()

let start (commands: Commands) (_args: ParseResults<Options.CLIArguments>) =
    // stdout is used for commands
    if Debug.output = stdout then
        Debug.output <- stderr

    tracefn "Starting"

    try
        startCore commands
    with
    | ex -> tracefn "LSP failed with %A" ex

    0
