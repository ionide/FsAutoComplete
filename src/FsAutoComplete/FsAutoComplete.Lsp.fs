module FsAutoComplete.Lsp

let private dbgf format = Printf.ksprintf (fun s -> System.Diagnostics.Trace.WriteLine(s)) format

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

let traceConfig () =
    Trace.Listeners.Clear()

    System.IO.File.WriteAllText(@"C:\temp\fsac.txt", "")
    let twtl = new TextWriterTraceListener(@"C:\temp\fsac.txt")
    twtl.Name <- "TextLogger"
    twtl.TraceOutputOptions <- TraceOptions.ThreadId ||| TraceOptions.DateTime

    Trace.Listeners.Add(twtl) |> ignore
    Trace.AutoFlush <- true

let protocolPosToPos (pos: LanguageServerProtocol.Protocol.Position): Pos =
    { Line = pos.Line + 1; Col = pos.Character + 1 }

let posToProtocolPos (pos: Pos): LanguageServerProtocol.Protocol.Position =
    { Line = pos.Line - 1; Character = pos.Col - 1 }

let fcsPosToLsp (pos: Microsoft.FSharp.Compiler.Range.pos): LanguageServerProtocol.Protocol.Position =
    { Line = pos.Line - 1; Character = pos.Column }

let fcsRangeToLsp(range: Microsoft.FSharp.Compiler.Range.range): LanguageServerProtocol.Protocol.Range =
    {
        Start = fcsPosToLsp range.Start
        End = fcsPosToLsp range.End
    }


type FSharpCompletionItemKind = Microsoft.FSharp.Compiler.SourceCodeServices.CompletionItemKind
type CompletionItemKind = LanguageServerProtocol.Protocol.General.CompletionItemKind

/// Compute the best possible CompletionItemKind for each FSharpGlyph according
// to the client capabilities
let glyphToCompletionKindGenerator (clientCapabilities: ClientCapabilities option) =
    let completionItemSet =
        clientCapabilities
        |> Option.bind(fun x -> x.TextDocument)
        |> Option.bind(fun x -> x.Completion)
        |> Option.bind(fun x -> x.CompletionItemKind)
        |> Option.bind(fun x -> x.ValueSet)
    let completionItemSet = defaultArg completionItemSet CompletionItemKindCapabilities.DefaultValueSet

    let bestAvailable (possible: CompletionItemKind[]) =
        let mutable found: CompletionItemKind option = None
        let mutable i = 0
        let possibleCount = possible.Length
        while found.IsNone && i < possibleCount do
            if Array.contains possible.[i] completionItemSet then
                found <- Some possible.[i]
            i <- i + 1
        found

    let getUncached code =
        match code with
        | FSharpGlyph.Class -> bestAvailable [| CompletionItemKind.Class |]
        | FSharpGlyph.Constant -> bestAvailable [| CompletionItemKind.Constant |]
        | FSharpGlyph.Delegate -> bestAvailable [| CompletionItemKind.Function |]
        | FSharpGlyph.Enum -> bestAvailable [| CompletionItemKind.Enum |]
        | FSharpGlyph.EnumMember -> bestAvailable [| CompletionItemKind.EnumMember; CompletionItemKind.Enum |]
        | FSharpGlyph.Event -> bestAvailable [| CompletionItemKind.Event |]
        | FSharpGlyph.Exception -> bestAvailable [| CompletionItemKind.Class |]
        | FSharpGlyph.Field -> bestAvailable [| CompletionItemKind.Field |]
        | FSharpGlyph.Interface -> bestAvailable [| CompletionItemKind.Interface; CompletionItemKind.Class |]
        | FSharpGlyph.Method -> bestAvailable [| CompletionItemKind.Method |]
        | FSharpGlyph.OverridenMethod-> bestAvailable [| CompletionItemKind.Method |]
        | FSharpGlyph.Module -> bestAvailable [| CompletionItemKind.Module; CompletionItemKind.Class |]
        | FSharpGlyph.NameSpace -> bestAvailable [| CompletionItemKind.Module |]
        | FSharpGlyph.Property -> bestAvailable [| CompletionItemKind.Property |]
        | FSharpGlyph.Struct -> bestAvailable [| CompletionItemKind.Struct; CompletionItemKind.Class |]
        | FSharpGlyph.Typedef -> bestAvailable [| CompletionItemKind.Class |]
        | FSharpGlyph.Type -> bestAvailable [| CompletionItemKind.Class |]
        | FSharpGlyph.Union -> bestAvailable [| CompletionItemKind.Class |]
        | FSharpGlyph.Variable -> bestAvailable [| CompletionItemKind.Variable |]
        | FSharpGlyph.ExtensionMethod -> bestAvailable [| CompletionItemKind.Method |]
        | FSharpGlyph.Error
        | _ -> None

    let unionCases = FSharpType.GetUnionCases(typeof<FSharpGlyph>)
    let cache = Dictionary<FSharpGlyph, CompletionItemKind option>(unionCases.Length)
    for info in unionCases do
        let glyph = FSharpValue.MakeUnion(info, [||]) :?> FSharpGlyph
        let completionItem = getUncached glyph
        cache.Add(glyph, completionItem)

    fun glyph ->
        cache.[glyph]

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

let errorToDiagnostic (error: FSharpErrorInfo) =
    {
        Range =
            {
                Start = { Line = error.StartLineAlternate - 1; Character = error.StartColumn }
                End = { Line = error.EndLineAlternate - 1; Character = error.EndColumn }
            }
        Severity = Some (match error.Severity with | FSharpErrorSeverity.Error  -> DiagnosticSeverity.Error | FSharpErrorSeverity.Warning -> DiagnosticSeverity.Warning)
        Source = Some (error.Subcategory)
        Message = error.Message
        Code = Some (DiagnosticCode.Number error.ErrorNumber)
    }

let filePathToUri (filePath: string) =
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

open LanguageServerProtocol
open LanguageServerProtocol.LspResult

type FsharpLspServer(commands: Commands, lspClient: LspClient) =
    inherit LspServer()

    let mutable clientCapabilities: ClientCapabilities option = None
    let mutable glyphToCompletionKind = glyphToCompletionKindGenerator None

    let parseAsync filePath (text: string) version = async {
        dbgf "[%s] Parse started" filePath
        let! resp = commands.ParseNoSerialize filePath (text.Split('\n')) version

        match resp with
        | ResultOrString.Error msg -> dbgf "[%s] Parse failed with %s" filePath msg
        | ResultOrString.Ok errors ->
            dbgf "[%s] Parse finished with success, reporting %d errors" filePath errors.Length
            let diagnostics = errors |> Array.map errorToDiagnostic
            do! lspClient.TextDocumentPublishDiagnostics({ Uri = filePathToUri filePath; Diagnostics = diagnostics })
    }

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

    override __.Initialize(p) = async {
        clientCapabilities <- p.Capabilities
        glyphToCompletionKind <- glyphToCompletionKindGenerator clientCapabilities

        match p.RootPath with
        | None -> ()
        | Some rootPath ->
            let projects = Directory.EnumerateFiles(rootPath, "*.fsproj", SearchOption.AllDirectories)
            dbgf "Loading projects: %A" projects
            let! response = commands.WorkspaceLoad ignore (List.ofSeq projects)
            dbgf "WorkspaceLoad result = %A" response
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
                dbgf "Parse not started, received partial change"
        | _ ->
            dbgf "Found no change for %s" filePath
            ()
    }

    override __.TextDocumentCompletion(p) = async {
        // Sublime-lsp doesn't like when we answer null so we answer an empty list instead
        let noCompletion = Result.Ok (Some { IsIncomplete = true; Items = [||] })
        let doc = p.TextDocument
        let file = Uri(doc.Uri).LocalPath
        match commands.TryGetFileCheckerOptionsWithLines file with
        | ResultOrString.Error s ->
            dbgf "Can't get filecheck options with lines: %s" s
            return noCompletion
        | ResultOrString.Ok (options, lines) ->
            let pos = protocolPosToPos p.Position
            let line = pos.Line
            let col = pos.Col
            let lineStr = lines.[line]
            let ok = line <= lines.Length && line >= 1 && col <= lineStr.Length + 1 && col >= 1
            if not ok then
                dbgf "Out of range"
                return noCompletion
            else
                let tyResOpt = commands.TryGetRecentTypeCheckResultsForFile(file, options)
                match tyResOpt with
                | None ->
                    dbgf "Cached typecheck results not yet available"
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
                        return Result.Ok (Some x)
                    | None ->
                        return noCompletion
    }

    override __.TextDocumentHover(p) = async {
        let uri = Uri(p.TextDocument.Uri)
        let pos = protocolPosToPos p.Position
        let filePath = uri.LocalPath

        dbgf "Hovering %s at %A" filePath pos

        match commands.TryGetFileCheckerOptionsWithLinesAndLineStr(filePath, pos) with
        | ResultOrString.Error s ->
            dbgf "TypeCheck error: %s" s
            return success None
        | ResultOrString.Ok (options, _lines, lineStr) ->
            // TODO: Should sometimes pass options.Source in here to force a reparse
            //       for completions e.g. `(some typed expr).$`
            let tyResOpt = commands.TryGetRecentTypeCheckResultsForFile(filePath, options)
            match tyResOpt with
            | None ->
                dbgf "No recent typecheck"
                return success None
            | Some tyRes ->
                let! tipResult = tyRes.TryGetToolTipEnhanced pos lineStr
                match tipResult with
                | Result.Error err ->
                    dbgf "Tooltip error: %s" err
                    return Result.Ok None
                | Result.Ok (tipText, _y, _z) ->
                    dbgf "Tootlip: %A" tipText
                    let s = tooltipToMarkdown tipText
                    dbgf "Tootlip: %A" s
                    let response =
                        {
                            Contents = MarkedString (StringAndLanguage { Language = "fsharp"; Value = s })
                            Range = None
                        }
                    return success (Some response)
    }

    override __.TextDocumentRename(p) = async {
        let uri = Uri(p.TextDocument.Uri)
        let filePath = uri.LocalPath
        let pos = protocolPosToPos p.Position
        dbgf "Rename for pos=%A" pos

        // TODO: How do we get a versioned answer ??? as we have the version
        match getRecentTypeCheckResultsForFile filePath with
        | Ok (_options, lines, tyRes) ->
            let lineStr = lines.[pos.Line-1]
            dbgf "Rename is for line=%s" lineStr
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
                    WorkspaceEdit.Create(documentChanges, clientCapabilities.Value) |> Some |> Result.Ok
            | Result.Error s ->
                return invalidParams (s.ToString())
        | Result.Error s ->
            return invalidParams (s.ToString())
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
    traceConfig()
    dbgf "Starting"

    try
        startCore commands
    with
    | ex -> dbgf "LSP failed with %A" ex
    ()
