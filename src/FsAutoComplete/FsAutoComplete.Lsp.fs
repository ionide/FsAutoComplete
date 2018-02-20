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

let startCore (commands: Commands) =
    use input = Console.OpenStandardInput()
    use output = Console.OpenStandardOutput()

    let mutable clientCapabilities: ClientCapabilities option = None
    let mutable glyphToCompletionKind = glyphToCompletionKindGenerator None

    let handleClientRequest (_sendToClient: RequestSender) (request: ClientRequest) = async {
        match request with
        | Initialize p ->
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
            ()

            return InitializeResponse
                { InitializeResult.Default with
                    Capabilities =
                        { ServerCapabilities.Default with
                            HoverProvider = Some true
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
        | DidOpenTextDocument documentParams ->
            let doc = documentParams.TextDocument
            let filePath = Uri(doc.Uri).LocalPath

            dbgf "Parse started for %s" filePath
            let! resp = commands.Parse filePath (doc.Text.Split('\n')) doc.Version
            dbgf "Parse finished with %A" resp

            return NoResponse
        | DidChangeTextDocument changeParams ->
            let doc = changeParams.TextDocument
            let filePath = Uri(doc.Uri).LocalPath
            let contentChange = changeParams.ContentChanges |> Seq.tryLast
            match contentChange, doc.Version with
            | Some contentChange, Some version ->
                if contentChange.Range.IsNone && contentChange.RangeLength.IsNone then
                    dbgf "Parse started for %s" filePath
                    let! resp = commands.Parse filePath (contentChange.Text.Split('\n')) version
                    dbgf "Parse finished with %A" resp
                else
                    dbgf "Parse started"
            | _ ->
                dbgf "Found no change for %s" filePath
                ()
            return NoResponse
        | Completion completionParams ->
            // Sublime-lsp doesn't like when we answer null so we answer an empty list instead
            let noCompletion = CompletionResponse (Some { IsIncomplete = true; Items = [||] })
            let doc = completionParams.TextDocument
            let file = Uri(doc.Uri).LocalPath
            match commands.TryGetFileCheckerOptionsWithLines file with
            | ResultOrString.Error s ->
                dbgf "Can't get filecheck options with lines: %s" s
                return noCompletion
            | ResultOrString.Ok (options, lines) ->
                let pos = protocolPosToPos completionParams.Position
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
                            return CompletionResponse (Some x)
                        | None ->
                            return noCompletion
        | Hover posParams ->
            let uri = Uri(posParams.TextDocument.Uri)
            let pos = protocolPosToPos posParams.Position
            let filePath = uri.LocalPath

            dbgf "Hovering %s at %A" filePath pos

            match commands.TryGetFileCheckerOptionsWithLinesAndLineStr(filePath, pos) with
            | ResultOrString.Error s ->
                dbgf "TypeCheck error: %s" s
                return HoverResponse None
            | ResultOrString.Ok (options, _lines, lineStr) ->
                // TODO: Should sometimes pass options.Source in here to force a reparse
                //       for completions e.g. `(some typed expr).$`
                let tyResOpt = commands.TryGetRecentTypeCheckResultsForFile(filePath, options)
                match tyResOpt with
                | None ->
                    dbgf "No recent typecheck"
                    return HoverResponse None
                | Some tyRes ->
                    let! tipResult = tyRes.TryGetToolTipEnhanced pos lineStr
                    match tipResult with
                    | Result.Error err ->
                        dbgf "Tooltip error: %s" err
                        return HoverResponse None
                    | Result.Ok (tipText, _y, _z) ->
                        dbgf "Tootlip: %A" tipText
                        let s = tooltipToMarkdown tipText
                        dbgf "Tootlip: %A" s
                        return HoverResponse (Some { Contents = MarkedString (StringAndLanguage { Language = "fsharp"; Value = s }); Range = None })
        | Exit ->
            Environment.Exit(0)
            return NoResponse
        | x when x.IsNotification -> return NoResponse
        | _ -> return UnhandledRequest
    }

    LanguageServerProtocol.Server.start input output handleClientRequest
    ()

let start (commands: Commands) (_args: ParseResults<Options.CLIArguments>) =
    traceConfig()
    dbgf "Starting"

    try
        startCore commands
    with
    | ex -> dbgf "LSP failed with %A" ex
    ()
