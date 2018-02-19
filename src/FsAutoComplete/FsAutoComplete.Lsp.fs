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

let traceConfig () =
    Trace.Listeners.Clear()

    System.IO.File.WriteAllText(@"C:\temp\fsac.txt", "")
    let twtl = new TextWriterTraceListener(@"C:\temp\fsac.txt")
    twtl.Name <- "TextLogger"
    twtl.TraceOutputOptions <- TraceOptions.ThreadId ||| TraceOptions.DateTime

    Trace.Listeners.Add(twtl) |> ignore
    Trace.AutoFlush <- true

let protocolPosToPos (pos: LanguageServerProtocol.Protocol.Position): Pos = { Line = pos.Line + 1; Col = pos.Character + 1 }

let start (commands: Commands) (_args: ParseResults<Options.CLIArguments>) =
    traceConfig()

    use input = Console.OpenStandardInput()
    use output = Console.OpenStandardOutput()

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

    let handleClientRequest (sendToClient: RequestSender) (request: ClientRequest) = async {
        match request with
        | Initialize p ->
            sendToClient (ShowMessage { Type = MessageType.Info; Message = "Hello world" })

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
                                         Save = Some { IncludeText = Some true } } } }
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
