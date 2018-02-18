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

let protocolPosToPos (pos: LanguageServerProtocol.Protocol.Position): Pos = { Line = pos.Line + 1; Col = pos.Character }

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

    let f (sendToClient: RequestSender) =
        function
        | Initialize p ->
            sendToClient (ShowMessage { Type = MessageType.Info; Message = "Hello world" })

            match p.RootPath with
            | None -> ()
            | Some rootPath ->
                let projects = Directory.EnumerateFiles(rootPath, "*.fsproj", SearchOption.AllDirectories)
                dbgf "Loading projects: %A" projects
                let response = commands.WorkspaceLoad ignore (List.ofSeq projects) |> Async.RunSynchronously
                dbgf "WorkspaceLoad result = %A" response
                ()
            ()

            InitializeResponse
                { InitializeResult.Default with
                    Capabilities =
                        { ServerCapabilities.Default with
                            HoverProvider = Some true
                            TextDocumentSync =
                                Some { TextDocumentSyncOptions.Default with
                                         OpenClose = Some true
                                         Change = Some TextDocumentSyncKind.Incremental
                                         Save = Some { IncludeText = Some true } } } }
        | DidOpenTextDocument documentParams ->
            let doc = documentParams.TextDocument
            let filePath = Uri(doc.Uri).LocalPath
            async
                {
                    dbgf "Parse started"
                    let! resp = commands.Parse filePath (doc.Text.Split('\n')) doc.Version
                    dbgf "Parse finished with %A" resp
                } |> Async.StartAsTask |> ignore
            NoResponse
        | Hover posParams ->
            let uri = Uri(posParams.TextDocument.Uri)
            let pos = protocolPosToPos posParams.Position
            let filePath = uri.LocalPath

            dbgf "Hovering %s at %A" filePath pos

            match commands.TryGetFileCheckerOptionsWithLinesAndLineStr(filePath, pos) with
            | ResultOrString.Error s ->
                dbgf "TypeCheck error: %s" s
                HoverResponse None
            | ResultOrString.Ok (options, lines, lineStr) ->
                // TODO: Should sometimes pass options.Source in here to force a reparse
                //       for completions e.g. `(some typed expr).$`
                let tyResOpt = commands.TryGetRecentTypeCheckResultsForFile(filePath, options)
                match tyResOpt with
                | None ->
                    dbgf "No recent typecheck"
                    HoverResponse None
                | Some tyRes ->
                    match tyRes.TryGetToolTipEnhanced pos lineStr |> Async.RunSynchronously with
                    | Result.Error err ->
                        dbgf "Tooltip error: %s" err
                        HoverResponse None
                    | Result.Ok (tipText, y, z) ->
                        let s = tooltipToMarkdown tipText
                        dbgf "Tootlip: %A" s
                        HoverResponse (Some { Contents = markdown s; Range = None})
        | Exit ->
            Environment.Exit(0)
            NoResponse
        | x when x.IsNotification -> NoResponse
        | _ -> UnhandledRequest

    LanguageServerProtocol.Server.start input output f
    ()
