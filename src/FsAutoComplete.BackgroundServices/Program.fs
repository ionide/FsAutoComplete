namespace FsAutoComplete.BackgroundServices

open System
open System.IO
open System.Text
open LanguageServerProtocol.Server
open LanguageServerProtocol.Types
open LanguageServerProtocol
open FSharp.Compiler
open FSharp.Compiler.Text
open FSharp.Compiler.SourceCodeServices
open System.Collections.Concurrent
open FsAutoComplete

type UpdateFileParms = {
    File: string
    Content: string
    Version: int
}

type ProjectParms = {
    Options: FSharpProjectOptions
    File: string
}

type FileParms = {
    File: string
}

type Msg = {Value: string}

type State ={
    Files : ConcurrentDictionary<SourceFilePath, VolatileFile>
    FileCheckOptions : ConcurrentDictionary<SourceFilePath, FSharpProjectOptions>
}
with
    static member Initial =
        { Files = ConcurrentDictionary(); FileCheckOptions = ConcurrentDictionary() }

module Helpers =
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




type FsacClient(sendServerRequest: ClientNotificationSender) =
    inherit LspClient ()

    member __.SendDiagnostics(p: PublishDiagnosticsParams) =
        sendServerRequest "background/diagnostics" (box p) |> Async.Ignore

    member __.Notifiy(o: Msg) =
        sendServerRequest "background/notify" o |> Async.Ignore

type BackgroundServiceServer(state: State, client: FsacClient) =
    inherit LspServer()

    let checker = FSharpChecker.Create(projectCacheSize = 200, keepAllBackgroundResolutions = false)

    do checker.ImplicitlyStartBackgroundWork <- false

    let getListOfFilesForProjectChecking file =
        match state.FileCheckOptions.TryFind file with
        | None -> []
        | Some opts ->
            let sf = opts.OtherOptions |> Seq.where (fun n -> not (n.StartsWith "-") && (n.EndsWith ".fs" || n.EndsWith ".fsi") ) |> Seq.toArray
            sf
            |> Array.skipWhile (fun n -> (Utils.normalizePath n) <> (Utils.normalizePath file))
            |> Array.map (fun n -> (Utils.normalizePath n))
            |> Array.toList

    let typecheckFile ignoredFile file =
        async {
            do! client.Notifiy {Value = sprintf "Typechecking %s" file }
            match state.Files.TryFind file, state.FileCheckOptions.TryFind file with
            | Some vf, Some opts ->
                let txt = vf.Lines |> String.concat "\n"
                let! pr, cr = checker.ParseAndCheckFileInProject(file, defaultArg vf.Version 0, SourceText.ofString txt, opts)
                match cr with
                | FSharpCheckFileAnswer.Aborted ->
                    do! client.Notifiy {Value = sprintf "Typechecking aborted %s" file }
                    return ()
                | FSharpCheckFileAnswer.Succeeded res ->
                    match ignoredFile with
                    | Some fn when fn = file -> return ()
                    | _ ->
                        let errors = Array.append pr.Errors res.Errors |> Array.map (Helpers.fcsErrorToDiagnostic)
                        let msg = {Diagnostics = errors; Uri = Helpers.filePathToUri file}
                        do! client.SendDiagnostics msg
                        async {
                            let! symbols = res.GetAllUsesOfAllSymbolsInFile()
                            SymbolCache.updateSymbols file symbols
                            return ()
                        } |> Async.Start

                        return ()
            | _ ->
                do! client.Notifiy {Value = sprintf "Couldn't find state %s" file }
                return ()
        }

    let getDependingProjects (file: FilePath) =
        let project = state.FileCheckOptions.TryFind file
        match project with
        | None -> []
        | Some s ->
            state.FileCheckOptions
            |> Seq.map (fun kv -> kv.Value)
            |> Seq.distinctBy (fun o -> o.ProjectFileName)
            |> Seq.filter (fun o ->
                o.ReferencedProjects
                |> Array.map (fun (_,v) -> Path.GetFullPath v.ProjectFileName)
                |> Array.contains s.ProjectFileName )
            |> Seq.toList




    let reactor = MailboxProcessor.Start(fun agent ->
        let rec recieveLast last =
            async {
                let! msg = agent.TryReceive(5)
                match msg with
                | Some s ->
                    return! recieveLast (Some s)
                | None ->
                    return last
            }

        let rec loop (isFromSave,lst) = async {
            let! msg = recieveLast None
            match msg, lst with
            //Empty
            | None, [] ->
                do! Async.Sleep 300
                return! loop (false, [])
            //Empty
            | Some (_,_,[]), [] ->
                do! Async.Sleep 300
                return! loop (false, [])
            //No request we just continue
            | None, x::xs ->
                do! typecheckFile None x
                return! loop (isFromSave, xs)

            //We've ended processing request we start new
            | Some(saveRequest,fn, x::xs), [] ->
                do! typecheckFile (Some fn) x
                return! loop (saveRequest, xs)

            //If incoming is normal update and current is from save request we continue current
            | Some (false,_,_), x::xs when isFromSave  ->
                do! typecheckFile None x
                return! loop (isFromSave, xs)

            //If incoming is normal and previous was normal
            | Some (false,fn,(x::xs)), _ ->
                do! typecheckFile (Some fn) x
                return! loop (false, xs)

            //If incoming request is from save we always start it
            | Some(true, fn, (x::xs)), _ ->
                do! typecheckFile (Some fn) x
                return! loop (true, xs)

            //If incoming request doesn't contain any list we just continue previous one
            | Some (_,fn,[]), x::xs ->
                do! typecheckFile (Some fn) x
                return! loop (isFromSave, xs)
        }

        loop (false, [])
    )

    let bouncer = Debounce(500, reactor.Post)


    member __.UpdateTextFile(p: UpdateFileParms) =
        async {
            do! client.Notifiy {Value = sprintf "File update %s" p.File }
            let file = Utils.normalizePath p.File
            let vf = {Lines = p.Content.Split( [|'\n' |] ); Touched = DateTime.Now; Version = Some p.Version  }
            state.Files.AddOrUpdate(file, (fun _ -> vf),( fun _ _ -> vf) ) |> ignore
            let filesToCheck = getListOfFilesForProjectChecking file
            bouncer.Bounce (false, file,filesToCheck)
            return LspResult.success ()
        }

    member __.UpdateProject(p: ProjectParms) =
        async {


            let sf = p.Options.OtherOptions |> Seq.where (fun n -> not (n.StartsWith "-") && (n.EndsWith ".fs" || n.EndsWith ".fsi") ) |> Seq.toArray

            sf
            |> Seq.iter (fun f ->
                let file = Utils.normalizePath f
                state.FileCheckOptions.AddOrUpdate(file, (fun _ -> p.Options), (fun _ _ -> p.Options))
                |> ignore
            )
            do! client.Notifiy {Value = sprintf "Project Updated %s" p.Options.ProjectFileName}
            return LspResult.success ()
        }

    member __.FileSaved(p: FileParms) =
        async {
            let file = Utils.normalizePath p.File

            do! client.Notifiy {Value = sprintf "File Saved %s " file }

            let projects = getDependingProjects file
            let filesToCheck =
                [
                    yield! getListOfFilesForProjectChecking file
                    yield! projects |> Seq.collect (fun o ->
                        o.OtherOptions |> Seq.where (fun n -> not (n.StartsWith "-") && (n.EndsWith ".fs" || n.EndsWith ".fsi") ) |> Seq.toArray
                    )
                ]
            bouncer.Bounce (true, file,filesToCheck)
            return LspResult.success ()
        }

module Program =

    let state = State.Initial

    let startCore () =
        use input = Console.OpenStandardInput()
        use output = Console.OpenStandardOutput()

        let requestsHandlings =
            Map.empty<string, RequestHandling<BackgroundServiceServer>>
            |> Map.add "background/update" (requestHandling (fun s p -> s.UpdateTextFile(p) ))
            |> Map.add "background/project" (requestHandling (fun s p -> s.UpdateProject(p) ))
            |> Map.add "background/save" (requestHandling (fun s p -> s.FileSaved(p) ))

        LanguageServerProtocol.Server.start requestsHandlings input output FsacClient (fun lspClient -> BackgroundServiceServer(state, lspClient))



    [<EntryPoint>]
    let main argv =
        let pid = Int32.Parse argv.[0]
        let originalFs = AbstractIL.Internal.Library.Shim.FileSystem
        let fs = FileSystem(originalFs, state.Files.TryFind)
        AbstractIL.Internal.Library.Shim.FileSystem <- fs
        ProcessWatcher.zombieCheckWithHostPID (fun () -> exit 0) pid
        let _ = startCore()
        0
