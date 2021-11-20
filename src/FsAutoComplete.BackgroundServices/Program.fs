namespace FsAutoComplete.BackgroundServices

open System
open System.IO
open System.Text
open LanguageServerProtocol.Server
open LanguageServerProtocol.Types
open LanguageServerProtocol
open FSharp.Compiler
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Text
open FSharp.Compiler.CodeAnalysis
open System.Collections.Concurrent
open FsAutoComplete
open Ionide.ProjInfo.ProjectSystem
open FSharp.UMX
open System.Reactive.Linq
open FSharp.Compiler.Text

type BackgroundFileCheckType =
| SourceFile of filePath: string
| ScriptFile of filePath: string * tfm: FSIRefs.TFM
with
    member x.FilePath =
        match x with
        | SourceFile(path)
        | ScriptFile(path, _) -> path


type UpdateFileParms = {
    File: BackgroundFileCheckType
    Content: string
    Version: int
}

type ProjectParms = {
    Options: FSharpProjectOptions
    ReferencedProjects: string array
    File: string
}

type FileParms = {
    File: BackgroundFileCheckType
}

type Msg = {Value: string}

type State = {
    Files : ConcurrentDictionary<string<LocalPath>, VolatileFile>
    FileCheckOptions : ConcurrentDictionary<string<LocalPath>, FSharpProjectOptions>
}

with
    static member Initial =
        { Files = ConcurrentDictionary(); FileCheckOptions = ConcurrentDictionary() }

module Helpers =
    let fcsSeverityToDiagnostic = function
        | FSharpDiagnosticSeverity.Error -> Some DiagnosticSeverity.Error
        | FSharpDiagnosticSeverity.Warning -> Some DiagnosticSeverity.Warning
        | FSharpDiagnosticSeverity.Hidden -> None
        | FSharpDiagnosticSeverity.Info -> Some DiagnosticSeverity.Information

    let urlForCompilerCode (number: int) =
      $"https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/compiler-messages/fs%04d{number}"

    let fcsErrorToDiagnostic (error: FSharpDiagnostic) =
        {
            Range =
                {
                    Start = { Line = error.StartLine - 1; Character = error.StartColumn }
                    End = { Line = error.StartLine - 1; Character = error.EndColumn }
                }
            Severity = fcsSeverityToDiagnostic error.Severity
            Source = "F# Compiler"
            Message = error.Message
            Code = Some (string error.ErrorNumber)
            RelatedInformation = Some [||]
            Tags = None
            Data = None
            CodeDescription = Some {
              Href = Some (Uri (urlForCompilerCode error.ErrorNumber))
            }
        }

    /// Algorithm from https://stackoverflow.com/a/35734486/433393 for converting file paths to uris,
    /// modified slightly to not rely on the System.Path members because they vary per-platform
    let filePathToUri (filePath: string<LocalPath>): DocumentUri =
        let filePath = UMX.untag filePath
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


type FsacClient(sendServerNotification: ClientNotificationSender, sendServerRequest: ClientRequestSender) =
    inherit LspClient ()

    member __.SendDiagnostics(p: PublishDiagnosticsParams) =
        sendServerNotification "background/diagnostics" (box p) |> Async.Ignore

    member __.Notify(o: Msg) =
        sendServerNotification "background/notify" o |> Async.Ignore

type BackgroundServiceServer(state: State, client: FsacClient) =
    inherit LspServer()

    let checker = FSharpChecker.Create(projectCacheSize = 1, keepAllBackgroundResolutions = false, suggestNamesForErrors = false)

    let mutable latestSdkVersion = lazy None
    let mutable latestRuntimeVersion = lazy None
    //TODO: does the backgroundservice ever get config updates?
    do
      let allowedVersionRange =
        let maxVersion = System.Environment.Version.Major + 1
        SemanticVersioning.Range.Parse $"< %d{maxVersion}"
      let dotnetExe = Ionide.ProjInfo.Paths.dotnetRoot.Value
      match dotnetExe with
      | Some dotnetExe when dotnetExe.Exists ->
        let sdk = lazy (
            Ionide.ProjInfo.SdkDiscovery.sdks dotnetExe
            |> Array.map (fun info -> info.Version)
            |> Environment.maxVersionWithThreshold (Some allowedVersionRange) true
        )
        let runtime = lazy (
            Ionide.ProjInfo.SdkDiscovery.runtimes dotnetExe
            |> Array.map (fun info -> info.Version)
            |> Environment.maxVersionWithThreshold (Some allowedVersionRange) true
        )

        latestSdkVersion <- sdk
        latestRuntimeVersion <- runtime
      | Some exe ->
        let message = $"A dotnet binary was located at '{exe.FullName}' but doesn't exist. This is...nonsensical to say the least."
        failwith message
      | None ->
        failwith "No dotnet binary could be located"

    let getFilesFromOpts (opts: FSharpProjectOptions) =
        (if Array.isEmpty opts.SourceFiles then
            opts.OtherOptions
            |> Seq.where (fun n -> not (n.StartsWith "-") && (n.EndsWith ".fs" || n.EndsWith ".fsi") )
            |> Seq.toArray
         else
            opts.SourceFiles)
        |> Array.map Utils.normalizePath

    let getListOfFilesForProjectChecking (file: BackgroundFileCheckType) =
        let replaceRefs (projOptions: FSharpProjectOptions) =
            let okOtherOpts = projOptions.OtherOptions |> Array.filter (fun r -> not <| r.StartsWith("-r"))
            let assemblyPaths =
              match latestSdkVersion.Value, latestRuntimeVersion.Value with
              | None, _
              | _, None ->
                []
              | Some sdkVersion, Some runtimeVersion ->
                FSIRefs.netCoreRefs Environment.dotnetSDKRoot.Value (string sdkVersion) (string runtimeVersion) (FSIRefs.tfmForRuntime sdkVersion) true
            let refs = assemblyPaths |> List.map (fun r -> "-r:" + r)
            let finalOpts = Array.append okOtherOpts (Array.ofList refs)
            { projOptions with OtherOptions = finalOpts }

        let getScriptOptions file text tfm =
            match tfm with
            | FSIRefs.NetFx ->
                checker.GetProjectOptionsFromScript(file, text, assumeDotNetFramework = true, useSdkRefs = false, useFsiAuxLib = true)
            | FSIRefs.NetCore ->
                async {
                    let! (opts, errors) = checker.GetProjectOptionsFromScript(file, text, assumeDotNetFramework = false, useSdkRefs = true, useFsiAuxLib = true)
                    return replaceRefs opts, errors
                }

        match file with
        | ScriptFile(file, tfm) ->
            state.Files.TryFind (Utils.normalizePath file) |> Option.map (fun st ->
                async {
                    let! (opts, _errors) = getScriptOptions file st.Lines tfm
                    let sf = getFilesFromOpts opts

                    return
                        sf
                        |> Array.skipWhile (fun n -> n <> (Utils.normalizePath file))
                        |> Array.toList
                }
            )
        | SourceFile file ->
            match state.FileCheckOptions.TryFind (Utils.normalizePath file) with
            | None ->
                client.Notify {Value = sprintf "Couldn't find file check options for %A" file } |> Async.Start
                client.Notify {Value = sprintf "Known files %A" (state.FileCheckOptions.Keys |> Seq.toArray) } |> Async.Start
                None
            | Some opts ->
                let sf = getFilesFromOpts opts

                sf
                |> Array.skipWhile (fun n -> n <> (Utils.normalizePath file))
                |> Array.toList
                |> async.Return
                |> Some

    let typecheckFile ignoredFile (file: string<LocalPath>) =
        async {
            do! client.Notify {Value = sprintf "Typechecking %s" (UMX.untag file) }
            match state.Files.TryFind file, state.FileCheckOptions.TryFind file with
            | Some vf, Some opts ->
                let txt = vf.Lines
                let! pr, cr = checker.ParseAndCheckFileInProject(UMX.untag file, defaultArg vf.Version 0, txt, opts)
                match cr with
                | FSharpCheckFileAnswer.Aborted ->
                    do! client.Notify {Value = sprintf "Typechecking aborted %s" (UMX.untag file) }
                    return ()
                | FSharpCheckFileAnswer.Succeeded res ->
                    do! client.Notify {Value = sprintf "Typechecking successful %s" (UMX.untag file) }
                    async {
                      let symbols = res.GetAllUsesOfAllSymbolsInFile()
                      do! client.Notify {Value = sprintf "Got symbols for file %s - %d" (UMX.untag file) (symbols |> Seq.length) }
                      SymbolCache.updateSymbols file symbols
                    } |> Async.Start
                    match ignoredFile with
                    | Some fn when fn = file -> return ()
                    | _ ->
                        let errors = Array.append pr.Diagnostics res.Diagnostics |> Array.map (Helpers.fcsErrorToDiagnostic)
                        let msg = {Diagnostics = errors; Uri = Helpers.filePathToUri file}
                        do! client.Notify {Value = sprintf "Sending diagnostics %s" (UMX.untag file) }
                        do! client.SendDiagnostics msg
                        return ()
            | Some vf, None when (UMX.untag file).EndsWith ".fsx" ->
                let txt = vf.Lines
                let! (opts, _errors) = checker.GetProjectOptionsFromScript(UMX.untag file, txt, assumeDotNetFramework = true, useSdkRefs = false)
                let! pr, cr = checker.ParseAndCheckFileInProject(UMX.untag file, defaultArg vf.Version 0, txt, opts)
                match cr with
                | FSharpCheckFileAnswer.Aborted ->
                    do! client.Notify {Value = sprintf "Typechecking aborted %s" (UMX.untag file) }
                    return ()
                | FSharpCheckFileAnswer.Succeeded res ->
                    async {
                      let symbols = res.GetAllUsesOfAllSymbolsInFile()
                      SymbolCache.updateSymbols file symbols
                    } |> Async.Start
                    match ignoredFile with
                    | Some fn when fn = file -> return ()
                    | _ ->
                        let errors = Array.append pr.Diagnostics res.Diagnostics |> Array.map (Helpers.fcsErrorToDiagnostic)
                        let msg = {Diagnostics = errors; Uri = Helpers.filePathToUri file}
                        do! client.SendDiagnostics msg
                        return ()
            | _ ->
                do! client.Notify {Value = sprintf "Couldn't find state %s" (UMX.untag file) }
                return ()
        }

    let getDependingProjects (file: string<LocalPath>) =
        let project = state.FileCheckOptions.TryFind file
        match project with
        | None -> []
        | Some s ->
            state.FileCheckOptions
            |> Seq.map (fun kv -> kv.Value)
            |> Seq.distinctBy (fun o -> o.ProjectFileName)
            |> Seq.filter (fun o ->
                o.ReferencedProjects
                |> Array.map (fun p -> Path.GetFullPath p.FileName)
                |> Array.contains s.ProjectFileName)
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

                // checker.ClearLanguageServiceRootCachesAndCollectAndFinalizeAllTransients ()
                do! Async.Sleep 300
                return! loop (false, [])
            //Empty
            | Some (_,_,[]), [] ->
                // checker.ClearLanguageServiceRootCachesAndCollectAndFinalizeAllTransients ()
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

    let bouncer = Debounce(200, reactor.Post)

    let clearOldFilesFromCache () =
      async {
        let! files = SymbolCache.getKnownFiles ()
        match files with
        | None -> ()
        | Some files ->
          for f in (Seq.distinct files) do
            if File.Exists f.FileName then ()
            else
              let! _ = SymbolCache.deleteFile f.FileName
              do! client.Notify { Value = sprintf "Cleaned file %s" f.FileName }
              ()
      }

    let clearOldCacheSubscription =
        Observable.Interval(TimeSpan.FromMinutes(5.))
        |> Observable.subscribe(fun _ -> clearOldFilesFromCache () |> Async.Start)

    member __.UpdateTextFile(p: UpdateFileParms) =
        async {
            do! client.Notify {Value = sprintf "File update %s" p.File.FilePath }
            let file = Utils.normalizePath p.File.FilePath

            let vf =
              { Lines = SourceText.ofString p.Content
                Touched = DateTime.Now
                Version = Some p.Version }
            state.Files.AddOrUpdate(file, (fun _ -> vf),( fun _ _ -> vf) ) |> ignore
            let! filesToCheck = defaultArg (getListOfFilesForProjectChecking p.File) (async.Return [])
            do! client.Notify { Value = sprintf "Files to check %A" filesToCheck }
            bouncer.Bounce (false, file,filesToCheck)
            return LspResult.success ()
        }

    member __.UpdateProject(p: ProjectParms) =
        async {

            do! client.Notify {Value = sprintf "Project update recived %s" p.File}
            let knowOptions =
              state.FileCheckOptions.Values

            let refs = p.ReferencedProjects |> Array.choose (fun p ->
              knowOptions
              |> Seq.tryFind (fun o ->
                //This is bad check, but no idea how to do it better
                let outputOpt =
                  o.OtherOptions
                  |> Seq.find (fun oo -> oo.StartsWith "-o:")
                let outputDll = outputOpt.Substring(3).Split('\\') |> Array.last
                let refDll = p.Split('\\') |> Array.last
                outputDll = refDll
              )
              |> Option.map (fun opt ->
                FSharpReferencedProject.CreateFSharp(p, opt)
              )
            )
            let options = {p.Options with ReferencedProjects = refs}


            let sf = getFilesFromOpts options

            sf
            |> Seq.iter (fun file ->
                state.FileCheckOptions.AddOrUpdate(file, (fun _ -> options), (fun _ _ -> options))
                |> ignore
            )
            do! client.Notify {Value = sprintf "Project Updated %s, references: %d" options.ProjectFileName options.ReferencedProjects.Length}
            return LspResult.success ()
        }

    member __.FileSaved(p: FileParms) =
        async {

            let file = Utils.normalizePath p.File.FilePath

            do! client.Notify {Value = sprintf "File Saved %s " (UMX.untag file) }

            let projects = getDependingProjects file
            let! filesToCheck = defaultArg (getListOfFilesForProjectChecking p.File) (async.Return [])
            let filesToCheck =
                [
                    yield! filesToCheck
                    yield! projects |> Seq.collect getFilesFromOpts
                ]
            bouncer.Bounce (true, file,filesToCheck)
            return LspResult.success ()
        }

    override _.Dispose () =
      clearOldCacheSubscription.Dispose()


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

        LanguageServerProtocol.Server.start requestsHandlings input output FsacClient (fun lspClient -> new BackgroundServiceServer(state, lspClient))

    open FSharp.Compiler.IO


    [<EntryPoint>]
    let main argv =

        let pid = Int32.Parse argv.[0]
        let originalFs = FileSystemAutoOpens.FileSystem
        let fs = FsAutoComplete.FileSystem(originalFs, state.Files.TryFind) :> IFileSystem
        FileSystemAutoOpens.FileSystem <- fs
        ProcessWatcher.zombieCheckWithHostPID (fun () -> exit 0) pid
        SymbolCache.initCache (Environment.CurrentDirectory)
        let _ = startCore()
        0
