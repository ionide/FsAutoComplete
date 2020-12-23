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
open Ionide.ProjInfo.ProjectSystem

type BackgroundFileCheckType =
  | SourceFile of filePath: string
  | ScriptFile of filePath: string * tfm: FSIRefs.TFM
  member x.FilePath =
    match x with
    | SourceFile (path)
    | ScriptFile (path, _) -> path


type UpdateFileParms =
  { File: BackgroundFileCheckType
    Content: string
    Version: int }

type ProjectParms =
  { Options: FSharpProjectOptions
    File: string }

type FileParms = { File: BackgroundFileCheckType }

type Msg = { Value: string }

type State =
  { Files: ConcurrentDictionary<SourceFilePath, VolatileFile>
    FileCheckOptions: ConcurrentDictionary<SourceFilePath, FSharpProjectOptions> }
  static member Initial =
    { Files = ConcurrentDictionary()
      FileCheckOptions = ConcurrentDictionary() }

module Helpers =
  let fcsSeverityToDiagnostic =
    function
    | FSharpErrorSeverity.Error -> DiagnosticSeverity.Error
    | FSharpErrorSeverity.Warning -> DiagnosticSeverity.Warning

  let fcsErrorToDiagnostic (error: FSharpErrorInfo) =
    { Range =
        { Start =
            { Line = error.StartLineAlternate - 1
              Character = error.StartColumn }
          End =
            { Line = error.EndLineAlternate - 1
              Character = error.EndColumn } }
      Severity = Some(fcsSeverityToDiagnostic error.Severity)
      Source = "F# Compiler"
      Message = error.Message
      Code = Some(string error.ErrorNumber)
      RelatedInformation = Some [||]
      Tags = None }

  /// Algorithm from https://stackoverflow.com/a/35734486/433393 for converting file paths to uris,
  /// modified slightly to not rely on the System.Path members because they vary per-platform
  let filePathToUri (filePath: string): DocumentUri =
    let uri = StringBuilder(filePath.Length)

    for c in filePath do
      if (c >= 'a' && c <= 'z')
         || (c >= 'A' && c <= 'Z')
         || (c >= '0' && c <= '9')
         || c = '+'
         || c = '/'
         || c = '.'
         || c = '-'
         || c = '_'
         || c = '~'
         || c > '\xFF' then
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


type FsacClient(sendServerRequest: ClientNotificationSender) =
  inherit LspClient()

  member __.SendDiagnostics(p: PublishDiagnosticsParams) =
    sendServerRequest "background/diagnostics" (box p)
    |> Async.Ignore

  member __.Notify(o: Msg) =
    sendServerRequest "background/notify" o
    |> Async.Ignore

type BackgroundServiceServer(state: State, client: FsacClient) =
  inherit LspServer()

  let checker =
    FSharpChecker.Create(projectCacheSize = 1, keepAllBackgroundResolutions = false, suggestNamesForErrors = true)

  do checker.ImplicitlyStartBackgroundWork <- false
  let mutable latestSdkVersion = lazy (None)
  let mutable latestRuntimeVersion = lazy (None)
  //TODO: does the backgroundservice ever get config updates?
  do
    let sdkRoot = Environment.dotnetSDKRoot.Value

    if sdkRoot.Exists then
      latestSdkVersion <- Environment.latest3xSdkVersion sdkRoot
      latestRuntimeVersion <- Environment.latest3xRuntimeVersion sdkRoot


  let getFilesFromOpts (opts: FSharpProjectOptions) =
    if Array.isEmpty opts.SourceFiles then
      opts.OtherOptions
      |> Seq.where
           (fun n ->
             not (n.StartsWith "-")
             && (n.EndsWith ".fs" || n.EndsWith ".fsi"))
      |> Seq.toArray
    else
      opts.SourceFiles

  let getListOfFilesForProjectChecking (file: BackgroundFileCheckType) =
    let replaceRefs (projOptions: FSharpProjectOptions) =
      let okOtherOpts =
        projOptions.OtherOptions
        |> Array.filter (fun r -> not <| r.StartsWith("-r"))

      let assemblyPaths =
        match latestSdkVersion.Value, latestRuntimeVersion.Value with
        | None, _
        | _, None -> []
        | Some sdkVersion, Some runtimeVersion ->
            FSIRefs.netCoreRefs
              Environment.dotnetSDKRoot.Value
              (string sdkVersion)
              (string runtimeVersion)
              (FSIRefs.tfmForRuntime sdkVersion)
              true

      let refs =
        assemblyPaths |> List.map (fun r -> "-r:" + r)

      let finalOpts =
        Array.append okOtherOpts (Array.ofList refs)

      { projOptions with
          OtherOptions = finalOpts }

    let getScriptOptions file lines tfm =
      match tfm with
      | FSIRefs.NetFx ->
          checker.GetProjectOptionsFromScript(
            file,
            SourceText.ofString lines,
            assumeDotNetFramework = true,
            useSdkRefs = false,
            useFsiAuxLib = true
          )
      | FSIRefs.NetCore ->
          async {
            let! (opts, errors) =
              checker.GetProjectOptionsFromScript(
                file,
                SourceText.ofString lines,
                assumeDotNetFramework = false,
                useSdkRefs = true,
                useFsiAuxLib = true
              )

            return replaceRefs opts, errors
          }

    match file with
    | ScriptFile (file, tfm) ->
        state.Files.TryFind(Utils.normalizePath file)
        |> Option.map
             (fun st ->
               async {
                 let! (opts, _errors) = getScriptOptions file (st.Lines |> String.concat "\n") tfm
                 let sf = getFilesFromOpts opts

                 return
                   sf
                   |> Array.skipWhile
                        (fun n ->
                          (Utils.normalizePath n)
                          <> (Utils.normalizePath file))
                   |> Array.map (fun n -> (Utils.normalizePath n))
                   |> Array.toList
               })
    | SourceFile file ->
        match state.FileCheckOptions.TryFind(Utils.normalizePath file) with
        | None ->
            client.Notify { Value = sprintf "Couldn't find file check options for %A" file }
            |> Async.Start

            client.Notify { Value = sprintf "Known files %A" (state.FileCheckOptions.Keys |> Seq.toArray) }
            |> Async.Start

            None
        | Some opts ->
            let sf = getFilesFromOpts opts

            sf
            |> Array.skipWhile
                 (fun n ->
                   (Utils.normalizePath n)
                   <> (Utils.normalizePath file))
            |> Array.map (fun n -> (Utils.normalizePath n))
            |> Array.toList
            |> async.Return
            |> Some

  let typecheckFile ignoredFile file =
    async {
      do! client.Notify { Value = sprintf "Typechecking %s" file }

      match state.Files.TryFind file, state.FileCheckOptions.TryFind file with
      | Some vf, Some opts ->
          let txt = vf.Lines |> String.concat "\n"
          let! pr, cr = checker.ParseAndCheckFileInProject(file, defaultArg vf.Version 0, SourceText.ofString txt, opts)

          match cr with
          | FSharpCheckFileAnswer.Aborted ->
              do! client.Notify { Value = sprintf "Typechecking aborted %s" file }
              return ()
          | FSharpCheckFileAnswer.Succeeded res ->
              let! symbols = res.GetAllUsesOfAllSymbolsInFile()
              SymbolCache.updateSymbols file symbols

              match ignoredFile with
              | Some fn when fn = file -> return ()
              | _ ->
                  let errors =
                    Array.append pr.Errors res.Errors
                    |> Array.map (Helpers.fcsErrorToDiagnostic)

                  let msg =
                    { Diagnostics = errors
                      Uri = Helpers.filePathToUri file }

                  do! client.SendDiagnostics msg
                  return ()
      | Some vf, None when file.EndsWith ".fsx" ->
          let txt = vf.Lines |> String.concat "\n"

          let! (opts, _errors) =
            checker.GetProjectOptionsFromScript(
              file,
              SourceText.ofString txt,
              assumeDotNetFramework = true,
              useSdkRefs = false
            )

          let! pr, cr = checker.ParseAndCheckFileInProject(file, defaultArg vf.Version 0, SourceText.ofString txt, opts)

          match cr with
          | FSharpCheckFileAnswer.Aborted ->
              do! client.Notify { Value = sprintf "Typechecking aborted %s" file }
              return ()
          | FSharpCheckFileAnswer.Succeeded res ->
              let! symbols = res.GetAllUsesOfAllSymbolsInFile()
              SymbolCache.updateSymbols file symbols

              match ignoredFile with
              | Some fn when fn = file -> return ()
              | _ ->
                  let errors =
                    Array.append pr.Errors res.Errors
                    |> Array.map (Helpers.fcsErrorToDiagnostic)

                  let msg =
                    { Diagnostics = errors
                      Uri = Helpers.filePathToUri file }

                  do! client.SendDiagnostics msg
                  return ()
      | _ ->
          do! client.Notify { Value = sprintf "Couldn't find state %s" file }
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
        |> Seq.filter
             (fun o ->
               o.ReferencedProjects
               |> Array.map (fun (_, v) -> Path.GetFullPath v.ProjectFileName)
               |> Array.contains s.ProjectFileName)
        |> Seq.toList




  let reactor =
    MailboxProcessor.Start
      (fun agent ->
        let rec recieveLast last =
          async {
            let! msg = agent.TryReceive(5)

            match msg with
            | Some s -> return! recieveLast (Some s)
            | None -> return last
          }

        let rec loop (isFromSave, lst) =
          async {
            let! msg = recieveLast None

            match msg, lst with
            //Empty
            | None, [] ->
                do! Async.Sleep 300
                return! loop (false, [])
            //Empty
            | Some (_, _, []), [] ->
                do! Async.Sleep 300
                return! loop (false, [])
            //No request we just continue
            | None, x :: xs ->
                do! typecheckFile None x
                return! loop (isFromSave, xs)

            //We've ended processing request we start new
            | Some (saveRequest, fn, x :: xs), [] ->
                do! typecheckFile (Some fn) x
                return! loop (saveRequest, xs)

            //If incoming is normal update and current is from save request we continue current
            | Some (false, _, _), x :: xs when isFromSave ->
                do! typecheckFile None x
                return! loop (isFromSave, xs)

            //If incoming is normal and previous was normal
            | Some (false, fn, (x :: xs)), _ ->
                do! typecheckFile (Some fn) x
                return! loop (false, xs)

            //If incoming request is from save we always start it
            | Some (true, fn, (x :: xs)), _ ->
                do! typecheckFile (Some fn) x
                return! loop (true, xs)

            //If incoming request doesn't contain any list we just continue previous one
            | Some (_, fn, []), x :: xs ->
                do! typecheckFile (Some fn) x
                return! loop (isFromSave, xs)
          }

        loop (false, []))

  let bouncer = Debounce(500, reactor.Post)

  let checkExistingFiles () =
    async {
      let! files = SymbolCache.getKnownFiles ()

      match files with
      | None -> ()
      | Some files ->
          for f in (Seq.distinct files) do
            if File.Exists f.FileName then
              ()
            else
              let! _ = SymbolCache.deleteFile f.FileName
              do! client.Notify { Value = sprintf "Cleaned file %s" f.FileName }
              ()
    }


  member __.UpdateTextFile(p: UpdateFileParms) =
    async {
      do! client.Notify { Value = sprintf "File update %s" p.File.FilePath }
      let file = Utils.normalizePath p.File.FilePath

      let vf =
        { Lines = p.Content.Split([| '\n' |])
          Touched = DateTime.Now
          Version = Some p.Version }

      state.Files.AddOrUpdate(file, (fun _ -> vf), (fun _ _ -> vf))
      |> ignore

      let! filesToCheck = defaultArg (getListOfFilesForProjectChecking p.File) (async.Return [])
      do! client.Notify { Value = sprintf "Files to check %A" filesToCheck }
      bouncer.Bounce(false, file, filesToCheck)
      return LspResult.success ()
    }

  member __.UpdateProject(p: ProjectParms) =
    async {
      checkExistingFiles () |> Async.Start

      let sf = getFilesFromOpts p.Options

      sf
      |> Seq.iter
           (fun f ->
             let file = Utils.normalizePath f

             state.FileCheckOptions.AddOrUpdate(file, (fun _ -> p.Options), (fun _ _ -> p.Options))
             |> ignore)

      do! client.Notify { Value = sprintf "Project Updated %s" p.Options.ProjectFileName }
      return LspResult.success ()
    }

  member __.FileSaved(p: FileParms) =
    async {

      let file = Utils.normalizePath p.File.FilePath

      do! client.Notify { Value = sprintf "File Saved %s " file }

      let projects = getDependingProjects file
      let! filesToCheck = defaultArg (getListOfFilesForProjectChecking p.File) (async.Return [])

      let filesToCheck =
        [ yield! filesToCheck
          yield! projects |> Seq.collect getFilesFromOpts ]

      bouncer.Bounce(true, file, filesToCheck)
      return LspResult.success ()
    }

module Program =

  let state = State.Initial

  let startCore () =
    use input = Console.OpenStandardInput()
    use output = Console.OpenStandardOutput()

    let requestsHandlings =
      Map.empty<string, RequestHandling<BackgroundServiceServer>>
      |> Map.add "background/update" (requestHandling (fun s p -> s.UpdateTextFile(p)))
      |> Map.add "background/project" (requestHandling (fun s p -> s.UpdateProject(p)))
      |> Map.add "background/save" (requestHandling (fun s p -> s.FileSaved(p)))

    LanguageServerProtocol.Server.start
      requestsHandlings
      input
      output
      FsacClient
      (fun lspClient -> BackgroundServiceServer(state, lspClient))



  [<EntryPoint>]
  let main argv =

    let pid = Int32.Parse argv.[0]

    let originalFs =
      AbstractIL.Internal.Library.Shim.FileSystem

    let fs =
      FileSystem(originalFs, state.Files.TryFind)

    AbstractIL.Internal.Library.Shim.FileSystem <- fs
    ProcessWatcher.zombieCheckWithHostPID (fun () -> exit 0) pid
    SymbolCache.initCache (Environment.CurrentDirectory)
    let _ = startCore ()
    0
