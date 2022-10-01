namespace FsAutoComplete.Lsp

open System
open System.IO
open System.Threading
open FsAutoComplete
open FsAutoComplete.Core
open FsAutoComplete.CodeFix
open FsAutoComplete.CodeFix.Types
open FsAutoComplete.Logging
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types.LspResult
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Newtonsoft.Json.Linq
open Ionide.ProjInfo.ProjectSystem

open FsToolkit.ErrorHandling
open FSharp.UMX

open FSharp.Compiler.Text
open CliWrap
open CliWrap.Buffered
open FSharp.Compiler.Tokenization
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Symbols
open Fantomas.Client.Contracts
open Fantomas.Client.LSPFantomasService

open FSharp.Data.Adaptive
open Ionide.ProjInfo
open FSharp.Compiler.CodeAnalysis
open System.Linq

open System.Reactive.Linq
open Microsoft.Build.Graph
open FsAutoComplete.LspHelpers

[<AutoOpen>]
module AdaptiveExtensions =
  type ChangeableHashMap<'Key, 'Value> with

    member x.AddOrUpdate(key, adder, updater) =
      match x.TryGetValue key with
      | None -> x.[key] <- adder key
      | Some v -> x.[key] <- updater key v

module Utils =
  let cheapEqual (a: 'T) (b: 'T) =
    ShallowEqualityComparer<'T>.Instance.Equals (a, b)

type MapDisposableTupleVal<'T1, 'T2, 'T3 when 'T3 :> IDisposable>(mapping: 'T1 -> ('T2 * 'T3), input: aval<'T1>) =
  inherit AVal.AbstractVal<'T2>()

  // can we avoid double caching (here and in AbstractVal)
  let mutable cache: ValueOption<struct ('T1 * 'T2 * 'T3)> = ValueNone

  override x.Compute(token: AdaptiveToken) =
    let i = input.GetValue token

    match cache with
    | ValueSome (struct (a, b, _)) when Utils.cheapEqual a i -> b
    | ValueSome (struct (a, b, c)) ->
      (c :> IDisposable).Dispose()
      let (b, c) = mapping i
      cache <- ValueSome(struct (i, b, c))
      b
    | ValueNone ->
      let (b, c) = mapping i
      cache <- ValueSome(struct (i, b, c))
      b




module AVal =
  let mapOption f = AVal.map (Option.map f)

  let mapDisposableTuple mapper value =
    MapDisposableTupleVal(mapper, value) :> aval<_>

  let mapWithAdditionalDependenies (mapping: 'a -> 'b * list<IAdaptiveValue>) (value: aval<'a>) : aval<'b> =
    let mutable lastDeps = HashSet.empty

    { new AVal.AbstractVal<'b>() with
        member x.Compute(token: AdaptiveToken) =
          let input = value.GetValue token

          // re-evaluate the mapping based on the (possibly new input)
          let result, deps = mapping input

          // compute the change in the additional dependencies and adjust the graph accordingly
          let newDeps = HashSet.ofList deps

          for op in HashSet.computeDelta lastDeps newDeps do
            match op with
            | Add (_, d) ->
              // the new dependency needs to be evaluated with our token, s.t. we depend on it in the future
              d.GetValueUntyped token |> ignore
            | Rem (_, d) ->
              // we no longer need to depend on the old dependency so we can remove ourselves from its outputs
              lock d.Outputs (fun () -> d.Outputs.Remove x) |> ignore

          lastDeps <- newDeps

          result }
    :> aval<_>


module ASet =
  let mapAtoAMap mapper src =
    src |> ASet.mapToAMap mapper |> AMap.mapA (fun _ v -> v)

module AMap =
  let tryFindAO key (map: amap<_, aval<option<'b>>>) =
    aval {
      let! item = AMap.tryFind key map

      match item with
      | Some x -> return! x
      | None -> return None
    }

  let tryFindA key (map: amap<_, aval<'b>>) =
    aval {
      let! item = AMap.tryFind key map

      match item with
      | Some v ->
        let! v2 = v
        return Some v2
      | None -> return None
    }

  let mapVA mapper (map: amap<_, aval<'b>>) =
    map |> AMap.mapA (fun k v -> AVal.map mapper v)


[<RequireQualifiedAccess>]
type WorkspaceChosen =
  | Sln of string<LocalPath> // TODO later when ionide supports sending specific choices instead of only fsprojs
  | Directory of string<LocalPath> // TODO later when ionide supports sending specific choices instead of only fsprojs
  | Projs of HashSet<string<LocalPath>>
  | NotChosen

[<RequireQualifiedAccess>]
type AdaptiveWorkspaceChosen =
  | Sln of aval<string<LocalPath> * DateTime> // TODO later when ionide supports sending specific choices instead of only fsprojs
  | Directory of aval<string<LocalPath> * DateTime> // TODO later when ionide supports sending specific choices instead of only fsprojs
  | Projs of aval<HashMap<string<LocalPath>, DateTime>>
  | NotChosen

type AdaptiveFSharpLspServer(workspaceLoader: IWorkspaceLoader, lspClient: FSharpLspClient) =

  let disposables = new System.Reactive.Disposables.CompositeDisposable()

  let config = cval<FSharpConfig> FSharpConfig.Default

  let checker =
    config |> AVal.map (fun c -> FSharpCompilerServiceChecker(c.EnableAnalyzers))

  let tfmConfig =
    config
    |> AVal.map (fun c ->
      if c.UseSdkScripts then
        FSIRefs.TFM.NetCore
      else
        FSIRefs.TFM.NetFx)

  let logger = LogProvider.getLoggerByName "AdaptiveFSharpLspServer"

  let sendDiagnostics (uri: DocumentUri) (diags: Diagnostic[]) =
    logger.info (
      Log.setMessage "SendDiag for {file}: {diags} entries"
      >> Log.addContextDestructured "file" uri
      >> Log.addContextDestructured "diags" diags.Length
    )

    { Uri = uri; Diagnostics = diags } |> lspClient.TextDocumentPublishDiagnostics

  let mutable lastFSharpDocumentationTypeCheck: ParseAndCheckResults option = None

  let diagnosticCollections = new DiagnosticCollection(sendDiagnostics)

  let notifications = Event<NotificationEvent>()

  let handleCommandEvents (n: NotificationEvent) =
    try
      async {
        try
          match n with
          | NotificationEvent.FileParsed fn ->
            let uri = Path.LocalPathToUri fn

            do! lspClient.CodeLensRefresh()
            do! ({ Content = UMX.untag uri }: PlainNotification) |> lspClient.NotifyFileParsed
          | NotificationEvent.Workspace ws ->

            let ws =
              match ws with
              | ProjectResponse.Project (x, _) -> CommandResponse.project JsonSerializer.writeJson x
              | ProjectResponse.ProjectError (_, errorDetails) ->
                CommandResponse.projectError JsonSerializer.writeJson errorDetails
              | ProjectResponse.ProjectLoading (projectFileName) ->
                CommandResponse.projectLoading JsonSerializer.writeJson projectFileName
              | ProjectResponse.WorkspaceLoad (finished) ->
                CommandResponse.workspaceLoad JsonSerializer.writeJson finished
              | ProjectResponse.ProjectChanged (projectFileName) -> failwith "Not Implemented"

            logger.info (Log.setMessage "Workspace Notify {ws}" >> Log.addContextDestructured "ws" ws)
            do! ({ Content = ws }: PlainNotification) |> lspClient.NotifyWorkspace

          | NotificationEvent.ParseError (errors, file) ->
            let uri = Path.LocalPathToUri file
            let diags = errors |> Array.map fcsErrorToDiagnostic
            diagnosticCollections.SetFor(uri, "F# Compiler", diags)

          | NotificationEvent.UnusedOpens (file, opens) ->
            let uri = Path.LocalPathToUri file

            let diags =
              opens
              |> Array.map (fun n ->
                { Range = fcsRangeToLsp n
                  Code = Some "FSAC0001"
                  Severity = Some DiagnosticSeverity.Hint
                  Source = "FSAC"
                  Message = "Unused open statement"
                  RelatedInformation = None
                  Tags = Some [| DiagnosticTag.Unnecessary |]
                  Data = None
                  CodeDescription = None })

            diagnosticCollections.SetFor(uri, "F# Unused opens", diags)

          | NotificationEvent.UnusedDeclarations (file, decls) ->
            let uri = Path.LocalPathToUri file

            let diags =
              decls
              |> Array.map (fun n ->
                { Range = fcsRangeToLsp n
                  Code = Some "FSAC0003"
                  Severity = Some DiagnosticSeverity.Hint
                  Source = "FSAC"
                  Message = "This value is unused"
                  RelatedInformation = Some [||]
                  Tags = Some [| DiagnosticTag.Unnecessary |]
                  Data = None
                  CodeDescription = None })

            diagnosticCollections.SetFor(uri, "F# Unused declarations", diags)

          | NotificationEvent.SimplifyNames (file, decls) ->
            let uri = Path.LocalPathToUri file

            let diags =
              decls
              |> Array.map

                (fun ({ Range = range
                        RelativeName = _relName }) ->
                  { Diagnostic.Range = fcsRangeToLsp range
                    Code = Some "FSAC0002"
                    Severity = Some DiagnosticSeverity.Hint
                    Source = "FSAC"
                    Message = "This qualifier is redundant"
                    RelatedInformation = Some [||]
                    Tags = Some [| DiagnosticTag.Unnecessary |]
                    Data = None
                    CodeDescription = None })

            diagnosticCollections.SetFor(uri, "F# simplify names", diags)

          // | NotificationEvent.Lint (file, warnings) ->
          //     let uri = Path.LocalPathToUri file
          //     // let fs =
          //     //     warnings |> List.choose (fun w ->
          //     //         w.Warning.Details.SuggestedFix
          //     //         |> Option.bind (fun f ->
          //     //             let f = f.Force()
          //     //             let range = fcsRangeToLsp w.Warning.Details.Range
          //     //             f |> Option.map (fun f -> range, {Range = range; NewText = f.ToText})
          //     //         )
          //     //     )

          //     let diags =
          //         warnings
          //         |> List.map(fun w ->
          //             let range = fcsRangeToLsp w.Warning.Details.Range
          //             let fixes =
          //               match w.Warning.Details.SuggestedFix with
          //               | None -> None
          //               | Some lazyFix ->
          //                 match lazyFix.Value with
          //                 | None -> None
          //                 | Some fix ->
          //                   Some (box [ { Range = fcsRangeToLsp fix.FromRange; NewText = fix.ToText } ] )
          //             let uri = Option.ofObj w.HelpUrl |> Option.map (fun url -> { Href = Some (Uri url) })
          //             { Range = range
          //               Code = Some w.Code
          //               Severity = Some DiagnosticSeverity.Information
          //               Source = "F# Linter"
          //               Message = w.Warning.Details.Message
          //               RelatedInformation = None
          //               Tags = None
          //               Data = fixes
          //               CodeDescription = uri }
          //         )
          //         |> List.sortBy (fun diag -> diag.Range)
          //         |> List.toArray
          //     diagnosticCollections.SetFor(uri, "F# Linter", diags)

          | NotificationEvent.Canceled (msg) ->
            let ntf: PlainNotification = { Content = msg }

            do! lspClient.NotifyCancelledRequest ntf
          | NotificationEvent.AnalyzerMessage (messages, file) ->
            let uri = Path.LocalPathToUri file

            match messages with
            | [||] -> diagnosticCollections.SetFor(uri, "F# Analyzers", [||])
            | messages ->
              let diags =
                messages
                |> Array.map (fun m ->
                  let range = fcsRangeToLsp m.Range

                  let severity =
                    match m.Severity with
                    | FSharp.Analyzers.SDK.Info -> DiagnosticSeverity.Information
                    | FSharp.Analyzers.SDK.Warning -> DiagnosticSeverity.Warning
                    | FSharp.Analyzers.SDK.Error -> DiagnosticSeverity.Error

                  let fixes =
                    match m.Fixes with
                    | [] -> None
                    | fixes ->
                      fixes
                      |> List.map (fun fix ->
                        { Range = fcsRangeToLsp fix.FromRange
                          NewText = fix.ToText })
                      |> Ionide.LanguageServerProtocol.Server.serialize
                      |> Some

                  { Range = range
                    Code = Option.ofObj m.Code
                    Severity = Some severity
                    Source = $"F# Analyzers (%s{m.Type})"
                    Message = m.Message
                    RelatedInformation = None
                    Tags = None
                    CodeDescription = None
                    Data = fixes })

              diagnosticCollections.SetFor(uri, "F# Analyzers", diags)
          | NotificationEvent.TestDetected (file, tests) ->
            let rec map
              (r: TestAdapter.TestAdapterEntry<FSharp.Compiler.Text.range>)
              : TestAdapter.TestAdapterEntry<Ionide.LanguageServerProtocol.Types.Range> =
              { Id = r.Id
                List = r.List
                Name = r.Name
                Type = r.Type
                Range = fcsRangeToLsp r.Range
                Childs = ResizeArray(r.Childs |> Seq.map map) }

            do!
              { File = Path.LocalPathToUri file
                Tests = tests |> Array.map map }
              |> lspClient.NotifyTestDetected
        with ex ->
          logger.error (
            Log.setMessage "Exception while handling command event {evt}: {ex}"
            >> Log.addContextDestructured "evt" n
            >> Log.addContext "ex" ex.Message
          )

        ()
      }
      |> Async.RunSynchronouslyWithCT CancellationToken.None

    with :? OperationCanceledException as e ->
      ()

  do
    disposables.Add(
      (notifications.Publish :> IObservable<_>)
        // .BufferedDebounce(TimeSpan.FromMilliseconds(200.))
        // .SelectMany(fun l -> l.Distinct())
        .Subscribe(fun e -> handleCommandEvents e)
    )

  let adaptiveFile (filePath: string<LocalPath>) =
    let file =
      AdaptiveFile.GetLastWriteTimeUtc(UMX.untag filePath)
      |> AVal.map (fun writeTime -> filePath, writeTime)

    let cb =
      file.AddMarkingCallback(fun () ->
        logger.info (
          Log.setMessage "Loading projects because of {delta}"
          >> Log.addContextDestructured "delta" filePath
        ))


    file |> AVal.mapDisposableTuple (fun x -> x, cb)

  let loader = cval<Ionide.ProjInfo.IWorkspaceLoader> workspaceLoader

  let rootPath = cval<string option> None

  // JB:TODO Adding to solution
  // JB:TODO Adding new project file not yet added to solution
  let workspacePaths: ChangeableValue<WorkspaceChosen> =
    cval WorkspaceChosen.NotChosen

  let adaptiveWorkspacePaths =
    workspacePaths
    |> AVal.map (fun wsp ->
      match wsp with
      | WorkspaceChosen.Sln v -> adaptiveFile v |> AdaptiveWorkspaceChosen.Sln
      | WorkspaceChosen.Directory d -> failwith "Need to use AdaptiveDirectory" |> AdaptiveWorkspaceChosen.Directory
      | WorkspaceChosen.Projs projs ->
        let projChanges =
          projs
          |> ASet.ofSeq
          |> ASet.mapAtoAMap (UMX.untag >> AdaptiveFile.GetLastWriteTimeUtc)

        let cb =
          projChanges.AddCallback(fun old delta ->
            logger.info (
              Log.setMessage "Loading projects because of {delta}"
              >> Log.addContextDestructured "delta" delta
            ))

        projChanges
        |> AMap.toAVal
        |> AVal.mapDisposableTuple (fun x -> x, cb)
        |> AdaptiveWorkspaceChosen.Projs

      | WorkspaceChosen.NotChosen -> AdaptiveWorkspaceChosen.NotChosen

    )


  let clientCapabilities = cval<ClientCapabilities option> None

  let glyphToCompletionKind =
    clientCapabilities |> AVal.map (glyphToCompletionKindGenerator)

  let glyphToSymbolKind = clientCapabilities |> AVal.map glyphToSymbolKindGenerator



  let loadedProjectOptions =
    (loader, adaptiveWorkspacePaths)
    ||> AVal.bind2 (fun loader wsp ->
      aval {
        match wsp with
        | AdaptiveWorkspaceChosen.NotChosen -> return []
        | AdaptiveWorkspaceChosen.Sln _ -> return raise (NotImplementedException())
        | AdaptiveWorkspaceChosen.Directory _ -> return raise (NotImplementedException())
        | AdaptiveWorkspaceChosen.Projs projects ->
          let! projectOptions =
            projects
            |> AVal.mapWithAdditionalDependenies (fun projects ->


              projects
              |> Seq.iter (fun (proj: string<LocalPath>, _) ->
                UMX.untag proj
                |> ProjectResponse.ProjectLoading
                |> NotificationEvent.Workspace
                |> notifications.Trigger)

              let projectOptions =
                loader.LoadProjects(projects |> Seq.map (fst >> UMX.untag) |> Seq.toList)
                |> Seq.toList

              for p in projectOptions do
                logger.info (
                  Log.setMessage "Found BaseIntermediateOutputPath of {path}"
                  >> Log.addContextDestructured "path" p.Properties
                )

              let additionalDependencies =

                [ for p in projectOptions do
                    match p.Properties |> Seq.tryFind (fun x -> x.Name = "ProjectAssetsFile") with
                    | Some v -> yield adaptiveFile (UMX.tag v.Value)
                    | None -> ()

                    let objPath =
                      p.Properties
                      |> Seq.tryFind (fun x -> x.Name = "BaseIntermediateOutputPath")
                      |> Option.map (fun v -> v.Value)

                    let isWithinObjFolder (file: string) =
                      match objPath with
                      | None -> true // if no obj folder provided assume we should track this file
                      | Some v -> file.Contains(v)

                    match p.Properties |> Seq.tryFind (fun x -> x.Name = "MSBuildAllProjects") with
                    | Some v ->
                      yield!
                        v.Value.Split(';', StringSplitOptions.RemoveEmptyEntries)
                        |> Array.filter (fun x -> x.EndsWith(".props") && isWithinObjFolder x)
                        |> Array.map (UMX.tag >> adaptiveFile)
                    | None -> () ]
                |> Seq.cast<IAdaptiveValue>
                |> Seq.toList

              projectOptions, additionalDependencies)


          let options =
            projectOptions
            |> List.map (fun o ->
              let fso = FCS.mapToFSharpProjectOptions o projectOptions

              fso, o)

          options
          |> List.iter (fun (opts, extraInfo) ->
            let projectFileName = opts.ProjectFileName
            let projViewerItemsNormalized = Ionide.ProjInfo.ProjectViewer.render extraInfo

            let responseFiles =
              projViewerItemsNormalized.Items
              |> List.map (function
                | ProjectViewerItem.Compile (p, c) -> ProjectViewerItem.Compile(Helpers.fullPathNormalized p, c))
              |> List.choose (function
                | ProjectViewerItem.Compile (p, _) -> Some p)

            let references = FscArguments.references (opts.OtherOptions |> List.ofArray)

            logger.info (
              Log.setMessage "ProjectLoaded {file}"
              >> Log.addContextDestructured "file" opts.ProjectFileName
            )

            let ws =
              { ProjectFileName = opts.ProjectFileName
                ProjectFiles = responseFiles
                OutFileOpt = Option.ofObj extraInfo.TargetPath
                References = references
                Extra = extraInfo
                ProjectItems = projViewerItemsNormalized.Items
                Additionals = Map.empty }

            ProjectResponse.Project(ws, false)
            |> NotificationEvent.Workspace
            |> notifications.Trigger)

          ProjectResponse.WorkspaceLoad true
          |> NotificationEvent.Workspace
          |> notifications.Trigger

          return options |> List.map fst
      })


  let fantomasLogger = LogProvider.getLoggerByName "Fantomas"
  let fantomasService: FantomasService = new LSPFantomasService() :> FantomasService

  // let openFiles = cmap<string<LocalPath>, VolatileFile * CancellationTokenSource> ()

  let openFiles =
    cmap<string<LocalPath>, cval<VolatileFile * CancellationTokenSource>> ()

  let openFilesA = openFiles |> AMap.map' (fun v -> v :> aval<_>)

  let updateOpenFiles (file: VolatileFile) =

    let adder _ = //file, new CancellationTokenSource()
      cval (file, new CancellationTokenSource())

    let updater _ (v: cval<_>) =
      let (oldFile, cts: CancellationTokenSource) = v.Value
      cts.Cancel()
      cts.Dispose()
      v.Value <- file, new CancellationTokenSource()
      v


    transact (fun () -> openFiles.AddOrUpdate(file.Lines.FileName, adder, updater))


  let volaTileFile path text version lastWriteTime =
    { Touched = lastWriteTime
      Lines = NamedText(path, text)
      Version = version }

  let getFileInfoForFile file = openFilesA |> AMap.tryFindA file

  let tryGetFile file =
    getFileInfoForFile file
    |> AVal.force
    |> Option.orElseWith (fun () ->
      try
        let untaged = UMX.untag file
        let lastWriteTime = File.GetLastWriteTimeUtc untaged

        if File.Exists untaged then
          let change = File.ReadAllText untaged

          let file =
            { Touched = lastWriteTime
              Lines = NamedText(file, change)
              Version = None }

          (file, new CancellationTokenSource()) |> Some
        else
          None
      with e ->
        logger.warn (
          Log.setMessage "Could not read file {file}"
          >> Log.addContextDestructured "file" file
          >> Log.addExn e
        )

        None)
    |> Result.ofOption (fun () -> $"Could not read file: {file}")

  let tryGetFileOpt filePath =
    getFileInfoForFile filePath |> AVal.force |> Option.map fst

  do
    FSharp.Compiler.IO.FileSystemAutoOpens.FileSystem <-
      FsAutoComplete.FileSystem(FSharp.Compiler.IO.FileSystemAutoOpens.FileSystem, tryGetFileOpt)

  let tryGetFileSource filePath =
    tryGetFile filePath |> Result.map (fun (f, _) -> f.Lines)

  let knownFsFilesToProjectOptions =
    openFilesA
    |> AMap.mapVA (fun (info, cts) ->
      let file = info.Lines.FileName

      if Utils.isAScript (UMX.untag file) then
        (checker, tfmConfig)
        ||> AVal.map2 (fun checker tfm ->
          let opts =
            checker.GetProjectOptionsFromScript(file, info.Lines, tfm)
            |> Async.RunSynchronouslyWithCTSafe(fun () -> cts.Token)

          opts |> Option.map List.singleton |> Option.defaultValue List.empty)

      else
        loadedProjectOptions
        |> AVal.map (fun opts ->
          opts
          |> List.filter (fun (opts) -> opts.SourceFiles |> Array.map Utils.normalizePath |> Array.contains (file))))


  let getProjectOptionsForFile file =
    knownFsFilesToProjectOptions
    |> AMap.tryFind file
    |> AVal.bind (Option.defaultValue (AVal.constant []))

  let autoCompleteItems: cmap<DeclName, DeclarationListItem * Position * string<LocalPath> * (Position -> option<string>) * FSharp.Compiler.Syntax.ParsedInput> =
    cmap ()

  let getAutoCompleteByDeclName name = autoCompleteItems |> AMap.tryFind name

  let autoCompleteNamespaces =
    autoCompleteItems
    |> AMap.choose (fun name (d, pos, fn, getline, ast) ->

      Commands.calculateNamespaceInsert (fun () -> Some ast) d pos getline)

  let getAutoCompleteNamespacesByDeclName name =
    autoCompleteNamespaces |> AMap.tryFind name

  let analyzeFile (filePath: string<LocalPath>, version, source, tyRes: ParseAndCheckResults) =
    let config = config |> AVal.force

    let checkUnusedOpens =
      async {
        try
          let! unused =
            UnusedOpens.getUnusedOpens (tyRes.GetCheckResults, (fun i -> (source: ISourceText).GetLineString(i - 1)))

          notifications.Trigger(NotificationEvent.UnusedOpens(filePath, (unused |> List.toArray)))
        with e ->
          logger.error (Log.setMessage "checkUnusedOpens failed" >> Log.addExn e)
      }

    let checkUnusedDeclarations =
      async {
        try
          let isScript = Utils.isAScript (UMX.untag filePath)
          let! unused = UnusedDeclarations.getUnusedDeclarations (tyRes.GetCheckResults, isScript)
          let unused = unused |> Seq.toArray

          notifications.Trigger(NotificationEvent.UnusedDeclarations(filePath, unused))
        with e ->
          logger.error (Log.setMessage "checkUnusedDeclarations failed" >> Log.addExn e)
      }

    let checkSimplifiedNames =
      async {
        try
          let getSourceLine lineNo = source.GetLineString(lineNo - 1)

          let! simplified = SimplifyNames.getSimplifiableNames (tyRes.GetCheckResults, getSourceLine)
          let simplified = Array.ofSeq simplified
          notifications.Trigger(NotificationEvent.SimplifyNames(filePath, simplified))
        with e ->
          logger.error (Log.setMessage "checkSimplifiedNames failed" >> Log.addExn e)
      }

    let analyzers =
      [
        // if config.Linter then
        //   commands.Lint filePath |> Async .Ignore
        if config.UnusedOpensAnalyzer then
          checkUnusedOpens
        if config.UnusedDeclarationsAnalyzer then
          checkUnusedDeclarations
        if config.SimplifyNameAnalyzer then
          checkSimplifiedNames ]

    async {
      do! analyzers |> Async.Parallel |> Async.Ignore<unit[]>

      do!
        lspClient.NotifyDocumentAnalyzed
          { TextDocument =
              { Uri = filePath |> Path.LocalPathToUri
                Version = version } }
    }

  let parseAndCheckFile (checker: FSharpCompilerServiceChecker) (file: VolatileFile) opts =
    async {
      logger.info (
        Log.setMessage "Getting typecheck results for {file} - {hash} - {date}"
        >> Log.addContextDestructured "file" file.Lines.FileName
        >> Log.addContextDestructured "hash" (file.Lines.GetHashCode())
        >> Log.addContextDestructured "date" (file.Touched)
      )

      use cts = new CancellationTokenSource()
      cts.CancelAfter(TimeSpan.FromSeconds(60.))

      let! result =
        checker.ParseAndCheckFileInProject(file.Lines.FileName, (file.Lines.GetHashCode()), file.Lines, opts)
        |> Async.withCancellation cts.Token

      notifications.Trigger(NotificationEvent.FileParsed(file.Lines.FileName))

      match result with
      | Error e ->
        logger.info (
          Log.setMessage "Typecheck failed for {file} with {error}"
          >> Log.addContextDestructured "file" file
          >> Log.addContextDestructured "error" e
        )

        return failwith e
      | Ok parseAndCheck ->
        logger.info (
          Log.setMessage "Typecheck completed successfully for {file}"
          >> Log.addContextDestructured "file" file.Lines.FileName
        )

        analyzeFile (file.Lines.FileName, file.Version, file.Lines, parseAndCheck)
        |> Async.Start

        let checkErrors = parseAndCheck.GetParseResults.Diagnostics
        let parseErrors = parseAndCheck.GetCheckResults.Diagnostics

        let errors =
          Array.append checkErrors parseErrors
          |> Array.distinctBy (fun e ->
            e.Severity, e.ErrorNumber, e.StartLine, e.StartColumn, e.EndLine, e.EndColumn, e.Message)

        let uri = Path.LocalPathToUri(file.Lines.FileName)
        let diags = errors |> Array.map fcsErrorToDiagnostic
        diagnosticCollections.SetFor(uri, "F# Compiler", diags)

        System.Runtime.GCSettings.LargeObjectHeapCompactionMode <-
          System.Runtime.GCLargeObjectHeapCompactionMode.CompactOnce

        GC.Collect()
        GC.WaitForPendingFinalizers()

        return parseAndCheck
    }

  let forceTypeCheck checker f =
    async {
      logger.info (Log.setMessage "Forced Check : {file}" >> Log.addContextDestructured "file" f)

      match getFileInfoForFile f |> AVal.force, getProjectOptionsForFile f |> AVal.force |> List.tryHead with
      | Some (fileInfo, _), Some (opts) -> return! parseAndCheckFile checker fileInfo opts |> Async.Ignore
      | _, _ -> ()
    }

  let knownFsFilesToParsedResults =
    openFilesA
    |> AMap.mapVA (fun (info, cts) ->
      aval {
        let file = info.Lines.FileName
        let sourceText = info.Lines

        let! checker = checker
        and! projectOptions = getProjectOptionsForFile file

        match List.tryHead projectOptions with
        | Some opts ->
          return
            Debug.measure "parseFile"
            <| fun () ->
                 let opts = Utils.projectOptionsToParseOptions opts

                 checker.ParseFile(file, info.Lines, opts)
                 |> Async.RunSynchronouslyWithCTSafe(fun () -> cts.Token)
        | None -> return None
      })


  let knownFsFilesToRecentCheckedFilesResults =
    openFilesA
    |> AMap.mapVA (fun (info, cts) ->
      aval {
        let file = info.Lines.FileName
        let! checker = checker
        and! projectOptions = getProjectOptionsForFile file

        match List.tryHead projectOptions with
        | Some (opts) ->
          let parseAndCheck = checker.TryGetRecentCheckResultsForFile(file, opts, info.Lines)

          return parseAndCheck
        | None -> return None
      })

  let knownFsFilesToCheckedFilesResults =
    openFilesA
    |> AMap.mapVA (fun (info, cts) ->
      aval {
        let file = info.Lines.FileName
        let! checker = checker
        and! projectOptions = getProjectOptionsForFile file

        match List.tryHead projectOptions with
        | Some (opts) ->
          let parseAndCheck =
            Debug.measure "parseAndCheckFile"
            <| fun () ->
                 parseAndCheckFile checker info opts
                 |> Async.RunSynchronouslyWithCTSafe(fun () -> cts.Token)

          return parseAndCheck
        | None -> return None
      })

  let getParseResults filePath =
    knownFsFilesToParsedResults |> AMap.tryFindAO filePath

  let getTypeCheckResults filePath =
    knownFsFilesToCheckedFilesResults |> AMap.tryFindAO (filePath)


  let getRecentTypeCheckResults filePath =
    knownFsFilesToRecentCheckedFilesResults |> AMap.tryFindAO (filePath)


  let tryGetLineStr pos (text: NamedText) =
    text.GetLine(pos)
    |> Result.ofOption (fun () -> $"No line in {text.FileName} at position {pos}")

  let tryGetParseResults filePath =
    getParseResults filePath
    |> AVal.force
    |> Result.ofOption (fun () -> $"No parse results for {filePath}")

  let tryGetTypeCheckResults filePath =
    let tyResults = getTypeCheckResults (filePath)

    match getRecentTypeCheckResults filePath |> AVal.force with
    | Some s ->
      if lock tyResults (fun () -> tyResults.OutOfDate) then
        Async.Start(async { tyResults |> AVal.force |> ignore })

      Some s
    | None -> tyResults |> AVal.force
    |> Result.ofOption (fun () -> $"No typecheck results for {filePath}")

  let knownFsFilesToCheckedDeclarations =
    knownFsFilesToCheckedFilesResults
    |> AMap.map' (AVal.mapOption (fun parseAndCheck -> parseAndCheck.GetParseResults.GetNavigationItems().Declarations))

  let getDeclarations filename =
    knownFsFilesToCheckedDeclarations |> AMap.tryFindAO (filename)

  let getFilePathAndPosition (p: ITextDocumentPositionParams) =
    let filePath = p.GetFilePath() |> Utils.normalizePath
    let pos = p.GetFcsPos()
    filePath, pos

  let codefixes =
    let getFileLines = tryGetFileSource

    let tryGetParseResultsForFile filePath pos =
      asyncResult {
        let! (file, _) = tryGetFile filePath
        let! lineStr = file.Lines |> tryGetLineStr pos
        let! tyRes = tryGetTypeCheckResults filePath
        return tyRes, lineStr, file.Lines
      }

    let getRangeText fileName (range: Ionide.LanguageServerProtocol.Types.Range) =
      getFileLines fileName
      |> Result.bind (fun lines -> lines.GetText(protocolRangeToRange (UMX.untag fileName) range))

    let tryFindUnionDefinitionFromPos _ =
      failwith "tryFindUnionDefinitionFromPos"

    let getUnionPatternMatchCases tyRes pos lines line =
      Commands.getUnionPatternMatchCases tryFindUnionDefinitionFromPos tyRes pos lines line

    let unionCaseStubReplacements (config) () =
      Map.ofList [ "$1", config.UnionCaseStubGenerationBody ]

    let tryGetProjectOptions filePath =
      getProjectOptionsForFile filePath
      |> AVal.force
      |> Seq.tryHead
      |> Result.ofOption (fun () -> $"Could not find project containing {filePath}")

    let implementInterfaceConfig config () : ImplementInterface.Config =
      { ObjectIdentifier = config.InterfaceStubGenerationObjectIdentifier
        MethodBody = config.InterfaceStubGenerationMethodBody
        IndentationSize = config.IndentationSize }

    let recordStubReplacements config () =
      Map.ofList [ "$1", config.RecordStubGenerationBody ]

    let tryFindRecordDefinitionFromPos _ _ =
      failwith "tryFindRecordDefinitionFromPos"

    let getRecordStub tyRes pos lines line =
      Commands.getRecordStub (tryFindRecordDefinitionFromPos) tyRes pos lines line

    let getLineText (lines: NamedText) (range: Ionide.LanguageServerProtocol.Types.Range) =
      lines.GetText(protocolRangeToRange (UMX.untag lines.FileName) range)

    let abstractClassStubReplacements config () =
      Map.ofList
        [ "$objectIdent", config.AbstractClassStubGenerationObjectIdentifier
          "$methodBody", config.AbstractClassStubGenerationMethodBody ]

    let tryFindAbstractClassExprInBufferAtPos _ =
      failwith "tryFindAbstractClassExprInBufferAtPos"

    let writeAbstractClassStub _ = failwith "writeAbstractClassStub"

    let getAbstractClassStub tyRes objExprRange lines lineStr =
      Commands.getAbstractClassStub
        tryFindAbstractClassExprInBufferAtPos
        writeAbstractClassStub
        tyRes
        objExprRange
        lines
        lineStr
      |> AsyncResult.foldResult id id

    config
    |> AVal.map (fun config ->
      [| Run.ifEnabled (fun _ -> config.UnusedOpensAnalyzer) (RemoveUnusedOpens.fix getFileLines)
         Run.ifEnabled
           (fun _ -> config.ResolveNamespaces)
           (ResolveNamespace.fix tryGetParseResultsForFile Commands.getNamespaceSuggestions)
         ReplaceWithSuggestion.fix
         RemoveRedundantQualifier.fix
         Run.ifEnabled (fun _ -> config.UnusedDeclarationsAnalyzer) (RenameUnusedValue.fix tryGetParseResultsForFile)
         AddNewKeywordToDisposableConstructorInvocation.fix getRangeText
         //  Run.ifEnabled
         //    (fun _ -> config.UnionCaseStubGeneration)
         //    (GenerateUnionCases.fix
         //      getFileLines
         //      tryGetParseResultsForFile
         //      getUnionPatternMatchCases
         //      (unionCaseStubReplacements config))
         ExternalSystemDiagnostics.linter
         ExternalSystemDiagnostics.analyzers
         Run.ifEnabled
           (fun _ -> config.InterfaceStubGeneration)
           (ImplementInterface.fix tryGetParseResultsForFile tryGetProjectOptions (implementInterfaceConfig config))
         //  Run.ifEnabled
         //    (fun _ -> config.RecordStubGeneration)
         //    (GenerateRecordStub.fix tryGetParseResultsForFile getRecordStub (recordStubReplacements config))
         //  Run.ifEnabled
         //    (fun _ -> config.AbstractClassStubGeneration)
         //    (GenerateAbstractClassStub.fix
         //      tryGetParseResultsForFile
         //      getAbstractClassStub
         //      (abstractClassStubReplacements config))
         AddMissingEqualsToTypeDefinition.fix getFileLines
         ChangePrefixNegationToInfixSubtraction.fix getFileLines
         ConvertDoubleEqualsToSingleEquals.fix getRangeText
         ChangeEqualsInFieldTypeToColon.fix
         WrapExpressionInParentheses.fix getRangeText
         ChangeRefCellDerefToNot.fix tryGetParseResultsForFile
         ChangeDowncastToUpcast.fix getRangeText
         MakeDeclarationMutable.fix tryGetParseResultsForFile tryGetProjectOptions
         UseMutationWhenValueIsMutable.fix tryGetParseResultsForFile
         ConvertInvalidRecordToAnonRecord.fix tryGetParseResultsForFile
         RemoveUnnecessaryReturnOrYield.fix tryGetParseResultsForFile getLineText
         ConvertCSharpLambdaToFSharpLambda.fix tryGetParseResultsForFile getLineText
         AddMissingFunKeyword.fix getFileLines getLineText
         MakeOuterBindingRecursive.fix tryGetParseResultsForFile getLineText
         AddMissingRecKeyword.fix getFileLines getLineText
         ConvertBangEqualsToInequality.fix getRangeText
         ChangeDerefBangToValue.fix tryGetParseResultsForFile getLineText
         RemoveUnusedBinding.fix tryGetParseResultsForFile
         AddTypeToIndeterminateValue.fix tryGetParseResultsForFile tryGetProjectOptions
         ChangeTypeOfNameToNameOf.fix tryGetParseResultsForFile
         AddMissingInstanceMember.fix
         AddExplicitTypeAnnotation.fix tryGetParseResultsForFile
         ConvertPositionalDUToNamed.fix tryGetParseResultsForFile getRangeText
         UseTripleQuotedInterpolation.fix tryGetParseResultsForFile getRangeText
         RenameParamToMatchSignature.fix tryGetParseResultsForFile |])

  let forgetDocument (uri: DocumentUri) =
    let filePath = uri |> Path.FileUriToLocalPath |> Utils.normalizePath

    let doesNotExist (file: string<LocalPath>) = not (File.Exists(UMX.untag file))

    let isOutsideWorkspace (file: string<LocalPath>) =
      rootPath
      |> AVal.map (fun rootPath ->
        match rootPath with
        | None -> true // no root workspace specified
        | Some rootPath ->
          let rec isInside (rootDir: DirectoryInfo, dirToCheck: DirectoryInfo) =
            if String.Equals(rootDir.FullName, dirToCheck.FullName, StringComparison.InvariantCultureIgnoreCase) then
              true
            else
              match dirToCheck.Parent with
              | null -> false
              | parent -> isInside (rootDir, parent)

          let rootDir = DirectoryInfo(rootPath)
          let fileDir = FileInfo(UMX.untag file).Directory

          if isInside (rootDir, fileDir) then
            false
          else
            getProjectOptionsForFile file
            |> AVal.map List.tryHead
            |> AVal.map (fun projectOptions ->
              match projectOptions with
              | None -> true
              | Some projectOptions ->
                if doesNotExist (UMX.tag projectOptions.ProjectFileName) then
                  true // script file
                else
                  // issue: fs-file does never get removed from project options (-> requires reload of FSAC to register)
                  // -> don't know if file still part of project (file might have been removed from project)
                  // -> keep cache for file
                  false)
            |> AVal.force)
      |> AVal.force

    if doesNotExist filePath || isOutsideWorkspace filePath then
      logger.info (
        Log.setMessage "Removing cached data for {file}."
        >> Log.addContext "file" filePath
      )

      transact (fun () -> openFiles.Remove filePath |> ignore)
      diagnosticCollections.ClearFor(uri)
    else
      logger.info (
        Log.setMessage "File {file} exists inside workspace so diagnostics will not be cleared"
        >> Log.addContext "file" filePath
      )


  member private x.handleSemanticTokens (filePath: string<LocalPath>) range : LspResult<SemanticTokens option> =
    result {

      let! tyRes = tryGetTypeCheckResults filePath |> Result.ofStringErr
      let r = tyRes.GetCheckResults.GetSemanticClassification(range)
      let filteredRanges = Commands.scrubRanges r


      let lspTypedRanges =
        filteredRanges
        |> Array.map (fun item ->
          let ty, mods = ClassificationUtils.map item.Type
          struct (fcsRangeToLsp item.Range, ty, mods))

      match encodeSemanticHighlightRanges lspTypedRanges with
      | None -> return None
      | Some encoded -> return (Some { Data = encoded; ResultId = None }) // TODO: provide a resultId when we support delta ranges
    }


  member __.HandleFormatting
    (
      fileName: string<LocalPath>,
      action: unit -> Async<Result<FormatDocumentResponse, string>>,
      handlerFormattedDoc: (NamedText * string) -> TextEdit[],
      handleFormattedRange: (NamedText * string * FormatSelectionRange) -> TextEdit[]
    ) : Async<LspResult<option<_>>> =
    asyncResult {
      try
        let! res = action () |> AsyncResult.ofStringErr

        let rootPath = rootPath |> AVal.force

        match res with
        | (FormatDocumentResponse.Formatted (lines, formatted)) ->
          let result = handlerFormattedDoc (lines, formatted)

          return (Some(result))
        | (FormatDocumentResponse.FormattedRange (lines, formatted, range)) ->
          let result = handleFormattedRange (lines, formatted, range)

          return (Some(result))
        | FormatDocumentResponse.Ignored ->
          let fileName = UMX.untag fileName |> Path.GetFileName

          do!
            lspClient.WindowShowMessage
              { Type = MessageType.Info
                Message = (sprintf "\"%s\" is ignored by a .fantomasignore file." fileName) }

          return None
        | FormatDocumentResponse.UnChanged -> return None
        | FormatDocumentResponse.ToolNotPresent ->
          let actions =
            [| if Option.isSome rootPath then
                 { Title = "Install locally" }
                 { Title = "Install globally" } |]

          let! response =
            lspClient.WindowShowMessageRequest
              { Type = MessageType.Warning
                Message = "No Fantomas install was found."
                Actions = Some actions }

          match response with
          | (Some { Title = "Install locally" }) ->
            do!
              rootPath
              |> Option.map (fun rootPath ->
                async {
                  let dotConfig = Path.Combine(rootPath, ".config", "dotnet-tools.json")

                  if not (File.Exists dotConfig) then
                    let! result =
                      Cli.Wrap("dotnet").WithArguments("new tool-manifest").WithWorkingDirectory(
                        rootPath
                      )
                        .ExecuteBufferedAsync()
                        .Task
                      |> Async.AwaitTask

                    if result.ExitCode <> 0 then
                      fantomasLogger.warn (
                        Log.setMessage (sprintf "Unable to create a new tool manifest in %s" rootPath)
                      )
                  else
                    let dotConfigContent = File.ReadAllText dotConfig

                    if dotConfigContent.Contains("fantomas-tool") then
                      // uninstall a older, non-compatible version of fantomas-tool
                      let! result =
                        Cli.Wrap("dotnet").WithArguments(
                          "tool uninstall fantomas-tool"
                        )
                          .WithWorkingDirectory(
                          rootPath
                        )
                          .ExecuteBufferedAsync()
                          .Task
                        |> Async.AwaitTask

                      if result.ExitCode <> 0 then
                        fantomasLogger.warn (
                          Log.setMessage (
                            sprintf "Unable to uninstall a non compatible version of fantomas-tool in %s" rootPath
                          )
                        )

                  let! result =
                    Cli.Wrap("dotnet").WithArguments(
                      "tool install fantomas-tool"
                    )
                      .WithWorkingDirectory(
                      rootPath
                    )
                      .ExecuteBufferedAsync()
                      .Task
                    |> Async.AwaitTask

                  if result.ExitCode = 0 then
                    fantomasLogger.info (Log.setMessage (sprintf "fantomas was installed locally at %A" rootPath))

                    do!
                      lspClient.WindowShowMessage
                        { Type = MessageType.Info
                          Message = "fantomas-tool was installed locally" }

                    fantomasService.ClearCache()
                  else
                    fantomasLogger.warn (
                      Log.setMessage (sprintf "Unable to install a compatible version of fantomas-tool in %s" rootPath)
                    )
                }

              )
              |> Option.defaultValue (async { return () })
          | (Some { Title = "Install globally" }) ->
            let! result =
              Cli.Wrap("dotnet").WithArguments(
                "tool install -g fantomas-tool"
              )
                .ExecuteBufferedAsync()
                .Task
              |> Async.AwaitTask

            if result.ExitCode = 0 then
              fantomasLogger.info (Log.setMessage "fantomas was installed globally")

              do!
                lspClient.WindowShowMessage
                  { Type = MessageType.Info
                    Message = "fantomas-tool was installed globally" }

              fantomasService.ClearCache()
            else
              fantomasLogger.warn (Log.setMessage "Unable to install a compatible version of fantomas-tool globally")
          | _ -> ()

          return! LspResult.internalError "Fantomas install not found."
        | (FormatDocumentResponse.Error ex) -> return! LspResult.internalError ex
      with e ->
        logger.error (Log.setMessage "HandleFormatting Request Errored {p}" >> Log.addExn e)
        return! LspResult.internalError (string e)
    }

  interface IFSharpLspServer with
    override x.Shutdown() =
      (x :> System.IDisposable).Dispose() |> async.Return

    override _.Initialize(p: InitializeParams) =
      asyncResult {
        try
          logger.info (Log.setMessage "Initialize Request {p}" >> Log.addContextDestructured "p" p)

          let c =
            p.InitializationOptions
            |> Option.bind (fun options -> if options.HasValues then Some options else None)
            |> Option.map Server.deserialize<FSharpConfigDto>
            |> Option.map FSharpConfig.FromDto
            |> Option.defaultValue FSharpConfig.Default

          let actualRootPath =
            match p.RootUri with
            | Some rootUri -> Some(Path.FileUriToLocalPath rootUri)
            | None -> p.RootPath

          let projs =
            match actualRootPath with
            | None -> []
            | Some actualRootPath ->
              let peeks =
                WorkspacePeek.peek
                  actualRootPath
                  c.WorkspaceModePeekDeepLevel
                  (c.ExcludeProjectDirectories |> List.ofArray)
                |> List.map Workspace.mapInteresting
                |> List.sortByDescending (fun x ->
                  match x with
                  | CommandResponse.WorkspacePeekFound.Solution sln -> Workspace.countProjectsInSln sln
                  | CommandResponse.WorkspacePeekFound.Directory _ -> -1)

              logger.info (
                Log.setMessage "Choosing from interesting items {items}"
                >> Log.addContextDestructured "items" peeks
              )

              match peeks with
              | [] -> []
              | [ CommandResponse.WorkspacePeekFound.Directory projs ] -> projs.Fsprojs
              | CommandResponse.WorkspacePeekFound.Solution sln :: _ ->
                sln.Items |> List.collect Workspace.foldFsproj |> List.map fst
              | _ -> []
              |> List.map (Utils.normalizePath)



          transact (fun () ->
            rootPath.Value <- actualRootPath
            clientCapabilities.Value <- p.Capabilities
            config.Value <- c
            workspacePaths.Value <- WorkspaceChosen.Projs(HashSet.ofList projs))


          return
            { InitializeResult.Default with
                Capabilities =
                  { ServerCapabilities.Default with
                      HoverProvider = Some true
                      RenameProvider = Some(U2.First true)
                      DefinitionProvider = Some true
                      TypeDefinitionProvider = Some true
                      ImplementationProvider = Some true
                      ReferencesProvider = Some true
                      DocumentHighlightProvider = Some true
                      DocumentSymbolProvider = Some true
                      WorkspaceSymbolProvider = Some true
                      DocumentFormattingProvider = Some true
                      DocumentRangeFormattingProvider = Some true
                      SignatureHelpProvider =
                        Some
                          { TriggerCharacters = Some [| '('; ','; ' ' |]
                            RetriggerCharacters = Some [| ','; ')'; ' ' |] }
                      CompletionProvider =
                        Some
                          { ResolveProvider = Some true
                            TriggerCharacters = Some([| '.'; ''' |])
                            AllCommitCharacters = None //TODO: what chars shoudl commit completions?
                          }
                      CodeLensProvider = Some { CodeLensOptions.ResolveProvider = Some true }
                      CodeActionProvider =
                        Some
                          { CodeActionKinds = None
                            ResolveProvider = None }
                      TextDocumentSync =
                        Some
                          { TextDocumentSyncOptions.Default with
                              OpenClose = Some true
                              Change = Some TextDocumentSyncKind.Full
                              Save = Some { IncludeText = Some true } }
                      FoldingRangeProvider = Some true
                      SelectionRangeProvider = Some true
                      SemanticTokensProvider =
                        Some
                          { Legend =
                              createTokenLegend<ClassificationUtils.SemanticTokenTypes, ClassificationUtils.SemanticTokenModifier>
                            Range = Some true
                            Full = Some(U2.First true) }
                      InlayHintProvider =
                        // None
                        // TODO: Fix bugs and performance
                        // FsAutoComplete.Core.Workaround.ServiceParseTreeWalk+SyntaxTraversal+defaultTraverse causing stack overflows
                        Some { ResolveProvider = Some false } } }

        with e ->
          logger.error (
            Log.setMessage "Initialize Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! LspResult.internalError (string e)
      }

    override __.Initialized(p: InitializedParams) =
      async {
        try
          logger.info (Log.setMessage "Initialized request {p}" >> Log.addContextDestructured "p" p)

          let options = loadedProjectOptions |> AVal.force

          return ()
        with e ->
          logger.error (
            Log.setMessage "Initialized Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return ()
      }

    override __.TextDocumentDidOpen(p: DidOpenTextDocumentParams) =
      async {
        try
          logger.info (
            Log.setMessage "TextDocumentDidOpen Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let doc = p.TextDocument
          let filePath = doc.GetFilePath() |> Utils.normalizePath
          let file = volaTileFile filePath doc.Text (Some doc.Version) DateTime.UtcNow
          updateOpenFiles file
          let _ = tryGetTypeCheckResults filePath
          return ()
        with e ->
          logger.error (
            Log.setMessage "TextDocumentDidOpen Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return ()
      }

    override __.TextDocumentDidClose(p: DidCloseTextDocumentParams) =
      async {
        try
          logger.info (
            Log.setMessage "TextDocumentDidClose Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let doc = p.TextDocument
          forgetDocument doc.Uri

          return ()
        with e ->
          logger.error (
            Log.setMessage "TextDocumentDidClose Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return ()
      }

    override __.TextDocumentDidChange(p: DidChangeTextDocumentParams) =
      async {
        try
          logger.info (
            Log.setMessage "TextDocumentDidChange Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let filePath = p.TextDocument.GetFilePath() |> Utils.normalizePath

          let changes = p.ContentChanges |> Array.head

          let file =
            volaTileFile filePath changes.Text (p.TextDocument.Version) DateTime.UtcNow

          updateOpenFiles file
          let _ = tryGetTypeCheckResults filePath

          return ()
        with e ->
          logger.error (
            Log.setMessage "TextDocumentDidChange Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return ()

      }

    override __.TextDocumentDidSave(p) =
      async {
        try
          logger.info (
            Log.setMessage "TextDocumentDidSave Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let doc = p.TextDocument
          let filePath = doc.GetFilePath() |> Utils.normalizePath
          let file = volaTileFile filePath p.Text.Value None DateTime.UtcNow

          updateOpenFiles file
          let knownFiles = openFilesA |> AMap.force

          logger.info (
            Log.setMessage "typechecking for files {files}"
            >> Log.addContextDestructured "files" knownFiles
          )

          let checker = (AVal.force checker)

          for (file, aFile) in knownFiles do
            let (_, cts) = aFile |> AVal.force

            forceTypeCheck checker file
            |> Async.RunSynchronouslyWithCTSafe(fun () -> cts.Token)
            |> ignore<unit option>

          return ()
        with e ->
          logger.error (
            Log.setMessage "TextDocumentDidSave Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )


        return ()
      }

    override __.TextDocumentCompletion(p: CompletionParams) =
      Debug.measureAsync "TextDocumentCompletion"
      <| asyncResult {
        try
          logger.info (
            Log.setMessage "TextDocumentCompletion Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let (filePath, pos) = getFilePathAndPosition p

          let! (namedText, _) = tryGetFile filePath |> Result.ofStringErr

          let! lineStr = namedText.Lines |> tryGetLineStr pos |> Result.ofStringErr

          let completionList =
            { IsIncomplete = false
              Items = KeywordList.hashSymbolCompletionItems }

          if lineStr.StartsWith "#" then
            let completionList =
              { IsIncomplete = false
                Items = KeywordList.hashSymbolCompletionItems }

            return! success (Some completionList)
          else
            let config = AVal.force config

            let rec retryAsyncOption (delay: TimeSpan) timesLeft action =
              async {
                match! action with
                | Ok x -> return Ok x
                | Error _ when timesLeft >= 0 ->
                  do! Async.Sleep(delay)
                  return! retryAsyncOption delay (timesLeft - 1) action
                | Error e -> return Error e
              }

            let getCompletions =
              asyncResult {
                let! typeCheckResults = tryGetTypeCheckResults filePath

                let getAllSymbols () =
                  if config.ExternalAutocomplete then
                    typeCheckResults.GetAllEntities true
                  else
                    []

                match!
                  Debug.measure "TextDocumentCompletion.TryGetCompletions" (fun () ->
                    typeCheckResults.TryGetCompletions pos lineStr None getAllSymbols)
                with
                | None -> return None
                | Some (decls, residue, shouldKeywords) ->
                  return Some(decls, residue, shouldKeywords, typeCheckResults, getAllSymbols)
              }

            match!
              retryAsyncOption (TimeSpan.FromMilliseconds(100.)) 5 getCompletions
              |> AsyncResult.ofStringErr
            with
            | None -> return! success (Some completionList)
            | Some (decls, residue, shouldKeywords, typeCheckResults, getAllSymbols) ->

              return!
                Debug.measure "TextDocumentCompletion.TryGetCompletions success"
                <| fun () ->
                     transact (fun () ->
                       HashMap.OfList(
                         [ for d in decls do
                             d.Name, (d, pos, filePath, namedText.Lines.GetLine, typeCheckResults.GetAST) ]
                       )
                       |> autoCompleteItems.UpdateTo)
                     |> ignore

                     let includeKeywords = config.KeywordsAutocomplete && shouldKeywords

                     let items =
                       decls
                       |> Array.mapi (fun id d ->
                         let code =
                           if System.Text.RegularExpressions.Regex.IsMatch(d.Name, """^[a-zA-Z][a-zA-Z0-9']+$""") then
                             d.Name
                           elif d.NamespaceToOpen.IsSome then
                             d.Name
                           else
                             FSharpKeywords.AddBackticksToIdentifierIfNeeded d.Name

                         let label =
                           match d.NamespaceToOpen with
                           | Some no -> sprintf "%s (open %s)" d.Name no
                           | None -> d.Name

                         { CompletionItem.Create(d.Name) with
                             Kind = (AVal.force glyphToCompletionKind) d.Glyph
                             InsertText = Some code
                             SortText = Some(sprintf "%06d" id)
                             FilterText = Some d.Name })

                     let its =
                       if not includeKeywords then
                         items
                       else
                         Array.append items KeywordList.keywordCompletionItems

                     let completionList = { IsIncomplete = false; Items = its }
                     success (Some completionList)

        with e ->
          logger.error (
            Log.setMessage "TextDocumentCompletion Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! LspResult.internalError (string e)
      }

    override __.CompletionItemResolve(ci: CompletionItem) =
      let mapHelpText (ci: CompletionItem) (text: HelpText) =
        match text with
        | HelpText.Simple (symbolName, text) ->
          let d = Documentation.Markup(markdown text)

          { ci with
              Detail = Some symbolName
              Documentation = Some d }
        | HelpText.Full (name, tip, additionalEdit) ->
          let (si, comment) = TipFormatter.formatCompletionItemTip tip

          let edits, label =
            match additionalEdit with
            | None -> None, ci.Label
            | Some { Namespace = ns; Position = fcsPos } ->
              let text =
                let indentation = String(' ', fcsPos.Column)
                $"{indentation}open {ns}\n"

              let insertPos = { (fcsPos |> fcsPosToLsp) with Character = 0 }

              Some
                [| { TextEdit.NewText = text
                     TextEdit.Range = { Start = insertPos; End = insertPos } } |],
              $"{ci.Label} (open {ns})"

          let d = Documentation.Markup(markdown comment)

          { ci with
              Detail = Some si
              Documentation = Some d
              AdditionalTextEdits = edits
              Label = label }

      let helpText sym =
        match KeywordList.keywordDescriptions.TryGetValue sym with
        | true, s -> CoreResponse.Res(HelpText.Simple(sym, s))
        | _ ->
          match KeywordList.hashDirectives.TryGetValue sym with
          | true, s -> CoreResponse.Res(HelpText.Simple(sym, s))
          | _ ->
            let sym =
              if sym.StartsWith "``" && sym.EndsWith "``" then
                sym.TrimStart([| '`' |]).TrimEnd([| '`' |])
              else
                sym

            let decls = knownFsFilesToCheckedDeclarations |> AMap.force |> Seq.map (snd)

            match getAutoCompleteByDeclName sym |> AVal.force with
            | None -> //Isn't in sync filled cache, we don't have result
              CoreResponse.ErrorRes(sprintf "No help text available for symbol '%s'" sym)
            | Some (decl, pos, fn, _, _) -> //Is in sync filled cache, try to get results from async filled caches or calculate if it's not there

              let tip = decl.Description

              let n =
                match getAutoCompleteNamespacesByDeclName sym |> AVal.force with
                | None -> None
                | Some s -> Some s

              CoreResponse.Res(HelpText.Full(sym, tip, n))

      Debug.measureAsync "CompletionItemResolve"
      <| asyncResult {
        try
          logger.info (
            Log.setMessage "CompletionItemResolve Request: {parms}"
            >> Log.addContextDestructured "parms" ci
          )

          return!
            match ci.InsertText with
            | None -> LspResult.internalError "No InsertText"
            | Some insertText ->
              helpText insertText
              |> Result.ofCoreResponse
              |> Result.fold (mapHelpText ci) (fun _ -> ci)
              |> success

        with e ->
          logger.error (
            Log.setMessage "CompletionItemResolve Request Errored {p}"
            >> Log.addContextDestructured "p" ci
            >> Log.addExn e
          )

          return! LspResult.internalError (string e)
      }

    override x.TextDocumentSignatureHelp(p: SignatureHelpParams) =
      asyncResult {
        try
          logger.info (
            Log.setMessage "TextDocumentSignatureHelp Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )


          let (filePath, pos) = getFilePathAndPosition p
          let! (namedText, _) = tryGetFile filePath |> Result.ofStringErr

          let! lineStr = namedText.Lines |> tryGetLineStr pos |> Result.ofStringErr
          let! tyRes = tryGetTypeCheckResults filePath |> Result.ofStringErr

          let charAtCaret = p.Context |> Option.bind (fun c -> c.TriggerCharacter)

          match!
            SignatureHelp.getSignatureHelpFor (tyRes, pos, namedText.Lines, charAtCaret, None)
            |> AsyncResult.ofStringErr
          with
          | None ->
            ()
            return! success None
          | Some sigHelp ->
            // sigHelpKind <- Some sigHelp.SigHelpKind

            let sigs =
              sigHelp.Methods
              |> Array.map (fun m ->
                let (signature, comment) = TipFormatter.formatPlainTip m.Description

                let parameters =
                  m.Parameters
                  |> Array.map (fun p ->
                    { ParameterInformation.Label = p.ParameterName
                      Documentation = Some(Documentation.String p.CanonicalTypeTextForSorting) })

                let d = Documentation.Markup(markdown comment)

                { SignatureInformation.Label = signature
                  Documentation = Some d
                  Parameters = Some parameters })

            let res =
              { Signatures = sigs
                ActiveSignature = sigHelp.ActiveOverload
                ActiveParameter = sigHelp.ActiveParameter }

            return! success (Some res)
        with e ->
          logger.error (
            Log.setMessage "TextDocumentSignatureHelp Request: {parms}"
            >> Log.addContextDestructured "parms" p
            >> Log.addExn e
          )

          return! LspResult.internalError (string e)
      }

    override x.TextDocumentHover(p: TextDocumentPositionParams) =
      asyncResult {
        try
          logger.info (
            Log.setMessage "TextDocumentHover Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let (filePath, pos) = getFilePathAndPosition p
          let! (namedText, _) = tryGetFile filePath |> Result.ofStringErr
          let! lineStr = namedText.Lines |> tryGetLineStr pos |> Result.ofStringErr
          let! tyRes = tryGetTypeCheckResults filePath |> Result.ofStringErr

          match tyRes.TryGetToolTipEnhanced pos lineStr with
          | Ok (Some (tip, signature, footer, typeDoc)) ->
            let formatCommentStyle =
              let config = AVal.force config

              if config.TooltipMode = "full" then
                TipFormatter.FormatCommentStyle.FullEnhanced
              else if config.TooltipMode = "summary" then
                TipFormatter.FormatCommentStyle.SummaryOnly
              else
                TipFormatter.FormatCommentStyle.Legacy

            match TipFormatter.formatTipEnhanced tip signature footer typeDoc formatCommentStyle with
            | (sigCommentFooter :: _) :: _ ->
              let signature, comment, footer = sigCommentFooter

              let markStr lang (value: string) =
                MarkedString.WithLanguage { Language = lang; Value = value }

              let fsharpBlock (lines: string[]) =
                lines |> String.concat Environment.NewLine |> markStr "fsharp"

              let sigContent =
                let lines =
                  signature.Split Environment.NewLine
                  |> Array.filter (not << String.IsNullOrWhiteSpace)

                match lines |> Array.splitAt (lines.Length - 1) with
                | (h, [| StartsWith "Full name:" fullName |]) ->
                  [| yield fsharpBlock h; yield MarkedString.String("*" + fullName + "*") |]
                | _ -> [| fsharpBlock lines |]


              let commentContent = comment |> MarkedString.String

              let footerContent =
                footer.Split Environment.NewLine
                |> Array.filter (not << String.IsNullOrWhiteSpace)
                |> Array.map (fun n -> MarkedString.String("*" + n + "*"))


              let response =
                { Contents = MarkedStrings [| yield! sigContent; yield commentContent; yield! footerContent |]
                  Range = None }

              return (Some response)
            | _ -> return None
          | Ok (None) ->

            return! LspResult.internalError $"No TryGetToolTipEnhanced results for {filePath}"
          | Error e ->
            logger.error (Log.setMessage "Failed with {error}" >> Log.addContext "error" e)
            return! LspResult.internalError e
        with e ->
          logger.error (
            Log.setMessage "TextDocumentHover Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! LspResult.internalError (string e)
      }

    override x.TextDocumentRename(p: RenameParams) =
      asyncResult {
        try
          logger.info (
            Log.setMessage "TextDocumentRename Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let (filePath, pos) = getFilePathAndPosition p
          let! (namedText, _) = tryGetFile filePath |> Result.ofStringErr
          let! lineStr = namedText.Lines |> tryGetLineStr pos |> Result.ofStringErr
          let! tyRes = tryGetTypeCheckResults filePath |> Result.ofStringErr
          let checker = checker |> AVal.force

          let findReferencesForSymbolInFile (file, project, symbol) =
            checker.FindReferencesForSymbolInFile(file, project, symbol)



          let getProjectOptions file =
            getProjectOptionsForFile file |> AVal.force |> List.tryHead

          let projectsThatContainFile file =
            getProjectOptionsForFile file |> AVal.force

          let getDependentProjectsOfProjects ps =
            let projectSnapshot = loadedProjectOptions |> AVal.force
            let allDependents = System.Collections.Generic.HashSet<FSharpProjectOptions>()

            let currentPass = ResizeArray()
            currentPass.AddRange(ps |> List.map (fun p -> p.ProjectFileName))

            let mutable continueAlong = true

            while continueAlong do
              let dependents =
                projectSnapshot
                |> Seq.filter (fun p ->
                  p.ReferencedProjects
                  |> Seq.exists (fun r ->
                    match r.ProjectFilePath with
                    | None -> false
                    | Some p -> currentPass.Contains(p)))

              if Seq.isEmpty dependents then
                continueAlong <- false
                currentPass.Clear()
              else
                for d in dependents do
                  allDependents.Add d |> ignore<bool>

                currentPass.Clear()
                currentPass.AddRange(dependents |> Seq.map (fun p -> p.ProjectFileName))

            Seq.toList allDependents

          let getProjectOptionsForFsproj file =
            loadedProjectOptions
            |> AVal.force
            |> Seq.tryFind (fun x -> x.ProjectFileName = file)

          let getDeclarationLocation (symUse, text) =
            SymbolLocation.getDeclarationLocation (
              symUse,
              text,
              getProjectOptions,
              projectsThatContainFile,
              getDependentProjectsOfProjects
            )


          let symbolUseWorkspace pos lineStr namedText tyRes =
            Commands.symbolUseWorkspace
              getDeclarationLocation
              findReferencesForSymbolInFile
              tryGetFileSource
              getProjectOptionsForFsproj
              pos
              lineStr
              namedText
              tyRes

          let! documentsAndRanges =
            Commands.renameSymbol symbolUseWorkspace tryGetFileSource pos tyRes lineStr namedText.Lines
            |> AsyncResult.ofStringErr

          let documentChanges =
            documentsAndRanges
            |> Seq.map (fun (namedText, symbols) ->
              let edits =
                let newName =
                  p.NewName
                  |> FSharp.Compiler.Syntax.PrettyNaming.AddBackticksToIdentifierIfNeeded

                symbols
                |> Seq.map (fun sym ->
                  let range = fcsRangeToLsp sym
                  { Range = range; NewText = newName })
                |> Array.ofSeq

              let version =
                tryGetFile namedText.FileName
                |> Option.ofResult
                |> Option.bind (fun (f, _) -> f.Version)

              { TextDocument =
                  { Uri = Path.FilePathToUri(UMX.untag namedText.FileName)
                    Version = version }
                Edits = edits })
            |> Array.ofSeq

          let clientCapabilities = clientCapabilities |> AVal.force |> Option.get
          return WorkspaceEdit.Create(documentChanges, clientCapabilities) |> Some
        with e ->
          logger.error (
            Log.setMessage "TextDocumentRename Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! LspResult.internalError (string e)
      }

    override x.TextDocumentDefinition(p: TextDocumentPositionParams) =
      asyncResult {
        try
          logger.info (
            Log.setMessage "TextDocumentDefinition Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let (filePath, pos) = getFilePathAndPosition p
          let! (namedText, _) = tryGetFile filePath |> Result.ofStringErr

          let! lineStr = namedText.Lines |> tryGetLineStr pos |> Result.ofStringErr
          let! tyRes = tryGetTypeCheckResults filePath |> Result.ofStringErr
          let! decl = tyRes.TryFindDeclaration pos lineStr |> AsyncResult.ofStringErr
          return decl |> findDeclToLspLocation |> GotoResult.Single |> Some
        with e ->
          logger.error (
            Log.setMessage "TextDocumentDefinition Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! LspResult.internalError (string e)
      }

    override x.TextDocumentTypeDefinition(p: TextDocumentPositionParams) =
      asyncResult {
        try
          logger.info (
            Log.setMessage "TextDocumentTypeDefinition Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let (filePath, pos) = getFilePathAndPosition p

          let! (namedText, _) = tryGetFile filePath |> Result.ofStringErr
          let! lineStr = namedText.Lines |> tryGetLineStr pos |> Result.ofStringErr
          let! tyRes = tryGetTypeCheckResults filePath |> Result.ofStringErr
          let! decl = tyRes.TryFindTypeDeclaration pos lineStr |> AsyncResult.ofStringErr
          return decl |> findDeclToLspLocation |> GotoResult.Single |> Some
        with e ->
          logger.error (
            Log.setMessage "TextDocumentTypeDefinition Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! LspResult.internalError (string e)
      }

    override x.TextDocumentReferences(p: ReferenceParams) =
      asyncResult {
        try
          logger.info (
            Log.setMessage "TextDocumentReferences Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let (filePath, pos) = getFilePathAndPosition p
          let! (namedText, _) = tryGetFile filePath |> Result.ofStringErr
          let! lineStr = tryGetLineStr pos namedText.Lines |> Result.ofStringErr
          let! tyRes = tryGetTypeCheckResults filePath |> Result.ofStringErr
          let checker = checker |> AVal.force

          let findReferencesForSymbolInFile (file, project, symbol) =
            checker.FindReferencesForSymbolInFile(file, project, symbol)



          let getProjectOptions file =
            getProjectOptionsForFile file |> AVal.force |> List.tryHead

          let projectsThatContainFile file =
            getProjectOptionsForFile file |> AVal.force


          let getDependentProjectsOfProjects ps =
            let projectSnapshot = loadedProjectOptions |> AVal.force

            let allDependents = System.Collections.Generic.HashSet<FSharpProjectOptions>()

            let currentPass = ResizeArray()
            currentPass.AddRange(ps |> List.map (fun p -> p.ProjectFileName))

            let mutable continueAlong = true

            while continueAlong do
              let dependents =
                projectSnapshot
                |> Seq.filter (fun p ->
                  p.ReferencedProjects
                  |> Seq.exists (fun r ->
                    match r.ProjectFilePath with
                    | None -> false
                    | Some p -> currentPass.Contains(p)))

              if Seq.isEmpty dependents then
                continueAlong <- false
                currentPass.Clear()
              else
                for d in dependents do
                  allDependents.Add d |> ignore<bool>

                currentPass.Clear()
                currentPass.AddRange(dependents |> Seq.map (fun p -> p.ProjectFileName))

            Seq.toList allDependents

          let getProjectOptionsForFsproj file =
            loadedProjectOptions
            |> AVal.force
            |> Seq.tryFind (fun x -> x.ProjectFileName = file)
          // |> Option.filter (fun x -> x.ProjectFileName.EndsWith("fsproj"))

          let! usages =
            let getDeclarationLocation (symUse, text) =
              SymbolLocation.getDeclarationLocation (
                symUse,
                text,
                getProjectOptions,
                projectsThatContainFile,
                getDependentProjectsOfProjects
              )

            Commands.symbolUseWorkspace
              getDeclarationLocation
              findReferencesForSymbolInFile
              tryGetFileSource
              getProjectOptionsForFsproj
              pos
              lineStr
              namedText.Lines
              tyRes
            |> AsyncResult.mapError (JsonRpc.Error.InternalErrorMessage)

          match usages with
          | Choice1Of2 (decls, usages) ->
            return
              Seq.append decls.Values usages.Values
              |> Seq.collect (fun kvp -> kvp |> Array.map fcsRangeToLspLocation)
              |> Seq.toArray
              |> Some
          | Choice2Of2 combinedRanges ->
            return
              combinedRanges.Values
              |> Seq.collect (fun kvp -> kvp |> Array.map fcsRangeToLspLocation)
              |> Seq.toArray
              |> Some
        with e ->
          logger.error (
            Log.setMessage "TextDocumentReferences Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! LspResult.internalError (string e)
      }

    override x.TextDocumentDocumentHighlight(p) =
      asyncResult {
        try
          logger.info (
            Log.setMessage "TextDocumentDocumentHighlight Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let (filePath, pos) = getFilePathAndPosition p
          let! (namedText, _) = tryGetFile filePath |> Result.ofStringErr
          let! lineStr = tryGetLineStr pos namedText.Lines |> Result.ofStringErr
          let! tyRes = tryGetTypeCheckResults filePath |> Result.ofStringErr

          let! (symbol, uses) = tyRes.TryGetSymbolUseAndUsages pos lineStr |> Result.ofStringErr

          return
            uses
            |> Array.map (fun s ->
              { DocumentHighlight.Range = fcsRangeToLsp s.Range
                Kind = None })
            |> Some
        with e ->
          logger.error (
            Log.setMessage "TextDocumentDocumentHighlight Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! LspResult.internalError (string e)

      }

    override x.TextDocumentImplementation(p) =
      asyncResult {
        try
          logger.info (
            Log.setMessage "TextDocumentImplementation Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let (filePath, pos) = getFilePathAndPosition p
          let! (namedText, _) = tryGetFile filePath |> Result.ofStringErr
          let! lineStr = tryGetLineStr pos namedText.Lines |> Result.ofStringErr
          let! tyRes = tryGetTypeCheckResults filePath |> Result.ofStringErr

          logger.info (
            Log.setMessage "TextDocumentImplementation Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let getProjectOptions file =
            getProjectOptionsForFile file |> AVal.force |> List.head

          let checker = checker |> AVal.force

          let getUsesOfSymbol (filePath, opts: _ list, symbol: FSharpSymbol) =
            checker.GetUsesOfSymbol(filePath, opts, symbol)

          let getAllProjects () =
            knownFsFilesToProjectOptions
            |> AMap.force
            |> Seq.toList
            |> List.choose (fun (path, opt) ->
              option {
                let! opt = AVal.force opt |> List.tryHead
                return UMX.untag path, opt
              })

          let! res =
            Commands.symbolImplementationProject getProjectOptions getUsesOfSymbol getAllProjects tyRes pos lineStr
            |> AsyncResult.ofCoreResponse


          let ranges: FSharp.Compiler.Text.Range[] =
            match res with
            | LocationResponse.Use (_, uses) -> uses |> Array.map (fun u -> u.Range)

          let mappedRanges = ranges |> Array.map fcsRangeToLspLocation

          match mappedRanges with
          | [||] -> return None
          | [| single |] -> return Some(GotoResult.Single single)
          | multiple -> return Some(GotoResult.Multiple multiple)
        with e ->
          logger.error (
            Log.setMessage "TextDocumentImplementation Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! LspResult.internalError (string e)
      }

    override __.TextDocumentDocumentSymbol(p) =
      asyncResult {
        try
          logger.info (
            Log.setMessage "TextDocumentDocumentSymbol Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let fn = p.TextDocument.GetFilePath() |> Utils.normalizePath

          match getDeclarations fn |> AVal.force with
          | Some decls ->
            return
              glyphToSymbolKind
              |> AVal.map (fun glyphToSymbolKind ->
                decls
                |> Array.collect (fun top ->
                  getSymbolInformations p.TextDocument.Uri glyphToSymbolKind top (fun s -> true)))
              |> AVal.force
              |> U2.First
              |> Some
          | None -> return! LspResult.internalError $"No declarations for {fn}"
        with e ->
          logger.error (
            Log.setMessage "TextDocumentDocumentSymbol Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! LspResult.internalError (string e)
      }


    override __.WorkspaceSymbol(symbolRequest: WorkspaceSymbolParams) =
      asyncResult {
        try
          logger.info (
            Log.setMessage "WorkspaceSymbol Request: {parms}"
            >> Log.addContextDestructured "parms" symbolRequest
          )

          let glyphToSymbolKind = glyphToSymbolKind |> AVal.force

          let decls =
            knownFsFilesToCheckedDeclarations
            |> AMap.force
            |> Seq.toArray
            |> Array.map (fun (p, ns) -> p, AVal.force ns)
            |> Array.choose (fun (p, ns) -> ns |> Option.map (fun ns -> p, ns))

          let res =
            decls
            |> Array.collect (fun (p, ns) ->
              let uri = Path.LocalPathToUri p

              ns
              |> Array.collect (fun n -> getSymbolInformations uri glyphToSymbolKind n (applyQuery symbolRequest.Query)))
            |> Some

          return res
        with e ->
          logger.error (
            Log.setMessage "WorkspaceSymbol Request Errored {p}"
            >> Log.addContextDestructured "p" symbolRequest
            >> Log.addExn e
          )

          return! LspResult.internalError (string e)
      }

    override x.TextDocumentFormatting(p: DocumentFormattingParams) =
      asyncResult {
        try
          let doc = p.TextDocument
          let fileName = doc.GetFilePath() |> Utils.normalizePath

          let action () =
            logger.info (
              Log.setMessage "TextDocumentFormatting Request: {parms}"
              >> Log.addContextDestructured "parms" p
            )

            let tryGetFileCheckerOptionsWithLines file = tryGetFileSource file
            let formatDocumentAsync x = fantomasService.FormatDocumentAsync x
            Commands.formatDocument tryGetFileCheckerOptionsWithLines formatDocumentAsync fileName

          let handlerFormattedDoc (lines: NamedText, formatted: string) =
            let range =
              let zero = { Line = 0; Character = 0 }
              let lastPos = lines.LastFilePosition

              { Start = zero
                End = fcsPosToLsp lastPos }

            [| { Range = range; NewText = formatted } |]

          return! x.HandleFormatting(fileName, action, handlerFormattedDoc, (fun (_, _, _) -> [||]))
        with e ->
          logger.error (
            Log.setMessage "TextDocumentFormatting Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! LspResult.internalError (string e)
      }

    override x.TextDocumentRangeFormatting(p: DocumentRangeFormattingParams) =
      asyncResult {
        try
          let doc = p.TextDocument
          let fileName = doc.GetFilePath() |> Utils.normalizePath

          let action () =
            logger.info (
              Log.setMessage "TextDocumentRangeFormatting Request: {parms}"
              >> Log.addContextDestructured "parms" p
            )

            let range =
              FormatSelectionRange(
                p.Range.Start.Line + 1,
                p.Range.Start.Character,
                p.Range.End.Line + 1,
                p.Range.End.Character
              )

            let tryGetFileCheckerOptionsWithLines file = tryGetFileSource file
            let formatSelectionAsync x = fantomasService.FormatSelectionAsync x
            Commands.formatSelection tryGetFileCheckerOptionsWithLines formatSelectionAsync fileName range


          let handlerFormattedRangeDoc (lines: NamedText, formatted: string, range: FormatSelectionRange) =
            let range =
              { Start =
                  { Line = range.StartLine - 1
                    Character = range.StartColumn }
                End =
                  { Line = range.EndLine - 1
                    Character = range.EndColumn } }

            [| { Range = range; NewText = formatted } |]


          return! x.HandleFormatting(fileName, action, (fun (_, _) -> [||]), handlerFormattedRangeDoc)
        with e ->
          logger.error (
            Log.setMessage "TextDocumentRangeFormatting Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! LspResult.internalError (string e)
      }


    override x.TextDocumentCodeAction(codeActionParams: CodeActionParams) =
      asyncResult {
        try
          logger.info (
            Log.setMessage "TextDocumentCodeAction Request: {parms}"
            >> Log.addContextDestructured "parms" codeActionParams
          )

          let filePath = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath
          let! namedText = tryGetFile filePath |> Result.ofStringErr

          let! tyRes = tryGetTypeCheckResults filePath |> Result.ofStringErr

          let (fixes: Async<Result<Fix list, string>[]>) =
            codefixes
            |> AVal.force
            |> Array.map (fun codeFix -> codeFix codeActionParams)
            |> Async.Parallel

          let! fixes = fixes
          let (actions: Fix list[], errors: string[]) = Array.partitionResults fixes
          let actions = actions |> List.concat

          if errors.Length <> 0 then
            logger.trace (
              Log.setMessage
                "Errors while processing code action request for {file} with {context} at {range}: {errors}"
              >> Log.addContextDestructured "file" codeActionParams.TextDocument.Uri
              >> Log.addContextDestructured "context" codeActionParams.Context
              >> Log.addContextDestructured "range" codeActionParams.Range
              >> Log.addContextDestructured "errors" errors
            )

          let tryGetFileVersion filePath =
            tryGetFile filePath |> Option.ofResult |> Option.bind (fun (f, _) -> f.Version)

          let clientCapabilities = clientCapabilities |> AVal.force

          match actions with
          | [] -> return None
          | actions ->
            return
              actions
              |> List.map (CodeAction.OfFix tryGetFileVersion clientCapabilities.Value >> U2.Second)
              |> List.toArray
              |> Some
        with e ->
          logger.error (
            Log.setMessage "TextDocumentCodeAction Request Errored {p}"
            >> Log.addContextDestructured "p" codeActionParams
            >> Log.addExn e
          )

          return! LspResult.internalError (string e)
      }

    override __.TextDocumentCodeLens(p: CodeLensParams) =
      asyncResult {
        try
          logger.info (
            Log.setMessage "TextDocumentCodeLens Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let fn = p.TextDocument.GetFilePath() |> Utils.normalizePath

          match getDeclarations (fn) |> AVal.force with
          | None -> return None
          | Some decls ->
            let config = AVal.force config

            let res =
              [| if config.LineLens.Enabled <> "replaceCodeLens" then
                   if config.CodeLenses.Signature.Enabled then
                     yield! decls |> Array.collect (getCodeLensInformation p.TextDocument.Uri "signature")
                 // we have two options here because we're deprecating the EnableReferenceCodeLens one (namespacing, etc)
                 if config.EnableReferenceCodeLens || config.CodeLenses.References.Enabled then
                   yield! decls |> Array.collect (getCodeLensInformation p.TextDocument.Uri "reference") |]

            return Some res
        with e ->
          logger.error (
            Log.setMessage "TextDocumentCodeLens Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! LspResult.internalError (string e)
      }

    override __.CodeLensResolve(p) =
      // JB:TODO see how to reuse existing code
      logger.info (
        Log.setMessage "CodeLensResolve Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      let handler (f) (arg: CodeLens) : Async<LspResult<CodeLens>> =
        asyncResult {
          let pos = protocolPosToPos arg.Range.Start

          let data = arg.Data.Value.ToObject<string[]>()

          let filePath = Path.FileUriToLocalPath data.[0] |> Utils.normalizePath

          try
            let! tyRes = tryGetTypeCheckResults filePath |> Result.ofStringErr


            logger.info (
              Log.setMessage "CodeLensResolve - Cached typecheck results now available for {file}."
              >> Log.addContextDestructured "file" filePath
            )

            let! (namedText: NamedText) = tryGetFileSource filePath |> Result.ofStringErr
            let! lineStr = namedText |> tryGetLineStr pos |> Result.ofStringErr

            let typ = data.[1]
            let! r = Async.Catch(f arg pos tyRes namedText lineStr typ filePath)

            match r with
            | Choice1Of2 (r: LspResult<CodeLens option>) ->
              match r with
              | Ok (Some r) -> return r
              | _ -> return Unchecked.defaultof<_>
            | Choice2Of2 e ->
              logger.error (
                Log.setMessage "CodeLensResolve - Child operation failed for {file}"
                >> Log.addContextDestructured "file" filePath
                >> Log.addExn e
              )

              let title = if typ = "signature" then "" else "0 References"

              let codeLens =
                { p with
                    Command =
                      Some
                        { Title = title
                          Command = ""
                          Arguments = None } }

              return codeLens
          with e ->
            logger.error (
              Log.setMessage "CodeLensResolve - Operation failed on {file}"
              >> Log.addContextDestructured "file" filePath
              >> Log.addExn e
            )

            return! LspResult.internalError (string e)
        }

      let writePayload (sourceFile: string<LocalPath>, triggerPos: pos, usageLocations: range[]) =
        Some
          [| JToken.FromObject(Path.LocalPathToUri sourceFile)
             JToken.FromObject(fcsPosToLsp triggerPos)
             JToken.FromObject(usageLocations |> Array.map fcsRangeToLspLocation) |]

      handler
        (fun p pos tyRes lines lineStr typ file ->
          async {
            if typ = "signature" then
              match
                Debug.measure "TryGetSignatureData" (fun () ->
                  tyRes.TryGetSignatureData pos lineStr
                  |> Result.bimap CoreResponse.Res CoreResponse.ErrorRes)
              with
              | CoreResponse.InfoRes msg
              | CoreResponse.ErrorRes msg ->
                logger.error (
                  Log.setMessage "CodeLensResolve - error on file {file}"
                  >> Log.addContextDestructured "file" file
                  >> Log.addContextDestructured "error" msg
                )

                return { p with Command = None } |> Some |> success
              | CoreResponse.Res (typ, parms, _) ->
                let formatted = SigantureData.formatSignature typ parms

                let cmd =
                  { Title = formatted
                    Command = ""
                    Arguments = None }

                return { p with Command = Some cmd } |> Some |> success
            else
              let checker = checker |> AVal.force

              let findReferencesForSymbolInFile (file, project, symbol) =
                checker.FindReferencesForSymbolInFile(file, project, symbol)


              let getProjectOptions file =
                getProjectOptionsForFile file |> AVal.force |> List.tryHead

              let projectsThatContainFile file =
                getProjectOptionsForFile file |> AVal.force

              let getDependentProjectsOfProjects ps =
                let projectSnapshot = loadedProjectOptions |> AVal.force
                let allDependents = System.Collections.Generic.HashSet<FSharpProjectOptions>()

                let currentPass = ResizeArray()
                currentPass.AddRange(ps |> List.map (fun p -> p.ProjectFileName))

                let mutable continueAlong = true

                while continueAlong do
                  let dependents =
                    projectSnapshot
                    |> Seq.filter (fun p ->
                      p.ReferencedProjects
                      |> Seq.exists (fun r ->
                        match r.ProjectFilePath with
                        | None -> false
                        | Some p -> currentPass.Contains(p)))

                  if Seq.isEmpty dependents then
                    continueAlong <- false
                    currentPass.Clear()
                  else
                    for d in dependents do
                      allDependents.Add d |> ignore<bool>

                    currentPass.Clear()
                    currentPass.AddRange(dependents |> Seq.map (fun p -> p.ProjectFileName))

                Seq.toList allDependents

              let getProjectOptionsForFsproj file =
                loadedProjectOptions
                |> AVal.force
                |> Seq.tryFind (fun x -> x.ProjectFileName = file)

              let! res =
                let getDeclarationLocation (symUse, text) =
                  SymbolLocation.getDeclarationLocation (
                    symUse,
                    text,
                    getProjectOptions,
                    projectsThatContainFile,
                    getDependentProjectsOfProjects
                  )

                Commands.symbolUseWorkspace
                  getDeclarationLocation
                  findReferencesForSymbolInFile
                  tryGetFileSource
                  getProjectOptionsForFsproj
                  pos
                  lineStr
                  lines
                  tyRes
                |> AsyncResult.mapError (JsonRpc.Error.InternalErrorMessage)

              let res =
                match res with
                | Core.Result.Error msg ->
                  logger.error (
                    Log.setMessage "CodeLensResolve - error getting symbol use for {file} - {error}"
                    >> Log.addContextDestructured "file" file
                    >> Log.addContextDestructured "error" msg
                  )

                  success (
                    Some
                      { p with
                          Command =
                            Some
                              { Title = ""
                                Command = ""
                                Arguments = None } }
                  )
                | Ok res ->
                  match res with
                  | Choice1Of2 (_, uses) ->
                    let allUses = uses.Values |> Array.concat

                    let cmd =
                      if allUses.Length = 0 then
                        { Title = "0 References"
                          Command = ""
                          Arguments = None }
                      else
                        { Title = $"%d{allUses.Length} References"
                          Command = "fsharp.showReferences"
                          Arguments = writePayload (file, pos, allUses) }

                    { p with Command = Some cmd } |> Some |> success
                  | Choice2Of2 mixedUsages ->
                    // mixedUsages will contain the declaration, so we need to do a bit of work here
                    let allUses = mixedUsages.Values |> Array.concat

                    let cmd =
                      if allUses.Length <= 1 then
                        // 1 reference means that it's only the declaration, so it's actually 0 references
                        { Title = "0 References"
                          Command = ""
                          Arguments = None }
                      else
                        // multiple references means that the declaration _and_ the references are all present.
                        // this is kind of a pain, so for now at least, we just return all of them
                        { Title = $"%d{allUses.Length - 1} References"
                          Command = "fsharp.showReferences"
                          Arguments = writePayload (file, pos, allUses) }

                    { p with Command = Some cmd } |> Some |> success

              return res
          })
        p

    override __.WorkspaceDidChangeWatchedFiles(p) =
      async {
        try
          logger.info (
            Log.setMessage "WorkspaceDidChangeWatchedFiles Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          p.Changes
          |> Array.iter (fun c ->
            if c.Type = FileChangeType.Deleted then
              forgetDocument c.Uri

            ())
        with e ->
          logger.error (
            Log.setMessage "WorkspaceDidChangeWatchedFiles Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )
      }

    override __.WorkspaceDidChangeConfiguration(p: DidChangeConfigurationParams) =
      async {
        try
          logger.info (
            Log.setMessage "WorkspaceDidChangeConfiguration Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let dto = p.Settings |> Server.deserialize<FSharpConfigRequest>
          let c = config |> AVal.force
          let c = c.AddDto dto.FSharp
          transact (fun () -> config.Value <- c)

        with e ->
          logger.error (
            Log.setMessage "WorkspaceDidChangeConfiguration Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )
      }

    override __.TextDocumentFoldingRange(rangeP: FoldingRangeParams) =
      asyncResult {
        try
          logger.info (
            Log.setMessage "TextDocumentFoldingRange Request: {parms}"
            >> Log.addContextDestructured "parms" rangeP
          )

          let file = rangeP.TextDocument.GetFilePath() |> Utils.normalizePath

          let getParseResultsForFile file =
            asyncResult {
              let! namedText = tryGetFileSource file
              let! parseResults = tryGetParseResults file
              return namedText, parseResults
            }

          let! scopes = Commands.scopesForFile getParseResultsForFile file |> AsyncResult.ofStringErr
          return scopes |> Seq.map Structure.toFoldingRange |> Set.ofSeq |> List.ofSeq |> Some
        with e ->
          logger.error (
            Log.setMessage "TextDocumentFoldingRange Request Errored {p}"
            >> Log.addContextDestructured "p" rangeP
            >> Log.addExn e
          )

          return! LspResult.internalError (string e)
      }

    override __.TextDocumentSelectionRange(selectionRangeP: SelectionRangeParams) =
      asyncResult {
        try
          logger.info (
            Log.setMessage "TextDocumentSelectionRange Request: {parms}"
            >> Log.addContextDestructured "parms" selectionRangeP
          )

          let rec mkSelectionRanges =
            function
            | [] -> None
            | r :: xs ->
              Some
                { Range = fcsRangeToLsp r
                  Parent = mkSelectionRanges xs }

          let file = selectionRangeP.TextDocument.GetFilePath() |> Utils.normalizePath

          let poss = selectionRangeP.Positions |> Array.map protocolPosToPos |> Array.toList

          let getParseResultsForFile file =
            asyncResult {
              let! parseResults = tryGetParseResults file
              return parseResults
            }

          let! ranges =
            Commands.getRangesAtPosition getParseResultsForFile file poss
            |> AsyncResult.ofStringErr

          return ranges |> List.choose mkSelectionRanges |> Some
        with e ->
          logger.error (
            Log.setMessage "TextDocumentSelectionRange Request Errored {p}"
            >> Log.addContextDestructured "p" selectionRangeP
            >> Log.addExn e
          )

          return! LspResult.internalError (string e)
      }

    override x.TextDocumentSemanticTokensFull(p: SemanticTokensParams) : AsyncLspResult<SemanticTokens option> =
      asyncResult {
        try
          logger.info (
            Log.setMessage "TextDocumentSemanticTokensFull request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let fn = p.TextDocument.GetFilePath() |> Utils.normalizePath
          return! x.handleSemanticTokens fn None

        with e ->
          logger.error (
            Log.setMessage "TextDocumentSemanticTokensFull Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! LspResult.internalError (string e)
      }



    override x.TextDocumentSemanticTokensRange(p: SemanticTokensRangeParams) : AsyncLspResult<SemanticTokens option> =
      asyncResult {
        try
          logger.info (
            Log.setMessage "TextDocumentSemanticTokensRange request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let fn = p.TextDocument.GetFilePath() |> Utils.normalizePath
          let fcsRange = protocolRangeToRange (UMX.untag fn) p.Range
          return! x.handleSemanticTokens fn (Some fcsRange)
        with e ->
          logger.error (
            Log.setMessage "TextDocumentSemanticTokensRange Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! LspResult.internalError (string e)
      }

    override x.TextDocumentInlayHint(p: InlayHintParams) : AsyncLspResult<InlayHint[] option> =
      asyncResult {
        try
          logger.info (
            Log.setMessage "TextDocumentInlayHint Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let filePath = p.TextDocument.GetFilePath() |> Utils.normalizePath
          let! (namedText, _) = tryGetFile filePath |> Result.ofStringErr

          let! tyRes = tryGetTypeCheckResults filePath |> Result.ofStringErr

          let fcsRange = protocolRangeToRange (UMX.untag filePath) p.Range
          let config = config |> AVal.force

          let! hints =
            Commands.InlayHints(
              namedText.Lines,
              tyRes,
              fcsRange,
              showTypeHints = config.InlayHints.typeAnnotations,
              showParameterHints = config.InlayHints.parameterNames
            )

          let getTooltip (h: InlayHints.Hint) =
            let hideTypeAnnotations = "fsharp.inlayHints.hideTypeAnnotations"
            let hideParameterNames = "fsharp.inlayHints.hideParameterNames"
            let hideAll = "fsharp.inlayHints.hideAll"
            let setToToggle = "fsharp.inlayHints.setToToggle"
            let disableLongTooltip = "fsharp.inlayHints.disableLongTooltip"

            if config.InlayHints.disableLongTooltip then
              h.Tooltip |> Option.map (InlayHintTooltip.String)
            else
              let lines = ResizeArray()

              let hideCommand =
                match h.Kind with
                | InlayHints.HintKind.Type -> hideTypeAnnotations
                | InlayHints.HintKind.Parameter -> hideParameterNames

              lines.Add $"To hide these hints, [click here](command:{hideCommand})."
              lines.Add $"To hide *ALL* hints, [click here](command:{hideAll})."

              // We don't have access to generic VSCode config so we don't know what inlay hints mode is used
              // if not isSetToToggle && toggleSupported then
              lines.Add
                $"Hints can also be hidden by default, and shown when Ctrl/Cmd+Alt is pressed. To do this, [click here](command:{setToToggle})."

              lines.Add $"Finally, to dismiss this long tooltip forever, [click here](command:{disableLongTooltip})."

              h.Tooltip
              |> Option.iter (fun t ->
                lines.Add ""
                lines.Add(t))

              String.concat "\n" lines |> markdown |> InlayHintTooltip.Markup |> Some

          let hints: InlayHint[] =
            hints
            |> Array.map (fun h ->
              { Position = fcsPosToLsp h.Pos
                Label = InlayHintLabel.String h.Text
                Kind =
                  match h.Kind with
                  | InlayHints.HintKind.Type -> Types.InlayHintKind.Type
                  | InlayHints.HintKind.Parameter -> Types.InlayHintKind.Parameter
                  |> Some
                TextEdits =
                  match h.Insertions with
                  | None -> None
                  | Some insertions ->
                    insertions
                    |> Array.map (fun insertion ->
                      { Range = fcsPosToProtocolRange insertion.Pos
                        NewText = insertion.Text })
                    |> Some
                Tooltip = getTooltip h
                PaddingLeft =
                  match h.Kind with
                  | InlayHints.HintKind.Type -> Some true
                  | _ -> None
                PaddingRight =
                  match h.Kind with
                  | InlayHints.HintKind.Parameter -> Some true
                  | _ -> None
                Data = None })

          return (Some hints)
        with e ->
          logger.error (
            Log.setMessage "TextDocumentSemanticTokensRange Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! LspResult.internalError (string e)
      }


    //unsupported -- begin
    override x.CodeActionResolve(p) =
      logger.info (
        Log.setMessage "CodeActionResolve Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      Helpers.notImplemented

    override x.DocumentLinkResolve(p) =
      logger.info (
        Log.setMessage "CodeActionResolve Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      Helpers.notImplemented

    override x.Exit() = Helpers.ignoreNotification

    override x.InlayHintResolve p =
      logger.info (
        Log.setMessage "InlayHintResolve Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      Helpers.notImplemented

    override x.TextDocumentDocumentColor p =
      logger.info (
        Log.setMessage "TextDocumentDocumentColor Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      Helpers.notImplemented

    override x.TextDocumentColorPresentation p =
      logger.info (
        Log.setMessage "TextDocumentColorPresentation Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      Helpers.notImplemented

    override x.TextDocumentDocumentLink p =
      logger.info (
        Log.setMessage "TextDocumentDocumentLink Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      Helpers.notImplemented

    override x.TextDocumentOnTypeFormatting p =
      logger.info (
        Log.setMessage "TextDocumentOnTypeFormatting Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      Helpers.notImplemented

    override x.TextDocumentPrepareRename p =
      logger.info (
        Log.setMessage "TextDocumentOnPrepareRename Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      Helpers.notImplemented

    override x.TextDocumentSemanticTokensFullDelta p =
      logger.info (
        Log.setMessage "TextDocumentSemanticTokensFullDelta Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      Helpers.notImplemented

    override x.TextDocumentWillSave p =
      logger.info (
        Log.setMessage "TextDocumentWillSave Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      Helpers.ignoreNotification

    override x.TextDocumentWillSaveWaitUntil p =
      logger.info (
        Log.setMessage "TextDocumentWillSaveWaitUntil Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      Helpers.notImplemented

    override x.WorkspaceDidChangeWorkspaceFolders p =
      logger.info (
        Log.setMessage "WorkspaceDidChangeWorkspaceFolders Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      Helpers.ignoreNotification

    override x.WorkspaceDidCreateFiles p =
      logger.info (
        Log.setMessage "WorkspaceDidCreateFiles Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      Helpers.ignoreNotification

    override x.WorkspaceDidDeleteFiles p =
      logger.info (
        Log.setMessage "WorkspaceDidDeleteFiles Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      Helpers.ignoreNotification

    override x.WorkspaceDidRenameFiles p =
      logger.info (
        Log.setMessage "WorkspaceDidRenameFiles Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      Helpers.ignoreNotification

    override x.WorkspaceExecuteCommand p =
      logger.info (
        Log.setMessage "WorkspaceExecuteCommand Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      Helpers.notImplemented

    override x.WorkspaceWillCreateFiles p =
      logger.info (
        Log.setMessage "WorkspaceWillCreateFiles Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      Helpers.notImplemented

    override x.WorkspaceWillDeleteFiles p =
      logger.info (
        Log.setMessage "WorkspaceWillDeleteFiles Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      Helpers.notImplemented

    override x.WorkspaceWillRenameFiles p =
      logger.info (
        Log.setMessage "WorkspaceWillRenameFiles Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      Helpers.notImplemented

    //unsupported -- end

    // -----------------
    // FSharp Operations
    // -----------------

    override x.FSharpLiterateRequest(p: FSharpLiterateRequest) =
      logger.info (
        Log.setMessage "FSharpLiterateRequest Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      Helpers.notImplemented

    override x.FSharpSignature(p: TextDocumentPositionParams) =
      asyncResult {
        try
          logger.info (
            Log.setMessage "FSharpSignature Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let (filePath, pos) = getFilePathAndPosition p
          let! (namedText, _) = tryGetFile filePath |> Result.ofStringErr

          let! lineStr = namedText.Lines |> tryGetLineStr pos |> Result.ofStringErr
          let! tyRes = tryGetTypeCheckResults filePath |> Result.ofStringErr
          let! tip = Commands.typesig tyRes pos lineStr |> Result.ofCoreResponse

          return { Content = CommandResponse.typeSig FsAutoComplete.JsonSerializer.writeJson tip }
        with e ->
          logger.error (
            Log.setMessage "FSharpSignature Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! LspResult.internalError (string e)
      }

    override x.FSharpSignatureData(p: TextDocumentPositionParams) =
      asyncResult {
        try
          logger.info (
            Log.setMessage "FSharpSignatureData Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let pos =
            FSharp.Compiler.Text.Position.mkPos (p.Position.Line) (p.Position.Character + 2)

          let filePath = p.TextDocument.GetFilePath() |> Utils.normalizePath
          let! (namedText, _) = tryGetFile filePath |> Result.ofStringErr
          let! lineStr = namedText.Lines |> tryGetLineStr pos |> Result.ofStringErr

          let! tyRes = tryGetTypeCheckResults filePath |> Result.ofStringErr
          let! (typ, parms, generics) = tyRes.TryGetSignatureData pos lineStr |> Result.ofStringErr

          return
            { Content = CommandResponse.signatureData FsAutoComplete.JsonSerializer.writeJson (typ, parms, generics) }

        with e ->
          logger.error (
            Log.setMessage "FSharpSignatureData Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! LspResult.internalError (string e)
      }


    override x.FSharpDocumentationGenerator(p: OptionallyVersionedTextDocumentPositionParams) =
      asyncResult {
        try
          logger.info (
            Log.setMessage "FSharpDocumentationGenerator Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let (filePath, pos) = getFilePathAndPosition p
          let! (namedText, _) = tryGetFile filePath |> Result.ofStringErr

          let! lineStr = namedText.Lines |> tryGetLineStr pos |> Result.ofStringErr
          let! tyRes = tryGetTypeCheckResults filePath |> Result.ofStringErr

          let! { InsertPosition = insertPos
                 InsertText = text } =
            Commands.GenerateXmlDocumentation(tyRes, pos, lineStr)
            |> AsyncResult.ofStringErr

          let edit: ApplyWorkspaceEditParams =
            { Label = Some "Generate Xml Documentation"
              Edit =
                { DocumentChanges =
                    Some
                      [| { TextDocument = p.TextDocument
                           Edits =
                             [| { Range = fcsPosToProtocolRange insertPos
                                  NewText = text } |] } |]
                  Changes = None } }

          let! response = lspClient.WorkspaceApplyEdit edit
          return ()

        with e ->
          logger.error (
            Log.setMessage "FSharpDocumentationGenerator Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! LspResult.internalError (string e)
      }

    override __.FSharpLineLense(p) =
      asyncResult {
        try
          logger.info (
            Log.setMessage "FSharpLineLense Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let fn = p.Project.GetFilePath() |> Utils.normalizePath

          match getDeclarations fn |> AVal.force with
          | None -> return! LspResult.internalError $"No declerations found for {fn}"
          | Some decls ->
            let decls = decls |> Array.map (fun d -> d, fn)

            return { Content = CommandResponse.declarations FsAutoComplete.JsonSerializer.writeJson decls }

        with e ->
          logger.error (
            Log.setMessage "FSharpLineLense Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! LspResult.internalError (string e)
      }

    override __.FSharpCompilerLocation(p) =
      asyncResult {
        try
          logger.info (
            Log.setMessage "FSharpCompilerLocation Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let checker = checker |> AVal.force
          let! (fsc, fsi, msbuild, sdk) = Commands.CompilerLocation checker |> Result.ofCoreResponse

          return
            { Content =
                CommandResponse.compilerLocation
                  FsAutoComplete.JsonSerializer.writeJson
                  fsc
                  fsi
                  msbuild
                  (sdk |> Option.map (fun (di: DirectoryInfo) -> di.FullName)) }
        with e ->
          logger.error (
            Log.setMessage "FSharpCompilerLocation Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! LspResult.internalError (string e)
      }

    override __.FSharpWorkspaceLoad(p: WorkspaceLoadParms) =
      asyncResult {
        try
          logger.info (
            Log.setMessage "FSharpWorkspaceLoad Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let projs =
            p.TextDocuments
            |> Array.map (fun t -> t.GetFilePath() |> Utils.normalizePath)
            |> HashSet.ofArray

          transact (fun () -> workspacePaths.Value <- (WorkspaceChosen.Projs projs))
          let options = loadedProjectOptions |> AVal.force

          return { Content = CommandResponse.workspaceLoad FsAutoComplete.JsonSerializer.writeJson true }

        with e ->
          logger.error (
            Log.setMessage "FSharpWorkspaceLoad Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! LspResult.internalError (string e)

      }


    override __.FSharpWorkspacePeek(p: WorkspacePeekRequest) =
      asyncResult {
        try
          logger.info (
            Log.setMessage "FSharpWorkspacePeek Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let res =
            WorkspacePeek.peek p.Directory p.Deep (p.ExcludedDirs |> List.ofArray)
            |> CoreResponse.Res

          let res =
            match res with
            | CoreResponse.InfoRes msg
            | CoreResponse.ErrorRes msg -> LspResult.internalError msg
            | CoreResponse.Res found ->
              { Content = CommandResponse.workspacePeek FsAutoComplete.JsonSerializer.writeJson found }
              |> success

          return! res
        with e ->
          logger.error (
            Log.setMessage "FSharpWorkspacePeek Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! LspResult.internalError (string e)
      }

    override __.FSharpProject(p) =
      asyncResult {
        try
          logger.info (
            Log.setMessage "FSharpProject Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let paths = workspacePaths |> AVal.force

          transact (fun () ->
            workspacePaths.Value <-
              match paths with
              | WorkspaceChosen.Sln x -> failwith "Can't load individual projects when Sln is chosen"
              | WorkspaceChosen.Directory x -> failwith "Can't load individual projects when Directory is chosen"
              | WorkspaceChosen.Projs ps ->
                p.Project.GetFilePath()
                |> Utils.normalizePath
                |> ps.Add
                |> WorkspaceChosen.Projs
              | WorkspaceChosen.NotChosen ->
                p.Project.GetFilePath()
                |> Utils.normalizePath
                |> HashSet.single
                |> WorkspaceChosen.Projs)

          loadedProjectOptions |> AVal.force |> ignore

          return! Helpers.notImplemented
        with e ->
          logger.error (
            Log.setMessage "FSharpWorkspacePeek Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! LspResult.internalError (string e)

      }

    override __.FSharpFsdn(p: FsdnRequest) =
      logger.info (
        Log.setMessage "FSharpFsdn Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )
      // Hasn't been online for a long time
      Helpers.notImplemented

    override __.FSharpDotnetNewList(p: DotnetNewListRequest) =
      asyncResult {
        try
          logger.info (
            Log.setMessage "FSharpDotnetNewList Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let! funcs = Commands.DotnetNewList() |> AsyncResult.ofCoreResponse
          return { Content = CommandResponse.dotnetnewlist FsAutoComplete.JsonSerializer.writeJson funcs }
        with e ->
          logger.error (
            Log.setMessage "FSharpDotnetNewList Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! LspResult.internalError (string e)
      }

    override __.FSharpDotnetNewRun(p: DotnetNewRunRequest) =
      asyncResult {
        try
          logger.info (
            Log.setMessage "FSharpDotnetNewRun Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          do!
            Commands.DotnetNewRun p.Template p.Name p.Output []
            |> AsyncResult.ofCoreResponse

          return { Content = "" }
        with e ->
          logger.error (
            Log.setMessage "FSharpDotnetNewRun Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! LspResult.internalError (string e)
      }

    override __.FSharpDotnetAddProject(p: DotnetProjectRequest) =
      asyncResult {
        try
          logger.info (
            Log.setMessage "FSharpDotnetAddProject Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          do! Commands.DotnetAddProject p.Target p.Reference |> AsyncResult.ofCoreResponse
          loadedProjectOptions |> AVal.force |> ignore
          return { Content = "" }
        with e ->
          logger.error (
            Log.setMessage "FSharpDotnetAddProject Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! LspResult.internalError (string e)
      }

    override __.FSharpDotnetRemoveProject(p: DotnetProjectRequest) =
      asyncResult {
        try
          logger.info (
            Log.setMessage "FSharpDotnetRemoveProject Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          do! Commands.DotnetRemoveProject p.Target p.Reference |> AsyncResult.ofCoreResponse
          loadedProjectOptions |> AVal.force |> ignore
          return { Content = "" }
        with e ->
          logger.error (
            Log.setMessage "FSharpDotnetRemoveProject Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! LspResult.internalError (string e)
      }

    override __.FSharpDotnetSlnAdd(p: DotnetProjectRequest) =
      asyncResult {
        try
          logger.info (
            Log.setMessage "FSharpDotnetSlnAdd Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          do! Commands.DotnetSlnAdd p.Target p.Reference |> AsyncResult.ofCoreResponse
          loadedProjectOptions |> AVal.force |> ignore
          return { Content = "" }
        with e ->
          logger.error (
            Log.setMessage "FSharpDotnetSlnAdd Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! LspResult.internalError (string e)
      }

    override x.FSharpHelp(p: TextDocumentPositionParams) =
      asyncResult {
        try
          logger.info (
            Log.setMessage "FSharpHelp Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let (filePath, pos) = getFilePathAndPosition p
          let! (namedText, _) = tryGetFile filePath |> Result.ofStringErr
          let! lineStr = namedText.Lines |> tryGetLineStr pos |> Result.ofStringErr
          let! tyRes = tryGetTypeCheckResults filePath |> Result.ofStringErr
          let! t = Commands.Help tyRes pos lineStr |> Result.ofCoreResponse
          return { Content = CommandResponse.help FsAutoComplete.JsonSerializer.writeJson t }
        with e ->
          logger.error (
            Log.setMessage "FSharpHelp Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! LspResult.internalError (string e)
      }

    override x.FSharpDocumentation(p: TextDocumentPositionParams) =
      asyncResult {
        try
          logger.info (
            Log.setMessage "FSharpDocumentation Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let (filePath, pos) = getFilePathAndPosition p
          let! (namedText, _) = tryGetFile filePath |> Result.ofStringErr
          let! lineStr = namedText.Lines |> tryGetLineStr pos |> Result.ofStringErr
          let! tyRes = tryGetTypeCheckResults filePath |> Result.ofStringErr
          lastFSharpDocumentationTypeCheck <- Some tyRes
          let! t = Commands.FormattedDocumentation tyRes pos lineStr |> Result.ofCoreResponse
          return { Content = CommandResponse.formattedDocumentation FsAutoComplete.JsonSerializer.writeJson t }
        with e ->
          logger.error (
            Log.setMessage "FSharpDocumentation Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! LspResult.internalError (string e)
      }

    override x.FSharpDocumentationSymbol(p: DocumentationForSymbolReuqest) =
      asyncResult {
        try
          logger.info (
            Log.setMessage "FSharpDocumentationSymbol Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let! tyRes =
            lastFSharpDocumentationTypeCheck
            |> Result.ofOption (fun () -> $"No typecheck results from FSharpDocumentation")
            |> Result.ofStringErr

          let! (xml, assembly, doc, signature, footer, cn) =
            Commands.FormattedDocumentationForSymbol tyRes p.XmlSig p.Assembly
            |> Result.ofCoreResponse

          let xmldoc =
            match doc with
            | FSharpXmlDoc.None -> [||]
            | FSharpXmlDoc.FromXmlFile _ -> [||]
            | FSharpXmlDoc.FromXmlText d -> d.GetElaboratedXmlLines()

          return
            { Content =
                CommandResponse.formattedDocumentationForSymbol
                  FsAutoComplete.JsonSerializer.writeJson
                  xml
                  assembly
                  xmldoc
                  (signature, footer, cn) }
        with e ->
          logger.error (
            Log.setMessage "FSharpDocumentationSymbol Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! LspResult.internalError (string e)
      }

    override __.LoadAnalyzers(path) =
      logger.info (
        Log.setMessage "LoadAnalyzers Request: {parms}"
        >> Log.addContextDestructured "parms" path
      )

      Helpers.notImplemented

    override x.FSharpPipelineHints(p: FSharpPipelineHintRequest) =
      asyncResult {
        try
          logger.info (
            Log.setMessage "FSharpPipelineHints Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let filePath = p.TextDocument.GetFilePath() |> Utils.normalizePath
          let! tyRes = tryGetTypeCheckResults filePath |> Result.ofStringErr
          let! res = Commands.pipelineHints tryGetFileSource tyRes |> Result.ofCoreResponse

          return { Content = CommandResponse.pipelineHint FsAutoComplete.JsonSerializer.writeJson res }
        with e ->
          logger.error (
            Log.setMessage "FSharpPipelineHints Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! LspResult.internalError (string e)
      }

    override __.FsProjMoveFileUp(p: DotnetFileRequest) =
      asyncResult {
        try
          logger.info (
            Log.setMessage "FsProjMoveFileUp Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          do!
            Commands.FsProjMoveFileUp p.FsProj p.FileVirtualPath
            |> AsyncResult.ofCoreResponse

          loadedProjectOptions |> AVal.force |> ignore
          return { Content = "" }
        with e ->
          logger.error (
            Log.setMessage "FsProjMoveFileUp Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! LspResult.internalError (string e)
      }


    override __.FsProjMoveFileDown(p: DotnetFileRequest) =
      asyncResult {
        try
          logger.info (
            Log.setMessage "FsProjMoveFileDown Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          do!
            Commands.FsProjMoveFileDown p.FsProj p.FileVirtualPath
            |> AsyncResult.ofCoreResponse

          loadedProjectOptions |> AVal.force |> ignore
          return { Content = "" }
        with e ->
          logger.error (
            Log.setMessage "FsProjMoveFileDown Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! LspResult.internalError (string e)
      }


    override __.FsProjAddFileAbove(p: DotnetFile2Request) =
      asyncResult {
        try
          logger.info (
            Log.setMessage "FsProjAddFileAbove Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          do!
            Commands.addFileAbove p.FsProj p.FileVirtualPath p.NewFile
            |> AsyncResult.ofCoreResponse

          loadedProjectOptions |> AVal.force |> ignore
          return { Content = "" }
        with e ->
          logger.error (
            Log.setMessage "FsProjAddFileAbove Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! LspResult.internalError (string e)
      }

    override __.FsProjAddFileBelow(p: DotnetFile2Request) =
      asyncResult {
        try
          logger.info (
            Log.setMessage "FsProjAddFileBelow Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          do!
            Commands.addFileBelow p.FsProj p.FileVirtualPath p.NewFile
            |> AsyncResult.ofCoreResponse

          loadedProjectOptions |> AVal.force |> ignore
          return { Content = "" }
        with e ->
          logger.error (
            Log.setMessage "FsProjAddFileBelow Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! LspResult.internalError (string e)
      }


    override __.FsProjAddFile(p: DotnetFileRequest) =
      asyncResult {
        try
          logger.info (
            Log.setMessage "FsProjAddFile Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          do! Commands.addFile p.FsProj p.FileVirtualPath |> AsyncResult.ofCoreResponse
          loadedProjectOptions |> AVal.force |> ignore
          return { Content = "" }
        with e ->
          logger.error (
            Log.setMessage "FsProjAddFile Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! LspResult.internalError (string e)
      }

    override _.FsProjRemoveFile(p: DotnetFileRequest) =
      asyncResult {
        try
          logger.info (
            Log.setMessage "FsProjRemoveFile Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          do! Commands.removeFile p.FsProj p.FileVirtualPath |> AsyncResult.ofCoreResponse
          loadedProjectOptions |> AVal.force |> ignore
          return { Content = "" }
        with e ->
          logger.error (
            Log.setMessage "FsProjRemoveFile Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! LspResult.internalError (string e)
      }

    override _.FsProjAddExistingFile(p: DotnetFileRequest) =
      asyncResult {
        try
          logger.info (
            Log.setMessage "FsProjAddExistingFile Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          do!
            Commands.addExistingFile p.FsProj p.FileVirtualPath
            |> AsyncResult.ofCoreResponse

          loadedProjectOptions |> AVal.force |> ignore
          return { Content = "" }
        with e ->
          logger.error (
            Log.setMessage "FsProjAddExistingFile Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! LspResult.internalError (string e)
      }

    override x.Dispose() = disposables.Dispose()

module AdaptiveFSharpLspServer =
  let startCore toolsPath workspaceLoaderFactory =
    use input = Console.OpenStandardInput()
    use output = Console.OpenStandardOutput()

    let requestsHandlings =
      (defaultRequestHandlings (): Map<string, ServerRequestHandling<IFSharpLspServer>>)
      |> Map.add "fsharp/signature" (serverRequestHandling (fun s p -> s.FSharpSignature(p)))
      |> Map.add "fsharp/signatureData" (serverRequestHandling (fun s p -> s.FSharpSignatureData(p)))
      |> Map.add "fsharp/documentationGenerator" (serverRequestHandling (fun s p -> s.FSharpDocumentationGenerator(p)))
      |> Map.add "fsharp/lineLens" (serverRequestHandling (fun s p -> s.FSharpLineLense(p)))
      |> Map.add "fsharp/compilerLocation" (serverRequestHandling (fun s p -> s.FSharpCompilerLocation(p)))
      |> Map.add "fsharp/workspaceLoad" (serverRequestHandling (fun s p -> s.FSharpWorkspaceLoad(p)))
      |> Map.add "fsharp/workspacePeek" (serverRequestHandling (fun s p -> s.FSharpWorkspacePeek(p)))
      |> Map.add "fsharp/project" (serverRequestHandling (fun s p -> s.FSharpProject(p)))
      |> Map.add "fsharp/fsdn" (serverRequestHandling (fun s p -> s.FSharpFsdn(p)))
      |> Map.add "fsharp/dotnetnewlist" (serverRequestHandling (fun s p -> s.FSharpDotnetNewList(p)))
      |> Map.add "fsharp/dotnetnewrun" (serverRequestHandling (fun s p -> s.FSharpDotnetNewRun(p)))
      |> Map.add "fsharp/dotnetaddproject" (serverRequestHandling (fun s p -> s.FSharpDotnetAddProject(p)))
      |> Map.add "fsharp/dotnetremoveproject" (serverRequestHandling (fun s p -> s.FSharpDotnetRemoveProject(p)))
      |> Map.add "fsharp/dotnetaddsln" (serverRequestHandling (fun s p -> s.FSharpDotnetSlnAdd(p)))
      |> Map.add "fsharp/f1Help" (serverRequestHandling (fun s p -> s.FSharpHelp(p)))
      |> Map.add "fsharp/documentation" (serverRequestHandling (fun s p -> s.FSharpDocumentation(p)))
      |> Map.add "fsharp/documentationSymbol" (serverRequestHandling (fun s p -> s.FSharpDocumentationSymbol(p)))
      |> Map.add "fsharp/loadAnalyzers" (serverRequestHandling (fun s p -> s.LoadAnalyzers(p)))
      // |> Map.add "fsharp/fsharpLiterate" (serverRequestHandling (fun s p -> s.FSharpLiterate(p) ))
      |> Map.add "fsharp/pipelineHint" (serverRequestHandling (fun s p -> s.FSharpPipelineHints(p)))
      |> Map.add "fsproj/moveFileUp" (serverRequestHandling (fun s p -> s.FsProjMoveFileUp(p)))
      |> Map.add "fsproj/moveFileDown" (serverRequestHandling (fun s p -> s.FsProjMoveFileDown(p)))
      |> Map.add "fsproj/addFileAbove" (serverRequestHandling (fun s p -> s.FsProjAddFileAbove(p)))
      |> Map.add "fsproj/addFileBelow" (serverRequestHandling (fun s p -> s.FsProjAddFileBelow(p)))
      |> Map.add "fsproj/addFile" (serverRequestHandling (fun s p -> s.FsProjAddFile(p)))
      |> Map.add "fsproj/addExistingFile" (serverRequestHandling (fun s p -> s.FsProjAddExistingFile(p)))
      |> Map.add "fsproj/removeFile" (serverRequestHandling (fun s p -> s.FsProjRemoveFile(p)))

    let adaptiveServer lspClient =
      let loader = workspaceLoaderFactory toolsPath
      new AdaptiveFSharpLspServer(loader, lspClient) :> IFSharpLspServer

    Ionide.LanguageServerProtocol.Server.start requestsHandlings input output FSharpLspClient adaptiveServer
