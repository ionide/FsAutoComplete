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
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types.LspResult
open Ionide.LanguageServerProtocol.Types
open Newtonsoft.Json.Linq
open Ionide.ProjInfo.ProjectSystem
open System.Reactive
open System.Reactive.Linq
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Buffers
open FsAutoComplete.Adaptive
open FsAutoComplete.LspHelpers

open FSharp.Control.Reactive
open FsToolkit.ErrorHandling
open FsAutoComplete.Telemetry
open FsAutoComplete.Utils.Tracing
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
open FsAutoComplete.LspHelpers
open FsAutoComplete.UnionPatternMatchCaseGenerator
open System.Collections.Concurrent
open System.Diagnostics
open System.Text.RegularExpressions
open IcedTasks
open System.Threading.Tasks
open FsAutoComplete.FCSPatches
open FSharp.Compiler.Syntax

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
  | Projs of amap<string<LocalPath>, DateTime>
  | NotChosen


[<CustomEquality; NoComparison>]
type LoadedProject =
  { FSharpProjectOptions: FSharpProjectOptions
    LanguageVersion: LanguageVersionShim }

  interface IEquatable<LoadedProject> with
    member x.Equals(other) = x.FSharpProjectOptions = other.FSharpProjectOptions

  override x.GetHashCode() = x.FSharpProjectOptions.GetHashCode()

  override x.Equals(other) =
    match other with
    | :? LoadedProject as other -> (x :> IEquatable<_>).Equals other
    | _ -> false

  member x.SourceFiles = x.FSharpProjectOptions.SourceFiles
  member x.ProjectFileName = x.FSharpProjectOptions.ProjectFileName


type AdaptiveFSharpLspServer
  (workspaceLoader: IWorkspaceLoader, lspClient: FSharpLspClient, sourceTextFactory: ISourceTextFactory) =

  let logger = LogProvider.getLoggerFor<AdaptiveFSharpLspServer> ()

  let thisType = typeof<AdaptiveFSharpLspServer>

  let disposables = new Disposables.CompositeDisposable()

  let rootPath = cval<string option> None

  let config = cval<FSharpConfig> FSharpConfig.Default

  let checker =
    config
    |> AVal.map (fun c -> c.EnableAnalyzers, c.Fsac.CachedTypeCheckCount, c.Fsac.ParallelReferenceResolution)
    |> AVal.map (FSharpCompilerServiceChecker)

  /// The reality is a file can be in multiple projects
  /// This is extracted to make it easier to do some type of customized select
  /// in the future
  let selectProject projs = projs |> List.tryHead

  let selectFSharpProject (projs: LoadedProject list) =
    projs |> List.tryHead |> Option.map (fun p -> p.FSharpProjectOptions)

  let mutable traceNotifications: ProgressListener option = None

  /// <summary>Toggles trace notifications on or off.</summary>
  /// <param name="shouldTrace">Determines if tracing should occur</param>
  /// <param name="traceNamespaces">The namespaces to start tracing</param>
  /// <returns></returns>
  let toggleTraceNotification shouldTrace traceNamespaces =
    traceNotifications |> Option.iter dispose

    if shouldTrace then
      traceNotifications <- Some(new ProgressListener(lspClient, traceNamespaces))
    else
      traceNotifications <- None

  /// <summary>Sets tje FSI arguments on the FSharpCompilerServiceChecker</summary>
  /// <param name="checker"></param>
  /// <param name="fsiCompilerToolLocations">Compiler tool locations</param>
  /// <param name="fsiExtraParameters">Any extra parameters to pass to FSI</param>
  let setFSIArgs
    (checker: FSharpCompilerServiceChecker)
    (fsiCompilerToolLocations: string array)
    (fsiExtraParameters: seq<string>)
    =
    let toCompilerToolArgument (path: string) = sprintf "--compilertool:%s" path

    checker.SetFSIAdditionalArguments
      [| yield! fsiCompilerToolLocations |> Array.map toCompilerToolArgument
         yield! fsiExtraParameters |]

  /// <summary>Loads F# Analyzers from the configured directories</summary>
  /// <param name="config">The FSharpConfig</param>
  /// <param name="rootPath">The RootPath</param>
  /// <returns></returns>
  let loadAnalyzers (config: FSharpConfig) (rootPath: string option) =
    if config.EnableAnalyzers then
      Loggers.analyzers.info (Log.setMessageI $"Using analyzer roots of {config.AnalyzersPath:roots}")

      config.AnalyzersPath
      |> Array.iter (fun analyzerPath ->
        match rootPath with
        | None -> ()
        | Some workspacePath ->
          let dir =
            if
              System.IO.Path.IsPathRooted analyzerPath
            // if analyzer is using absolute path, use it as is
            then
              analyzerPath
            // otherwise, it is a relative path and should be combined with the workspace path
            else
              System.IO.Path.Combine(workspacePath, analyzerPath)

          Loggers.analyzers.info (Log.setMessageI $"Loading analyzers from {dir:dir}")

          let (dllCount, analyzerCount) = dir |> FSharp.Analyzers.SDK.Client.loadAnalyzers

          Loggers.analyzers.info (
            Log.setMessageI
              $"From {analyzerPath:name}: {dllCount:dllNo} dlls including {analyzerCount:analyzersNo} analyzers"
          ))

    else
      Loggers.analyzers.info (Log.setMessage "Analyzers disabled")

  /// <summary></summary>
  /// <param name="checker">the FSharpCompilerServiceChecker</param>
  /// <param name="dotnetRoot">The path to dotnet</param>
  /// <param name="rootPath">The root path</param>
  /// <returns></returns>
  let setDotnetRoot (checker: FSharpCompilerServiceChecker) (dotnetRoot: string) (rootPath: string option) =
    let di = DirectoryInfo dotnetRoot

    if di.Exists then
      let dotnetBinary =
        if
          System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(Runtime.InteropServices.OSPlatform.Windows)
        then
          FileInfo(Path.Combine(di.FullName, "dotnet.exe"))
        else
          FileInfo(Path.Combine(di.FullName, "dotnet"))

      if dotnetBinary.Exists then
        checker.SetDotnetRoot(dotnetBinary, defaultArg rootPath System.Environment.CurrentDirectory |> DirectoryInfo)

    else
      // if we were mistakenly given the path to a dotnet binary
      // then use the parent directory as the dotnet root instead
      let fi = FileInfo(di.FullName)

      if fi.Exists && (fi.Name = "dotnet" || fi.Name = "dotnet.exe") then
        checker.SetDotnetRoot(fi, defaultArg rootPath System.Environment.CurrentDirectory |> DirectoryInfo)

  let configChanges =
    aval {
      let! config = config
      and! checker = checker
      and! rootPath = rootPath

      return config, checker, rootPath
    }

  // Syncs config changes to the mutable world
  do
    AVal.Observable.onValueChangedWeak configChanges
    |> Observable.subscribe (fun (config, checker, rootPath) ->
      toggleTraceNotification config.Notifications.Trace config.Notifications.TraceNamespaces

      setFSIArgs checker config.FSICompilerToolLocations config.FSIExtraParameters

      loadAnalyzers config rootPath

      setDotnetRoot checker config.DotNetRoot rootPath)
    |> disposables.Add

  let updateConfig c = transact (fun () -> config.Value <- c)

  let tfmConfig =
    config
    |> AVal.map (fun c ->
      if c.UseSdkScripts then
        FSIRefs.TFM.NetCore
      else
        FSIRefs.TFM.NetFx)


  let sendDiagnostics (uri: DocumentUri) (diags: Diagnostic[]) =
    logger.info (Log.setMessageI $"SendDiag for {uri:file}: {diags.Length:diags} entries")

    // TODO: providing version would be very useful
    { Uri = uri
      Diagnostics = diags
      Version = None }
    |> lspClient.TextDocumentPublishDiagnostics

  let mutable lastFSharpDocumentationTypeCheck: ParseAndCheckResults option = None

  let diagnosticCollections = new DiagnosticCollection(sendDiagnostics)

  let notifications = Event<NotificationEvent * CancellationToken>()

  let scriptFileProjectOptions = Event<FSharpProjectOptions>()

  let fileParsed =
    Event<FSharpParseFileResults * FSharpProjectOptions * CancellationToken>()

  let fileChecked = Event<ParseAndCheckResults * VolatileFile * CancellationToken>()

  let detectTests (parseResults: FSharpParseFileResults) (proj: FSharpProjectOptions) ct =
    try
      logger.info (Log.setMessageI $"Test Detection of {parseResults.FileName:file} started")

      let fn = UMX.tag parseResults.FileName

      let res =
        if proj.OtherOptions |> Seq.exists (fun o -> o.Contains "Expecto.dll") then
          TestAdapter.getExpectoTests parseResults.ParseTree
        elif proj.OtherOptions |> Seq.exists (fun o -> o.Contains "nunit.framework.dll") then
          TestAdapter.getNUnitTest parseResults.ParseTree
        elif proj.OtherOptions |> Seq.exists (fun o -> o.Contains "xunit.assert.dll") then
          TestAdapter.getXUnitTest parseResults.ParseTree
        else
          []

      logger.info (Log.setMessageI $"Test Detection of {parseResults.FileName:file} - {res:res}")

      notifications.Trigger(NotificationEvent.TestDetected(fn, res |> List.toArray), ct)
    with e ->
      logger.info (
        Log.setMessageI $"Test Detection of {parseResults.FileName:file} failed"
        >> Log.addExn e
      )

  do
    disposables.Add
    <| fileParsed.Publish.Subscribe(fun (parseResults, proj, ct) -> detectTests parseResults proj ct)

  let builtInCompilerAnalyzers config (file: VolatileFile) (tyRes: ParseAndCheckResults) =
    let filePath = file.FileName
    let filePathUntag = UMX.untag filePath
    let source = file.Source
    let version = file.Version
    let fileName = Path.GetFileName filePathUntag


    let inline getSourceLine lineNo = (source: ISourceText).GetLineString(lineNo - 1)

    let checkUnusedOpens =
      async {
        try
          use progress = new ServerProgressReport(lspClient)
          do! progress.Begin($"Checking unused opens {fileName}...", message = filePathUntag)

          let! unused = UnusedOpens.getUnusedOpens (tyRes.GetCheckResults, getSourceLine)

          let! ct = Async.CancellationToken
          notifications.Trigger(NotificationEvent.UnusedOpens(filePath, (unused |> List.toArray)), ct)
        with e ->
          logger.error (Log.setMessage "checkUnusedOpens failed" >> Log.addExn e)
      }

    let checkUnusedDeclarations =
      async {
        try
          use progress = new ServerProgressReport(lspClient)
          do! progress.Begin($"Checking unused declarations {fileName}...", message = filePathUntag)

          let isScript = Utils.isAScript (filePathUntag)
          let! unused = UnusedDeclarations.getUnusedDeclarations (tyRes.GetCheckResults, isScript)
          let unused = unused |> Seq.toArray

          let! ct = Async.CancellationToken
          notifications.Trigger(NotificationEvent.UnusedDeclarations(filePath, unused), ct)
        with e ->
          logger.error (Log.setMessage "checkUnusedDeclarations failed" >> Log.addExn e)
      }

    let checkSimplifiedNames =
      async {
        try
          use progress = new ServerProgressReport(lspClient)
          do! progress.Begin($"Checking simplifing of names {fileName}...", message = filePathUntag)

          let! simplified = SimplifyNames.getSimplifiableNames (tyRes.GetCheckResults, getSourceLine)
          let simplified = Array.ofSeq simplified
          let! ct = Async.CancellationToken
          notifications.Trigger(NotificationEvent.SimplifyNames(filePath, simplified), ct)
        with e ->
          logger.error (Log.setMessage "checkSimplifiedNames failed" >> Log.addExn e)
      }

    let inline isNotExcluded (exclusions: Regex array) =
      exclusions |> Array.exists (fun r -> r.IsMatch filePathUntag) |> not

    let analyzers =
      [
        // if config.Linter then
        //   commands.Lint filePath |> Async .Ignore
        if config.UnusedOpensAnalyzer && isNotExcluded config.UnusedOpensAnalyzerExclusions then
          checkUnusedOpens
        if
          config.UnusedDeclarationsAnalyzer
          && isNotExcluded config.UnusedDeclarationsAnalyzerExclusions
        then
          checkUnusedDeclarations
        if
          config.SimplifyNameAnalyzer
          && isNotExcluded config.SimplifyNameAnalyzerExclusions
        then
          checkSimplifiedNames ]

    async {
      do! analyzers |> Async.parallel75 |> Async.Ignore<unit[]>

      do!
        lspClient.NotifyDocumentAnalyzed
          { TextDocument =
              { Uri = filePath |> Path.LocalPathToUri
                Version = version } }
    }


  let runAnalyzers (config: FSharpConfig) (parseAndCheck: ParseAndCheckResults) (volatileFile: VolatileFile) =
    async {
      if config.EnableAnalyzers then
        let file = volatileFile.FileName

        try
          use progress = new ServerProgressReport(lspClient)
          do! progress.Begin("Running analyzers...", message = UMX.untag file)

          Loggers.analyzers.info (
            Log.setMessage "begin analysis of {file}"
            >> Log.addContextDestructured "file" file
          )

          match parseAndCheck.GetCheckResults.ImplementationFile with
          | Some tast ->
            // Since analyzers are not async, we need to switch to a new thread to not block threadpool
            do! Async.SwitchToNewThread()

            let res =
              Commands.analyzerHandler (
                file,
                volatileFile.Source.ToString().Split("\n"),
                parseAndCheck.GetParseResults.ParseTree,
                tast,
                parseAndCheck.GetCheckResults.PartialAssemblySignature.Entities |> Seq.toList,
                parseAndCheck.GetAllEntities
              )

            let! ct = Async.CancellationToken
            notifications.Trigger(NotificationEvent.AnalyzerMessage(res, file), ct)

            Loggers.analyzers.info (Log.setMessageI $"end analysis of {file:file}")

          | _ ->
            Loggers.analyzers.info (Log.setMessageI $"missing components of {file:file} to run analyzers, skipped them")

            ()
        with ex ->
          Loggers.analyzers.error (Log.setMessageI $"Run failed for {file:file}" >> Log.addExn ex)
    }

  do
    disposables.Add
    <| fileChecked.Publish.Subscribe(fun (parseAndCheck, volatileFile, ct) ->
      if volatileFile.Source.Length = 0 then
        () // Don't analyze and error on an empty file
      else
        async {
          let config = config |> AVal.force
          do! builtInCompilerAnalyzers config volatileFile parseAndCheck
          do! runAnalyzers config parseAndCheck volatileFile

        }
        |> Async.StartWithCT ct)


  let handleCommandEvents (n: NotificationEvent, ct: CancellationToken) =
    try
      async {

        try
          match n with
          | NotificationEvent.FileParsed fn ->
            let uri = Path.LocalPathToUri fn

            do! ({ Content = UMX.untag uri }: PlainNotification) |> lspClient.NotifyFileParsed
          | NotificationEvent.Workspace ws ->

            let ws =
              match ws with
              | ProjectResponse.Project(x, _) -> CommandResponse.project JsonSerializer.writeJson x
              | ProjectResponse.ProjectError(_, errorDetails) ->
                CommandResponse.projectError JsonSerializer.writeJson errorDetails
              | ProjectResponse.ProjectLoading(projectFileName) ->
                CommandResponse.projectLoading JsonSerializer.writeJson projectFileName
              | ProjectResponse.WorkspaceLoad(finished) ->
                CommandResponse.workspaceLoad JsonSerializer.writeJson finished
              | ProjectResponse.ProjectChanged(projectFileName) -> failwith "Not Implemented"

            logger.info (Log.setMessage "Workspace Notify {ws}" >> Log.addContextDestructured "ws" ws)
            do! ({ Content = ws }: PlainNotification) |> lspClient.NotifyWorkspace

          | NotificationEvent.ParseError(errors, file) ->
            let uri = Path.LocalPathToUri file
            let diags = errors |> Array.map fcsErrorToDiagnostic
            diagnosticCollections.SetFor(uri, "F# Compiler", diags)

          | NotificationEvent.UnusedOpens(file, opens) ->
            let uri = Path.LocalPathToUri file

            let diags =
              opens
              |> Array.map (fun n ->
                { Range = fcsRangeToLsp n
                  Code = Some "FSAC0001"
                  Severity = Some DiagnosticSeverity.Hint
                  Source = Some "FSAC"
                  Message = "Unused open statement"
                  RelatedInformation = None
                  Tags = Some [| DiagnosticTag.Unnecessary |]
                  Data = None
                  CodeDescription = None })

            diagnosticCollections.SetFor(uri, "F# Unused opens", diags)

          | NotificationEvent.UnusedDeclarations(file, decls) ->
            let uri = Path.LocalPathToUri file

            let diags =
              decls
              |> Array.map (fun n ->
                { Range = fcsRangeToLsp n
                  Code = Some "FSAC0003"
                  Severity = Some DiagnosticSeverity.Hint
                  Source = Some "FSAC"
                  Message = "This value is unused"
                  RelatedInformation = Some [||]
                  Tags = Some [| DiagnosticTag.Unnecessary |]
                  Data = None
                  CodeDescription = None })

            diagnosticCollections.SetFor(uri, "F# Unused declarations", diags)

          | NotificationEvent.SimplifyNames(file, decls) ->
            let uri = Path.LocalPathToUri file

            let diags =
              decls
              |> Array.map

                (fun
                     ({ Range = range
                        RelativeName = _relName }) ->
                  { Diagnostic.Range = fcsRangeToLsp range
                    Code = Some "FSAC0002"
                    Severity = Some DiagnosticSeverity.Hint
                    Source = Some "FSAC"
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

          | NotificationEvent.Canceled(msg) ->
            let ntf: PlainNotification = { Content = msg }

            do! lspClient.NotifyCancelledRequest ntf
          | NotificationEvent.AnalyzerMessage(messages, file) ->
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
                    Source = Some $"F# Analyzers (%s{m.Type})"
                    Message = m.Message
                    RelatedInformation = None
                    Tags = None
                    CodeDescription = None
                    Data = fixes })

              diagnosticCollections.SetFor(uri, "F# Analyzers", diags)
          | NotificationEvent.TestDetected(file, tests) ->
            let rec map
              (r: TestAdapter.TestAdapterEntry<FSharp.Compiler.Text.range>)
              : TestAdapter.TestAdapterEntry<Ionide.LanguageServerProtocol.Types.Range> =
              { Id = r.Id
                List = r.List
                Name = r.Name
                Type = r.Type
                ModuleType = r.ModuleType
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
      |> fun work -> Async.StartImmediate(work, ct)
    with :? OperationCanceledException as e ->
      ()


  do
    disposables.Add(
      (notifications.Publish :> IObservable<_>)
        // .BufferedDebounce(TimeSpan.FromMilliseconds(200.))
        // .SelectMany(fun l -> l.Distinct())
        .Subscribe(fun e -> handleCommandEvents e)
    )

  let getLastUTCChangeForFile (filePath: string<LocalPath>) =
    AdaptiveFile.GetLastWriteTimeUtc(UMX.untag filePath)
    |> AVal.map (fun writeTime -> filePath, writeTime)

  let addAValLogging cb (aval: aval<_>) =
    let cb = aval.AddWeakMarkingCallback(cb)
    aval |> AVal.mapDisposableTuple (fun x -> x, cb)

  let projectFileChanges project (filePath: string<LocalPath>) =
    let file = getLastUTCChangeForFile filePath

    let logMsg () =
      logger.info (Log.setMessageI $"Loading {project:project} because of {filePath:filePath}")

    file |> addAValLogging logMsg

  let loader = cval<Ionide.ProjInfo.IWorkspaceLoader> workspaceLoader

  let binlogConfig =
    aval {
      let! generateBinLog = config |> AVal.map (fun c -> c.GenerateBinlog)
      and! rootPath = rootPath

      match generateBinLog, rootPath with
      | _, None
      | false, _ -> return Ionide.ProjInfo.BinaryLogGeneration.Off
      | true, Some rootPath ->
        return Ionide.ProjInfo.BinaryLogGeneration.Within(DirectoryInfo(Path.Combine(rootPath, ".ionide")))
    }

  // JB:TODO Adding to solution
  // JB:TODO Adding new project file not yet added to solution
  let workspacePaths: ChangeableValue<WorkspaceChosen> =
    cval WorkspaceChosen.NotChosen

  let noopDisposable =
    { new IDisposable with
        member this.Dispose() : unit = () }

  let adaptiveWorkspacePaths =
    workspacePaths
    |> AVal.map (fun wsp ->
      match wsp with
      | WorkspaceChosen.Sln v -> projectFileChanges v v |> AdaptiveWorkspaceChosen.Sln, noopDisposable
      | WorkspaceChosen.Directory d ->
        failwith "Need to use AdaptiveDirectory" |> AdaptiveWorkspaceChosen.Directory, noopDisposable
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

        projChanges |> AdaptiveWorkspaceChosen.Projs, cb

      | WorkspaceChosen.NotChosen -> AdaptiveWorkspaceChosen.NotChosen, noopDisposable

    )
    |> AVal.mapDisposableTuple (id)

  let clientCapabilities = cval<ClientCapabilities option> None

  let glyphToCompletionKind =
    clientCapabilities |> AVal.map (glyphToCompletionKindGenerator)

  let glyphToSymbolKind = clientCapabilities |> AVal.map glyphToSymbolKindGenerator

  let tryFindProp name (props: list<Types.Property>) =
    match props |> Seq.tryFind (fun x -> x.Name = name) with
    | Some v -> v.Value |> Option.ofObj
    | None -> None

  let (|ProjectAssetsFile|_|) (props: list<Types.Property>) = tryFindProp "ProjectAssetsFile" props

  let (|BaseIntermediateOutputPath|_|) (props: list<Types.Property>) = tryFindProp "BaseIntermediateOutputPath" props

  let (|MSBuildAllProjects|_|) (props: list<Types.Property>) =
    tryFindProp "MSBuildAllProjects" props
    |> Option.map (fun v -> v.Split(';', StringSplitOptions.RemoveEmptyEntries))

  let loadedProjectOptions =
    aval {
      let! loader = loader
      and! wsp = adaptiveWorkspacePaths

      match wsp with
      | AdaptiveWorkspaceChosen.NotChosen -> return []
      | AdaptiveWorkspaceChosen.Sln _ -> return raise (NotImplementedException())
      | AdaptiveWorkspaceChosen.Directory _ -> return raise (NotImplementedException())
      | AdaptiveWorkspaceChosen.Projs projects ->
        let! binlogConfig = binlogConfig

        let! projectOptions =
          projects
          |> AMap.mapWithAdditionalDependencies (fun projects ->

            projects
            |> Seq.iter (fun (proj: string<LocalPath>, _) ->
              let not =
                UMX.untag proj |> ProjectResponse.ProjectLoading |> NotificationEvent.Workspace

              notifications.Trigger(not, CancellationToken.None))


            use progressReport = new ServerProgressReport(lspClient)

            progressReport.Begin ($"Loading {projects.Count} Projects") (CancellationToken.None)
            |> ignore<Task<unit>>

            let projectOptions =
              loader.LoadProjects(projects |> Seq.map (fst >> UMX.untag) |> Seq.toList, [], binlogConfig)
              |> Seq.toList

            for p in projectOptions do
              logger.info (
                Log.setMessage "Found BaseIntermediateOutputPath of {path}"
                >> Log.addContextDestructured "path" p.Properties
              )

            let additionalDependencies (p: Types.ProjectOptions) =
              [ let projectFileChanges = projectFileChanges p.ProjectFileName

                match p.Properties with
                | ProjectAssetsFile v -> yield projectFileChanges (UMX.tag v)
                | _ -> ()

                let objPath = (|BaseIntermediateOutputPath|_|) p.Properties

                let isWithinObjFolder (file: string) =
                  match objPath with
                  | None -> true // if no obj folder provided assume we should track this file
                  | Some objPath -> file.Contains(objPath)

                match p.Properties with
                | MSBuildAllProjects v ->
                  yield!
                    v
                    |> Array.filter (fun x -> x.EndsWith(".props") && isWithinObjFolder x)
                    |> Array.map (UMX.tag >> projectFileChanges)
                | _ -> () ]

            HashMap.ofList
              [ for p in projectOptions do
                  UMX.tag p.ProjectFileName, (p, additionalDependencies p) ]

          )
          |> AMap.toAVal
          |> AVal.map HashMap.toValueList


        and! checker = checker
        checker.ClearCaches() // if we got new projects assume we're gonna need to clear caches

        let options =
          let fsharpOptions = projectOptions |> FCS.mapManyOptions |> Seq.toList

          List.zip projectOptions fsharpOptions
          |> List.map (fun (projectOption, fso) ->

            let langversion = LanguageVersionShim.fromFSharpProjectOptions fso

            // Set some default values as FCS uses these for identification/caching purposes
            let fso =
              { fso with
                  SourceFiles = fso.SourceFiles |> Array.map (Utils.normalizePath >> UMX.untag)
                  Stamp = fso.Stamp |> Option.orElse (Some DateTime.UtcNow.Ticks)
                  ProjectId = fso.ProjectId |> Option.orElse (Some(Guid.NewGuid().ToString())) }

            { FSharpProjectOptions = fso
              LanguageVersion = langversion },
            projectOption)

        options
        |> List.iter (fun (loadedProject, projectOption) ->
          let projectFileName = loadedProject.ProjectFileName
          let projViewerItemsNormalized = ProjectViewer.render projectOption

          let responseFiles =
            projViewerItemsNormalized.Items
            |> List.map (function
              | ProjectViewerItem.Compile(p, c) -> ProjectViewerItem.Compile(Helpers.fullPathNormalized p, c))
            |> List.choose (function
              | ProjectViewerItem.Compile(p, _) -> Some p)

          let references =
            FscArguments.references (loadedProject.FSharpProjectOptions.OtherOptions |> List.ofArray)

          logger.info (
            Log.setMessage "ProjectLoaded {file}"
            >> Log.addContextDestructured "file" projectFileName
          )

          let ws =
            { ProjectFileName = projectFileName
              ProjectFiles = responseFiles
              OutFileOpt = Option.ofObj projectOption.TargetPath
              References = references
              Extra = projectOption
              ProjectItems = projViewerItemsNormalized.Items
              Additionals = Map.empty }

          let not = ProjectResponse.Project(ws, false) |> NotificationEvent.Workspace
          notifications.Trigger(not, CancellationToken.None))

        let not = ProjectResponse.WorkspaceLoad true |> NotificationEvent.Workspace

        notifications.Trigger(not, CancellationToken.None)

        return options |> List.map fst
    }

  /// <summary>
  /// Evaluates the adaptive value <see cref='F:loadedProjectOptions '/> and returns its current value.
  /// This should not be used inside the adaptive evaluation of other AdaptiveObjects since it does not track dependencies.
  /// </summary>
  /// <returns>A list of FSharpProjectOptions</returns>
  let forceLoadProjects () = loadedProjectOptions |> AVal.force

  do
    // Reload Projects with some debouncing if `loadedProjectOptions` is out of date.
    AVal.Observable.onOutOfDateWeak loadedProjectOptions
    |> Observable.throttleOn Concurrency.NewThreadScheduler.Default (TimeSpan.FromMilliseconds(200.))
    |> Observable.observeOn Concurrency.NewThreadScheduler.Default
    |> Observable.subscribe (fun _ -> forceLoadProjects () |> ignore<list<LoadedProject>>)
    |> disposables.Add


  let sourceFileToProjectOptions =
    aval {
      let! options = loadedProjectOptions

      return
        options
        |> List.collect (fun proj ->
          proj.SourceFiles
          |> Array.map (fun source -> Utils.normalizePath source, proj)
          |> Array.toList)
        |> List.groupByFst

    }
    |> AMap.ofAVal

  let fantomasLogger = LogProvider.getLoggerByName "Fantomas"
  let fantomasService: FantomasService = new LSPFantomasService() :> FantomasService

  let openFilesTokens =
    ConcurrentDictionary<string<LocalPath>, CancellationTokenSource>()

  let tryGetOpenFileToken filePath =
    match openFilesTokens.TryGetValue(filePath) with
    | (true, v) -> Some v
    | _ -> None

  let openFiles = cmap<string<LocalPath>, cval<VolatileFile>> ()
  let openFilesReadOnly = openFiles |> AMap.map (fun _ x -> x :> aval<_>)

  let textChanges =
    cmap<string<LocalPath>, cset<DidChangeTextDocumentParams * DateTime>> ()

  let textChangesReadOnly = textChanges |> AMap.map (fun _ x -> x :> aset<_>)

  let logTextChange (v: VolatileFile) =
    logger.debug (
      Log.setMessage "TextChanged for file : {fileName} {touched} {version}"
      >> Log.addContextDestructured "fileName" v.FileName
      >> Log.addContextDestructured "touched" v.LastTouched
      >> Log.addContextDestructured "version" v.Version
    )

  let openFilesWithChanges: amap<_, aval<VolatileFile>> =
    openFilesReadOnly
    |> AMap.map (fun filePath file ->
      aval {
        let! file = file
        and! changes = textChangesReadOnly |> AMap.tryFind filePath

        match changes with
        | None -> return file
        | Some c ->
          let! ps = c |> ASet.toAVal

          let changes =
            ps
            |> Seq.sortBy (fun (x, _) -> x.TextDocument.Version)
            |> Seq.collect (fun (p, touched) ->
              p.ContentChanges |> Array.map (fun x -> x, p.TextDocument.Version, touched))

          let file =
            (file, changes)
            ||> Seq.fold (fun text (change, version, touched) ->
              match change.Range with
              | None -> // replace entire content
                VolatileFile.Create(sourceTextFactory.Create(filePath, change.Text), version, touched)
              | Some rangeToReplace ->
                // replace just this slice
                let fcsRangeToReplace = protocolRangeToRange (UMX.untag filePath) rangeToReplace

                try
                  match text.Source.ModifyText(fcsRangeToReplace, change.Text) with
                  | Ok text -> VolatileFile.Create(text, version, touched)

                  | Error message ->
                    logger.error (
                      Log.setMessage
                        "Error applying {change} to document {file} for version {version} - {range} : {message} "
                      >> Log.addContextDestructured "file" filePath
                      >> Log.addContextDestructured "version" version
                      >> Log.addContextDestructured "message" message
                      >> Log.addContextDestructured "range" fcsRangeToReplace
                      >> Log.addContextDestructured "change" change
                    )

                    text
                with e ->
                  logger.error (
                    Log.setMessage "Error applying {change} to document {file} for version {version} - {range}"
                    >> Log.addContextDestructured "file" filePath
                    >> Log.addContextDestructured "range" fcsRangeToReplace
                    >> Log.addContextDestructured "version" version
                    >> Log.addContextDestructured "change" change
                    >> Log.addExn e
                  )

                  text)

          logTextChange file
          return file
      })


  let cancelToken filePath (cts: CancellationTokenSource) =

    try
      logger.info (
        Log.setMessage "Cancelling {filePath} - {version}"
        >> Log.addContextDestructured "filePath" filePath
      // >> Log.addContextDestructured "version" oldFile.Version
      )

      cts.Cancel()
      cts.Dispose()
    with
    | :? OperationCanceledException
    | :? ObjectDisposedException as e when e.Message.Contains("CancellationTokenSource has been disposed") ->
      // ignore if already cancelled
      ()

  [<return: Struct>]
  let rec (|Cancelled|_|) (e: exn) =
    match e with
    | :? TaskCanceledException -> ValueSome()
    | :? OperationCanceledException -> ValueSome()
    | :? System.AggregateException as aex ->
      if aex.InnerExceptions.Count = 1 then
        (|Cancelled|_|) aex.InnerException
      else
        ValueNone
    | _ -> ValueNone

  let returnException e =
    match e with
    | Cancelled -> LspResult.requestCancelled
    | e -> LspResult.internalError (string e)

  let cachedFileContents = cmap<string<LocalPath>, asyncaval<VolatileFile>> ()

  let resetCancellationToken filePath =
    let adder _ = new CancellationTokenSource()

    let updater key value =
      cancelToken filePath value
      new CancellationTokenSource()

    openFilesTokens.AddOrUpdate(filePath, adder, updater)
    |> ignore<CancellationTokenSource>


  let updateOpenFiles (file: VolatileFile) =
    let adder _ = cval file

    let updater _ (v: cval<_>) = v.Value <- file

    resetCancellationToken file.FileName
    transact (fun () -> openFiles.AddOrElse(file.Source.FileName, adder, updater))

  let updateTextchanges filePath p =
    let adder _ = cset<_> [ p ]
    let updater _ (v: cset<_>) = v.Add p |> ignore<bool>

    resetCancellationToken filePath
    transact (fun () -> textChanges.AddOrElse(filePath, adder, updater))

  let isFileOpen file = openFiles |> AMap.tryFindA file |> AVal.map (Option.isSome)

  let findFileInOpenFiles file = openFilesWithChanges |> AMap.tryFindA file

  let forceFindOpenFile filePath = findFileInOpenFiles filePath |> AVal.force

  let forceFindOpenFileOrRead file =
    asyncOption {

      match findFileInOpenFiles file |> AVal.force with
      | Some s -> return s
      | None ->
        // TODO: Log how many times this kind area gets hit and possibly if this should be rethought
        try
          logger.debug (
            Log.setMessage "forceFindOpenFileOrRead else - {file}"
            >> Log.addContextDestructured "file" file
          )

          if File.Exists(UMX.untag file) then
            use s = File.openFileStreamForReadingAsync file

            let! source = sourceTextFactory.Create(file, s) |> Async.AwaitCancellableValueTask

            return
              { LastTouched = File.getLastWriteTimeOrDefaultNow file
                Source = source
                Version = 0 }

          else // When a user does "File -> New Text File -> Select a language -> F#" without saving, the file won't exist
            return
              { LastTouched = DateTime.UtcNow
                Source = sourceTextFactory.Create(file, "")
                Version = 0 }
        with e ->
          logger.warn (
            Log.setMessage "Could not read file {file}"
            >> Log.addContextDestructured "file" file
            >> Log.addExn e
          )

          return! None
    }
    |> Async.map (Result.ofOption (fun () -> $"Could not read file: {file}"))

  do
    let fileshimChanges = openFilesWithChanges |> AMap.mapA (fun _ v -> v)
    // let cachedFileContents = cachedFileContents |> cmap.mapA (fun _ v -> v)

    let filesystemShim file =
      // GetLastWriteTimeShim gets called _alot_ and when we do checks on save we use Async.Parallel for type checking.
      // Adaptive uses lots of locks under the covers, so many threads can get blocked waiting for data.
      // flattening openFilesWithChanges makes this check a lot quicker as it's not needing to recalculate each value.

      fileshimChanges |> AMap.force |> HashMap.tryFind file

    FSharp.Compiler.IO.FileSystemAutoOpens.FileSystem <-
      FileSystem(FSharp.Compiler.IO.FileSystemAutoOpens.FileSystem, filesystemShim)

  /// <summary>Parses a source code for a file and caches the results. Returns an AST that can be traversed for various features.</summary>
  /// <param name="checker">The FSharpCompilerServiceChecker.</param>
  /// <param name="source">The source to be parsed.</param>
  /// <param name="parseOpts">Parsing options for the project or script</param>
  /// <param name="options">The options for the project or script.</param>
  /// <returns></returns>
  let parseFile (checker: FSharpCompilerServiceChecker) (source: VolatileFile) parseOpts options =
    async {
      let! result = checker.ParseFile(source.FileName, source.Source, parseOpts)

      let! ct = Async.CancellationToken
      fileParsed.Trigger(result, options, ct)
      return result
    }



  /// <summary>Parses all files in the workspace. This is mostly used to trigger finding tests.</summary>
  let parseAllFiles () =
    asyncAVal {
      let! projects = loadedProjectOptions
      and! (checker: FSharpCompilerServiceChecker) = checker

      return
        projects
        |> Array.ofList
        |> Array.Parallel.collect (fun p ->
          let parseOpts = Utils.projectOptionsToParseOptions p.FSharpProjectOptions
          p.SourceFiles |> Array.Parallel.map (fun s -> p, parseOpts, s))
        |> Array.Parallel.map (fun (opts, parseOpts, fileName) ->
          let fileName = UMX.tag fileName

          asyncResult {
            let! file = forceFindOpenFileOrRead fileName
            return! parseFile checker file parseOpts opts.FSharpProjectOptions
          }
          |> Async.map Result.toOption)
        |> Async.parallel75
    }

  let forceFindSourceText filePath = forceFindOpenFileOrRead filePath |> AsyncResult.map (fun f -> f.Source)


  let openFilesToChangesAndProjectOptions =
    openFilesWithChanges
    |> AMapAsync.mapAVal (fun filePath file ctok ->
      asyncAVal {
        if Utils.isAScript (UMX.untag filePath) then
          let! (checker: FSharpCompilerServiceChecker) = checker
          and! tfmConfig = tfmConfig

          let! projs =
            taskOption {
              let! cts = tryGetOpenFileToken filePath

              let! opts =
                checker.GetProjectOptionsFromScript(filePath, file.Source, tfmConfig)
                |> Async.withCancellation cts.Token
                |> Async.startImmediateAsTask ctok

              opts |> scriptFileProjectOptions.Trigger

              return
                { FSharpProjectOptions = opts
                  LanguageVersion = LanguageVersionShim.fromFSharpProjectOptions opts }
            }

          return file, Option.toList projs
        else
          let! projs =
            sourceFileToProjectOptions
            |> AMap.tryFind filePath
            |> AVal.map (Option.defaultValue [])

          return file, projs
      })

  let allFSharpFilesAndProjectOptions =
    let wins =
      openFilesToChangesAndProjectOptions
      |> AMap.map (fun k v -> v |> AsyncAVal.mapSync (fun (file, projects) _ -> Some file, projects))

    let loses =
      sourceFileToProjectOptions
      |> AMap.map (fun filePath v ->
        asyncAVal {
          let! file = forceFindOpenFileOrRead filePath |> Async.StartAsTask
          return (Result.toOption file, v)
        })

    AMap.union loses wins

  let allFSharpProjectOptions =
    allFSharpFilesAndProjectOptions
    |> AMapAsync.mapAsyncAVal (fun filePath (file, options) ctok -> AsyncAVal.constant options)

  let allFilesParsed =
    allFSharpFilesAndProjectOptions
    |> AMapAsync.mapAsyncAVal (fun filePath (file, options: LoadedProject list) ctok ->
      asyncAVal {
        let! (checker: FSharpCompilerServiceChecker) = checker

        return!
          taskOption {
            let! project = options |> selectProject
            let options = project.FSharpProjectOptions
            let parseOpts = Utils.projectOptionsToParseOptions project.FSharpProjectOptions
            let! file = file


            return! parseFile checker file parseOpts options
          }

      })


  let getAllProjectOptions () =
    async {
      let! set =
        allFSharpProjectOptions
        |> AMap.toASetValues
        |> ASet.force
        |> HashSet.toArray
        |> Array.map (AsyncAVal.forceAsync)
        |> Async.parallel75

      return set |> Array.collect (List.toArray)
    }


  let getAllFSharpProjectOptions () =
    getAllProjectOptions ()
    |> Async.map (Array.map (fun x -> x.FSharpProjectOptions))

  let getProjectOptionsForFile (filePath: string<LocalPath>) =
    asyncAVal {
      match! allFSharpProjectOptions |> AMapAsync.tryFindA filePath with
      | Some projs -> return projs
      | None -> return []
    }

  let autoCompleteItems
    : cmap<DeclName, DeclarationListItem *
      Position *
      string<LocalPath> *
      (Position -> option<string>) *
      FSharp.Compiler.Syntax.ParsedInput> =
    cmap ()

  let getAutoCompleteByDeclName name = autoCompleteItems |> AMap.tryFind name

  let autoCompleteNamespaces =
    autoCompleteItems
    |> AMap.choose (fun name (d, pos, fn, getline, ast) ->

      Commands.calculateNamespaceInsert (fun () -> Some ast) d pos getline)

  let getAutoCompleteNamespacesByDeclName name = autoCompleteNamespaces |> AMap.tryFind name


  /// <summary>Gets Parse and Check results of a given file while also handling other concerns like Progress, Logging, Eventing.</summary>
  /// <param name="checker">The FSharpCompilerServiceChecker.</param>
  /// <param name="file">The name of the file in the project whose source to find a typecheck.</param>
  /// <param name="options">The options for the project or script.</param>
  /// <param name="shouldCache">Determines if the typecheck should be cached for autocompletions.</param>
  /// <returns></returns>
  let parseAndCheckFile (checker: FSharpCompilerServiceChecker) (file: VolatileFile) options shouldCache =
    async {
      let tags =
        [ SemanticConventions.fsac_sourceCodePath, box (UMX.untag file.Source.FileName)
          SemanticConventions.projectFilePath, box (options.ProjectFileName) ]

      use _ = fsacActivitySource.StartActivityForType(thisType, tags = tags)


      logger.info (
        Log.setMessage "Getting typecheck results for {file} - {hash} - {date}"
        >> Log.addContextDestructured "file" file.Source.FileName
        >> Log.addContextDestructured "hash" (file.Source.GetHashCode())
        >> Log.addContextDestructured "date" (file.LastTouched)
      )

      let! ct = Async.CancellationToken

      use progressReport = new ServerProgressReport(lspClient)

      let simpleName = Path.GetFileName(UMX.untag file.Source.FileName)
      do! progressReport.Begin($"Typechecking {simpleName}", message = $"{file.Source.FileName}")

      let! result =
        checker.ParseAndCheckFileInProject(
          file.Source.FileName,
          (file.Source.GetHashCode()),
          file.Source,
          options,
          shouldCache = shouldCache
        )
        |> Debug.measureAsync $"checker.ParseAndCheckFileInProject - {file.Source.FileName}"

      do! progressReport.End($"Typechecked {file.Source.FileName}")

      notifications.Trigger(NotificationEvent.FileParsed(file.Source.FileName), ct)

      match result with
      | Error e ->
        logger.error (
          Log.setMessage "Typecheck failed for {file} with {error}"
          >> Log.addContextDestructured "file" file.FileName
          >> Log.addContextDestructured "error" e
        )

        return failwith e
      | Ok parseAndCheck ->
        logger.info (
          Log.setMessage "Typecheck completed successfully for {file}"
          >> Log.addContextDestructured "file" file.Source.FileName
        )

        Async.Start(
          async {

            fileParsed.Trigger(parseAndCheck.GetParseResults, options, ct)
            fileChecked.Trigger(parseAndCheck, file, ct)
            let checkErrors = parseAndCheck.GetParseResults.Diagnostics
            let parseErrors = parseAndCheck.GetCheckResults.Diagnostics

            let errors =
              Array.append checkErrors parseErrors
              |> Array.distinctBy (fun e ->
                e.Severity, e.ErrorNumber, e.StartLine, e.StartColumn, e.EndLine, e.EndColumn, e.Message)

            notifications.Trigger(NotificationEvent.ParseError(errors, file.Source.FileName), ct)
          },
          ct
        )


        return parseAndCheck
    }

  /// Bypass Adaptive checking and tell the checker to check a file
  let bypassAdaptiveTypeCheck (filePath: string<LocalPath>) opts =
    async {
      try
        logger.info (
          Log.setMessage "Forced Check : {file}"
          >> Log.addContextDestructured "file" filePath
        )

        let checker = checker |> AVal.force

        match! forceFindOpenFileOrRead filePath with
        // Don't cache for autocompletions as we really only want to cache "Opened" files.
        | Ok(fileInfo) -> return! parseAndCheckFile checker fileInfo opts false |> Async.Ignore
        | _ -> ()
      with e ->

        logger.warn (
          Log.setMessage "Forced Check error : {file}"
          >> Log.addContextDestructured "file" filePath
          >> Log.addExn e
        )
    }


  let openFilesToRecentCheckedFilesResults =
    openFilesToChangesAndProjectOptions
    |> AMapAsync.mapAsyncAVal (fun _ (info, projectOptions) _ ->
      asyncAVal {
        let file = info.Source.FileName
        let! checker = checker

        return
          option {
            let! opts = selectProject projectOptions
            return! checker.TryGetRecentCheckResultsForFile(file, opts.FSharpProjectOptions, info.Source)
          }
      })

  let openFilesToCheckedFilesResults =
    openFilesToChangesAndProjectOptions
    |> AMapAsync.mapAsyncAVal (fun _ (info, projectOptions) ctok ->
      asyncAVal {
        let file = info.Source.FileName
        let! checker = checker

        return!
          taskOption {
            let! opts = selectProject projectOptions
            and! cts = tryGetOpenFileToken file

            return!
              parseAndCheckFile checker info opts.FSharpProjectOptions true
              |> Async.withCancellation cts.Token
              |> fun work -> Async.StartImmediateAsTask(work, ctok)
          }

      })

  let getParseResults filePath = allFilesParsed |> AMapAsync.tryFindAndFlatten filePath

  let getTypeCheckResults filePath = openFilesToCheckedFilesResults |> AMapAsync.tryFindAndFlatten (filePath)

  let getRecentTypeCheckResults filePath =
    openFilesToRecentCheckedFilesResults |> AMapAsync.tryFindAndFlatten (filePath)

  let tryGetLineStr pos (text: IFSACSourceText) =
    text.GetLine(pos)
    |> Result.ofOption (fun () -> $"No line in {text.FileName} at position {pos}")

  let forceGetParseResults filePath =
    async {
      let! results = getParseResults filePath |> AsyncAVal.forceAsync
      return results |> Result.ofOption (fun () -> $"No parse results for {filePath}")
    }

  let forceGetRecentTypeCheckResults filePath =
    async {
      let! results = getRecentTypeCheckResults filePath |> AsyncAVal.forceAsync
      return results |> Result.ofOption (fun () -> $"No typecheck results for {filePath}")
    }

  let forceGetTypeCheckResults (filePath: string<LocalPath>) =
    async {
      let! results = getTypeCheckResults (filePath) |> AsyncAVal.forceAsync
      return results |> Result.ofOption (fun () -> $"No typecheck results for {filePath}")
    }

  /// <summary>
  /// This will attempt to get typecheck results in this order
  ///
  /// 1. From our internal typecheck cache
  /// 2. From checker.TryGetRecentCheckResultsForFile
  /// 3. Failing both, will type check the file.
  ///
  /// Additionally, it will start typechecking the file in the background to force latest results on the next request.
  /// </summary>
  /// <param name="filePath">The name of the file in the project whose source to find a typecheck.</param>
  /// <returns>A Result of ParseAndCheckResults</returns>
  let forceGetTypeCheckResultsStale (filePath: string<LocalPath>) =
    asyncAVal {
      let! (checker: FSharpCompilerServiceChecker) = checker

      let inline tryGetLastCheckResultForFile filePath =
        checker.TryGetLastCheckResultForFile(filePath)
        |> Result.ofOption (fun () -> $"No typecheck results for {filePath}")
        |> async.Return

      return
        tryGetLastCheckResultForFile filePath
        |> AsyncResult.orElseWith (fun _ -> forceGetRecentTypeCheckResults filePath)
        |> AsyncResult.orElseWith (fun _ -> forceGetTypeCheckResults filePath)
        |> Async.map (fun r ->
          Async.Start(
            async {
              // This needs to be in a try catch as it can throw on cancellation which causes the server to crash
              try
                do!
                  forceGetTypeCheckResults filePath
                  |> Async.Ignore<Result<ParseAndCheckResults, string>>
              with e ->
                ()
            }
          )

          r)
    }
    |> AsyncAVal.forceAsync

  let allFilesToDeclarations =
    allFilesParsed
    |> AMap.map (fun k v -> v |> AsyncAVal.mapOption (fun p _ -> p.GetNavigationItems().Declarations))

  let getAllDeclarations () =
    async {
      let! results =
        allFilesToDeclarations
        |> AMap.force
        |> HashMap.toArray
        |> Array.map (fun (k, v) ->
          async {
            let! decls = AsyncAVal.forceAsync v
            return Option.map (fun v -> k, v) decls
          })
        |> Async.parallel75

      return results |> Array.Parallel.choose id

    }

  let getDeclarations filename = allFilesToDeclarations |> AMapAsync.tryFindAndFlatten filename

  let getFilePathAndPosition (p: ITextDocumentPositionParams) =
    let filePath = p.GetFilePath() |> Utils.normalizePath
    let pos = p.GetFcsPos()
    filePath, pos


  let forceGetProjectOptions filePath =
    asyncAVal {
      let! projects = getProjectOptionsForFile filePath
      let project = selectProject projects

      return
        project
        |> Result.ofOption (fun () -> $"Could not find project containing {filePath}")

    }
    |> AsyncAVal.forceAsync

  let forceGetFSharpProjectOptions filePath =
    forceGetProjectOptions filePath
    |> Async.map (Result.map (fun p -> p.FSharpProjectOptions))

  let codeGenServer =
    { new ICodeGenerationService with
        member x.TokenizeLine(file, i) =
          asyncOption {
            let! (text) = forceFindOpenFileOrRead file |> Async.map Option.ofResult

            try
              let! line = text.Source.GetLine(Position.mkPos i 0)
              return Lexer.tokenizeLine [||] line
            with _ ->
              return! None
          }

        member x.GetSymbolAtPosition(file, pos) =
          asyncOption {
            try
              let! (text) = forceFindOpenFileOrRead file |> Async.map Option.ofResult
              let! line = tryGetLineStr pos text.Source |> Option.ofResult
              return! Lexer.getSymbol pos.Line pos.Column line SymbolLookupKind.Fuzzy [||]
            with _ ->
              return! None
          }

        member x.GetSymbolAndUseAtPositionOfKind(fileName, pos, kind) =
          asyncOption {
            let! symbol = x.GetSymbolAtPosition(fileName, pos)

            if symbol.Kind = kind then
              let! (text) = forceFindOpenFileOrRead fileName |> Async.map Option.ofResult
              let! line = tryGetLineStr pos text.Source |> Option.ofResult
              let! tyRes = forceGetTypeCheckResults fileName |> Async.map (Option.ofResult)
              let symbolUse = tyRes.TryGetSymbolUse pos line
              return! Some(symbol, symbolUse)
            else
              return! None
          }

        member x.ParseFileInProject(file) = forceGetParseResults file |> Async.map (Option.ofResult) }

  let getDependentProjectsOfProjects ps =
    let projectSnapshot = forceLoadProjects ()

    let allDependents = System.Collections.Generic.HashSet<FSharpProjectOptions>()

    let currentPass = ResizeArray()
    currentPass.AddRange(ps |> List.map (fun p -> p.ProjectFileName))

    let mutable continueAlong = true

    while continueAlong do
      let dependents =
        projectSnapshot
        |> Seq.filter (fun p ->
          p.FSharpProjectOptions.ReferencedProjects
          |> Seq.exists (fun r ->
            match r.ProjectFilePath with
            | None -> false
            | Some p -> currentPass.Contains(p)))

      if Seq.isEmpty dependents then
        continueAlong <- false
        currentPass.Clear()
      else
        for d in dependents do
          allDependents.Add d.FSharpProjectOptions |> ignore<bool>

        currentPass.Clear()
        currentPass.AddRange(dependents |> Seq.map (fun p -> p.ProjectFileName))

    Seq.toList allDependents

  let getDeclarationLocation (symbolUse, text) =
    let getProjectOptions file =
      async {
        let! projects = getProjectOptionsForFile file |> AsyncAVal.forceAsync

        return
          selectProject projects
          |> Option.map (fun project -> project.FSharpProjectOptions)


      }

    let projectsThatContainFile file =
      async {
        let! projects = getProjectOptionsForFile file |> AsyncAVal.forceAsync
        return projects |> List.map (fun p -> p.FSharpProjectOptions)
      }

    SymbolLocation.getDeclarationLocation (
      symbolUse,
      text,
      getProjectOptions,
      projectsThatContainFile,
      getDependentProjectsOfProjects
    )

  let symbolUseWorkspace
    (includeDeclarations: bool)
    (includeBackticks: bool)
    (errorOnFailureToFixRange: bool)
    pos
    lineStr
    text
    tyRes
    =

    let findReferencesForSymbolInFile (file: string<LocalPath>, project, symbol) =
      async {
        let checker = checker |> AVal.force

        if File.Exists(UMX.untag file) then
          // `FSharpChecker.FindBackgroundReferencesInFile` only works with existing files
          return! checker.FindReferencesForSymbolInFile(UMX.untag file, project, symbol)
        else
          // untitled script files
          match! forceGetTypeCheckResultsStale file with
          | Error _ -> return Seq.empty
          | Ok tyRes ->
            let! ct = Async.CancellationToken
            let usages = tyRes.GetCheckResults.GetUsesOfSymbolInFile(symbol, ct)
            return usages |> Seq.map (fun u -> u.Range)
      }

    let tryGetProjectOptionsForFsproj (file: string<LocalPath>) =
      forceGetFSharpProjectOptions file |> Async.map Option.ofResult

    Commands.symbolUseWorkspace
      getDeclarationLocation
      findReferencesForSymbolInFile
      forceFindSourceText
      tryGetProjectOptionsForFsproj
      (getAllFSharpProjectOptions >> Async.map Array.toSeq)
      includeDeclarations
      includeBackticks
      errorOnFailureToFixRange
      pos
      lineStr
      text
      tyRes

  let symbolUseWorkspace2
    (includeDeclarations: bool)
    (includeBackticks: bool)
    (errorOnFailureToFixRange: bool)
    pos
    lineStr
    text
    tyRes
    =
    let findReferencesForSymbolInFile (file: string<LocalPath>, project, symbol) =
      async {
        let checker = checker |> AVal.force

        if File.Exists(UMX.untag file) then
          // `FSharpChecker.FindBackgroundReferencesInFile` only works with existing files
          return! checker.FindReferencesForSymbolInFile(UMX.untag file, project, symbol)
        else
          // untitled script files
          match! forceGetTypeCheckResultsStale file with
          | Error _ -> return Seq.empty
          | Ok tyRes ->
            let! ct = Async.CancellationToken
            let usages = tyRes.GetCheckResults.GetUsesOfSymbolInFile(symbol, ct)
            return usages |> Seq.map (fun u -> u.Range)
      }

    let tryGetProjectOptionsForFsproj (file: string<LocalPath>) =
      forceGetFSharpProjectOptions file |> Async.map Option.ofResult

    Commands.symbolUseWorkspace
      getDeclarationLocation
      findReferencesForSymbolInFile
      forceFindSourceText
      tryGetProjectOptionsForFsproj
      (getAllFSharpProjectOptions >> Async.map Array.toSeq)
      includeDeclarations
      includeBackticks
      errorOnFailureToFixRange
      pos
      lineStr
      text
      tyRes

  let codefixes =

    let tryGetParseResultsForFile filePath pos =
      asyncResult {
        let! (file) = forceFindOpenFileOrRead filePath
        let! lineStr = file.Source |> tryGetLineStr pos
        and! tyRes = forceGetTypeCheckResults filePath
        return tyRes, lineStr, file.Source
      }

    let getRangeText fileName (range: Ionide.LanguageServerProtocol.Types.Range) =
      asyncResult {
        let! sourceText = forceFindSourceText fileName
        return! sourceText.GetText(protocolRangeToRange (UMX.untag fileName) range)
      }

    let tryFindUnionDefinitionFromPos = tryFindUnionDefinitionFromPos codeGenServer

    let getUnionPatternMatchCases tyRes pos sourceText line =
      Commands.getUnionPatternMatchCases tryFindUnionDefinitionFromPos tyRes pos sourceText line

    let unionCaseStubReplacements (config) () = Map.ofList [ "$1", config.UnionCaseStubGenerationBody ]


    let implementInterfaceConfig config () : ImplementInterface.Config =
      { ObjectIdentifier = config.InterfaceStubGenerationObjectIdentifier
        MethodBody = config.InterfaceStubGenerationMethodBody
        IndentationSize = config.IndentationSize }

    let recordStubReplacements config () = Map.ofList [ "$1", config.RecordStubGenerationBody ]

    let tryFindRecordDefinitionFromPos =
      RecordStubGenerator.tryFindRecordDefinitionFromPos codeGenServer

    let getRecordStub tyRes pos sourceText line =
      Commands.getRecordStub (tryFindRecordDefinitionFromPos) tyRes pos sourceText line

    let getLineText (sourceText: IFSACSourceText) (range: Ionide.LanguageServerProtocol.Types.Range) =
      sourceText.GetText(protocolRangeToRange (UMX.untag sourceText.FileName) range)
      |> Async.singleton

    let abstractClassStubReplacements config () =
      Map.ofList
        [ "$objectIdent", config.AbstractClassStubGenerationObjectIdentifier
          "$methodBody", config.AbstractClassStubGenerationMethodBody ]

    let tryFindAbstractClassExprInBufferAtPos =
      AbstractClassStubGenerator.tryFindAbstractClassExprInBufferAtPos codeGenServer

    let writeAbstractClassStub =
      AbstractClassStubGenerator.writeAbstractClassStub codeGenServer

    let getAbstractClassStub tyRes objExprRange sourceText lineStr =
      Commands.getAbstractClassStub
        tryFindAbstractClassExprInBufferAtPos
        writeAbstractClassStub
        tyRes
        objExprRange
        sourceText
        lineStr
      |> AsyncResult.foldResult id id

    let getLanguageVersion (file: string<LocalPath>) =
      async {
        let! projectOptions = forceGetProjectOptions file

        return
          match projectOptions with
          | Ok projectOptions -> projectOptions.LanguageVersion
          | Error _ -> LanguageVersionShim.defaultLanguageVersion.Value
      }

    config
    |> AVal.map (fun config ->
      [| Run.ifEnabled (fun _ -> config.UnusedOpensAnalyzer) (RemoveUnusedOpens.fix forceFindSourceText)
         Run.ifEnabled
           (fun _ -> config.ResolveNamespaces)
           (ResolveNamespace.fix tryGetParseResultsForFile Commands.getNamespaceSuggestions)
         ReplaceWithSuggestion.fix
         RemoveRedundantQualifier.fix
         Run.ifEnabled (fun _ -> config.UnusedDeclarationsAnalyzer) (RenameUnusedValue.fix tryGetParseResultsForFile)
         AddNewKeywordToDisposableConstructorInvocation.fix getRangeText
         Run.ifEnabled
           (fun _ -> config.UnionCaseStubGeneration)
           (GenerateUnionCases.fix
             forceFindSourceText
             tryGetParseResultsForFile
             getUnionPatternMatchCases
             (unionCaseStubReplacements config))
         ExternalSystemDiagnostics.linter
         ExternalSystemDiagnostics.analyzers
         Run.ifEnabled
           (fun _ -> config.InterfaceStubGeneration)
           (ImplementInterface.fix
             tryGetParseResultsForFile
             forceGetFSharpProjectOptions
             (implementInterfaceConfig config))
         Run.ifEnabled
           (fun _ -> config.RecordStubGeneration)
           (GenerateRecordStub.fix tryGetParseResultsForFile getRecordStub (recordStubReplacements config))
         Run.ifEnabled
           (fun _ -> config.AbstractClassStubGeneration)
           (GenerateAbstractClassStub.fix
             tryGetParseResultsForFile
             getAbstractClassStub
             (abstractClassStubReplacements config))
         AddMissingEqualsToTypeDefinition.fix forceFindSourceText
         ChangePrefixNegationToInfixSubtraction.fix forceFindSourceText
         ConvertDoubleEqualsToSingleEquals.fix getRangeText
         ChangeEqualsInFieldTypeToColon.fix
         WrapExpressionInParentheses.fix getRangeText
         ChangeRefCellDerefToNot.fix tryGetParseResultsForFile
         ChangeDowncastToUpcast.fix getRangeText
         MakeDeclarationMutable.fix tryGetParseResultsForFile forceGetFSharpProjectOptions
         UseMutationWhenValueIsMutable.fix tryGetParseResultsForFile
         ConvertInvalidRecordToAnonRecord.fix tryGetParseResultsForFile
         RemoveUnnecessaryReturnOrYield.fix tryGetParseResultsForFile getLineText
         ConvertCSharpLambdaToFSharpLambda.fix tryGetParseResultsForFile getLineText
         AddMissingFunKeyword.fix forceFindSourceText getLineText
         MakeOuterBindingRecursive.fix tryGetParseResultsForFile getLineText
         AddMissingRecKeyword.fix forceFindSourceText getLineText
         ConvertBangEqualsToInequality.fix getRangeText
         ChangeDerefBangToValue.fix tryGetParseResultsForFile getLineText
         RemoveUnusedBinding.fix tryGetParseResultsForFile
         AddTypeToIndeterminateValue.fix tryGetParseResultsForFile forceGetFSharpProjectOptions
         ChangeTypeOfNameToNameOf.fix tryGetParseResultsForFile
         AddMissingInstanceMember.fix
         AddMissingXmlDocumentation.fix tryGetParseResultsForFile
         AddExplicitTypeAnnotation.fix tryGetParseResultsForFile
         ConvertPositionalDUToNamed.fix tryGetParseResultsForFile getRangeText
         ConvertTripleSlashCommentToXmlTaggedDoc.fix tryGetParseResultsForFile getRangeText
         GenerateXmlDocumentation.fix tryGetParseResultsForFile
         RemoveRedundantAttributeSuffix.fix tryGetParseResultsForFile
         Run.ifEnabled
           (fun _ -> config.AddPrivateAccessModifier)
           (AddPrivateAccessModifier.fix tryGetParseResultsForFile symbolUseWorkspace2)
         UseTripleQuotedInterpolation.fix tryGetParseResultsForFile getRangeText
         RenameParamToMatchSignature.fix tryGetParseResultsForFile
         RemovePatternArgument.fix tryGetParseResultsForFile
         ToInterpolatedString.fix tryGetParseResultsForFile getLanguageVersion
         AdjustConstant.fix tryGetParseResultsForFile |])

  let forgetDocument (uri: DocumentUri) =
    async {
      let filePath = uri |> Path.FileUriToLocalPath |> Utils.normalizePath

      let doesNotExist (file: string<LocalPath>) = not (File.Exists(UMX.untag file))

      let isOutsideWorkspace (file: string<LocalPath>) =
        asyncAVal {
          let! rootPath = rootPath

          match rootPath with
          | None -> return true // no root workspace specified
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
              return false
            else
              let! projectOptions = getProjectOptionsForFile file

              match projectOptions |> selectProject with
              | None -> return true
              | Some projectOptions ->
                if doesNotExist (UMX.tag projectOptions.ProjectFileName) then
                  return true // script file
                else
                  // issue: fs-file does never get removed from project options (-> requires reload of FSAC to register)
                  // -> don't know if file still part of project (file might have been removed from project)
                  // -> keep cache for file
                  return false
        }

        |> AsyncAVal.forceAsync

      transact (fun () ->
        openFiles.Remove filePath |> ignore<bool>

        match openFilesTokens.TryRemove(filePath) with
        | (true, cts) -> cancelToken filePath cts
        | _ -> ()

        textChanges.Remove filePath |> ignore<bool>)

      let! isOutsideWorkspace = isOutsideWorkspace filePath

      if doesNotExist filePath || isOutsideWorkspace then
        logger.info (
          Log.setMessage "Removing cached data for {file}."
          >> Log.addContext "file" filePath
        )

        diagnosticCollections.ClearFor(uri)
      else
        logger.info (
          Log.setMessage "File {file} exists inside workspace so diagnostics will not be cleared"
          >> Log.addContext "file" filePath
        )
    }


  let getDependentFilesForFile file =
    async {
      let! projects = getProjectOptionsForFile file |> AsyncAVal.forceAsync

      return
        projects
        |> List.toArray
        |> Array.collect (fun proj ->
          logger.info (
            Log.setMessage "Source Files: {sourceFiles}"
            >> Log.addContextDestructured "sourceFiles" proj.SourceFiles
          )

          let idx = proj.SourceFiles |> Array.findIndex (fun x -> x = UMX.untag file)

          proj.SourceFiles
          |> Array.splitAt idx
          |> snd
          |> Array.map (fun sourceFile -> proj.FSharpProjectOptions, sourceFile))
        |> Array.distinct
    }


  let bypassAdaptiveAndCheckDepenenciesForFile (filePath: string<LocalPath>) =
    async {
      let tags = [ SemanticConventions.fsac_sourceCodePath, box (UMX.untag filePath) ]
      use _ = fsacActivitySource.StartActivityForType(thisType, tags = tags)
      let! dependentFiles = getDependentFilesForFile filePath

      let! projs = getProjectOptionsForFile filePath |> AsyncAVal.forceAsync

      let dependentProjects =
        projs
        |> List.map (fun x -> x.FSharpProjectOptions)
        |> getDependentProjectsOfProjects
        |> List.toArray
        |> Array.collect (fun proj -> proj.SourceFiles |> Array.map (fun sourceFile -> proj, sourceFile))

      let mutable checksCompleted = 0

      use progressReporter = new ServerProgressReport(lspClient)

      let percentage numerator denominator =
        if denominator = 0 then
          0u
        else
          ((float numerator) / (float denominator)) * 100.0 |> uint32

      let checksToPerform =
        let innerChecks =
          Array.concat [| dependentFiles; dependentProjects |]
          |> Array.filter (fun (_, file) ->
            file.Contains "AssemblyInfo.fs" |> not
            && file.Contains "AssemblyAttributes.fs" |> not)

        let checksToPerformLength = innerChecks.Length

        innerChecks
        |> Array.map (fun (proj, file) ->
          let file = UMX.tag file

          let token =
            tryGetOpenFileToken filePath
            |> Option.map (fun cts -> cts.Token)
            |> Option.defaultWith (fun () -> CancellationToken.None)

          bypassAdaptiveTypeCheck (file) (proj)
          |> Async.withCancellationSafe (fun () -> token)
          |> Async.Ignore
          |> Async.bind (fun _ ->
            async {
              let checksCompleted = Interlocked.Increment(&checksCompleted)

              do!
                progressReporter.Report(
                  message = $"{checksCompleted}/{checksToPerformLength} remaining",
                  percentage = percentage checksCompleted checksToPerformLength
                )
            }))


      do!
        progressReporter.Begin(
          "Typechecking Dependent F# files",
          message = $"0/{checksToPerform.Length} remaining",
          percentage = percentage 0 checksToPerform.Length
        )

      do! checksToPerform |> Async.parallel75 |> Async.Ignore<unit array>

    }


  member private x.handleSemanticTokens (filePath: string<LocalPath>) range : AsyncLspResult<SemanticTokens option> =
    asyncResult {

      let! tyRes = forceGetTypeCheckResults filePath |> AsyncResult.ofStringErr
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
      handlerFormattedDoc: (IFSACSourceText * string) -> TextEdit[],
      handleFormattedRange: (IFSACSourceText * string * FormatSelectionRange) -> TextEdit[]
    ) : Async<LspResult<option<_>>> =
    asyncResult {
      let tags = [ SemanticConventions.fsac_sourceCodePath, box fileName ]
      use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

      try


        let! res = action () |> AsyncResult.ofStringErr

        let rootPath = rootPath |> AVal.force

        match res with
        | (FormatDocumentResponse.Formatted(sourceText, formatted)) ->
          let result = handlerFormattedDoc (sourceText, formatted)

          return (Some(result))
        | (FormatDocumentResponse.FormattedRange(sourceText, formatted, range)) ->
          let result = handleFormattedRange (sourceText, formatted, range)

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
                      Cli
                        .Wrap("dotnet")
                        .WithArguments("new tool-manifest")
                        .WithWorkingDirectory(rootPath)
                        .ExecuteBufferedAsync()
                        .Task
                      |> Async.AwaitTask

                    if result.ExitCode <> 0 then
                      fantomasLogger.warn (
                        Log.setMessage (sprintf "Unable to create a new tool manifest in %s" rootPath)
                      )
                  else
                    let dotConfigContent = File.ReadAllText dotConfig

                    if dotConfigContent.Contains("fantomas") then
                      // uninstall a older, non-compatible version of fantomas
                      let! result =
                        Cli
                          .Wrap("dotnet")
                          .WithArguments("tool uninstall fantomas")
                          .WithWorkingDirectory(rootPath)
                          .ExecuteBufferedAsync()
                          .Task
                        |> Async.AwaitTask

                      if result.ExitCode <> 0 then
                        fantomasLogger.warn (
                          Log.setMessage (
                            sprintf "Unable to uninstall a non compatible version of fantomas in %s" rootPath
                          )
                        )

                  let! result =
                    Cli
                      .Wrap("dotnet")
                      .WithArguments("tool install fantomas")
                      .WithWorkingDirectory(rootPath)
                      .ExecuteBufferedAsync()
                      .Task
                    |> Async.AwaitTask

                  if result.ExitCode = 0 then
                    fantomasLogger.info (Log.setMessage (sprintf "fantomas was installed locally at %A" rootPath))

                    do!
                      lspClient.WindowShowMessage
                        { Type = MessageType.Info
                          Message = "fantomas was installed locally" }

                    fantomasService.ClearCache()
                  else
                    fantomasLogger.warn (
                      Log.setMessage (sprintf "Unable to install a compatible version of fantomas in %s" rootPath)
                    )
                }

              )
              |> Option.defaultValue (async { return () })
          | (Some { Title = "Install globally" }) ->
            let! result =
              Cli
                .Wrap("dotnet")
                .WithArguments("tool install -g fantomas")
                .ExecuteBufferedAsync()
                .Task
              |> Async.AwaitTask

            if result.ExitCode = 0 then
              fantomasLogger.info (Log.setMessage "fantomas was installed globally")

              do!
                lspClient.WindowShowMessage
                  { Type = MessageType.Info
                    Message = "fantomas was installed globally" }

              fantomasService.ClearCache()
            else
              fantomasLogger.warn (Log.setMessage "Unable to install a compatible version of fantomas globally")
          | _ -> ()

          return! LspResult.internalError "Fantomas install not found."
        | (FormatDocumentResponse.Error ex) -> return! LspResult.internalError ex
      with e ->
        trace |> Tracing.recordException e
        logger.error (Log.setMessage "HandleFormatting Request Errored {p}" >> Log.addExn e)
        return! returnException e
    }

  member __.ScriptFileProjectOptions = scriptFileProjectOptions.Publish

  member private x.logUnimplementedRequest<'t, 'u>
    (
      argValue: 't,
      [<CallerMemberName; Optional; DefaultParameterValue("")>] caller: string
    ) =
    logger.info (
      Log.setMessage $"{caller} request: {{parms}}"
      >> Log.addContextDestructured "parms" argValue
    )

    Helpers.notImplemented<'u>

  member private x.logIgnoredNotification<'t>
    (
      argValue: 't,
      [<CallerMemberName; Optional; DefaultParameterValue("")>] caller: string
    ) =
    logger.info (
      Log.setMessage $"{caller} request: {{parms}}"
      >> Log.addContextDestructured "parms" argValue
    )

    Helpers.ignoreNotification

  interface IFSharpLspServer with
    override x.Shutdown() = (x :> System.IDisposable).Dispose() |> async.Return

    override _.Initialize(p: InitializeParams) =
      asyncResult {
        let tags = [ "InitializeParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (Log.setMessage "Initialize Request {p}" >> Log.addContextDestructured "p" p)

          let c =
            p.InitializationOptions
            |> Option.bind (fun options -> if options.HasValues then Some options else None)
            |> Option.map Server.deserialize<FSharpConfigDto>
            |> Option.map FSharpConfig.FromDto
            |> Option.defaultValue FSharpConfig.Default

          logger.info (
            Log.setMessage "Intialization options {items}"
            >> Log.addContextDestructured "items" c
          )

          let inlineValueToggle: InlineValueOptions option =
            match c.InlineValues.Enabled with
            | Some true -> Some { ResolveProvider = Some false }
            | Some false -> None
            | None -> None

          let actualRootPath =
            match p.RootUri with
            | Some rootUri -> Some(Path.FileUriToLocalPath rootUri)
            | None -> p.RootPath

          let projs =
            match p.RootPath, c.AutomaticWorkspaceInit with
            | None, _
            | _, false -> workspacePaths |> AVal.force
            | Some actualRootPath, true ->
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
              |> HashSet.ofList
              |> WorkspaceChosen.Projs

          transact (fun () ->
            rootPath.Value <- actualRootPath
            clientCapabilities.Value <- p.Capabilities
            lspClient.ClientCapabilities <- p.Capabilities

            diagnosticCollections.ClientSupportsDiagnostics <-
              match p.Capabilities with
              | Some { TextDocument = Some { PublishDiagnostics = Some _ } } -> true
              | _ -> false

            updateConfig c
            workspacePaths.Value <- projs)

          let defaultSettings =
            { Helpers.defaultServerCapabilities with
                TextDocumentSync =
                  Helpers.defaultServerCapabilities.TextDocumentSync
                  |> Option.map (fun x ->
                    { x with
                        Change = Some TextDocumentSyncKind.Incremental })
                InlineValueProvider = inlineValueToggle }

          return
            { InitializeResult.Default with
                Capabilities = defaultSettings }

        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "Initialize Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override __.Initialized(p: InitializedParams) =
      async {
        let tags = [ "InitializedParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (Log.setMessage "Initialized request {p}" >> Log.addContextDestructured "p" p)
          let! _ = parseAllFiles () |> AsyncAVal.forceAsync
          return ()
        with e ->

          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "Initialized Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return ()
      }

    override __.TextDocumentDidOpen(p: DidOpenTextDocumentParams) =
      async {
        let tags = [ "DidOpenTextDocumentParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "TextDocumentDidOpen Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let doc = p.TextDocument
          let filePath = doc.GetFilePath() |> Utils.normalizePath

          if isFileOpen filePath |> AVal.force then
            return ()
          else
            // We want to try to use the file system's datetime if available
            let file =
              VolatileFile.Create(sourceTextFactory.Create(filePath, doc.Text), doc.Version)

            updateOpenFiles file
            let! _ = forceGetTypeCheckResults filePath
            return ()
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentDidOpen Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return ()
      }

    override __.TextDocumentDidClose(p: DidCloseTextDocumentParams) =
      async {
        let tags = [ "DidCloseTextDocumentParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "TextDocumentDidClose Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let doc = p.TextDocument
          do! forgetDocument doc.Uri
          return ()

        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentDidClose Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return ()
      }

    override __.TextDocumentDidChange(p: DidChangeTextDocumentParams) =
      async {
        let tags = [ "DidChangeTextDocumentParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "TextDocumentDidChange Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let doc = p.TextDocument
          let filePath = doc.GetFilePath() |> Utils.normalizePath

          updateTextchanges filePath (p, DateTime.UtcNow)

          let! _ = forceGetTypeCheckResults filePath


          return ()
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentDidChange Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return ()
      }

    override __.TextDocumentDidSave(p: DidSaveTextDocumentParams) =
      async {
        let tags = [ "DidSaveTextDocumentParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try

          logger.info (
            Log.setMessage "TextDocumentDidSave Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let doc = p.TextDocument
          let filePath = doc.GetFilePath() |> Utils.normalizePath

          let file =
            option {
              let! oldFile = forceFindOpenFile filePath

              let oldFile =
                p.Text
                |> Option.map (fun t -> sourceTextFactory.Create(oldFile.FileName, t))
                |> Option.map (oldFile.SetSource)
                |> Option.defaultValue oldFile

              return oldFile.UpdateTouched()
            }
            |> Option.defaultWith (fun () ->
              // Very unlikely to get here
              VolatileFile.Create(sourceTextFactory.Create(filePath, p.Text.Value), 0))

          transact (fun () ->
            updateOpenFiles file
            textChanges.Remove filePath |> ignore<bool>)

          let! _ = forceGetTypeCheckResults filePath
          do! bypassAdaptiveAndCheckDepenenciesForFile filePath
          do! lspClient.CodeLensRefresh()

          logger.info (
            Log.setMessage "TextDocumentDidSave Request Finished: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          return ()
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentDidSave Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

        return ()
      }

    override __.TextDocumentCompletion(p: CompletionParams) =
      asyncResult {
        let tags = [ "CompletionParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "TextDocumentCompletion Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let (filePath, pos) = getFilePathAndPosition p

          let! volatileFile = forceFindOpenFileOrRead filePath |> AsyncResult.ofStringErr

          if volatileFile.Source.Length = 0 then
            return None // An empty file has empty completions. Otherwise we would error down there
          else

            let! lineStr = volatileFile.Source |> tryGetLineStr pos |> Result.ofStringErr

            if lineStr.StartsWith "#" then
              let completionList =
                { IsIncomplete = false
                  Items = KeywordList.hashSymbolCompletionItems
                  ItemDefaults = None }


              return! success (Some completionList)
            else
              let config = AVal.force config

              let rec retryAsyncOption (delay: TimeSpan) timesLeft handleError action =
                async {
                  match! action with
                  | Ok x -> return Ok x
                  | Error e when timesLeft >= 0 ->
                    let nextAction = handleError e
                    do! Async.Sleep(delay)
                    return! retryAsyncOption delay (timesLeft - 1) handleError nextAction
                  | Error e -> return Error e
                }

              let getCompletions forceGetTypeCheckResultsStale =
                asyncResult {

                  let! volatileFile = forceFindOpenFileOrRead filePath
                  let! lineStr = volatileFile.Source |> tryGetLineStr pos

                  // TextDocumentCompletion will sometimes come in before TextDocumentDidChange
                  // This will require the trigger character to be at the place VSCode says it is
                  // Otherwise we'll fail here and our retry logic will come into place
                  do!
                    match p.Context with
                    | Some({ triggerKind = CompletionTriggerKind.TriggerCharacter } as context) ->
                      volatileFile.Source.TryGetChar pos = context.triggerCharacter
                    | _ -> true
                    |> Result.requireTrue $"TextDocumentCompletion was sent before TextDocumentDidChange"

                  // Special characters like parentheses, brackets, etc. require a full type check
                  let isSpecialChar = Option.exists (Char.IsLetterOrDigit >> not)

                  let previousCharacter = volatileFile.Source.TryGetChar(FcsPos.subtractColumn pos 1)

                  let! typeCheckResults =
                    if isSpecialChar previousCharacter then
                      forceGetTypeCheckResults filePath
                    else
                      forceGetTypeCheckResultsStale filePath

                  let getAllSymbols () =
                    if config.ExternalAutocomplete then
                      typeCheckResults.GetAllEntities true
                    else
                      []

                  let! (decls, residue, shouldKeywords) =
                    Debug.measure "TextDocumentCompletion.TryGetCompletions" (fun () ->
                      typeCheckResults.TryGetCompletions pos lineStr None getAllSymbols
                      |> AsyncResult.ofOption (fun () -> "No TryGetCompletions results"))

                  do! Result.requireNotEmpty "Should not have empty completions" decls

                  return Some(decls, residue, shouldKeywords, typeCheckResults, getAllSymbols, volatileFile)
                }

              let handleError e =
                match e with
                | "Should not have empty completions" ->
                  // If we don't get any completions, assume we need to wait for a full typecheck
                  getCompletions forceGetTypeCheckResults
                | _ -> getCompletions forceGetTypeCheckResultsStale

              match!
                retryAsyncOption
                  (TimeSpan.FromMilliseconds(15.))
                  100
                  handleError
                  (getCompletions forceGetTypeCheckResultsStale)
                |> AsyncResult.ofStringErr
              with
              | None -> return! success (None)
              | Some(decls, _, shouldKeywords, typeCheckResults, _, volatileFile) ->

                return!
                  Debug.measure "TextDocumentCompletion.TryGetCompletions success"
                  <| fun () ->
                    transact (fun () ->
                      HashMap.OfList(
                        [ for d in decls do
                            d.NameInList, (d, pos, filePath, volatileFile.Source.GetLine, typeCheckResults.GetAST) ]
                      )
                      |> autoCompleteItems.UpdateTo)
                    |> ignore<bool>

                    let includeKeywords = config.KeywordsAutocomplete && shouldKeywords

                    let items =
                      decls
                      |> Array.mapi (fun id d ->
                        let code =
                          if
                            System.Text.RegularExpressions.Regex.IsMatch(d.NameInList, """^[a-zA-Z][a-zA-Z0-9']+$""")
                          then
                            d.NameInList
                          elif d.NamespaceToOpen.IsSome then
                            d.NameInList
                          else
                            FSharpKeywords.NormalizeIdentifierBackticks d.NameInList

                        let label =
                          match d.NamespaceToOpen with
                          | Some no -> sprintf "%s (open %s)" d.NameInList no
                          | None -> d.NameInList

                        { CompletionItem.Create(d.NameInList) with
                            Kind = (AVal.force glyphToCompletionKind) d.Glyph
                            InsertText = Some code
                            SortText = Some(sprintf "%06d" id)
                            FilterText = Some d.NameInList
                            Label = label })

                    let its =
                      if not includeKeywords then
                        items
                      else
                        Array.append items KeywordList.keywordCompletionItems

                    let completionList =
                      { IsIncomplete = false
                        Items = its
                        ItemDefaults = None }

                    success (Some completionList)

        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentCompletion Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override __.CompletionItemResolve(ci: CompletionItem) =
      let mapHelpText (ci: CompletionItem) (text: HelpText) =
        match text with
        | HelpText.Simple(symbolName, text) ->
          let d = Documentation.Markup(markdown text)

          { ci with
              Detail = Some symbolName
              Documentation = Some d }
        | HelpText.Full(name, tip, additionalEdit) ->
          let (si, comment) = TipFormatter.formatCompletionItemTip tip

          let edits, label =
            match additionalEdit with
            | None -> None, ci.Label
            | Some { Namespace = ns; Position = fcsPos } ->
              let text =
                let indentation = String(' ', fcsPos.Column)
                $"{indentation}open {ns}\n"

              let insertPos =
                { (fcsPos |> fcsPosToLsp) with
                    Character = 0 }

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

            match getAutoCompleteByDeclName sym |> AVal.force with
            | None -> //Isn't in sync filled cache, we don't have result
              CoreResponse.ErrorRes(sprintf "No help text available for symbol '%s'" sym)
            | Some(decl, pos, fn, _, _) -> //Is in sync filled cache, try to get results from async filled caches or calculate if it's not there

              let tip = decl.Description

              let n =
                match getAutoCompleteNamespacesByDeclName sym |> AVal.force with
                | None -> None
                | Some s -> Some s

              CoreResponse.Res(HelpText.Full(sym, tip, n))


      asyncResult {
        let tags = [ "CompletionItem", box ci ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

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
              |> Result.bimap
                (function
                | None -> ci
                | Some text -> mapHelpText ci text)
                (fun _ -> ci)
              |> success

        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "CompletionItemResolve Request Errored {p}"
            >> Log.addContextDestructured "p" ci
            >> Log.addExn e
          )

          return! returnException e
      }

    override x.TextDocumentSignatureHelp(p: SignatureHelpParams) =
      asyncResult {
        let tags = [ "SignatureHelpParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "TextDocumentSignatureHelp Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )


          let (filePath, pos) = getFilePathAndPosition p
          let! volatileFile = forceFindOpenFileOrRead filePath |> AsyncResult.ofStringErr
          and! tyRes = forceGetTypeCheckResults filePath |> AsyncResult.ofStringErr



          let charAtCaret = p.Context |> Option.bind (fun c -> c.TriggerCharacter)

          match!
            SignatureHelp.getSignatureHelpFor (tyRes, pos, volatileFile.Source, charAtCaret, None)
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
                    { ParameterInformation.Label = U2.First p.ParameterName
                      Documentation = Some(Documentation.String p.CanonicalTypeTextForSorting) })

                let d = Documentation.Markup(markdown comment)

                { SignatureInformation.Label = signature
                  Documentation = Some d
                  Parameters = Some parameters
                  ActiveParameter = None })

            let res =
              { Signatures = sigs
                ActiveSignature = sigHelp.ActiveOverload
                ActiveParameter = sigHelp.ActiveParameter }

            return! success (Some res)
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentSignatureHelp Request: {parms}"
            >> Log.addContextDestructured "parms" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override x.TextDocumentHover(p: TextDocumentPositionParams) =
      asyncResult {
        let tags = [ "TextDocumentPositionParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "TextDocumentHover Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let (filePath, pos) = getFilePathAndPosition p
          let! volatileFile = forceFindOpenFileOrRead filePath |> AsyncResult.ofStringErr
          let! lineStr = volatileFile.Source |> tryGetLineStr pos |> Result.ofStringErr
          and! tyRes = forceGetTypeCheckResultsStale filePath |> AsyncResult.ofStringErr

          match tyRes.TryGetToolTipEnhanced pos lineStr with
          | Ok(Some tooltipResult) ->
            logger.info (
              Log.setMessage "TryGetToolTipEnhanced : {parms}"
              >> Log.addContextDestructured "parms" tooltipResult
            )

            let formatCommentStyle =
              let config = AVal.force config

              if config.TooltipMode = "full" then
                TipFormatter.FormatCommentStyle.FullEnhanced
              else if config.TooltipMode = "summary" then
                TipFormatter.FormatCommentStyle.SummaryOnly
              else
                TipFormatter.FormatCommentStyle.Legacy

            match TipFormatter.tryFormatTipEnhanced tooltipResult.ToolTipText formatCommentStyle with
            | TipFormatter.TipFormatterResult.Success tooltipInfo ->

              // Display the signature as a code block
              let signature =
                tooltipResult.Signature
                |> TipFormatter.prepareSignature
                |> (fun content -> MarkedString.WithLanguage { Language = "fsharp"; Value = content })

              // Display each footer line as a separate line
              let footerLines =
                tooltipResult.Footer
                |> TipFormatter.prepareFooterLines
                |> Array.map MarkedString.String

              let contents =
                [| signature
                   MarkedString.String tooltipInfo.DocComment
                   match tooltipResult.SymbolInfo with
                   | TryGetToolTipEnhancedResult.Keyword _ -> ()
                   | TryGetToolTipEnhancedResult.Symbol symbolInfo ->
                     TipFormatter.renderShowDocumentationLink
                       tooltipInfo.HasTruncatedExamples
                       symbolInfo.XmlDocSig
                       symbolInfo.Assembly
                     |> MarkedString.String
                   yield! footerLines |]

              let response =
                { Contents = MarkedStrings contents
                  Range = None }

              return (Some response)

            | TipFormatter.TipFormatterResult.Error error ->
              let contents = [| MarkedString.String "<Note>"; MarkedString.String error |]

              let response =
                { Contents = MarkedStrings contents
                  Range = None }

              return (Some response)

            | TipFormatter.TipFormatterResult.None -> return None

          | Ok(None) ->

            return! LspResult.internalError $"No TryGetToolTipEnhanced results for {filePath}"
          | Error e ->
            trace.RecordError(e, "TextDocumentHover.Error") |> ignore<Activity>
            logger.error (Log.setMessage "Failed with {error}" >> Log.addContext "error" e)
            return! LspResult.internalError e
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentHover Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override x.TextDocumentPrepareRename p =
      asyncResult {
        logger.info (
          Log.setMessage "TextDocumentOnPrepareRename Request: {parms}"
          >> Log.addContextDestructured "parms" p
        )

        let (filePath, pos) = getFilePathAndPosition p
        let! volatileFile = forceFindOpenFileOrRead filePath |> AsyncResult.ofStringErr
        let! lineStr = volatileFile.Source |> tryGetLineStr pos |> Result.ofStringErr
        let! tyRes = forceGetTypeCheckResults filePath |> AsyncResult.ofStringErr

        let! (_, _, range) =
          Commands.renameSymbolRange getDeclarationLocation false pos lineStr volatileFile.Source tyRes
          |> AsyncResult.mapError (fun msg -> JsonRpc.Error.Create(JsonRpc.ErrorCodes.invalidParams, msg))

        return range |> fcsRangeToLsp |> PrepareRenameResult.Range |> Some
      }

    override x.TextDocumentRename(p: RenameParams) =
      asyncResult {
        let tags = [ "RenameParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "TextDocumentRename Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let (filePath, pos) = getFilePathAndPosition p
          let! volatileFile = forceFindOpenFileOrRead filePath |> AsyncResult.ofStringErr
          let! lineStr = volatileFile.Source |> tryGetLineStr pos |> Result.ofStringErr
          and! tyRes = forceGetTypeCheckResults filePath |> AsyncResult.ofStringErr

          // validate name and surround with backticks if necessary
          let! newName =
            Commands.adjustRenameSymbolNewName pos lineStr volatileFile.Source tyRes p.NewName
            |> AsyncResult.mapError (fun msg -> JsonRpc.Error.Create(JsonRpc.ErrorCodes.invalidParams, msg))

          // safety check: rename valid?
          let! _ =
            Commands.renameSymbolRange getDeclarationLocation false pos lineStr volatileFile.Source tyRes
            |> AsyncResult.mapError (fun msg -> JsonRpc.Error.Create(JsonRpc.ErrorCodes.invalidParams, msg))

          let! ranges =
            symbolUseWorkspace true true true pos lineStr volatileFile.Source tyRes
            |> AsyncResult.mapError (fun msg -> JsonRpc.Error.Create(JsonRpc.ErrorCodes.invalidParams, msg))

          let! documentChanges =
            ranges
            |> Seq.map (fun kvp ->
              async {
                let edits =
                  kvp.Value
                  |> Array.map (fun range ->
                    let range = fcsRangeToLsp range
                    { Range = range; NewText = newName })

                let file: string<LocalPath> = kvp.Key

                let! version =
                  async {
                    let! file = forceFindOpenFileOrRead file
                    return file |> Option.ofResult |> Option.map (fun (f) -> f.Version)
                  }

                return
                  { TextDocument =
                      { Uri = Path.FilePathToUri(UMX.untag file)
                        Version = version }
                    Edits = edits }
              })
            |> Async.parallel75

          let clientCapabilities = clientCapabilities |> AVal.force |> Option.get
          return WorkspaceEdit.Create(documentChanges, clientCapabilities) |> Some

        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentRename Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override x.TextDocumentDefinition(p: TextDocumentPositionParams) =
      asyncResult {
        let tags = [ "TextDocumentPositionParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "TextDocumentDefinition Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let (filePath, pos) = getFilePathAndPosition p
          let! volatileFile = forceFindOpenFileOrRead filePath |> AsyncResult.ofStringErr

          let! lineStr = volatileFile.Source |> tryGetLineStr pos |> Result.ofStringErr
          and! tyRes = forceGetTypeCheckResults filePath |> AsyncResult.ofStringErr
          let! decl = tyRes.TryFindDeclaration pos lineStr |> AsyncResult.ofStringErr
          return decl |> findDeclToLspLocation |> GotoResult.Single |> Some
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentDefinition Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override x.TextDocumentTypeDefinition(p: TextDocumentPositionParams) =
      asyncResult {
        let tags = [ "TextDocumentPositionParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "TextDocumentTypeDefinition Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let (filePath, pos) = getFilePathAndPosition p

          let! volatileFile = forceFindOpenFileOrRead filePath |> AsyncResult.ofStringErr
          let! lineStr = volatileFile.Source |> tryGetLineStr pos |> Result.ofStringErr
          and! tyRes = forceGetTypeCheckResults filePath |> AsyncResult.ofStringErr
          let! decl = tyRes.TryFindTypeDeclaration pos lineStr |> AsyncResult.ofStringErr
          return decl |> findDeclToLspLocation |> GotoResult.Single |> Some
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentTypeDefinition Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override x.TextDocumentReferences(p: ReferenceParams) =
      asyncResult {
        let tags = [ "ReferenceParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "TextDocumentReferences Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let (filePath, pos) = getFilePathAndPosition p
          let! volatileFile = forceFindOpenFileOrRead filePath |> AsyncResult.ofStringErr
          let! lineStr = tryGetLineStr pos volatileFile.Source |> Result.ofStringErr
          and! tyRes = forceGetTypeCheckResults filePath |> AsyncResult.ofStringErr

          let! usages =
            symbolUseWorkspace true true false pos lineStr volatileFile.Source tyRes
            |> AsyncResult.mapError (JsonRpc.Error.InternalErrorMessage)

          let references =
            usages.Values |> Seq.collect (Seq.map fcsRangeToLspLocation) |> Seq.toArray

          return Some references
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentReferences Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override x.TextDocumentDocumentHighlight(p: TextDocumentPositionParams) =
      asyncResult {
        let tags = [ "TextDocumentPositionParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "TextDocumentDocumentHighlight Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let (filePath, pos) = getFilePathAndPosition p
          let! volatileFile = forceFindOpenFileOrRead filePath |> AsyncResult.ofStringErr
          let! lineStr = tryGetLineStr pos volatileFile.Source |> Result.ofStringErr
          and! tyRes = forceGetTypeCheckResults filePath |> AsyncResult.ofStringErr

          match
            tyRes.TryGetSymbolUseAndUsages pos lineStr
            |> Result.bimap CoreResponse.Res CoreResponse.InfoRes
          with
          | CoreResponse.InfoRes msg -> return None
          | CoreResponse.ErrorRes msg -> return! LspResult.internalError msg
          | CoreResponse.Res(symbol, uses) ->
            return
              uses
              |> Array.map (fun s ->
                { DocumentHighlight.Range = fcsRangeToLsp s.Range
                  Kind = None })
              |> Some
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentDocumentHighlight Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e

      }

    override x.TextDocumentImplementation(p: TextDocumentPositionParams) =
      asyncResult {
        let tags = [ "TextDocumentPositionParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "TextDocumentImplementation Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let (filePath, pos) = getFilePathAndPosition p
          let! volatileFile = forceFindOpenFileOrRead filePath |> AsyncResult.ofStringErr
          let! lineStr = tryGetLineStr pos volatileFile.Source |> Result.ofStringErr
          and! tyRes = forceGetTypeCheckResults filePath |> AsyncResult.ofStringErr

          logger.info (
            Log.setMessage "TextDocumentImplementation Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let getProjectOptions file =
            getProjectOptionsForFile file
            |> AsyncAVal.forceAsync
            |> Async.map List.head
            |> Async.map (fun x -> x.FSharpProjectOptions)

          let checker = checker |> AVal.force

          let getUsesOfSymbol (filePath, opts: _ list, symbol: FSharpSymbol) =
            checker.GetUsesOfSymbol(filePath, opts, symbol)

          let getAllProjects () =
            allFSharpProjectOptions
            |> AMap.force
            |> Seq.toList
            |> Seq.map (fun (k, v) ->
              async {
                let! proj = AsyncAVal.forceAsync v
                return Option.map (fun proj -> UMX.untag k, proj) (selectFSharpProject proj)
              })
            |> Async.parallel75
            |> Async.map (Array.choose id >> List.ofArray)

          let! res =
            Commands.symbolImplementationProject getProjectOptions getUsesOfSymbol getAllProjects tyRes pos lineStr
            |> AsyncResult.ofCoreResponse

          match res with
          | None -> return None
          | Some res ->
            let ranges: FSharp.Compiler.Text.Range[] =
              match res with
              | LocationResponse.Use(_, uses) -> uses |> Array.map (fun u -> u.Range)

            let mappedRanges = ranges |> Array.map fcsRangeToLspLocation

            match mappedRanges with
            | [||] -> return None
            | [| single |] -> return Some(GotoResult.Single single)
            | multiple -> return Some(GotoResult.Multiple multiple)
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentImplementation Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override __.TextDocumentDocumentSymbol(p: DocumentSymbolParams) =
      asyncResult {
        let tags = [ "DocumentSymbolParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "TextDocumentDocumentSymbol Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let fn = p.TextDocument.GetFilePath() |> Utils.normalizePath

          match! getDeclarations fn |> AsyncAVal.forceAsync with
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
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentDocumentSymbol Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }


    override __.WorkspaceSymbol(symbolRequest: WorkspaceSymbolParams) =
      asyncResult {
        let tags = [ "WorkspaceSymbolParams", box symbolRequest ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "WorkspaceSymbol Request: {parms}"
            >> Log.addContextDestructured "parms" symbolRequest
          )

          let glyphToSymbolKind = glyphToSymbolKind |> AVal.force

          let! decls = getAllDeclarations ()

          let res =
            decls
            |> Array.collect (fun (p, ns) ->
              let uri = Path.LocalPathToUri p

              ns
              |> Array.collect (fun n ->
                getSymbolInformations uri glyphToSymbolKind n (applyQuery symbolRequest.Query)))
            |> U2.First
            |> Some

          return res
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "WorkspaceSymbol Request Errored {p}"
            >> Log.addContextDestructured "p" symbolRequest
            >> Log.addExn e
          )

          return! returnException e
      }

    override x.TextDocumentFormatting(p: DocumentFormattingParams) =
      asyncResult {
        let tags = [ "DocumentFormattingParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          let doc = p.TextDocument
          let fileName = doc.GetFilePath() |> Utils.normalizePath

          let action () =
            logger.info (
              Log.setMessage "TextDocumentFormatting Request: {parms}"
              >> Log.addContextDestructured "parms" p
            )

            let tryGetFileCheckerOptionsWithLines file = forceFindSourceText file
            let formatDocumentAsync x = fantomasService.FormatDocumentAsync x
            Commands.formatDocument tryGetFileCheckerOptionsWithLines formatDocumentAsync fileName

          let handlerFormattedDoc (sourceText: IFSACSourceText, formatted: string) =
            let range =
              let zero = { Line = 0; Character = 0 }
              let lastPos = sourceText.LastFilePosition

              { Start = zero
                End = fcsPosToLsp lastPos }

            [| { Range = range; NewText = formatted } |]

          return! x.HandleFormatting(fileName, action, handlerFormattedDoc, (fun (_, _, _) -> [||]))
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentFormatting Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override x.TextDocumentRangeFormatting(p: DocumentRangeFormattingParams) =
      asyncResult {
        let tags = [ "DocumentRangeFormattingParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

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

            let tryGetFileCheckerOptionsWithLines file = forceFindSourceText file
            let formatSelectionAsync x = fantomasService.FormatSelectionAsync x
            Commands.formatSelection tryGetFileCheckerOptionsWithLines formatSelectionAsync fileName range


          let handlerFormattedRangeDoc (sourceText: IFSACSourceText, formatted: string, range: FormatSelectionRange) =
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
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentRangeFormatting Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }


    override x.TextDocumentCodeAction(codeActionParams: CodeActionParams) =
      asyncResult {
        let tags = [ "CodeActionParams", box codeActionParams ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "TextDocumentCodeAction Request: {parms}"
            >> Log.addContextDestructured "parms" codeActionParams
          )

          let (fixes: Async<Result<Fix list, string>[]>) =
            codefixes
            |> AVal.force
            |> Array.map (fun codeFix ->
              async {
                try
                  return! codeFix codeActionParams
                with e ->
                  logger.error (
                    Log.setMessage "Exception in CodeFix: {error}"
                    >> Log.addContextDestructured "error" (e.ToString())
                  )

                  return Ok []
              })
            |> Async.parallel75

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
            async {
              let! foo = forceFindOpenFileOrRead filePath
              return foo |> Option.ofResult |> Option.map (fun (f) -> f.Version)
            }

          let clientCapabilities = clientCapabilities |> AVal.force

          match actions with
          | [] -> return None
          | actions ->
            let! fixes =
              actions
              |> List.map (CodeAction.OfFix tryGetFileVersion clientCapabilities.Value)
              |> Async.parallel75

            return Some(fixes |> Array.map U2.Second)
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentCodeAction Request Errored {p}"
            >> Log.addContextDestructured "p" codeActionParams
            >> Log.addExn e
          )

          return! returnException e
      }

    override __.TextDocumentCodeLens(p: CodeLensParams) =
      asyncResult {
        let tags = [ "CodeLensParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "TextDocumentCodeLens Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let fn = p.TextDocument.GetFilePath() |> Utils.normalizePath

          match! getDeclarations (fn) |> AsyncAVal.forceAsync with
          | None -> return None
          | Some decls ->
            let config = AVal.force config

            let res =
              [| if config.LineLens.Enabled <> "replaceCodeLens" then
                   if config.CodeLenses.Signature.Enabled then
                     yield! decls |> Array.collect (getCodeLensInformation p.TextDocument.Uri "signature")
                 if config.CodeLenses.References.Enabled then
                   yield! decls |> Array.collect (getCodeLensInformation p.TextDocument.Uri "reference") |]

            return Some res
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentCodeLens Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override __.CodeLensResolve(p: CodeLens) =
      // JB:TODO see how to reuse existing code
      logger.info (
        Log.setMessage "CodeLensResolve Request: {parms}"
        >> Log.addContextDestructured "parms" p
      )

      let handler (f) (arg: CodeLens) : Async<LspResult<CodeLens>> =
        asyncResult {

          let tags = [ "CodeLens", box p ]
          use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

          let pos = protocolPosToPos arg.Range.Start

          let data = arg.Data.Value.ToObject<string[]>()

          let filePath = Path.FileUriToLocalPath data.[0] |> Utils.normalizePath

          try
            let! tyRes = forceGetTypeCheckResultsStale filePath |> AsyncResult.ofStringErr


            logger.info (
              Log.setMessage "CodeLensResolve - Cached typecheck results now available for {file}."
              >> Log.addContextDestructured "file" filePath
            )

            let! (sourceText: IFSACSourceText) = forceFindSourceText filePath |> AsyncResult.ofStringErr
            let! lineStr = sourceText |> tryGetLineStr pos |> Result.ofStringErr

            let typ = data.[1]
            let! r = Async.Catch(f arg pos tyRes sourceText lineStr typ filePath)

            match r with
            | Choice1Of2(r: LspResult<CodeLens option>) ->
              match r with
              | Ok(Some r) -> return r
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
            trace |> Tracing.recordException e

            logger.error (
              Log.setMessage "CodeLensResolve - Operation failed on {file}"
              >> Log.addContextDestructured "file" filePath
              >> Log.addExn e
            )

            return! returnException e
        }

      let writePayload (sourceFile: string<LocalPath>, triggerPos: pos, usageLocations: range[]) =
        Some
          [| JToken.FromObject(Path.LocalPathToUri sourceFile)
             JToken.FromObject(fcsPosToLsp triggerPos)
             JToken.FromObject(usageLocations |> Array.map fcsRangeToLspLocation) |]

      handler
        (fun p pos tyRes sourceText lineStr typ file ->
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
              | CoreResponse.Res(typ, parms, _) ->
                let formatted = SigantureData.formatSignature typ parms

                let cmd =
                  { Title = formatted
                    Command = ""
                    Arguments = None }

                return { p with Command = Some cmd } |> Some |> success
            elif typ = "reference" then
              let! uses =
                symbolUseWorkspace false true false pos lineStr sourceText tyRes
                |> AsyncResult.mapError (JsonRpc.Error.InternalErrorMessage)

              match uses with
              | Error msg ->
                logger.error (
                  Log.setMessage "CodeLensResolve - error getting symbol use for {file}"
                  >> Log.addContextDestructured "file" file
                  >> Log.addContextDestructured "error" msg
                )

                return
                  success (
                    Some
                      { p with
                          Command =
                            Some
                              { Title = ""
                                Command = ""
                                Arguments = None } }
                  )

              | Ok uses ->
                let allUses = uses.Values |> Array.concat

                let cmd =
                  if Array.isEmpty allUses then
                    { Title = "0 References"
                      Command = ""
                      Arguments = None }
                  else
                    { Title = $"%d{allUses.Length} References"
                      Command = "fsharp.showReferences"
                      Arguments = writePayload (file, pos, allUses) }

                return { p with Command = Some cmd } |> Some |> success
            else
              logger.error (
                Log.setMessage "CodeLensResolve - unknown type {file} - {typ}"
                >> Log.addContextDestructured "file" file
                >> Log.addContextDestructured "typ" typ
              )

              return { p with Command = None } |> Some |> success
          })
        p

    override __.WorkspaceDidChangeWatchedFiles(p: DidChangeWatchedFilesParams) =
      async {

        let tags = [ "DidChangeWatchedFilesParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "WorkspaceDidChangeWatchedFiles Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          for c in p.Changes do
            if c.Type = FileChangeType.Deleted then
              do! forgetDocument c.Uri
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "WorkspaceDidChangeWatchedFiles Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )
      }

    override __.WorkspaceDidChangeConfiguration(p: DidChangeConfigurationParams) =
      async {
        let tags = [ "DidChangeConfigurationParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "WorkspaceDidChangeConfiguration Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let dto = p.Settings |> Server.deserialize<FSharpConfigRequest>

          dto.FSharp
          |> Option.iter (fun fsharpConfig ->
            let c = config |> AVal.force
            let c = c.AddDto fsharpConfig
            updateConfig c)

        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "WorkspaceDidChangeConfiguration Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )
      }

    override __.TextDocumentFoldingRange(rangeP: FoldingRangeParams) =
      asyncResult {
        let tags = [ "FoldingRangeParams", box rangeP ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "TextDocumentFoldingRange Request: {parms}"
            >> Log.addContextDestructured "parms" rangeP
          )

          let file = rangeP.TextDocument.GetFilePath() |> Utils.normalizePath

          let getParseResultsForFile file =
            asyncResult {
              let! sourceText = forceFindSourceText file
              and! parseResults = forceGetParseResults file
              return sourceText, parseResults
            }

          let! scopes = Commands.scopesForFile getParseResultsForFile file |> AsyncResult.ofStringErr
          return scopes |> Seq.map Structure.toFoldingRange |> Set.ofSeq |> List.ofSeq |> Some
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentFoldingRange Request Errored {p}"
            >> Log.addContextDestructured "p" rangeP
            >> Log.addExn e
          )

          return! returnException e
      }

    override __.TextDocumentSelectionRange(selectionRangeP: SelectionRangeParams) =
      asyncResult {
        let tags = [ "SelectionRangeParams", box selectionRangeP ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

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
              let! parseResults = forceGetParseResults file
              return parseResults
            }

          let! ranges =
            Commands.getRangesAtPosition getParseResultsForFile file poss
            |> AsyncResult.ofStringErr

          return ranges |> List.choose mkSelectionRanges |> Some
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentSelectionRange Request Errored {p}"
            >> Log.addContextDestructured "p" selectionRangeP
            >> Log.addExn e
          )

          return! returnException e
      }

    override x.TextDocumentSemanticTokensFull(p: SemanticTokensParams) : AsyncLspResult<SemanticTokens option> =
      asyncResult {
        let tags = [ "SemanticTokensParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "TextDocumentSemanticTokensFull request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let fn = p.TextDocument.GetFilePath() |> Utils.normalizePath
          return! x.handleSemanticTokens fn None

        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentSemanticTokensFull Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }



    override x.TextDocumentSemanticTokensRange(p: SemanticTokensRangeParams) : AsyncLspResult<SemanticTokens option> =
      asyncResult {
        let tags = [ "SemanticTokensRangeParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "TextDocumentSemanticTokensRange request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let fn = p.TextDocument.GetFilePath() |> Utils.normalizePath
          let fcsRange = protocolRangeToRange (UMX.untag fn) p.Range
          return! x.handleSemanticTokens fn (Some fcsRange)
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentSemanticTokensRange Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override x.TextDocumentInlayHint(p: InlayHintParams) : AsyncLspResult<InlayHint[] option> =
      asyncResult {
        let tags = [ "InlayHintParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "TextDocumentInlayHint Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let filePath = p.TextDocument.GetFilePath() |> Utils.normalizePath
          let! volatileFile = forceFindOpenFileOrRead filePath |> AsyncResult.ofStringErr

          and! tyRes = forceGetTypeCheckResults filePath |> AsyncResult.ofStringErr

          let fcsRange = protocolRangeToRange (UMX.untag filePath) p.Range
          let config = config |> AVal.force

          let! hints =
            Commands.InlayHints(
              volatileFile.Source,
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
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentInlayHint Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override x.TextDocumentInlineValue(p: InlineValueParams) =
      asyncResult {
        let tags = [ "InlineValueParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "TextDocumentInlineValue Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let filePath = p.TextDocument.GetFilePath() |> Utils.normalizePath
          let! volatileFile = forceFindOpenFileOrRead filePath |> AsyncResult.ofStringErr


          let! tyRes = forceGetTypeCheckResults filePath |> AsyncResult.ofStringErr

          let fcsRange = protocolRangeToRange (UMX.untag filePath) p.Range

          let! pipelineHints = Commands.InlineValues(volatileFile.Source, tyRes)

          let hints =
            pipelineHints
            |> Array.map (fun (pos, linehints) ->
              { InlineValueText.Range = fcsPosToProtocolRange pos
                Text = linehints }
              |> InlineValue.InlineValueText)
            |> Some

          return hints
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentInlineValue Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    //unsupported -- begin
    override x.CodeActionResolve(p) = x.logUnimplementedRequest p

    override x.DocumentLinkResolve(p) = x.logUnimplementedRequest p

    override x.Exit() = x.logIgnoredNotification (())

    override x.InlayHintResolve p = x.logUnimplementedRequest p

    override x.TextDocumentDocumentColor p = x.logUnimplementedRequest p

    override x.TextDocumentColorPresentation p = x.logUnimplementedRequest p

    override x.TextDocumentDocumentLink p = x.logUnimplementedRequest p

    override x.TextDocumentOnTypeFormatting p = x.logUnimplementedRequest p

    override x.TextDocumentSemanticTokensFullDelta p = x.logUnimplementedRequest p

    override x.TextDocumentWillSave p = x.logIgnoredNotification p

    override x.TextDocumentWillSaveWaitUntil p = x.logUnimplementedRequest p

    override x.WorkspaceDidChangeWorkspaceFolders p = x.logIgnoredNotification p

    override x.WorkspaceDidCreateFiles p = x.logIgnoredNotification p

    override x.WorkspaceDidDeleteFiles p = x.logIgnoredNotification p

    override x.WorkspaceDidRenameFiles p = x.logIgnoredNotification p

    override x.WorkspaceExecuteCommand p = x.logUnimplementedRequest p

    override x.WorkspaceWillCreateFiles p = x.logUnimplementedRequest p

    override x.WorkspaceWillDeleteFiles p = x.logUnimplementedRequest p

    override x.WorkspaceWillRenameFiles p = x.logUnimplementedRequest p

    override x.CallHierarchyIncomingCalls(p: CallHierarchyIncomingCallsParams) =
      asyncResult {
        let tags = [ "CallHierarchyIncomingCalls", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "CallHierarchyIncomingCalls Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let filePath = Path.FileUriToLocalPath p.Item.Uri |> Utils.normalizePath
          let pos = protocolPosToPos p.Item.SelectionRange.Start
          let! volatileFile = forceFindOpenFileOrRead filePath |> AsyncResult.ofStringErr
          let! lineStr = tryGetLineStr pos volatileFile.Source |> Result.ofStringErr
          and! tyRes = forceGetTypeCheckResults filePath |> AsyncResult.ofStringErr


          let! usages =
            symbolUseWorkspace true true false pos lineStr volatileFile.Source tyRes
            |> AsyncResult.mapError (JsonRpc.Error.InternalErrorMessage)

          let locationToCallHierarchyItem (loc: Location) =
            asyncOption {

              // Don't process ourselves
              if p.Item.SelectionRange.Start = loc.Range.Start then
                do! None

              let fn = loc.Uri |> Path.FileUriToLocalPath |> Utils.normalizePath

              let! parseResults = getParseResults fn |> AsyncAVal.forceAsync

              // member _.TryRangeOfNameOfNearestOuterBindingContainingPos pos =
              // doesn't find things within member declarations, we'll need to copy and expand the logic
              // to find members
              let! containerRange =
                parseResults.TryRangeOfNameOfNearestOuterBindingContainingPos(protocolPosToPos loc.Range.Start)

              let! volatileFile = forceFindOpenFileOrRead fn |> Async.map Result.toOption
              let! lineStr = tryGetLineStr containerRange.Start volatileFile.Source |> Result.toOption

              let! (_, idents) = Lexer.findLongIdents (containerRange.Start.Column, lineStr)

              let retVals =
                { From =
                    { Name = String.concat "." idents
                      Kind = SymbolKind.Method
                      Tags = None
                      Detail = None
                      Uri = loc.Uri
                      Range = fcsRangeToLsp containerRange
                      SelectionRange = fcsRangeToLsp containerRange
                      Data = None }
                  FromRanges = [| loc.Range |] }


              return retVals
            }


          let! references =
            usages.Values
            |> Seq.collect (Seq.map fcsRangeToLspLocation)
            |> Seq.toArray
            |> Array.map locationToCallHierarchyItem
            |> Async.parallel75
            |> Async.map (Array.choose id)

          return Some references
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "CallHierarchyIncomingCalls Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e

      }

    override x.CallHierarchyOutgoingCalls p =
      asyncResult {
        let tags = [ "CallHierarchyOutgoingCalls", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "CallHierarchyOutgoingCalls Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let filePath = Path.FileUriToLocalPath p.Item.Uri |> Utils.normalizePath
          let pos = protocolPosToPos p.Item.SelectionRange.Start
          let! volatileFile = forceFindOpenFileOrRead filePath |> AsyncResult.ofStringErr
          let! lineStr = tryGetLineStr pos volatileFile.Source |> Result.ofStringErr
          and! tyRes = forceGetTypeCheckResults filePath |> AsyncResult.ofStringErr

          let! implFile =
            tyRes.GetCheckResults.ImplementationFile
            |> Result.ofOption (fun () -> "No implementation file found")
            |> Result.ofStringErr

          let membersOrFunctions = ResizeArray<_>()
          CallHierarchy.gatherFSharpMemberOrFunction (membersOrFunctions.Add) implFile.Declarations

          let createOutGoingCall (range: FSharp.Compiler.Text.Range, x: FSharpMemberOrFunctionOrValue) =

            let file = range.FileName |> Utils.normalizePath |> Path.LocalPathToUri
            // let! tyRes = forceGetTypeCheckResults filePath |> AsyncResult.ofStringErr
            // and! lineStr = tryGetLineStr pos volatileFile.Source |> Result.ofStringErr
            let declRange =
              try
                fcsRangeToLsp x.DeclarationLocation
              with e ->
                LspRange.Zero

            { To =
                { Name = x.DisplayName
                  Kind = SymbolKind.Function
                  Tags = None
                  Detail = None
                  Uri = file
                  Range = declRange
                  SelectionRange = declRange
                  Data = None }
              FromRanges = [| fcsRangeToLsp range |] }


          let allRemainingExpr =
            membersOrFunctions
            |> Seq.filter (fun (range, x) ->
              let lookingFor =

                x.IsFunction
                || x.IsMethod
                || x.IsPropertyGetterMethod
                || x.IsPropertySetterMethod
              // TODO get bodyRange and check if it is in the range
              range.Start.Line >= pos.Line && lookingFor

            )
            |> Seq.toArray
          // implFile.Declarations.Head

          // let visitor =
          //     { new SyntaxVisitorBase<_>() with
          //         override _.VisitExpr(_, _, defaultTraverse, expr) = defaultTraverse expr
          //     }
          // let foo =SyntaxTraversal.Traverse(pos, tyRes.GetParseResults.ParseTree, visitor)

          let response = allRemainingExpr |> Array.map (createOutGoingCall)

          return Some response
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "CallHierarchyOutgoingCalls Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override x.TextDocumentPrepareCallHierarchy(p: CallHierarchyPrepareParams) =
      asyncResult {
        let tags = [ "CallHierarchyPrepareParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "CallHierarchyPrepareParams Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let (filePath, pos) =
            { new ITextDocumentPositionParams with
                member __.TextDocument = p.TextDocument
                member __.Position = p.Position }
            |> getFilePathAndPosition

          let! volatileFile = forceFindOpenFileOrRead filePath |> AsyncResult.ofStringErr
          let! lineStr = tryGetLineStr pos volatileFile.Source |> Result.ofStringErr
          and! tyRes = forceGetTypeCheckResults filePath |> AsyncResult.ofStringErr

          let! decl = tyRes.TryFindDeclaration pos lineStr |> AsyncResult.ofStringErr

          let! lexedResult =
            Lexer.getSymbol pos.Line pos.Column lineStr SymbolLookupKind.Fuzzy [||]
            |> Result.ofOption (fun () -> "No symbol found")
            |> Result.ofStringErr

          let location = findDeclToLspLocation decl

          let returnValue =
            [| { Name = lexedResult.Text
                 Kind = SymbolKind.Function
                 Tags = None
                 Detail = None
                 Uri = location.Uri
                 Range = location.Range
                 SelectionRange = location.Range
                 Data = None } |]

          return Some returnValue
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "CallHierarchyPrepareParams Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override x.TextDocumentPrepareTypeHierarchy p = x.logUnimplementedRequest p

    override x.TypeHierarchySubtypes p = x.logUnimplementedRequest p

    override x.TypeHierarchySupertypes p = x.logUnimplementedRequest p

    override x.TextDocumentDeclaration p = x.logUnimplementedRequest p

    override x.TextDocumentDiagnostic p = x.logUnimplementedRequest p

    override x.TextDocumentLinkedEditingRange p = x.logUnimplementedRequest p

    override x.TextDocumentMoniker p = x.logUnimplementedRequest p

    override x.WorkspaceDiagnostic p = x.logUnimplementedRequest p

    override x.WorkspaceSymbolResolve p = x.logUnimplementedRequest p

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

        let tags = [ "TextDocumentPositionParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "FSharpSignature Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let (filePath, pos) = getFilePathAndPosition p
          let! volatileFile = forceFindOpenFileOrRead filePath |> AsyncResult.ofStringErr

          let! lineStr = volatileFile.Source |> tryGetLineStr pos |> Result.ofStringErr
          and! tyRes = forceGetTypeCheckResults filePath |> AsyncResult.ofStringErr
          let! tip = Commands.typesig tyRes pos lineStr |> Result.ofCoreResponse

          return
            tip
            |> Option.map (fun tip -> { Content = CommandResponse.typeSig FsAutoComplete.JsonSerializer.writeJson tip })
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FSharpSignature Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override x.FSharpSignatureData(p: TextDocumentPositionParams) =
      asyncResult {
        let tags = [ "TextDocumentPositionParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "FSharpSignatureData Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let pos =
            FSharp.Compiler.Text.Position.mkPos (p.Position.Line) (p.Position.Character + 2)

          let filePath = p.TextDocument.GetFilePath() |> Utils.normalizePath
          let! volatileFile = forceFindOpenFileOrRead filePath |> AsyncResult.ofStringErr
          let! lineStr = volatileFile.Source |> tryGetLineStr pos |> Result.ofStringErr

          and! tyRes = forceGetTypeCheckResults filePath |> AsyncResult.ofStringErr
          let! (typ, parms, generics) = tyRes.TryGetSignatureData pos lineStr |> Result.ofStringErr

          return
            Some
              { Content = CommandResponse.signatureData FsAutoComplete.JsonSerializer.writeJson (typ, parms, generics) }

        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FSharpSignatureData Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }


    override x.FSharpDocumentationGenerator(p: OptionallyVersionedTextDocumentPositionParams) =
      asyncResult {
        let tags = [ "OptionallyVersionedTextDocumentPositionParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "FSharpDocumentationGenerator Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let (filePath, pos) = getFilePathAndPosition p
          let! volatileFile = forceFindOpenFileOrRead filePath |> AsyncResult.ofStringErr

          let! lineStr = volatileFile.Source |> tryGetLineStr pos |> Result.ofStringErr
          and! tyRes = forceGetTypeCheckResults filePath |> AsyncResult.ofStringErr

          match!
            Commands.GenerateXmlDocumentation(tyRes, pos, lineStr)
            |> AsyncResult.ofStringErr
          with
          | None -> return ()
          | Some { InsertPosition = insertPos
                   InsertText = text } ->

            let edit: ApplyWorkspaceEditParams =
              { Label = Some "Generate Xml Documentation"
                Edit =
                  { DocumentChanges =
                      Some
                        [| { TextDocument =
                               { Uri = p.TextDocument.Uri
                                 Version = Some p.TextDocument.Version }
                             Edits =
                               [| { Range = fcsPosToProtocolRange insertPos
                                    NewText = text } |] } |]
                    Changes = None } }

            let! _ = lspClient.WorkspaceApplyEdit edit
            return ()

        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FSharpDocumentationGenerator Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override __.FSharpLineLens(p: ProjectParms) =
      asyncResult {
        let tags = [ "ProjectParms", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "FSharpLineLense Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let fn = p.Project.GetFilePath() |> Utils.normalizePath

          match! getDeclarations fn |> AsyncAVal.forceAsync with
          | None -> return! LspResult.internalError $"No declerations found for {fn}"
          | Some decls ->
            let decls = decls |> Array.map (fun d -> d, fn)

            return Some { Content = CommandResponse.declarations FsAutoComplete.JsonSerializer.writeJson decls }

        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FSharpLineLense Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override __.FSharpWorkspaceLoad(p: WorkspaceLoadParms) =
      asyncResult {
        let tags = [ "WorkspaceLoadParms", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

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
          let! _ = parseAllFiles () |> AsyncAVal.forceAsync

          return { Content = CommandResponse.workspaceLoad FsAutoComplete.JsonSerializer.writeJson true }

        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FSharpWorkspaceLoad Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e

      }


    override __.FSharpWorkspacePeek(p: WorkspacePeekRequest) =
      asyncResult {
        let tags = [ "WorkspacePeekRequest", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

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
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FSharpWorkspacePeek Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override __.FSharpProject(p: ProjectParms) =
      asyncResult {
        let tags = [ "ProjectParms", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

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

          return! Helpers.notImplemented
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FSharpWorkspacePeek Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e

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
        let tags = [ "DotnetNewListRequest", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "FSharpDotnetNewList Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          match! Commands.DotnetNewList() |> AsyncResult.ofCoreResponse with
          | Some funcs ->
            return Some { Content = CommandResponse.dotnetnewlist FsAutoComplete.JsonSerializer.writeJson funcs }
          | None -> return None
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FSharpDotnetNewList Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override __.FSharpDotnetNewRun(p: DotnetNewRunRequest) =
      asyncResult {
        let tags = [ "DotnetNewRunRequest", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "FSharpDotnetNewRun Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          do!
            Commands.DotnetNewRun p.Template p.Name p.Output []
            |> AsyncResult.ofCoreResponse
            |> AsyncResult.ignore<unit option, JsonRpc.Error> // mapping unit option to unit

          return None
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FSharpDotnetNewRun Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override __.FSharpDotnetAddProject(p: DotnetProjectRequest) =
      asyncResult {
        let tags = [ "DotnetProjectRequest", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "FSharpDotnetAddProject Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          do!
            Commands.DotnetAddProject p.Target p.Reference
            |> AsyncResult.ofCoreResponse
            |> AsyncResult.ignore<unit option, JsonRpc.Error> // mapping unit option to unit

          return None
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FSharpDotnetAddProject Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override __.FSharpDotnetRemoveProject(p: DotnetProjectRequest) =
      asyncResult {
        let tags = [ "DotnetProjectRequest", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "FSharpDotnetRemoveProject Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          do!
            Commands.DotnetRemoveProject p.Target p.Reference
            |> AsyncResult.ofCoreResponse
            |> AsyncResult.ignore<unit option, JsonRpc.Error>

          return None
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FSharpDotnetRemoveProject Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override __.FSharpDotnetSlnAdd(p: DotnetProjectRequest) =
      asyncResult {
        let tags = [ "DotnetProjectRequest", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "FSharpDotnetSlnAdd Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          do!
            Commands.DotnetSlnAdd p.Target p.Reference
            |> AsyncResult.ofCoreResponse
            |> AsyncResult.ignore<unit option, JsonRpc.Error>

          return None
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FSharpDotnetSlnAdd Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override x.FSharpHelp(p: TextDocumentPositionParams) =
      asyncResult {
        let tags = [ "TextDocumentPositionParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "FSharpHelp Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let (filePath, pos) = getFilePathAndPosition p
          let! volatileFile = forceFindOpenFileOrRead filePath |> AsyncResult.ofStringErr
          let! lineStr = volatileFile.Source |> tryGetLineStr pos |> Result.ofStringErr
          and! tyRes = forceGetTypeCheckResults filePath |> AsyncResult.ofStringErr

          match! Commands.Help tyRes pos lineStr |> Result.ofCoreResponse with
          | Some t -> return Some { Content = CommandResponse.help FsAutoComplete.JsonSerializer.writeJson t }
          | None -> return None
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FSharpHelp Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override x.FSharpDocumentation(p: TextDocumentPositionParams) =
      asyncResult {
        let tags = [ "TextDocumentPositionParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "FSharpDocumentation Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let (filePath, pos) = getFilePathAndPosition p
          let! volatileFile = forceFindOpenFileOrRead filePath |> AsyncResult.ofStringErr
          let! lineStr = volatileFile.Source |> tryGetLineStr pos |> Result.ofStringErr
          and! tyRes = forceGetTypeCheckResults filePath |> AsyncResult.ofStringErr
          lastFSharpDocumentationTypeCheck <- Some tyRes

          match! Commands.FormattedDocumentation tyRes pos lineStr |> Result.ofCoreResponse with
          | Some(tip, xml, signature, footer, xmlKey) ->
            return
              Some
                { Content =
                    CommandResponse.formattedDocumentation
                      JsonSerializer.writeJson
                      {| Tip = tip
                         XmlSig = xml
                         Signature = signature
                         Footer = footer
                         XmlKey = xmlKey |} }
          | None -> return None
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FSharpDocumentation Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override x.FSharpDocumentationSymbol(p: DocumentationForSymbolReuqest) =
      asyncResult {
        let tags = [ "DocumentationForSymbolReuqest", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "FSharpDocumentationSymbol Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let! tyRes =
            lastFSharpDocumentationTypeCheck
            |> Result.ofOption (fun () -> $"No typecheck results from FSharpDocumentation")
            |> Result.ofStringErr

          match!
            Commands.FormattedDocumentationForSymbol tyRes p.XmlSig p.Assembly
            |> Result.ofCoreResponse
          with
          | None -> return None
          | Some(xml, assembly, xmlDoc, signature, footer, xmlKey) ->

            return
              { Content =
                  CommandResponse.formattedDocumentationForSymbol
                    JsonSerializer.writeJson
                    {| Xml = xml
                       Assembly = assembly
                       XmlDoc = xmlDoc
                       Signature = signature
                       Footer = footer
                       XmlKey = xmlKey |} }
              |> Some
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FSharpDocumentationSymbol Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override __.LoadAnalyzers(path) =
      async {

        use trace = fsacActivitySource.StartActivityForType(thisType)

        logger.info (
          Log.setMessage "LoadAnalyzers Request: {parms}"
          >> Log.addContextDestructured "parms" path
        )

        try
          // since the analyzer state handling code is in `updateConfig`, re-trigger it here
          transact
          <| fun () ->
            config.MarkOutdated()
            updateConfig config.Value

          return LspResult.success ()
        with e ->
          trace |> Tracing.recordException e
          Loggers.analyzers.error (Log.setMessage "Loading failed" >> Log.addExn e)
          return LspResult.success ()
      }

    override x.FSharpPipelineHints(p: FSharpPipelineHintRequest) =
      asyncResult {
        let tags = [ "FSharpPipelineHintRequest", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "FSharpPipelineHints Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let filePath = p.TextDocument.GetFilePath() |> Utils.normalizePath
          let! tyRes = forceGetTypeCheckResults filePath |> AsyncResult.ofStringErr

          match! Commands.pipelineHints forceFindSourceText tyRes |> AsyncResult.ofCoreResponse with
          | None -> return None
          | Some res ->
            return Some { Content = CommandResponse.pipelineHint FsAutoComplete.JsonSerializer.writeJson res }
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FSharpPipelineHints Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override __.FsProjMoveFileUp(p: DotnetFileRequest) =
      asyncResult {
        let tags = [ "DotnetFileRequest", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "FsProjMoveFileUp Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          do!
            Commands.FsProjMoveFileUp p.FsProj p.FileVirtualPath
            |> AsyncResult.ofCoreResponse
            |> AsyncResult.ignore<unit option, JsonRpc.Error>

          return None
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FsProjMoveFileUp Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }


    override __.FsProjMoveFileDown(p: DotnetFileRequest) =
      asyncResult {
        let tags = [ "DotnetFileRequest", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "FsProjMoveFileDown Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          do!
            Commands.FsProjMoveFileDown p.FsProj p.FileVirtualPath
            |> AsyncResult.ofCoreResponse
            |> AsyncResult.ignore<unit option, JsonRpc.Error>

          return None
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FsProjMoveFileDown Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }


    override __.FsProjAddFileAbove(p: DotnetFile2Request) =
      asyncResult {
        let tags = [ "DotnetFile2Request", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "FsProjAddFileAbove Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          do!
            Commands.addFileAbove p.FsProj p.FileVirtualPath p.NewFile
            |> AsyncResult.ofCoreResponse
            |> AsyncResult.ignore<unit option, JsonRpc.Error>

          return None
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FsProjAddFileAbove Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override __.FsProjAddFileBelow(p: DotnetFile2Request) =
      asyncResult {
        let tags = [ "DotnetFile2Request", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "FsProjAddFileBelow Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          do!
            Commands.addFileBelow p.FsProj p.FileVirtualPath p.NewFile
            |> AsyncResult.ofCoreResponse
            |> AsyncResult.ignore<unit option, JsonRpc.Error>

          return None
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FsProjAddFileBelow Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override __.FsProjRenameFile(p: DotnetRenameFileRequest) =
      asyncResult {
        let tags = [ "DotnetRenameFileRequest", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "FsProjRenameFile Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          do!
            Commands.renameFile p.FsProj p.OldFileVirtualPath p.NewFileName
            |> AsyncResult.ofCoreResponse
            |> AsyncResult.ignore<unit option, JsonRpc.Error>

          return None
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FsProjRenameFile Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }


    override __.FsProjAddFile(p: DotnetFileRequest) =
      asyncResult {
        let tags = [ "DotnetFileRequest", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "FsProjAddFile Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          do!
            Commands.addFile p.FsProj p.FileVirtualPath
            |> AsyncResult.ofCoreResponse
            |> AsyncResult.ignore<unit option, JsonRpc.Error>

          return None
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FsProjAddFile Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override _.FsProjRemoveFile(p: DotnetFileRequest) =
      asyncResult {
        let tags = [ "DotnetFileRequest", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "FsProjRemoveFile Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          let fullPath = Path.Combine(Path.GetDirectoryName p.FsProj, p.FileVirtualPath)

          do!
            Commands.removeFile p.FsProj p.FileVirtualPath
            |> AsyncResult.ofCoreResponse
            |> AsyncResult.ignore<unit option, JsonRpc.Error>

          let fileUri = Path.FilePathToUri fullPath
          diagnosticCollections.ClearFor fileUri

          return None
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FsProjRemoveFile Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override _.FsProjAddExistingFile(p: DotnetFileRequest) =
      asyncResult {
        let tags = [ "DotnetFileRequest", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "FsProjAddExistingFile Request: {parms}"
            >> Log.addContextDestructured "parms" p
          )

          do!
            Commands.addExistingFile p.FsProj p.FileVirtualPath
            |> AsyncResult.ofCoreResponse
            |> AsyncResult.ignore<unit option, JsonRpc.Error>

          return None
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FsProjAddExistingFile Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override x.Dispose() =
      traceNotifications |> Option.iter (dispose)
      disposables.Dispose()

    member this.WorkDoneProgessCancel(token: ProgressToken) : Async<unit> =
      async {

        let tags = [ "ProgressToken", box token ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "WorkDoneProgessCancel Request: {parms}"
            >> Log.addContextDestructured "parms" token
          )

        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "WorkDoneProgessCancel Request Errored {p}"
            >> Log.addContextDestructured "token" token
            >> Log.addExn e
          )

        return ()
      }

module AdaptiveFSharpLspServer =

  open System.Threading.Tasks
  open StreamJsonRpc

  let createRpc (handler: IJsonRpcMessageHandler) : JsonRpc =
    let rec (|HandleableException|_|) (e: exn) =
      match e with
      | :? LocalRpcException -> Some()
      | :? TaskCanceledException -> Some()
      | :? OperationCanceledException -> Some()
      | :? System.AggregateException as aex ->
        if aex.InnerExceptions.Count = 1 then
          (|HandleableException|_|) aex.InnerException
        else
          None
      | _ -> None

    let strategy = StreamJsonRpcTracingStrategy(Tracing.fsacActivitySource)

    { new JsonRpc(handler, ActivityTracingStrategy = strategy) with
        member this.IsFatalException(ex: Exception) =
          match ex with
          | HandleableException -> false
          | _ -> true }

  let startCore toolsPath workspaceLoaderFactory sourceTextFactory =
    use input = Console.OpenStandardInput()
    use output = Console.OpenStandardOutput()

    let requestsHandlings =
      (defaultRequestHandlings (): Map<string, ServerRequestHandling<IFSharpLspServer>>)
      |> Map.add "fsharp/signature" (serverRequestHandling (fun s p -> s.FSharpSignature(p)))
      |> Map.add "fsharp/signatureData" (serverRequestHandling (fun s p -> s.FSharpSignatureData(p)))
      |> Map.add "fsharp/documentationGenerator" (serverRequestHandling (fun s p -> s.FSharpDocumentationGenerator(p)))
      |> Map.add "fsharp/lineLens" (serverRequestHandling (fun s p -> s.FSharpLineLens(p)))
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
      |> Map.add "fsproj/renameFile" (serverRequestHandling (fun s p -> s.FsProjRenameFile(p)))
      |> Map.add "fsproj/removeFile" (serverRequestHandling (fun s p -> s.FsProjRemoveFile(p)))

    let adaptiveServer lspClient =
      let loader = workspaceLoaderFactory toolsPath
      new AdaptiveFSharpLspServer(loader, lspClient, sourceTextFactory) :> IFSharpLspServer

    Ionide.LanguageServerProtocol.Server.start requestsHandlings input output FSharpLspClient adaptiveServer createRpc
