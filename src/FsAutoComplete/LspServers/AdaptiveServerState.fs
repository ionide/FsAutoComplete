namespace FsAutoComplete.Lsp

open System
open System.IO
open System.Threading
open FsAutoComplete
open FsAutoComplete.CodeFix
open FsAutoComplete.Logging
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
open Ionide.ProjInfo.ProjectSystem
open System.Reactive

open FsAutoComplete.Adaptive
open FsAutoComplete.LspHelpers

open FSharp.Control.Reactive
open FsToolkit.ErrorHandling
open FsAutoComplete.Telemetry
open FsAutoComplete.Utils.Tracing
open FSharp.UMX

open FSharp.Compiler.Text
open CliWrap
open FSharp.Compiler.EditorServices

open FSharp.Data.Adaptive
open Ionide.ProjInfo
open FSharp.Compiler.CodeAnalysis
open FsAutoComplete.UnionPatternMatchCaseGenerator
open System.Collections.Concurrent
open System.Text.RegularExpressions
open IcedTasks
open System.Threading.Tasks
open FsAutoComplete.FCSPatches
open FsAutoComplete.Lsp
open FsAutoComplete.Lsp.Helpers
open FSharp.Compiler.Syntax


[<RequireQualifiedAccess>]
type WorkspaceChosen =
  | Projs of HashSet<string<LocalPath>>
  | NotChosen

[<RequireQualifiedAccess>]
type AdaptiveWorkspaceChosen =
  | Projs of amap<string<LocalPath>, DateTime>
  | NotChosen


[<CustomEquality; NoComparison>]
type LoadedProject =
  { FSharpProjectOptions: FSharpProjectOptions
    LanguageVersion: LanguageVersionShim }

  interface IEquatable<LoadedProject> with
    member x.Equals(other) = x.FSharpProjectOptions = other.FSharpProjectOptions

  override x.GetHashCode() = x.FSharpProjectOptions.GetHashCode()

  override x.Equals(other: obj) =
    match other with
    | :? LoadedProject as other -> (x :> IEquatable<_>).Equals other
    | _ -> false

  member x.SourceFiles = x.FSharpProjectOptions.SourceFiles
  member x.ProjectFileName = x.FSharpProjectOptions.ProjectFileName
  static member op_Implicit(x: LoadedProject) = x.FSharpProjectOptions

/// The reality is a file can be in multiple projects
/// This is extracted to make it easier to do some type of customized select in the future
type IFindProject =
  abstract member FindProject:
    sourceFile: string<LocalPath> * projects: LoadedProject seq -> Result<LoadedProject, string>

type FindFirstProject() =
  interface IFindProject with
    member x.FindProject(sourceFile, projects) =
      projects
      |> Seq.sortBy (fun p -> p.ProjectFileName)
      |> Seq.tryFind (fun p -> p.SourceFiles |> Array.exists (fun f -> f = UMX.untag sourceFile))
      |> Result.ofOption (fun () ->
        $"Couldn't find a corresponding project for {sourceFile}. Have the projects loaded yet or have you tried restoring your project/solution?")


type AdaptiveState(lspClient: FSharpLspClient, sourceTextFactory: ISourceTextFactory, workspaceLoader: IWorkspaceLoader)
  =
  let logger = LogProvider.getLoggerFor<AdaptiveState> ()
  let thisType = typeof<AdaptiveState>
  let disposables = new Disposables.CompositeDisposable()


  let projectSelector = cval<IFindProject> (FindFirstProject())

  let rootPath = cval<string option> None

  let config = cval<FSharpConfig> FSharpConfig.Default

  let checker =
    config
    |> AVal.map (fun c -> c.EnableAnalyzers, c.Fsac.CachedTypeCheckCount, c.Fsac.ParallelReferenceResolution)
    |> AVal.map (FSharpCompilerServiceChecker)

  let configChanges =
    aval {
      let! config = config
      and! checker = checker
      and! rootPath = rootPath

      return config, checker, rootPath
    }


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

  // let analyzersClient =
  //   FSharp.Analyzers.SDK.Client<FSharp.Analyzers.SDK.EditorAnalyzerAttribute, FSharp.Analyzers.SDK.EditorContext>(
  //     Microsoft.Extensions.Logging.Abstractions.NullLogger.Instance
  //   )

  /// <summary>Loads F# Analyzers from the configured directories</summary>
  /// <param name="config">The FSharpConfig</param>
  /// <param name="rootPath">The RootPath</param>
  /// <returns></returns>
  let loadAnalyzers (config: FSharpConfig) (rootPath: string option) =
    if config.EnableAnalyzers then
      Loggers.analyzers.info (Log.setMessageI $"Using analyzer roots of {config.AnalyzersPath:roots}")

      // let excludeInclude =
      //   match config.ExcludeAnalyzers, config.IncludeAnalyzers with
      //   | e, [||] -> FSharp.Analyzers.SDK.ExcludeInclude.ExcludeFilter(fun (s: string) -> Array.contains s e)
      //   | [||], i -> FSharp.Analyzers.SDK.ExcludeInclude.IncludeFilter(fun (s: string) -> Array.contains s i)
      //   | _e, i ->
      //     Loggers.analyzers.warn (
      //       Log.setMessage
      //         "--exclude-analyzers and --include-analyzers are mutually exclusive, ignoring --exclude-analyzers"
      //     )

      //     FSharp.Analyzers.SDK.ExcludeInclude.IncludeFilter(fun (s: string) -> Array.contains s i)

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
      // let assemblyLoadStats = analyzersClient.LoadAnalyzers(dir, excludeInclude)

      // Loggers.analyzers.info (
      //   Log.setMessageI
      //     $"From {analyzerPath:name}: {assemblyLoadStats.AnalyzerAssemblies:dllNo} dlls including {assemblyLoadStats.Analyzers:analyzersNo} analyzers"
      // )
      )

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



  // Syncs config changes to the mutable world
  do
    AVal.Observable.onValueChangedWeak configChanges
    |> Observable.subscribe (fun (config, checker, rootPath) ->
      toggleTraceNotification config.Notifications.Trace config.Notifications.TraceNamespaces

      setFSIArgs checker config.FSICompilerToolLocations config.FSIExtraParameters

      loadAnalyzers config rootPath

      setDotnetRoot checker config.DotNetRoot rootPath)
    |> disposables.Add


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
          notifications.Trigger(NotificationEvent.UnusedOpens(filePath, (unused |> List.toArray), file.Version), ct)
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
          notifications.Trigger(NotificationEvent.UnusedDeclarations(filePath, unused, file.Version), ct)
        with e ->
          logger.error (Log.setMessage "checkUnusedDeclarations failed" >> Log.addExn e)
      }

    let checkSimplifiedNames =
      async {
        try
          use progress = new ServerProgressReport(lspClient)
          do! progress.Begin($"Checking simplifying of names {fileName}...", message = filePathUntag)

          let! simplified = SimplifyNames.getSimplifiableNames (tyRes.GetCheckResults, getSourceLine)
          let simplified = Array.ofSeq simplified
          let! ct = Async.CancellationToken
          notifications.Trigger(NotificationEvent.SimplifyNames(filePath, simplified, file.Version), ct)
        with e ->
          logger.error (Log.setMessage "checkSimplifiedNames failed" >> Log.addExn e)
      }

    let checkUnnecessaryParentheses =
      async {
        try
          use progress = new ServerProgressReport(lspClient)
          do! progress.Begin($"Checking for unnecessary parentheses {fileName}...", message = filePathUntag)

          let unnecessaryParentheses =
            (System.Collections.Generic.HashSet(comparer = Range.comparer), tyRes.GetAST)
            ||> ParsedInput.fold (fun ranges path node ->
              match node with
              | SyntaxNode.SynExpr(SynExpr.Paren(expr = inner; rightParenRange = Some _; range = range)) when
                not (SynExpr.shouldBeParenthesizedInContext getSourceLine path inner)
                ->
                ignore (ranges.Add range)
                ranges

              | SyntaxNode.SynPat(SynPat.Paren(inner, range)) when
                not (SynPat.shouldBeParenthesizedInContext path inner)
                ->
                ignore (ranges.Add range)
                ranges

              | _ -> ranges)

          let! ct = Async.CancellationToken

          notifications.Trigger(
            NotificationEvent.UnnecessaryParentheses(filePath, Array.ofSeq unnecessaryParentheses, file.Version),
            ct
          )
        with e ->
          logger.error (Log.setMessage "checkUnnecessaryParentheses failed" >> Log.addExn e)
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
          checkSimplifiedNames
        if config.UnnecessaryParenthesesAnalyzer then
          checkUnnecessaryParentheses ]

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
          | Some _tast ->
            // Since analyzers are not async, we need to switch to a new thread to not block threadpool
            do! Async.SwitchToNewThread()

            // let! res =
            //   Commands.analyzerHandler (
            //     analyzersClient,
            //     file,
            //     volatileFile.Source,
            //     parseAndCheck.GetParseResults,
            //     tast,
            //     parseAndCheck.GetCheckResults
            //   )

            let! _ct = Async.CancellationToken
            // notifications.Trigger(NotificationEvent.AnalyzerMessage(res, file, volatileFile.Version), ct)

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
              | ProjectResponse.ProjectChanged(_projectFileName) -> failwith "Not Implemented"

            logger.info (Log.setMessage "Workspace Notify {ws}" >> Log.addContextDestructured "ws" ws)
            do! ({ Content = ws }: PlainNotification) |> lspClient.NotifyWorkspace

          | NotificationEvent.ParseError(errors, file, version) ->
            let uri = Path.LocalPathToUri file
            let diags = errors |> Array.map fcsErrorToDiagnostic
            diagnosticCollections.SetFor(uri, "F# Compiler", version, diags)

          | NotificationEvent.UnusedOpens(file, opens, version) ->
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

            diagnosticCollections.SetFor(uri, "F# Unused opens", version, diags)

          | NotificationEvent.UnusedDeclarations(file, decls, version) ->
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

            diagnosticCollections.SetFor(uri, "F# Unused declarations", version, diags)

          | NotificationEvent.SimplifyNames(file, decls, version) ->
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

            diagnosticCollections.SetFor(uri, "F# simplify names", version, diags)

          | NotificationEvent.UnnecessaryParentheses(file, ranges, version) ->
            let uri = Path.LocalPathToUri file

            let diags =
              ranges
              |> Array.map (fun range ->
                { Diagnostic.Range = fcsRangeToLsp range
                  Code = Some "FSAC0004"
                  Severity = Some DiagnosticSeverity.Hint
                  Source = Some "FSAC"
                  Message = "Parentheses can be removed"
                  RelatedInformation = Some [||]
                  Tags = Some [| DiagnosticTag.Unnecessary |]
                  Data = None
                  CodeDescription = None })

            diagnosticCollections.SetFor(uri, "F# unnecessary parentheses", version, diags)

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
          // | NotificationEvent.AnalyzerMessage(messages, file, version) ->
          //   let uri = Path.LocalPathToUri file

          //   match messages with
          //   | [||] -> diagnosticCollections.SetFor(uri, "F# Analyzers", version, [||])
          //   | messages ->
          //     let diags =
          //       messages
          //       |> Array.map (fun m ->
          //         let range = fcsRangeToLsp m.Range

          //         let severity = DiagnosticSeverity.Hint
          //         // match m.Severity with
          //         // | FSharp.Analyzers.SDK.Severity.Hint -> DiagnosticSeverity.Hint
          //         // | FSharp.Analyzers.SDK.Severity.Info -> DiagnosticSeverity.Information
          //         // | FSharp.Analyzers.SDK.Severity.Warning -> DiagnosticSeverity.Warning
          //         // | FSharp.Analyzers.SDK.Severity.Error -> DiagnosticSeverity.Error

          //         let fixes =
          //           match m.Fixes with
          //           | [] -> None
          //           | fixes ->
          //             fixes
          //             |> List.map (fun fix ->
          //               { Range = fcsRangeToLsp fix.FromRange
          //                 NewText = fix.ToText })
          //             |> Ionide.LanguageServerProtocol.Server.serialize
          //             |> Some

          //         { Range = range
          //           Code = Option.ofObj m.Code
          //           Severity = Some severity
          //           Source = Some $"F# Analyzers (%s{m.Type})"
          //           Message = m.Message
          //           RelatedInformation = None
          //           Tags = None
          //           CodeDescription = None
          //           Data = fixes })

          //     diagnosticCollections.SetFor(uri, "F# Analyzers", version, diags)
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
    with :? OperationCanceledException ->
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

  let readFileFromDisk lastTouched (file: string<LocalPath>) =
    async {
      if File.Exists(UMX.untag file) then
        use s = File.openFileStreamForReadingAsync file

        let! source = sourceTextFactory.Create(file, s) |> Async.AwaitCancellableValueTask

        return
          { LastTouched = lastTouched
            Source = source
            Version = 0 }

      else // When a user does "File -> New Text File -> Select a language -> F#" without saving, the file won't exist
        return
          { LastTouched = DateTime.UtcNow
            Source = sourceTextFactory.Create(file, "")
            Version = 0 }
    }

  let getLatestFileChange (filePath: string<LocalPath>) =
    asyncAVal {
      let! (_, lastTouched) = getLastUTCChangeForFile filePath
      return! readFileFromDisk lastTouched filePath
    }

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

  let workspacePaths: ChangeableValue<WorkspaceChosen> =
    cval WorkspaceChosen.NotChosen

  let noopDisposable =
    { new IDisposable with
        member this.Dispose() : unit = () }

  let adaptiveWorkspacePaths =
    workspacePaths
    |> AVal.map (fun wsp ->
      match wsp with
      | WorkspaceChosen.Projs projs ->
        let projChanges =
          projs
          |> ASet.ofSeq
          |> ASet.mapAtoAMap (UMX.untag >> AdaptiveFile.GetLastWriteTimeUtc)

        let cb =
          projChanges.AddWeakCallback(fun _old delta ->
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
                >> Log.addContextDestructured "path" ((|BaseIntermediateOutputPath|_|) p.Properties)
              )

            // Collect other files that should trigger a reload of a project
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
                    |> Array.filter (fun x -> x.EndsWith(".props", StringComparison.Ordinal) && isWithinObjFolder x)
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


  let openFilesTokens =
    ConcurrentDictionary<string<LocalPath>, CancellationTokenSource>()

  let tryGetOpenFileToken filePath =
    match openFilesTokens.TryGetValue(filePath) with
    | (true, v) -> Some v
    | _ -> None

  let getOpenFileTokenOrDefault filePath =
    match tryGetOpenFileToken filePath with
    | Some v -> v.Token
    | None -> CancellationToken.None

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

  let resetCancellationToken (filePath: string<LocalPath>) =

    let adder _ = new CancellationTokenSource()

    let updater _key value =
      cancelToken filePath value
      new CancellationTokenSource()

    openFilesTokens.AddOrUpdate(filePath, adder, updater)
    |> ignore<CancellationTokenSource>


  let updateOpenFiles (file: VolatileFile) =
    let adder _ = cval file

    let updater _ (v: cval<_>) = v.Value <- file

    resetCancellationToken file.FileName
    transact (fun () -> openFiles.AddOrElse(file.Source.FileName, adder, updater))

  let updateTextChanges filePath p =
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

          let lastTouched = File.getLastWriteTimeOrDefaultNow file

          return! readFileFromDisk lastTouched file

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
    let fileShimChanges = openFilesWithChanges |> AMap.mapA (fun _ v -> v)
    // let cachedFileContents = cachedFileContents |> cmap.mapA (fun _ v -> v)

    let filesystemShim file =
      // GetLastWriteTimeShim gets called _a lot_ and when we do checks on save we use Async.Parallel for type checking.
      // Adaptive uses lots of locks under the covers, so many threads can get blocked waiting for data.
      // flattening openFilesWithChanges makes this check a lot quicker as it's not needing to recalculate each value.

      fileShimChanges |> AMap.force |> HashMap.tryFind file

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
            asyncResult {
              let cts = getOpenFileTokenOrDefault filePath
              use linkedCts = CancellationTokenSource.CreateLinkedTokenSource(ctok, cts)

              try
                let! (opts, errors) =
                  checker.GetProjectOptionsFromScript(filePath, file.Source, tfmConfig)
                  |> Async.withCancellation linkedCts.Token

                opts |> scriptFileProjectOptions.Trigger
                let diags = errors |> Array.ofList |> Array.map fcsErrorToDiagnostic

                diagnosticCollections.SetFor(
                  Path.LocalPathToUri filePath,
                  "F# Script Project Options",
                  file.Version,
                  diags
                )

                return
                  { FSharpProjectOptions = opts
                    LanguageVersion = LanguageVersionShim.fromFSharpProjectOptions opts }
                  |> List.singleton
              with e ->
                logger.error (
                  Log.setMessage "Error getting project options for {filePath}"
                  >> Log.addContextDestructured "filePath" filePath
                  >> Log.addExn e
                )

                return! Error $"Error getting project options for {filePath} - {e.Message}"
            }

          return file, projs
        else
          let! projs =
            sourceFileToProjectOptions
            |> AMap.tryFindR
              $"Couldn't find {filePath} in LoadedProjects. Have the projects loaded yet or have you tried restoring your project/solution?"
              filePath


          return file, projs
      })

  let allFSharpFilesAndProjectOptions =
    let wins =
      openFilesToChangesAndProjectOptions
      |> AMap.map (fun _k v -> v |> AsyncAVal.mapSync (fun (file, projects) _ -> file, projects))

    let loses =
      sourceFileToProjectOptions
      |> AMap.map (fun filePath v ->
        asyncAVal {
          let! file = getLatestFileChange filePath
          return (file, Ok v)
        })

    AMap.union loses wins

  let allFilesToFSharpProjectOptions =
    allFSharpFilesAndProjectOptions
    |> AMapAsync.mapAsyncAVal (fun _filePath (_file, options) _ctok -> AsyncAVal.constant options)

  let allFilesParsed =
    allFSharpFilesAndProjectOptions
    |> AMapAsync.mapAsyncAVal (fun _filePath (file, options: Result<LoadedProject list, string>) _ctok ->
      asyncAVal {
        let! (checker: FSharpCompilerServiceChecker) = checker
        and! selectProject = projectSelector

        return!
          asyncResult {
            let! options = options
            let! project = selectProject.FindProject(file.FileName, options)
            let options = project.FSharpProjectOptions
            let parseOpts = Utils.projectOptionsToParseOptions project.FSharpProjectOptions
            return! parseFile checker file parseOpts options
          }

      })

  let getAllFilesToProjectOptions () =
    allFilesToFSharpProjectOptions
    // |> AMap.toASetValues
    |> AMap.force
    |> HashMap.toArray
    |> Array.map (fun (sourceTextPath, projects) ->
      async {
        let! projs = AsyncAVal.forceAsync projects
        return sourceTextPath, projs
      })
    |> Async.parallel75

  let getAllFilesToProjectOptionsSelected () =
    async {
      let! set = getAllFilesToProjectOptions ()
      let selectProject = projectSelector |> AVal.force
      let findProject file projects = selectProject.FindProject(file, projects)

      return
        set
        |> Array.choose (fun (k, v) ->
          v
          |> Result.bind (findProject k)
          |> Result.toOption
          |> Option.map (fun v -> k, v))
    }

  let getAllProjectOptions () =
    async {
      let! set =
        allFilesToFSharpProjectOptions
        |> AMap.toASetValues
        |> ASet.force
        |> HashSet.toArray
        |> Array.map (AsyncAVal.forceAsync)
        |> Async.parallel75

      let set = set |> Array.choose (Result.toOption)

      return set |> Array.collect (List.toArray)
    }


  let getAllFSharpProjectOptions () =
    getAllProjectOptions ()
    |> Async.map (Array.map (fun x -> x.FSharpProjectOptions))

  let getProjectOptionsForFile (filePath: string<LocalPath>) =
    asyncAVal {
      match! allFilesToFSharpProjectOptions |> AMapAsync.tryFindA filePath with
      | Some projs -> return projs
      | None -> return Error $"Couldn't find project for {filePath}. Have you tried restoring your project/solution?"
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
    |> AMap.choose (fun _name (d, pos, _fn, getLine, ast) ->

      Commands.calculateNamespaceInsert (fun () -> Some ast) d pos getLine)

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

        return Error e
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

            notifications.Trigger(NotificationEvent.ParseError(errors, file.Source.FileName, file.Version), ct)
          },
          ct
        )


        return Ok parseAndCheck
    }

  /// Bypass Adaptive checking and tell the checker to check a file
  let bypassAdaptiveTypeCheck (filePath: string<LocalPath>) opts =
    asyncResult {
      try
        logger.info (
          Log.setMessage "Forced Check : {file}"
          >> Log.addContextDestructured "file" filePath
        )

        let checker = checker |> AVal.force

        let! fileInfo = forceFindOpenFileOrRead filePath
        // Don't cache for autocompletions as we really only want to cache "Opened" files.
        return! parseAndCheckFile checker fileInfo opts false

      with e ->

        logger.warn (
          Log.setMessage "Forced Check error : {file}"
          >> Log.addContextDestructured "file" filePath
          >> Log.addExn e
        )

        return! Error(e.ToString())
    }

  let openFilesToRecentCheckedFilesResults =
    openFilesToChangesAndProjectOptions
    |> AMapAsync.mapAsyncAVal (fun _ (info, projectOptions) _ ->
      asyncAVal {
        let file = info.Source.FileName
        let! checker = checker
        and! selectProject = projectSelector

        return
          result {
            let! projectOptions = projectOptions
            let! opts = selectProject.FindProject(file, projectOptions)

            return!
              checker.TryGetRecentCheckResultsForFile(file, opts.FSharpProjectOptions, info.Source)
              |> Result.ofOption (fun () ->
                $"No recent typecheck results for {file}. This may be ok if the file has not been checked yet.")
          }
      })

  let openFilesToCheckedFilesResults =
    openFilesToChangesAndProjectOptions
    |> AMapAsync.mapAsyncAVal (fun _ (info, projectOptions) ctok ->
      asyncAVal {
        let file = info.FileName
        let! checker = checker
        and! selectProject = projectSelector

        return!
          asyncResult {
            let! projectOptions = projectOptions
            let! opts = selectProject.FindProject(file, projectOptions)
            let cts = getOpenFileTokenOrDefault file
            use linkedCts = CancellationTokenSource.CreateLinkedTokenSource(ctok, cts)

            return!
              parseAndCheckFile checker info opts.FSharpProjectOptions true
              |> Async.withCancellation linkedCts.Token
          }

      })

  let getParseResults filePath =
    allFilesParsed
    |> AMapAsync.tryFindAndFlattenR $"No parse results found for {filePath}" filePath

  let getOpenFileTypeCheckResults filePath =
    openFilesToCheckedFilesResults
    |> AMapAsync.tryFindAndFlattenR $"No check results found for {filePath}" (filePath)

  let getOpenFileRecentTypeCheckResults filePath =
    openFilesToRecentCheckedFilesResults
    |> AMapAsync.tryFindAndFlattenR
      $"No recent typecheck results for {filePath}. This may be ok if the file has not been checked yet."
      (filePath)

  let forceGetParseResults filePath = getParseResults filePath |> AsyncAVal.forceAsync


  let forceGetOpenFileRecentTypeCheckResults filePath =
    asyncResult {
      let! results = getOpenFileRecentTypeCheckResults filePath |> AsyncAVal.forceAsync
      return results
    }

  let forceGetOpenFileTypeCheckResults (filePath: string<LocalPath>) =
    getOpenFileTypeCheckResults (filePath) |> AsyncAVal.forceAsync



  let forceGetProjectOptions filePath =
    asyncAVal {
      let! projects = getProjectOptionsForFile filePath
      and! selectProject = projectSelector

      match projects with
      | Ok projects -> return selectProject.FindProject(filePath, projects)
      | Error e -> return Error e
    }
    |> AsyncAVal.forceAsync

  let forceGetFSharpProjectOptions filePath =
    forceGetProjectOptions filePath
    |> Async.map (Result.map (fun p -> p.FSharpProjectOptions))


  let forceGetOpenFileTypeCheckResultsOrCheck file =
    async {
      match! forceGetOpenFileTypeCheckResults file with
      | Ok x -> return Ok x
      | Error _ ->
        match! forceGetFSharpProjectOptions file with
        | Ok opts -> return! bypassAdaptiveTypeCheck file opts
        | Error e -> return Error e
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
  let forceGetOpenFileTypeCheckResultsStale (filePath: string<LocalPath>) =
    asyncAVal {
      let! (checker: FSharpCompilerServiceChecker) = checker

      let inline tryGetLastCheckResultForFile filePath =
        checker.TryGetLastCheckResultForFile(filePath)
        |> Result.ofOption (fun () ->
          $"No cached typecheck results for {filePath}. This may be ok if the file has not been checked yet.")
        |> async.Return

      return
        tryGetLastCheckResultForFile filePath
        |> AsyncResult.orElseWith (fun _ -> forceGetOpenFileRecentTypeCheckResults filePath)
        |> AsyncResult.orElseWith (fun _ -> forceGetOpenFileTypeCheckResults filePath)
        |> Async.map (fun r ->
          Async.Start(
            async {
              // This needs to be in a try catch as it can throw on cancellation which causes the server to crash
              try
                do!
                  forceGetOpenFileTypeCheckResults filePath
                  |> Async.Ignore<Result<ParseAndCheckResults, string>>
              with _e ->
                ()
            }
          )

          r)
    }
    |> AsyncAVal.forceAsync

  let allFilesToDeclarations =
    allFilesParsed
    |> AMap.map (fun _k v -> v |> AsyncAVal.mapResult (fun p _ -> p.GetNavigationItems().Declarations))

  let getAllDeclarations () =
    async {
      let! results =
        allFilesToDeclarations
        |> AMap.force
        |> HashMap.toArray
        |> Array.map (fun (k, v) ->
          async {
            let! decls = AsyncAVal.forceAsync v
            return Result.map (fun v -> k, v) decls
          })
        |> Async.parallel75

      let results = results |> Array.choose (Result.toOption)
      return results

    }

  let getDeclarations filename =
    allFilesToDeclarations
    |> AMapAsync.tryFindAndFlattenR $"Could not find getDeclarations for {filename}" filename

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
              let! tyRes = forceGetOpenFileTypeCheckResults fileName |> Async.map (Option.ofResult)
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
        let selectProject = projectSelector |> AVal.force

        return
          projects
          |> Result.bind (fun p -> selectProject.FindProject(file, p))
          |> Result.toOption
          |> Option.map (fun project -> project.FSharpProjectOptions)

      }

    let projectsThatContainFile file =
      async {
        let! projects = getProjectOptionsForFile file |> AsyncAVal.forceAsync
        let projects = projects |> Result.toOption |> Option.defaultValue []
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
          match! forceGetOpenFileTypeCheckResultsStale file with
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

    let tryGetParseAndCheckResultsForFile filePath pos =
      asyncResult {
        let! (file) = forceFindOpenFileOrRead filePath

        let! lineStr =
          file.Source
          |> tryGetLineStr pos
          |> Result.mapError ErrorMsgUtils.formatLineLookErr
        //TODO ⮝⮝⮝ good candidate for better error model -- review!
        and! tyRes = forceGetOpenFileTypeCheckResultsOrCheck filePath
        return tyRes, lineStr, file.Source
      }

    let getRangeText fileName (range: Ionide.LanguageServerProtocol.Types.Range) =
      asyncResult {
        let! sourceText = forceFindSourceText fileName
        return! sourceText.GetText(protocolRangeToRange (UMX.untag fileName) range)
      }

    let tryFindUnionDefinitionFromPos = tryFindUnionDefinitionFromPos codeGenServer

    let getUnionPatternMatchCases tyRes pos sourceText =
      Commands.getUnionPatternMatchCases tryFindUnionDefinitionFromPos tyRes pos sourceText

    let unionCaseStubReplacements (config) () = Map.ofList [ "$1", config.UnionCaseStubGenerationBody ]


    let implementInterfaceConfig config () : ImplementInterface.Config =
      { ObjectIdentifier = config.InterfaceStubGenerationObjectIdentifier
        MethodBody = config.InterfaceStubGenerationMethodBody
        IndentationSize = config.IndentationSize }

    let recordStubReplacements config () = Map.ofList [ "$1", config.RecordStubGenerationBody ]

    let tryFindRecordDefinitionFromPos =
      RecordStubGenerator.tryFindRecordDefinitionFromPos codeGenServer

    let getRecordStub tyRes pos sourceText =
      Commands.getRecordStub (tryFindRecordDefinitionFromPos) tyRes pos sourceText

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
           (ResolveNamespace.fix tryGetParseAndCheckResultsForFile Commands.getNamespaceSuggestions)
         ReplaceWithSuggestion.fix
         RemoveRedundantQualifier.fix
         Run.ifEnabled
           (fun _ -> config.UnusedDeclarationsAnalyzer)
           (RenameUnusedValue.fix tryGetParseAndCheckResultsForFile)
         AddNewKeywordToDisposableConstructorInvocation.fix
         Run.ifEnabled
           (fun _ -> config.UnionCaseStubGeneration)
           (GenerateUnionCases.fix
             forceFindSourceText
             tryGetParseAndCheckResultsForFile
             getUnionPatternMatchCases
             (unionCaseStubReplacements config))
         ExternalSystemDiagnostics.linter
         ExternalSystemDiagnostics.analyzers
         Run.ifEnabled
           (fun _ -> config.InterfaceStubGeneration)
           (ImplementInterface.fix tryGetParseAndCheckResultsForFile (implementInterfaceConfig config))
         Run.ifEnabled
           (fun _ -> config.RecordStubGeneration)
           (GenerateRecordStub.fix tryGetParseAndCheckResultsForFile getRecordStub (recordStubReplacements config))
         Run.ifEnabled
           (fun _ -> config.AbstractClassStubGeneration)
           (GenerateAbstractClassStub.fix
             tryGetParseAndCheckResultsForFile
             getAbstractClassStub
             (abstractClassStubReplacements config))
         AddMissingEqualsToTypeDefinition.fix forceFindSourceText
         ChangePrefixNegationToInfixSubtraction.fix forceFindSourceText
         ConvertDoubleEqualsToSingleEquals.fix getRangeText
         ChangeEqualsInFieldTypeToColon.fix
         WrapExpressionInParentheses.fix
         ChangeRefCellDerefToNot.fix tryGetParseAndCheckResultsForFile
         ChangeDowncastToUpcast.fix getRangeText
         MakeDeclarationMutable.fix tryGetParseAndCheckResultsForFile forceGetFSharpProjectOptions
         UseMutationWhenValueIsMutable.fix tryGetParseAndCheckResultsForFile
         ConvertInvalidRecordToAnonRecord.fix tryGetParseAndCheckResultsForFile
         RemoveUnnecessaryReturnOrYield.fix tryGetParseAndCheckResultsForFile getLineText
         ConvertCSharpLambdaToFSharpLambda.fix tryGetParseAndCheckResultsForFile getLineText
         AddMissingFunKeyword.fix forceFindSourceText getLineText
         MakeOuterBindingRecursive.fix tryGetParseAndCheckResultsForFile getLineText
         AddMissingRecKeyword.fix forceFindSourceText getLineText
         ConvertBangEqualsToInequality.fix getRangeText
         ChangeDerefBangToValue.fix tryGetParseAndCheckResultsForFile
         RemoveUnusedBinding.fix tryGetParseAndCheckResultsForFile
         AddTypeToIndeterminateValue.fix tryGetParseAndCheckResultsForFile forceGetFSharpProjectOptions
         ChangeTypeOfNameToNameOf.fix tryGetParseAndCheckResultsForFile
         AddMissingInstanceMember.fix
         AddMissingXmlDocumentation.fix tryGetParseAndCheckResultsForFile
         AddExplicitTypeAnnotation.fix tryGetParseAndCheckResultsForFile
         ConvertPositionalDUToNamed.fix tryGetParseAndCheckResultsForFile
         ConvertTripleSlashCommentToXmlTaggedDoc.fix tryGetParseAndCheckResultsForFile
         GenerateXmlDocumentation.fix tryGetParseAndCheckResultsForFile
         RemoveRedundantAttributeSuffix.fix tryGetParseAndCheckResultsForFile
         Run.ifEnabled
           (fun _ -> config.AddPrivateAccessModifier)
           (AddPrivateAccessModifier.fix tryGetParseAndCheckResultsForFile symbolUseWorkspace)
         UseTripleQuotedInterpolation.fix tryGetParseAndCheckResultsForFile
         RenameParamToMatchSignature.fix tryGetParseAndCheckResultsForFile
         RemovePatternArgument.fix tryGetParseAndCheckResultsForFile
         ToInterpolatedString.fix tryGetParseAndCheckResultsForFile getLanguageVersion
         AdjustConstant.fix tryGetParseAndCheckResultsForFile
         UpdateValueInSignatureFile.fix tryGetParseAndCheckResultsForFile
         RemoveUnnecessaryParentheses.fix forceFindSourceText
         AddTypeAliasToSignatureFile.fix forceGetFSharpProjectOptions tryGetParseAndCheckResultsForFile
         UpdateTypeAbbreviationInSignatureFile.fix tryGetParseAndCheckResultsForFile
         AddBindingToSignatureFile.fix forceGetFSharpProjectOptions tryGetParseAndCheckResultsForFile
         ReplaceLambdaWithDotLambda.fix getLanguageVersion tryGetParseAndCheckResultsForFile
         NegateBooleanExpression.fix tryGetParseAndCheckResultsForFile |])

  let forgetDocument (uri: DocumentUri) =
    async {
      let filePath = uri |> Path.FileUriToLocalPath |> Utils.normalizePath

      let doesNotExist (file: string<LocalPath>) = not (File.Exists(UMX.untag file))

      let isOutsideWorkspace (file: string<LocalPath>) =
        asyncAVal {
          let! rootPath = rootPath
          and! selectProject = projectSelector

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

              match
                projectOptions
                |> Result.bind (fun projs -> selectProject.FindProject(file, projs))
              with
              | Error _ -> return true
              | Ok projectOptions ->
                if doesNotExist (UMX.tag projectOptions.ProjectFileName) then
                  return true // script file
                else
                  // issue: fs-file does never get removed from project options (-> requires reload of FSAC to register)
                  // -> don't know if file still part of project (file might have been removed from project)
                  // -> keep cache for file
                  return false
        }

        |> AsyncAVal.forceAsync

      (AVal.force checker).RemoveFileFromCache(filePath)

      transact (fun () ->
        openFiles.Remove filePath |> ignore<bool>

        match openFilesTokens.TryRemove(filePath) with
        | (true, cts) -> cancelToken filePath cts
        | _ -> ()

        textChanges.Remove filePath |> ignore<bool>)

      // Flush changes to the adaptive system so they can remove the file from their cache
      let! _ = forceGetOpenFileTypeCheckResults filePath
      let! _ = forceGetOpenFileTypeCheckResultsStale filePath

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
      let projects = projects |> Result.toOption |> Option.defaultValue []

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


  let bypassAdaptiveAndCheckDependenciesForFile (filePath: string<LocalPath>) =
    async {
      let tags = [ SemanticConventions.fsac_sourceCodePath, box (UMX.untag filePath) ]
      use _ = fsacActivitySource.StartActivityForType(thisType, tags = tags)
      let! dependentFiles = getDependentFilesForFile filePath

      let! projs = getProjectOptionsForFile filePath |> AsyncAVal.forceAsync
      let projs = projs |> Result.toOption |> Option.defaultValue []

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

          let token = getOpenFileTokenOrDefault filePath

          bypassAdaptiveTypeCheck (file) (proj)
          |> Async.withCancellation token
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


  member x.RootPath
    with get () = AVal.force rootPath
    and set v = transact (fun () -> rootPath.Value <- v)

  member x.Config
    with get () = AVal.force config
    and set v = transact (fun () -> config.Value <- v)

  member x.LoadAnalyzers() =
    transact
    <| fun () ->
      config.MarkOutdated()
      x.Config <- config.Value

  member x.ClientCapabilities
    with get () = AVal.force clientCapabilities
    and set v = transact (fun () -> clientCapabilities.Value <- v)

  member x.WorkspacePaths
    with get () = AVal.force workspacePaths
    and set v = transact (fun () -> workspacePaths.Value <- v)

  member x.DiagnosticCollections = diagnosticCollections

  member x.ScriptFileProjectOptions = scriptFileProjectOptions

  member x.OpenDocument(filePath, text: string, version) =
    cancellableTask {
      if isFileOpen filePath |> AVal.force then
        return ()
      else
        // We want to try to use the file system's DateTime if available
        let file = VolatileFile.Create(sourceTextFactory.Create(filePath, text), version)

        updateOpenFiles file

        do!
          forceGetOpenFileTypeCheckResults filePath
          |> Async.Ignore<Result<ParseAndCheckResults, string>>

      return ()
    }

  member x.ChangeDocument(filePath, p: DidChangeTextDocumentParams) =
    cancellableTask {
      updateTextChanges filePath (p, DateTime.UtcNow)

      do!
        forceGetOpenFileTypeCheckResults filePath
        |> Async.Ignore<Result<ParseAndCheckResults, string>>
    }

  member x.SaveDocument(filePath: string<LocalPath>, text: string option) =
    cancellableTask {
      let file =
        option {
          let! oldFile = forceFindOpenFile filePath

          let oldFile =
            text
            |> Option.map (fun t -> sourceTextFactory.Create(oldFile.FileName, t))
            |> Option.map (oldFile.SetSource)
            |> Option.defaultValue oldFile

          return oldFile.UpdateTouched()
        }
        |> Option.defaultWith (fun () ->
          // Very unlikely to get here
          VolatileFile.Create(sourceTextFactory.Create(filePath, text.Value), 0))

      transact (fun () ->
        updateOpenFiles file
        textChanges.Remove filePath |> ignore<bool>)

      let! _ = forceGetOpenFileTypeCheckResults filePath
      do! bypassAdaptiveAndCheckDependenciesForFile filePath
    }



  member x.ForgetDocument(filePath) = forgetDocument filePath

  member x.ParseAllFiles() = parseAllFiles () |> AsyncAVal.forceAsync

  member x.GetOpenFile(filePath) = forceFindOpenFile filePath

  member x.GetOpenFileSource(filePath) = forceFindSourceText filePath

  member x.GetOpenFileOrRead(filePath) = forceFindOpenFileOrRead filePath

  member x.GetParseResults filePath = forceGetParseResults filePath

  member x.GetOpenFileTypeCheckResults(file) = forceGetOpenFileTypeCheckResults file

  member x.GetOpenFileTypeCheckResultsCached(filePath) = forceGetOpenFileTypeCheckResultsStale filePath

  member x.GetProjectOptionsForFile(filePath) = forceGetFSharpProjectOptions filePath

  member x.GetTypeCheckResultsForFile(filePath, opts) = bypassAdaptiveTypeCheck filePath opts

  member x.GetTypeCheckResultsForFile(filePath) =
    asyncResult {
      let! opts = forceGetProjectOptions filePath
      return! x.GetTypeCheckResultsForFile(filePath, opts)
    }

  member x.GetFilesToProject() = getAllFilesToProjectOptionsSelected ()

  member x.GetUsesOfSymbol(filePath, opts, symbol) = (AVal.force checker).GetUsesOfSymbol(filePath, opts, symbol)

  member x.Codefixes = codefixes |> AVal.force

  member x.GlyphToCompletionKind = glyphToCompletionKind |> AVal.force

  member x.UpdateAutocompleteItems(items: list<_ * _>) =
    transact (fun () -> autoCompleteItems.UpdateTo(HashMap.OfList(items)))

  member x.GetAutoCompleteByDeclName declName = getAutoCompleteByDeclName declName |> AVal.force

  member x.GetAutoCompleteNamespacesByDeclName declName = getAutoCompleteNamespacesByDeclName declName |> AVal.force

  member x.SymbolUseWorkspace
    (
      includeDeclarations,
      includeBackticks,
      errorOnFailureToFixRange,
      pos,
      lineStr,
      text,
      tyRes
    ) =
    symbolUseWorkspace includeDeclarations includeBackticks errorOnFailureToFixRange pos lineStr text tyRes

  member x.GetDeclarationLocation(symbolUse, text) = getDeclarationLocation (symbolUse, text)

  member x.GetDeclarations(filename) = getDeclarations filename |> AsyncAVal.forceAsync

  member x.GetAllDeclarations() = getAllDeclarations ()

  member x.GlyphToSymbolKind = glyphToSymbolKind |> AVal.force

  interface IDisposable with
    member this.Dispose() =

      traceNotifications |> Option.iter (dispose)
      disposables.Dispose()
