module Helpers

open System
open Expecto
open System.IO
open FsAutoComplete.Lsp
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.Control.Reactive
open System.Threading
open FSharp.UMX

module Expecto =
  open System.Threading.Tasks

  /// Like Expecto's Test.timeout, but when the time is up it also cancels the test, so the test stops at its next
  /// asynchronous step instead of running on next to the tests after it. Synchronous test code cannot be cancelled
  /// and keeps Expecto's behaviour.
  val cancelOnTimeout: timeout: TimeSpan -> code: TestCode -> TestCode
  val inline testBuilderWithTimeout: ts: TimeSpan -> name: string -> testCase: TestCode -> focus: FocusState -> Test
  val inline testCaseWithTimeout: ts: TimeSpan -> name: string -> test: (unit -> unit) -> Test
  val inline ftestCaseWithTimeout: ts: TimeSpan -> name: string -> test: (unit -> unit) -> Test
  val inline ptestCaseWithTimeout: ts: TimeSpan -> name: string -> test: (unit -> unit) -> Test
  val inline testCaseAsyncWithTimeout: ts: TimeSpan -> name: string -> test: Async<unit> -> Test
  val inline ftestCaseAsyncWithTimeout: ts: TimeSpan -> name: string -> test: Async<unit> -> Test
  val inline ptestCaseAsyncWithTimeout: ts: TimeSpan -> name: string -> test: Async<unit> -> Test
  val inline testCaseTaskWithTimeout: ts: TimeSpan -> name: string -> test: (unit -> Task<unit>) -> Test
  val inline ftestCaseTaskWithTimeout: ts: TimeSpan -> name: string -> test: (unit -> Task<unit>) -> Test
  val inline ptestCaseTaskWithTimeout: ts: TimeSpan -> name: string -> test: (unit -> Task<unit>) -> Test
  val DEFAULT_TIMEOUT: TimeSpan

  /// Contains testCase functions that have a `DEFAULT_TIMEOUT` set to them
  module ShadowedTimeouts =
    val testCase: (string -> (unit -> unit) -> Test)
    val ptestCase: (string -> (unit -> unit) -> Test)
    val ftestCase: (string -> (unit -> unit) -> Test)
    val testCaseAsync: (string -> Async<unit> -> Test)
    val ptestCaseAsync: (string -> Async<unit> -> Test)
    val ftestCaseAsync: (string -> Async<unit> -> Test)

type DisposableDirectory =
  new: directory: string * deleteParentDir: bool -> DisposableDirectory
  static member Create: ?name: string -> DisposableDirectory
  static member From: sourceDir: DirectoryInfo -> DisposableDirectory
  member DirectoryInfo: DirectoryInfo
  interface IDisposable

[<Class>]
type Async =
  /// Behaves like AwaitObservable, but calls the specified guarding function
  /// after a subscriber is registered with the observable.
  static member GuardedAwaitObservable: ev1: IObservable<'T1> -> guardFunction: (unit -> unit) -> Async<'T1>
  /// Creates an asynchronous workflow that will be resumed when the
  /// specified observables produces a value. The workflow will return
  /// the value produced by the observable.
  static member AwaitObservable: ev1: IObservable<'T1> -> Async<'T1>
  /// Creates an asynchronous workflow that runs the asynchronous workflow
  /// given as an argument at most once. When the returned workflow is
  /// started for the second time, it reuses the result of the
  /// previous execution.
  static member Cache: input: Async<'T> -> Async<'T>

val logger: Lazy<Serilog.ILogger>
type Cacher<'t> = System.Reactive.Subjects.ReplaySubject<'t>
type ClientEvents = IObservable<string * obj>

module Range =
  val rangeContainsPos: range: Range -> pos: Position -> bool

val record: cacher: Cacher<'a * 'b> -> ('a -> 'b -> AsyncLspResult<'c>)

val createAdaptiveServer:
  workspaceLoader: (unit -> #Ionide.ProjInfo.IWorkspaceLoader) ->
  sourceTextFactory: ISourceTextFactory ->
  useTransparentCompiler: bool ->
    IFSharpLspServer * ClientEvents

/// Stands in for a server and forwards every call to `Target`. Tests cache their server for the whole run, so a
/// test group that is done can let go of the real server by clearing `Target`.
[<Class>]
type ServerHandle =
  inherit System.Reflection.DispatchProxy
  new: unit -> ServerHandle
  member Target: IFSharpLspServer with get, set
  override Invoke: method: System.Reflection.MethodInfo * args: obj array -> obj

/// Wraps `server` in a `ServerHandle`. The returned function disposes the server and empties the handle.
val handleFor: server: IFSharpLspServer -> IFSharpLspServer * (unit -> unit)

/// The server behind a `ServerHandle`, for tests that need the concrete server type.
val realServer: server: IFSharpLspServer -> IFSharpLspServer

val defaultConfigDto: FSharpConfigDto
val clientCaps: ClientCapabilities

open Expecto.Logging
open Expecto.Logging.Message
open System.Threading
open FsAutoComplete.CommandResponse
open CliWrap
open CliWrap.Buffered

val logEvent: name: 'a * payload: 'b -> unit
val logDotnetRestore: section: 'a -> line: string -> unit
val dotnetCleanup: baseDir: string -> unit
val runProcess: workingDir: string -> exePath: string -> args: string -> Async<BufferedCommandResult>
val inline expectExitCodeZero: r: BufferedCommandResult -> unit
val dotnetRestore: dir: string -> Async<unit>
val dotnetToolRestore: dir: string -> Async<unit>

/// Deletes `obj` and `bin` of `path` and restores every F# project below it, once per test run.
val prepareTestProjects: path: string -> Async<unit>

val serverInitialize:
  path: string ->
  config: FSharpConfigDto ->
  createServer: (unit -> IFSharpLspServer * 'a) ->
    Async<IFSharpLspServer * 'a>
    when 'a :> IObservable<'b * 'c>

val loadDocument: path: string -> TextDocumentItem
val parseProject: projectFilePath: string -> server: IFSharpLspServer -> Async<unit>
val (|UnwrappedPlainNotification|_|): eventType: string -> notification: PlainNotification -> 't option
val internal defaultTimeout: TimeSpan
val waitForWorkspaceFinishedParsing: events: ClientEvents -> Async<unit>

val workspaceEdits: (IObservable<string * obj> -> IObservable<ApplyWorkspaceEditParams>)

val editsFor:
  file: string -> (IObservable<ApplyWorkspaceEditParams> -> IObservable<U2<TextEdit, AnnotatedTextEdit> array>)

val fileDiagnostics: file: string -> (IObservable<string * obj> -> IObservable<Diagnostic array>)
val fileDiagnosticsForUri: uri: string -> (IObservable<string * obj> -> IObservable<Diagnostic array>)
val diagnosticsFromSource: desiredSource: String -> (IObservable<Diagnostic array> -> IObservable<Diagnostic array>)
val analyzerDiagnostics: file: string -> (IObservable<string * obj> -> IObservable<Diagnostic array>)
val linterDiagnostics: file: string -> (IObservable<string * obj> -> IObservable<Diagnostic array>)
val fsacDiagnostics: file: string -> (IObservable<string * obj> -> IObservable<Diagnostic array>)
val compilerDiagnostics: file: string -> (IObservable<string * obj> -> IObservable<Diagnostic array>)
val diagnosticsToResult: (IObservable<Diagnostic array> -> IObservable<Result<unit, Diagnostic array>>)
val waitForParseResultsForFile: file: string -> (IObservable<string * obj> -> Async<Result<unit, Diagnostic array>>)
val waitForDiagnosticErrorForFile: file: string -> (IObservable<string * obj> -> Async<Result<unit, Diagnostic array>>)
val waitForFsacDiagnosticsForFile: file: string -> (IObservable<string * obj> -> Async<Result<unit, Diagnostic array>>)

val waitForCompilerDiagnosticsForFile:
  file: string -> (IObservable<string * obj> -> Async<Result<unit, Diagnostic array>>)

val waitForParsedScript: event: ClientEvents -> Async<PublishDiagnosticsParams>
val waitForTestDetected: fileName: string -> events: ClientEvents -> Async<TestDetectedNotification>
val waitForEditsForFile: file: string -> (IObservable<string * obj> -> Async<U2<TextEdit, AnnotatedTextEdit> array>)
val trySerialize: t: string -> 't option
val (|As|_|): m: PlainNotification -> 't option
val (|CodeActions|_|): t: TextDocumentCodeActionResult -> CodeAction array option
