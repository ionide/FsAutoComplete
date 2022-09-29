namespace FsAutoComplete.Lsp


open FsAutoComplete.Lsp
open System
open System.IO
open System.Threading
open FsAutoComplete
open FsAutoComplete.Core
open FsAutoComplete.LspHelpers
open FsAutoComplete.CodeFix
open FsAutoComplete.CodeFix.Types
open FsAutoComplete.Logging
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types.LspResult
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Newtonsoft.Json.Linq
open Ionide.ProjInfo.ProjectSystem
open FSharp.Control.Reactive.Observable
open FsToolkit.ErrorHandling
open FSharp.UMX
open FSharp.Analyzers
open FSharp.Compiler.Text
open CliWrap
open CliWrap.Buffered
open FSharp.Compiler.Tokenization
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Symbols
open Fantomas.Client.Contracts
open Fantomas.Client.LSPFantomasService




module Result =
  let ofStringErr r =
    r |> Result.mapError JsonRpc.Error.InternalErrorMessage

  let ofCoreResponse (r: CoreResponse<'a>) =
    match r with
    | CoreResponse.Res a -> Ok a
    | CoreResponse.ErrorRes msg
    | CoreResponse.InfoRes msg -> Error(JsonRpc.Error.InternalErrorMessage msg)

module AsyncResult =
  let ofCoreResponse (ar: Async<CoreResponse<'a>>) = ar |> Async.map Result.ofCoreResponse

  let ofStringErr (ar: Async<Result<'a, string>>) =
    ar |> AsyncResult.mapError JsonRpc.Error.InternalErrorMessage



type DiagnosticMessage =
  | Add of source: string * diags: Diagnostic[]
  | Clear of source: string

/// a type that handles bookkeeping for sending file diagnostics.  It will debounce calls and handle sending diagnostics via the configured function when safe
type DiagnosticCollection(sendDiagnostics: DocumentUri -> Diagnostic[] -> Async<unit>) =
  let send uri (diags: Map<string, Diagnostic[]>) =
    Map.toArray diags |> Array.collect snd |> sendDiagnostics uri

  let agents =
    System.Collections.Concurrent.ConcurrentDictionary<DocumentUri, MailboxProcessor<DiagnosticMessage> * CancellationTokenSource>
      ()

  let rec restartAgent (fileUri: DocumentUri) =
    removeAgent fileUri
    getOrAddAgent fileUri |> ignore

  and removeAgent (fileUri: DocumentUri) =
    match agents.TryRemove(fileUri) with
    | false, _ -> ()
    | true, (_, ctok) -> ctok.Cancel()

  and agentFor (uri: DocumentUri) cTok =
    let logger = LogProvider.getLoggerByName $"Diagnostics/{uri}"

    let mailbox =
      MailboxProcessor.Start(
        (fun inbox ->
          let rec loop (state: Map<string, Diagnostic[]>) =
            async {
              match! inbox.Receive() with
              | Add (source, diags) ->
                let newState = state |> Map.add source diags
                do! send uri newState
                return! loop newState
              | Clear source ->
                let newState = state |> Map.remove source
                do! send uri newState
                return! loop newState
            }

          loop Map.empty),
        cTok
      )

    mailbox.Error.Add(fun exn ->
      logger.error (
        Log.setMessage "Error while sending diagnostics: {message}"
        >> Log.addExn exn
        >> Log.addContext "message" exn.Message
      ))

    mailbox.Error.Add(fun exn -> restartAgent uri)
    mailbox

  and getOrAddAgent fileUri =
    agents.GetOrAdd(
      fileUri,
      fun fileUri ->
        let cts = new CancellationTokenSource()
        let mailbox = agentFor fileUri cts.Token
        (mailbox, cts)
    )
    |> fst

  member x.SetFor(fileUri: DocumentUri, kind: string, values: Diagnostic[]) =
    let mailbox = getOrAddAgent fileUri

    match values with
    | [||] -> mailbox.Post(Clear kind)
    | values -> mailbox.Post(Add(kind, values))

  member x.ClearFor(fileUri: DocumentUri) =
    removeAgent fileUri
    sendDiagnostics fileUri [||] |> Async.Start

  member x.ClearFor(fileUri: DocumentUri, kind: string) =
    let mailbox = getOrAddAgent fileUri
    mailbox.Post(Clear kind)

  interface IDisposable with
    member x.Dispose() =
      for (_, cts) in agents.Values do
        cts.Cancel()

module Async =
  open System.Threading.Tasks

  let withCancellation (ct: CancellationToken) (a: Async<'a>) : Async<'a> =
    async {
      let! ct2 = Async.CancellationToken
      use cts = CancellationTokenSource.CreateLinkedTokenSource(ct, ct2)
      let tcs = new TaskCompletionSource<'a>()
      use _reg = cts.Token.Register(fun () -> tcs.TrySetCanceled() |> ignore)

      let a =
        async {
          try
            let! a = a
            tcs.TrySetResult a |> ignore
          with ex ->
            tcs.TrySetException ex |> ignore
        }

      Async.Start(a, cts.Token)
      return! tcs.Task |> Async.AwaitTask
    }

  let RunSynchronouslyWithCT ct work =
    Async.RunSynchronously(work, cancellationToken = ct)

  let RunSynchronouslyWithCTSafe ct work =
    try
      work |> RunSynchronouslyWithCT ct |> Some
    with
    | :? OperationCanceledException as e -> None
    | :? ObjectDisposedException as e when e.Message.Contains("CancellationTokenSource has been disposed") -> None

[<AutoOpen>]
module ObservableExtensions =
  open System.Reactive.Linq

  type IObservable<'T> with

    /// Fires an event only after the specified interval has passed in which no other pending event has fired. Buffers all events leading up to that emit.
    member x.BufferedDebounce(ts: TimeSpan) =
      x
        .GroupByUntil((fun _ -> true), id, (fun (g: IGroupedObservable<_, _>) -> g.Throttle(ts)))
        .SelectMany(fun x -> x.ToList())

module Helpers =
  let notImplemented<'t> = async.Return LspResult.notImplemented<'t>
  let ignoreNotification = async.Return(())

  let fullPathNormalized = Path.GetFullPath >> Utils.normalizePath >> UMX.untag
