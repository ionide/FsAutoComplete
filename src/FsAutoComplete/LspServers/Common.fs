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

[<AutoOpen>]
module AsyncExtensions =
  type Microsoft.FSharp.Control.Async with

    /// Creates an asynchronous workflow that will be resumed when the
    /// specified observables produces a value. The workflow will return
    /// the value produced by the observable.
    static member AwaitObservable(observable: IObservable<'T1>) =

      /// Helper that can be used for writing CPS-style code that resumes
      /// on the same thread where the operation was started.
      let synchronize f =
        let ctx = System.Threading.SynchronizationContext.Current

        f (fun g ->
          let nctx = System.Threading.SynchronizationContext.Current

          if ctx <> null && ctx <> nctx then
            ctx.Post((fun _ -> g ()), null)
          else
            g ())

      let mutable removeObj: IDisposable option = None
      let removeLock = new obj ()

      let setRemover r =
        lock removeLock (fun () -> removeObj <- Some r)

      let remove () =
        lock removeLock (fun () ->
          match removeObj with
          | Some d ->
            removeObj <- None
            d.Dispose()
          | None -> ())

      synchronize (fun f ->
        let workflow =
          Async.FromContinuations(
            (fun (cont, econt, ccont) ->
              let rec finish cont value =
                remove ()
                f (fun () -> cont value)

              setRemover
              <| observable.Subscribe(
                { new IObserver<_> with
                    member x.OnNext(v) = finish cont v
                    member x.OnError(e) = finish econt e

                    member x.OnCompleted() =
                      let msg =
                        "Cancelling the workflow, because the Observable awaited using AwaitObservable has completed."

                      finish ccont (new System.OperationCanceledException(msg)) }
              )

              ())
          )

        async {
          let! cToken = Async.CancellationToken
          let token: CancellationToken = cToken
          use registration = token.Register((fun _ -> remove ()), null)
          return! workflow
        })

module Async =
  let RunSynchronouslyWithCT ct work =
    Async.RunSynchronously(work, cancellationToken = ct)

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
