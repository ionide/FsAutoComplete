namespace FsAutoComplete.Lsp


open System
open System.IO
open System.Threading
open System.Threading.Tasks
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FsAutoComplete.Logging
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
open FsToolkit.ErrorHandling
open FSharp.UMX
open CliWrap


module ErrorMsgUtils =
  let formatLineLookErr
    (x:
      {| FileName: string<LocalPath>
         Position: FcsPos |})
    =
    let position = fcsPosToLsp x.Position
    $"No line in {x.FileName} at position {position}"


module Result =
  let ofStringErr r = r |> Result.mapError (fun s -> JsonRpc.Error.InternalError s)

  let lineLookupErr
    (r:
      Result<
        'T,
        {| FileName: string<LocalPath>
           Position: FcsPos |}
       >)
    =
    r
    |> Result.mapError (fun s -> JsonRpc.Error.InternalError(s |> ErrorMsgUtils.formatLineLookErr))

  let ofCoreResponse (r: CoreResponse<'a>) =
    match r with
    | CoreResponse.Res a -> Ok(Some a)
    | CoreResponse.ErrorRes msg -> Error(JsonRpc.Error.InternalError msg)
    | CoreResponse.InfoRes _ -> Ok None

module AsyncResult =
  let ofCoreResponse (ar: Async<CoreResponse<'a>>) = ar |> Async.map Result.ofCoreResponse
  let ofStringErr (ar: Async<Result<'a, string>>) = ar |> AsyncResult.mapError (fun s -> JsonRpc.Error.InternalError s)


module AcknowledgedNotification =
  let private canceled (ct: CancellationToken) =
    Async.FromContinuations(fun (_, _, cancellation) -> cancellation (OperationCanceledException ct))

  let triggerAndWait
    (notifications: Event<'notification * CancellationToken * TaskCompletionSource<unit> option>)
    (notification: 'notification)
    (ct: CancellationToken)
    =
    async {
      let completion =
        TaskCompletionSource<unit>(TaskCreationOptions.RunContinuationsAsynchronously)

      let gate = obj ()
      let mutable cancellationRequested = false

      use _registration =
        ct.Register(fun () ->
          lock gate (fun () ->
            cancellationRequested <- true
            completion.TrySetCanceled(ct) |> ignore))

      let triggered =
        lock gate (fun () ->
          if cancellationRequested then
            false
          else
            notifications.Trigger(notification, ct, Some completion)
            true)

      if not triggered then
        return! canceled ct
      else
        try
          do! completion.Task |> Async.AwaitTask
        with :? OperationCanceledException ->
          return! canceled ct
    }



type DiagnosticMessage =
  | Add of source: string * Version * diags: Diagnostic[] * completion: TaskCompletionSource<unit> option
  | Clear of source: string * completion: TaskCompletionSource<unit> option

/// a type that handles bookkeeping for sending file diagnostics.  It will debounce calls and handle sending diagnostics via the configured function when safe
type DiagnosticCollection(sendDiagnostics: DocumentUri -> int option -> Diagnostic[] -> Async<unit>) =
  let send uri (diags: Map<string, Version * Diagnostic[]>) =
    let allDiags = Map.toArray diags |> Array.collect (snd >> snd)

    let maxVersion =
      if Map.isEmpty diags then
        None
      else
        Map.toArray diags |> Array.map (snd >> fst) |> Array.max |> Some

    sendDiagnostics uri maxVersion allDiags

  let agents =
    System.Collections.Concurrent.ConcurrentDictionary<
      DocumentUri,
      MailboxProcessor<DiagnosticMessage> * CancellationTokenSource
     >()

  let rec agentFor (uri: DocumentUri) (cts: CancellationTokenSource) =
    let logger = LogProvider.getLoggerByName $"Diagnostics/{uri}"

    let mailbox =
      MailboxProcessor.Start(
        (fun inbox ->
          let rec loop (state: Map<string, Version * Diagnostic[]>) =
            async {
              let! message = inbox.Receive()

              let completion =
                match message with
                | Add(_, _, _, completion)
                | Clear(_, completion) -> completion

              try
                let! newState =
                  async {
                    match message with
                    | Add(source, version, diags, _) ->
                      match Map.tryFind source state with
                      | Some(oldVersion, _) when oldVersion > version -> return state
                      | _ ->
                        let newState = state |> Map.add source (version, diags)
                        do! send uri newState
                        return newState
                    | Clear(source, _) ->
                      let newState = state |> Map.remove source
                      do! send uri newState
                      return newState
                  }

                completion |> Option.iter (fun value -> value.TrySetResult() |> ignore)
                return! loop newState
              with ex ->
                replaceAgent uri (inbox, cts)
                completion |> Option.iter (fun value -> value.TrySetException ex |> ignore)

                logger.error (
                  Log.setMessage "Error while sending diagnostics: {message}"
                  >> Log.addExn ex
                  >> Log.addContext "message" ex.Message
                )

                cts.Cancel()
            }

          loop Map.empty),
        cts.Token
      )

    mailbox

  and replaceAgent fileUri failedAgent =
    let replacementCts = new CancellationTokenSource()
    let replacementAgent = agentFor fileUri replacementCts, replacementCts

    if not (agents.TryUpdate(fileUri, replacementAgent, failedAgent)) then
      replacementCts.Cancel()

  let getOrAddAgent fileUri =
    agents.GetOrAdd(
      fileUri,
      fun fileUri ->
        let cts = new CancellationTokenSource()
        let mailbox = agentFor fileUri cts
        (mailbox, cts)
    )

  let removeAgent (fileUri: DocumentUri) =
    match agents.TryRemove(fileUri) with
    | false, _ -> ()
    | true, (_, ctok) -> ctok.Cancel()

  /// If false, no diagnostics will be collected or sent to the client
  member val ClientSupportsDiagnostics = true with get, set

  member x.SetFor(fileUri: DocumentUri, kind: string, version: Version, values: Diagnostic[]) =
    if x.ClientSupportsDiagnostics then
      let mailbox, _ = getOrAddAgent fileUri

      match values with
      | [||] -> mailbox.Post(Clear(kind, None))
      | values -> mailbox.Post(Add(kind, version, values, None))

  member x.SetForAndWait(fileUri: DocumentUri, kind: string, version: Version, values: Diagnostic[]) =
    async {
      if x.ClientSupportsDiagnostics then
        let completion =
          TaskCompletionSource<unit>(TaskCreationOptions.RunContinuationsAsynchronously)

        let mailbox, cts = getOrAddAgent fileUri

        use _registration =
          cts.Token.Register(fun () -> completion.TrySetCanceled(cts.Token) |> ignore)

        match values with
        | [||] -> mailbox.Post(Clear(kind, Some completion))
        | values -> mailbox.Post(Add(kind, version, values, Some completion))

        try
          do! completion.Task |> Async.AwaitTask
        with :? AggregateException as aggregate when aggregate.InnerExceptions.Count = 1 ->
          return raise aggregate.InnerException
    }

  member x.ClearFor(fileUri: DocumentUri) =
    if x.ClientSupportsDiagnostics then
      removeAgent fileUri
      sendDiagnostics fileUri None [||] |> Async.Start

  member x.ClearFor(fileUri: DocumentUri, kind: string) =
    if x.ClientSupportsDiagnostics then
      let mailbox, _ = getOrAddAgent fileUri
      mailbox.Post(Clear(kind, None))

  interface IDisposable with
    member x.Dispose() =
      for (_, cts) in agents.Values do
        cts.Cancel()

module Async =
  open System.Threading.Tasks
  open IcedTasks

  let rec logger = LogProvider.getLoggerByQuotation <@ logger @>

  let inline logCancelled e = logger.trace (Log.setMessage "Operation Cancelled" >> Log.addExn e)

  let withCancellation (ct: CancellationToken) (a: Async<'a>) : Async<'a> =
    asyncEx {
      let! ct2 = Async.CancellationToken
      use cts = CancellationTokenSource.CreateLinkedTokenSource(ct, ct2)

      let tcs =
        new TaskCompletionSource<'a>(TaskCreationOptions.RunContinuationsAsynchronously)

      use _reg = cts.Token.Register(fun () -> tcs.TrySetCanceled(cts.Token) |> ignore)

      let a =
        async {
          try
            let! a = a
            tcs.TrySetResult a |> ignore
          with ex ->
            tcs.TrySetException ex |> ignore
        }

      Async.Start(a, cts.Token)
      return! tcs.Task
    }

  let withCancellationSafe ct work =
    async {
      try
        let! result = withCancellation (ct ()) work
        return Some result
      with
      | :? OperationCanceledException as e ->
        logCancelled e
        return None
      | :? ObjectDisposedException as e when e.Message.Contains("CancellationTokenSource has been disposed") ->
        logCancelled e
        return None
    }

  let StartWithCT ct work = Async.Start(work, ct)

  let startImmediateAsTask ct work = Async.StartImmediateAsTask(work, ct)

[<AutoOpen>]
module ObservableExtensions =
  open System.Reactive.Linq

  type IObservable<'T> with

    /// Fires an event only after the specified interval has passed in which no other pending event has fired. Buffers all events leading up to that emit.
    member x.BufferedDebounce(ts: TimeSpan) =
      x.Publish(fun shared -> shared.Window(shared.Throttle(ts))).SelectMany(fun l -> l.ToList())

module Helpers =
  open Ionide.LanguageServerProtocol.JsonRpc
  let notImplemented<'t> = async.Return LspResult.notImplemented<'t>
  let ignoreNotification = async.Return(())

  let tryGetLineStr pos (text: IFSACSourceText) =
    text.GetLine(pos)
    |> Result.ofOption (fun () ->
      {| FileName = text.FileName
         Position = pos |})


  let fullPathNormalized = Path.GetFullPath >> Utils.normalizePath >> UMX.untag

  let defaultServerCapabilities =
    { ServerCapabilities.Default with
        HoverProvider = Some(U2.C1 true)
        RenameProvider =
          Some(
            U2.C2
              { PrepareProvider = Some true
                WorkDoneProgress = Some false }
          )
        DefinitionProvider = Some(U2.C1 true)
        TypeDefinitionProvider = Some(U3.C1 true)
        ImplementationProvider = Some(U3.C1 true)
        ReferencesProvider = Some(U2.C1 true)
        DocumentHighlightProvider = Some(U2.C1 true)
        DocumentSymbolProvider =
          Some(
            U2.C2
              { Label = Some "F#"
                WorkDoneProgress = Some false }
          )
        WorkspaceSymbolProvider =
          Some(
            U2.C2
              { ResolveProvider = Some true
                WorkDoneProgress = Some false }
          )
        DocumentFormattingProvider = Some(U2.C1 true)
        DocumentRangeFormattingProvider = Some(U2.C1 true)
        SignatureHelpProvider =
          Some
            { TriggerCharacters = Some [| "("; ","; " " |]
              RetriggerCharacters = Some [| ","; ")"; " " |]
              WorkDoneProgress = Some false }
        CompletionProvider =
          Some
            { ResolveProvider = Some true
              TriggerCharacters = Some([| "."; "'"; "{" |])
              AllCommitCharacters = None //TODO: what chars should commit completions?
              CompletionItem = None
              WorkDoneProgress = Some false }
        CodeLensProvider =
          Some
            { CodeLensOptions.ResolveProvider = Some true
              WorkDoneProgress = Some false }
        CodeActionProvider =
          Some(
            U2.C2
              { CodeActionKinds = None
                ResolveProvider = None
                WorkDoneProgress = Some false }
          )
        TextDocumentSync =
          Some
          <| U2.C1
            { TextDocumentSyncOptions.Default with
                OpenClose = Some true
                Change = Some TextDocumentSyncKind.Incremental
                Save = Some <| U2.C2 { IncludeText = Some true } }
        FoldingRangeProvider = Some(U3.C1 true)
        SelectionRangeProvider = Some(U3.C1 true)
        CallHierarchyProvider = Some(U3.C1 true)
        SemanticTokensProvider =
          Some
          <| U2.C1
            { Legend =
                createTokenLegend<ClassificationUtils.SemanticTokenTypes, ClassificationUtils.SemanticTokenModifier>
              Range = Some <| U2.C1 true
              Full = Some(U2.C1 true)
              WorkDoneProgress = Some false }
        InlayHintProvider =
          Some
          <| U3.C2
            { ResolveProvider = Some false
              WorkDoneProgress = Some false } }
