namespace FsAutoComplete.Lsp


open System
open System.IO
open System.Threading
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
  let ofStringErr r = r |> Result.mapError JsonRpc.Error.InternalErrorMessage

  let lineLookupErr
    (r:
      Result<
        'T,
        {| FileName: string<LocalPath>
           Position: FcsPos |}
       >)
    =
    r
    |> Result.mapError (ErrorMsgUtils.formatLineLookErr >> JsonRpc.Error.InternalErrorMessage)

  let ofCoreResponse (r: CoreResponse<'a>) =
    match r with
    | CoreResponse.Res a -> Ok(Some a)
    | CoreResponse.ErrorRes msg -> Error(JsonRpc.Error.InternalErrorMessage msg)
    | CoreResponse.InfoRes _ -> Ok None

module AsyncResult =
  let ofCoreResponse (ar: Async<CoreResponse<'a>>) = ar |> Async.map Result.ofCoreResponse
  let ofStringErr (ar: Async<Result<'a, string>>) = ar |> AsyncResult.mapError JsonRpc.Error.InternalErrorMessage



type DiagnosticMessage =
  | Add of source: string * Version * diags: Diagnostic[]
  | Clear of source: string

/// a type that handles bookkeeping for sending file diagnostics.  It will debounce calls and handle sending diagnostics via the configured function when safe
type DiagnosticCollection(sendDiagnostics: DocumentUri -> Version option * Diagnostic[] -> Async<unit>) =
  let send uri (diags: Map<string, Version * Diagnostic[]>) =
    // Map.toArray diags |> Array.collect (snd >> snd) |> sendDiagnostics uri
    if diags.Values |> Seq.isEmpty then
      sendDiagnostics uri (None, [||])
    else
      diags.Values |> Seq.maxBy(fun (version, _) -> version) |> fun (version, diags) -> sendDiagnostics uri (Some version, diags)

  let agents =
    System.Collections.Concurrent.ConcurrentDictionary<
      DocumentUri,
      MailboxProcessor<DiagnosticMessage> * CancellationTokenSource
     >()

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
          let rec loop (state: Map<string, Version * Diagnostic[]>) =
            async {
              match! inbox.Receive() with
              | Add(source, version, diags) ->
                match Map.tryFind source state with
                | Some(oldVersion, _) when oldVersion > version -> return! loop state
                | _ ->
                  let newState = state |> Map.add source (version, diags)
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

    mailbox.Error.Add(fun _exn -> restartAgent uri)
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

  /// If false, no diagnostics will be collected or sent to the client
  member val ClientSupportsDiagnostics = true with get, set

  member x.SetFor(fileUri: DocumentUri, kind: string, version: Version, values: Diagnostic[]) =
    if x.ClientSupportsDiagnostics then
      let mailbox = getOrAddAgent fileUri

      match values with
      | [||] -> mailbox.Post(Clear kind)
      | values -> mailbox.Post(Add(kind, version, values))

  member x.ClearFor(fileUri: DocumentUri, ?version) =
    if x.ClientSupportsDiagnostics then
      removeAgent fileUri
      sendDiagnostics fileUri (version, [||]) |> Async.Start

  member x.ClearFor(fileUri: DocumentUri, kind: string) =
    if x.ClientSupportsDiagnostics then
      let mailbox = getOrAddAgent fileUri
      mailbox.Post(Clear kind)

  interface IDisposable with
    member x.Dispose() =
      for (_, cts) in agents.Values do
        cts.Cancel()

module CancellableTask =
  open IcedTasks
  open Microsoft.FSharp.Core.CompilerServices

  let inline startAsTask (ct: CancellationToken) ([<InlineIfLambda>] ctask: CancellableTask<'T>) = ctask ct

  let inline fireAndForget (ct: CancellationToken) ([<InlineIfLambda>] ctask: CancellableTask<'T>) =
    startAsTask ct ctask
    |> ignore

  let inline withCancellation (ct: CancellationToken) ([<InlineIfLambda>] ctask: CancellableTask<'T>) =
    cancellableTask {
      let! ct2 = CancellableTask.getCancellationToken ()
      use cts = CancellationTokenSource.CreateLinkedTokenSource(ct, ct2)
      let! result = startAsTask cts.Token ctask
      return result
    }

  let inline withCancellations (ct: CancellationToken array) ([<InlineIfLambda>] ctask: CancellableTask<'T>) =
    cancellableTask {

      let! ct2 = CancellableTask.getCancellationToken ()

      use cts =
        let mutable tokens = ArrayCollector<CancellationToken>()
        tokens.Add ct2
        tokens.AddManyAndClose ct |> CancellationTokenSource.CreateLinkedTokenSource

      let! result = startAsTask cts.Token ctask
      return result
    }

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
      x
        .Publish(fun shared -> shared.Window(shared.Throttle(ts)))
        .SelectMany(fun l -> l.ToList())

module Helpers =
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
              TriggerCharacters = Some([| "."; "'" |])
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
