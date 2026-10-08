namespace FsAutoComplete.TestServer

open System
open System.Collections.Generic
open System.Runtime.ExceptionServices
open System.Threading
open System.Threading.Tasks
open FsAutoComplete.TestingPlatform.Client

/// Drives Microsoft.Testing.Platform applications over the server-mode protocol. Each application
/// is its own test runner, so there is no separate runner to locate.
module MtpWrapper =
  /// Path to a dll or executable that speaks the server-mode protocol.
  type TestApplication = string

  /// A node of a test tree, paired with the application that reported it.
  type DiscoveredNode = TestApplication * TestNodeUpdate

  type TestDiscoveryUpdate =
    | Progress of DiscoveredNode list
    | LogMessage of ClientLogLevel * string

  let private clientOptions =
    { MtpClientOptions.Default with
        ClientName = "FsAutoComplete" }

  /// Starts a task under the async's cancellation token and reports its outcome only once it has
  /// finished: its result, its exception unwrapped, or request cancellation as async cancellation.
  /// Async.AwaitTask checks for cancellation after the task it is given is already running, so a
  /// racing cancellation is reported while that task carries on, and anything it produces is lost.
  /// Cancellation is therefore reported late on purpose: only once the task has noticed it and
  /// released everything it holds, which for a test host includes tearing the host down.
  /// Internal so the tests can reach it.
  let internal startTaskAsync (start: CancellationToken -> Task<'T>) : Async<'T> =
    async {
      let! cancellationToken = Async.CancellationToken

      return!
        Async.FromContinuations(fun (ok, error, cancelled) ->
          (start cancellationToken)
            .ContinueWith(
              (fun (completed: Task<'T>) ->
                if completed.IsCompletedSuccessfully then
                  ok completed.Result
                else
                  try
                    completed.GetAwaiter().GetResult() |> ignore
                  with
                  | :? OperationCanceledException as failure when cancellationToken.IsCancellationRequested ->
                    cancelled failure
                  | failure -> error failure),
              TaskScheduler.Default
            )
          |> ignore)
    }

  let private rethrow (error: exn) : 'a =
    ExceptionDispatchInfo.Capture(error).Throw()
    Unchecked.defaultof<'a>

  /// Acquires a resource and owns it from acquisition to disposal, all within one task, so no
  /// cancellation can separate the two. The resource is released, then disposed, before the
  /// outcome of <c>work</c> is reported. Release is best-effort, so that outcome stays the one
  /// observed; it runs first so that disposal finds nothing left to wait for.
  /// Internal so the tests can reach it.
  let internal ownedAsync<'R, 'T when 'R :> IDisposable>
    (acquire: CancellationToken -> Task<'R>)
    (release: 'R -> Task)
    (work: CancellationToken -> 'R -> Task<'T>)
    : Async<'T> =
    startTaskAsync (fun cancellationToken ->
      task {
        use! resource = acquire cancellationToken

        let! outcome =
          task {
            try
              let! result = work cancellationToken resource
              return Choice1Of2 result
            with error ->
              return Choice2Of2 error
          }

        try
          do! release resource
        with _ ->
          ()

        match outcome with
        | Choice1Of2 result -> return result
        | Choice2Of2 error -> return rethrow error
      })

  /// Launches <c>application</c> and owns its client until it is shut down. Shutdown goes through
  /// <c>ShutdownAsync</c>, which does not block a thread while the host is torn down, as a
  /// synchronous <c>Dispose</c> would.
  let private withClientAsync (application: TestApplication) (work: CancellationToken -> MtpClient -> Task<'T>) =
    ownedAsync
      (fun cancellationToken -> MtpClient.LaunchAsync(application, clientOptions, cancellationToken))
      (fun client -> client.ShutdownAsync())
      work

  /// Runs <c>work</c> for one application of several, so that one which is not built, cannot be
  /// launched or fails partway is reported at error level instead of failing the others. Its
  /// outcome is <c>Error</c> with the failure; cancellation is not contained.
  let private isolatedAsync
    (logError: string -> unit)
    (application: TestApplication)
    (work: Async<'T list>)
    : Async<Result<'T list, exn>> =
    async {
      if not (IO.File.Exists application) then
        let message =
          $"Skipped test application '{application}': it does not exist. Build its project first."

        logError message
        return Result.Error(IO.FileNotFoundException(message, application) :> exn)
      else
        match! Async.Catch work with
        | Choice1Of2 nodes -> return Ok nodes
        | Choice2Of2 error ->
          logError $"Test application '{application}' failed: {error.Message}"
          return Result.Error error
    }

  /// Collects every node an application reports, notifying as the batches arrive.
  let private discoverFromAsync (notify: TestDiscoveryUpdate -> unit) (application: TestApplication) =
    withClientAsync application (fun cancellationToken client ->
      task {
        let discovered = ResizeArray<DiscoveredNode>()

        use _ =
          client.TestNodesUpdated.Subscribe(fun batch ->
            let nodes = batch.Updates |> List.map (fun node -> application, node)
            discovered.AddRange nodes
            notify (Progress nodes))

        use _ =
          client.LogReceived.Subscribe(fun log -> notify (LogMessage(log.Level, log.Message)))

        let! _capabilities = client.InitializeAsync(cancellationToken)
        do! client.DiscoverTestsAsync(cancellationToken)
        do! client.ExitAsync(cancellationToken)

        return List.ofSeq discovered
      })

  /// A node reported while running, paired with the application that reported it. A test is
  /// reported more than once: once as it starts, and again with its outcome.
  type RunNode = TestApplication * TestNodeUpdate

  type TestRunUpdate =
    | Progress of RunNode list
    | LogMessage of ClientLogLevel * string

  [<RequireQualifiedAccess>]
  type TestSelection =
    | All
    | Uids of string list

  /// Each application runs either all tests or an explicit set of discovered test uids.
  type RunRequest = TestApplication * TestSelection

  type ProcessId = int
  type DidDebuggerAttach = bool

  /// Answers the server's request to have a debugger attached. Unlike VSTest, where the client
  /// launches the test host itself to get ahead of it, the platform runs its own host and asks the
  /// client to attach to it; the run continues once this answers.
  let attachDebuggerHandler (onAttachDebugger: ProcessId -> DidDebuggerAttach) : ServerRequestHandler =
    fun name parameters _cancellationToken ->
      if name <> "client/attachDebugger" then
        Task.FromResult None
      else
        let processId =
          parameters
          |> Option.bind (fun p ->
            match p.TryGetValue "processId" with
            | true, processId -> Some(Convert.ToInt32 processId)
            | _ -> None)

        let attached = processId |> Option.map onAttachDebugger |> Option.defaultValue false

        Dictionary<string, obj>(dict [ "success", box attached ]) :> IReadOnlyDictionary<string, obj>
        |> Some
        |> Task.FromResult

  /// Runs the requested tests of one application, notifying as the outcomes arrive.
  let private runOnAsync
    (notify: TestRunUpdate -> unit)
    (onAttachDebugger: (ProcessId -> DidDebuggerAttach) option)
    ((application, selection): RunRequest)
    =
    withClientAsync application (fun cancellationToken client ->
      task {
        let reported = ResizeArray<RunNode>()

        use _ =
          client.TestNodesUpdated.Subscribe(fun batch ->
            let nodes = batch.Updates |> List.map (fun node -> application, node)
            reported.AddRange nodes
            notify (Progress nodes))

        use _ =
          client.LogReceived.Subscribe(fun log -> notify (LogMessage(log.Level, log.Message)))

        let attachOnce =
          onAttachDebugger
          |> Option.map (fun attach ->
            let mutable attachedProcess = None

            fun processId ->
              if attachedProcess = Some processId then
                true
              else
                let didAttach = attach processId

                if didAttach then
                  attachedProcess <- Some processId

                didAttach)

        attachOnce
        |> Option.iter (fun attach -> client.ServerRequestHandler <- Some(attachDebuggerHandler attach))

        let! _capabilities = client.InitializeAsync(cancellationToken)

        // Protocol 1.0 reserves attachDebugger but its server never sends that request.
        // The launched application is the test host, so attach to its pid before execution.
        attachOnce |> Option.iter (fun attach -> attach client.ProcessId |> ignore)

        let! _result =
          match selection with
          | TestSelection.All -> client.RunTestsAsync(cancellationToken)
          | TestSelection.Uids uids -> client.RunTestsAsync(uids, cancellationToken)

        do! client.ExitAsync(cancellationToken)

        return List.ofSeq reported
      })

  /// Runs the requested tests of every given application. A debugger is attached only where the
  /// application asks for one, which it does when the run was started under a debugger. An
  /// application that cannot run is reported and the others still run; only a run in which no
  /// application could run fails, with the first application's failure.
  let runTestsWithDebuggerAsync
    (notify: TestRunUpdate -> unit)
    (onAttachDebugger: (ProcessId -> DidDebuggerAttach) option)
    (requests: RunRequest list)
    : Async<RunNode list> =
    async {
      let logError message = notify (TestRunUpdate.LogMessage(ClientLogLevel.Error, message))

      // The platform treats an empty uid collection as "run all". An explicit empty
      // selection must therefore never be sent to an application.
      let! perApplication =
        requests
        |> List.filter (fun (_, selection) -> selection <> TestSelection.Uids [])
        |> List.map (fun request -> isolatedAsync logError (fst request) (runOnAsync notify onAttachDebugger request))
        |> Async.Sequential

      let failures =
        perApplication
        |> Array.choose (function
          | Result.Error error -> Some error
          | Ok _ -> None)

      if failures.Length > 0 && failures.Length = perApplication.Length then
        return rethrow failures[0]
      else
        return perApplication |> Seq.collect (Result.defaultValue []) |> List.ofSeq
    }

  /// Runs the requested tests of every given application, undebugged.
  let runTestsAsync (notify: TestRunUpdate -> unit) (requests: RunRequest list) : Async<RunNode list> =
    runTestsWithDebuggerAsync notify None requests

  /// Discovers the tests of every given application. An application that cannot be discovered is
  /// reported and contributes no tests.
  let discoverTestsAsync
    (notify: TestDiscoveryUpdate -> unit)
    (applications: TestApplication list)
    : Async<DiscoveredNode list> =
    async {
      let logError message = notify (TestDiscoveryUpdate.LogMessage(ClientLogLevel.Error, message))

      let! perApplication =
        applications
        |> List.map (fun application -> isolatedAsync logError application (discoverFromAsync notify application))
        |> Async.Sequential

      return perApplication |> Seq.collect (Result.defaultValue []) |> List.ofSeq
    }
