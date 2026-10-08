module MtpShutdownTests

open Expecto
open System
open System.Diagnostics
open System.IO
open System.Threading
open System.Threading.Tasks
open FsAutoComplete.TestingPlatform.Client

let private sampleApp =
  Path.Combine(ResourceLocators.sampleProjectsRootDir, "Mtp.XUnit/bin/Debug/net8.0/Mtp.XUnit.dll")

let private nameOf (node: TestNodeUpdate) = node.DisplayName |> Option.defaultValue node.Uid

/// Launches the sample application and leaves it executing the gated sample test, which blocks
/// while its gate file exists, so the host cannot exit on its own during <c>body</c>. The test only
/// deletes the gate and kills the host in the finally, after <c>body</c> has made its assertions.
let private withBlockedHost (options: MtpClientOptions) (body: MtpClient -> Process -> Task<unit>) =
  task {
    let! client = MtpClient.LaunchAsync(sampleApp, options)
    let host = Process.GetProcessById client.ProcessId

    let gate =
      Path.Combine(Path.GetTempPath(), $"fsac-mtp-cancellation-{client.ProcessId}")

    let mutable run = None

    try
      File.WriteAllText(gate, "wait")
      let discovered = ResizeArray<TestNodeUpdate>()

      use _ =
        client.TestNodesUpdated.Subscribe(fun batch -> discovered.AddRange batch.Updates)

      let! _capabilities = client.InitializeAsync()
      do! client.DiscoverTestsAsync()

      let uid =
        discovered
        |> Seq.find (fun node -> nameOf node = "Tests.Waits for cancellation")
        |> _.Uid

      run <- Some(client.RunTestsAsync [ uid ])

      let started = gate + ".started"
      let elapsed = Stopwatch.StartNew()

      while not (File.Exists started) && elapsed.Elapsed < TimeSpan.FromSeconds 30.0 do
        do! Task.Delay 25

      Expect.isTrue (File.Exists started) "the gated test is executing"
      do! body client host
    finally
      File.Delete gate
      File.Delete(gate + ".started")

      if not host.HasExited then
        host.Kill(true)
        host.WaitForExit(5000) |> ignore

      host.Dispose()
      (client :> IDisposable).Dispose()
      // The run ends when its host is torn down; its outcome is not what these tests observe.
      run
      |> Option.iter (fun r -> r.ContinueWith(fun (t: Task<RunResult>) -> t.Exception) |> ignore)
  }
  |> Async.AwaitTask

/// ExitAsync gives up at once, so the host is still running when it returns.
let private noShutdownWait =
  { MtpClientOptions.Default with
      ServerShutdownTimeout = TimeSpan.Zero }

[<Tests>]
let tests =
  testList
    "MtpClient shutdown"
    [ testCaseAsync "ShutdownAsync terminates a host still running when ExitAsync timed out"
      <| withBlockedHost noShutdownWait (fun client host ->
        task {
          do! client.ExitAsync()
          Expect.isFalse host.HasExited "the host outlives ExitAsync's wait"

          do! client.ShutdownAsync()
          Expect.isTrue (host.WaitForExit 5000) "ShutdownAsync terminates the host"
        })

      testCaseAsync "Dispose terminates a host still running when ExitAsync timed out"
      <| withBlockedHost noShutdownWait (fun client host ->
        task {
          do! client.ExitAsync()
          Expect.isFalse host.HasExited "the host outlives ExitAsync's wait"

          (client :> IDisposable).Dispose()
          Expect.isTrue (host.WaitForExit 5000) "Dispose terminates the host"
        })

      testCaseAsync "ShutdownAsync terminates a host still running when ExitAsync's wait was cancelled"
      <| withBlockedHost
        { MtpClientOptions.Default with
            ServerShutdownTimeout = TimeSpan.FromMinutes 5.0 }
        (fun client host ->
          task {
            // Armed only once ExitAsync has returned. The one cancellable step of sending the exit
            // notification is taking the write lock, which nothing else holds here and is taken before
            // ExitAsync returns, so only the wait for exit is cut short.
            use cancellation = new CancellationTokenSource()
            let elapsed = Stopwatch.StartNew()
            let exiting = client.ExitAsync(cancellation.Token)
            cancellation.CancelAfter(TimeSpan.FromMilliseconds 250.0)
            do! exiting

            Expect.isLessThan
              elapsed.Elapsed
              (TimeSpan.FromMinutes 1.0)
              "cancellation ends the wait before its deadline"

            Expect.isFalse host.HasExited "the host outlives ExitAsync's wait"

            do! client.ShutdownAsync()
            Expect.isTrue (host.WaitForExit 5000) "ShutdownAsync terminates the host"
          }) ]
