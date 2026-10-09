namespace FsAutoComplete.Tests.Lsp.Helpers

module DotnetCli =
  open System

  let private executeProcess (wd: string option) (processName: string) (processArgs: string) =
    let psi = new Diagnostics.ProcessStartInfo(processName, processArgs)
    psi.UseShellExecute <- false
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    psi.CreateNoWindow <- true
    wd |> Option.iter (fun wd -> psi.WorkingDirectory <- wd)
    use proc = Diagnostics.Process.Start(psi)
    let output = new Text.StringBuilder()
    let error = new Text.StringBuilder()
    proc.OutputDataReceived.Add(fun args -> output.AppendLine(args.Data) |> ignore)
    proc.ErrorDataReceived.Add(fun args -> error.AppendLine(args.Data) |> ignore)
    proc.BeginErrorReadLine()
    proc.BeginOutputReadLine()

    // A hanging build would otherwise block the test, and the thread it runs on, until the whole run times out.
    if not (proc.WaitForExit(TimeSpan.FromMinutes 5.)) then
      proc.Kill(entireProcessTree = true)
      failwith $"`{processName} {processArgs}` did not finish within 5 minutes and was stopped."

    // Waits until the redirected output is read to the end.
    proc.WaitForExit()

    {| ExitCode = proc.ExitCode
       StdOut = output.ToString()
       StdErr = error.ToString() |}

  let private builtPaths = Collections.Concurrent.ConcurrentQueue<string>()

  /// The paths built since the last call. Their `obj` and `bin` no longer look like a fresh restore.
  let takeBuiltPaths () =
    let paths = ResizeArray()
    let mutable path = null

    while builtPaths.TryDequeue(&path) do
      paths.Add path

    List.ofSeq paths

  /// Builds in the directory of `path`, so `dotnet` resolves the SDK that global.json selects for the tests.
  let build (path: string) =
    let directory =
      if IO.Directory.Exists path then
        path
      else
        IO.Path.GetDirectoryName path

    let result = executeProcess (Some directory) "dotnet" $"build {path}"
    builtPaths.Enqueue(IO.Path.GetFullPath path)
    result
