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
    let proc = Diagnostics.Process.Start(psi)
    let output = new Text.StringBuilder()
    let error = new Text.StringBuilder()
    proc.OutputDataReceived.Add(fun args -> output.Append(args.Data) |> ignore)
    proc.ErrorDataReceived.Add(fun args -> error.Append(args.Data) |> ignore)
    proc.BeginErrorReadLine()
    proc.BeginOutputReadLine()
    proc.WaitForExit()

    {| ExitCode = proc.ExitCode
       StdOut = output.ToString()
       StdErr = error.ToString() |}

  /// Builds in the directory of `path`, so `dotnet` resolves the SDK that global.json selects for the tests.
  let build (path: string) =
    let directory =
      if IO.Directory.Exists path then
        path
      else
        IO.Path.GetDirectoryName path

    executeProcess (Some directory) "dotnet" $"build {path}"
