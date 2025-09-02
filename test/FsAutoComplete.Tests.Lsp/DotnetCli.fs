namespace FsAutoComplete.Tests.Lsp.Helpers

module DotnetCli =
  open System

  let private executeProcess (wd: string) (processName: string) (processArgs: string) =
    let psi = new Diagnostics.ProcessStartInfo(processName, processArgs)
    psi.UseShellExecute <- false
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    psi.CreateNoWindow <- true
    psi.WorkingDirectory <- wd
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

  let build path = executeProcess path "dotnet" "build"
