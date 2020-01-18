// --------------------------------------------------------------------------------------
// (c) Robin Neatherway
// --------------------------------------------------------------------------------------
namespace FsAutoComplete

open System

module Options =
  open Argu

  type CLIArguments =
      | Version
      | [<AltCommandLine("-v")>] Verbose
      | AttachDebugger
      | [<EqualsAssignment; AltCommandLine("-l")>] Logfile of path:string
      | VFilter of filter:string
      | [<CustomCommandLine("--wait-for-debugger")>] WaitForDebugger
      | [<EqualsAssignment; CustomCommandLine("--hostPID")>] HostPID of pid:int
      | [<CustomCommandLine("--background-service-enabled")>] BackgroundServiceEnabled
      with
          interface IArgParserTemplate with
              member s.Usage =
                  match s with
                  | Version -> "display versioning information"
                  | AttachDebugger -> "launch the system debugger and break."
                  | Verbose -> "enable verbose mode"
                  | Logfile _ -> "send verbose output to specified log file"
                  | VFilter _ -> "apply a comma-separated {FILTER} to verbose output"
                  | WaitForDebugger _ -> "wait for a debugger to attach to the process"
                  | HostPID _ -> "the Host process ID."
                  | BackgroundServiceEnabled -> "enable background service"

  let apply (args: ParseResults<CLIArguments>) =

    let applyArg arg =
      match arg with
      | Verbose ->
          // TODO: set level flag
          ()
      | AttachDebugger ->
          System.Diagnostics.Debugger.Launch() |> ignore<bool>
      | Logfile s ->
          try
            // TODO: set logger output to tee to file
            ()
          with
          | e ->
            printfn "Bad log file: %s" e.Message
            exit 1
      | VFilter v ->
          // TODO: set category level
          ()
      | Version
      | WaitForDebugger
      | BackgroundServiceEnabled
      | HostPID _  ->
          ()

    args.GetAllResults()
    |> List.iter applyArg
