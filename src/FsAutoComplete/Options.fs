// --------------------------------------------------------------------------------------
// (c) Robin Neatherway
// --------------------------------------------------------------------------------------
namespace FsAutoComplete

open System
open System.Diagnostics
open System.Reflection

module Version =

  type private T = class end

  open System.Reflection

  let string () =
    let assemblies = typeof<T>.Assembly.GetCustomAttributes(typeof<AssemblyInformationalVersionAttribute>, true)
    match assemblies with
    | [| x |] ->
      let assembly = x :?> AssemblyInformationalVersionAttribute
      assembly.InformationalVersion
    | _ -> ""

module Options =

  let commandText = @"
    Supported commands
    ==================
    quit
      - quit the program
    declarations ""filename""
      - get information about top-level declarations in a file with location
    parse ""<filename>"" [sync]
      - trigger full background parse request; should be
        followed by content of a file (ended with <<EOF>>)
        Optional 'sync' is used to force the parse to occur
        synchronously for testing purposes. Not intended for
        use in production.
    completion ""<filename>"" ""<lineStr>"" <line> <col> [timeout] [filter=(StartsWith|Contains)]
      - trigger completion request for the specified location
        optionally filter in the specified manner
    helptext <candidate>
      - fetch type signature for specified completion candidate
        (from last completion request). Only use in JSON mode.
    symboluse ""<filename>"" ""<lineStr>"" <line> <col> [timeout]
      - find all uses of the symbol for the specified location
    symboluseproject ""<filename>"" ""<lineStr>"" <line> <col> [timeout]
      - find all uses of the symbol for the specified location across whole project
    tooltip ""<filename>"" ""<lineStr>"" <line> <col> [timeout]
      - get tool tip for the specified location
    typesig ""<filename>"" ""<lineStr>"" <line> <col> [timeout]
      - get type signature for the specified location
    finddecl ""<filename>"" ""<lineStr>"" <line> <col> [timeout]
      - find the point of declaration of the symbol at specified location
    methods ""<filename>"" ""<lineStr>"" <line> <col> [timeout]
      - find the method signatures at specified location
    project ""<filename>""
      - associates the current session with the specified project
    compilerlocation
      - prints the best guess for the location of fsc and fsi
        (or fsharpc and fsharpi on unix)
    "

  open Argu

  type TransportMode =
      | Stdio
      | Http
      | Lsp

  type CLIArguments =
      | Version
      | [<AltCommandLine("-v")>] Verbose
      | [<EqualsAssignment; AltCommandLine("-l")>] Logfile of path:string
      | VFilter of filter:string
      | Commands
      | [<CustomCommandLine("--wait-for-debugger")>] WaitForDebugger
      | [<EqualsAssignment; CustomCommandLine("--hostPID")>] HostPID of pid:int
      | Mode of TransportMode
      | Port of tcp_port:int
      with
          interface IArgParserTemplate with
              member s.Usage =
                  match s with
                  | Version -> "display versioning information"
                  | Verbose -> "enable verbose mode"
                  | Logfile _ -> "send verbose output to specified log file"
                  | VFilter _ -> "apply a comma-separated {FILTER} to verbose output"
                  | Commands -> "list the commands that this program understands"
                  | WaitForDebugger _ -> "wait for a debugger to attach to the process"
                  | HostPID _ -> "the Host process ID."
                  | Port _ -> "the listening port."
                  | Mode _ -> "the transport type."

  let apply (args: ParseResults<CLIArguments>) =

    let applyArg arg =
      match arg with
      | Verbose ->
          Debug.verbose <- true
      | Logfile s ->
          try
            Debug.output <- (IO.File.CreateText(s) :> IO.TextWriter)
          with
          | e ->
            printfn "Bad log file: %s" e.Message
            exit 1
      | VFilter v ->
          Debug.categories <- v.Split(',') |> set |> Some
      | Commands
      | Version
      | WaitForDebugger
      | HostPID _
      | Mode _
      | Port _ ->
          ()

    args.GetAllResults()
    |> List.iter applyArg
