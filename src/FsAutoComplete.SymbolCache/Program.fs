module FsAutoComplete.Program

open System

let zombieCheckWithHostPID quit pid =
    try
        let hostProcess = System.Diagnostics.Process.GetProcessById(pid)
        ProcessWatcher.watch hostProcess (fun _ -> quit ())
    with
    | e ->
        printfn "Host process ID %i not found: %s" pid e.Message
        // If the process dies before we get here then request shutdown
        // immediately
        quit ()

[<EntryPoint>]
let entry args =
    try
        let port = Int32.Parse args.[0]
        let pid = Int32.Parse args.[1]
        let dir = args.[2]
        zombieCheckWithHostPID (fun () -> exit 0) pid
        FsAutoComplete.SymbolCacheApi.start port dir
    with
    | e ->
      printfn "Server crashing error - %s \n %s" e.Message e.StackTrace
      3
