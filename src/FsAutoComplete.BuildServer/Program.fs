module FsAutoComplete.BuildServer.Program

open System
open System.IO
open FsAutoComplete.Logging
open JsonRpcServer

[<EntryPoint>]
let main _args =
  // Set up basic logging
  printfn "FsAutoComplete Build Server starting"

  try
    // Run the JSON RPC server
    let serverTask = runServer ()
    serverTask.Wait()
    0
  with ex ->
    printfn "Build server error: %s" ex.Message
    1
