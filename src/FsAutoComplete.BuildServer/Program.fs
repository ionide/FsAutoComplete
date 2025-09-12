module FsAutoComplete.BuildServer.Program

open System
open System.IO
open FsAutoComplete.Logging

[<EntryPoint>]
let main _args =
  // Set up basic logging
  printfn "FsAutoComplete Build Server starting"

  try
    // For now, just echo stdin to stdout to test communication
    use reader = new StreamReader(Console.OpenStandardInput())
    use writer = new StreamWriter(Console.OpenStandardOutput())
    
    let mutable keepRunning = true
    
    while keepRunning do
      let line = reader.ReadLine()
      if isNull line then
        keepRunning <- false
      else
        writer.WriteLine($"Echo: {line}")
        writer.Flush()
    
    printfn "Build server stopped"
    0
  with
  | ex ->
    printfn $"Build server failed: {ex.Message}"
    1