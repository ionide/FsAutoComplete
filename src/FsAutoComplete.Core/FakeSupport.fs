namespace FsAutoComplete

open Fake.Runtime
open Fake.Runtime.Trace

module FakeSupport =
  type DebugTraceListener() =
    interface ITraceListener with
      /// Writes the given message to the Console.
      member this.Write msg = 
        //let color = colorMap msg
        match msg with
        | ImportantMessage text | ErrorMessage text ->
          //writeText true color true text
          Debug.print "%s" text
        | LogMessage(text, newLine) | TraceMessage(text, newLine) ->
          //writeText false color newLine text
          Debug.print "%s" text

  let mutable private loggingSetup = false
  let private setupLogging () =
    if not loggingSetup then
      listeners.Clear()
      listeners.Add(DebugTraceListener())
      loggingSetup <- true

  let detectFakeScript (file) =
    setupLogging()
    if not (System.IO.File.Exists file) then None
    else
      let config = FakeRuntime.createConfigSimple Verbose [] file [] true false
      let prepared = FakeRuntime.prepareFakeScript config
      let isFakeScript = prepared.DependencyType <> FakeRuntime.PreparedDependencyType.DefaultDependencies
      
      if isFakeScript then Some (config, prepared) else None

  let getProjectOptions (config) (prepared) =
    setupLogging()
    let prov = FakeRuntime.restoreAndCreateCachingProvider prepared
    let context, cache = CoreCache.prepareContext config prov
    let args = context.Config.CompileOptions.AsArgs
    let args =
      args |> Seq.toList
      |> List.filter (fun arg -> arg <> "--")
    
    "--simpleresolution" :: "--targetprofile:netstandard" :: "--nowin32manifest" :: args
    |> List.toArray