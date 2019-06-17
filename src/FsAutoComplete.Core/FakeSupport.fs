namespace FsAutoComplete

open Fake.Runtime
open Fake.Runtime.Trace
open System
open System.IO
open System.IO.Compression
open System.Net
open Newtonsoft.Json.Linq

module FakeSupport =
  let private downloadUri = Uri "https://github.com/fsharp/FAKE/releases/latest/download/fake-dotnetcore-portable.zip"

  let downloadAndExtract (uri:Uri) directory = async {
    use client = new WebClient()
    client.Headers.Add("user-agent", "Ionide")
    let tempFile = Path.GetTempFileName()
    try
      do! client.DownloadFileTaskAsync(uri, tempFile) |> Async.AwaitTask
      ZipFile.ExtractToDirectory(tempFile, directory)
    finally
      try
        File.Delete tempFile
      with e -> ()
  }

  let private downloadAndGetFakeDll uri directory = async {
    let fakeDll = Path.Combine(directory, "fake.dll")
    if File.Exists fakeDll then
      return fakeDll
    else    
      do! downloadAndExtract uri directory
      return fakeDll
  }

  let mutable private fakeDll = None
  let private locker = obj()
  let private getFakeRuntimeAsTask() =
    match fakeDll with
    | None ->
      lock locker (fun _ ->
        match fakeDll with
        | None ->
          // use ~/.fsac if possible
          let usersDir = Environment.GetFolderPath Environment.SpecialFolder.UserProfile
          let fsac = System.IO.Path.Combine(usersDir, ".fsac")
          if not (Directory.Exists fsac) then Directory.CreateDirectory fsac |> ignore
          let runtimeDir = Path.Combine(fsac, "fake")
          if not (Directory.Exists runtimeDir) then Directory.CreateDirectory runtimeDir |> ignore
          let t = downloadAndGetFakeDll downloadUri runtimeDir |> Async.StartAsTask
          fakeDll <- Some t
          t
        | Some s -> s
      )
    | Some s -> s
  
  let getFakeRuntime (runtimeDir) = getFakeRuntimeAsTask runtimeDir |> Async.AwaitTask  

  type Declaration =
      { File : string
        Line : int
        Column : int }

  type FakeContext =
      { DotNetRuntime : string }

  /// a target dependency, either a hard or a soft dependency.
  type Dependency =
      { Name : string
        Declaration : Declaration }
  /// a FAKE target, its description and its relations to other targets (dependencies), including the declaration lines of the target and the dependencies.           
  type Target =
      { Name : string
        HardDependencies : Dependency []
        SoftDependencies : Dependency []
        Declaration : Declaration
        Description : string }

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
 
  let private errorTarget file msg desc =
      let decl = 
          { File = file
            Line = 0
            Column = 0 }
      let target =
          { Name = msg
            HardDependencies = [||]
            SoftDependencies = [||]
            Declaration = decl
            Description = desc }
      target

  let private getTargetsLegacy (file:string) (ctx:FakeContext) : Async<Target []> = async {
    // pre Fake.Core.Targets upgrade
    let decl = 
        { File = file
          Line = 0
          Column = 0 }
    let dep =
        { Name = "To see dependencies, upgrade Fake.Core.Target to 5.15"
          Declaration = decl }
    let! rt = getFakeRuntime ()
    let lines = ResizeArray<_>()
    let fileName = Path.GetFileName file
    let workingDir = Path.GetDirectoryName file
    let fakeArgs = sprintf "-s run \"%s\" -- --list" fileName
    let args = sprintf "\"%s\" %s" rt fakeArgs
    let exitCode, _ = Utils.runProcess (lines.Add) workingDir ctx.DotNetRuntime args
    if exitCode <> 0 then
      return [| errorTarget file (sprintf "Running Script 'fake %s' failed (%d)" fakeArgs exitCode) "We tried to list the targets but your script failed" |]
    else
      let targets =
        lines
        |> Seq.filter (isNull >> not)
        |> Seq.skip 1
        |> Seq.map (fun line ->
          let targetName = line.Trim()
          { Name = targetName
            HardDependencies = [| dep |]
            SoftDependencies = [||]
            Declaration = decl
            Description = "To see a description, upgrade Fake.Core.Target to 5.15" })
        |> Seq.toArray
      return targets
  }

  let private getTargetsJson (file:string) (ctx:FakeContext) : Async<Target []> = async {
    // with --write-info support
    let! rt = getFakeRuntime ()
    let lines = ResizeArray<_>()
    let fileName = Path.GetFileName file
    let resultsFile = Path.GetTempFileName()
    try
      let workingDir = Path.GetDirectoryName file
      let fakeArgs = sprintf "-s run --fsiargs \"--debug:portable --optimize-\" \"%s\" -- --write-info \"%s\"" fileName resultsFile
      let args = sprintf "\"%s\" %s" rt fakeArgs
      let exitCode, _ = Utils.runProcess (lines.Add) workingDir ctx.DotNetRuntime args
      if exitCode <> 0 then
        return [| errorTarget file (sprintf "Running Script 'fake %s' failed (%d)" fakeArgs exitCode) "We tried to retrieve the targets but your script failed" |]
      else
        let jsonStr = File.ReadAllText resultsFile
        let jobj = JObject.Parse jsonStr

        let parseDecl (t:JToken) =
            { File = string t.["file"]; Line = int t.["line"]; Column = int t.["column"] }
        let parseDep (t:JToken) =
            { Name = string t.["name"]; Declaration = parseDecl t.["declaration"] }
        let parseArray parseItem (a:JToken) =
            (a :?> JArray)
            |> Seq.map parseItem
            |> Seq.toArray
        let parseTarget (t:JToken) =
            { Name = string t.["name"]
              Declaration = parseDecl t.["declaration"]
              HardDependencies = parseArray parseDep t.["hardDependencies"]
              SoftDependencies = parseArray parseDep t.["softDependencies"]
              Description = string t.["description"] }
        let jTargets = jobj.["targets"] :?> JArray
        let targets =
          jTargets
          |> Seq.map parseTarget
          |> Seq.toArray
        return targets
    finally
      try File.Delete resultsFile with e -> ()     
  }      

  let private getTargetsVersion (context:Runners.FakeContext) : Version option =

    // soon there will be public apis and getProp can be removed...
    let getProp (n:string) (t:obj) : 't =
      let bf = System.Reflection.BindingFlags.Instance ||| System.Reflection.BindingFlags.NonPublic
      t.GetType().GetProperty(n, bf).GetMethod.Invoke(t, [||]) :?> 't
    let runtimeDeps : Runners.AssemblyInfo list = getProp "RuntimeDependencies" context.Config.RuntimeOptions
    let targetVersion =
      runtimeDeps
      |> Seq.map (fun r -> System.Reflection.AssemblyName(r.FullName))
      |> Seq.tryFind (fun a -> a.Name = "Fake.Core.Target")
      |> Option.map (fun a -> a.Version)
    targetVersion

  let getTargets (file:string) (ctx:FakeContext) : Async<Target []> = async {
    setupLogging()
    match detectFakeScript file with
    | None ->
      return [| errorTarget file "not a FAKE 5 script" "This file is not a valid FAKE 5 script" |]

    | Some (config, prepared) ->
      // TODO: Cache targets until file is modified?
      let! rt = getFakeRuntime ()
      let prov = FakeRuntime.restoreAndCreateCachingProvider prepared
      let context, cache = CoreCache.prepareContext config prov
      match getTargetsVersion context with
      | None ->
        return [| errorTarget file "No Fake.Core.Target dependency" "This file a valid FAKE 5 script, but doesn't use Fake.Core.Target" |]
      | Some v when v <= Version(5, 15, 0, 0) ->
        let! targets = getTargetsLegacy file ctx
        return targets
      | Some v ->
        // Use newer logic
        let! targets = getTargetsJson file ctx
        return targets
  }
    