namespace FsAutoComplete

open Fake.Runtime
open Fake.Runtime.Trace
open System
open System.IO
open System.IO.Compression
open System.Threading.Tasks
open System.Net
open Newtonsoft.Json.Linq

module FakeSupport =
  let private compatibleFakeVersion = "5.15.0"

  let private fakeDownloadUri version =
    sprintf "https://github.com/fsharp/FAKE/releases/download/%s/fake-dotnetcore-portable.zip" version
    |> Uri

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
      if not (File.Exists fakeDll) then
        failwithf "No 'fake.dll' in '%s' after downloading fake" directory
      return fakeDll
  }

  let mutable private fakeDll = None
  let private locker = obj()
  let private getFakeRuntimeAsTask() =
    let installRuntime () =
      // use ~/.fsac if possible
      let usersDir = Environment.GetFolderPath Environment.SpecialFolder.UserProfile
      let fsac = System.IO.Path.Combine(usersDir, ".fsac")
      if not (Directory.Exists fsac) then Directory.CreateDirectory fsac |> ignore
      let runtimeDir = Path.Combine(fsac, "fake", compatibleFakeVersion)
      if not (Directory.Exists runtimeDir) then Directory.CreateDirectory runtimeDir |> ignore
      let downloadUri = fakeDownloadUri compatibleFakeVersion
      let t = downloadAndGetFakeDll downloadUri runtimeDir |> Async.StartAsTask
      t

    let checkAndReturn (s:Task<string>) =
      if s.IsCompleted && not (File.Exists s.Result) then installRuntime()
      else s
    match fakeDll with
    | None ->
      lock locker (fun _ ->
        match fakeDll with
        | None ->
          let t = installRuntime ()
          fakeDll <- Some t
          t
        | Some s -> checkAndReturn s
      )
    | Some s -> checkAndReturn s
  
  let getFakeRuntime () =
      getFakeRuntimeAsTask () |> Async.AwaitTask  

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
        { File = null
          Line = 0
          Column = 0 }
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
        |> Seq.choose (fun line ->
          // heuristic
          if line.StartsWith "   " then
            let targetNameAndDesc = line.Substring(3)
            // If people use ' - ' in their target name we are lost...
            let splitIdx = targetNameAndDesc.IndexOf(" - ")
            let targetName, description =
              if splitIdx > 0 then targetNameAndDesc.Substring(0, splitIdx), targetNameAndDesc.Substring(splitIdx + 3)
              else targetNameAndDesc, ""
            { Name = targetName
              HardDependencies = [||]
              SoftDependencies = [||]
              Declaration = decl
              Description = description }
            |> Some
          else None)
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

  type GetTargetsWarningOrErrorType =
    | NoFakeScript = 1
    | MissingFakeCoreTargets = 2
    // Most likely due to missing `Target.initEnvironment()`
    | MissingNavigationInfo = 4
    | FakeCoreTargetsOlderThan5_15 = 3

  type GetTargetsResult = { WarningsAndErrors : GetTargetsWarningOrErrorType []; Targets : Target [] }


  let getTargets (file:string) (ctx:FakeContext) : Async<GetTargetsResult> = async {
    setupLogging()
    match detectFakeScript file with
    | None ->
      return { WarningsAndErrors = [|GetTargetsWarningOrErrorType.NoFakeScript|]; Targets = [||] }// Ok [| errorTarget file "not a FAKE 5 script" "This file is not a valid FAKE 5 script" |]

    | Some (config, prepared) ->
      // TODO: Cache targets until file is modified?
      let! rt = getFakeRuntime ()
      let prov = FakeRuntime.restoreAndCreateCachingProvider prepared
      let context, cache = CoreCache.prepareContext config prov
      match getTargetsVersion context with
      | None ->
        return { WarningsAndErrors = [|GetTargetsWarningOrErrorType.MissingFakeCoreTargets|]; Targets = [||] }
      | Some v when v < Version(5, 15) ->
        let! targets = getTargetsLegacy file ctx
        return { WarningsAndErrors = [|GetTargetsWarningOrErrorType.FakeCoreTargetsOlderThan5_15|]; Targets = targets }
      | Some v ->
        // Use newer logic
        let! targets = getTargetsJson file ctx
        let warnings =
          if targets.Length > 0 then
            if isNull targets.[0].Declaration.File then [|GetTargetsWarningOrErrorType.MissingNavigationInfo|]
            else [||]
          else [||]
        return { WarningsAndErrors = warnings; Targets = targets }
  }
    