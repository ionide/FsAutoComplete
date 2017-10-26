namespace FsAutoComplete

module DotnetProjInfoInspectHelpers =

  let (|MsbuildOk|_|) x =
    match x with
#if NETSTANDARD2_0
    | Ok x -> Some x
    | Error _ -> None
#else
    | Choice1Of2 x -> Some x
    | Choice2Of2 _ -> None
#endif

  let (|MsbuildError|_|) x =
    match x with
#if NETSTANDARD2_0
    | Ok _ -> None
    | Error x -> Some x
#else
    | Choice1Of2 _ -> None
    | Choice2Of2 x -> Some x
#endif

  let msbuildPropBool (s: string) =
    match s.Trim() with
    | "" -> None
    | Dotnet.ProjInfo.Inspect.MSBuild.ConditionEquals "True" -> Some true
    | _ -> Some false

  let msbuildPropStringList (s: string) =
    match s.Trim() with
    | "" -> []
    | Dotnet.ProjInfo.Inspect.MSBuild.StringList list  -> list
    | _ -> []

module NETFrameworkInfoProvider =

  open System
  open System.IO
  open DotnetProjInfoInspectHelpers

  let private getInfoFromMsbuild getArgs additionalProps =
    let file = Dotnet.ProjInfo.NETFrameworkInfoFromMSBuild.createEnvInfoProj ()

    let result, log =
        let loggedMessages = System.Collections.Concurrent.ConcurrentQueue<string>()

        let msbuildExec =
            let runCmd exePath args =
              Utils.runProcess loggedMessages.Enqueue (Path.GetDirectoryName file) exePath (args |> String.concat " ")

            let msbuildPath =
                Dotnet.ProjInfo.Inspect.MSBuildExePath.Path "msbuild"

            Dotnet.ProjInfo.Inspect.msbuild msbuildPath runCmd

        let infoResult =
            let additionalArgs = additionalProps |> List.map Dotnet.ProjInfo.Inspect.MSBuild.Property

            file
            |> Dotnet.ProjInfo.Inspect.getProjectInfoOldSdk loggedMessages.Enqueue msbuildExec getArgs additionalArgs

        infoResult, (loggedMessages.ToArray() |> Array.toList)

    match result with
    | MsbuildOk r ->
        r, log
    | MsbuildError x ->
        match x with
        | Dotnet.ProjInfo.Inspect.GetProjectInfoErrors.MSBuildSkippedTarget ->
            failwithf "Unexpected MSBuild result, all targets skipped"
        | Dotnet.ProjInfo.Inspect.GetProjectInfoErrors.UnexpectedMSBuildResult(r) ->
            failwithf "Unexpected MSBuild result %s" r
        | Dotnet.ProjInfo.Inspect.GetProjectInfoErrors.MSBuildFailed(exitCode, (workDir, exePath, args)) ->
            let logMsg = [ yield "Log: "; yield! log ] |> String.concat (Environment.NewLine)
            let msbuildErrorMsg =
                [ sprintf "MSBuild failed with exitCode %i" exitCode
                  sprintf "Working Directory: '%s'" workDir
                  sprintf "Exe Path: '%s'" exePath
                  sprintf "Args: '%s'" args ]
                |> String.concat " "
            
            failwithf "%s%s%s" msbuildErrorMsg (Environment.NewLine) logMsg
    | _ ->
        failwithf "error getting msbuild info: internal error"


  // let private lazyGetReferenceAssembliesPath =
  //   lazy( getReferenceAssembliesPathFromMsbuild () )

  // let getReferenceAssembliesPath () =
  //   lazyGetReferenceAssembliesPath.Force()

  // let private getAdditionalArgumentsBy targetFramework =
  
  //   [ yield "--simpleresolution"
  //     yield "--noframework"
  //     yield! Dotnet.ProjInfo.Inspect. getFrameworkInfo targetFramework
  //            |> fun x -> x.ReferencePath
  //            |> List.map (sprintf "-r:%s") ]

  // let getAdditionalArguments targetFramework =
  //   getAdditionalArgumentsBy targetFramework

  let private getInstalledNETVersions () =
    let result, _ = getInfoFromMsbuild (Dotnet.ProjInfo.NETFrameworkInfoFromMSBuild.installedNETFrameworks) []
    match result with
    | Dotnet.ProjInfo.Inspect.GetResult.InstalledNETFw fws ->
        fws
    | r ->
        failwithf "error getting msbuild info: unexpected %A" r

  let private installedNETVersionsLazy = lazy (getInstalledNETVersions ())

  let installedNETVersions () = installedNETVersionsLazy.Force()

  let private defaultReferencesForNonProjectFiles () =
    // ref https://github.com/fsharp/FSharp.Compiler.Service/blob/1f497ef86fd5d0a18e5a935f3d16984fda91f1de/src/fsharp/CompileOps.fs#L1801
    // This list is the default set of references for "non-project" files
    
    // TODO make somehow this list public on FCS and use that directly instead of hardcode it in FSAC

    let GetDefaultSystemValueTupleReference () =
      //TODO check by tfm
      None

    // from https://github.com/fsharp/FSharp.Compiler.Service/blob/1f497ef86fd5d0a18e5a935f3d16984fda91f1de/src/fsharp/CompileOps.fs#L1803-L1832
    [
          yield "System"
          yield "System.Xml" 
          yield "System.Runtime.Remoting"
          yield "System.Runtime.Serialization.Formatters.Soap"
          yield "System.Data"
          yield "System.Drawing"
          yield "System.Core"
          // These are the Portable-profile and .NET Standard 1.6 dependencies of FSharp.Core.dll.  These are needed
          // when an F# sript references an F# profile 7, 78, 259 or .NET Standard 1.6 component which in turn refers 
          // to FSharp.Core for profile 7, 78, 259 or .NET Standard.
          yield "System.Runtime" // lots of types
          yield "System.Linq" // System.Linq.Expressions.Expression<T> 
          yield "System.Reflection" // System.Reflection.ParameterInfo
          yield "System.Linq.Expressions" // System.Linq.IQueryable<T>
          yield "System.Threading.Tasks" // valuetype [System.Threading.Tasks]System.Threading.CancellationToken
          yield "System.IO"  //  System.IO.TextWriter
          //yield "System.Console"  //  System.Console.Out etc.
          yield "System.Net.Requests"  //  System.Net.WebResponse etc.
          yield "System.Collections" // System.Collections.Generic.List<T>
          yield "System.Runtime.Numerics" // BigInteger
          yield "System.Threading"  // OperationCanceledException
          // always include a default reference to System.ValueTuple.dll in scripts and out-of-project sources
          match GetDefaultSystemValueTupleReference() with 
          | None -> ()
          | Some v -> yield v

          yield "System.Web"
          yield "System.Web.Services"
          yield "System.Windows.Forms"
          yield "System.Numerics" 
    ]

  let private getAdditionalArgumentsBy targetFramework =
    let refs =
      let allRefs = defaultReferencesForNonProjectFiles ()
      let props = targetFramework |> Option.map (fun tfm -> "TargetFrameworkVersion", tfm) |> Option.toList
      let result, _ = getInfoFromMsbuild (fun () -> Dotnet.ProjInfo.NETFrameworkInfoFromMSBuild.getReferencePaths allRefs) props
      match result with
      | Dotnet.ProjInfo.Inspect.GetResult.ResolvedNETRefs resolvedRefs ->
          resolvedRefs
      | r ->
          failwithf "error getting msbuild info: unexpected %A" r
    [ yield "--simpleresolution"
      yield "--noframework"
      yield! refs |> List.map (sprintf "-r:%s") ]

  let private additionalArgsByTfm = System.Collections.Concurrent.ConcurrentDictionary<string, string list>()

  let additionalArgumentsBy targetFramework =
    //memoize because expensive
    let f tfm = getAdditionalArgumentsBy (if String.IsNullOrEmpty(tfm) then None else Some tfm)
    let key = targetFramework |> Option.getOrElse ""
    additionalArgsByTfm.GetOrAdd(key, f)


