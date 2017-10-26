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

  let private getInfoFromMsbuild getArgs =
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
            file
            |> Dotnet.ProjInfo.Inspect.getProjectInfoOldSdk loggedMessages.Enqueue msbuildExec getArgs []

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

  let installedNETVersions () =
    let result, _ = getInfoFromMsbuild (Dotnet.ProjInfo.NETFrameworkInfoFromMSBuild.installedNETFrameworks)
    match result with
    | Dotnet.ProjInfo.Inspect.GetResult.InstalledNETFw fws ->
        fws
    | r ->
        failwithf "error getting msbuild info: unexpected %A" r

  let getAdditionalArgumentsBy targetFramework =
    [ yield "--simpleresolution"
      yield "--noframework" ]
