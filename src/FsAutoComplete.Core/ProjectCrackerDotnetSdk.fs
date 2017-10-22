namespace FsAutoComplete

open System
open System.IO

open Microsoft.FSharp.Compiler.SourceCodeServices

module MSBuildPrj = Dotnet.ProjInfo.Inspect

exception ProjectInspectException of GetProjectOptionsErrors

type NavigateProjectSM =
    | NoCrossTargeting of NoCrossTargetingData
    | CrossTargeting of string list
and NoCrossTargetingData = { FscArgs: string list; P2PRefs: MSBuildPrj.ResolvedP2PRefsInfo list; Properties: Map<string,string> }

module ProjectCrackerDotnetSdk =

  let msbuildPropBool (s: string) =
    match s.Trim() with
    | "" -> None
    | MSBuildPrj.MSBuild.ConditionEquals "True" -> Some true
    | _ -> Some false

  let msbuildPropStringList (s: string) =
    match s.Trim() with
    | "" -> []
    | MSBuildPrj.MSBuild.StringList list  -> list
    | _ -> []

  let msbuildPropProjectOutputType (s: string) =
    match s.Trim() with
    | MSBuildPrj.MSBuild.ConditionEquals "Exe" -> ProjectOutputType.Exe
    | MSBuildPrj.MSBuild.ConditionEquals "Library" -> ProjectOutputType.Library
    | x -> ProjectOutputType.Custom x

  let getExtraInfo props =
    let msbuildPropBool prop =
        props |> Map.tryFind prop |> Option.bind msbuildPropBool
    let msbuildPropStringList prop =
        props |> Map.tryFind prop |> Option.map msbuildPropStringList
    let msbuildPropString prop =
        props |> Map.tryFind prop

    { ProjectSdkTypeDotnetSdk.IsTestProject = msbuildPropBool "IsTestProject" |> Option.getOrElse false
      Configuration = msbuildPropString "Configuration" |> Option.getOrElse ""
      IsPackable = msbuildPropBool "IsPackable" |> Option.getOrElse false
      TargetFramework = msbuildPropString "TargetFramework" |> Option.getOrElse ""
      TargetFrameworkIdentifier = msbuildPropString "TargetFrameworkIdentifier" |> Option.getOrElse ""
      TargetFrameworkVersion = msbuildPropString "TargetFrameworkVersion" |> Option.getOrElse ""

      MSBuildAllProjects = msbuildPropStringList "MSBuildAllProjects" |> Option.getOrElse []
      MSBuildToolsVersion = msbuildPropString "MSBuildToolsVersion" |> Option.getOrElse ""

      ProjectAssetsFile = msbuildPropString "ProjectAssetsFile" |> Option.getOrElse ""
      RestoreSuccess = msbuildPropBool "RestoreSuccess" |> Option.getOrElse false

      Configurations = msbuildPropStringList "Configurations" |> Option.getOrElse []
      TargetFrameworks = msbuildPropStringList "TargetFrameworks" |> Option.getOrElse []

      RunArguments = msbuildPropString "RunArguments"
      RunCommand = msbuildPropString "RunCommand"

      IsPublishable = msbuildPropBool "IsPublishable" }

  type private ProjectParsingSdk = DotnetSdk | VerboseSdk

  let private getProjectOptionsFromProjectFile parseAsSdk (file : string) =

    let rec projInfo additionalMSBuildProps file =
        let projDir = Path.GetDirectoryName file

        match parseAsSdk with
        | ProjectParsingSdk.DotnetSdk ->
            let projectAssetsJsonPath = Path.Combine(projDir, "obj", "project.assets.json")
            if not(File.Exists(projectAssetsJsonPath)) then
                raise (ProjectInspectException (ProjectNotRestored file))
        | ProjectParsingSdk.VerboseSdk ->
            ()

        let getFscArgs =
            match parseAsSdk with
            | ProjectParsingSdk.DotnetSdk ->
                Dotnet.ProjInfo.Inspect.getFscArgs
            | ProjectParsingSdk.VerboseSdk ->
#if NO_PROJECTCRACKER
                let asFscArgs props =
                    let fsc = Microsoft.FSharp.Build.Fsc()
                    Dotnet.ProjInfo.FakeMsbuildTasks.getResponseFileFromTask props fsc
                Dotnet.ProjInfo.Inspect.getFscArgsOldSdk (fun _ -> Ok [])
#else
                failwithf "project parsing not supported on verbose sdk"
#endif

        let getP2PRefs = Dotnet.ProjInfo.Inspect.getResolvedP2PRefs
        let additionalInfo = //needed for extra
            [ "OutputType"
              "IsTestProject"
              "Configuration"
              "IsPackable"
              "TargetFramework"
              "TargetFrameworkIdentifier"
              "TargetFrameworkVersion"
              "MSBuildAllProjects"
              "ProjectAssetsFile"
              "RestoreSuccess"
              "Configurations"
              "TargetFrameworks"
              "RunArguments"
              "RunCommand"
              "IsPublishable"
            ]
        let gp () = Dotnet.ProjInfo.Inspect.getProperties (["TargetPath"; "IsCrossTargetingBuild"; "TargetFrameworks"] @ additionalInfo)

        let results, log =
            let loggedMessages = System.Collections.Concurrent.ConcurrentQueue<string>()

            let runCmd exePath args = Utils.runProcess loggedMessages.Enqueue projDir exePath (args |> String.concat " ")

            let msbuildExec =
                let msbuildPath =
                    match parseAsSdk with
                    | ProjectParsingSdk.DotnetSdk ->
                        Dotnet.ProjInfo.Inspect.MSBuildExePath.DotnetMsbuild "dotnet"
                    | ProjectParsingSdk.VerboseSdk ->
                        Dotnet.ProjInfo.Inspect.MSBuildExePath.Path "msbuild"
                Dotnet.ProjInfo.Inspect.msbuild msbuildPath runCmd

            let additionalArgs = additionalMSBuildProps |> List.map (Dotnet.ProjInfo.Inspect.MSBuild.MSbuildCli.Property)

            let inspect =
                match parseAsSdk with
                | ProjectParsingSdk.DotnetSdk ->
                    Dotnet.ProjInfo.Inspect.getProjectInfos
                | ProjectParsingSdk.VerboseSdk ->
                    Dotnet.ProjInfo.Inspect.getProjectInfosOldSdk

            let infoResult =
                file
                |> inspect loggedMessages.Enqueue msbuildExec [getFscArgs; getP2PRefs; gp] additionalArgs

            infoResult, (loggedMessages.ToArray() |> Array.toList)

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

        let todo =
            match results with
            | MsbuildOk [getFscArgsResult; getP2PRefsResult; gpResult] ->
                match getFscArgsResult, getP2PRefsResult, gpResult with
                | MsbuildError(MSBuildPrj.MSBuildSkippedTarget), MsbuildError(MSBuildPrj.MSBuildSkippedTarget), MsbuildOk (MSBuildPrj.GetResult.Properties props) ->
                    // Projects with multiple target frameworks, fails if the target framework is not choosen
                    let prop key = props |> Map.ofList |> Map.tryFind key

                    match prop "IsCrossTargetingBuild", prop "TargetFrameworks" with
                    | Some (MSBuildPrj.MSBuild.ConditionEquals "true"), Some (MSBuildPrj.MSBuild.StringList tfms) ->
                        CrossTargeting tfms
                    | _ ->
                        failwithf "error getting msbuild info: some targets skipped, found props: %A" props
                | MsbuildOk (MSBuildPrj.GetResult.FscArgs fa), MsbuildOk (MSBuildPrj.GetResult.ResolvedP2PRefs p2p), MsbuildOk (MSBuildPrj.GetResult.Properties p) ->
                    NoCrossTargeting { FscArgs = fa; P2PRefs = p2p; Properties = p |> Map.ofList }
                | r ->
                    failwithf "error getting msbuild info: %A" r
            | MsbuildOk r ->
                failwithf "error getting msbuild info: internal error, more info returned than expected %A" r
            | MsbuildError r ->
                match r with
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

        match todo with
        | CrossTargeting (tfm :: _) ->
            // Atm setting a preferenece is not supported in FSAC
            // As workaround, lets choose the first of the target frameworks and use that
            file |> projInfo ["TargetFramework", tfm]
        | CrossTargeting [] ->
            failwithf "Unexpected, found cross targeting but empty target frameworks list"
        | NoCrossTargeting { FscArgs = rsp; P2PRefs = p2ps; Properties = props } ->

            //TODO cache projects info of p2p ref
            let p2pProjects =
                p2ps
                // do not follow others lang project, is not supported by FCS anyway
                |> List.filter (fun p2p -> p2p.ProjectReferenceFullPath.ToLower().EndsWith(".fsproj"))
                |> List.map (fun p2p -> p2p.ProjectReferenceFullPath |> projInfo ["TargetFramework", p2p.TargetFramework] )

            let tar =
                match props |> Map.tryFind "TargetPath" with
                | Some t -> t
                | None -> failwith "error, 'TargetPath' property not found"

            let rspNormalized =
                //workaround, arguments in rsp can use relative paths
                rsp |> List.map (FscArguments.useFullPaths projDir)
            
            let sdkTypeData =
                match parseAsSdk with
                | ProjectParsingSdk.DotnetSdk ->
                    let extraInfo = getExtraInfo props
                    ProjectSdkType.DotnetSdk(extraInfo)
                | ProjectParsingSdk.VerboseSdk ->
                    ProjectSdkType.Verbose

            let po =
                {
                    ProjectFileName = file
                    SourceFiles = [||]
                    OtherOptions = rspNormalized |> Array.ofList
                    ReferencedProjects = p2pProjects |> Array.ofList
                    IsIncompleteTypeCheckEnvironment = false
                    UseScriptResolutionRules = false
                    LoadTime = DateTime.Now
                    UnresolvedReferences = None
                    OriginalLoadReferences = []
                    Stamp = None
                    ExtraProjectInfo =
                        Some (box {
                            ExtraProjectInfoData.ProjectSdkType = sdkTypeData
                            ExtraProjectInfoData.ProjectOutputType = FscArguments.outType rspNormalized
                        })
                }

            tar, po

    let _, po = projInfo [] file
    po


  let private loadBySdk parseAsSdk file =
      try
        let po = getProjectOptionsFromProjectFile parseAsSdk file
        let compileFiles = FscArguments.compileFiles (po.OtherOptions |> List.ofArray)
        Ok (po, Seq.toList compileFiles, Map<string,string>([||]))
      with
        | ProjectInspectException d -> Error d
        | e -> Error (GenericError(e.Message))

  let load file =
      loadBySdk ProjectParsingSdk.DotnetSdk file

  let loadVerboseSdk file =
      loadBySdk ProjectParsingSdk.VerboseSdk file
