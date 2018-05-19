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

module MSBuildKnownProperties =
    let TargetFramework = "TargetFramework"

module ProjectCrackerDotnetSdk =

  open DotnetProjInfoInspectHelpers

  let msbuildPropProjectOutputType (s: string) =
    match s.Trim() with
    | MSBuildPrj.MSBuild.ConditionEquals "Exe" -> ProjectOutputType.Exe
    | MSBuildPrj.MSBuild.ConditionEquals "Library" -> ProjectOutputType.Library
    | x -> ProjectOutputType.Custom x

  let getExtraInfo targetPath props =
    let msbuildPropBool prop =
        props |> Map.tryFind prop |> Option.bind msbuildPropBool
    let msbuildPropStringList prop =
        props |> Map.tryFind prop |> Option.map msbuildPropStringList
    let msbuildPropString prop =
        props |> Map.tryFind prop

    { ProjectSdkTypeDotnetSdk.IsTestProject = msbuildPropBool "IsTestProject" |> Option.getOrElse false
      Configuration = msbuildPropString "Configuration" |> Option.getOrElse ""
      IsPackable = msbuildPropBool "IsPackable" |> Option.getOrElse false
      TargetFramework = msbuildPropString MSBuildKnownProperties.TargetFramework |> Option.getOrElse ""
      TargetFrameworkIdentifier = msbuildPropString "TargetFrameworkIdentifier" |> Option.getOrElse ""
      TargetFrameworkVersion = msbuildPropString "TargetFrameworkVersion" |> Option.getOrElse ""
      TargetPath = targetPath

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

  type ParsedProject = string * FSharpProjectOptions * ((string * string) list)
  type ParsedProjectCache = Collections.Concurrent.ConcurrentDictionary<string, ParsedProject>

  let private getProjectOptionsFromProjectFile notifyState (cache: ParsedProjectCache) parseAsSdk (file : string) =

    let rec projInfoOf additionalMSBuildProps file : ParsedProject =
        let projDir = Path.GetDirectoryName file

        notifyState (WorkspaceProjectState.Loading file)

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
                let asFscArgs props =
                    let fsc = Microsoft.FSharp.Build.Fsc()
                    Dotnet.ProjInfo.FakeMsbuildTasks.getResponseFileFromTask props fsc
                let ok =
#if NETSTANDARD2_0
                    Ok
#else
                    Choice1Of2
#endif
                Dotnet.ProjInfo.Inspect.getFscArgsOldSdk (asFscArgs >> ok)

        let getP2PRefs = Dotnet.ProjInfo.Inspect.getResolvedP2PRefs
        let additionalInfo = //needed for extra
            [ "OutputType"
              "IsTestProject"
              "Configuration"
              "IsPackable"
              MSBuildKnownProperties.TargetFramework
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
            file |> projInfo [MSBuildKnownProperties.TargetFramework, tfm]
        | CrossTargeting [] ->
            failwithf "Unexpected, found cross targeting but empty target frameworks list"
        | NoCrossTargeting { FscArgs = rsp; P2PRefs = p2ps; Properties = props } ->

            //TODO cache projects info of p2p ref
            let p2pProjects =
                p2ps
                // do not follow others lang project, is not supported by FCS anyway
                |> List.filter (fun p2p -> p2p.ProjectReferenceFullPath.ToLower().EndsWith(".fsproj"))
                |> List.map (fun p2p ->
                    let followP2pArgs =
                        p2p.TargetFramework
                        |> Option.map (fun tfm -> MSBuildKnownProperties.TargetFramework, tfm)
                        |> Option.toList
                    p2p.ProjectReferenceFullPath |> projInfo followP2pArgs )

            let tar =
                match props |> Map.tryFind "TargetPath" with
                | Some t -> t
                | None -> failwith "error, 'TargetPath' property not found"

            let rspNormalized =
                //workaround, arguments in rsp can use relative paths
                rsp |> List.map (FscArguments.useFullPaths projDir)

            let sdkTypeData, log =
                match parseAsSdk with
                | ProjectParsingSdk.DotnetSdk ->
                    let extraInfo = getExtraInfo tar props
                    ProjectSdkType.DotnetSdk(extraInfo), []
                | ProjectParsingSdk.VerboseSdk ->
                    //compatibility with old behaviour, so output is exactly the same
                    let mergedLog =
                        [ yield (file, "")
                          yield! p2pProjects |> List.collect (fun (_,_,x) -> x) ]
                    ProjectSdkType.Verbose { TargetPath = tar }, mergedLog

            let po =
                {
                    ProjectFileName = file
                    SourceFiles = [||]
                    OtherOptions = rspNormalized |> Array.ofList
                    ReferencedProjects = p2pProjects |> List.map (fun (x,y,_) -> (x,y)) |> Array.ofList
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

            tar, po, log

    and projInfo additionalMSBuildProps file : ParsedProject =
        let key = sprintf "%s;%A" file additionalMSBuildProps
        match cache.TryGetValue(key) with
        | true, alreadyParsed ->
            alreadyParsed
        | false, _ ->
            let p = file |> projInfoOf additionalMSBuildProps
            cache.AddOrUpdate(key, p, fun _ _ -> p)


    let _, po, log = projInfo [] file
    po, log

  let private (|ProjectExtraInfoBySdk|_|) po =
      match po.ExtraProjectInfo with
      | None -> None
      | Some x ->
          match x with
          | :? ExtraProjectInfoData as extraInfo ->
              Some extraInfo
          | _ -> None

  let private loadBySdk notifyState (cache: ParsedProjectCache) parseAsSdk file =
      try
        let po, log = getProjectOptionsFromProjectFile notifyState cache parseAsSdk file

        let compileFiles =
            let sources = FscArguments.compileFiles (po.OtherOptions |> List.ofArray)
            match po with
            | ProjectExtraInfoBySdk extraInfo ->
                match extraInfo.ProjectSdkType with
                | ProjectSdkType.Verbose _ ->
                    //compatibility with old behaviour (projectcracker), so test output is exactly the same
                    //the temp source files (like generated assemblyinfo.fs) are not added to sources
                    let isTempFile (name: string) =
                        let tempPath = Path.GetTempPath()
                        let s = name.ToLower()
                        s.StartsWith(tempPath.ToLower())
                    sources
                    |> List.filter (not << isTempFile)
                | ProjectSdkType.ProjectJson
                | ProjectSdkType.DotnetSdk _ ->
                    sources
            | _ -> sources

        Ok (po, Seq.toList compileFiles, (log |> Map.ofList))
      with
        | ProjectInspectException d -> Error d
        | e -> Error (GenericError(file, e.Message))

  let load notifyState (cache: ParsedProjectCache) file =
      loadBySdk notifyState cache ProjectParsingSdk.DotnetSdk file

  let loadVerboseSdk notifyState (cache: ParsedProjectCache) file =
      loadBySdk notifyState cache ProjectParsingSdk.VerboseSdk file
