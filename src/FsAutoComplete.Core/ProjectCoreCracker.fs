namespace FsAutoComplete

open System
open System.IO

[<RequireQualifiedAccess>]
type ProjectSdkType =
    | Verbose
    | ProjectJson
    | DotnetSdk

[<RequireQualifiedAccess>]
type ProjectOutputType =
    | Library
    | Exe
    | Custom of string

open Microsoft.FSharp.Compiler.SourceCodeServices

module MSBuildPrj = Dotnet.ProjInfo.Inspect

type GetProjectOptionsErrors =
     | ProjectNotRestored of string
     | GenericError of string

exception ProjectInspectException of GetProjectOptionsErrors

type NavigateProjectSM =
    | NoCrossTargeting of NoCrossTargetingData
    | CrossTargeting of string list
and NoCrossTargetingData = { FscArgs: string list; P2PRefs: MSBuildPrj.ResolvedP2PRefsInfo list; Properties: Map<string,string> }

module ProjectCoreCracker =
  let GetProjectOptionsFromResponseFile (file : string)  =
    let projDir = Path.GetDirectoryName file
    let rsp =
      Directory.GetFiles(projDir, "dotnet-compile-fsc.rsp", SearchOption.AllDirectories)
      |> Seq.head
      |> File.ReadAllLines
      |> Array.map (fun s -> if s.EndsWith ".fs" then
                                let p = Path.GetFullPath s
                                (p.Chars 0).ToString().ToLower() + p.Substring(1)
                             else s )
      |> Array.filter((<>) "--nocopyfsharpcore")

    {
      ProjectFileName = file
      ProjectFileNames = [||]
      OtherOptions = rsp
      ReferencedProjects = [||]
      IsIncompleteTypeCheckEnvironment = false
      UseScriptResolutionRules = false
      LoadTime = DateTime.Now
      UnresolvedReferences = None;
      OriginalLoadReferences = []
      ExtraProjectInfo = None
    }

  let runProcess (workingDir: string) (exePath: string) (args: string) =
      let psi = System.Diagnostics.ProcessStartInfo()
      psi.FileName <- exePath
      psi.WorkingDirectory <- workingDir 
      psi.RedirectStandardOutput <- true
      psi.RedirectStandardError <- true
      psi.Arguments <- args
      psi.CreateNoWindow <- true
      psi.UseShellExecute <- false

      use p = new System.Diagnostics.Process()
      p.StartInfo <- psi

      let sbOut = System.Text.StringBuilder()
      p.OutputDataReceived.Add(fun ea -> sbOut.AppendLine(ea.Data) |> ignore)

      let sbErr = System.Text.StringBuilder()
      p.ErrorDataReceived.Add(fun ea -> sbErr.AppendLine(ea.Data) |> ignore)

      p.Start() |> ignore
      p.BeginOutputReadLine()
      p.BeginErrorReadLine()
      p.WaitForExit()
      
      let exitCode = p.ExitCode
      exitCode, (workingDir, exePath, args)

  let GetProjectOptionsFromProjectFile (file : string) =
    
    let rec projInfo additionalMSBuildProps file =
        let projDir = Path.GetDirectoryName file

        let projectAssetsJsonPath = Path.Combine(projDir, "obj", "project.assets.json")
        if not(File.Exists(projectAssetsJsonPath)) then
            raise (ProjectInspectException (ProjectNotRestored file))

        let getFscArgs = Dotnet.ProjInfo.Inspect.getFscArgs
        let getP2PRefs = Dotnet.ProjInfo.Inspect.getResolvedP2PRefs
        let gp () = Dotnet.ProjInfo.Inspect.getProperties ["TargetPath"; "IsCrossTargetingBuild"; "TargetFrameworks"]

        let results =
            let runCmd exePath args = runProcess projDir exePath (args |> String.concat " ")

            let msbuildExec = Dotnet.ProjInfo.Inspect.dotnetMsbuild runCmd
            let log = ignore

            let additionalArgs = additionalMSBuildProps |> List.map (Dotnet.ProjInfo.Inspect.MSBuild.MSbuildCli.Property)

            file
            |> Dotnet.ProjInfo.Inspect.getProjectInfos log msbuildExec [getFscArgs; getP2PRefs; gp] additionalArgs
    
        let todo =
            match results with
            | Choice1Of2 [getFscArgsResult; getP2PRefsResult; gpResult] -> 
                match getFscArgsResult, getP2PRefsResult, gpResult with
                | Choice2Of2(MSBuildPrj.MSBuildSkippedTarget), Choice2Of2(MSBuildPrj.MSBuildSkippedTarget), Choice1Of2 (MSBuildPrj.GetResult.Properties props) ->
                    // Projects with multiple target frameworks, fails if the target framework is not choosen
                    let prop key = props |> Map.ofList |> Map.tryFind key

                    match prop "IsCrossTargetingBuild", prop "TargetFrameworks" with
                    | Some (MSBuildPrj.MSBuild.ConditionEquals "true"), Some (MSBuildPrj.MSBuild.StringList tfms) ->
                        CrossTargeting tfms
                    | _ ->
                        failwithf "error getting msbuild info: some targets skipped, found props: %A" props
                | Choice1Of2 (MSBuildPrj.GetResult.FscArgs fa), Choice1Of2 (MSBuildPrj.GetResult.ResolvedP2PRefs p2p), Choice1Of2 (MSBuildPrj.GetResult.Properties p) ->
                    NoCrossTargeting { FscArgs = fa; P2PRefs = p2p; Properties = p |> Map.ofList }
                | r ->
                    failwithf "error getting msbuild info: %A" r
            | Choice1Of2 r -> 
                failwithf "error getting msbuild info: internal error, more info returned than expected %A" r
            | Choice2Of2 r ->
                match r with
                | Dotnet.ProjInfo.Inspect.GetProjectInfoErrors.MSBuildSkippedTarget -> 
                    failwithf "Unexpected MSBuild result, all targets skipped"
                | Dotnet.ProjInfo.Inspect.GetProjectInfoErrors.UnexpectedMSBuildResult(r) -> 
                    failwithf "Unexpected MSBuild result %s" r
                | Dotnet.ProjInfo.Inspect.GetProjectInfoErrors.MSBuildFailed(exitCode, (workDir, exePath, args)) -> 
                    [ sprintf "MSBuild failed with exitCode %i" exitCode
                      sprintf "Working Directory: '%s'" workDir
                      sprintf "Exe Path: '%s'" exePath
                      sprintf "Args: '%s'" args ]
                    |> String.concat " "
                    |> failwith

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

            let compileFilesToAbsolutePath (f: string) =
                if f.EndsWith(".fs") then
                    if Path.IsPathRooted f then f else Path.Combine(projDir, f)
                else
                    f

            let po =
                {
                    ProjectFileName = file
                    ProjectFileNames = [||]
                    OtherOptions = rsp |> List.map compileFilesToAbsolutePath |> Array.ofList
                    ReferencedProjects = p2pProjects |> Array.ofList
                    IsIncompleteTypeCheckEnvironment = false
                    UseScriptResolutionRules = false
                    LoadTime = DateTime.Now
                    UnresolvedReferences = None;
                    OriginalLoadReferences = []
                    ExtraProjectInfo = None
                }

            tar, po

    let _, po = projInfo [] file
    po
