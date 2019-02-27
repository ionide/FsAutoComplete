module FsAutoComplete.Workspace

open ProjectRecognizer
open System.IO

let getProjectOptions notifyState (cache: ProjectCrackerDotnetSdk.ParsedProjectCache) verbose (projectFileName: SourceFilePath) =
    if not (File.Exists projectFileName) then
        Error (GenericError(projectFileName, sprintf "File '%s' does not exist" projectFileName))
    else
        match projectFileName with
        | NetCoreProjectJson -> ProjectCrackerProjectJson.load projectFileName
        | NetCoreSdk -> ProjectCrackerDotnetSdk.load notifyState cache projectFileName
        | FSharpNetSdk -> Error (GenericError(projectFileName, (sprintf "Project file '%s' using FSharp.NET.Sdk not supported" projectFileName)))
#if NO_PROJECTCRACKER
        | Net45 -> ProjectCrackerDotnetSdk.loadVerboseSdk notifyState cache projectFileName
        | Unsupported -> Error (GenericError(projectFileName, (sprintf "Project file '%s' not supported" projectFileName)))
#else
        | Net45 -> ProjectCrackerVerbose.load notifyState FSharpCompilerServiceCheckerHelper.ensureCorrectFSharpCore projectFileName verbose
        | Unsupported -> ProjectCrackerVerbose.load notifyState FSharpCompilerServiceCheckerHelper.ensureCorrectFSharpCore projectFileName verbose
#endif

let private bindExtraOptions (opts: FSharp.Compiler.SourceCodeServices.FSharpProjectOptions, projectFiles, logMap) =
    match opts.ExtraProjectInfo with
    | None ->
        Error (GenericError(opts.ProjectFileName, "expected ExtraProjectInfo after project parsing, was None"))
    | Some x ->
        match x with
        | :? ExtraProjectInfoData as extraInfo ->
            Ok (opts, extraInfo, projectFiles, logMap)
        | x ->
            Error (GenericError(opts.ProjectFileName, (sprintf "expected ExtraProjectInfo after project parsing, was %A" x)))

let private deduplicateReferences (opts: FSharp.Compiler.SourceCodeServices.FSharpProjectOptions, projectFiles, logMap) =
    let projs =
        opts.ReferencedProjects |> Array.map fst

    let references =
        opts.OtherOptions
        |> Array.choose (fun n -> if n.StartsWith "-r:" then Some (n.Substring(3)) else None)
        |> Array.groupBy (Path.GetFullPathSafe)
        |> Array.map (fun (_,lst) ->
            match lst |> Array.tryFind (fun n -> projs |> Array.contains n) with
            | Some s -> s
            | None -> Array.head lst )

    let oos = [|
        yield! (opts.OtherOptions |> Array.filter (fun n -> not (n.StartsWith "-r:")))
        yield! (references |> Array.map (sprintf "-r:%s"))
    |]
    let opts = {opts with OtherOptions = oos}
    opts, projectFiles, logMap

let private removeDeprecatedArgs (opts: FSharp.Compiler.SourceCodeServices.FSharpProjectOptions, projectFiles, logMap) =
    let oos = opts.OtherOptions |> Array.filter (fun n -> n <> "--times" && n <> "--no-jit-optimize")
    let opts = {opts with OtherOptions = oos}
    opts, projectFiles, logMap

let parseProject verbose projectFileName =
    let projsCache = new ProjectCrackerDotnetSdk.ParsedProjectCache()
    projectFileName
    |> getProjectOptions ignore projsCache verbose
    |> Result.map deduplicateReferences
    |> Result.map removeDeprecatedArgs
    |> Result.bind bindExtraOptions

let loadInBackground onLoaded verbose (projects: Project list) = async {
    let projsCache = new ProjectCrackerDotnetSdk.ParsedProjectCache()

    projects
    |> List.iter(fun project ->
        match project.Response with
        | Some res ->
            onLoaded (WorkspaceProjectState.Loaded (res.Options, res.ExtraInfo, res.Files, res.Log))
        | None ->
            project.FileName
            |> getProjectOptions onLoaded projsCache verbose
            |> Result.map deduplicateReferences
            |> Result.map removeDeprecatedArgs
            |> Result.bind bindExtraOptions
            |> function
            | Ok (opts, extraInfo, projectFiles, logMap) ->
                    onLoaded (WorkspaceProjectState.Loaded (opts, extraInfo, projectFiles, logMap))
            | Error error ->
                    onLoaded (WorkspaceProjectState.Failed (project.FileName, error))
    )
    }
