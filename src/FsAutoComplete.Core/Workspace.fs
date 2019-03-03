module FsAutoComplete.Workspace

open ProjectRecognizer
open System.IO

type DPW_ProjectOptions = Dotnet.ProjInfo.Workspace.ProjectOptions
type DPW_ProjectSdkType = Dotnet.ProjInfo.Workspace.ProjectSdkType
type DPW_ProjectOutputType = Dotnet.ProjInfo.Workspace.ProjectOutputType
type DPW_ExtraProjectInfoData = Dotnet.ProjInfo.Workspace.ExtraProjectInfoData

let private getProjectOptions (loader: Dotnet.ProjInfo.Workspace.Loader, fcsBinder: Dotnet.ProjInfo.Workspace.FCS.FCSBinder) (projectFileName: SourceFilePath) =
    if not (File.Exists projectFileName) then
        Error (GenericError(projectFileName, sprintf "File '%s' does not exist" projectFileName))
    else
        match projectFileName with
        | Net45
        | NetCoreSdk ->
            loader.LoadProjects [projectFileName]

            match fcsBinder.GetProjectOptions (projectFileName) with
            | Some po ->
                // same useless log to remain compatible with tests baseline
                let logMap = [ projectFileName, "" ] |> Map.ofList

                Result.Ok (po, List.ofArray po.SourceFiles, logMap)
            | None -> 
                Error (GenericError(projectFileName, (sprintf "Project file '%s' parsing failed" projectFileName)))
        | NetCoreProjectJson ->
            Error (GenericError(projectFileName, (sprintf "Project file '%s' format project.json not supported" projectFileName)))
        | FSharpNetSdk ->
            Error (GenericError(projectFileName, (sprintf "Project file '%s' using FSharp.NET.Sdk not supported" projectFileName)))
        | Unsupported ->
            Error (GenericError(projectFileName, (sprintf "Project file '%s' not supported" projectFileName)))

let extractOptionsDPW (opts: FSharp.Compiler.SourceCodeServices.FSharpProjectOptions) =
    match opts.ExtraProjectInfo with
    | None ->
        Error (GenericError(opts.ProjectFileName, "expected ExtraProjectInfo after project parsing, was None"))
    | Some x ->
        match x with
        | :? DPW_ProjectOptions as poDPW ->
            Ok poDPW
        | x ->
            Error (GenericError(opts.ProjectFileName, (sprintf "expected ExtraProjectInfo after project parsing, was %A" x)))

let private parseProject' (loader, fcsBinder) projectFileName =
    projectFileName
    |> getProjectOptions (loader, fcsBinder)
    |> Result.bind (fun (fcsOpts, projectFiles, logMap) ->
        match extractOptionsDPW fcsOpts with
        | Ok optsDPW -> Ok (fcsOpts, optsDPW, projectFiles, logMap)
        | Error x -> Error x)

let parseProject (loader, fcsBinder) projectFileName =
    projectFileName
    |> parseProject' (loader, fcsBinder)

let loadInBackground onLoaded (loader, fcsBinder) (projects: Project list) = async {

    for project in projects do
        match project.Response with
        | Some res ->
            onLoaded (WorkspaceProjectState.Loaded (res.Options, res.ExtraInfo, res.Files, res.Log))
        | None ->
            project.FileName
            |> parseProject' (loader, fcsBinder)
            |> function
               | Ok (opts, optsDPW, projectFiles, logMap) ->
                   onLoaded (WorkspaceProjectState.Loaded (opts, optsDPW.ExtraProjectInfo, projectFiles, logMap))
               | Error error ->
                   onLoaded (WorkspaceProjectState.Failed (project.FileName, error))

    }
