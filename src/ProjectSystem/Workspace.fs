module ProjectSystem.Workspace

open Dotnet.ProjInfo.Workspace
open ProjectRecognizer
open System.IO

type DPW_ProjectOptions = Dotnet.ProjInfo.Workspace.ProjectOptions
type DPW_ProjectSdkType = Dotnet.ProjInfo.Workspace.ProjectSdkType
type DPW_ProjectOutputType = Dotnet.ProjInfo.Workspace.ProjectOutputType
type DPW_ExtraProjectInfoData = Dotnet.ProjInfo.Workspace.ExtraProjectInfoData

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

let private getProjectOptions (loader: Dotnet.ProjInfo.Workspace.Loader, fcsBinder: Dotnet.ProjInfo.Workspace.FCS.FCSBinder) (projectFileName: string) =
    if not (File.Exists projectFileName) then
        Error (GenericError(projectFileName, sprintf "File '%s' does not exist" projectFileName))
    else
        match projectFileName with
        | Net45
        | NetCoreSdk ->
            loader.LoadProjects [projectFileName]

            fcsBinder.GetProjectOptions (projectFileName)
            |> Result.bind (fun po ->
                extractOptionsDPW po
                |> Result.bind (fun optsDPW ->
                    let logMap = [ projectFileName, "" ] |> Map.ofList
                    let projViewer = Dotnet.ProjInfo.Workspace.ProjectViewer ()
                    let view = projViewer.Render optsDPW
                    let items =
                        if obj.ReferenceEquals(view.Items, null) then [] else view.Items
                    Result.Ok (po, optsDPW, items, logMap)))
        | NetCoreProjectJson ->
            Error (GenericError(projectFileName, (sprintf "Project file '%s' format project.json not supported" projectFileName)))
        | FSharpNetSdk ->
            Error (GenericError(projectFileName, (sprintf "Project file '%s' using FSharp.NET.Sdk not supported" projectFileName)))
        | Unsupported ->
            Error (GenericError(projectFileName, (sprintf "Project file '%s' not supported" projectFileName)))

let private parseProject' (loader, fcsBinder) projectFileName =
    projectFileName
    |> getProjectOptions (loader, fcsBinder)

let internal parseProject (loader, fcsBinder) projectFileName =
    projectFileName
    |> parseProject' (loader, fcsBinder)

let internal loadInBackground onLoaded (loader, fcsBinder) (projects: Project list) = async {

    for project in projects do
        match project.Response with
        | Some res ->
            onLoaded (ProjectSystem.WorkspaceProjectState.Loaded (res.Options, res.ExtraInfo, res.Items, res.Log))
        | None ->
            project.FileName
            |> parseProject' (loader, fcsBinder)
            |> function
               | Ok (opts, optsDPW, projViewerItems, logMap) ->
                   onLoaded (ProjectSystem.WorkspaceProjectState.Loaded (opts, optsDPW.ExtraProjectInfo, projViewerItems, logMap))
               | Error error ->
                   onLoaded (ProjectSystem.WorkspaceProjectState.Failed (project.FileName, error))

    }
