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

let private bindResults isFromCache fn res  =
  res
  |> Result.bind (fun po ->
    extractOptionsDPW po
    |> Result.bind (fun optsDPW ->
        let logMap = [ fn, "" ] |> Map.ofList
        let projViewer = Dotnet.ProjInfo.Workspace.ProjectViewer ()
        let view = projViewer.Render optsDPW
        let items =
            if obj.ReferenceEquals(view.Items, null) then [] else view.Items
        Result.Ok (po, optsDPW, items, logMap, isFromCache)))

let private loaderNotificationHandler (fcsBinder: Dotnet.ProjInfo.Workspace.FCS.FCSBinder) ((loader, state): Loader * WorkspaceProjectState) =
  match state with
  | WorkspaceProjectState.Loading(_,_) -> None //we just ignore loading notifications in this case
  | WorkspaceProjectState.Loaded(po, _, isFromCache) ->
    let projectFileName = po.ProjectFileName
    let x =
      fcsBinder.GetProjectOptions projectFileName
      |> bindResults isFromCache projectFileName

    Some x
  | WorkspaceProjectState.Failed(projectFileName, _) ->
    let x =
      fcsBinder.GetProjectOptions projectFileName
      |> bindResults false projectFileName

    Some x

let private getProjectOptions (loader: Dotnet.ProjInfo.Workspace.Loader) (fcsBinder: Dotnet.ProjInfo.Workspace.FCS.FCSBinder) (onLoaded: ProjectSystem.WorkspaceProjectState -> unit) (generateBinlog: bool) (projectFileNames: string list) =
    let existing, notExisting = projectFileNames |> List.partition (File.Exists)
    for e in notExisting do
      let error = GenericError(e, sprintf "File '%s' does not exist" e)
      onLoaded (ProjectSystem.WorkspaceProjectState.Failed (e, error))

    let supported, notSupported = existing |> List.partition (isSupported)
    for e in notSupported do
      let error = GenericError(e, (sprintf "Project file '%s' not supported" e))
      onLoaded (ProjectSystem.WorkspaceProjectState.Failed (e, error))

    let handler res =
      loaderNotificationHandler fcsBinder res
      |> Option.iter (fun n ->
          match n with
          | Ok (opts, optsDPW, projViewerItems, logMap, isFromCache) ->
              onLoaded (ProjectSystem.WorkspaceProjectState.Loaded (opts, optsDPW.ExtraProjectInfo, projViewerItems, logMap, isFromCache))
          | Error error ->
              onLoaded (ProjectSystem.WorkspaceProjectState.Failed (error.ProjFile, error))
      )

    use notif = loader.Notifications.Subscribe handler
    loader.LoadProjects(supported, generateBinlog, 1)

let internal loadInBackground onLoaded (loader, fcsBinder) (projects: Project list) (generateBinlog: bool) = async {
    let (resProjects, otherProjects) =
      projects |> List.partition (fun n -> n.Response.IsSome)

    for project in resProjects do
      match project.Response with
      | Some res ->
          onLoaded (ProjectSystem.WorkspaceProjectState.Loaded (res.Options, res.ExtraInfo, res.Items, res.Log, false))
      | None ->
        () //Shouldn't happen

    otherProjects
    |> List.map (fun n -> n.FileName)
    |> getProjectOptions loader fcsBinder onLoaded generateBinlog
  }


let private getProjectOptionsSingle (loader: Dotnet.ProjInfo.Workspace.Loader, fcsBinder: Dotnet.ProjInfo.Workspace.FCS.FCSBinder) (generateBinlog: bool) (projectFileName: string) =
    if not (File.Exists projectFileName) then
        Error (GenericError(projectFileName, sprintf "File '%s' does not exist" projectFileName))
    else
        match projectFileName with
        | Net45
        | NetCoreSdk ->
            loader.LoadProjects([projectFileName], generateBinlog)

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


let internal parseProject (loader, fcsBinder) (generateBinlog: bool) projectFileName =
    projectFileName
    |> getProjectOptionsSingle (loader, fcsBinder) generateBinlog
