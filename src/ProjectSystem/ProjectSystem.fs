namespace ProjectSystem

open System
open System.IO
open System.Collections.Concurrent
open FSharp.Compiler.SourceCodeServices

type ProjectResult = {
    projectFileName: string
    projectFiles: List<string>
    outFileOpt : string option
    references : string list
    logMap : Map<string,string>
    extra: Dotnet.ProjInfo.Workspace.ExtraProjectInfoData
    projectItems: Dotnet.ProjInfo.Workspace.ProjectViewerItem list
    additionals : Map<string,string> }

[<RequireQualifiedAccess>]
type ProjectResponse =
    | Project of ProjectResult
    | ProjectError of errorDetails: GetProjectOptionsErrors
    | ProjectLoading of projectFileName: string
    | WorkspaceLoad of finished: bool

/// Public API for any operations related to workspace and projects.
/// Internally keeps all the information related to project files in current workspace.
/// It's responsible for refreshing and caching - should be used only as source of information and public API
type ProjectController(checker : FSharpChecker) =
    let fileCheckOptions = ConcurrentDictionary<string, FSharpProjectOptions> ()
    let projects = ConcurrentDictionary<string, Project> ()
    let mutable isWorkspaceReady = false
    let workspaceReady = Event<unit>()
    let notify = Event<ProjectResponse>()

    let updateState (response: ProjectCrackerCache) =
        let normalizeOptions (opts : FSharpProjectOptions) =
            { opts with
                SourceFiles = opts.SourceFiles |> Array.map (Path.GetFullPath)
                OtherOptions = opts.OtherOptions |> Array.map (fun n -> if FscArguments.isCompileFile(n) then Path.GetFullPath n else n)
            }

        for file in response.Items |> List.choose (function Dotnet.ProjInfo.Workspace.ProjectViewerItem.Compile(p, _) -> Some p) do
            fileCheckOptions.[file] <- normalizeOptions response.Options


    let toProjectCache (opts, extraInfo: Dotnet.ProjInfo.Workspace.ExtraProjectInfoData, projViewerItems: Dotnet.ProjInfo.Workspace.ProjectViewerItem list, logMap) =
        let outFileOpt = Some (extraInfo.TargetPath)
        let references = FscArguments.references (opts.OtherOptions |> List.ofArray)
        let fullPathNormalized = Path.GetFullPath >> Utils.normalizePath
        let projViewerItemsNormalized = if obj.ReferenceEquals(null, projViewerItems) then [] else projViewerItems
        let projViewerItemsNormalized =
            projViewerItemsNormalized
            |> List.map (function
                | Dotnet.ProjInfo.Workspace.ProjectViewerItem.Compile(p, c) ->
                    Dotnet.ProjInfo.Workspace.ProjectViewerItem.Compile(fullPathNormalized p, c))

        let cached = {
            ProjectCrackerCache.Options = opts
            OutFile = outFileOpt
            References = references
            Log = logMap
            ExtraInfo = extraInfo
            Items = projViewerItemsNormalized
        }

        (opts.ProjectFileName, cached)

    let workspaceBinder () =
        let loaderConfig = Dotnet.ProjInfo.Workspace.LoaderConfig.Default Environment.msbuildLocator
        let loader = Dotnet.ProjInfo.Workspace.Loader.Create(loaderConfig)
        let infoConfig = Dotnet.ProjInfo.Workspace.NetFWInfoConfig.Default Environment.msbuildLocator
        let netFwInfo = Dotnet.ProjInfo.Workspace.NetFWInfo.Create(infoConfig)
        let binder = Dotnet.ProjInfo.Workspace.FCS.FCSBinder(netFwInfo, loader, checker)
        loader, binder

    member __.WorkspaceReady = workspaceReady.Publish

    member __.NotifyWorkspace = notify.Publish

    member __.IsWorkspaceReady
        with get() = isWorkspaceReady

    member __.GetProjectOptions(file: string) : FSharpProjectOptions option =
        let file = Utils.normalizePath file
        fileCheckOptions.TryFind file

    member __.SetProjectOptions(file: string, opts: FSharpProjectOptions) =
        let file = Utils.normalizePath file
        fileCheckOptions.AddOrUpdate(file, (fun _ -> opts), (fun _ _ -> opts))
        |> ignore

    member __.RemoveProjectOptions(file) =
        let file = Utils.normalizePath file
        fileCheckOptions.TryRemove file |> ignore

    member __.ProjectOptions =
        fileCheckOptions
        |> Seq.map (|KeyValue|)

    member __.GetProject(file: string) : Project option =
        let file = Utils.normalizePath file
        projects.TryFind file

    member __.Projects =
        projects
        |> Seq.map (|KeyValue|)

    member __.LoadProject projectFileName onChange (tfmForScripts: FSIRefs.TFM) onProjectLoaded = async {
        let projectFileName = Path.GetFullPath projectFileName
        let project =
            match projects.TryFind projectFileName with
            | Some prj -> prj
            | None ->
                let proj = new Project(projectFileName, onChange)
                projects.[projectFileName] <- proj
                proj

        let workspaceBinder = workspaceBinder ()

        let projResponse =
            match project.Response with
            | Some response ->
                Result.Ok (projectFileName, response)
            | None ->
                let projectCached =
                    projectFileName
                    |> Workspace.parseProject workspaceBinder
                    |> Result.map (fun (opts, optsDPW, projectFiles, logMap) -> toProjectCache(opts, optsDPW.ExtraProjectInfo, projectFiles, logMap) )
                match projectCached with
                | Result.Ok (projectFileName, response) ->
                    project.Response <- Some response
                    Result.Ok (projectFileName, response)
                | Result.Error error ->
                    project.Response <- None
                    Result.Error error
        return
            match projResponse with
            | Result.Ok (projectFileName, response) ->
                updateState response
                onProjectLoaded projectFileName response tfmForScripts
                let responseFiles =
                    response.Items
                    |> List.choose (function Dotnet.ProjInfo.Workspace.ProjectViewerItem.Compile(p, _) -> Some p)
                let projInfo : ProjectResult =
                    { projectFileName = projectFileName
                      projectFiles = responseFiles
                      outFileOpt = response.OutFile
                      references = response.References
                      logMap = response.Log
                      extra = response.ExtraInfo
                      projectItems = response.Items
                      additionals = Map.empty}

                ProjectResponse.Project projInfo
            | Result.Error error ->
                ProjectResponse.ProjectError error
    }

    member __.LoadWorkspace onChange (files: string list) (tfmForScripts: FSIRefs.TFM) onProjectLoaded = async {
        //TODO check full path
        let projectFileNames = files |> List.map Path.GetFullPath

        let prjs =
            projectFileNames
            |> List.map (fun projectFileName -> projectFileName, new Project(projectFileName, onChange))

        for projectFileName, proj in prjs do
            projects.[projectFileName] <- proj

        let projectLoadedSuccessfully projectFileName response =
            let project =
                match projects.TryFind projectFileName with
                | Some prj -> prj
                | None ->
                    let proj = new Project(projectFileName, onChange)
                    projects.[projectFileName] <- proj
                    proj

            project.Response <- Some response

            updateState response
            onProjectLoaded projectFileName response

        let onLoaded p =
            match p with
            | WorkspaceProjectState.Loading projectFileName ->
                ProjectResponse.ProjectLoading projectFileName
                |> notify.Trigger
            | WorkspaceProjectState.Loaded (opts, extraInfo, projectFiles, logMap) ->
                let projectFileName, response = toProjectCache(opts, extraInfo, projectFiles, logMap)
                projectLoadedSuccessfully projectFileName response tfmForScripts

                let responseFiles =
                    response.Items
                    |> List.choose (function Dotnet.ProjInfo.Workspace.ProjectViewerItem.Compile(p, _) -> Some p)
                let projInfo : ProjectResult =
                    { projectFileName = projectFileName
                      projectFiles = responseFiles
                      outFileOpt = response.OutFile
                      references = response.References
                      logMap = response.Log
                      extra = response.ExtraInfo
                      projectItems = projectFiles
                      additionals = Map.empty}

                ProjectResponse.Project projInfo
                |> notify.Trigger
            | WorkspaceProjectState.Failed (projectFileName, error) ->
                ProjectResponse.ProjectError error
                |> notify.Trigger

        ProjectResponse.WorkspaceLoad false
        |> notify.Trigger

        // this is to delay the project loading notification (of this thread)
        // after the workspaceload started response returned below in outer async
        // Make test output repeteable, and notification in correct order
        match Environment.workspaceLoadDelay() with
        | delay when delay > TimeSpan.Zero ->
            do! Async.Sleep(Environment.workspaceLoadDelay().TotalMilliseconds |> int)
        | _ -> ()

        let loader, fcsBinder = workspaceBinder ()

        let projViewer = Dotnet.ProjInfo.Workspace.ProjectViewer ()

        let bindNewOnloaded (n: Dotnet.ProjInfo.Workspace.WorkspaceProjectState) : WorkspaceProjectState option =
            match n with
            | Dotnet.ProjInfo.Workspace.WorkspaceProjectState.Loading (path, _) ->
                Some (WorkspaceProjectState.Loading path)
            | Dotnet.ProjInfo.Workspace.WorkspaceProjectState.Loaded (opts, logMap) ->
                match fcsBinder.GetProjectOptions(opts.ProjectFileName) with
                | Ok fcsOpts ->
                    match Workspace.extractOptionsDPW fcsOpts with
                    | Ok optsDPW ->
                        let view = projViewer.Render optsDPW
                        Some (WorkspaceProjectState.Loaded (fcsOpts, optsDPW.ExtraProjectInfo, view.Items, logMap))
                    | Error _ ->
                        None //TODO not ignore the error
                | Error _ ->
                    //TODO notify C# project too
                    None
            | Dotnet.ProjInfo.Workspace.WorkspaceProjectState.Failed (path, e) ->
                let error = e
                Some (WorkspaceProjectState.Failed (path, error))

        loader.Notifications.Add(fun (_, arg) ->
            arg |> bindNewOnloaded |> Option.iter onLoaded )

        do! Workspace.loadInBackground onLoaded (loader, fcsBinder) (prjs |> List.map snd)

        ProjectResponse.WorkspaceLoad true
        |> notify.Trigger

        isWorkspaceReady <- true
        workspaceReady.Trigger ()

        return true
    }

    member __.PeekWorkspace (dir: string) (deep: int) (excludedDirs: string list) =
        WorkspacePeek.peek dir deep excludedDirs