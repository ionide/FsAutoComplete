namespace FsAutoComplete.BuildServer

open System
open System.IO
open FsAutoComplete.Logging
open Ionide.ProjInfo.ProjectSystem
open Ionide.ProjInfo.Types
open System.Collections.Generic
open System.Threading.Tasks
open FsToolkit.ErrorHandling
open BuildServerProtocol
open IcedTasks

/// Core workspace operations that wrap Ionide.ProjInfo functionality
module WorkspaceOperations =

  let private logger = LogProvider.getLoggerByName "WorkspaceOperations"

  type WorkspaceState = 
    { WorkspaceLoader: IWorkspaceLoader option
      LoadedProjects: Map<string, ProjectOptions>
      RootPath: string option
      FileWatchers: IDisposable list }

  let mutable private currentState = 
    { WorkspaceLoader = None
      LoadedProjects = Map.empty
      RootPath = None
      FileWatchers = [] }

  /// Initialize the workspace loader with the given tools path
  let initializeWorkspaceLoader (toolsPath: Ionide.ProjInfo.IToolsPath) useProjectGraph =
    logger.info "Initializing workspace loader"
    
    let loader = 
      if useProjectGraph then
        Ionide.ProjInfo.WorkspaceLoaderViaProjectGraph.Create(toolsPath, [])
      else
        Ionide.ProjInfo.WorkspaceLoader.Create(toolsPath, [])
    
    currentState <- { currentState with WorkspaceLoader = Some loader }
    loader

  /// Dispose of current watchers and state
  let private disposeWatchers () =
    currentState.FileWatchers |> List.iter (fun w -> w.Dispose())
    currentState <- { currentState with FileWatchers = [] }

  /// Discover potential projects and solutions in a directory
  let peekWorkspace (request: WorkspacePeekRequest) : Task<Result<WorkspacePeekResponse, string>> =
    task {
      try
        logger.info $"Peeking workspace at {request.Directory}"
        
        let directory = DirectoryInfo(request.Directory)
        if not directory.Exists then
          return Error $"Directory does not exist: {request.Directory}"
        else
          let excludedDirs = Set.ofArray request.ExcludedDirs
          
          let rec findProjects (dir: DirectoryInfo) depth =
            if depth <= 0 then []
            else
              let subdirs = 
                try
                  dir.GetDirectories()
                  |> Array.filter (fun d -> not (excludedDirs.Contains d.Name))
                  |> Array.toList
                with
                | _ -> []
              
              let projectFiles = 
                try
                  [
                    yield! dir.GetFiles("*.sln") |> Array.map (fun f -> f.FullName, "solution")
                    yield! dir.GetFiles("*.fsproj") |> Array.map (fun f -> f.FullName, "project")
                    yield! dir.GetFiles("*.csproj") |> Array.map (fun f -> f.FullName, "project")
                    yield! dir.GetFiles("*.vbproj") |> Array.map (fun f -> f.FullName, "project")
                  ]
                with
                | _ -> []
              
              let currentProjects = 
                projectFiles
                |> List.map (fun (path, kind) ->
                  { Project = 
                    { Project = path
                      Name = Path.GetFileNameWithoutExtension(path)
                      Virtual = Some false
                      Dependencies = None }
                    Crosswalk = None
                    Sdk = 
                      if kind = "project" then
                        Some { Type = "dotnet"; Path = None }
                      else
                        None })
              
              let subdirProjects = 
                subdirs
                |> List.collect (fun subdir -> findProjects subdir (depth - 1))
              
              currentProjects @ subdirProjects
          
          let found = findProjects directory request.Deep
          let response = { Found = Array.ofList found }
          
          logger.info $"Found {found.Length} potential projects"
          return Ok response
      with
      | ex ->
        logger.error $"Error peeking workspace: {ex.Message}"
        return Error ex.Message
    }

  /// Load workspace projects
  let loadWorkspace (request: WorkspaceLoadRequest) : Task<Result<WorkspaceLoadResponse, string>> =
    task {
      try
        match currentState.WorkspaceLoader with
        | None -> 
          return Error "Workspace loader not initialized"
        | Some loader ->
          logger.info $"Loading workspace with {request.TextDocuments.Length} documents"
          
          // Dispose existing watchers
          disposeWatchers()
          
          // Find project files from text documents
          let projectFiles = 
            request.TextDocuments
            |> Array.choose (fun doc ->
              let docPath = 
                if Uri.IsWellFormedUriString(doc, UriKind.Absolute) then
                  Uri(doc).LocalPath
                else 
                  doc
              
              // Look for project file in the same directory or parent directories
              let rec findProjectFile (dir: DirectoryInfo) =
                if isNull dir then None
                else
                  let projFiles = 
                    dir.GetFiles("*.fsproj") 
                    |> Array.append (dir.GetFiles("*.csproj"))
                    |> Array.append (dir.GetFiles("*.vbproj"))
                  
                  if projFiles.Length > 0 then
                    Some projFiles.[0].FullName
                  else
                    findProjectFile dir.Parent
              
              let docDir = DirectoryInfo(Path.GetDirectoryName(docPath))
              findProjectFile docDir)
            |> Array.distinct
          
          if projectFiles.Length = 0 then
            return Error "No project files found for the provided documents"
          else
            // Load projects using Ionide.ProjInfo
            let! loadResult = 
              loader.LoadProjects(
                List.ofArray projectFiles, 
                [], 
                Ionide.ProjInfo.BinaryLogGeneration.Off)
            
            match loadResult with
            | Ok projects ->
              // Convert to our format
              let projectDetails = 
                projects
                |> List.map (fun proj ->
                  { Project = proj.ProjectFileName
                    Name = Path.GetFileNameWithoutExtension(proj.ProjectFileName)
                    SourceFiles = proj.SourceFiles |> Array.ofList
                    ProjectReferences = 
                      proj.ReferencedProjects 
                      |> List.choose (function
                        | FSharpReferencedProject.FSharpReference(options = opts) -> Some opts.ProjectFileName
                        | _ -> None)
                      |> Array.ofList
                    PackageReferences = 
                      proj.PackageReferences
                      |> List.map (fun pkg ->
                        { Name = pkg.Name
                          Version = pkg.Version
                          FullPath = pkg.FullPath })
                      |> Array.ofList
                    FrameworkVersion = 
                      proj.TargetFramework |> Option.defaultValue "Unknown"
                    TargetFramework = 
                      proj.TargetFramework |> Option.defaultValue "Unknown"
                    OutputType = "Library" // TODO: extract from project
                    OutputFile = proj.OutputFile |> Option.defaultValue ""
                    IsTestProject = None // TODO: detect test projects
                    Properties = None })
              
              // Store loaded projects
              let projectMap = 
                projects
                |> List.map (fun p -> p.ProjectFileName, p)
                |> Map.ofList
              
              currentState <- 
                { currentState with 
                    LoadedProjects = projectMap
                    RootPath = 
                      if projectFiles.Length > 0 then
                        Some (Path.GetDirectoryName(projectFiles.[0]))
                      else None }
              
              let response = 
                { WorkspaceRoot = currentState.RootPath |> Option.defaultValue ""
                  Projects = Array.ofList projectDetails }
              
              logger.info $"Successfully loaded {projectDetails.Length} projects"
              return Ok response
              
            | Error errorMsg ->
              logger.error $"Failed to load projects: {errorMsg}"
              return Error errorMsg
      with
      | ex ->
        logger.error $"Error loading workspace: {ex.Message}"
        return Error ex.Message
    }

  /// Get details for a specific project
  let getProject (request: FSharpProjectRequest) : Task<Result<FSharpProjectResponse, string>> =
    task {
      try
        match currentState.LoadedProjects.TryFind request.Project with
        | Some project ->
          let details = 
            { Project = project.ProjectFileName
              Name = Path.GetFileNameWithoutExtension(project.ProjectFileName)
              SourceFiles = project.SourceFiles |> Array.ofList
              ProjectReferences = 
                project.ReferencedProjects 
                |> List.choose (function
                  | FSharpReferencedProject.FSharpReference(options = opts) -> Some opts.ProjectFileName
                  | _ -> None)
                |> Array.ofList
              PackageReferences = 
                project.PackageReferences
                |> List.map (fun pkg ->
                  { Name = pkg.Name
                    Version = pkg.Version
                    FullPath = pkg.FullPath })
                |> Array.ofList
              FrameworkVersion = 
                project.TargetFramework |> Option.defaultValue "Unknown"
              TargetFramework = 
                project.TargetFramework |> Option.defaultValue "Unknown"
              OutputType = "Library" // TODO: extract from project
              OutputFile = project.OutputFile |> Option.defaultValue ""
              IsTestProject = None // TODO: detect test projects
              Properties = None }
          
          return Ok { Project = details }
        | None ->
          return Error $"Project not found: {request.Project}"
      with
      | ex ->
        logger.error $"Error getting project details: {ex.Message}"
        return Error ex.Message
    }

  /// Compile/build specific targets
  let compileTargets (request: CompileParams) : Task<Result<CompileResult, string>> =
    task {
      try
        // For now, return success - actual compilation would be implemented here
        logger.info $"Compiling {request.Targets.Length} targets"
        
        let result = 
          { OriginId = request.OriginId
            StatusCode = 0
            DataKind = None
            Data = None }
        
        return Ok result
      with
      | ex ->
        logger.error $"Error compiling targets: {ex.Message}"
        return Error ex.Message
    }

  /// Clean up resources
  let shutdown () =
    logger.info "Shutting down workspace operations"
    disposeWatchers()
    currentState <- 
      { WorkspaceLoader = None
        LoadedProjects = Map.empty
        RootPath = None
        FileWatchers = [] }