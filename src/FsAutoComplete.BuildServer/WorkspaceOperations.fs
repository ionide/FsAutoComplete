namespace FsAutoComplete.BuildServer

open System
open System.IO
open System.Threading.Tasks
open FsAutoComplete.Logging
open FsAutoComplete.BuildServerProtocol.JsonRpc
open FsAutoComplete.BuildServerProtocol.BuildServerProtocol

/// Simple workspace operations for Build Server Protocol
module WorkspaceOperations =

  let private logger = LogProvider.getLoggerByName "WorkspaceOperations"

  /// Initialize workspace - for now just log and return success
  let initializeWorkspace () =
    logger.info (Log.setMessage "Initializing workspace...")
    Task.FromResult(Result.Ok())

  /// Peek workspace - simplified for now
  let peekWorkspace (request: WorkspacePeekRequest) =
    logger.info (
      Log.setMessage "Peeking workspace at {directory}"
      >> Log.addContext "directory" request.Directory
    )

    let response = { Found = [||] }
    Task.FromResult(Result.Ok response)

  /// Load workspace - simplified for now
  let loadWorkspace (request: WorkspaceLoadRequest) =
    logger.info (
      Log.setMessage "Loading workspace with {documentCount} documents"
      >> Log.addContext "documentCount" request.TextDocuments.Length
    )

    let response =
      { WorkspaceRoot = Environment.CurrentDirectory
        Projects = [||] }

    Task.FromResult(Result.Ok response)

  /// Shutdown workspace
  let shutdown () =
    logger.info (Log.setMessage "Shutting down workspace...")
    Task.FromResult(Result.Ok())
