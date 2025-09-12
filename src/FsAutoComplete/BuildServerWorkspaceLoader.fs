namespace FsAutoComplete.Lsp

open System
open System.IO
open System.Diagnostics
open System.Threading.Tasks
open System.Text
open FsAutoComplete.Logging
open Ionide.ProjInfo.ProjectSystem
open Ionide.ProjInfo.Types
open Ionide.ProjInfo

/// Build Server Protocol client for communicating with the separate build server process
module BuildServerClient =

  let logger = LogProvider.getLoggerByName "BuildServerClient"

  type BuildServerProcess = 
    { Process: Process
      Writer: StreamWriter
      Reader: StreamReader }

  let mutable private currentBuildServer: BuildServerProcess option = None

  /// Start the build server process
  let startBuildServer (buildServerPath: string) : Task<Result<BuildServerProcess, string>> =
    task {
      try
        logger.info (Log.setMessage "Starting build server")
        
        let startInfo = ProcessStartInfo()
        startInfo.FileName <- "dotnet"
        startInfo.Arguments <- buildServerPath
        startInfo.UseShellExecute <- false
        startInfo.RedirectStandardInput <- true
        startInfo.RedirectStandardOutput <- true
        startInfo.RedirectStandardError <- true
        startInfo.CreateNoWindow <- true
        
        let proc = Process.Start(startInfo)
        
        if isNull proc then
          return Error "Failed to start build server process"
        else
          let writer = new StreamWriter(proc.StandardInput.BaseStream, Encoding.UTF8)
          let reader = new StreamReader(proc.StandardOutput.BaseStream, Encoding.UTF8)
          
          let buildServer = 
            { Process = proc
              Writer = writer
              Reader = reader }
          
          currentBuildServer <- Some buildServer
          logger.info (Log.setMessage "Build server started successfully")
          return Ok buildServer
      with
      | ex ->
        logger.error (Log.setMessage "Failed to start build server" >> Log.addExn ex)
        return Error ex.Message
    }

  /// Send a message to the build server
  let sendMessage (buildServer: BuildServerProcess) (message: string) : Task<Result<string, string>> =
    task {
      try
        logger.debug (Log.setMessage "Sending message to build server")
        
        do! buildServer.Writer.WriteLineAsync(message)
        do! buildServer.Writer.FlushAsync()
        
        let! response = buildServer.Reader.ReadLineAsync()
        
        if isNull response then
          return Error "Build server returned null response"
        else
          logger.debug (Log.setMessage "Received response from build server")
          return Ok response
      with
      | ex ->
        logger.error (Log.setMessage "Failed to communicate with build server" >> Log.addExn ex)
        return Error ex.Message
    }

  /// Stop the build server process
  let stopBuildServer (buildServer: BuildServerProcess) : Task<unit> =
    task {
      try
        logger.info (Log.setMessage "Stopping build server")
        
        buildServer.Writer.Dispose()
        buildServer.Reader.Dispose()
        
        if not buildServer.Process.HasExited then
          buildServer.Process.Kill()
          let! _exited = buildServer.Process.WaitForExitAsync()
          ()
        
        buildServer.Process.Dispose()
        
        logger.info (Log.setMessage "Build server stopped")
      with
      | ex ->
        logger.error (Log.setMessage "Error stopping build server" >> Log.addExn ex)
    }

/// Factory function to create a Build Server workspace loader
module BuildServerWorkspaceLoaderFactory =
  
  let create (toolsPath) : IWorkspaceLoader =
    let logger = LogProvider.getLoggerByName "BuildServerWorkspaceLoaderFactory"
    logger.info (Log.setMessage "Creating BuildServerWorkspaceLoader - falling back to regular loader for now")
    // For now, fall back to the regular workspace loader until we implement the full BSP
    Ionide.ProjInfo.WorkspaceLoader.Create(toolsPath, [])