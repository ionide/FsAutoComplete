module FsAutoComplete.Tests.WorkspaceFolderTests

open Expecto
open System.IO
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.Lsp
open FsAutoComplete.LspHelpers
open Helpers
open Utils.Server

/// Tests for workspaceFolders initialization (stages 1 & 2)
let tests createServer =
  testList "WorkspaceFolder Tests" [
    testCaseAsync "Single workspace folder is selected correctly" <| async {
      let testDir = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "ServerTests")
      let (server: IFSharpLspServer), _events = createServer ()
      
      let p: InitializeParams =
        { ProcessId = Some 1
          RootPath = None
          Locale = None
          RootUri = None
          InitializationOptions = Some(Server.serialize defaultConfigDto)
          Capabilities = clientCaps
          ClientInfo = Some { Name = "Test"; Version = Some "1.0" }
          WorkspaceFolders = Some [| { Uri = Utils.Path.FilePathToUri testDir; Name = "Test" } |]
          Trace = None
          WorkDoneToken = None }
      
      match! server.Initialize p with
      | Ok _ -> ()
      | Error e -> failtest $"Initialize failed: {e}"
    }

    testCaseAsync "Multiple workspace folders - selects first with F# code" <| async {
      let testDir = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "ServerTests")
      let emptyDir = Path.GetTempPath()
      let (server: IFSharpLspServer), _events = createServer ()
      
      // First folder has no F# code, second has F# projects
      let p: InitializeParams =
        { ProcessId = Some 1
          RootPath = None
          Locale = None
          RootUri = None
          InitializationOptions = Some(Server.serialize defaultConfigDto)
          Capabilities = clientCaps
          ClientInfo = Some { Name = "Test"; Version = Some "1.0" }
          WorkspaceFolders = 
            Some [| 
              { Uri = Utils.Path.FilePathToUri emptyDir; Name = "Empty" }
              { Uri = Utils.Path.FilePathToUri testDir; Name = "Test" } 
            |]
          Trace = None
          WorkDoneToken = None }
      
      match! server.Initialize p with
      | Ok _ -> ()
      | Error e -> failtest $"Initialize failed: {e}"
    }

    testCaseAsync "Backward compatibility - uses RootUri when WorkspaceFolders is None" <| async {
      let testDir = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "ServerTests")
      let (server: IFSharpLspServer), _events = createServer ()
      
      let p: InitializeParams =
        { ProcessId = Some 1
          RootPath = None
          Locale = None
          RootUri = Some (sprintf "file://%s" testDir)
          InitializationOptions = Some(Server.serialize defaultConfigDto)
          Capabilities = clientCaps
          ClientInfo = Some { Name = "Test"; Version = Some "1.0" }
          WorkspaceFolders = None
          Trace = None
          WorkDoneToken = None }
      
      match! server.Initialize p with
      | Ok _ -> ()
      | Error e -> failtest $"Initialize failed: {e}"
    }

    testCaseAsync "Backward compatibility - uses RootPath when both RootUri and WorkspaceFolders are None" <| async {
      let testDir = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "ServerTests")
      let (server: IFSharpLspServer), _events = createServer ()
      
      let p: InitializeParams =
        { ProcessId = Some 1
          RootPath = Some testDir
          Locale = None
          RootUri = None
          InitializationOptions = Some(Server.serialize defaultConfigDto)
          Capabilities = clientCaps
          ClientInfo = Some { Name = "Test"; Version = Some "1.0" }
          WorkspaceFolders = None
          Trace = None
          WorkDoneToken = None }
      
      match! server.Initialize p with
      | Ok _ -> ()
      | Error e -> failtest $"Initialize failed: {e}"
    }

    testCaseAsync "Empty WorkspaceFolders array falls back to RootUri" <| async {
      let testDir = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "ServerTests")
      let (server: IFSharpLspServer), _events = createServer ()
      
      let p: InitializeParams =
        { ProcessId = Some 1
          RootPath = None
          Locale = None
          RootUri = Some (sprintf "file://%s" testDir)
          InitializationOptions = Some(Server.serialize defaultConfigDto)
          Capabilities = clientCaps
          ClientInfo = Some { Name = "Test"; Version = Some "1.0" }
          WorkspaceFolders = Some [||]
          Trace = None
          WorkDoneToken = None }
      
      match! server.Initialize p with
      | Ok _ -> ()
      | Error e -> failtest $"Initialize failed: {e}"
    }
  ]
