module FsAutoComplete.Tests.ScriptTest

open Expecto
open System.IO
open LanguageServerProtocol
open LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open Helpers
open FsToolkit.ErrorHandling
open FSharpx.Control.Observable
open FSharp.Control.Reactive
open System

let scriptPreviewTests toolsPath workspaceLoaderFactory =
  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "PreviewScriptFeatures")
      let scriptPath = Path.Combine(path, "Script.fsx")
      let! (server, events) = serverInitialize path { defaultConfigDto with FSIExtraParameters = Some [| "--langversion:preview" |] } toolsPath workspaceLoaderFactory
      do! waitForWorkspaceFinishedParsing events
      return server, events, scriptPath
    }
    |> Async.Cache

  testList "script features" [
    testCaseAsync "can typecheck scripts when preview features are used" (async {
      let! server, events, scriptPath = server
      do! server.TextDocumentDidOpen { TextDocument = loadDocument scriptPath }
      match! waitForParseResultsForFile "Script.fsx" events with
      | Ok () ->
        () // all good, no parsing/checking errors
      | Core.Result.Error errors ->
        failwithf "Errors while parsing script %s: %A" scriptPath errors
    })
  ]

let scriptEvictionTests toolsPath workspaceLoaderFactory =
  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "ScriptEviction")
      let scriptPath = Path.Combine(path, "Script.fsx")
      let! (server, events) = serverInitialize path defaultConfigDto toolsPath workspaceLoaderFactory
      do! waitForWorkspaceFinishedParsing events
      return server, events, scriptPath
    }
    |> Async.Cache

  testList "script eviction tests" [
    testCaseAsync "can update script typechecking when arguments change" (async {
      let! server, events, scriptPath = server
      let openScript () = server.TextDocumentDidOpen { TextDocument = loadDocument scriptPath }

      do! openScript ()
      match! waitForParseResultsForFile "Script.fsx" events with
      | Ok () ->
        failwithf "Expected errors before we trigger script changes"
      | Core.Result.Error errors ->
        ()

      let configChange: DidChangeConfigurationParams =
        let config : FSharpConfigRequest =
          { FSharp = { defaultConfigDto with FSIExtraParameters = Some [| "--nowarn:760" |] } }
        { Settings = Server.serialize config }
      do! server.WorkspaceDidChangeConfiguration configChange
      do! openScript ()
      // skipping 2 to get past the default workspace and the explicit opens
      match! fileDiagnostics "Script.fsx" events |> Observable.skip 2 |> Async.AwaitObservable with
      | _name, { Diagnostics = [||] } ->
        ()
      | _, { Diagnostics = errors} ->
        Expect.isEmpty errors "Should be no typecheck errors after we set the preview argument"
    })
  ]

let dependencyManagerTests toolsPath workspaceLoaderFactory =
  let server =
    async {
      let workingDir = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "DependencyManagement")
      let dependencyManagerAssemblyDir = Path.Combine(__SOURCE_DIRECTORY__, "..", "FsAutoComplete.DependencyManager.Dummy", "bin", "Debug", "netstandard2.0")
      let dependencyManagerEnabledConfig =
        { defaultConfigDto with
            FSIExtraParameters = Some [| "--langversion:preview" |] }
      let! (server, events) = serverInitialize workingDir dependencyManagerEnabledConfig toolsPath workspaceLoaderFactory
      do! waitForWorkspaceFinishedParsing events
      return server, events, workingDir
    }
    |> Async.Cache

  testList "dependencyManager integrations" [
    testCaseAsync "can typecheck script that depends on #r dummy dependency manager" (async {
      let! server, events, workingDir = server
      let scriptName = "DepManagerPresentScript.fsx"
      let scriptPath = Path.Combine(workingDir, scriptName)
      do! server.TextDocumentDidOpen { TextDocument = loadDocument scriptPath }
      match! waitForParseResultsForFile scriptName events with
      | Ok _ -> ()
      | Core.Result.Error e ->
        failwithf "Error during typechecking: %A" e
    })

    testCaseAsync "fails to typecheck script when dependency manager is missing" (async {
      let! server, events, workingDir = server
      let scriptName = "DepManagerAbsentScript.fsx"
      let scriptPath = Path.Combine(workingDir, scriptName)
      do! server.TextDocumentDidOpen { TextDocument = loadDocument scriptPath }

      match! waitForParseResultsForFile scriptName events with
      | Ok _ ->
        failwith "Expected to fail typechecking a script with a dependency manager that's missing"
      | Core.Result.Error e ->
        match e with
        | [| { Code = Some "3400" }; _ |] -> () // this is the error code that signals a missing dependency manager, so this is a 'success'
        | e -> failwithf "Unexpected error during typechecking: %A" e
    })
  ]

let scriptProjectOptionsCacheTests toolsPath workspaceLoaderFactory =
  let server = async {
    let workingDir = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "ScriptProjectOptsCache")
    let previewEnabledConfig =
      { defaultConfigDto with
          FSIExtraParameters = Some [| "--langversion:preview" |] }
    let! (server, events) = serverInitialize workingDir previewEnabledConfig toolsPath workspaceLoaderFactory
    let options = ResizeArray()
    server.ScriptFileProjectOptions.Add(options.Add)
    let scriptPath = Path.Combine(workingDir, "Script.fsx")
    do! waitForWorkspaceFinishedParsing events
    return server, events, workingDir, scriptPath, options
  }

  testList "ScriptProjectOptionsCache" [
    testCaseAsync "reopening the script file should return same project options for file" (async {
      let! server, events, workingDir, testFilePath, allOpts = server

      do! server.TextDocumentDidOpen { TextDocument = loadDocument testFilePath }
      do! Async.Sleep (TimeSpan.FromSeconds 3.)
      do! server.TextDocumentDidOpen { TextDocument = loadDocument testFilePath }
      do! Async.Sleep (TimeSpan.FromSeconds 3.)
      Expect.hasLength allOpts 2 "should only have two events"
      Expect.equal allOpts.[0] allOpts.[1] "Project opts should be eqaul"
    })
  ]

let scriptGotoTests toolsPath workspaceLoaderFactory =
  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "GoToTests")
      let! (server, event) = serverInitialize path defaultConfigDto toolsPath workspaceLoaderFactory
      do! waitForWorkspaceFinishedParsing event
      let scriptPath = Path.Combine(path, "Script.fsx")
      let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument scriptPath }
      do! server.TextDocumentDidOpen tdop
      return server, scriptPath
    }

  testSequenced <| testList "Script GoTo Tests" [
    testCaseAsync "Go-to-definition on #load integration test" (async {
      let! server, scriptPath = server
      let p : TextDocumentPositionParams = {
        TextDocument = { Uri = Path.FilePathToUri scriptPath }
        Position = { Line = 0; Character = 10 }
      }
      let! res = server.TextDocumentDefinition p
      match res with
      | Error e -> failtestf "Request failed: %A" e
      | Ok None -> failtest "Request none"
      | Ok (Some (GotoResult.Multiple _)) -> failtest "Should only get one location"
      | Ok (Some (GotoResult.Single r)) ->
        Expect.stringEnds r.Uri "/simple.fsx" "should navigate to the mentioned script file"
        ()
    })
  ]
