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

let scriptPreviewTests state =
  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "PreviewScriptFeatures")
      let scriptPath = Path.Combine(path, "Script.fsx")
      let! (server, events) = serverInitialize path { defaultConfigDto with FSIExtraParameters = Some [| "--langversion:preview" |] } state
      do! waitForWorkspaceFinishedParsing events
      return server, events, scriptPath
    }
    |> Async.Cache

  testSequenced <|
    testList "script features" [
      testList "tests" [
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
      testCaseAsync "cleanup" (async {
        let! server, _, _ = server
        do! server.Shutdown()
      })
    ]

let scriptEvictionTests state =
  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "ScriptEviction")
      let scriptPath = Path.Combine(path, "Script.fsx")
      let! (server, events) = serverInitialize path defaultConfigDto state
      do! waitForWorkspaceFinishedParsing events
      return server, events, scriptPath
    }
    |> Async.Cache

  testSequenced <|
    testList "script eviction tests" [
      testList "tests" [
        testCaseAsync "can update script typechecking when arguments change" (async {
          let! server, events, scriptPath = server
          let openScript () = server.TextDocumentDidOpen { TextDocument = loadDocument scriptPath }

          do! openScript ()
          do! waitForParseResultsForFile "Script.fsx" events |> AsyncResult.bimap (fun _ -> failtest "Expected errors before we trigger script changes") ignore

          let configChange: DidChangeConfigurationParams =
            let config : FSharpConfigRequest =
              { FSharp = { defaultConfigDto with FSIExtraParameters = Some [| "--nowarn:760" |] } }
            { Settings = Server.serialize config }
          do! server.WorkspaceDidChangeConfiguration configChange
          do! openScript ()
          // skipping 2 to get past the default workspace and the explicit opens
          do!
            fileDiagnostics "Script.fsx" events
            |> Observable.skip 2
            |> diagnosticsToResult
            |> Async.AwaitObservable
            |> AsyncResult.bimap ignore (fun errors -> Expect.isEmpty errors "Should be no typecheck errors after we set the preview argument")
        })
      ]
      testCaseAsync "cleanup" (async {
        let! server, _, _ = server
        do! server.Shutdown()
      })
    ]

let dependencyManagerTests state =
  let server =
    async {
      let workingDir = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "DependencyManagement")
      let dependencyManagerAssemblyDir = Path.Combine(__SOURCE_DIRECTORY__, "..", "FsAutoComplete.DependencyManager.Dummy", "bin", "Debug", "netstandard2.0")
      let dependencyManagerEnabledConfig =
        { defaultConfigDto with
            FSIExtraParameters = Some [| "--langversion:preview" |] }
      let! (server, events) = serverInitialize workingDir dependencyManagerEnabledConfig state
      do! waitForWorkspaceFinishedParsing events
      return server, events, workingDir
    }
    |> Async.Cache

  testSequenced <|
    testList "dependencyManager integrations" [
      testList "tests" [
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
            | [| { Code = Some "3216" }; _ |] -> () // this is the error code that signals a missing dependency manager, so this is a 'success'
            | e -> failwithf "Unexpected error during typechecking: %A" e
        })
      ]
      testCaseAsync "cleanup" (async {
        let! server, _, _ = server
        do! server.Shutdown()
      })
    ]

let scriptProjectOptionsCacheTests state =
  let server = async {
    let workingDir = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "ScriptProjectOptsCache")
    let previewEnabledConfig =
      { defaultConfigDto with
          FSIExtraParameters = Some [| "--langversion:preview" |] }
    let! (server, events) = serverInitialize workingDir previewEnabledConfig state
    let options = ResizeArray()
    server.ScriptFileProjectOptions.Add(options.Add)
    let scriptPath = Path.Combine(workingDir, "Script.fsx")
    do! waitForWorkspaceFinishedParsing events
    return server, events, workingDir, scriptPath, options
  }

  testSequenced <| testList "ScriptProjectOptionsCache" [
    testList "tests" [
      testCaseAsync "reopening the script file should return same project options for file" (async {
        let! server, events, workingDir, testFilePath, allOpts = server

        do! server.TextDocumentDidOpen { TextDocument = loadDocument testFilePath }
        do! Async.Sleep (TimeSpan.FromSeconds 3.)
        do! server.TextDocumentDidOpen { TextDocument = loadDocument testFilePath }
        do! Async.Sleep (TimeSpan.FromSeconds 3.)
        Expect.hasLength allOpts 2 "should only have two events"
        Expect.equal allOpts.[0] allOpts.[1] "Project opts should be eqaul"
      })
      testCaseAsync "cleanup" (async {
        let! server, _, _, _, _ = server
        do! server.Shutdown()
      })
    ]
  ]
