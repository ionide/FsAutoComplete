module FsAutoComplete.Tests.ScriptTest

open System
open Expecto
open System.IO
open LanguageServerProtocol
open LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open Helpers

let scriptPreviewTests =
  let serverStart = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "PreviewScriptFeatures")
    let scriptPath = Path.Combine(path, "Script.fsx")
    let (server, events) = serverInitialize path { defaultConfigDto with FSIExtraParameters = Some [| "--langversion:preview" |] }
    do waitForWorkspaceFinishedParsing events
    server, events, scriptPath
  )
  let serverTest f () = f serverStart.Value

  ptestList "script preview language features" [
    testCase "can typecheck scripts when preview features are used" (serverTest (fun (server, events, scriptPath) ->
      do server.TextDocumentDidOpen { TextDocument = loadDocument scriptPath } |> Async.RunSynchronously
      match waitForParseResultsForFile "Script.fsx" events with
      | Ok () ->
        () // all good, no parsing/checking errors
      | Core.Result.Error errors ->
        failwithf "Errors while parsing script %s: %A" scriptPath errors
    ))
  ]

let scriptEvictionTests =
  let serverStart = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "PreviewScriptFeatures")
    let scriptPath = Path.Combine(path, "Script.fsx")
    let (server, events) = serverInitialize path defaultConfigDto
    do waitForWorkspaceFinishedParsing events
    server, events, scriptPath
  )
  let serverTest f () = f serverStart.Value

  ptestList "script eviction tests" [
    testCase "can update script typechecking when arguments change" (serverTest (fun (server, events, scriptPath) ->
      let openScript () = do server.TextDocumentDidOpen { TextDocument = loadDocument scriptPath } |> Async.RunSynchronously

      openScript ()
      match waitForParseResultsForFile "Script.fsx" events with
      | Ok () ->
        failwithf "Expected errors before we trigger script changes"
      | Core.Result.Error errors ->
        ()

      let configChange: DidChangeConfigurationParams =
        let config : FSharpConfigRequest =
          { FSharp = { defaultConfigDto with FSIExtraParameters = Some [| "--langversion:preview" |] } }
        { Settings = Server.serialize config }
      do server.WorkspaceDidChangeConfiguration configChange |> Async.RunSynchronously
      do waitForParseResultsForFile "Script.fsx" events |> ignore // errors returned by background checking, not sure why it worked before...

      openScript ()
      match waitForParseResultsForFile "Script.fsx" events with
      | Ok () ->
        ()
      | Core.Result.Error errors ->
        failwithf "Should be no typecheck errors after we set the preview argument"
    ))
  ]


let dependencyManagerTests =
  let serverStart useCorrectPaths =
    let workingDir = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "DependencyManagement")
    let dependencyManagerAssemblyDir = Path.Combine(__SOURCE_DIRECTORY__, "..", "FsAutoComplete.DependencyManager.Dummy", "bin", "Debug", "netstandard2.0")
    let dependencyManagerEnabledConfig =
      { defaultConfigDto with
          FSIExtraParameters = Some [| "--langversion:preview" |]
          FSICompilerToolLocations = Some [| if useCorrectPaths then dependencyManagerAssemblyDir |] }
    let (server, events) = serverInitialize workingDir dependencyManagerEnabledConfig
    let scriptPath = Path.Combine(workingDir, "Script.fsx")
    do waitForWorkspaceFinishedParsing events
    do server.TextDocumentDidOpen { TextDocument = loadDocument scriptPath } |> Async.RunSynchronously
    server, events, workingDir, scriptPath

  let serverTest correctManagerPaths f = fun () -> f (serverStart correctManagerPaths)

  testList "dependencyManager integrations" [
    testCase "can typecheck script that depends on #r dummy dependency manager" (serverTest true (fun (server, events, workingDir, testFilePath) ->
      do server.TextDocumentDidOpen { TextDocument = loadDocument testFilePath } |> Async.RunSynchronously
      match waitForParseResultsForFile "Script.fsx" events with
      | Ok _ -> ()
      | Core.Result.Error e ->
        failwithf "Error during typechecking: %A" e
    ))
    testCase "fails to typecheck script when dependency manager is missing" (serverTest false (fun (server, events, workingDir, testFilePath) ->
      do server.TextDocumentDidOpen { TextDocument = loadDocument testFilePath } |> Async.RunSynchronously
      match waitForParseResultsForFile "Script.fsx" events with
      | Ok _ ->
        failwith "Expected to fail typechecking a script with a dependency manager that's missing"
      | Core.Result.Error e ->
        match e with
        | [| { Code = Some "3216" } |] -> () // this is the error code that signals a missing dependency manager, so this is a 'success'
        | e -> failwithf "Unexpected error during typechecking: %A" e
    ))
  ]



let scriptProjectOptionsCacheTests =
  let serverStart () =
    let workingDir = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "ScriptProjectOptsCache")
    let previewEnabledConfig =
      { defaultConfigDto with
          FSIExtraParameters = Some [| "--langversion:preview" |] }
    let (server, events) = serverInitialize workingDir previewEnabledConfig
    let scriptPath = Path.Combine(workingDir, "Script.fsx")
    do waitForWorkspaceFinishedParsing events
    server, events, workingDir, scriptPath

  let serverTest f = fun () -> f (serverStart ())

  testList "ScriptProjectOptionsCache" [
    testCase "reopening the script file should return same project options for file" (serverTest (fun (server, events, workingDir, testFilePath) ->
      waitForScriptFilePropjectOptions server
      do server.TextDocumentDidOpen { TextDocument = loadDocument testFilePath } |> Async.RunSynchronously
      do System.Threading.Thread.Sleep 3000
      do server.TextDocumentDidOpen { TextDocument = loadDocument testFilePath } |> Async.RunSynchronously
      do System.Threading.Thread.Sleep 3000

      let opts1 = projectOptsList.[0]
      let opts2 = projectOptsList.[1]

      Expect.equal opts1 opts2 "Project opts should be eqaul"
    ))
  ]
