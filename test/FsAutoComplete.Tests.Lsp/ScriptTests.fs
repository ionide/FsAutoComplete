module FsAutoComplete.Tests.ScriptTest

open Expecto
open System.IO
open LanguageServerProtocol
open LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open Helpers

let scriptPreviewTests toolsPath =
  let serverStart = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "PreviewScriptFeatures")
    let scriptPath = Path.Combine(path, "Script.fsx")
    let (server, events) = serverInitialize path { defaultConfigDto with FSIExtraParameters = Some [| "--langversion:preview" |] } toolsPath
    do waitForWorkspaceFinishedParsing events
    server, events, scriptPath
  )
  let serverTest f () = f serverStart.Value

  testList "script features" [
    testCase "can typecheck scripts when preview features are used" (serverTest (fun (server, events, scriptPath) ->
      do server.TextDocumentDidOpen { TextDocument = loadDocument scriptPath } |> Async.RunSynchronously
      match waitForParseResultsForFile "Script.fsx" events with
      | Ok () ->
        () // all good, no parsing/checking errors
      | Core.Result.Error errors ->
        failwithf "Errors while parsing script %s: %A" scriptPath errors
    ))
  ]

let scriptEvictionTests toolsPath =
  let serverStart = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "PreviewScriptFeatures")
    let scriptPath = Path.Combine(path, "Script.fsx")
    let (server, events) = serverInitialize path defaultConfigDto toolsPath
    do waitForWorkspaceFinishedParsing events
    server, events, scriptPath
  )
  let serverTest f () = f serverStart.Value

  testList "script eviction tests" [
    ptestCase "can update script typechecking when arguments change" (serverTest (fun (server, events, scriptPath) ->
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


let dependencyManagerTests toolsPath =
  let serverStart = lazy (
    let workingDir = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "DependencyManagement")
    let dependencyManagerAssemblyDir = Path.Combine(__SOURCE_DIRECTORY__, "..", "FsAutoComplete.DependencyManager.Dummy", "bin", "Debug", "netstandard2.0")
    let dependencyManagerEnabledConfig =
      { defaultConfigDto with
          FSIExtraParameters = Some [| "--langversion:preview" |] }
    let (server, events) = serverInitialize workingDir dependencyManagerEnabledConfig toolsPath
    do waitForWorkspaceFinishedParsing events
    server, events, workingDir
  )

  let serverTest f = fun () -> f serverStart.Value

  testList "dependencyManager integrations" [
    testCase "can typecheck script that depends on #r dummy dependency manager" (serverTest (fun (server, events, workingDir) ->
      let scriptName = "DepManagerPresentScript.fsx"
      let scriptPath = Path.Combine(workingDir, scriptName)
      do server.TextDocumentDidOpen { TextDocument = loadDocument scriptPath } |> Async.RunSynchronously
      match waitForParseResultsForFile scriptName events with
      | Ok _ -> ()
      | Core.Result.Error e ->
        failwithf "Error during typechecking: %A" e
    ))

    testCase "fails to typecheck script when dependency manager is missing" (serverTest (fun (server, events, workingDir) ->
      let scriptName = "DepManagerAbsentScript.fsx"
      let scriptPath = Path.Combine(workingDir, scriptName)
      do server.TextDocumentDidOpen { TextDocument = loadDocument scriptPath } |> Async.RunSynchronously

      match waitForParseResultsForFile scriptName events with
      | Ok _ ->
        failwith "Expected to fail typechecking a script with a dependency manager that's missing"
      | Core.Result.Error e ->
        match e with
        | [| { Code = Some "3400" }; _ |] -> () // this is the error code that signals a missing dependency manager, so this is a 'success'
        | e -> failwithf "Unexpected error during typechecking: %A" e
    ))
  ]

let scriptProjectOptionsCacheTests toolsPath =
  let serverStart () =
    let workingDir = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "ScriptProjectOptsCache")
    let previewEnabledConfig =
      { defaultConfigDto with
          FSIExtraParameters = Some [| "--langversion:preview" |] }
    let (server, events) = serverInitialize workingDir previewEnabledConfig toolsPath
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

let scriptGotoTests toolsPath =
  let serverStart = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "GoToTests")

    let (server, event) = serverInitialize path defaultConfigDto toolsPath
    do waitForWorkspaceFinishedParsing event

    let scriptPath = Path.Combine(path, "Script.fsx")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument scriptPath }
    do server.TextDocumentDidOpen tdop |> Async.RunSynchronously

    (server, scriptPath)
  )
  let serverTest f () =
    let (server, path) = serverStart.Value
    f server path

  testSequenced <| testList "Script GoTo Tests" [
    testCase "Go-to-definition on #load integration test" (serverTest (fun server scriptPath ->
      let p : TextDocumentPositionParams = {
        TextDocument = { Uri = Path.FilePathToUri scriptPath }
        Position = { Line = 0; Character = 10 }
      }
      let res = server.TextDocumentDefinition p |> Async.RunSynchronously
      match res with
      | Error e -> failtestf "Request failed: %A" e
      | Ok None -> failtest "Request none"
      | Ok (Some (GotoResult.Multiple _)) -> failtest "Should only get one location"
      | Ok (Some (GotoResult.Single r)) ->
        Expect.stringEnds r.Uri "/simple.fsx" "should navigate to the mentioned script file"
        ()
    ))
  ]
