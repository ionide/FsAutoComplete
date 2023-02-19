module FsAutoComplete.Tests.DetectUnitTests

open Expecto
open System.IO
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open Helpers
open FsToolkit.ErrorHandling
open Helpers.Expecto.ShadowedTimeouts

let tests state =
  let geTestNotification projectFolder fileName =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", projectFolder)
      let! server, events = serverInitialize path defaultConfigDto state
      do! waitForWorkspaceFinishedParsing events
      let path = Path.Combine(path, fileName)
      let tdop: DidOpenTextDocumentParams = { TextDocument = loadDocument path }
      let! child = Async.StartChild(server.TextDocumentDidOpen tdop)
      let! _diagnostics = waitForParseResultsForFile fileName events
      do! child
      return! waitForTestDetected fileName events
    }
    |> Async.Cache

  testList
    "Find unit tests"
    [ testCaseAsync
        "Find nunit test"
        (async {
          let! testNotification = geTestNotification "NUnitTests" "UnitTest1.fs"
          Expect.hasLength testNotification.Tests 1 "Expected to have found 1 nunit test"
        })
      testCaseAsync
        "Find xunit test"
        (async {
          let! testNotification = geTestNotification "XUnitTests" "Tests.fs"
          Expect.hasLength testNotification.Tests 1 "Expected to have found 1 xunit test"
        })
      testCaseAsync
        "Find expecto tests"
        (async {
          let! testNotification = geTestNotification "ExpectoTests" "Sample.fs"
          Expect.hasLength testNotification.Tests 1 "Expected to have found 1 expecto test list"
          Expect.hasLength testNotification.Tests.[0].Childs 8 "Expected to have found 8 expecto tests"
        }) ]
