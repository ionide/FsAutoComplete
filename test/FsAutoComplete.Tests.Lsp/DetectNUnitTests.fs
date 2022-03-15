module FsAutoComplete.Tests.DetectNUnitTests

open Expecto
open System.IO
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open Helpers
open FsToolkit.ErrorHandling

let tests state =
  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "NUnitTests")

      let! server, events = serverInitialize path defaultConfigDto state
      do! waitForWorkspaceFinishedParsing events
      let fileName = "UnitTest1.fs"
      let path = Path.Combine(path, fileName)
      let tdop: DidOpenTextDocumentParams = { TextDocument = loadDocument path }
      do! server.TextDocumentDidOpen tdop

      let! _diagnostics = waitForParseResultsForFile fileName events

      return! waitForTestDetected fileName events
    }
    |> Async.Cache

  testList
    "Find nunit tests"
    [ testCaseAsync
        "Find nunit test as function"
        (async {
          let! testNotification = server
          Expect.hasLength testNotification.Tests 1 "Expected to have found 1 nunit test"
        }) ]
