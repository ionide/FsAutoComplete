module FsAutoComplete.Tests.DetectUnitTests

open Expecto
open System.IO
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open Helpers
open FsToolkit.ErrorHandling
open Helpers.Expecto.ShadowedTimeouts
open Utils.Server

let tests state =
  let getTestNotification (prepareProject: bool) projectFolder fileName =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", projectFolder)

      let! server =
        if prepareProject then
          Server.create (Some path) defaultConfigDto state
        else
          Server.createForPreparedProjects (Some path) defaultConfigDto state

      let events = server.Events
      let path = Path.Combine(path, fileName)
      let tdop: DidOpenTextDocumentParams = { TextDocument = loadDocument path }
      let! child = Async.StartChild(server.Server.TextDocumentDidOpen tdop)
      let! _diagnostics = waitForParseResultsForFile fileName events
      do! child
      return! waitForTestDetected fileName events
    }
    |> Async.Cache

  testList
    "Find unit tests with code analysis"
    [ testSequenced
      <| testList
        "For VSTest projects"
        [ let getTestNotification = getTestNotification false

          testCaseAsync
            "Find nunit test"
            (async {
              let! testNotification = getTestNotification "NUnitTests" "UnitTest1.fs"
              Expect.hasLength testNotification.Tests 1 "Expected to have found 1 nunit test"

              Expect.equal
                testNotification.Tests[0].Childs[1].Childs[0].Name
                "Inner"
                "Expect nested module to be named Inner"

              Expect.equal
                testNotification.Tests[0].Childs[1].Childs[0].ModuleType
                "Module"
                "Expect nested module to be a module type"

              Expect.equal
                testNotification.Tests[0].Childs[1].Childs[1].Name
                "InnerClass"
                "Expect nested module to be named Inner"

              Expect.equal
                testNotification.Tests[0].Childs[1].Childs[1].ModuleType
                "TypeInModule"
                "Expect nested module to be a module type"
            })

          testCaseAsync
            "Find xunit test"
            (async {
              let! testNotification = getTestNotification "XUnitTests" "Tests.fs"
              Expect.hasLength testNotification.Tests 1 "Expected to have found 1 xunit test list"
              Expect.equal testNotification.Tests[0].ModuleType "Module" "Expected top list to be module"

              Expect.hasLength testNotification.Tests[0].Childs 3 "Expected to have found 3 child tests of top test"

              Expect.equal
                testNotification.Tests[0].Childs[0].ModuleType
                "NoneModule"
                "Expect My test to be none module type"

              Expect.equal
                testNotification.Tests[0].Childs[2].ModuleType
                "ModuleWithSuffix"
                "Expect Clashing test to be a module with suffix"
            })

          testCaseAsync
            "Find expecto tests"
            (async {
              let! testNotification = getTestNotification "ExpectoTests" "Sample.fs"
              Expect.hasLength testNotification.Tests 1 "Expected to have found 1 expecto test list"
              Expect.hasLength testNotification.Tests.[0].Childs 15 "Expected to have found 13 expecto tests"
            }) ]
      testSequenced
      <| testList
        "For MTP projects"
        [ let getTestNotification = getTestNotification true

          testCaseAsync
            "Find nunit test"
            (async {
              let! testNotification = getTestNotification "NUnitTests.MTP" "UnitTest1.fs"
              Expect.hasLength testNotification.Tests 1 "Expected to have found 1 nunit test"

              Expect.equal
                testNotification.Tests[0].Childs[1].Childs[0].Name
                "Inner"
                "Expect nested module to be named Inner"

              Expect.equal
                testNotification.Tests[0].Childs[1].Childs[0].ModuleType
                "Module"
                "Expect nested module to be a module type"

              Expect.equal
                testNotification.Tests[0].Childs[1].Childs[1].Name
                "InnerClass"
                "Expect nested module to be named Inner"

              Expect.equal
                testNotification.Tests[0].Childs[1].Childs[1].ModuleType
                "TypeInModule"
                "Expect nested module to be a module type"
            })

          testCaseAsync
            "Find xunit test"
            (async {
              let! testNotification = getTestNotification "XUnitTests.MTP" "Tests.fs"
              Expect.hasLength testNotification.Tests 1 "Expected to have found 1 xunit test list"
              Expect.equal testNotification.Tests[0].ModuleType "Module" "Expected top list to be module"

              Expect.hasLength testNotification.Tests[0].Childs 3 "Expected to have found 3 child tests of top test"

              Expect.equal
                testNotification.Tests[0].Childs[0].ModuleType
                "NoneModule"
                "Expect My test to be none module type"

              Expect.equal
                testNotification.Tests[0].Childs[2].ModuleType
                "ModuleWithSuffix"
                "Expect Clashing test to be a module with suffix"
            }) ] ]
