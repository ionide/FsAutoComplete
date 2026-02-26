module FsAutoComplete.Tests.InfoPanelTests

open Expecto
open System.IO
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open Helpers
open FsToolkit.ErrorHandling
open Helpers.Expecto.ShadowedTimeouts

let docFormattingTest state =
  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "FormattedDocumentation")
      let config = defaultConfigDto
      let! (server, events) = serverInitialize path config state
      let path = Path.Combine(path, "Script.fsx")
      let tdop: DidOpenTextDocumentParams = { TextDocument = loadDocument path }
      do! server.TextDocumentDidOpen tdop

      do!
        waitForParseResultsForFile "Script.fsx" events
        |> AsyncResult.bimap id (fun _e -> failtest "should have not had check errors")

      return (server, path)
    }
    |> Async.Cache

  testList
    "Generic Parameter Format Tests"
    [ testCaseAsync
        "Two params have a separator"
        (async {
          let! (server, path) = server

          let! doc =
            server.FSharpDocumentation
              { TextDocument = { Uri = path }
                Position = { Character = 5u; Line = 0u } } // Map.map

          match doc with
          | Result.Error err -> failtest $"Doc error: {err.Message}"
          | Result.Ok(Some(As(model: FsAutoComplete.CommandResponse.DocumentationDescription))) ->
            Expect.stringContains model.Signature "'Key, 'U" "Formatted doc contains both params separated by (, )"
          | Result.Ok _ -> failtest "couldn't parse doc as the json type we expected"
        })

      testCaseAsync
        "Tupled params have only asterisk"
        (async {
          let! (server, path) = server

          let! doc =
            server.FSharpDocumentation
              { TextDocument = { Uri = path }
                Position = { Character = 7u; Line = 1u } } // List.unzip3

          match doc with
          | Result.Error err -> failtest $"Doc error: {err.Message}"
          | Result.Ok(Some(As(model: FsAutoComplete.CommandResponse.DocumentationDescription))) ->
            Expect.stringContains model.Signature "'T1 * 'T2 * 'T3" "Formatted doc contains 3 params separated by ( * )"
          | Result.Ok _ -> failtest "couldn't parse doc as the json type we expected"
        })

      testCaseAsync
        "Delegate type shows clean 'delegate of <args> -> <ret>' signature (issue #627)"
        (async {
          let! (server, path) = server

          // Line 3 (0-indexed), char 5: hovering on "DelegateWithArgs" type name
          let! doc =
            server.FSharpDocumentation
              { TextDocument = { Uri = path }
                Position = { Character = 5u; Line = 3u } }

          match doc with
          | Result.Error err -> failtest $"Doc error: {err.Message}"
          | Result.Ok(Some(As(model: FsAutoComplete.CommandResponse.DocumentationDescription))) ->
            Expect.stringContains model.Signature "delegate of" "Delegate signature should contain 'delegate of'"
            Expect.stringContains model.Signature "name" "Delegate signature should contain parameter name 'name'"
            Expect.stringContains model.Signature "count" "Delegate signature should contain parameter name 'count'"
          | Result.Ok _ -> failtest "couldn't parse doc as the json type we expected"
        }) ]
