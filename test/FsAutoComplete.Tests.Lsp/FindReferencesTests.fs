module FsAutoComplete.Tests.FindReferences

open Expecto
open System.IO
open FsAutoComplete
open Helpers
open Ionide.LanguageServerProtocol.Types

let tests state =
  testList
    "Find All References tests"
    [ let server =
        async {
          let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "FindReferences")

          let! (server, event) = serverInitialize path defaultConfigDto state
          do! waitForWorkspaceFinishedParsing event
          let scriptPath = Path.Combine(path, "Script.fsx")

          let tdop: DidOpenTextDocumentParams = { TextDocument = loadDocument scriptPath }

          do! server.TextDocumentDidOpen tdop
          return server, scriptPath
        }
        |> Async.Cache

      ptestCaseAsync
        "Can find references for foo identifier in script"
        (async {
          let! server, scriptPath = server

          let request: ReferenceParams =
            { TextDocument = { Uri = Path.FilePathToUri scriptPath }
              Position = { Line = 2; Character = 0 } // beginning of the usage of the `foo` function
              Context = { IncludeDeclaration = true } } // beginning of the usage of the `foo` function

          let! response = server.TextDocumentReferences request

          match response with
          | Ok None -> failtestf "Should have gotten some references for this identifier"
          | Error e -> failtestf "Errored while getting references for identifier: %A" e
          | Ok (Some references) ->
            Expect.hasLength references 2 "Should have a reference for the definition and usage"
            let reference = references.[0]
            Expect.stringEnds reference.Uri (Path.GetFileName scriptPath) "should point to the same script"

            Expect.equal
              reference.Range
              { Start = { Line = 0; Character = 4 }
                End = { Line = 0; Character = 7 } }
              "should point to the definition of `foo`"
        }) ]
