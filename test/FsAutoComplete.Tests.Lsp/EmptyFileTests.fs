module FsAutoComplete.Tests.EmptyFileTests

open Expecto
open System.IO
open Helpers
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete.Utils
open FsAutoComplete.Lsp
open FsToolkit.ErrorHandling
open Utils.Server
open Helpers.Expecto.ShadowedTimeouts

#nowarn "44" //we're testing so need to be able to use deprecated fields

let tests state =
  let createServer () =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "EmptyFileTests")

      let scriptPath = Path.Combine(path, "EmptyFile.fsx")

      let! (server, events) = serverInitialize path defaultConfigDto state

      do! waitForWorkspaceFinishedParsing events
      return server, events, scriptPath
    }
    |> Async.Cache

  let server1 = createServer ()
  let server2 = createServer ()

  testList
    "empty file features"
    [ testList
        "tests"
        [ testCaseAsync
            "no parsing/checking errors"
            (async {
              let! server, events, scriptPath = server1
              do! server.TextDocumentDidOpen { TextDocument = loadDocument scriptPath }

              match! waitForParseResultsForFile "EmptyFile.fsx" events with
              | Ok _ -> () // all good, no parsing/checking errors
              | Core.Result.Error errors -> failwithf "Errors while parsing script %s: %A" scriptPath errors
            })

          testCaseAsync
            "auto completion does not throw and is empty"
            (async {
              let! server, _, path = server1
              do! server.TextDocumentDidOpen { TextDocument = loadDocument path }

              let completionParams: CompletionParams =
                { TextDocument = { Uri = Path.FilePathToUri path }
                  Position = { Line = 0; Character = 0 }
                  Context =
                    Some
                      { triggerKind = CompletionTriggerKind.Invoked
                        triggerCharacter = None } }

              match! server.TextDocumentCompletion completionParams with
              | Ok(Some _) -> failtest "An empty file has empty completions"
              | Ok None -> ()
              | Error e -> failtestf "Got an error while retrieving completions: %A" e
            })
          testCaseAsync
            "type 'c' for checking error and autocompletion starts with 'async'"
            (async {
              let! server, events, scriptPath = server2
              do! server.TextDocumentDidOpen { TextDocument = loadDocument scriptPath }

              do!
                server.TextDocumentDidChange
                  { TextDocument =
                      { Uri = Path.FilePathToUri scriptPath
                        Version = 1 }
                    ContentChanges =
                      [| { Range =
                             Some
                               { Start = { Line = 0; Character = 0 }
                                 End = { Line = 0; Character = 0 } }
                           RangeLength = Some 0
                           Text = "c" } |] }

              let! completions =
                server.TextDocumentCompletion
                  { TextDocument = { Uri = Path.FilePathToUri scriptPath }
                    Position = { Line = 0; Character = 1 }
                    Context =
                      Some
                        { triggerKind = CompletionTriggerKind.Invoked
                          triggerCharacter = None } }
                |> Async.StartChild

              let! compilerResults = waitForCompilerDiagnosticsForFile "EmptyFile.fsx" events |> Async.StartChild

              match! compilerResults with
              | Ok() -> failtest "should get an F# compiler checking error from a 'c' by itself"
              | Core.Result.Error errors ->
                Expect.hasLength errors 1 "should have only an error FS0039: identifier not defined"

                Expect.exists
                  errors
                  (fun error -> error.Code = Some "39")
                  "should have an error FS0039: identifier not defined"

              match! completions with
              | Ok(Some completions) ->
                Expect.isGreaterThan
                  completions.Items.Length
                  30
                  "should have a complete completion list all containing c"

                let firstItem = completions.Items.[0]
                Expect.equal firstItem.Label "async" "first member should be async"
              | Ok None -> failtest "Should have gotten some completion items"
              | Error e -> failtestf "Got an error while retrieving completions: %A" e
            }) ] ]
