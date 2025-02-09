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
open FsAutoComplete.LspHelpers

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

  testSequenced
  <| testList
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
                  Position = { Line = 0u; Character = 0u }
                  Context =
                    Some
                      { TriggerKind = CompletionTriggerKind.Invoked
                        TriggerCharacter = None }
                  WorkDoneToken = None
                  PartialResultToken = None }

              match! server.TextDocumentCompletion completionParams with
              | Ok(Some _) -> failtest "An empty file has empty completions"
              | Ok None -> ()
              | Error e -> failtestf "Got an error while retrieving completions: %A" e
            })
          testCaseAsync
            "type 'c' for checking error and autocompletion starts with 'abs'"
            (async {
              let! server, events, scriptPath = server2
              do! server.TextDocumentDidOpen { TextDocument = loadDocument scriptPath }

              do!
                server.TextDocumentDidChange
                  { TextDocument =
                      { Uri = Path.FilePathToUri scriptPath
                        Version = 1 }
                    ContentChanges =
                      [| U2.C1
                           { Range =
                               { Start = { Line = 0u; Character = 0u }
                                 End = { Line = 0u; Character = 0u } }
                             RangeLength = Some 0u
                             Text = "c" } |] }

              let! completions =
                server.TextDocumentCompletion
                  { TextDocument = { Uri = Path.FilePathToUri scriptPath }
                    Position = { Line = 0u; Character = 1u }
                    Context =
                      Some
                        { TriggerKind = CompletionTriggerKind.Invoked
                          TriggerCharacter = None }
                    WorkDoneToken = None
                    PartialResultToken = None }
                |> Async.StartChild

              let! compilerResults = waitForCompilerDiagnosticsForFile "EmptyFile.fsx" events |> Async.StartChild

              match! compilerResults with
              | Ok() -> failtest "should get an F# compiler checking error from a 'c' by itself"
              | Core.Result.Error errors ->
                Expect.hasLength errors 1 "should have only an error FS0039: identifier not defined"

                Expect.exists
                  errors
                  (fun error -> error.CodeAsString = Some "39")
                  $"should have an error FS0039: identifier not defined %A{errors}"

              match! completions with
              | Ok(Some completions) ->
                Expect.isGreaterThan
                  completions.Items.Length
                  30
                  "should have a complete completion list all containing c"

                let firstItem = completions.Items.[0]
                Expect.equal firstItem.Label "abs" "first member should be abs"
              | Ok None -> failtest "Should have gotten some completion items"
              | Error e -> failtestf "Got an error while retrieving completions: %A" e
            }) ] ]
