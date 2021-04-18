module FsAutoComplete.Tests.Completion

open Expecto
open System.IO
open Helpers
open LanguageServerProtocol.Types
open FsAutoComplete.Utils

let tests state =
  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "Completion")
      let! (server, events) = serverInitialize path defaultConfigDto state
      do! waitForWorkspaceFinishedParsing events
      let path = Path.Combine(path, "Script.fsx")
      let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path }
      do! server.TextDocumentDidOpen tdop
      let! diagnostics = waitForParseResultsForFile "Script.fsx" events |> AsyncResult.bimap (fun _ -> failtest "Should have had errors") (fun e -> e)
      return (server, path)
    }
    |> Async.Cache

  testList "Completion Tests" [
    testCaseAsync "simple module member completion on dot" (async {
      let! server, path = server
      let completionParams : CompletionParams =
        {
          TextDocument = { Uri = Path.FilePathToUri path }
          Position = { Line = 3; Character = 9 } // the '.' in 'Async.'
          Context = Some { triggerKind = CompletionTriggerKind.TriggerCharacter; triggerCharacter = Some '.' }
        }
      let! response = server.TextDocumentCompletion completionParams
      match response with
      | Ok (Some completions) ->
        Expect.isLessThan completions.Items.Length 100 "shouldn't have an incredibly huge completion list for a simple module completion"
      | Ok None ->
        failtest "Should have gotten some completion items"
      | Error e ->
        failtestf "Got an error while retrieving completions: %A" e
    })
  ]
