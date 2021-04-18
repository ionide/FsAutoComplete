module FsAutoComplete.Tests.Completion

open Expecto
open System.IO
open Helpers
open LanguageServerProtocol.Types
open FsAutoComplete.Utils
open FsAutoComplete.Lsp

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
          Position = { Line = 3; Character = 10 } // the '.' in 'Async.'
          Context = Some { triggerKind = CompletionTriggerKind.TriggerCharacter; triggerCharacter = Some '.' }
        }
      let! response = server.TextDocumentCompletion completionParams
      match response with
      | Ok (Some completions) ->
        Expect.isLessThan completions.Items.Length 100 "shouldn't have an incredibly huge completion list for a simple module completion"
        let firstItem = completions.Items.[0]
        Expect.equal firstItem.Label "CancellationToken" "first member should be CancellationToken, since properties are preferred over functions"
      | Ok None ->
        failtest "Should have gotten some completion items"
      | Error e ->
        failtestf "Got an error while retrieving completions: %A" e
    })
  ]

  ///Tests for getting autocomplete
let autocompleteTest state =
  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "AutocompleteTest")
      let! (server, event) = serverInitialize path defaultConfigDto state
      let projectPath = Path.Combine(path, "AutocompleteTest.fsproj")
      do! parseProject projectPath server
      do! waitForWorkspaceFinishedParsing event
      let path = Path.Combine(path, "Script.fs")
      let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path}
      do! server.TextDocumentDidOpen tdop
      return (server, path)
    }
    |> Async.Cache

  let scriptServer =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "AutocompleteScriptTest")
      let! (server, event) = serverInitialize path defaultConfigDto state
      do! waitForWorkspaceFinishedParsing event
      let path = Path.Combine(path, "Script.fsx")
      let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path}
      do! server.TextDocumentDidOpen tdop
      return (server, path)
    }
    |> Async.Cache

  let makeAutocompleteTestList (serverConfig: (FSharpLspServer * string) Async) = [
    testCaseAsync "Get Autocomplete module members" (
      async {
        let! server, path = serverConfig
        let p : CompletionParams = { TextDocument = { Uri = Path.FilePathToUri path}
                                     Position = { Line = 8; Character = 2 }
                                     Context = None }
        let! res = server.TextDocumentCompletion p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->

          Expect.equal res.Items.Length 2 "Autocomplete has all symbols"
          Expect.exists res.Items (fun n -> n.Label = "func") "Autocomplete contains given symbol"
          Expect.exists res.Items (fun n -> n.Label = "sample func") "Autocomplete contains given symbol"
      })

    testCaseAsync "Get Autocomplete namespace" (
      async {
        let! server, path = serverConfig
        let p : CompletionParams = { TextDocument = { Uri = Path.FilePathToUri path}
                                     Position = { Line = 10; Character = 2 }
                                     Context = None }
        let! res = server.TextDocumentCompletion p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          //TODO
          // Expect.equal res.Items.Length 1 "Autocomplete has all symbols"
          Expect.exists res.Items (fun n -> n.Label = "System") "Autocomplete contains given symbol"

      })

    testCaseAsync "Get Autocomplete namespace members" (
      async {
        let! server, path = serverConfig
        let p : CompletionParams = { TextDocument = { Uri = Path.FilePathToUri path}
                                     Position = { Line = 12; Character = 7 }
                                     Context = None }
        let! res = server.TextDocumentCompletion p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          //TODO
          // Expect.equal res.Items.Length 1 "Autocomplete has all symbols"
          Expect.exists res.Items (fun n -> n.Label = "DateTime") "Autocomplete contains given symbol"

      })

    testCaseAsync "Get Autocomplete module doublebackticked members" (
      async {
        let! server, path = serverConfig
        let p : CompletionParams = { TextDocument = { Uri = Path.FilePathToUri path}
                                     Position = { Line = 14; Character = 18 }
                                     Context = None }
        let! res = server.TextDocumentCompletion p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->

          Expect.equal res.Items.Length 1 "Autocomplete has all symbols"
          Expect.exists res.Items (fun n -> n.Label = "z") "Autocomplete contains given symbol"
      })

    testCaseAsync "Autocomplete record members" (
      async {
        let! server, path = serverConfig
        let p : CompletionParams = {
          TextDocument = { Uri = Path.FilePathToUri path }
          Position = { Line = 25; Character = 4 }
          Context = None
        }
        let! res = server.TextDocumentCompletion p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          Expect.exists res.Items (fun n -> n.Label = "bar") "Autocomplete contains given symbol"
          Expect.exists res.Items (fun n -> n.Label = "baz") "Autocomplete contains given symbol"
      })

    testCaseAsync "Autocomplete class constructor with properties" (
      async {
        let! server, path = serverConfig
        let p : CompletionParams = {
          TextDocument = { Uri = Path.FilePathToUri path }
          Position = { Line = 32; Character = 26 }
          Context = None
        }
        let! res = server.TextDocumentCompletion p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          Expect.isTrue ((res.Items |> Seq.findIndex (fun n -> n.Label = "Bar")) < 2) "Autocomplete contains given symbol"
          Expect.isTrue ((res.Items |> Seq.findIndex (fun n -> n.Label = "Baz")) < 2) "Autocomplete contains given symbol"
      })

    testCaseAsync "cleanup" (async {
        let! server, _ = server
        do! server.Shutdown()
      })
  ]

  testSequenced (
    testList "Autocomplete Tests" [
      testList "Autocomplete within project files" (makeAutocompleteTestList server)
      testList "Autocomplete within script files" (makeAutocompleteTestList scriptServer)
    ]
  )
