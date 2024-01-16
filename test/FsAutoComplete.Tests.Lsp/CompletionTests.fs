module FsAutoComplete.Tests.Completion

open System
open System.IO
open Expecto
open Helpers
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete.Utils
open FsAutoComplete.Lsp
open FsToolkit.ErrorHandling
open Helpers.Expecto.ShadowedTimeouts

#nowarn "44" //we're testing so need to be able to use deprecated fields

let tests state =
  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "Completion")
      let! (server, events) = serverInitialize path defaultConfigDto state
      do! waitForWorkspaceFinishedParsing events
      let path = Path.Combine(path, "Script.fsx")
      let tdop: DidOpenTextDocumentParams = { TextDocument = loadDocument path }
      do! server.TextDocumentDidOpen tdop

      let! _diagnostics =
        waitForParseResultsForFile "Script.fsx" events
        |> AsyncResult.bimap (fun _ -> failtest "Should have had errors") (fun e -> e)

      return (server, path)
    }
    |> Async.Cache

  testSequenced
  <| testList
    "Completion Tests"
    [ testCaseAsync
        "simple module member completion on dot"
        (async {
          let! server, path = server

          let completionParams: CompletionParams =
            { TextDocument = { Uri = Path.FilePathToUri path }
              Position = { Line = 3; Character = 9 } // the '.' in 'Async.'
              Context =
                Some
                  { triggerKind = CompletionTriggerKind.TriggerCharacter
                    triggerCharacter = Some '.' } }

          let! response = server.TextDocumentCompletion completionParams

          match response with
          | Ok(Some completions) ->
            Expect.isLessThan
              completions.Items.Length
              100
              "shouldn't have an incredibly huge completion list for a simple module completion"

            let firstItem = completions.Items.[0]

            Expect.equal
              firstItem.Label
              "CancellationToken"
              "first member should be CancellationToken, since properties are preferred over functions"
          | Ok None -> failtest "Should have gotten some completion items"
          | Error e -> failtestf "Got an error while retrieving completions: %A" e
        })


      testCaseAsync
        "simple completion for ending of a function call after text change"
        (async {
          let! server, path = server

          let lineUnderTest = """Path.GetDirectoryName("foo")"""
          let line = 15
          let character = lineUnderTest.Length

          let textChange: DidChangeTextDocumentParams =
            { TextDocument =
                { Uri = Path.FilePathToUri path
                  Version = 1 }
              ContentChanges =
                [| { Range =
                       Some
                         { Start = { Line = line; Character = character }
                           End = { Line = line; Character = character } }
                     RangeLength = Some 0
                     Text = "." } |] }

          let! c = server.TextDocumentDidChange textChange |> Async.StartChild

          let completionParams: CompletionParams =
            { TextDocument = { Uri = Path.FilePathToUri path }
              Position =
                { Line = line
                  Character = character + 1 }
              Context =
                Some
                  { triggerKind = CompletionTriggerKind.TriggerCharacter
                    triggerCharacter = Some '.' } }

          let! response = server.TextDocumentCompletion completionParams
          do! c

          match response with
          | Ok(Some completions) ->
            Expect.isLessThan
              completions.Items.Length
              100
              "shouldn't have an incredibly huge completion list for a simple module completion"

            let firstItem = completions.Items.[0]

            Expect.equal
              firstItem.Label
              "Chars"
              "first member should be Chars, since properties are preferred over functions"
          | Ok None -> failtest "Should have gotten some completion items"
          | Error e -> failtestf "Got an error while retrieving completions: %A" e
        })

      testCaseAsync
        "simple completion for ending of a function call"
        (async {
          let! server, path = server

          let lineUnderTest = """Path.GetDirectoryName("foo")."""
          let line = 14
          let character = lineUnderTest.Length

          let completionParams: CompletionParams =
            { TextDocument = { Uri = Path.FilePathToUri path }
              Position = { Line = line; Character = character } // the '.' in 'GetDirectoryName().'
              Context =
                Some
                  { triggerKind = CompletionTriggerKind.TriggerCharacter
                    triggerCharacter = Some '.' } }

          let! response = server.TextDocumentCompletion completionParams

          match response with
          | Ok(Some completions) ->
            Expect.isLessThan
              completions.Items.Length
              100
              "shouldn't have an incredibly huge completion list for a simple module completion"

            let firstItem = completions.Items.[0]

            Expect.equal
              firstItem.Label
              "Chars"
              "first member should be Chars, since properties are preferred over functions"
          | Ok None -> failtest "Should have gotten some completion items"
          | Error e -> failtestf "Got an error while retrieving completions: %A" e
        })

      testCaseAsync
        "simple completion for ending of a string after text change"
        (async {
          let! server, path = server

          let line = 18
          let lineUnderTest = "\"bareString\""
          let character = lineUnderTest.Length

          let textChange: DidChangeTextDocumentParams =
            { TextDocument =
                { Uri = Path.FilePathToUri path
                  Version = 1 }
              ContentChanges =
                [| { Range =
                       Some
                         { Start = { Line = line; Character = character }
                           End = { Line = line; Character = character } }
                     RangeLength = Some 0
                     Text = "." } |] }

          let! c = server.TextDocumentDidChange textChange |> Async.StartChild

          let completionParams: CompletionParams =
            { TextDocument = { Uri = Path.FilePathToUri path }
              Position =
                { Line = line
                  Character = character + 1 }
              Context =
                Some
                  { triggerKind = CompletionTriggerKind.TriggerCharacter
                    triggerCharacter = Some '.' } }

          let! response = server.TextDocumentCompletion completionParams
          do! c

          match response with
          | Ok(Some completions) ->
            Expect.isLessThan
              completions.Items.Length
              100
              "shouldn't have an incredibly huge completion list for a simple module completion"

            let firstItem = completions.Items.[0]

            Expect.equal
              firstItem.Label
              "Chars"
              "first member should be Chars, since properties are preferred over functions"
          | Ok None -> failtest "Should have gotten some completion items"
          | Error e -> failtestf "Got an error while retrieving completions: %A" e
        })

      testCaseAsync
        "simple completion for ending of a string"
        (async {
          let! server, path = server

          let line = 17
          let lineUnderTest = """"bareString"."""
          let character = lineUnderTest.Length

          let completionParams: CompletionParams =
            { TextDocument = { Uri = Path.FilePathToUri path }
              Position = { Line = line; Character = character }
              Context =
                Some
                  { triggerKind = CompletionTriggerKind.TriggerCharacter
                    triggerCharacter = Some '.' } }

          let! response = server.TextDocumentCompletion completionParams

          match response with
          | Ok(Some completions) ->
            Expect.isLessThan
              completions.Items.Length
              100
              "shouldn't have an incredibly huge completion list for a simple module completion"

            let firstItem = completions.Items.[0]

            Expect.equal
              firstItem.Label
              "Chars"
              "first member should be Chars, since properties are preferred over functions"
          | Ok None -> failtest "Should have gotten some completion items"
          | Error e -> failtestf "Got an error while retrieving completions: %A" e
        })

      testCaseAsync
        "simple completion for ending of a list after text change"
        (async {
          let! server, path = server

          let line = 21
          let lineUnderTest = "[1;2;3]"
          let character = lineUnderTest.Length

          let textChange: DidChangeTextDocumentParams =
            { TextDocument =
                { Uri = Path.FilePathToUri path
                  Version = 1 }
              ContentChanges =
                [| { Range =
                       Some
                         { Start = { Line = line; Character = character }
                           End = { Line = line; Character = character } }
                     RangeLength = Some 0
                     Text = "." } |] }

          let! c = server.TextDocumentDidChange textChange |> Async.StartChild

          let completionParams: CompletionParams =
            { TextDocument = { Uri = Path.FilePathToUri path }
              Position =
                { Line = line
                  Character = character + 1 }
              Context =
                Some
                  { triggerKind = CompletionTriggerKind.TriggerCharacter
                    triggerCharacter = Some '.' } }

          let! response = server.TextDocumentCompletion completionParams
          do! c

          match response with
          | Ok(Some completions) ->
            Expect.isLessThan
              completions.Items.Length
              100
              "shouldn't have an incredibly huge completion list for a simple module completion"

            let firstItem = completions.Items.[0]

            Expect.equal
              firstItem.Label
              "Head"
              "first member should be Head, since properties are preferred over functions"
          | Ok None -> failtest "Should have gotten some completion items"
          | Error e -> failtestf "Got an error while retrieving completions: %A" e
        })

      testCaseAsync
        "simple completion for ending of a list"
        (async {
          let! server, path = server

          let line = 20
          let lineUnderTest = "[1;2;3]."
          let character = lineUnderTest.Length

          let completionParams: CompletionParams =
            { TextDocument = { Uri = Path.FilePathToUri path }
              Position = { Line = line; Character = character }
              Context =
                Some
                  { triggerKind = CompletionTriggerKind.TriggerCharacter
                    triggerCharacter = Some '.' } }

          let! response = server.TextDocumentCompletion completionParams

          match response with
          | Ok(Some completions) ->
            Expect.isLessThan
              completions.Items.Length
              100
              "shouldn't have an incredibly huge completion list for a simple module completion"

            let firstItem = completions.Items.[0]

            Expect.equal
              firstItem.Label
              "Head"
              "first member should be Head, since properties are preferred over functions"
          | Ok None -> failtest "Should have gotten some completion items"
          | Error e -> failtestf "Got an error while retrieving completions: %A" e
        })

      testCaseAsync
        "completion at start of line"
        (async {
          let! server, path = server

          let completionParams: CompletionParams =
            { TextDocument = { Uri = Path.FilePathToUri path }
              Position = { Line = 6; Character = 5 } // the '.' in 'List.'
              Context =
                Some
                  { triggerKind = CompletionTriggerKind.TriggerCharacter
                    triggerCharacter = Some '.' } }

          let! response = server.TextDocumentCompletion completionParams

          match response with
          | Ok(Some completions) ->
            Expect.equal completions.Items.Length 106 "at time of writing the List module has 106 exposed members"
            let firstItem = completions.Items.[0]

            Expect.equal
              firstItem.Label
              "Empty"
              "first member should be List.Empty, since properties are preferred over functions"
          | Ok None -> failtest "Should have gotten some completion items"
          | Error e -> failtestf "Got an error while retrieving completions: %A" e
        })

      testCaseAsync
        "completion at end of line"
        (async {
          let! server, path = server

          let completionParams: CompletionParams =
            { TextDocument = { Uri = Path.FilePathToUri path }
              Position = { Line = 8; Character = 16 } // the '.' in 'List.'
              Context =
                Some
                  { triggerKind = CompletionTriggerKind.TriggerCharacter
                    triggerCharacter = Some '.' } }

          let! response = server.TextDocumentCompletion completionParams

          match response with
          | Ok(Some completions) ->
            Expect.equal completions.Items.Length 106 "at time of writing the List module has 106 exposed members"
            let firstItem = completions.Items.[0]

            Expect.equal
              firstItem.Label
              "Empty"
              "first member should be List.Empty, since properties are preferred over functions"
          | Ok None -> failtest "Should have gotten some completion items"
          | Error e -> failtestf "Got an error while retrieving completions: %A" e
        })
      testCaseAsync
        "completion in record defn field type"
        (async {
          let! server, path = server

          let completionParams: CompletionParams =
            { TextDocument = { Uri = Path.FilePathToUri path }
              Position = { Line = 11; Character = 10 } // after Lis partial type name in Id record field declaration
              Context =
                Some
                  { triggerKind = CompletionTriggerKind.Invoked
                    triggerCharacter = None } }

          let! response = server.TextDocumentCompletion completionParams

          match response with
          | Ok(Some completions) ->
            Expect.isLessThan
              completions.Items.Length
              300
              "shouldn't have a very long list of completion items that are only types"

            Expect.isGreaterThan
              completions.Items.Length
              100
              "should have a reasonable number of completion items that are only types"

            Expect.exists completions.Items (fun item -> item.Label = "list") "completion should contain the list type"
          | Ok None -> failtest "Should have gotten some completion items"
          | Error e -> failtestf "Got an error while retrieving completions: %A" e
        })

      testCaseAsync
        "completion before first character of expression"
        (async {
          let! server, path = server

          let completionParams: CompletionParams =
            { TextDocument = { Uri = Path.FilePathToUri path }
              Position = { Line = 8; Character = 12 } // after the 'L' in 'List.'
              Context =
                Some
                  { triggerKind = CompletionTriggerKind.Invoked
                    triggerCharacter = None } }

          let! response = server.TextDocumentCompletion completionParams

          match response with
          | Ok(Some completions) ->
            Expect.isGreaterThan completions.Items.Length 100 "should have a very long list of all symbols"
            let firstItem = completions.Items.[0]

            Expect.equal
              firstItem.Label
              "async"
              "first member should be async, alphabetically first in the full symbol list"
          | Ok None -> failtest "Should have gotten some completion items"
          | Error e -> failtestf "Got an error while retrieving completions: %A" e
        })

      testCaseAsync
        "completion after first character of expression"
        (async {
          let! server, path = server

          let completionParams: CompletionParams =
            { TextDocument = { Uri = Path.FilePathToUri path }
              Position = { Line = 8; Character = 11 } // before the 'L' in 'List.'
              Context =
                Some
                  { triggerKind = CompletionTriggerKind.Invoked
                    triggerCharacter = None } }

          let! response = server.TextDocumentCompletion completionParams

          match response with
          | Ok(Some completions) ->
            Expect.isGreaterThan completions.Items.Length 100 "should have a very long list of all symbols"
            let firstItem = completions.Items.[0]

            Expect.equal
              firstItem.Label
              "async"
              "first member should be async, alphabetically first in the full symbol list"
          | Ok None -> failtest "Should have gotten some completion items"
          | Error e -> failtestf "Got an error while retrieving completions: %A" e
        })

      testCaseAsync
        "no backticks in completionitem signatures"
        (asyncResult {
          let! server, path = server

          let completionParams: CompletionParams =
            { TextDocument = { Uri = Path.FilePathToUri path }
              Position = { Line = 3; Character = 9 } // the '.' in 'Async.'
              Context =
                Some
                  { triggerKind = CompletionTriggerKind.TriggerCharacter
                    triggerCharacter = Some '.' } }

          let! response = server.TextDocumentCompletion completionParams |> AsyncResult.map Option.get
          let ctokMember = response.Items[0]
          let! resolved = server.CompletionItemResolve(ctokMember)

          Expect.equal
            resolved.Label
            "CancellationToken"
            "Just making sure we're on the right member, one that should have backticks"

          Expect.equal
            resolved.Detail
            (Some "property Async.CancellationToken: Async<System.Threading.CancellationToken> with get")
            "Signature shouldn't have backticks"

         }
         |> AsyncResult.bimap id (fun e -> failwithf "%O" e))

      testCaseAsync
        "completion in interpolated string"
        (async {
          let! server, path = server

          let completionParams: CompletionParams =
            { TextDocument = { Uri = Path.FilePathToUri path }
              Position = { Line = 23; Character = 8 } // the '.' in 'List.'
              Context =
                Some
                  { triggerKind = CompletionTriggerKind.TriggerCharacter
                    triggerCharacter = Some '.' } }

          let! response = server.TextDocumentCompletion completionParams

          match response with
          | Ok(Some completions) ->
            Expect.equal completions.Items.Length 106 "at time of writing the List module has 106 exposed members"
            let firstItem = completions.Items.[0]

            Expect.equal
              firstItem.Label
              "Empty"
              "first member should be List.Empty, since properties are preferred over functions"
          | Ok None -> failtest "Should have gotten some completion items"
          | Error e -> failtestf "Got an error while retrieving completions: %A" e
        })

      testCaseAsync
        "completion in interpolated string with whitespace"
        (async {
          let! server, path = server

          let completionParams: CompletionParams =
            { TextDocument = { Uri = Path.FilePathToUri path }
              Position = { Line = 24; Character = 9 } // the '.' in 'List.'
              Context =
                Some
                  { triggerKind = CompletionTriggerKind.TriggerCharacter
                    triggerCharacter = Some '.' } }

          let! response = server.TextDocumentCompletion completionParams

          match response with
          | Ok(Some completions) ->
            Expect.equal completions.Items.Length 106 "at time of writing the List module has 106 exposed members"
            let firstItem = completions.Items.[0]

            Expect.equal
              firstItem.Label
              "Empty"
              "first member should be List.Empty, since properties are preferred over functions"
          | Ok None -> failtest "Should have gotten some completion items"
          | Error e -> failtestf "Got an error while retrieving completions: %A" e
        }) ]

///Tests for getting autocomplete
let autocompleteTest state =
  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "AutocompleteTest")
      let! (server, event) = serverInitialize path defaultConfigDto state
      let projectPath = Path.Combine(path, "AutocompleteTest.fsproj")
      do! waitForWorkspaceFinishedParsing event
      do! parseProject projectPath server
      let path = Path.Combine(path, "Script.fs")
      let tdop: DidOpenTextDocumentParams = { TextDocument = loadDocument path }
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
      let tdop: DidOpenTextDocumentParams = { TextDocument = loadDocument path }
      do! server.TextDocumentDidOpen tdop
      return (server, path)
    }
    |> Async.Cache

  let makeAutocompleteTestList (serverConfig: (IFSharpLspServer * string) Async) =
    [ testCaseAsync
        "Get Autocomplete module members"
        (async {
          let! server, path = serverConfig

          let p: CompletionParams =
            { TextDocument = { Uri = Path.FilePathToUri path }
              Position = { Line = 8; Character = 2 }
              Context = None }

          let! res = server.TextDocumentCompletion p

          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok(Some res) ->

            Expect.equal res.Items.Length 2 "Autocomplete has all symbols"
            Expect.exists res.Items (fun n -> n.Label = "func") "Autocomplete contains given symbol"
            Expect.exists res.Items (fun n -> n.Label = "sample func") "Autocomplete contains given symbol"
        })

      testCaseAsync
        "Get Autocomplete namespace"
        (async {
          let! server, path = serverConfig

          let p: CompletionParams =
            { TextDocument = { Uri = Path.FilePathToUri path }
              Position = { Line = 10; Character = 2 }
              Context = None }

          let! res = server.TextDocumentCompletion p

          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok(Some res) ->
            //TODO
            // Expect.equal res.Items.Length 1 "Autocomplete has all symbols"
            Expect.exists res.Items (fun n -> n.Label = "System") "Autocomplete contains given symbol"

        })

      testCaseAsync
        "Get Autocomplete namespace members"
        (async {
          let! server, path = serverConfig

          let p: CompletionParams =
            { TextDocument = { Uri = Path.FilePathToUri path }
              Position = { Line = 12; Character = 7 }
              Context = None }

          let! res = server.TextDocumentCompletion p

          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok(Some res) ->
            //TODO
            // Expect.equal res.Items.Length 1 "Autocomplete has all symbols"
            Expect.exists res.Items (fun n -> n.Label = "DateTime") "Autocomplete contains given symbol"

        })

      testCaseAsync
        "Get Autocomplete module doublebackticked members"
        (async {
          let! server, path = serverConfig

          let p: CompletionParams =
            { TextDocument = { Uri = Path.FilePathToUri path }
              Position = { Line = 14; Character = 18 }
              Context = None }

          let! res = server.TextDocumentCompletion p

          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok(Some res) ->

            Expect.equal res.Items.Length 1 "Autocomplete has all symbols"
            Expect.exists res.Items (fun n -> n.Label = "z") "Autocomplete contains given symbol"
        })

      testCaseAsync
        "Autocomplete record members"
        (async {
          let! server, path = serverConfig

          let p: CompletionParams =
            { TextDocument = { Uri = Path.FilePathToUri path }
              Position = { Line = 25; Character = 4 }
              Context = None }

          let! res = server.TextDocumentCompletion p

          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok(Some res) ->
            Expect.exists res.Items (fun n -> n.Label = "bar") "Autocomplete contains given symbol"
            Expect.exists res.Items (fun n -> n.Label = "baz") "Autocomplete contains given symbol"
        })

      testCaseAsync
        "Autocomplete class constructor with properties"
        (async {
          let! server, path = serverConfig

          let p: CompletionParams =
            { TextDocument = { Uri = Path.FilePathToUri path }
              Position = { Line = 32; Character = 25 }
              Context = None }

          let! res = server.TextDocumentCompletion p

          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok(Some res) ->
            Expect.exists res.Items (fun n -> n.Label = "Bar") "Autocomplete contains given symbol"
            Expect.exists res.Items (fun n -> n.Label = "Baz") "Autocomplete contains given symbol"
        }) ]

  testList
    "Autocomplete Tests"
    [ testList "Autocomplete within project files" (makeAutocompleteTestList server)
      testList "Autocomplete within script files" (makeAutocompleteTestList scriptServer) ]

///TODO: these are broken in FCS 43.7.200 - something in the tokenization isn't searching the System namespace
let autoOpenTests state =
  let dirPath =
    Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "CompletionAutoOpenTests")

  let serverFor (scriptPath: string) =
    async {
      // Auto Open requires unopened things in completions -> External
      let config =
        { defaultConfigDto with
            ExternalAutocomplete = Some true
            ResolveNamespaces = Some true }

      let dirPath = Path.GetDirectoryName scriptPath
      let scriptName = Path.GetFileName scriptPath
      let! (server, events) = serverInitialize dirPath config state
      do! waitForWorkspaceFinishedParsing events

      let tdop: DidOpenTextDocumentParams = { TextDocument = loadDocument scriptPath }
      do! server.TextDocumentDidOpen tdop

      do!
        waitForParseResultsForFile scriptName events
        |> AsyncResult.bimap (fun _ -> failtest "Should have had errors") id
        |> Async.Ignore

      return (server, scriptPath)
    }

  let calcOpenPos (edit: TextEdit) =
    let text = edit.NewText
    let pos = edit.Range.Start

    let indentation = pos.Character + (text.Length - text.TrimStart().Length)

    { Line = pos.Line
      Character = indentation }

  let getQuickFix (server: IFSharpLspServer, path: string) (word: string, _ns: string) (cursor: Position) =
    async {
      let p =
        { CodeActionParams.TextDocument = { Uri = Path.FilePathToUri path }
          Range = { Start = cursor; End = cursor }
          Context =
            { Diagnostics =
                [| { Range = { Start = cursor; End = cursor }
                     Severity = Some DiagnosticSeverity.Error
                     // Message required for QuickFix to fire ("is not defined")
                     Message = $"The value or constructor '{word}' is not defined."
                     Code = Some "39"
                     Source = Some "F# Compiler"
                     RelatedInformation = None
                     Tags = None
                     Data = None
                     CodeDescription = None } |]
              Only = None
              TriggerKind = None } }

      let (|ContainsOpenAction|_|) (codeActions: CodeAction[]) =
        codeActions
        |> Array.tryFind (fun ca ->
          ca.Kind = Some "quickfix"
          && ca.Title.StartsWith("open ", StringComparison.Ordinal))

      match! server.TextDocumentCodeAction p with
      | Error e -> return failtestf "Quick fix Request failed: %A" e
      | Ok None -> return failtest "Quick fix Request none"
      | Ok(Some(CodeActions(ContainsOpenAction quickfix))) ->
        let ns = quickfix.Title.Substring("open ".Length)

        let edit = quickfix.Edit.Value.DocumentChanges.Value.[0].Edits.[0]

        let openPos = calcOpenPos edit
        return (edit, ns, openPos)
      | Ok _ -> return failtest $"Quick fix on `{word}` doesn't contain open action"
    }

  let test
    (compareWithQuickFix: bool)
    (name: string option)
    (server: Async<IFSharpLspServer * string>)
    (word: string, ns: string)
    (cursor: Position)
    (expectedOpen: Position)
    pending
    =
    let name =
      name
      |> Option.defaultWith (fun _ ->
        sprintf
          "completion on `Regex` at (%i, %i) should `open System.Text.RegularExpressions` at (%i, %i) (0-based)"
          (cursor.Line)
          (cursor.Character)
          (expectedOpen.Line)
          (expectedOpen.Character))

    let runner = if pending then ptestCaseAsync else testCaseAsync

    runner name
    <| async {
      let! server, path = server

      let p: CompletionParams =
        { TextDocument = { Uri = Path.FilePathToUri path }
          // Line AND Column are ZERO-based!
          Position = cursor
          Context = None }

      match! server.TextDocumentCompletion p with
      | Error e -> failtestf "Request failed: %A" e
      | Ok None -> failtest "Request none"
      | Ok(Some res) ->
        Expect.isFalse res.IsIncomplete "Result is incomplete"
        let ci = res.Items |> Array.tryFind (fun c -> c.Label = word)

        if ci = None then
          failwithf
            $"Couldn't find completion item for `{word}` among the items %A{res.Items |> Array.map (fun i -> i.Label)}"
          |> ignore

        // now get details: `completionItem/resolve` (previous request was `textDocument/completion` -> List of all completions, but without details)
        match! server.CompletionItemResolve ci.Value with
        | Error e -> failtestf "Request failed: %A" e
        | Ok ci ->
          Expect.equal ci.Label $"{word} (open {ns})" $"Should be unopened {word}"
          let edit = ci.AdditionalTextEdits.Value |> Array.head
          let text = edit.NewText
          Expect.equal (text.Trim()) $"open {ns}" $"Edit should be `open {ns}`"
          let openPos = calcOpenPos edit
          Expect.equal openPos.Line expectedOpen.Line "Should be on correct line"
          Expect.equal openPos.Character expectedOpen.Character "Should have correct indentation"
          Expect.stringEnds text "\n" "Should end with New Line"

          if compareWithQuickFix then
            // must be same as open quick fix (`ResolveNamespace`)
            // NOTE: currently code completion and quick fix open in different locations:
            //  * Code Completion: nearest position
            //  * Quick Fix: Top Level
            let! (_, _, qfOpenPos) = getQuickFix (server, path) (word, ns) cursor
            Expect.equal qfOpenPos openPos "Auto-Open and Open Quick Fix should open at same location"
    }

  //  05: //...
  /// ```
  /// * Expected open:
  ///   * Line: `-1` relative to Line `04` -> Line `03`
  ///   * Column: `|-2`: indentation - 2 spaces
  ///     * Current indentation is 4 spaces
  ///     * -> Expected indentation is 2 spaces -> Column 2
  ///   * -> Position of open: (3,2)
  ///     * NOTE: 0-based, but display in editor is 1-based (Line 4, Column 3 in editor!)
  let readData path =
    let regex = System.Text.RegularExpressions.Regex("\(\*(?<data>.*)\*\)")

    let parseData (line, column) (lineStr: string) (data: string) =
      match data.Split(',') with
      | [| l; c |] ->
        let calcN (current: int) (n: string) =
          let n = n.Trim()

          match n.[0] with
          | '|' ->
            //relative to indentation of current line
            let ind = lineStr.Length - lineStr.TrimStart().Length

            match n.Substring(1).Trim() with
            | "" -> ind
            | n -> ind + int n
          | '+'
          | '-' ->
            // relative to current position
            current + int n
          | _ ->
            // absolute
            int n

        let (l, c) = (calcN line l, calcN column c)
        { Line = l; Character = c }
      | _ -> failwithf "Invalid data in line (%i,%i) '%s'" line column lineStr

    let extractData (lineNumber: int) (line: string) =
      let m = regex.Match line

      if not m.Success then
        None
      else
        let data = m.Groups.["data"]
        let (l, c) = (lineNumber, m.Index)
        let openPos = parseData (l, c) line data.Value
        let cursorPos = { Line = l; Character = c }

        (cursorPos, openPos) |> Some

    System.IO.File.ReadAllLines path
    |> Seq.mapi (fun i l -> (i, l))
    |> Seq.filter (fun (_, l) -> l.Contains "(*")
    |> Seq.choose (fun (i, l) -> extractData i l)
    |> Seq.toList

  let testScript name scriptName =
    testList
      name
      [ let scriptPath = Path.Combine(dirPath, scriptName)
        let server = serverFor scriptPath

        let tests =
          readData scriptPath
          |> List.map (fun (cursor, expectedOpen) ->
            test false None server ("Regex", "System.Text.RegularExpressions") cursor expectedOpen false)

        yield! tests ]

  let ptestScript name scriptName =
    testList
      name
      [ let scriptPath = Path.Combine(dirPath, scriptName)
        let server = serverFor scriptPath

        let tests =
          readData scriptPath
          |> List.map (fun (cursor, expectedOpen) ->
            test false None server ("Regex", "System.Text.RegularExpressions") cursor expectedOpen true)

        yield! tests ]

  ptestList
    "Completion.AutoOpen"
    [
      // NOTE: Positions are ZERO-based!: { Line = 3; Character = 9 } -> Line 4, Column 10 in editor display
      testScript "with root module with new line" "ModuleWithNewLine.fsx"
      testScript "with root module" "Module.fsx"
      testScript "with root module with open" "ModuleWithOpen.fsx"
      testScript "with root module with open and new line" "ModuleWithOpenAndNewLine.fsx"
      ptestScript "with namespace with new line" "NamespaceWithNewLine.fsx"
      ptestScript "with namespace" "Namespace.fsx"
      testScript "with namespace with open" "NamespaceWithOpen.fsx"
      testScript "with namespace with open and new line" "NamespaceWithOpenAndNewLine.fsx"
      testScript "with implicit top level module with new line" "ImplicitTopLevelModuleWithNewLine.fsx"
      testScript "with implicit top level module" "ImplicitTopLevelModule.fsx"
      testScript "with implicit top level module with open" "ImplicitTopLevelModuleWithOpen.fsx"
      testScript "with implicit top level module with open and new line" "ImplicitTopLevelModuleWithOpenAndNewLine.fsx"
      testScript
        "with implicit top level module with open and new lines"
        "ImplicitTopLevelModuleWithOpenAndNewLines.fsx"
      testScript "with root module with comments and new line before open" "ModuleDocsAndNewLineBeforeOpen.fsx" ]

let fullNameExternalAutocompleteTest state =
  let server =
    async {
      let config =
        { defaultConfigDto with
            ExternalAutocomplete = Some true
            FullNameExternalAutocomplete = Some true }

      let path =
        Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "FullNameExternalAutocompleteTest")

      let! (server, event) = serverInitialize path config state
      let projectPath = Path.Combine(path, "FullNameExternalAutocompleteTest.fsproj")
      do! waitForWorkspaceFinishedParsing event
      do! parseProject projectPath server
      let path = Path.Combine(path, "Script.fs")
      let tdop: DidOpenTextDocumentParams = { TextDocument = loadDocument path }
      do! server.TextDocumentDidOpen tdop
      return (server, path)
    }
    |> Async.Cache

  let scriptServer =
    async {
      let config =
        { defaultConfigDto with
            ExternalAutocomplete = Some true
            FullNameExternalAutocomplete = Some true }

      let path =
        Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "FullNameExternalAutocompleteTest")

      let! (server, event) = serverInitialize path config state
      do! waitForWorkspaceFinishedParsing event
      let path = Path.Combine(path, "Script.fsx")
      let tdop: DidOpenTextDocumentParams = { TextDocument = loadDocument path }
      do! server.TextDocumentDidOpen tdop
      return (server, path)
    }
    |> Async.Cache

  let makeAutocompleteTest (serverConfig: (IFSharpLspServer * string) Async) testName (line, character) expects =
    testCaseAsync
      testName
      (async {
        let! server, path = serverConfig

        let p: CompletionParams =
          { TextDocument = { Uri = Path.FilePathToUri path }
            Position = { Line = line; Character = character }
            Context =
              Some
                { triggerKind = CompletionTriggerKind.Invoked
                  triggerCharacter = None } }

        let! res = server.TextDocumentCompletion p

        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok(Some res) -> expects res
      })

  let makeAutocompleteTestList (serverConfig: (IFSharpLspServer * string) Async) =
    [ makeAutocompleteTest serverConfig "Autocomplete for Array.map contains no backticks" (0, 8) (fun res ->
        let n = res.Items |> Array.tryFind (fun i -> i.Label = "Array.map")
        Expect.isSome n "Completion doesn't exist"
        Expect.equal n.Value.InsertText (Some "Array.map") "Autocomplete for Array.map contains backticks")

      makeAutocompleteTest serverConfig "Autocomplete for ``a.b`` contains backticks" (2, 1) (fun res ->
        let n = res.Items |> Array.tryFind (fun i -> i.Label = "a.b")
        Expect.isSome n "Completion doesn't exist"
        Expect.equal n.Value.InsertText (Some "``a.b``") "Autocomplete for a.b contains no backticks")

      testCaseAsync
        "Autocompletes with same label have different description"
        (asyncResult {
          let! server, path = serverConfig

          let p: CompletionParams =
            { TextDocument = { Uri = Path.FilePathToUri path }
              Position = { Line = 3; Character = 4 }
              Context =
                Some
                  { triggerKind = CompletionTriggerKind.Invoked
                    triggerCharacter = None } }

          let! response = server.TextDocumentCompletion p |> AsyncResult.map Option.get

          let! items =
            response.Items
            |> Array.filter (fun i -> i.Label = "bind")
            |> Array.map (server.CompletionItemResolve)
            |> Async.Parallel

          let count =
            items
            |> Array.distinctBy (function
              | Ok x -> x.Detail
              | Error _ -> None)
            |> Array.length

          Expect.equal count items.Length "These completions doesn't have different description"
         }
         |> AsyncResult.bimap id (fun e -> failwithf "%O" e))

      makeAutocompleteTest serverConfig "Check Autocomplete for System.Text.RegularExpressions.Regex" (4, 5) (fun res ->
        let n =
          res.Items
          |> Array.tryFind (fun i -> i.Label = "Regex (System.Text.RegularExpressions)")

        Expect.isSome n "Completion doesn't exist"

        Expect.equal
          n.Value.InsertText
          (Some "System.Text.RegularExpressions.Regex")
          "Autocomplete for Regex is not System.Text.RegularExpressions.Regex"

        Expect.equal
          n.Value.FilterText
          (Some "RegexSystem.Text.RegularExpressions.Regex")
          "Autocomplete for Regex is not System.Text.RegularExpressions.Regex")

      makeAutocompleteTest serverConfig "Autocomplete for Result is just Result" (5, 6) (fun res ->
        let n = res.Items |> Array.tryFind (fun i -> i.Label = "Result")
        Expect.isSome n "Completion doesn't exist"
        Expect.equal n.Value.InsertText (Some "Result") "Autocomplete contains given symbol") ]

  testList
    "fullNameExternalAutocompleteTest Tests"
    [ testList "fullNameExternalAutocompleteTest within project files" (makeAutocompleteTestList server)
      testList "fullNameExternalAutocompleteTest within script files" (makeAutocompleteTestList scriptServer) ]
