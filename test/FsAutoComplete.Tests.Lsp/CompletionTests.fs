module FsAutoComplete.Tests.Completion

open Expecto
open System.IO
open Helpers
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete.Utils
open FsAutoComplete.Lsp

let tests state =
  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "Completion")
      let! (server, events) = serverInitialize path defaultConfigDto state
      do! waitForWorkspaceFinishedParsing events
      let path = Path.Combine(path, "Script.fsx")
      let tdop: DidOpenTextDocumentParams = { TextDocument = loadDocument path }
      do! server.TextDocumentDidOpen tdop

      let! diagnostics =
        waitForParseResultsForFile "Script.fsx" events
        |> AsyncResult.bimap (fun _ -> failtest "Should have had errors") (fun e -> e)

      return (server, path)
    }
    |> Async.Cache

  testList
    "Completion Tests"
    [ testCaseAsync
        "simple module member completion on dot"
        (async {
          let! server, path = server

          let completionParams: CompletionParams =
            { TextDocument = { Uri = Path.FilePathToUri path }
              Position = { Line = 3; Character = 10 } // the '.' in 'Async.'
              Context =
                Some
                  { triggerKind = CompletionTriggerKind.TriggerCharacter
                    triggerCharacter = Some '.' } }

          let! response = server.TextDocumentCompletion completionParams

          match response with
          | Ok (Some completions) ->
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
          | Ok (Some completions) ->
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
          | Ok (Some completions) ->
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
              Position = { Line = 13; Character = 10 } // after str
              Context =
                Some
                  { triggerKind = CompletionTriggerKind.Invoked
                    triggerCharacter = None } }

          let! response = server.TextDocumentCompletion completionParams

          match response with
          | Ok (Some completions) ->
            Expect.isLessThan
              completions.Items.Length
              300
              "shouldn't have a very long list of completion items that are only types"
            Expect.isGreaterThan
              completions.Items.Length
              100
              "should have a reasonable number of completion items that are only types"

            Expect.exists
              completions.Items
              (fun item -> item.Label = "list")
              "completion should contain the list type"
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

  let makeAutocompleteTestList (serverConfig: (FSharpLspServer * string) Async) =
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
          | Result.Ok (Some res) ->

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
          | Result.Ok (Some res) ->
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
          | Result.Ok (Some res) ->
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
          | Result.Ok (Some res) ->

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
          | Result.Ok (Some res) ->
            Expect.exists res.Items (fun n -> n.Label = "bar") "Autocomplete contains given symbol"
            Expect.exists res.Items (fun n -> n.Label = "baz") "Autocomplete contains given symbol"
        })

      testCaseAsync
        "Autocomplete class constructor with properties"
        (async {
          let! server, path = serverConfig

          let p: CompletionParams =
            { TextDocument = { Uri = Path.FilePathToUri path }
              Position = { Line = 32; Character = 26 }
              Context = None }

          let! res = server.TextDocumentCompletion p

          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->
            Expect.isTrue
              ((res.Items
                |> Seq.findIndex (fun n -> n.Label = "Bar")) < 2)
              "Autocomplete contains given symbol"

            Expect.isTrue
              ((res.Items
                |> Seq.findIndex (fun n -> n.Label = "Baz")) < 2)
              "Autocomplete contains given symbol"
        }) ]

  testList
    "Autocomplete Tests"
    [ testList "Autocomplete within project files" (makeAutocompleteTestList server)
      testList "Autocomplete within script files" (makeAutocompleteTestList scriptServer) ]


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

    let indentation =
      pos.Character
      + (text.Length - text.TrimStart().Length)

    { Line = pos.Line
      Character = indentation }

  let getQuickFix (server: FSharpLspServer, path: string) (word: string, ns: string) (cursor: Position) =
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
                     Source = "F# Compiler"
                     RelatedInformation = None
                     Tags = None
                     Data = None
                     CodeDescription = None } |] } }

      let (|ContainsOpenAction|_|) (codeActions: CodeAction []) =
        codeActions
        |> Array.tryFind (fun ca ->
          ca.Kind = Some "quickfix"
          && ca.Title.StartsWith "open ")

      match! server.TextDocumentCodeAction p with
      | Error e -> return failtestf "Quick fix Request failed: %A" e
      | Ok None -> return failtest "Quick fix Request none"
      | Ok (Some (TextDocumentCodeActionResult.CodeActions (ContainsOpenAction quickfix))) ->
        let ns = quickfix.Title.Substring("open ".Length)

        let edit =
          quickfix.Edit.Value.DocumentChanges.Value.[0]
            .Edits.[0]

        let openPos = calcOpenPos edit
        return (edit, ns, openPos)
      | Ok _ -> return failtest $"Quick fix on `{word}` doesn't contain open action"
    }

  let test
    (compareWithQuickFix: bool)
    (name: string option)
    (server: Async<FSharpLspServer * string>)
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

    let runner =
      if pending then
        ptestCaseAsync
      else
        testCaseAsync

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
      | Ok (Some res) ->
        Expect.isFalse res.IsIncomplete "Result is incomplete"
        let ci = res.Items |> Array.find (fun c -> c.Label = word)

        // now get details: `completionItem/resolve` (previous request was `textDocument/completion` -> List of all completions, but without details)
        match! server.CompletionItemResolve ci with
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

  testList
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
