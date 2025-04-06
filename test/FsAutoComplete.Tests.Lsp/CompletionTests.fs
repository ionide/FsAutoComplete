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
open Utils.Server
open FsAutoComplete.LspHelpers


let documentChanges path range text : DidChangeTextDocumentParams =
  { TextDocument =
      { Uri = Path.FilePathToUri path
        Version = 1 }

    ContentChanges =
      [| U2.C1
         <| { Range = range
              RangeLength = Some 0u
              Text = text } |] }

type CompletionKind =
  | Nada
  | Invoked
  | Char of char
  | Incomplete

let completion doc pos trigger : CompletionParams =
  { TextDocument = { Uri = Path.FilePathToUri doc }
    WorkDoneToken = None
    PartialResultToken = None
    Position = pos
    Context =
      match trigger with
      | Nada -> None
      | Invoked ->
        Some
          { TriggerKind = CompletionTriggerKind.Invoked
            TriggerCharacter = None }
      | Char c ->
        Some
          { TriggerKind = CompletionTriggerKind.TriggerCharacter
            TriggerCharacter = Some(string c) }
      | Incomplete ->
        Some
          { TriggerKind = CompletionTriggerKind.TriggerForIncompleteCompletions
            TriggerCharacter = None } }

let pos l c = { Line = l; Character = c }

let posRange l c = { Start = pos l c; End = pos l c }

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
        waitForDiagnosticErrorForFile "Script.fsx" events
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

          let completionParams: CompletionParams = completion path (pos 3u 9u) (Char '.') // the '.' in 'Async.'

          let! response = server.TextDocumentCompletion completionParams

          let handleCompletion (completions: CompletionItem seq) =
            Expect.isLessThan
              (Seq.length completions)
              100
              "shouldn't have an incredibly huge completion list for a simple module completion"

            let firstItem = completions |> Seq.head

            Expect.equal
              firstItem.Label
              "CancellationToken"
              "first member should be CancellationToken, since properties are preferred over functions"

          match response with
          | Ok(Some(CompletionItems completions)) -> handleCompletion completions
          | Ok None -> failtest "Should have gotten some completion items"
          | Error e -> failtestf "Got an error while retrieving completions: %A" e
        })


      testCaseAsync
        "simple completion for ending of a function call after text change"
        (async {
          let! server, path = server

          let lineUnderTest = """Path.GetDirectoryName("foo")"""
          let line = 15u
          let character = uint32 lineUnderTest.Length

          let textChange: DidChangeTextDocumentParams =
            documentChanges path (posRange line character) "."

          let! c = server.TextDocumentDidChange textChange |> Async.StartChild

          let completionParams: CompletionParams =
            completion path (pos line (character + 1u)) (Char '.')

          let! response = server.TextDocumentCompletion completionParams
          do! c

          match response with
          | Ok(Some(CompletionItems completions)) ->
            Expect.isLessThan
              completions.Length
              100
              "shouldn't have an incredibly huge completion list for a simple module completion"

            let firstItem = completions.[0]

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
          let line = 14u
          let character = uint32 lineUnderTest.Length

          let completionParams: CompletionParams =
            completion path (pos line character) (Char '.')

          let! response = server.TextDocumentCompletion completionParams

          match response with
          | Ok(Some(CompletionItems completions)) ->
            Expect.isLessThan
              completions.Length
              100
              "shouldn't have an incredibly huge completion list for a simple module completion"

            let firstItem = completions.[0]

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

          let line = 18u
          let lineUnderTest = "\"bareString\""
          let character = uint32 lineUnderTest.Length

          let textChange: DidChangeTextDocumentParams =
            documentChanges path (posRange line character) "."

          let! c = server.TextDocumentDidChange textChange |> Async.StartChild

          let completionParams: CompletionParams =
            completion path (pos line (character + 1u)) (Char '.')

          let! response = server.TextDocumentCompletion completionParams
          do! c

          match response with
          | Ok(Some(CompletionItems completions)) ->
            Expect.isLessThan
              completions.Length
              100
              "shouldn't have an incredibly huge completion list for a simple module completion"

            let firstItem = completions.[0]

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

          let line = 17u
          let lineUnderTest = """"bareString"."""
          let character = uint32 lineUnderTest.Length

          let completionParams: CompletionParams =
            completion path (pos line character) (Char '.')

          let! response = server.TextDocumentCompletion completionParams

          match response with
          | Ok(Some(CompletionItems completions)) ->
            Expect.isLessThan
              completions.Length
              100
              "shouldn't have an incredibly huge completion list for a simple module completion"

            let firstItem = completions.[0]

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

          let line = 21u
          let lineUnderTest = "[1;2;3]"
          let character = uint32 lineUnderTest.Length

          let textChange: DidChangeTextDocumentParams =
            documentChanges path (posRange line character) "."

          let! c = server.TextDocumentDidChange textChange |> Async.StartChild

          let completionParams: CompletionParams =
            completion path (pos line (character + 1u)) (Char '.')

          let! response = server.TextDocumentCompletion completionParams
          do! c

          match response with
          | Ok(Some(CompletionItems completions)) ->
            Expect.isLessThan
              completions.Length
              100
              "shouldn't have an incredibly huge completion list for a simple module completion"

            let firstItem = completions.[0]

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

          let line = 20u
          let lineUnderTest = "[1;2;3]."
          let character = uint32 lineUnderTest.Length

          let completionParams: CompletionParams =
            completion path (pos line character) (Char '.')

          let! response = server.TextDocumentCompletion completionParams

          match response with
          | Ok(Some(CompletionItems completions)) ->
            Expect.isLessThan
              completions.Length
              100
              "shouldn't have an incredibly huge completion list for a simple module completion"

            let firstItem = completions.[0]

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

          let completionParams: CompletionParams = completion path (pos 6u 5u) (Char '.')
          let! response = server.TextDocumentCompletion completionParams

          match response with
          | Ok(Some(CompletionItems completions)) ->
            Expect.isGreaterThanOrEqual
              completions.Length
              106
              "at time of writing the List module has 106 exposed members"

            let firstItem = completions.[0]

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

          let completionParams: CompletionParams = completion path (pos 8u 16u) (Char '.')
          let! response = server.TextDocumentCompletion completionParams

          match response with
          | Ok(Some(CompletionItems completions)) ->
            Expect.isGreaterThanOrEqual
              completions.Length
              106
              "at time of writing the List module has 106 exposed members"

            let firstItem = completions.[0]

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

          let completionParams: CompletionParams = completion path (pos 11u 10u) Invoked

          let! response = server.TextDocumentCompletion completionParams

          match response with
          | Ok(Some(CompletionItems completions)) ->
            Expect.isLessThan
              completions.Length
              300
              "shouldn't have a very long list of completion items that are only types"

            Expect.isGreaterThanOrEqual
              completions.Length
              100
              "should have a reasonable number of completion items that are only types"

            Expect.exists completions (fun item -> item.Label = "list") "completion should contain the list type"
          | Ok None -> failtest "Should have gotten some completion items"
          | Error e -> failtestf "Got an error while retrieving completions: %A" e
        })

      testCaseAsync
        "completion before first character of expression"
        (async {
          let! server, path = server

          let completionParams: CompletionParams = completion path (pos 8u 12u) Invoked

          let! response = server.TextDocumentCompletion completionParams

          match response with
          | Ok(Some(CompletionItems completions)) ->
            Expect.isGreaterThan completions.Length 100 "should have a very long list of all symbols"
            let firstItem = completions.[0]

            Expect.equal
              firstItem.Label
              "abs"
              "first member should be async, alphabetically first in the full symbol list"
          | Ok None -> failtest "Should have gotten some completion items"
          | Error e -> failtestf "Got an error while retrieving completions: %A" e
        })

      testCaseAsync
        "completion after first character of expression"
        (async {
          let! server, path = server

          let completionParams: CompletionParams = completion path (pos 8u 11u) Invoked
          let! response = server.TextDocumentCompletion completionParams

          match response with
          | Ok(Some(CompletionItems completions)) ->
            Expect.isGreaterThan completions.Length 100 "should have a very long list of all symbols"
            let firstItem = completions.[0]

            Expect.equal
              firstItem.Label
              "abs"
              "first member should be async, alphabetically first in the full symbol list"
          | Ok None -> failtest "Should have gotten some completion items"
          | Error e -> failtestf "Got an error while retrieving completions: %A" e
        })

      testCaseAsync
        "no backticks in completionitem signatures"
        (asyncResult {
          let! server, path = server

          let completionParams: CompletionParams = completion path (pos 3u 9u) Invoked

          let! CompletionItems response = server.TextDocumentCompletion completionParams |> AsyncResult.map Option.get

          let! resolved = server.CompletionItemResolve(Seq.head response)

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

          let completionParams: CompletionParams = completion path (pos 23u 8u) (Char '.')
          let! response = server.TextDocumentCompletion completionParams

          match response with
          | Ok(Some(CompletionItems completions)) ->
            Expect.isGreaterThanOrEqual
              completions.Length
              106
              "at time of writing the List module has 106 exposed members"

            let firstItem = completions.[0]

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

          let completionParams: CompletionParams = completion path (pos 24u 9u) (Char '.')
          let! response = server.TextDocumentCompletion completionParams

          match response with
          | Ok(Some(CompletionItems completions)) ->
            Expect.isGreaterThanOrEqual
              completions.Length
              106
              "at time of writing the List module has 106 exposed members"

            let firstItem = completions.[0]

            Expect.equal
              firstItem.Label
              "Empty"
              "first member should be List.Empty, since properties are preferred over functions"
          | Ok None -> failtest "Should have gotten some completion items"
          | Error e -> failtestf "Got an error while retrieving completions: %A" e
        })

      testCaseAsync
        "completion in indented hash directive"
        (async {
          let! server, path = server

          let completionParams: CompletionParams = completion path (pos 26u 4u) Invoked
          let! response = server.TextDocumentCompletion completionParams

          match response with
          | Ok(Some(CompletionItems completions)) ->
            Expect.isGreaterThanOrEqual completions.Length 2 "endif and else should be listed"

            let labels = completions |> Array.map _.Label
            Expect.contains labels "#else" "#else is a possible directive"
            Expect.contains labels "#endif" "#endif is another possible directive"

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

          let p: CompletionParams = completion path (pos 8u 2u) Nada

          let! res = server.TextDocumentCompletion p

          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok(Some(CompletionItems res)) ->

            Expect.equal res.Length 2 "Autocomplete has all symbols"
            Expect.exists res (fun n -> n.Label = "func") "Autocomplete contains given symbol"
            Expect.exists res (fun n -> n.Label = "sample func") "Autocomplete contains given symbol"
        })

      testCaseAsync
        "Get Autocomplete namespace"
        (async {
          let! server, path = serverConfig

          let p: CompletionParams = completion path (pos 10u 2u) Nada
          let! res = server.TextDocumentCompletion p

          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok(Some(CompletionItems res)) ->
            //TODO
            // Expect.equal res.Items.Length 1 "Autocomplete has all symbols"
            Expect.exists res (fun n -> n.Label = "System") "Autocomplete contains given symbol"

        })

      testCaseAsync
        "Get Autocomplete namespace members"
        (async {
          let! server, path = serverConfig

          let p: CompletionParams = completion path (pos 12u 7u) Nada

          let! res = server.TextDocumentCompletion p

          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok(Some(CompletionItems res)) ->
            //TODO
            // Expect.equal res.Items.Length 1 "Autocomplete has all symbols"
            Expect.exists res (fun n -> n.Label = "DateTime") "Autocomplete contains given symbol"

        })

      testCaseAsync
        "Get Autocomplete module doublebackticked members"
        (async {
          let! server, path = serverConfig

          let p: CompletionParams = completion path (pos 14u 18u) Nada

          let! res = server.TextDocumentCompletion p

          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok(Some(CompletionItems res)) ->

            Expect.equal res.Length 1 "Autocomplete has all symbols"
            Expect.exists res (fun n -> n.Label = "z") "Autocomplete contains given symbol"
        })

      testCaseAsync
        "Autocomplete record members"
        (async {
          let! server, path = serverConfig

          let p: CompletionParams = completion path (pos 25u 4u) Nada

          let! res = server.TextDocumentCompletion p

          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok(Some(CompletionItems res)) ->
            Expect.exists res (fun n -> n.Label = "bar") "Autocomplete contains given symbol"
            Expect.exists res (fun n -> n.Label = "baz") "Autocomplete contains given symbol"
        })

      testCaseAsync
        "Autocomplete class constructor with properties"
        (async {
          let! server, path = serverConfig

          let p: CompletionParams = completion path (pos 32u 25u) Nada

          let! res = server.TextDocumentCompletion p

          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok(Some(CompletionItems res)) ->
            Expect.exists res (fun n -> n.Label = "Bar") "Autocomplete contains given symbol"
            Expect.exists res (fun n -> n.Label = "Baz") "Autocomplete contains given symbol"
        }) ]

  testSequenced
  <| testList
    "Autocomplete Tests"
    [ testList "Autocomplete within project files" (makeAutocompleteTestList server)
      testList "Autocomplete within script files" (makeAutocompleteTestList scriptServer) ]

let autoOpenTests state =
  let dirPath =
    Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "CompletionAutoOpenTests")

  let autoOpenServer =
    // Auto Open requires unopened things in completions -> External
    let config =
      { defaultConfigDto with
          ExternalAutocomplete = Some true
          ResolveNamespaces = Some true }

    Server.create (Some dirPath) config state


  let serverFor (scriptPath: string) scriptText =
    async {
      let! _document, _diagnostics = Server.openDocumentWithText scriptPath (String.join "\n" scriptText) autoOpenServer
      let _scriptName = Path.GetFileName scriptPath
      return (autoOpenServer, scriptPath)
    }

  let calcOpenPos (edit: TextEdit) =
    let text = edit.NewText
    let pos = edit.Range.Start

    let indentation = pos.Character + uint32 (text.Length - text.TrimStart().Length)

    { Line = pos.Line
      Character = indentation }

  let getQuickFix (server: IFSharpLspServer, path: string) (word: string, _ns: string) (cursor: Position) =
    async {
      let p =
        { CodeActionParams.TextDocument = { Uri = Path.FilePathToUri path }
          WorkDoneToken = None
          PartialResultToken = None
          Range = { Start = cursor; End = cursor }
          Context =
            { Diagnostics =
                [| { Range = { Start = cursor; End = cursor }
                     Severity = Some DiagnosticSeverity.Error
                     // Message required for QuickFix to fire ("is not defined")
                     Message = $"The value or constructor '{word}' is not defined."
                     Code = Some(U2.C1 39)
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

      let (|SingleEdit|_|) (action: CodeAction) =
        match action with
        | { Edit = Some { DocumentChanges = Some [| U4.C1 { Edits = [| U2.C1 edit |] } |] } } -> Some edit
        | _ -> None

      match! server.TextDocumentCodeAction p with
      | Error e -> return failtestf "Quick fix Request failed: %A" e
      | Ok None -> return failtest "Quick fix Request none"
      | Ok(Some(CodeActions(ContainsOpenAction({ Title = title } & SingleEdit edit)))) ->
        let ns = title.Substring("open ".Length)
        let openPos = calcOpenPos edit
        return (edit, ns, openPos)
      | Ok _ -> return failtest $"Quick fix on `{word}` doesn't contain open action"
    }

  let test
    (compareWithQuickFix: bool)
    (name: string option)
    (server: Async<CachedServer * string>)
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
      let! (server, filePath) = server
      let! server = server
      let server = server.Server
      let p: CompletionParams = completion filePath cursor Nada

      match! server.TextDocumentCompletion p with
      | Error e -> failtestf "Request failed: %A" e
      | Ok None -> failtest "Request none"
      | Ok(Some(U2.C1 _res)) -> failwith "Should be a CompletionList"
      | Ok(Some(U2.C2 res)) ->
        Expect.isFalse res.IsIncomplete "Result is incomplete"
        // with ExternalAutoComplete, completions are like "Regex (open System.Text.RegularExpressions)"
        let ci = res.Items |> Array.filter (fun c -> c.Label.Split()[0] = word)

        let ci =
          match ci with
          | [||] ->
            failwithf
              $"Couldn't find completion item for `{word}` among the items %A{res.Items |> Array.map (fun i -> i.Label)}"
          | [| ci |] -> ci
          | _ ->
            failwithf
              $"Multiple completion items for `{word}` (%A{ci |> Array.map (fun ci -> ci.Label)}) found among the items %A{res.Items |> Array.map (fun i -> i.Label)}"

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
            let! (_, _, qfOpenPos) = getQuickFix (server, filePath) (word, ns) cursor
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
        let calcN (current: uint32) (n: string) =
          let n = n.Trim()

          match n.[0] with
          | '|' ->
            //relative to indentation of current line
            let ind = uint32 (lineStr.Length - lineStr.TrimStart().Length)

            match n.Substring(1).Trim() with
            | "" -> ind
            | n -> uint32 (int ind + int n)
          | '+'
          | '-' ->
            // relative to current position
            uint32 (int current + int n)
          | _ ->
            // absolute
            uint32 n

        let (l, c) = (calcN line l, calcN column c)
        { Line = l; Character = c }
      | _ -> failwithf "Invalid data in line (%i,%i) '%s'" line column lineStr

    let extractData (lineNumber: uint32) (line: string) : (Position * Position * string) option =
      let m = regex.Match line

      if not m.Success then
        None
      else
        let data = m.Groups.["data"]
        let (l, c) = (lineNumber, uint32 m.Index)
        let openPos = parseData (l, c) line data.Value
        let cursorPos = { Line = l; Character = c }

        Some(cursorPos, openPos, line.Remove(m.Index, m.Length))

    let positions = ResizeArray()
    let lines = ResizeArray()

    for i, line in Seq.indexed (System.IO.File.ReadAllLines path) do
      if line.Contains "(*" then
        do
          match extractData (uint32 i) line with
          | Some(cursorPos, openPos, trimmedLine) ->
            positions.Add(cursorPos, openPos)
            lines.Add trimmedLine
          | None -> ()
      else
        lines.Add line

    positions, lines


  let testScript name scriptName =
    testList
      name
      [ let scriptPath = Path.Combine(dirPath, scriptName)
        let testPositions, cleanScript = readData scriptPath
        let server = serverFor scriptPath cleanScript

        yield!
          testPositions
          |> Seq.map (fun (cursor, expectedOpen) ->
            test false None server ("Regex", "System.Text.RegularExpressions") cursor expectedOpen false) ]

  let _ptestScript name scriptName =
    testList
      name
      [ let scriptPath = Path.Combine(dirPath, scriptName)
        let testPositions, cleanScript = readData scriptPath
        let server = serverFor scriptPath cleanScript

        yield!
          testPositions
          |> Seq.map (fun (cursor, expectedOpen) ->
            test false None server ("Regex", "System.Text.RegularExpressions") cursor expectedOpen true) ]

  testList
    "Completion.AutoOpen"
    [
      // NOTE: Positions are ZERO-based!: { Line = 3; Character = 9 } -> Line 4, Column 10 in editor display
      testScript "with root module with new line" "ModuleWithNewLine.fsx"
      testScript "with root module" "Module.fsx"
      testScript "with root module with open" "ModuleWithOpen.fsx"
      testScript "with root module with open and new line" "ModuleWithOpenAndNewLine.fsx"
      testScript "with namespace with new line" "NamespaceWithNewLine.fsx"
      testScript "with namespace" "Namespace.fsx"
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

        let p: CompletionParams = completion path (pos line character) Invoked
        let! res = server.TextDocumentCompletion p

        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok(Some res) -> expects res
      })

  let makeAutocompleteTestList (serverConfig: (IFSharpLspServer * string) Async) =
    [ makeAutocompleteTest
        serverConfig
        "Autocomplete for Array.map contains no backticks"
        (0u, 8u)
        (fun (CompletionItems res) ->
          let n = res |> Array.tryFind (fun i -> i.Label = "Array.map")
          Expect.isSome n "Completion doesn't exist"
          Expect.equal n.Value.InsertText (Some "Array.map") "Autocomplete for Array.map contains backticks")

      makeAutocompleteTest
        serverConfig
        "Autocomplete for ``a.b`` contains backticks"
        (2u, 1u)
        (fun (CompletionItems res) ->
          let n = res |> Array.tryFind (fun i -> i.Label = "a.b")
          Expect.isSome n "Completion doesn't exist"
          Expect.equal n.Value.InsertText (Some "``a.b``") "Autocomplete for a.b contains no backticks")

      testCaseAsync
        "Autocompletes with same label have different description"
        (asyncResult {
          let! server, path = serverConfig

          let p: CompletionParams = completion path (pos 3u 4u) Invoked

          let! CompletionItems response = server.TextDocumentCompletion p |> AsyncResult.map Option.get

          let! items =
            response
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

      makeAutocompleteTest
        serverConfig
        "Check Autocomplete for System.Text.RegularExpressions.Regex"
        (4u, 5u)
        (fun (CompletionItems res) ->
          let n =
            res
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

      makeAutocompleteTest serverConfig "Autocomplete for Result is just Result" (5u, 6u) (fun (CompletionItems res) ->
        let n = res |> Array.tryFind (fun i -> i.Label = "Result")
        Expect.isSome n "Completion doesn't exist"
        Expect.equal n.Value.InsertText (Some "Result") "Autocomplete contains given symbol") ]

  testSequenced
  <| testList
    "fullNameExternalAutocompleteTest Tests"
    [ testList "fullNameExternalAutocompleteTest within project files" (makeAutocompleteTestList server)
      testList "fullNameExternalAutocompleteTest within script files" (makeAutocompleteTestList scriptServer) ]
