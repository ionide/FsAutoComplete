module FsAutoComplete.Tests.CoreTest

open System
open Expecto
open System.IO
open LanguageServerProtocol
open LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open Helpers
open FsToolkit.ErrorHandling

///Test for initialization of the server
let initTests state =
  testCaseAsync "InitTest" (async {
    let (server, event) = createServer state

    let p : InitializeParams =
      { ProcessId = Some 1
        RootPath = Some __SOURCE_DIRECTORY__
        RootUri = None
        InitializationOptions = Some (Server.serialize defaultConfigDto)
        Capabilities = Some clientCaps
        trace = None}

    let! result = server.Initialize p
    match result with
    | Result.Ok res ->
      Expect.equal res.Capabilities.CodeActionProvider (Some true) "Code Action Provider"
      Expect.equal res.Capabilities.CodeLensProvider (Some {CodeLensOptions.ResolveProvider = Some true}) "Code Lens Provider"
      Expect.equal res.Capabilities.DefinitionProvider (Some true) "Definition Provider"
      Expect.equal res.Capabilities.DocumentFormattingProvider (Some true) "Document Formatting Provider"
      Expect.equal res.Capabilities.DocumentHighlightProvider (Some true) "Document Highlighting Provider"
      Expect.equal res.Capabilities.DocumentLinkProvider None "Document Link Provider"
      Expect.equal res.Capabilities.DocumentOnTypeFormattingProvider None "Document OnType Formatting Provider"
      Expect.equal res.Capabilities.DocumentRangeFormattingProvider (Some false) "Document Range Formatting Provider"
      Expect.equal res.Capabilities.DocumentSymbolProvider (Some true) "Document Symbol Provider"
      Expect.equal res.Capabilities.ExecuteCommandProvider None "Execute Command Provider"
      Expect.equal res.Capabilities.Experimental None "Experimental"
      Expect.equal res.Capabilities.HoverProvider (Some true) "Hover Provider"
      Expect.equal res.Capabilities.ImplementationProvider (Some true) "Implementation Provider"
      Expect.equal res.Capabilities.ReferencesProvider (Some true) "References Provider"
      Expect.equal res.Capabilities.RenameProvider (Some true) "Rename Provider"
      Expect.equal res.Capabilities.SignatureHelpProvider (Some {
        TriggerCharacters = Some [| '('; ','; ' '|]
        RetriggerCharacters = Some [| ','; ')'; ' ' |]
      } ) "Signature Help Provider"
      let td =
        { TextDocumentSyncOptions.Default with
            OpenClose = Some true
            Change = Some TextDocumentSyncKind.Full
            Save = Some { IncludeText = Some true }
        }
      Expect.equal res.Capabilities.TextDocumentSync (Some td) "Text Document Provider"
      Expect.equal res.Capabilities.TypeDefinitionProvider (Some true) "Type Definition Provider"
      Expect.equal res.Capabilities.WorkspaceSymbolProvider (Some true) "Workspace Symbol Provider"
      Expect.equal res.Capabilities.FoldingRangeProvider (Some true) "Folding Range Provider active"
    | Result.Error e ->
      failtest "Initialization failed"
  })

///Tests for basic operations like hover, getting document symbols or code lens on simple file
let basicTests state =
  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "BasicTest")
      let! (server, event) = serverInitialize path defaultConfigDto state
      let path = Path.Combine(path, "Script.fsx")
      let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path }
      do! server.TextDocumentDidOpen tdop
      do! waitForParseResultsForFile "Script.fsx" event |> Async.Ignore
      return (server, path)
    }
    |> Async.Cache

  /// normalizes the line endings in returned markdown strings for cross-platform comparisons
  let normalizeMarkedString = function | MarkedString.WithLanguage { Language = lang; Value = v } -> MarkedString.WithLanguage { Language = lang; Value = v.Replace("\r\n", "\n") }
                                       | MarkedString.String s -> MarkedString.String (s.Replace("\r\n", "\n"))

  let normalizeHoverContent = function | HoverContent.MarkedStrings strings -> MarkedStrings (strings |> Array.map normalizeMarkedString)
                                       | HoverContent.MarkedString str -> MarkedString (normalizeMarkedString str)
                                       | HoverContent.MarkupContent content -> MarkupContent content

  testSequenced <| testList "Basic Tests" [
      testSequenced <| ftestList "Hover Tests" [
        testCaseAsync "Hover Tests - let keyword" (async {
          let! server, path = server
          let p : TextDocumentPositionParams =
            { TextDocument = { Uri = Path.FilePathToUri path}
              Position = { Line = 0; Character = 2}}
          let! res = server.TextDocumentHover p
          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->
            let expected =
              MarkedStrings
                [|  MarkedString.WithLanguage {Language = "fsharp"; Value = "let"}
                    MarkedString.String "**Description**\n\n\nUsed to associate, or bind, a name to a value or function.\n"|]

            Expect.equal (normalizeHoverContent res.Contents) expected "Hover test - let keyword"
        })

        testCaseAsync "Hover Tests - out of position" (async {
          let! server, path = server
          let p : TextDocumentPositionParams =
            { TextDocument = { Uri = Path.FilePathToUri path}
              Position = { Line = 1; Character = 2}}
          let! res = server.TextDocumentHover p
          match res with
          | Result.Error e -> ()
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->
            failtest "Expected failure"
        })
      ]
  ]

///Tests for getting and resolving code(line) lenses with enabled reference code lenses
let codeLensTest state =
  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "CodeLensTest")
      let! (server, event) = serverInitialize path {defaultConfigDto with EnableReferenceCodeLens = Some true} state
      let projectPath = Path.Combine(path, "CodeLensTest.fsproj")
      do! parseProject projectPath server
      let path = Path.Combine(path, "Script.fs")
      let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path}
      do! server.TextDocumentDidOpen tdop
      return (server, path)
    }
    |> Async.Cache

  testSequenced <| testList "Code Lens Tests" [
      testCaseAsync "Get Code Lens" (async {
          let! (server, path) = server
          let p : CodeLensParams = { TextDocument = { Uri = Path.FilePathToUri path}}
          let! res = server.TextDocumentCodeLens p
          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->
            Expect.equal res.Length 20 "Get Code Lens has all locations"
      })
      testCaseAsync "Resolve Code Lens" (async {
          let! (server, path) = server
          let p : CodeLensParams = { TextDocument = { Uri = Path.FilePathToUri path}}
          let! res = server.TextDocumentCodeLens p
          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some result) ->
            let cl = result.[1]
            let! res = server.CodeLensResolve cl
            let cl = result.[11]
            let! res2 = server.CodeLensResolve cl
            let cl = result.[10]
            let! res3 = server.CodeLensResolve cl
            match res, res2 with //TODO: Match res3 when FCS is fixed
            | Result.Ok cl, Result.Ok cl2 ->
              //TODO
              //Expect.equal cl.Command.Value.Title "1 Reference" "Code Lens contains reference count"
              Expect.equal cl2.Command.Value.Title "string -> unit" "Code Lens contains signature"
            | e -> failtestf "Request failed: %A" e
      })

      testCaseAsync "Resolve Code Lens 2" (async {
          let! (server, path) = server
          let p : CodeLensParams = { TextDocument = { Uri = Path.FilePathToUri path}}
          let! res = server.TextDocumentCodeLens p
          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some result) ->
            let cl = result.[3]
            let! res = server.CodeLensResolve cl
            let cl = result.[14]
            let! res2 = server.CodeLensResolve cl
            match res, res2 with
            | Result.Ok cl, Result.Ok cl2 ->
              //TODO
              //Expect.equal cl.Command.Value.Title "1 Reference" "Code Lens contains reference count"
              Expect.equal cl2.Command.Value.Title "unit -> (int64 -> System.DateTime)" "Code Lens contains signature"

            | e -> failtestf "Request failed: %A" e
      })

      testCaseAsync "cleanup" (async {
          let! server, _ = server
          do! server.Shutdown()
        })
  ]

///Tests for getting document symbols
let documentSymbolTest state =
  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "DocumentSymbolTest")
      let! (server, event) = serverInitialize path defaultConfigDto state
      let projectPath = Path.Combine(path, "DocumentSymbolTest.fsproj")
      do! parseProject projectPath server
      let path = Path.Combine(path, "Script.fsx")
      let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path}
      do! server.TextDocumentDidOpen tdop
      return (server, path)
    }
    |> Async.Cache

  testSequenced <| testList "Document Symbols Tests" [
      testCaseAsync "Get Document Symbols" (async {
        let! server, path = server
        let p : DocumentSymbolParams = { TextDocument = { Uri = Path.FilePathToUri path}}
        let! res = server.TextDocumentDocumentSymbol p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->

          Expect.equal res.Length 15 "Document Symbol has all symbols"
          Expect.exists res (fun n -> n.Name = "MyDateTime" && n.Kind = SymbolKind.Class) "Document symbol contains given symbol"
      })
      testCaseAsync "cleanup" (async {
          let! server, _ = server
          do! server.Shutdown()
        })
  ]

let foldingTests state =
  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "FoldingTests")

      let! (server, event) = serverInitialize path defaultConfigDto state
      do! waitForWorkspaceFinishedParsing event
      let libraryPath = Path.Combine(path, "Library.fs")
      let libFile = loadDocument libraryPath
      let tdop : DidOpenTextDocumentParams = { TextDocument = libFile }
      do! server.TextDocumentDidOpen tdop
      return server, libraryPath
    }
    |> Async.Cache

  testList "folding tests" [
    testCaseAsync "can get ranges for sample file" (async {
      let! server, libraryPath = server
      let! rangeResponse = server.TextDocumentFoldingRange({ TextDocument = { Uri = Path.FilePathToUri libraryPath } })
      match rangeResponse with
      | Ok(Some(ranges)) ->
        Expect.hasLength ranges 3 "Should be three ranges: one comment, one module, one let-binding"
      | Ok(None) -> failtestf "No ranges found in file, problem parsing?"
      | LspResult.Error e -> failtestf "Error from range LSP call: %A" e
    })

    testCaseAsync "cleanup" (async {
      let! server, _ = server
      do! server.Shutdown()
    })
  ]


let tooltipTests state =
  let (|Signature|_|) (hover: Hover) =
    match hover with
    | { Contents = MarkedStrings [| MarkedString.WithLanguage { Language = "fsharp"; Value = tooltip }; MarkedString.String newline; MarkedString.String fullname; MarkedString.String assembly |] } -> Some tooltip
    | _ -> None

  let (|Description|_|) (hover: Hover) =
    match hover with
    | { Contents = MarkedStrings [| MarkedString.WithLanguage { Language = "fsharp"; Value = tooltip }; MarkedString.String description; |] } -> Some description
    | { Contents = MarkedStrings [| MarkedString.WithLanguage { Language = "fsharp"; Value = tooltip }; MarkedString.String description; MarkedString.String fullname; MarkedString.String assembly |] } -> Some description
    | _ -> None

  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "Tooltips")
      let scriptPath = Path.Combine(path, "Script.fsx")
      let! (server, events) = serverInitialize path defaultConfigDto state
      do! waitForWorkspaceFinishedParsing events
      do! server.TextDocumentDidOpen { TextDocument = loadDocument scriptPath }
      match! waitForParseResultsForFile "Script.fsx" events with
      | Ok () ->
        () // all good, no parsing/checking errors
      | Core.Result.Error errors ->
        failtestf "Errors while parsing script %s: %A" scriptPath errors

      return server, scriptPath
    }
    |> Async.Cache

  let verifySignature line character expectedSignature =
    testCaseAsync (sprintf "tooltip for line %d character %d should be '%s'" line character expectedSignature) (async {
      let! server, scriptPath = server
      let pos: TextDocumentPositionParams = {
        TextDocument =  { Uri = sprintf "file://%s" scriptPath }
        Position = { Line = line; Character = character }
      }
      match! server.TextDocumentHover pos with
      | Ok (Some (Signature signature)) ->
        Expect.equal signature expectedSignature (sprintf "Should have a signature of '%s'" expectedSignature)
      | Ok response ->
        failtestf "Should have gotten signature but got %A" response
      | Result.Error errors ->
        failtestf "Error while getting signature: %A" errors
    })

  let concatLines = String.concat Environment.NewLine

  let verifyDescription line character expectedDescription =
    let expectedDescription = concatLines expectedDescription
    testCaseAsync (sprintf "description for line %d character %d should be '%s" line character expectedDescription) (async {
      let! server, scriptPath = server
      let pos: TextDocumentPositionParams = {
        TextDocument =  { Uri = sprintf "file://%s" scriptPath }
        Position = { Line = line; Character = character }
      }
      match! server.TextDocumentHover pos with
      | Ok (Some (Description description)) ->
        Expect.equal description expectedDescription (sprintf "Should have a description of '%s'" expectedDescription)
      | Ok response ->
        failtestf "Should have gotten description but got %A" response
      | Result.Error errors ->
        failtestf "Error while getting description: %A" errors
    })


  testSequenced <|
    ftestList "tooltip evaluation" [
      testList "tests" [
        verifyDescription 0 2 ["**Description**";"";"";"Used to associate, or bind, a name to a value or function.";""] // `let` keyword
        verifySignature 0 4 "val arrayOfTuples : (int * int) array" // verify that even the first letter of the tooltip triggers correctly
        verifySignature 0 5 "val arrayOfTuples : (int * int) array" // inner positions trigger
        verifySignature 1 5 "val listOfTuples : list<int * int>" // verify we default to prefix-generics style
        verifySignature 2 5 "val listOfStructTuples : list<struct (int * int)>" // verify we render struct tuples in a round-tripabble format
        verifySignature 3 5 "val floatThatShouldHaveGenericReportedInTooltip : float" // verify we strip <MeasureOne> measure annotations
        verifyDescription 4 4 ["**Description**";"";"Print to a string using the given format.";"";"**Parameters**";"";"* `format`: The formatter.";"";"**Returns**";"";"The formatted result.";"";"**Generic Parameters**";"";"* `'T` is `string`"]
        verifyDescription 13 11 ["**Description**"; ""; ""; "My super summary"; " "; ""; "**Parameters**"; ""; "* `c`: foo"; "* `b`: bar"; "* `a`: baz"; ""; "**Returns**"; ""; ""]
        verifySignature 14 5 "val nestedTuples : int * ((int * int) * int)" // verify that tuples render correctly (parens, etc)
        verifySignature 15 5 "val nestedStructTuples : int * struct (int * int)" // verify we can differentiate between struct and non-struct tuples
        verifySignature 21 9 "val speed : float<m/s>" // verify we nicely-render measure annotations
        // verify formatting of function-parameters to values. NOTE: we want to wrap them in parens for user clarity eventually.
        verifySignature 26 5 (concatLines ["val funcWithFunParam:"; "   f: int -> unit ->"; "   i: int";"   -> unit"])
        // verify formatting of tuple args.  NOTE: we want to wrap tuples in parens for user clarify eventually.
        verifySignature 30 12 (concatLines ["val funcWithTupleParam:";"   : int *"; "   : int";"   -> int * int"])
        // verify formatting of struct tuple args in parameter tooltips.
        verifySignature 32 12 (concatLines ["val funcWithStructTupleParam:";"   f: struct (int * int)";"   -> struct (int * int)"])
        verifySignature 36 15 (concatLines ["member Foo:"; "   stuff: int * int * int"; "       -> int"])
        verifySignature 37 15 (concatLines ["member Bar:"; "   a: int *";"   b: int *";"   c: int";"   -> int"])
        // verify formatting for multi-char operators
        verifySignature 39 7  (concatLines ["val ( .>> ):"; "   x: int ->"; "   y: int";"   -> int"])
        // verify formatting for single-char operators
        verifySignature 41 6  (concatLines ["val ( ^ ):"; "   x: int ->"; "   y: int"; "   -> int"])
        // verify rendeirng of generic constraints
        verifySignature 43 13 (concatLines ["val add:"; "   x: ^a (requires static member ( + ) ) ->"; "   y: ^b (requires static member ( + ) )"; "   -> ^c"])
        // verify rendering of solved generic constraints in tooltips for members where they are solved
        verifyDescription 45 15 ["";"";"**Generic Parameters**";"";"* `'a` is `int`";"* `'b` is `int`";"* `'c` is `int`"]
      ]
      testCaseAsync "cleanup" (async {
        let! server, _ = server
        do! server.Shutdown()
      })
  ]
