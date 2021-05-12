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
  let normalizeMarkedString = function | MarkedString.WithLanguage l -> MarkedString.WithLanguage l
                                       | MarkedString.String s -> MarkedString.String (s.Replace("\r\n", "\n"))

  let normalizeHoverContent = function | HoverContent.MarkedStrings strings -> MarkedStrings (strings |> Array.map normalizeMarkedString)
                                       | HoverContent.MarkedString str -> MarkedString (normalizeMarkedString str)
                                       | HoverContent.MarkupContent content -> MarkupContent content

  testSequenced <| testList "Basic Tests" [
      testSequenced <| testList "Hover Tests" [

        testCaseAsync "simple symbol" (async {
          let! (server, path) = server
          let p : TextDocumentPositionParams =
            { TextDocument = { Uri = Path.FilePathToUri path}
              Position = { Line = 0; Character = 5 } }
          let! res = server.TextDocumentHover p
          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->
            let expected =
              MarkedStrings
                [|  MarkedString.WithLanguage {Language = "fsharp"; Value = "val t : int"}
                    MarkedString.String ""
                    MarkedString.String "*Full name: Script.t*"
                    MarkedString.String "*Assembly: Script*"|]

            Expect.equal (normalizeHoverContent res.Contents) expected "Hover test - simple symbol"
        })

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

        //Test to reproduce: https://github.com/ionide/ionide-vscode-fsharp/issues/1203
        testCaseAsync "Hover Tests - operator" (async {
          let! server, path = server
          let p : TextDocumentPositionParams =
            { TextDocument = { Uri = Path.FilePathToUri path}
              Position = { Line = 2; Character = 7}}
          let! res = server.TextDocumentHover p
          match res with
          | Result.Error e -> ()
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->
            let expected =
              MarkedStrings
                [|  MarkedString.WithLanguage {Language = "fsharp"; Value = "val ( .>> ): \n   x: int ->\n   y: int \n   -> int"}
                    MarkedString.String ""
                    MarkedString.String "*Full name: Script.( .>> )*"
                    MarkedString.String "*Assembly: Script*"|]

            Expect.equal (normalizeHoverContent res.Contents) expected "Hover test - let keyword"
        })

        //Test to reproduce: https://github.com/ionide/ionide-vscode-fsharp/issues/1203
        testCaseAsync "Hover Tests - operator ^" (async {
          let! server, path = server
          let p : TextDocumentPositionParams =
            { TextDocument = { Uri = Path.FilePathToUri path}
              Position = { Line = 4; Character = 6}}
          let! res = server.TextDocumentHover p
          match res with
          | Result.Error e -> ()
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->
            let expected =
              MarkedStrings
                [|  MarkedString.WithLanguage {Language = "fsharp"; Value = "val ( ^ ): \n   x: int ->\n   y: int \n   -> int"}
                    MarkedString.String ""
                    MarkedString.String "*Full name: Script.( ^ )*"
                    MarkedString.String "*Assembly: Script*"|]

            return Expect.equal (normalizeHoverContent res.Contents) expected "Hover test - let keyword"
        })

        testCaseAsync "Hover Tests - inline function with open generics" (async {
          let! server, path = server
          let p : TextDocumentPositionParams =
            { TextDocument = { Uri = Path.FilePathToUri path}
              Position = { Line = 6; Character = 13}} // middle of the `add` function
          let! res = server.TextDocumentHover p
          match res with
          | Result.Error e -> ()
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->
            let expected =
              MarkedStrings
                [|  MarkedString.WithLanguage {Language = "fsharp"; Value = "val add: \n   x: ^a (requires static member ( + ) )->\n   y: ^b (requires static member ( + ) )\n   -> ^c"}
                    MarkedString.String ""
                    MarkedString.String "*Full name: Script.add*"
                    MarkedString.String "*Assembly: Script*"|]

            return Expect.equal (normalizeHoverContent res.Contents) expected "should have rendered the inline generics"
        })

        testCaseAsync "Hover Tests - use of generic function renders fixed generic parameters" (async {
          let! server, path = server
          let p : TextDocumentPositionParams =
            { TextDocument = { Uri = Path.FilePathToUri path}
              Position = { Line = 8; Character = 14 } } // middle of the use of the `add` function, where generics are fixed
          let! res = server.TextDocumentHover p
          match res with
          | Result.Error e -> ()
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->
            let expected =
              MarkedStrings
                [|  MarkedString.WithLanguage {Language = "fsharp"; Value = "val add: \n   x: ^a (requires static member ( + ) )->\n   y: ^b (requires static member ( + ) )\n   -> ^c"}
                    MarkedString.String "\n\n**Generic Parameters**\n\n* `'a` is `int`\n* `'b` is `int`\n* `'c` is `int`"
                    MarkedString.String "*Full name: Script.add*"
                    MarkedString.String "*Assembly: Script*"|]

            return Expect.equal (normalizeHoverContent res.Contents) expected "should have rendered the inline generics"
        })
        testCaseAsync "cleanup" (async {
          let! server, _ = server
          do! server.Shutdown()
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
  let (|Tooltip|_|) (hover: Hover) =
    match hover with
    | { Contents = MarkedStrings [| MarkedString.WithLanguage { Language = "fsharp"; Value = tooltip }; MarkedString.String newline; MarkedString.String fullname; MarkedString.String assembly |] } -> Some tooltip
    | _ -> None

  let (|Description|_|) (hover: Hover) =
    match hover with
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

  let verifyTooltip line character expectedTooltip =
    testCaseAsync (sprintf "tooltip for line %d character %d should be '%s'" line character expectedTooltip) (async {
      let! server, scriptPath = server
      let pos: TextDocumentPositionParams = {
        TextDocument =  { Uri = sprintf "file://%s" scriptPath }
        Position = { Line = line; Character = character }
      }
      match! server.TextDocumentHover pos with
      | Ok (Some (Tooltip tooltip)) ->
        Expect.equal tooltip expectedTooltip (sprintf "Should have a tooltip of '%s'" expectedTooltip)
      | Ok response ->
        failtestf "Should have gotten tooltip but got %A" response
      | Result.Error errors ->
        failtestf "Error while getting tooltip: %A" errors
    })

  let verifyDescription line character expectedTooltip =
    testCaseAsync (sprintf "description for line %d character %d should be '%s" line character expectedTooltip) (async {
      let! server, scriptPath = server
      let pos: TextDocumentPositionParams = {
        TextDocument =  { Uri = sprintf "file://%s" scriptPath }
        Position = { Line = line; Character = character }
      }
      match! server.TextDocumentHover pos with
      | Ok (Some (Description tooltip)) ->
        Expect.equal tooltip expectedTooltip (sprintf "Should have a tooltip of '%s'" expectedTooltip)
      | Ok response ->
        failtestf "Should have gotten description but got %A" response
      | Result.Error errors ->
        failtestf "Error while getting description: %A" errors
    })

  let concatLines = String.concat Environment.NewLine

  testSequenced <|
    testList "tooltip evaluation" [
      testList "tests" [
        verifyTooltip 0 4 "val arrayOfTuples : (int * int) array" // verify that even the first letter of the tooltip triggers correctly
        verifyTooltip 0 5 "val arrayOfTuples : (int * int) array"
        verifyTooltip 1 5 "val listOfTuples : list<int * int>"
        verifyTooltip 2 5 "val listOfStructTuples : list<struct (int * int)>"
        verifyTooltip 3 5 "val floatThatShouldHaveGenericReportedInTooltip : float" //<MeasureOne>
        //verifyDescription 4 4 """**Description**\n\nPrint to a string using the given format.\n\n**Parameters**\n\n* `format`: The formatter.\n\n**Returns**\n\nThe formatted result.\n\n**Generic parameters**\n\n* `'T` is `string`"""
        verifyDescription 13 11 (concatLines ["**Description**"; ""; "\nMy super summary\n "; ""; "**Parameters**"; ""; "* `c`: foo"; "* `b`: bar"; "* `a`: baz"; ""; "**Returns**"; ""; ""])
        verifyTooltip 14 5 "val nestedTuples : int * ((int * int) * int)"
        verifyTooltip 15 5 "val nestedStructTuples : int * struct (int * int)"
        verifyTooltip 21 9 "val speed : float<m/s>"
      ]
      testCaseAsync "cleanup" (async {
        let! server, _ = server
        do! server.Shutdown()
      })
  ]
