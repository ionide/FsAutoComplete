module FsAutoComplete.Tests.CoreTest

open System
open Expecto
open System.IO
open LanguageServerProtocol
open LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open Helpers

///Test for initialization of the server
let initTests toolsPath =
  test "InitTest" {
    let (server, event) = createServer(toolsPath)

    let p : InitializeParams =
      { ProcessId = Some 1
        RootPath = Some __SOURCE_DIRECTORY__
        RootUri = None
        InitializationOptions = Some (Server.serialize defaultConfigDto)
        Capabilities = Some clientCaps
        trace = None}

    let result = server.Initialize p |> Async.RunSynchronously
    match result with
    | Result.Ok res ->
      Expect.equal res.Capabilities.CodeActionProvider (Some true) "Code Action Provider"
      Expect.equal res.Capabilities.CodeLensProvider (Some {CodeLensOptions.ResolveProvider = Some true}) "Code Lens Provider"
      Expect.equal res.Capabilities.DefinitionProvider (Some true) "Definition Provider"
      Expect.equal res.Capabilities.DocumentFormattingProvider (Some true) "Document Formatting Provider"
      Expect.equal res.Capabilities.DocumentHighlightProvider (Some true) "Document Highligthing Provider"
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
        TriggerCharacters = Some [| '('; ','|]
        RetriggerCharacters = Some [| ')' |]
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
      failwith "Initialization failed"
  }


///Tests for basic operations like hover, getting document symbols or code lens on simple file
let basicTests toolsPath =
  let serverStart = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "BasicTest")
    let (server, event) = serverInitialize path defaultConfigDto toolsPath
    let projectPath = Path.Combine(path, "BasicTest.fsproj")
    parseProject projectPath server |> Async.RunSynchronously
    let path = Path.Combine(path, "Script.fs")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path}

    do server.TextDocumentDidOpen tdop |> Async.RunSynchronously
    (server, path)
    )
  let serverTest f () =
    let (server, path) = serverStart.Value
    f server path

  testSequenced <| testList "Basic Tests" [
      testSequenced <| testList "Hover Tests" [

        testCase "Hover Tests - simple symbol" (serverTest (fun server path ->
          let p : TextDocumentPositionParams =
            { TextDocument = { Uri = Path.FilePathToUri path}
              Position = { Line = 0; Character = 4}}
          let res = server.TextDocumentHover p |> Async.RunSynchronously
          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->
            let expected =
              MarkedStrings
                [|  MarkedString.WithLanguage {Language = "fsharp"; Value = "val t : int"}
                    MarkedString.String ""
                    MarkedString.String "*Full name: Script.t*"
                    MarkedString.String "*Assembly: BasicTest*"|]

            Expect.equal res.Contents expected "Hover test - simple symbol"
        ))

        testCase "Hover Tests - let keyword" (serverTest (fun server path ->
          let p : TextDocumentPositionParams =
            { TextDocument = { Uri = Path.FilePathToUri path}
              Position = { Line = 0; Character = 2}}
          let res = server.TextDocumentHover p |> Async.RunSynchronously
          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->
            let expected =
              MarkedStrings
                [|  MarkedString.WithLanguage {Language = "fsharp"; Value = "let"}
                    MarkedString.String "**Description**\n\n\nUsed to associate, or bind, a name to a value or function.\n"|]

            Expect.equal res.Contents expected "Hover test - let keyword"
        ))

        testCase "Hover Tests - out of position" (serverTest (fun server path ->
          let p : TextDocumentPositionParams =
            { TextDocument = { Uri = Path.FilePathToUri path}
              Position = { Line = 1; Character = 2}}
          let res = server.TextDocumentHover p |> Async.RunSynchronously
          match res with
          | Result.Error e -> ()
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->
            failtest "Expected failure"
        ))

        //Test to reproduce: https://github.com/ionide/ionide-vscode-fsharp/issues/1203
        testCase "Hover Tests - operator" (serverTest (fun server path ->
          let p : TextDocumentPositionParams =
            { TextDocument = { Uri = Path.FilePathToUri path}
              Position = { Line = 2; Character = 7}}
          let res = server.TextDocumentHover p |> Async.RunSynchronously
          match res with
          | Result.Error e -> ()
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->
            let expected =
              MarkedStrings
                [|  MarkedString.WithLanguage {Language = "fsharp"; Value = "val ( .>> ): \n   x: int ->\n   y: int \n   -> int"}
                    MarkedString.String ""
                    MarkedString.String "*Full name: Script.( .>> )*"
                    MarkedString.String "*Assembly: BasicTest*"|]

            Expect.equal res.Contents expected "Hover test - let keyword"
        ))

        //Test to reproduce: https://github.com/ionide/ionide-vscode-fsharp/issues/1203
        testCase "Hover Tests - operator ^" (serverTest (fun server path ->
          let p : TextDocumentPositionParams =
            { TextDocument = { Uri = Path.FilePathToUri path}
              Position = { Line = 4; Character = 6}}
          let res = server.TextDocumentHover p |> Async.RunSynchronously
          match res with
          | Result.Error e -> ()
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->
            let expected =
              MarkedStrings
                [|  MarkedString.WithLanguage {Language = "fsharp"; Value = "val ( ^ ): \n   x: int ->\n   y: int \n   -> int"}
                    MarkedString.String ""
                    MarkedString.String "*Full name: Script.( ^ )*"
                    MarkedString.String "*Assembly: BasicTest*"|]

            Expect.equal res.Contents expected "Hover test - let keyword"
        ))
      ]
      testSequenced <| testList "Document Symbol Tests" [
        testCase "Document Symbol" (serverTest (fun server path ->
          let p : DocumentSymbolParams = { TextDocument = { Uri = Path.FilePathToUri path}}
          let res = server.TextDocumentDocumentSymbol p |> Async.RunSynchronously
          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->

            Expect.equal res.Length 4  "Document Symbol has all symbols"
        ))
      ]
      testSequenced <| testList "Code Lens Tests" [
        testCase "Get Code Lens" (serverTest (fun server path ->
          let p : CodeLensParams = { TextDocument = { Uri = Path.FilePathToUri path}}
          let res = server.TextDocumentCodeLens p |> Async.RunSynchronously
          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->

            Expect.equal res.Length 3 "Get Code Lens has all locations"
        ))

        testCase "Resolve Code Lens" (serverTest (fun server path ->
          let p : CodeLensParams = { TextDocument = { Uri = Path.FilePathToUri path}}
          let res = server.TextDocumentCodeLens p |> Async.RunSynchronously
          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->
            let cl = res.[0]
            let res = server.CodeLensResolve cl |> Async.RunSynchronously
            match res with
            | Result.Error e -> failtestf "Request failed: %A" e
            | Result.Ok cl ->
              Expect.equal cl.Command.Value.Title "int -> int -> int" "Code Lens contains signature"
        ))
      ]

  ]


///Tests for getting and resolving code(line) lenses with enabled reference code lenses
let codeLensTest toolsPath =
  let serverStart = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "CodeLensTest")
    let (server, event) = serverInitialize path {defaultConfigDto with EnableReferenceCodeLens = Some true} toolsPath
    let projectPath = Path.Combine(path, "CodeLensTest.fsproj")
    parseProject projectPath server |> Async.RunSynchronously
    let path = Path.Combine(path, "Script.fs")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path}
    do server.TextDocumentDidOpen tdop |> Async.RunSynchronously
    (server, path)
  )
  let serverTest f () =
    let (server, path) = serverStart.Value
    f server path

  testSequenced <| testList "Code Lens Tests" [
      testCase "Get Code Lens" (serverTest (fun server path ->
          let p : CodeLensParams = { TextDocument = { Uri = Path.FilePathToUri path}}
          let res = server.TextDocumentCodeLens p |> Async.RunSynchronously
          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->

            Expect.equal res.Length 18 "Get Code Lens has all locations"
      ))
      testCase "Resolve Code Lens" (serverTest (fun server path ->
          let p : CodeLensParams = { TextDocument = { Uri = Path.FilePathToUri path}}
          let res = server.TextDocumentCodeLens p |> Async.RunSynchronously
          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some result) ->
            let cl = result.[0]
            let res = server.CodeLensResolve cl |> Async.RunSynchronously
            let cl = result.[9]
            let res2 = server.CodeLensResolve cl |> Async.RunSynchronously
            match res, res2 with
            | Result.Ok cl, Result.Ok cl2 ->
              //TODO
              //Expect.equal cl.Command.Value.Title "1 Reference" "Code Lens contains reference count"
              Expect.equal cl2.Command.Value.Title "string -> unit" "Code Lens contains signature"

            | e -> failtestf "Request failed: %A" e
      ))

      testCase "Resolve Code Lens 2" (serverTest (fun server path ->
          let p : CodeLensParams = { TextDocument = { Uri = Path.FilePathToUri path}}
          let res = server.TextDocumentCodeLens p |> Async.RunSynchronously
          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some result) ->
            let cl = result.[3]
            let res = server.CodeLensResolve cl |> Async.RunSynchronously
            let cl = result.[12]
            let res2 = server.CodeLensResolve cl |> Async.RunSynchronously
            match res, res2 with
            | Result.Ok cl, Result.Ok cl2 ->
              //TODO
              //Expect.equal cl.Command.Value.Title "1 Reference" "Code Lens contains reference count"
              Expect.equal cl2.Command.Value.Title "unit -> (int64 -> System.DateTime)" "Code Lens contains signature"

            | e -> failtestf "Request failed: %A" e
      ))
  ]

///Tests for getting document symbols
let documentSymbolTest toolsPath =
  let serverStart = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "DocumentSymbolTest")
    let (server, event) = serverInitialize path defaultConfigDto toolsPath
    let projectPath = Path.Combine(path, "DocumentSymbolTest.fsproj")
    parseProject projectPath server |> Async.RunSynchronously
    let path = Path.Combine(path, "Script.fsx")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path}
    do server.TextDocumentDidOpen tdop |> Async.RunSynchronously
    (server, path)
  )
  let serverTest f () =
    let (server, path) = serverStart.Value
    f server path

  testSequenced <| testList "Document Symbols Tests" [
      testCase "Get Document Symbols" (serverTest (fun server path ->
        let p : DocumentSymbolParams = { TextDocument = { Uri = Path.FilePathToUri path}}
        let res = server.TextDocumentDocumentSymbol p |> Async.RunSynchronously
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->

          Expect.equal res.Length 15 "Document Symbol has all symbols"
          Expect.exists res (fun n -> n.Name = "MyDateTime" && n.Kind = SymbolKind.Class) "Document symbol contains given symbol"
      ))
  ]

///Tests for getting autocomplete
let autocompleteTest toolsPath =
  let serverStart = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "AutocompleteTest")
    let (server, event) = serverInitialize path defaultConfigDto toolsPath
    let projectPath = Path.Combine(path, "AutocompleteTest.fsproj")
    parseProject projectPath server |> Async.RunSynchronously
    let path = Path.Combine(path, "Script.fsx")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path}
    do server.TextDocumentDidOpen tdop |> Async.RunSynchronously
    (server, path)
  )

  let scriptProjServerStart = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "AutocompleteScriptTest")
    let (server, event) = serverInitialize path defaultConfigDto toolsPath
    do waitForWorkspaceFinishedParsing event
    let path = Path.Combine(path, "Script.fsx")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path}
    do server.TextDocumentDidOpen tdop |> Async.RunSynchronously
    (server, path)
  )

  let makeAutocompleteTestList (forScriptProject:bool) = [
    let serverTest =
      let serverStart =
        if forScriptProject
          then scriptProjServerStart
          else serverStart
      fun f ->
        let (server, path) = serverStart.Value
        f server path

    testCaseAsync "Get Autocomplete module members" (serverTest (fun server path ->
      async {
        let p : CompletionParams = { TextDocument = { Uri = Path.FilePathToUri path}
                                     Position = { Line = 8; Character = 2}
                                     Context = None }
        let! res = server.TextDocumentCompletion p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->

          Expect.equal res.Items.Length 2 "Autocomplete has all symbols"
          Expect.exists res.Items (fun n -> n.Label = "func") "Autocomplete contains given symbol"
          Expect.exists res.Items (fun n -> n.Label = "sample func") "Autocomplete contains given symbol"
      }))

    testCaseAsync "Get Autocomplete namespace" (serverTest (fun server path ->
      async {
        let p : CompletionParams = { TextDocument = { Uri = Path.FilePathToUri path}
                                     Position = { Line = 10; Character = 2}
                                     Context = None }
        let! res = server.TextDocumentCompletion p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          //TODO
          // Expect.equal res.Items.Length 1 "Autocomplete has all symbols"
          Expect.exists res.Items (fun n -> n.Label = "System") "Autocomplete contains given symbol"

      }))

    testCaseAsync "Get Autocomplete namespace members" (serverTest (fun server path ->
      async {
        let p : CompletionParams = { TextDocument = { Uri = Path.FilePathToUri path}
                                     Position = { Line = 12; Character = 7}
                                     Context = None }
        let! res = server.TextDocumentCompletion p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          //TODO
          // Expect.equal res.Items.Length 1 "Autocomplete has all symbols"
          Expect.exists res.Items (fun n -> n.Label = "DateTime") "Autocomplete contains given symbol"

      }))

    testCaseAsync "Get Autocomplete module doublebackticked members" (serverTest (fun server path ->
      async {
        let p : CompletionParams = { TextDocument = { Uri = Path.FilePathToUri path}
                                     Position = { Line = 14; Character = 18}
                                     Context = None }
        let! res = server.TextDocumentCompletion p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->

          Expect.equal res.Items.Length 1 "Autocomplete has all symbols"
          Expect.exists res.Items (fun n -> n.Label = "z") "Autocomplete contains given symbol"
      }))

    testCaseAsync "Autocomplete record members" (serverTest (fun server path ->
      async {
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
      }))

    testCaseAsync "Autocomplete class constructor with properties" (serverTest (fun server path ->
      async {
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
      }))
  ]

  testSequenced (
    testList "Autocomplete Tests" [
      testList "Autocomplete within project files" (makeAutocompleteTestList false)
      testList "Autocomplete within script files" (makeAutocompleteTestList true)
    ]
  )

///Rename tests
let renameTest toolsPath =
  let serverStart = lazy (
    let testDir = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "RenameTest")
    let (server, event) = serverInitialize testDir defaultConfigDto toolsPath

    let pathTest = Path.Combine(testDir, "Test.fs")
    let path = Path.Combine(testDir, "Program.fs")

    do waitForWorkspaceFinishedParsing event

    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument pathTest}
    do server.TextDocumentDidOpen tdop |> Async.RunSynchronously

    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path}
    do server.TextDocumentDidOpen tdop |> Async.RunSynchronously

    //Hack to wait for typechecking of 2 opened files
    System.Threading.Thread.Sleep 1000

    (server, path, pathTest) )

  let serverTest f () =
    let (server, path, pathTest) = serverStart.Value
    f server path pathTest

  testSequenced <| testList "Rename Tests" [
      ptestCase "Rename from usage" (serverTest (fun server path _ ->

        let p : RenameParams = { TextDocument = { Uri = Path.FilePathToUri path}
                                 Position = { Line = 7; Character = 12}
                                 NewName = "y" }

        let res = server.TextDocumentRename p |> Async.RunSynchronously
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          match res.DocumentChanges with
          | None -> failtest "No changes"
          | Some result ->
            Expect.equal result.Length 2 "Rename has all changes"
            Expect.exists result (fun n -> n.TextDocument.Uri.Contains "Program.fs" && n.Edits |> Seq.exists (fun r -> r.Range = { Start = {Line = 7; Character = 12 }; End = {Line = 7; Character = 13 } }) ) "Rename contains changes in Program.fs"
            Expect.exists result (fun n -> n.TextDocument.Uri.Contains "Test.fs" && n.Edits |> Seq.exists (fun r -> r.Range = { Start = {Line = 2; Character = 4 }; End = {Line = 2; Character = 5 } }) ) "Rename contains changes in Test.fs"
            ()
      ))

      ptestCase "Rename from definition" (serverTest (fun server path pathTest ->
        let p : RenameParams = { TextDocument = { Uri = Path.FilePathToUri pathTest}
                                 Position = { Line = 2; Character = 4}
                                 NewName = "y" }
        let res = server.TextDocumentRename p |> Async.RunSynchronously
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          match res.DocumentChanges with
          | None -> failtest "No changes"
          | Some result ->
            // TODO
            Expect.equal result.Length 2 "Rename has all changes"
            Expect.exists result (fun n -> n.TextDocument.Uri.Contains "Program.fs" && n.Edits |> Seq.exists (fun r -> r.Range = { Start = {Line = 7; Character = 12 }; End = {Line = 7; Character = 13 } }) ) "Rename contains changes in Program.fs"
            Expect.exists result (fun n -> n.TextDocument.Uri.Contains "Test.fs" && n.Edits |> Seq.exists (fun r -> r.Range = { Start = {Line = 2; Character = 4 }; End = {Line = 2; Character = 5 } }) ) "Rename contains changes in Test.fs"
            ()
      ))

  ]

///GoTo tests
let gotoTest toolsPath =
  let serverStart = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "GoToTests")

    let (server, event) = serverInitialize path defaultConfigDto toolsPath
    do waitForWorkspaceFinishedParsing event
    System.Threading.Thread.Sleep 1000
    let definitionPath = Path.Combine(path, "Definition.fs")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument definitionPath }
    do server.TextDocumentDidOpen tdop |> Async.RunSynchronously

    let externalPath = Path.Combine(path, "External.fs")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument externalPath }
    do server.TextDocumentDidOpen tdop |> Async.RunSynchronously

    let path = Path.Combine(path, "Library.fs")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path }
    do server.TextDocumentDidOpen tdop |> Async.RunSynchronously

    //Hack to wait for typechecking of 3 opened files
    System.Threading.Thread.Sleep 1000

    (server, path, externalPath, definitionPath)
  )
  let serverTest f () =
    let (server, path, externalPath, definitionPath) = serverStart.Value
    f server path externalPath definitionPath

  testSequenced <| testList "GoTo Tests" [
      testCase "Go-to-definition on external symbol (System.Net.HttpWebRequest)" (serverTest (fun server path externalPath definitionPath ->
        let p : TextDocumentPositionParams = {
          TextDocument = { Uri = Path.FilePathToUri externalPath }
          Position = { Line = 4; Character = 30 }
        }

        let res = server.TextDocumentDefinition p |> Async.RunSynchronously
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some (GotoResult.Multiple _)) -> failtest "Should only get one location"
        | Result.Ok (Some (GotoResult.Single r)) when r.Uri.EndsWith("startup") ->
          failtest "Should not generate the startup dummy file"
        | Result.Ok (Some (GotoResult.Single r)) ->
          Expect.stringEnds r.Uri ".cs" "should have generated a C# code file"
          Expect.stringContains r.Uri "System.Net.HttpWebRequest" "The generated file should be for the HttpWebRequest type"
          () // should
      ))

      testCase "Go-to-definition on external namespace (System.Net) should error when going to a namespace " (serverTest (fun server path externalPath definitionPath ->
        let p : TextDocumentPositionParams = {
          TextDocument = { Uri = Path.FilePathToUri externalPath }
          Position = { Line = 2; Character = 15 }
        }

        let res = server.TextDocumentDefinition p |> Async.RunSynchronously
        match res with
        | Result.Error e ->
          Expect.equal "Could not find declaration" e.Message "Should report failure for navigating to a namespace"
        | Result.Ok r -> failtestf "Declaration request should not work on a namespace, instead we got %A" r
      ))

      testCase "Go-to-definition" (serverTest (fun server path externalPath definitionPath ->
        let p : TextDocumentPositionParams  =
          { TextDocument = { Uri = Path.FilePathToUri path}
            Position = { Line = 2; Character = 29}}
        let res = server.TextDocumentDefinition p |> Async.RunSynchronously
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            Expect.stringContains res.Uri "Definition.fs" "Result should be in Definition.fs"
            Expect.equal res.Range { Start = {Line = 2; Character = 4 }; End = {Line = 2; Character = 16 }} "Result should have correct range"
      ))

      testCase "Go-to-definition on custom type binding" (serverTest (fun server path externalPath definitionPath ->
        let p : TextDocumentPositionParams  =
          { TextDocument = { Uri = Path.FilePathToUri path}
            Position = { Line = 4; Character = 24}}
        let res = server.TextDocumentDefinition p |> Async.RunSynchronously
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            Expect.stringContains res.Uri "Definition.fs" "Result should be in Definition.fs"
            Expect.equal res.Range { Start = {Line = 6; Character = 4 }; End = {Line = 6; Character = 19 }} "Result should have correct range"
      ))

      testCase "Go-to-implementation-on-interface-definition" (serverTest (fun server path externalPath definitionPath ->
        let p : TextDocumentPositionParams  =
          { TextDocument = { Uri = Path.FilePathToUri definitionPath}
            Position = { Line = 8; Character = 11}}
        let res = server.TextDocumentImplementation p |> Async.RunSynchronously
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          match res with
          | GotoResult.Single res -> failtest "Should be multiple GotoResult"
          | GotoResult.Multiple res ->
            // TODO???
            // Expect.exists res (fun r -> r.Uri.Contains "Library.fs" && r.Range = { Start = {Line = 7; Character = 8 }; End = {Line = 7; Character = 30 }}) "First result should be in Library.fs"
            // Expect.exists res (fun r -> r.Uri.Contains "Library.fs" && r.Range = { Start = {Line = 13; Character = 14 }; End = {Line = 13; Character = 36 }}) "Second result should be in Library.fs"
            ()
      ))

      testCase "Go-to-implementation on sourcelink file with sourcelink in PDB" (serverTest (fun server path externalPath definitionPath ->
        // check for the 'button' member in giraffe view engine
        let p : TextDocumentPositionParams  =
          { TextDocument = { Uri = Path.FilePathToUri externalPath}
            Position = { Line = 9; Character = 34} }

        let res = server.TextDocumentDefinition p |> Async.RunSynchronously
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            Expect.stringContains res.Uri "GiraffeViewEngine.fs" "Result should be in GiraffeViewEngine"
            let localPath = Path.FileUriToLocalPath res.Uri
            Expect.isTrue (System.IO.File.Exists localPath) (sprintf "File '%s' should exist locally after being downloaded" localPath)
      ))

      testCase "Go-to-implementation on sourcelink file with sourcelink in DLL" (serverTest (fun server path externalPath definitionPath ->
        // check for the 'List.concat' member in FSharp.Core
        let p : TextDocumentPositionParams  =
          { TextDocument = { Uri = Path.FilePathToUri externalPath}
            Position = { Line = 12; Character = 36} }

        let res = server.TextDocumentDefinition p |> Async.RunSynchronously
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            Expect.stringContains res.Uri "FSharp.Core/list.fs" "Result should be in FSharp.Core's list.fs"
            let localPath = Path.FileUriToLocalPath res.Uri
            Expect.isTrue (System.IO.File.Exists localPath) (sprintf "File '%s' should exist locally after being downloaded" localPath)
      ))

      // marked pending because we don't have filename information for C# sources
      ptestCase "Go-to-implementation on C# file" (serverTest (fun server path externalPath definitionPath ->
        // check for the 'Stirng.Join' member in the BCL
        let p : TextDocumentPositionParams  =
          { TextDocument = { Uri = Path.FilePathToUri externalPath}
            Position = { Line = 14; Character = 79} }

        let res = server.TextDocumentDefinition p |> Async.RunSynchronously
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            let localPath = Path.FileUriToLocalPath res.Uri
            if localPath.Contains "System.String netstandard_ Version_2.0.0.0_ Culture_neutral_ PublicKeyToken_cc7b13ffcd2ddd51"
            then failwithf "should not decompile when sourcelink is available"
            Expect.stringContains localPath "System.String" "Result should be in the BCL's source files"
            Expect.isTrue (System.IO.File.Exists localPath) (sprintf "File '%s' should exist locally after being downloaded" localPath)
      ))

      testCase "Go-to-type-definition" (serverTest (fun server path externalPath definitionPath ->
        let p : TextDocumentPositionParams  =
          { TextDocument = { Uri = Path.FilePathToUri path}
            Position = { Line = 4; Character = 24}}
        let res = server.TextDocumentTypeDefinition p |> Async.RunSynchronously
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            Expect.stringContains res.Uri "Definition.fs" "Result should be in Definition.fs"
            Expect.equal res.Range { Start = {Line = 4; Character = 5 }; End = {Line = 4; Character = 6 }} "Result should have correct range"
      ))

      testCase "Go-to-type-defintion on parameter" (serverTest (fun server path externalPath definitionPath ->
        // check for parameter of type `'a list` -> FSharp.Core
        (*
          `let myConcat listA listB = List.concat [listA; listB]`
                          ^
                          position
        *)
        let p: TextDocumentPositionParams =
          { TextDocument = { Uri = Path.FilePathToUri externalPath}
            Position = { Line = 12; Character = 16}}
        let res = server.TextDocumentTypeDefinition p |> Async.RunSynchronously
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            Expect.stringContains res.Uri "FSharp.Core/prim-types" "Result should be in FSharp.Core's prim-types"
            let localPath = Path.FileUriToLocalPath res.Uri
            Expect.isTrue (System.IO.File.Exists localPath) (sprintf "File '%s' should exist locally after being downloaded" localPath)
      ))

      testCase "Go-to-type-defintion on variable" (serverTest (fun server path externalPath definitionPath ->
        // check for variable of type `System.Collections.Generic.List<_>`
        (*
          `let myList = System.Collections.Generic.List<string>()`
                 ^
                 position
        *)
        let p: TextDocumentPositionParams =
          { TextDocument = { Uri = Path.FilePathToUri externalPath}
            Position = { Line = 16; Character = 6}}
        let res = server.TextDocumentTypeDefinition p |> Async.RunSynchronously
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            let localPath = Path.FileUriToLocalPath res.Uri
            Expect.stringContains res.Uri "System.Collections.Generic.List" "Result should be for System.Collections.Generic.List"
            Expect.isTrue (System.IO.File.Exists localPath) (sprintf "File '%s' should exist locally after being downloaded" localPath)
      ))

      testCase "Go-to-type-defintion on constructor" (serverTest (fun server path externalPath definitionPath ->
        // check for constructor of type `System.Collections.Generic.List<_>`
        (*
          `let myList = System.Collections.Generic.List<string>()`
                                                     ^
                                                     position
        *)
        let p: TextDocumentPositionParams =
          { TextDocument = { Uri = Path.FilePathToUri externalPath}
            Position = { Line = 16; Character = 42}}
        let res = server.TextDocumentTypeDefinition p |> Async.RunSynchronously
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            let localPath = Path.FileUriToLocalPath res.Uri
            Expect.stringContains res.Uri "System.Collections.Generic.List" "Result should be for System.Collections.Generic.List"
            Expect.isTrue (System.IO.File.Exists localPath) (sprintf "File '%s' should exist locally after being downloaded" localPath)
      ))

      testCase "Go-to-type-defintion on union case" (serverTest (fun server path externalPath definitionPath ->
        // check for union case of type `_ option`
        (*
          `let o v = Some v`
                       ^
                       position
        *)
        let p: TextDocumentPositionParams =
          { TextDocument = { Uri = Path.FilePathToUri externalPath}
            Position = { Line = 18; Character = 12}}
        let res = server.TextDocumentTypeDefinition p |> Async.RunSynchronously
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            Expect.stringContains res.Uri "FSharp.Core/prim-types" "Result should be in FSharp.Core's prim-types"
            let localPath = Path.FileUriToLocalPath res.Uri
            Expect.isTrue (System.IO.File.Exists localPath) (sprintf "File '%s' should exist locally after being downloaded" localPath)
      ))

      testCase "Go-to-type-defintion on property" (serverTest (fun server path externalPath definitionPath ->
        // check for property of type `string option`
        (*
          `b.Value |> ignore`
                ^
                position
        *)
        let p: TextDocumentPositionParams =
          { TextDocument = { Uri = Path.FilePathToUri externalPath}
            Position = { Line = 24; Character = 5}}
        let res = server.TextDocumentTypeDefinition p |> Async.RunSynchronously
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            Expect.stringContains res.Uri "FSharp.Core/prim-types" "Result should be in FSharp.Core's prim-types"
            let localPath = Path.FileUriToLocalPath res.Uri
            Expect.isTrue (System.IO.File.Exists localPath) (sprintf "File '%s' should exist locally after being downloaded" localPath)
      ))
  ]


let foldingTests toolsPath=
  let serverStart = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "FoldingTests")

    let (server, event) = serverInitialize path defaultConfigDto toolsPath
    do waitForWorkspaceFinishedParsing event
    let libraryPath = Path.Combine(path, "Library.fs")
    let libFile = loadDocument libraryPath
    let tdop : DidOpenTextDocumentParams = { TextDocument = libFile }
    do server.TextDocumentDidOpen tdop |> Async.RunSynchronously
    server, libraryPath
  )
  let serverTest f () = f serverStart.Value
  testList "folding tests" [
    testCase "can get ranges for sample file" (serverTest (fun (server, libraryPath) ->
      let rangeResponse = server.TextDocumentFoldingRange({ TextDocument = { Uri = Path.FilePathToUri libraryPath } }) |> Async.RunSynchronously
      match rangeResponse with
      | Ok(Some(ranges)) ->
        Expect.hasLength ranges 3 "Should be three ranges: one comment, one module, one let-binding"
      | Ok(None) -> failwithf "No ranges found in file, problem parsing?"
      | LspResult.Error e -> failwithf "Error from range LSP call: %A" e
    ))
  ]


let tooltipTests toolsPath =
  let (|Tooltip|_|) (hover: Hover) =
    match hover with
    | { Contents = MarkedStrings [| MarkedString.WithLanguage { Language = "fsharp"; Value = tooltip }; MarkedString.String newline; MarkedString.String fullname; MarkedString.String assembly |] } -> Some tooltip
    | _ -> None

  let (|Description|_|) (hover: Hover) =
    match hover with
    | { Contents = MarkedStrings [| MarkedString.WithLanguage { Language = "fsharp"; Value = tooltip }; MarkedString.String description; MarkedString.String fullname; MarkedString.String assembly |] } -> Some description
    | _ -> None

  let serverStart = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "Tooltips")
    let scriptPath = Path.Combine(path, "Script.fsx")
    let (server, events) = serverInitialize path defaultConfigDto toolsPath
    do waitForWorkspaceFinishedParsing events
    do server.TextDocumentDidOpen { TextDocument = loadDocument scriptPath } |> Async.RunSynchronously
    match waitForParseResultsForFile "Script.fsx" events with
    | Ok () ->
      () // all good, no parsing/checking errors
    | Core.Result.Error errors ->
      failwithf "Errors while parsing script %s: %A" scriptPath errors

    server, scriptPath
  )

  let verifyTooltip line character expectedTooltip =
    testCase (sprintf "tooltip for line %d character %d should be '%s" line character expectedTooltip) (fun _ ->
      let server, scriptPath = serverStart.Value
      let pos: TextDocumentPositionParams = {
        TextDocument =  { Uri = sprintf "file://%s" scriptPath }
        Position = { Line = line; Character = character }
      }
      match server.TextDocumentHover pos |> Async.RunSynchronously with
      | Ok (Some (Tooltip tooltip)) ->
        Expect.equal tooltip expectedTooltip (sprintf "Should have a tooltip of '%s'" expectedTooltip)
      | Ok _ ->
        failwithf "Should have gotten hover text"
      | Result.Error errors ->
        failwithf "Error while getting hover text: %A" errors
    )

  let verifyDescription line character expectedTooltip =
    testCase (sprintf "tooltip for line %d character %d should be '%s" line character expectedTooltip) (fun _ ->
      let server, scriptPath = serverStart.Value
      let pos: TextDocumentPositionParams = {
        TextDocument =  { Uri = sprintf "file://%s" scriptPath }
        Position = { Line = line; Character = character }
      }
      match server.TextDocumentHover pos |> Async.RunSynchronously with
      | Ok (Some (Description tooltip)) ->
        Expect.equal tooltip expectedTooltip (sprintf "Should have a tooltip of '%s'" expectedTooltip)
      | Ok _ ->
        failwithf "Should have gotten hover text"
      | Result.Error errors ->
        failwithf "Error while getting hover text: %A" errors
    )

  let concatLines = String.concat Environment.NewLine

  testList "tooltip evaluation" [
    verifyTooltip 0 4 "val arrayOfTuples : (int * int) array"
    verifyTooltip 1 4 "val listOfTuples : list<int * int>"
    verifyTooltip 2 4 "val listOfStructTuples : list<struct(int * int)>"
    verifyTooltip 3 4 "val floatThatShouldHaveGenericReportedInTooltip : float" //<MeasureOne>
    //verifyDescription 4 4 """**Description**\n\nPrint to a string using the given format.\n\n**Parameters**\n\n* `format`: The formatter.\n\n**Returns**\n\nThe formatted result.\n\n**Generic parameters**\n\n* `'T` is `string`"""
    verifyDescription 13 10 (concatLines ["**Description**"; ""; "\nMy super summary\n "; ""; "**Parameters**"; ""; "* `c`: foo"; "* `b`: bar"; "* `a`: baz"; ""; "**Returns**"; ""; ""])
  ]


let highlightingTests toolsPath =
  let serverStart = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "CodeLensTest")
    let (server, event) = serverInitialize path defaultConfigDto toolsPath
    let projectPath = Path.Combine(path, "CodeLensTest.fsproj")
    parseProject projectPath server |> Async.RunSynchronously
    let path = Path.Combine(path, "Script.fs")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path}

    do server.TextDocumentDidOpen tdop |> Async.RunSynchronously
    (server, path)
    )
  let serverTest f () =
    let (server, path) = serverStart.Value
    f server path

  testCase "Document Highlighting" (serverTest (fun server path ->
    let p : HighlightingRequest = { FileName =  path}
    let res = server.GetHighlighting p |> Async.RunSynchronously
    printfn "%A" res
    ()
    // Expect.equal res.Length 2 "Document Symbol has all symbols"
  ))


let signatureHelpTests toolsPath =


  let serverStart = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "SignatureHelpTest")
    let scriptPath = Path.Combine(path, "Script1.fsx")
    let (server, events) = serverInitialize path defaultConfigDto toolsPath
    do waitForWorkspaceFinishedParsing events
    do server.TextDocumentDidOpen { TextDocument = loadDocument scriptPath } |> Async.RunSynchronously
    match waitForParseResultsForFile "Script1.fsx" events with
    | Ok () ->
      () // all good, no parsing/checking errors
    | Core.Result.Error errors ->
      failwithf "Errors while parsing script %s: %A" scriptPath errors

    server, scriptPath
  )

  testSequenced <| testList "SignatureHelp" [
    ptestCase "signature help is also shown for overload without parameters" (fun _ ->
        let server, testFilePath = serverStart.Value

        do server.TextDocumentDidOpen { TextDocument = loadDocument testFilePath } |> Async.RunSynchronously

        let getSignatureHelpAt line character =
          let sigHelpParams: SignatureHelpParams =
            { TextDocument = { Uri = Path.FilePathToUri testFilePath }
              Position = { Line = line; Character = character }
              Context = Some {
                TriggerKind = SignatureHelpTriggerKind.Invoked
                TriggerCharacter = None
                IsRetrigger = false
                ActiveSignatureHelp = None
              } }
          server.TextDocumentSignatureHelp sigHelpParams

        let expectSomeOverloads sigHelpLspRes =
          let sigHelp : SignatureHelp =
            sigHelpLspRes
            |> Flip.Expect.wantOk "Expected success SLP result"
            |> Flip.Expect.wantSome "Expected some signature help"
          sigHelp.Signatures |> Flip.Expect.isNonEmpty "Expected some overloads"

        // let __ = new System.IO.MemoryStream(|)
        let result = getSignatureHelpAt 0 36 |> Async.RunSynchronously
        result |> expectSomeOverloads

        // let ___ = new System.IO.MemoryStream (|||)
        for c in 38 .. 40 do
          let result = getSignatureHelpAt 1 c |> Async.RunSynchronously
          result |> expectSomeOverloads

        // let _____ = new System.IO.MemoryStream(|4|2|)
        for c in 39 .. 41 do
          let result = getSignatureHelpAt 2 c |> Async.RunSynchronously
          result |> expectSomeOverloads
      )
  ]
