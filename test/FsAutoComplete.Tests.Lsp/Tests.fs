module FsAutoComplete.Tests.Lsp

open System
open Expecto
open System.IO
open LanguageServerProtocol
open LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open Helpers
open Expecto.Logging
open Expecto.Logging.Message

let logger = Expecto.Logging.Log.create "LSPTests"

///Test for initialization of the server
let initTests =
  test "InitTest" {
    let (server, event) = createServer()

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
      Expect.equal res.Capabilities.SignatureHelpProvider (Some {SignatureHelpOptions.TriggerCharacters = Some [| "("; ","|]} ) "Signature Help Provider"
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
let basicTests =
  let serverStart = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "BasicTest")
    let (server, event) = serverInitialize path defaultConfigDto
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
            { TextDocument = { Uri = filePathToUri path}
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
            { TextDocument = { Uri = filePathToUri path}
              Position = { Line = 0; Character = 2}}
          let res = server.TextDocumentHover p |> Async.RunSynchronously
          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->
            let expected =
              MarkedStrings
                [|  MarkedString.WithLanguage {Language = "fsharp"; Value = "let"}
                    MarkedString.String "Used to associate, or bind, a name to a value or function."|]

            Expect.equal res.Contents expected "Hover test - let keyword"
        ))

        testCase "Hover Tests - out of position" (serverTest (fun server path ->
          let p : TextDocumentPositionParams =
            { TextDocument = { Uri = filePathToUri path}
              Position = { Line = 1; Character = 2}}
          let res = server.TextDocumentHover p |> Async.RunSynchronously
          match res with
          | Result.Error e -> ()
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->
            failtest "Expected failure"
        ))
      ]
      testSequenced <| testList "Document Symbol Tests" [
        testCase "Document Symbol" (serverTest (fun server path ->
          let p : DocumentSymbolParams = { TextDocument = { Uri = filePathToUri path}}
          let res = server.TextDocumentDocumentSymbol p |> Async.RunSynchronously
          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->

            Expect.equal res.Length 2 "Document Symbol has all symbols"
        ))
      ]
      testSequenced <| testList "Code Lens Tests" [
        testCase "Get Code Lens" (serverTest (fun server path ->
          let p : CodeLensParams = { TextDocument = { Uri = filePathToUri path}}
          let res = server.TextDocumentCodeLens p |> Async.RunSynchronously
          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->

            Expect.equal res.Length 1 "Get Code Lens has all locations"
        ))

        testCase "Resolve Code Lens" (serverTest (fun server path ->
          let p : CodeLensParams = { TextDocument = { Uri = filePathToUri path}}
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
              Expect.equal cl.Command.Value.Title "int" "Code Lens contains signature"
        ))
      ]

  ]

///Tests for getting and resolving code(line) lenses with enabled reference code lenses
let codeLensTest =
  let serverStart = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "CodeLensTest")
    let (server, event) = serverInitialize path {defaultConfigDto with EnableReferenceCodeLens = Some true}
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
          let p : CodeLensParams = { TextDocument = { Uri = filePathToUri path}}
          let res = server.TextDocumentCodeLens p |> Async.RunSynchronously
          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->

            Expect.equal res.Length 18 "Get Code Lens has all locations"
      ))
      testCase "Resolve Code Lens" (serverTest (fun server path ->
          let p : CodeLensParams = { TextDocument = { Uri = filePathToUri path}}
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
          let p : CodeLensParams = { TextDocument = { Uri = filePathToUri path}}
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
let documentSymbolTest =
  let serverStart = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "DocumentSymbolTest")
    let (server, event) = serverInitialize path defaultConfigDto
    let projectPath = Path.Combine(path, "DocumentSymbolTest.fsproj")
    parseProject projectPath server |> Async.RunSynchronously
    let path = Path.Combine(path, "Script.fs")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path}
    do server.TextDocumentDidOpen tdop |> Async.RunSynchronously
    (server, path)
  )
  let serverTest f () =
    let (server, path) = serverStart.Value
    f server path

  testSequenced <| testList "Document Symbols Tests" [
      testCase "Get Document Symbols" (serverTest (fun server path ->
        let p : DocumentSymbolParams = { TextDocument = { Uri = filePathToUri path}}
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
let autocompleteTest =
  let serverStart = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "AutocompleteTest")
    let (server, event) = serverInitialize path defaultConfigDto
    let projectPath = Path.Combine(path, "AutocompleteTest.fsproj")
    parseProject projectPath server |> Async.RunSynchronously
    let path = Path.Combine(path, "Script.fs")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path}
    do server.TextDocumentDidOpen tdop |> Async.RunSynchronously
    (server, path)
  )
  let serverTest f () =
    let (server, path) = serverStart.Value
    f server path

  testSequenced <| testList "Autocomplete Tests" [
      testCase "Get Autocomplete module members" (serverTest (fun server path ->
        let p : CompletionParams = { TextDocument = { Uri = filePathToUri path}
                                     Position = { Line = 8; Character = 2}
                                     Context = None }
        let res = server.TextDocumentCompletion p |> Async.RunSynchronously
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->

          Expect.equal res.Items.Length 2 "Autocomplete has all symbols"
          Expect.exists res.Items (fun n -> n.Label = "func") "Autocomplete contains given symbol"
          Expect.exists res.Items (fun n -> n.Label = "sample func") "Autocomplete contains given symbol"
      ))

      testCase "Get Autocomplete namespace" (serverTest (fun server path ->
        let p : CompletionParams = { TextDocument = { Uri = filePathToUri path}
                                     Position = { Line = 10; Character = 2}
                                     Context = None }
        let res = server.TextDocumentCompletion p |> Async.RunSynchronously
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          //TODO
          // Expect.equal res.Items.Length 1 "Autocomplete has all symbols"
          Expect.exists res.Items (fun n -> n.Label = "System") "Autocomplete contains given symbol"

      ))

      testCase "Get Autocomplete namespace members" (serverTest (fun server path ->
        let p : CompletionParams = { TextDocument = { Uri = filePathToUri path}
                                     Position = { Line = 12; Character = 7}
                                     Context = None }
        let res = server.TextDocumentCompletion p |> Async.RunSynchronously
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          //TODO
          // Expect.equal res.Items.Length 1 "Autocomplete has all symbols"
          Expect.exists res.Items (fun n -> n.Label = "DateTime") "Autocomplete contains given symbol"

      ))

      testCase "Get Autocomplete module doublebackticked members" (serverTest (fun server path ->
        let p : CompletionParams = { TextDocument = { Uri = filePathToUri path}
                                     Position = { Line = 14; Character = 18}
                                     Context = None }
        let res = server.TextDocumentCompletion p |> Async.RunSynchronously
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->

          Expect.equal res.Items.Length 1 "Autocomplete has all symbols"
          Expect.exists res.Items (fun n -> n.Label = "z") "Autocomplete contains given symbol"

      ))

  ]

let logDotnetRestore section line =
  if not (String.IsNullOrWhiteSpace(line)) then
    logger.debug (eventX "[{section}] dotnet restore: {line}" >> setField "section" section >> setField "line" line)

let getDiagnosticsEvents =
  Event.filter (fun (typ, _o) -> typ = "textDocument/publishDiagnostics")
  >> Event.map (fun (_typ, o) -> unbox<LanguageServerProtocol.Types.PublishDiagnosticsParams> o)

/// note that the files here are intended to be the filename only., not the full URI.
let matchFiles (files: string Set) =
  Event.choose (fun (p: LanguageServerProtocol.Types.PublishDiagnosticsParams) ->
    let filename = p.Uri.Split([| '/'; '\\' |], StringSplitOptions.RemoveEmptyEntries) |> Array.last
    if Set.contains filename files
    then Some (filename, p)
    else None
  )

let waitForParseResultsForFile file (events: Event<string*obj>) =
  let matchingFileEvents =
    events.Publish
    |> getDiagnosticsEvents
    |> matchFiles (Set.ofList [file])
  async {
    let! (filename, args) = Async.AwaitEvent matchingFileEvents
    match args.Diagnostics with
    | [||] -> return Ok ()
    | errors -> return Core.Result.Error errors
  }
  |> Async.RunSynchronously

let waitForParsed (m: System.Threading.ManualResetEvent) files (event: Event<string * obj>) =

  let found =
    let d = new System.Collections.Concurrent.ConcurrentDictionary<string, bool>()
    for file in files do
      d.[file] <- false
    d

  let fileNames = files |> Set.ofList
  logger.debug (eventX "waiting for {files} to be parsed" >> setField "files" fileNames)
  event.Publish
  |> getDiagnosticsEvents
  |> matchFiles fileNames
  |> Event.add (fun (filename, n) ->
      if Array.isEmpty n.Diagnostics then // no errors
        found.AddOrUpdate(filename, true, (fun x y -> true)) |> ignore
        logger.debug (eventX "{file} was parsed successfully" >> setField "file" filename)

      match found |> Seq.filter (fun (KeyValue(name, found)) -> not found) with
      | s when Seq.isEmpty s ->
        logger.debug (eventX "all parsed without error, signaling...")
        m.Set() |> ignore
      | s ->
        logger.debug (eventX "still waiting for {files}" >> setField "files" (s |> Seq.map (fun (KeyValue(name, _)) -> name)))
        ()
      )

///Rename tests
let renameTest =
  let serverStart = lazy (
    let testDir = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "RenameTest")

    dotnetCleanup testDir

    Utils.runProcess (logDotnetRestore "RenameTest") testDir "dotnet" "restore"
    |> expectExitCodeZero

    let m = new System.Threading.ManualResetEvent(false)

    let (server, event) = serverInitialize testDir defaultConfigDto

    event |> waitForParsed m ["Test.fs"; "Program.fs"]

    let pathTest = Path.Combine(testDir, "Test.fs")
    let path = Path.Combine(testDir, "Program.fs")

    do waitForWorkspaceFinishedParsing event

    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument pathTest}
    do server.TextDocumentDidOpen tdop |> Async.RunSynchronously

    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path}
    do server.TextDocumentDidOpen tdop |> Async.RunSynchronously

    m.WaitOne() |> ignore

    (server, path, pathTest) )

  let serverTest f () =
    let (server, path, pathTest) = serverStart.Value
    f server path pathTest

  testSequenced <| testList "Rename Tests" [
      testCase "Rename from usage" (serverTest (fun server path _ ->

        let p : RenameParams = { TextDocument = { Uri = filePathToUri path}
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

      testCase "Rename from definition" (serverTest (fun server path pathTest ->
        let p : RenameParams = { TextDocument = { Uri = filePathToUri pathTest}
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
            // Expect.equal result.Length 2 "Rename has all changes"
            // Expect.exists result (fun n -> n.TextDocument.Uri.Contains "Program.fs" && n.Edits |> Seq.exists (fun r -> r.Range = { Start = {Line = 7; Character = 12 }; End = {Line = 7; Character = 13 } }) ) "Rename contains changes in Program.fs"
            // Expect.exists result (fun n -> n.TextDocument.Uri.Contains "Test.fs" && n.Edits |> Seq.exists (fun r -> r.Range = { Start = {Line = 2; Character = 4 }; End = {Line = 2; Character = 5 } }) ) "Rename contains changes in Test.fs"
            ()
      ))

  ]

///GoTo tests
let gotoTest =
  let serverStart = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "GoToTests")

    dotnetCleanup path

    Utils.runProcess (logDotnetRestore "GoToTests") path "dotnet" "restore"
    |> expectExitCodeZero

    let (server, event) = serverInitialize path defaultConfigDto
    do waitForWorkspaceFinishedParsing event
    let definitionPath = Path.Combine(path, "Definition.fs")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument definitionPath }
    do server.TextDocumentDidOpen tdop |> Async.RunSynchronously

    let externalPath = Path.Combine(path, "External.fs")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument externalPath }
    do server.TextDocumentDidOpen tdop |> Async.RunSynchronously

    let path = Path.Combine(path, "Library.fs")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path }
    do server.TextDocumentDidOpen tdop |> Async.RunSynchronously

    logger.debug (eventX "finished start up")

    (server, path, externalPath, definitionPath)
  )
  let serverTest f () =
    let (server, path, externalPath, definitionPath) = serverStart.Value
    f server path externalPath definitionPath

  testSequenced <| testList "GoTo Tests" [
      testCase "Go-to-definition on external symbol (System.Net.HttpWebRequest)" (serverTest (fun server path externalPath definitionPath ->
        let p : TextDocumentPositionParams = {
          TextDocument = { Uri = filePathToUri externalPath }
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
          logger.debug (eventX "wrote external definition file to {location}" >> setField "location" r.Uri)
          Expect.stringEnds r.Uri ".cs" "should have generated a C# code file"
          Expect.stringContains r.Uri "System.Net.HttpWebRequest" "The generated file should be for the HttpWebRequest type"
          () // should
      ))

      testCase "Go-to-definition on external namespace (System.Net) should error when going to a namespace " (serverTest (fun server path externalPath definitionPath ->
        let p : TextDocumentPositionParams = {
          TextDocument = { Uri = filePathToUri externalPath }
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
          { TextDocument = { Uri = filePathToUri path}
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
          { TextDocument = { Uri = filePathToUri path}
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

      testCase "Go-to-type-definition" (serverTest (fun server path externalPath definitionPath ->
        let p : TextDocumentPositionParams  =
          { TextDocument = { Uri = filePathToUri path}
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

      testCase "Go-to-implementation-on-interface-definition" (serverTest (fun server path externalPath definitionPath ->
        let p : TextDocumentPositionParams  =
          { TextDocument = { Uri = filePathToUri definitionPath}
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
  ]

let fsdnTest =
  let serverStart = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "Empty")
    let (server, event) = serverInitialize path defaultConfigDto
    waitForWorkspaceFinishedParsing event
    server
  )
  let serverTest f () =
    f serverStart.Value

  testList "FSDN Tests" [
      testCase "FSDN on list" (serverTest (fun server ->
        let p : FsdnRequest = {
            Query = "('a -> 'b) -> 'a list -> 'b list"
        }

        let res = server.FSharpFsdn p |> Async.RunSynchronously
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok n ->
          Expect.stringContains n.Content "List.map" (sprintf "the List.map is a valid response, but was %A" n)
          let r = JsonSerializer.readJson<CommandResponse.ResponseMsg<CommandResponse.FsdnResponse>>(n.Content)
          Expect.equal r.Kind "fsdn" (sprintf "fsdn response, but was %A, from json %A" r n)
          Expect.contains r.Data.Functions "List.map" (sprintf "the List.map is a valid response, but was %A, from json %A" r n)
      ))
  ]

let uriTests =
  let verifyUri (given: string) (expectedLocal: string) = test (sprintf "roundtrip '%s' -> '%s'" given expectedLocal) {
    let actual = LspHelpers.Conversions.fileUriToLocalPath given
    Expect.equal actual expectedLocal (sprintf "LocalPath of '%s' should be '%s'" given expectedLocal)
  }

  let convertRawPathToUri (rawPath: string) (expectedPath: string) = test (sprintf "convert '%s' -> '%s'" rawPath expectedPath) {
    let createdFilePath = filePathToUri rawPath
    let createdUri = createdFilePath |> Uri |> string
    Expect.equal createdUri expectedPath (sprintf "converting raw path '%s' should generate a Uri with LocalPath '%s'" createdFilePath expectedPath)
  }

  let samples =
    [ "file:///C%3A/foo/bar/baz", "C:/foo/bar/baz"
      "file:///C%3A/foo/bar bar/baz", "C:/foo/bar bar/baz" // spaces, windows-root
      "file:///Users/bob jones/foo/bar", "/Users/bob jones/foo/bar" // spaces, unix-root
      "file:///Users/bobjones/foo/bar", "/Users/bobjones/foo/bar"
      "file:///C%3A/f%23/bar/baz", "C:/f#/bar/baz" // escaped chars, windows-root
      "file:///Users/carlyrae/oss/f%23test", "/Users/carlyrae/oss/f#test" // escaped chars, unix-root
      "file:///C%3A/carly rae/oss/f%23test", "C:/carly rae/oss/f#test" // spaces and escaped chars, windows-root
      "file:///Users/carly rae/oss/f%23test", "/Users/carly rae/oss/f#test" // spaces and escaped chars, unix-root
      "file:///d%3A/code/Saturn/src/Saturn/Utils.fs", "d:/code/Saturn/src/Saturn/Utils.fs"
    ]

  testList "Uri tests"[
    testList "roundtrip tests" (samples |> List.map (fun (uriForm, filePath) -> verifyUri uriForm filePath))
    testList "fileName to uri tests" (samples |> List.map (fun (uriForm, filePath) -> convertRawPathToUri filePath uriForm))
 ]

let dotnetnewTest =
  let serverStart = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "Empty")
    let (server, event) = serverInitialize path defaultConfigDto
    waitForWorkspaceFinishedParsing event
    server
  )
  let serverTest f () =
    f serverStart.Value

  testList "DotnetNew Tests" [
      testCase "DotnetNewList on list" (serverTest (fun server ->
        let p : DotnetNewListRequest = {
            Query = "Console Application"
        }

        let sampleTemplate : DotnetNewTemplate.Template = { Name = "Console Application";
                                               ShortName = "console";
                                               Language = [ DotnetNewTemplate.TemplateLanguage.CSharp; DotnetNewTemplate.TemplateLanguage.FSharp; DotnetNewTemplate.TemplateLanguage.VB ];
                                               Tags = ["Common"; "Console"] }

        let res = server.FSharpDotnetNewList p |> Async.RunSynchronously
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok n ->
          Expect.stringContains n.Content "Console Application" (sprintf "the Console Application is a valid response, but was %A" n)
          let r = JsonSerializer.readJson<CommandResponse.ResponseMsg<CommandResponse.DotnetNewListResponse>>(n.Content)
          Expect.equal r.Kind "dotnetnewlist" (sprintf "dotnetnewlist response, but was %A, from json %A" r n)
          Expect.equal r.Data.Installed.[0].Name sampleTemplate.Name (sprintf "the Console Application is a valid response, but was %A, from json %A" r n)
          Expect.equal r.Data.Installed.[0].Language sampleTemplate.Language (sprintf "the Console Application is a valid response, but was %A, from json %A" r n)
      ))

      testCase "DotnetNewGetDetails on list" (serverTest (fun server ->
        let p : DotnetNewGetDetailsRequest = {
            Query = "Console Application"
        }

        let sampleTemplate : DotnetNewTemplate.DetailedTemplate = { TemplateName = "Console Application";
                                                                    Author = "Microsoft";
                                                                    TemplateDescription = "A project for creating a command-line application that can run on .NET Core on Windows, Linux and macOS";
                                                                    Options =
                                                                    [ { ParameterName = "--no-restore";
                                                                        ShortName = "";
                                                                        ParameterType = DotnetNewTemplate.TemplateParameterType.Bool;
                                                                        ParameterDescription = "If specified, skips the automatic restore of the project on create.";
                                                                        DefaultValue = "false / (*) true" }
                                                                    ] }

        let res = server.FSharpDotnetNewGetDetails p |> Async.RunSynchronously
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok n ->
          Expect.stringContains n.Content "Console Application" (sprintf "the Console Application is a valid response, but was %A" n)
          let r = JsonSerializer.readJson<CommandResponse.ResponseMsg<CommandResponse.DotnetNewGetDetailsResponse>>(n.Content)
          Expect.equal r.Kind "dotnetnewgetDetails" (sprintf "dotnetnewgetDetails response, but was %A, from json %A" r n)
          Expect.equal r.Data.Detailed.TemplateName sampleTemplate.TemplateName (sprintf "the Console Application is a valid response, but was %A, from json %A" r n)
          Expect.equal r.Data.Detailed.Options.[0].ParameterName sampleTemplate.Options.[0].ParameterName (sprintf "the Console Application is a valid response, but was %A, from json %A" r n)
          Expect.equal r.Data.Detailed.Options.[0].ParameterType sampleTemplate.Options.[0].ParameterType (sprintf "the Console Application is a valid response, but was %A, from json %A" r n)
      ))
    ]

let foldingTests =
  let serverStart = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "FoldingTests")

    dotnetCleanup path

    Utils.runProcess (logDotnetRestore "FoldingTests") path "dotnet" "restore"
    |> expectExitCodeZero

    let (server, event) = serverInitialize path defaultConfigDto
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
      let rangeResponse = server.TextDocumentFoldingRange({ TextDocument = { Uri = filePathToUri libraryPath } }) |> Async.RunSynchronously
      match rangeResponse with
      | Ok(Some(ranges)) ->
        Expect.hasLength ranges 3 "Should be three ranges: one comment, one module, one let-binding"
      | Ok(None) -> failwithf "No ranges found in file, problem parsing?"
      | LspResult.Error e -> failwithf "Error from range LSP call: %A" e
    ))
  ]

let scriptPreviewTests =
  let serverStart = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "PreviewScriptFeatures")
    let scriptPath = Path.Combine(path, "Script.fsx")
    let (server, events) = serverInitialize path defaultConfigDto
    do waitForWorkspaceFinishedParsing events
    server, events, scriptPath
  )
  let serverTest f () = f serverStart.Value

  testList "script preview language features" [
    testCase "can typecheck scripts when preview features are used" (serverTest (fun (server, events, scriptPath) ->
      do server.TextDocumentDidOpen { TextDocument = loadDocument scriptPath } |> Async.RunSynchronously
      match waitForParseResultsForFile "Script.fsx" events with
      | Ok () ->
        () // all good, no parsing/checking errors
      | Core.Result.Error errors ->
        failwithf "Errors while parsing script %s: %A" scriptPath errors
    ))
  ]

///Global list of tests
let tests =
   testSequenced <| testList "lsp" [
    initTests
    basicTests
    codeLensTest
    documentSymbolTest
    autocompleteTest
    renameTest
    gotoTest
    fsdnTest
    uriTests
    dotnetnewTest
    foldingTests
#if false // commented out because this will only work in a netcoreapp3.0 context, which CI doesn't have.
    scriptPreviewTests
#endif
  ]
