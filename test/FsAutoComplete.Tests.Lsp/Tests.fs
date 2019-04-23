module FsAutoComplete.Tests.Lsp

open System
open Expecto
open System.IO
open LanguageServerProtocol
open LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open Helpers



[<Tests>]
let tests =
  test "InitTest" {
    let server = createServer()

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
      Expect.equal res.Capabilities.DocumentFormattingProvider None "Document Formatting Provider"
      Expect.equal res.Capabilities.DocumentHighlightProvider (Some true) "Document Highligthing Provider"
      Expect.equal res.Capabilities.DocumentLinkProvider None "Document Link Provider"
      Expect.equal res.Capabilities.DocumentOnTypeFormattingProvider None "Document OnType Formatting Provider"
      Expect.equal res.Capabilities.DocumentRangeFormattingProvider None "Document Range Formatting Provider"
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
    | Result.Error e ->
      failwith "Initialization failed"
  }


[<Tests>]
//Tests for basic operations like hover, getting document symbols or code lens on simple file
let basicTests =
  let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "BasicTest")
  serverTest path defaultConfigDto (fun server ->
    let path = Path.Combine(path, "Script.fsx")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path}
    do server.TextDocumentDidOpen tdop |> Async.RunSynchronously
    testList "Basic Tests" [
      testList "Hover Tests" [

        test "Hover Tests - simple symbol" {
          let p : TextDocumentPositionParams =
            { TextDocument = { Uri = filePathToUri path}
              Position = { Line = 0; Character = 4}}
          let res = server.TextDocumentHover p |> Async.RunSynchronously
          match res with
          | Result.Error e -> failtest "Request failed"
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->
            let expected =
              MarkedStrings
                [|  MarkedString.WithLanguage {Language = "fsharp"; Value = "val t : int"}
                    MarkedString.String ""
                    MarkedString.String "*Full name: Script.t*"
                    MarkedString.String "*Assembly: Script*"|]

            Expect.equal res.Contents expected "Hover test - simple symbol"
        }

        test "Hover Tests - let keyword" {
          let p : TextDocumentPositionParams =
            { TextDocument = { Uri = filePathToUri path}
              Position = { Line = 0; Character = 2}}
          let res = server.TextDocumentHover p |> Async.RunSynchronously
          match res with
          | Result.Error e -> failtest "Request failed"
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->
            let expected =
              MarkedStrings
                [|  MarkedString.WithLanguage {Language = "fsharp"; Value = "let"}
                    MarkedString.String "Used to associate, or bind, a name to a value or function."|]

            Expect.equal res.Contents expected "Hover test - let keyword"
        }

        test "Hover Tests - out of position" {
          let p : TextDocumentPositionParams =
            { TextDocument = { Uri = filePathToUri path}
              Position = { Line = 1; Character = 2}}
          let res = server.TextDocumentHover p |> Async.RunSynchronously
          match res with
          | Result.Error e -> ()
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->
            failtest "Expected failure"
        }
      ]
      testList "Document Symbol Tests" [
        test "Document Symbol" {
          let p : DocumentSymbolParams = { TextDocument = { Uri = filePathToUri path}}
          server.FileInit <- true
          let res = server.TextDocumentDocumentSymbol p |> Async.RunSynchronously
          match res with
          | Result.Error e -> failtest "Request failed"
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->

            Expect.equal res.Length 2 "Document Symbol has all symbols"
        }
      ]
      testList "Code Lens Tests" [
        test "Get Code Lens" {
          let p : CodeLensParams = { TextDocument = { Uri = filePathToUri path}}
          let res = server.TextDocumentCodeLens p |> Async.RunSynchronously
          match res with
          | Result.Error e -> failtest "Request failed"
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->

            Expect.equal res.Length 1 "Get Code Lens has all locations"
        }

        test "Resolve Code Lens" {
          let p : CodeLensParams = { TextDocument = { Uri = filePathToUri path}}
          let res = server.TextDocumentCodeLens p |> Async.RunSynchronously
          match res with
          | Result.Error e -> failtest "Request failed"
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->
            let cl = res.[0]
            let res = server.CodeLensResolve cl |> Async.RunSynchronously
            match res with
            | Result.Error e -> failtest "Request failed"
            | Result.Ok cl ->
              Expect.equal cl.Command.Value.Title "int" "Code Lens contains signature"
        }
      ]


    ])

[<Tests>]
//Tests for getting and resolving code(line) lenses with enabled reference code lenses
let codeLensTest =
  let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "CodeLensTest")
  serverTest path {defaultConfigDto with EnableBackgroundSymbolCache = Some true; EnableReferenceCodeLens = Some true} (fun server ->
    let path = Path.Combine(path, "Script.fsx")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path}
    do server.TextDocumentDidOpen tdop |> Async.RunSynchronously
    testList "Code Lens Tests" [
      test "Get Code Lens" {
          let p : CodeLensParams = { TextDocument = { Uri = filePathToUri path}}
          let res = server.TextDocumentCodeLens p |> Async.RunSynchronously
          match res with
          | Result.Error e -> failtest "Request failed"
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->

            Expect.equal res.Length 18 "Get Code Lens has all locations"
        }
      test "Resolve Code Lens" {
          let p : CodeLensParams = { TextDocument = { Uri = filePathToUri path}}
          let res = server.TextDocumentCodeLens p |> Async.RunSynchronously
          match res with
          | Result.Error e -> failtest "Request failed"
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some result) ->
            let cl = result.[0]
            let res = server.CodeLensResolve cl |> Async.RunSynchronously
            let cl = result.[9]
            let res2 = server.CodeLensResolve cl |> Async.RunSynchronously
            match res, res2 with
            | Result.Ok cl, Result.Ok cl2 ->
              //Expect.equal cl.Command.Value.Title "1 Reference" "Code Lens contains reference count"
              Expect.equal cl2.Command.Value.Title "string -> unit" "Code Lens contains signature"

            | _ -> failtest "Request failed"
        }

      test "Resolve Code Lens 2" {
          let p : CodeLensParams = { TextDocument = { Uri = filePathToUri path}}
          let res = server.TextDocumentCodeLens p |> Async.RunSynchronously
          match res with
          | Result.Error e -> failtest "Request failed"
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some result) ->
            let cl = result.[3]
            let res = server.CodeLensResolve cl |> Async.RunSynchronously
            let cl = result.[12]
            let res2 = server.CodeLensResolve cl |> Async.RunSynchronously
            match res, res2 with
            | Result.Ok cl, Result.Ok cl2 ->
              //Expect.equal cl.Command.Value.Title "1 Reference" "Code Lens contains reference count"
              Expect.equal cl2.Command.Value.Title "unit -> (int64 -> System.DateTime)" "Code Lens contains signature"

            | _ -> failtest "Request failed"
        }
    ]
  )