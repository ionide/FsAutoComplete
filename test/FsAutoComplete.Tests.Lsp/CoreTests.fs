module FsAutoComplete.Tests.CoreTest

open System
open Expecto
open Expecto.Logging
open Expecto.Logging.Message
open System.IO
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open Helpers
open FsToolkit.ErrorHandling
open FSharp.Control.Reactive
open FsAutoComplete.Lsp
open Utils.ServerTests
open Utils.Server
open Utils.TextEdit
open Mono.Cecil.Cil
open Utils
open Utils.Utils
open FsToolkit.ErrorHandling.Operator.AsyncResult
open FSharpx.Control
open Utils.Tests
open Helpers.Expecto.ShadowedTimeouts

///Test for initialization of the server
let initTests createServer =
  testCaseAsync
    "InitTest"
    (async {
      let tempDir =
        Path.Combine(Path.GetTempPath(), "FsAutoComplete.Tests", Guid.NewGuid().ToString())

      let (server: IFSharpLspServer, _event) = createServer ()

      let p: InitializeParams =
        { ProcessId = Some 1
          RootPath = Some __SOURCE_DIRECTORY__
          Locale = None
          RootUri = None
          InitializationOptions = Some(Server.serialize defaultConfigDto)
          Capabilities = clientCaps
          ClientInfo =
            Some
              { Name = "FSAC Tests"
                Version = Some "0.0.0" }
          WorkspaceFolders =
            Some
              [| { Uri = Path.FilePathToUri tempDir
                   Name = "Test Folder" } |]
          Trace = None
          WorkDoneToken = None }

      let! result = server.Initialize p

      match result with
      | Result.Ok res ->
        do! server.Initialized()

        Expect.equal
          res.Capabilities.CodeActionProvider
          (Some(
            U2.C2
              { CodeActionOptions.ResolveProvider = None
                CodeActionOptions.CodeActionKinds = None
                WorkDoneProgress = Some false }
          ))
          "Code Action Provider"

        Expect.equal
          res.Capabilities.CodeLensProvider
          (Some
            { CodeLensOptions.ResolveProvider = Some true
              WorkDoneProgress = Some false })
          "Code Lens Provider"

        Expect.equal res.Capabilities.DefinitionProvider (Some(U2.C1 true)) "Definition Provider"
        Expect.equal res.Capabilities.DocumentFormattingProvider (Some(U2.C1 true)) "Document Formatting Provider"
        Expect.equal res.Capabilities.DocumentHighlightProvider (Some(U2.C1 true)) "Document Highlighting Provider"
        Expect.equal res.Capabilities.DocumentLinkProvider None "Document Link Provider"
        Expect.equal res.Capabilities.DocumentOnTypeFormattingProvider None "Document OnType Formatting Provider"

        Expect.equal
          res.Capabilities.DocumentRangeFormattingProvider
          (Some(U2.C1 true))
          "Document Range Formatting Provider"

        Expect.equal
          res.Capabilities.DocumentSymbolProvider
          (Some(
            U2.C2
              { Label = Some "F#"
                WorkDoneProgress = Some false }
          ))
          "Document Symbol Provider"

        Expect.equal res.Capabilities.ExecuteCommandProvider None "Execute Command Provider"
        Expect.equal res.Capabilities.Experimental None "Experimental"
        Expect.equal res.Capabilities.HoverProvider (Some(U2.C1 true)) "Hover Provider"
        Expect.equal res.Capabilities.ImplementationProvider (Some(U3.C1 true)) "Implementation Provider"
        Expect.equal res.Capabilities.ReferencesProvider (Some(U2.C1 true)) "References Provider"

        Expect.equal
          res.Capabilities.RenameProvider
          (Some(
            U2.C2
              { PrepareProvider = Some true
                WorkDoneProgress = Some false }
          ))
          "Rename Provider"

        Expect.equal
          res.Capabilities.SignatureHelpProvider
          (Some
            { TriggerCharacters = Some [| "("; ","; " " |]
              RetriggerCharacters = Some [| ","; ")"; " " |]
              WorkDoneProgress = Some false })
          "Signature Help Provider"

        let td =
          { TextDocumentSyncOptions.Default with
              OpenClose = Some true
              Change = Some TextDocumentSyncKind.Incremental
              Save = Some(U2.C2 { IncludeText = Some true }) }

        Expect.equal res.Capabilities.TextDocumentSync (Some(U2.C1 td)) "Text Document Provider"
        Expect.equal res.Capabilities.TypeDefinitionProvider (Some(U3.C1 true)) "Type Definition Provider"

        Expect.equal
          res.Capabilities.WorkspaceSymbolProvider
          (Some(
            U2.C2
              { ResolveProvider = Some true
                WorkDoneProgress = Some false }
          ))
          "Workspace Symbol Provider"

        Expect.equal res.Capabilities.FoldingRangeProvider (Some(U3.C1 true)) "Folding Range Provider active"
      | Result.Error _e -> failtest "Initialization failed"
    })

let validateSymbolExists msgType symbolInfos predicate =
  Expect.exists symbolInfos predicate $"{msgType}s do not contain the expected symbol"

let allSymbolInfosExist (infos: SymbolInformation seq) predicates =
  predicates |> List.iter (validateSymbolExists (nameof SymbolInformation) infos)

let allWorkspaceSymbolsExist (infos: WorkspaceSymbol seq) predicates =
  predicates |> List.iter (validateSymbolExists (nameof WorkspaceSymbol) infos)

let allDocumentSymbolsExist (infos: DocumentSymbol seq) predicates =
  predicates |> List.iter (validateSymbolExists (nameof DocumentSymbol) infos)

///Tests for getting document symbols
let documentSymbolTest state =
  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "DocumentSymbolTest")
      let! (server, _event) = serverInitialize path defaultConfigDto state
      let path = Path.Combine(path, "Script.fsx")
      let tdop: DidOpenTextDocumentParams = { TextDocument = loadDocument path }
      do! server.TextDocumentDidOpen tdop
      return (server, path)
    }
    |> Async.Cache

  testList
    "Document Symbols Tests"
    [ testCaseAsync "Get Document Symbols"
      <| async {
        let! server, path = server

        let p: DocumentSymbolParams =
          { TextDocument = { Uri = Path.FilePathToUri path }
            WorkDoneToken = None
            PartialResultToken = None }

        let! res = server.TextDocumentDocumentSymbol p

        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Ok None -> failtest "Request none"
        | Ok(Some(U2.C1 symbolInformations)) ->
          Expect.equal symbolInformations.Length 15 "Document Symbol has all symbols"

          allSymbolInfosExist symbolInformations [ fun n -> n.Name = "MyDateTime" && n.Kind = SymbolKind.Class ]

        | Ok(Some(U2.C2 documentSymbols)) ->
          Expect.equal documentSymbols.Length 15 "Document Symbol has all symbols"

          allDocumentSymbolsExist documentSymbols [ fun n -> n.Name = "MyDateTime" && n.Kind = SymbolKind.Class ]
      } ]

let workspaceSymbolTest state =
  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "WorkspaceSymbolTest")
      let! (server, _event) = serverInitialize path defaultConfigDto state
      let path = Path.Combine(path, "Script.fsx")
      let tdop: DidOpenTextDocumentParams = { TextDocument = loadDocument path }
      do! server.TextDocumentDidOpen tdop
      return (server, path)
    }
    |> Async.Cache

  testList
    "Workspace Symbols Tests"
    [ testCaseAsync "Get Workspace Symbols Using Filename of Script File as Query"
      <| async {
        let! server, _path = server

        let p: WorkspaceSymbolParams =
          { Query = "Script"
            WorkDoneToken = None
            PartialResultToken = None }

        let! res = server.WorkspaceSymbol p

        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Ok None -> failtest "Request none"
        | Ok(Some(U2.C1 symbolInfos)) ->
          Expect.equal symbolInfos.Length 1 "Workspace did not find all the expected symbols"

          allSymbolInfosExist symbolInfos [ fun n -> n.Name = "Script" && n.Kind = SymbolKind.Module ]

        | Ok(Some(U2.C2 workspaceSymbols)) ->
          Expect.equal workspaceSymbols.Length 1 "Workspace did not find all the expected symbols"

          allWorkspaceSymbolsExist workspaceSymbols [ fun n -> n.Name = "Script" && n.Kind = SymbolKind.Module ]
      }

      testCaseAsync "Get Workspace Symbols Using Query w/ Text"
      <| async {
        let! server, _path = server

        let p: WorkspaceSymbolParams =
          { Query = "X"
            WorkDoneToken = None
            PartialResultToken = None }

        let! res = server.WorkspaceSymbol p

        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Ok None -> failtest "Request none"
        | Ok(Some(U2.C1 symbolInfos)) ->
          Expect.equal symbolInfos.Length 5 "Workspace did not find all the expected symbols"

          allSymbolInfosExist
            symbolInfos
            [ fun n -> n.Name = "X" && n.Kind = SymbolKind.Class
              fun n -> n.Name = "X" && n.Kind = SymbolKind.Class
              fun n -> n.Name = "X.X" && n.Kind = SymbolKind.Module
              fun n -> n.Name = "X.Y" && n.Kind = SymbolKind.Module
              fun n -> n.Name = "X.Z" && n.Kind = SymbolKind.Class ]

        | Ok(Some(U2.C2 workspaceSymbols)) ->
          Expect.equal workspaceSymbols.Length 5 "Workspace did not find all the expected symbols"

          allWorkspaceSymbolsExist
            workspaceSymbols
            [ fun n -> n.Name = "X" && n.Kind = SymbolKind.Class
              fun n -> n.Name = "X" && n.Kind = SymbolKind.Class
              fun n -> n.Name = "X.X" && n.Kind = SymbolKind.Module
              fun n -> n.Name = "X.Y" && n.Kind = SymbolKind.Module
              fun n -> n.Name = "X.Z" && n.Kind = SymbolKind.Class ]
      }

      testCaseAsync "Get Workspace Symbols Using Query w/o Text"
      <| async {
        let! server, _path = server

        let p: WorkspaceSymbolParams =
          { Query = String.Empty
            WorkDoneToken = None
            PartialResultToken = None }

        let! res = server.WorkspaceSymbol p

        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Ok None -> failtest "Request none"
        | Ok(Some(U2.C1 res)) -> Expect.equal res.Length 0 "Workspace found symbols when we didn't expect to find any"
        | Ok(Some(U2.C2 res)) -> Expect.equal res.Length 0 "Workspace found symbols when we didn't expect to find any"
      } ]


let foldingTests state =
  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "FoldingTests")

      let! (server, event) = serverInitialize path defaultConfigDto state
      do! waitForWorkspaceFinishedParsing event
      let libraryPath = Path.Combine(path, "Library.fs")
      let libFile = loadDocument libraryPath
      let tdop: DidOpenTextDocumentParams = { TextDocument = libFile }
      do! server.TextDocumentDidOpen tdop
      return server, libraryPath
    }
    |> Async.Cache

  testSequenced
  <| testList
    "folding tests"
    [ testCaseAsync
        "can get ranges for sample file"
        (async {
          let! server, libraryPath = server

          let! rangeResponse =
            server.TextDocumentFoldingRange(
              { TextDocument = { Uri = Path.FilePathToUri libraryPath }
                WorkDoneToken = None
                PartialResultToken = None }
            )

          match rangeResponse with
          | Ok(Some(ranges)) ->
            Expect.hasLength ranges 3 "Should be three ranges: one comment, one module, one let-binding"
          | Ok(None) -> failtestf "No ranges found in file, problem parsing?"
          | LspResult.Error e -> failtestf "Error from range LSP call: %A" e
        }) ]

let inline _lang<'t when 't: (member Language: string)> (x: 't) = x.Language

let inline _value<'t when 't: (member Value: string)> (x: 't) = x.Value

[<return: Struct>]
let inline (|FSharpLanguage|_|) x = if _lang x = "fsharp" then ValueSome() else ValueNone

let inline (|Value|) x = _value x

let tooltipTests state =
  let (|Signature|_|) (hover: Hover) =
    match hover with
    | { Contents = U3.C3 [| U2.C2(FSharpLanguage & Value tooltip); U2.C1 _docComment; U2.C1 _fullname; U2.C1 _assembly |] } ->
      Some tooltip
    | { Contents = U3.C3 [| U2.C2(FSharpLanguage & Value tooltip)
                            U2.C1 _docComment
                            U2.C1 _showDocumentationLink
                            U2.C1 _fullname
                            U2.C1 _assembly |] } -> Some tooltip
    | _ -> None

  let (|Description|_|) (hover: Hover) =
    match hover with
    | { Contents = U3.C3 [| U2.C2(FSharpLanguage & Value _tooltip); U2.C1 description |] } -> Some description
    | { Contents = U3.C3 [| U2.C2(FSharpLanguage & Value _tooltip); U2.C1 description; U2.C1 _fullname; U2.C1 _assembly |] } ->
      Some description
    | { Contents = U3.C3 [| U2.C2(FSharpLanguage & Value _tooltip)
                            U2.C1 description
                            U2.C1 _showDocumentationLink
                            U2.C1 _fullname
                            U2.C1 _assembly |] } -> Some description
    | _ -> None

  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "Tooltips")
      let scriptPath = Path.Combine(path, "Script.fsx")
      let! (server, events) = serverInitialize path defaultConfigDto state
      do! waitForWorkspaceFinishedParsing events
      do! server.TextDocumentDidOpen { TextDocument = loadDocument scriptPath }

      match! waitForParseResultsForFile "Script.fsx" events with
      | Ok() -> () // all good, no parsing/checking errors
      | Core.Result.Error errors -> failtestf "Errors while parsing script %s: %A" scriptPath errors

      return server, scriptPath
    }
    |> Async.Cache

  let verifySignature name line character expectedSignature =
    testCaseAsync
      name
      (async {
        let! server, scriptPath = server

        let pos: TextDocumentPositionParams =
          { TextDocument = { Uri = sprintf "file://%s" scriptPath }
            Position = { Line = line; Character = character } }

        match! server.TextDocumentHover pos with
        | Ok(Some(Signature signature)) ->
          Expect.equal signature expectedSignature (sprintf "Should have a signature of '%s'" expectedSignature)
        | Ok response -> failtestf "Should have gotten signature but got %A" response
        | Result.Error errors -> failtestf "Error while getting signature: %A" errors
      })

  let concatLines = String.concat Environment.NewLine

  let verifyDescriptionImpl testCaseAsync name line character expectedDescription =
    let expectedDescription = concatLines expectedDescription

    testCaseAsync
      (sprintf name)
      (async {
        let! server, scriptPath = server

        let pos: TextDocumentPositionParams =
          { TextDocument = { Uri = sprintf "file://%s" scriptPath }
            Position = { Line = line; Character = character } }

        match! server.TextDocumentHover pos with
        | Ok(Some(Description description)) ->
          Expect.equal description expectedDescription (sprintf "Should have a description of '%s'" expectedDescription)
        | Ok response -> failtestf "Should have gotten description but got %A" response
        | Result.Error errors -> failtestf "Error while getting description: %A" errors
      })

  let verifyDescription name line character expectedDescription =
    verifyDescriptionImpl testCaseAsync name line character expectedDescription

  testSequenced
  <| testList
    "tooltip evaluation"
    [ testList
        "tests"
        [ verifyDescription
            "language keywords"
            0u
            2u
            [ "**Description**"
              ""
              ""
              "Used to associate, or bind, a name to a value or function."
              "" ] // `let` keyword
          verifySignature "trigger.firstCharacter" 0u 4u "val arrayOfTuples: (int * int) array" // verify that even the first letter of the tooltip triggers correctly
          verifySignature "trigger.innerCharacter" 0u 5u "val arrayOfTuples: (int * int) array" // inner positions trigger
          verifySignature "uses prefix generics" 1u 5u "val listOfTuples: list<int * int>" // verify we default to prefix-generics style
          verifySignature "struct tuple handling" 2u 5u "val listOfStructTuples: list<struct (int * int)>" // verify we render struct tuples in a round-tripabble format
          verifySignature
            "strip meaningless units of measure"
            3u
            5u
            "val floatThatShouldHaveGenericReportedInTooltip: float" // verify we strip <MeasureOne> measure annotations
          verifyDescription
            "tooltip formatting for external library functions"
            4u
            4u
            [ "**Description**"
              ""
              "Print to a string using the given format."
              ""
              "**Parameters**"
              ""
              "* `format`: The formatter."
              ""
              "**Returns**"
              ""
              "The formatted result."
              ""
              "**Generic Parameters**"
              ""
              "* `'T` is `System.String`" ] // verify fancy descriptions for external library functions and correct backticks for multiple segments
          verifyDescription
            "multiple generic parameters are explained"
            13u
            11u
            [ "**Description**"
              ""
              ""
              "My super summary"
              " "
              ""
              "**Parameters**"
              ""
              "* `c`: foo"
              "* `b`: bar"
              "* `a`: baz"
              ""
              "**Returns**"
              ""
              "" ]
          verifySignature "nested tuples" 14u 5u "val nestedTuples: int * ((int * int) * int)" // verify that tuples render correctly (parens, etc)
          verifySignature "mixed struct and reference tuples" 15u 5u "val nestedStructTuples: int * struct (int * int)" // verify we can differentiate between struct and non-struct tuples
          verifySignature "units of measure are rendered nicely" 21u 9u "val speed: float<m/s>" // verify we nicely-render measure annotations
          // verify formatting of function-parameters to values. NOTE: we want to wrap them in parens for user clarity eventually.
          verifySignature
            "parameters that are functions are wrapped in parens for clarity"
            26u
            5u
            (concatLines [ "val funcWithFunParam:"; "   f: (int -> unit) ->"; "   i: int"; "   -> unit" ])
          // verify formatting of tuple args.  NOTE: we want to wrap tuples in parens for user clarify eventually.
          verifySignature
            "tuple args are split on each line"
            30u
            12u
            (concatLines [ "val funcWithTupleParam:"; "      int *"; "      int"; "   -> int * int" ])
          // verify formatting of struct tuple args in parameter tooltips.
          verifySignature
            "struct tuple parameters are rendered correctly"
            32u
            12u
            (concatLines
              [ "val funcWithStructTupleParam:"
                "   f: struct (int * int)"
                "   -> struct (int * int)" ])
          verifySignature
            "tuple member parameters are gathered on single line"
            36u
            15u
            (concatLines [ "member Foo:"; "   stuff: int * int * int"; "       -> int" ])
          verifySignature
            "multiple named member parameters are split on each line"
            37u
            15u
            (concatLines [ "member Bar:"; "   a: int *"; "   b: int *"; "   c: int"; "   -> int" ])
          // verify formatting for multi-char operators
          verifySignature
            "multi-char operator rendering"
            39u
            7u
            (concatLines [ "val ( .>> ):"; "   x: int ->"; "   y: int"; "   -> int" ])
          // verify formatting for single-char operators
          verifySignature
            "single-char operator rendering"
            41u
            6u
            (concatLines [ "val ( ^ ):"; "   x: int ->"; "   y: int"; "   -> int" ])
          // verify rendering of generic constraints
          verifySignature
            "generic constraints on parameters"
            43u
            13u
            (concatLines
              [ "val inline add:"
                "   x: 'a (requires static member ( + )) ->"
                "   y: 'b (requires static member ( + ))"
                "   -> 'c" ])
          //verify rendering of solved generic constraints in tooltips for members where they are solved
          verifyDescription
            "solved generic parameters are called out in tooltip"
            45u
            15u
            [ "**Generic Parameters**"
              ""
              "* `'a` is `int`"
              "* `'b` is `int`"
              "* `'c` is `int`" ]
          verifySignature
            "optional member parameters are rendered with leading question mark"
            48u
            28u
            (concatLines
              [ "static member Start:"
                "   body              : (MailboxProcessor<string> -> Async<unit>) *"
                "   ?cancellationToken: System.Threading.CancellationToken"
                "                    -> MailboxProcessor<string>" ])
          verifySignature
            "union case named parameters are rendered"
            54u
            9u
            "Case2 of string * newlineBefore: bool * newlineAfter: bool"
          verifySignature
            "active pattern signatures with potentially-nullable results"
            60u
            7u
            (concatLines
#if NET8_0
              [ "active pattern Value: "
                "   input: Expr"
                "       -> option<obj * System.Type>" ])
#else
              [ "active pattern Value: "
                "   input: Expr"
                "       -> option<objnull * System.Type>" ])
#endif
          verifySignature
            "generic constraint rendering for IWSAM"
            77u
            5u
            (concatLines
              [ "val testIWSAMTest:"
                "      unit"
                "   -> Result<string | null,'e> (requires :> IWSAMTest<'e>)" ])
          verifySignature
            "multiple generic constraints for IWASMs"
            90u
            25u
            (concatLines
              [ "static member GetAwaiter:"
                "   awaitable: 'Awaitable (requires member GetAwaiter)"
                "           -> Awaiter<^Awaiter,'TResult> (requires :> ICriticalNotifyCompletion and member IsCompleted and member GetResult)" ])
          verifySignature
            "basic active pattern"
            65u
            7u
            (concatLines
              [ "active pattern DefaultValue: "
                "   input: Expr"
                "       -> option<System.Type>" ])
          verifySignature
            "basic active pattern with nullability awareness"
            70u
            7u
            (concatLines
#if NET8_0
              [ "active pattern ValueWithName: "
                "   input: Expr"
                "       -> option<obj * System.Type * string>" ])
#else
              [ "active pattern ValueWithName: "
                "   input: Expr"
                "       -> option<objnull * System.Type * string>" ])
#endif
          verifySignature
            "interface with members with and without parameter names"
            96u
            7u
            (concatLines
              [ "interface IWithAndWithoutParamNames"
                "  abstract member WithParamNames: arg1: int * arg2: float -> string"
                "  abstract member WithoutParamNames: int * string -> int" ])
#if NET8_0_OR_GREATER
          verifySignature
            "function with unsolved nullable parameter"
            102u
            7u
            (concatLines [ "val usesNullable:"; "   x: 't | null"; "   -> 't (requires reference)" ])
          verifySignature
            "function with concrete nullable parameter"
            103u
            7u
            (concatLines [ "val usesConcreteNullable:"; "   x: string | null"; "   -> String" ])
          verifySignature
            "function with generic nullable return"
            104u
            7u
            (concatLines [ "val makesNullable:"; "   x: 'x"; "   -> 'x | null (requires reference)" ])
          verifySignature
            "function with concrete nullable return"
            105u
            7u
            (concatLines [ "val makesConcreteNullable:"; "   x: string"; "   -> string | null" ])
          verifySignature
            "function with nullable return from BCL call"
            106u
            7u
            (concatLines [ "val usesBCLNullable:"; "   key: string"; "     -> string | null" ])
          verifySignature "simple value" 107u 7u ("val envKey: string | null")
#endif
          ] ]

let closeTests state =
  // Note: clear diagnostics also implies clear caches (-> remove file & project options from State).
  let root = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "CloseTests")
  let workspace = Path.Combine(root, "Workspace")

  serverTestList "close tests" state defaultConfigDto (Some workspace) (fun server ->
    [ testCaseAsync
        "closing untitled script file clears diagnostics"
        (async {

          let source =
            // The value or constructor 'untitled' is not defined.
            "let foo = untitled"

          let! (doc, diags) = server |> Server.createUntitledDocument source

          Expect.isNonEmpty diags "There should be an error"
          do! doc |> Document.close

          let! diags = doc |> Document.waitForLatestDiagnostics (TimeSpan.FromSeconds 5.0)
          Expect.equal diags Array.empty "There should be a final publishDiagnostics without any diags"
        })
      testCaseAsync
        "closing existing script file inside workspace doesn't clear diagnostics"
        (async {
          let! (doc, diags) = server |> Server.openDocument "Script.fsx"
          Expect.isNonEmpty diags "There should be an error"
          do! doc |> Document.close

          let! diags = doc |> Document.waitForLatestDiagnostics (TimeSpan.FromSeconds 5.0)
          Expect.isNonEmpty diags "There should be no publishDiagnostics without any diags after close"
        })
      testCaseAsync
        "closing existing script file outside workspace clears diagnostics"
        (async {
          let file = Path.Combine(root, "Script.fsx")
          let! (doc, diags) = server |> Server.openDocument file
          Expect.isNonEmpty diags "There should be an error"
          do! doc |> Document.close

          let! diags = doc |> Document.waitForLatestDiagnostics (TimeSpan.FromSeconds 5.0)
          Expect.isEmpty diags "There should be a final publishDiagnostics without any diags"
        })

      testCaseAsync
        "closing existing file inside project & workspace doesn't clear diagnostics"
        (async {
          let! (doc, diags) = server |> Server.openDocument "InsideProjectInsideWorkspace.fs"
          Expect.isNonEmpty diags "There should be an error"
          do! doc |> Document.close

          let! diags = doc |> Document.waitForLatestDiagnostics (TimeSpan.FromSeconds 5.0)
          Expect.isNonEmpty diags "There should be no publishDiagnostics without any diags after close"
        })
      testCaseAsync
        "closing existing file inside project but outside workspace doesn't clear diagnostics"
        (async {
          let file = Path.Combine(root, "InsideProjectOutsideWorkspace.fs")
          let! (doc, diags) = server |> Server.openDocument file
          Expect.isNonEmpty diags "There should be an error"
          do! doc |> Document.close

          let! diags = doc |> Document.waitForLatestDiagnostics (TimeSpan.FromSeconds 5.0)
          Expect.isNonEmpty diags "There should be no publishDiagnostics without any diags after close"
        }) ])

let diagnosticsTest state =
  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "DiagnosticFormatting")
      let! (server, events) = serverInitialize path defaultConfigDto state
      let path = Path.Combine(path, "Program.fs")
      do! waitForWorkspaceFinishedParsing events
      return (server, events, path)
    }
    |> Async.Cache

  testList
    "Diagnostics formatting Tests"
    [ testCaseAsync
        "replacing unicode paragraph by newline"
        (async {
          let! (server, events, path) = server

          let tdop: DidOpenTextDocumentParams = { TextDocument = loadDocument path }
          do! server.TextDocumentDidOpen tdop

          let! compilerResults = waitForCompilerDiagnosticsForFile "Program.fs" events |> Async.StartChild

          match! compilerResults with
          | Ok() -> failtest "should get an F# compiler checking error"
          | Core.Result.Error errors ->
            Expect.exists
              errors
              (fun error ->
                match error.CodeAsString with
                | Some("39" | "41") -> true
                | _ -> false)
              "should have an error FS0039(identifier not defined) or FS0041(a unique overload for method 'TryParse' could not be determined based on type information prior to this program point)"

            Expect.all
              errors
              (fun error -> not <| error.Message.Contains(unicodeParagraphCharacter))
              "message should not contains unicode paragraph characters"
        }) ]
