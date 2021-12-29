module FsAutoComplete.Tests.ExtensionsTests

open System
open Expecto
open System.IO
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open Helpers
open FsAutoComplete.Lsp


let fsdnTest state =

  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "FsdnTest")
      let! (server, event) = serverInitialize path defaultConfigDto state
      do! waitForWorkspaceFinishedParsing event
      return server
    }
    |> Async.Cache

  testList "FSDN Tests" [
      testCaseAsync "FSDN on list" (async {
        let! server = server
        let p : FsdnRequest = {
            Query = "('a -> 'b) -> 'a list -> 'b list"
        }
        let! res = server.FSharpFsdn p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok n ->
          Expect.stringContains n.Content "List.map" (sprintf "the List.map is a valid response, but was %A" n)
          let r = JsonSerializer.readJson<CommandResponse.ResponseMsg<CommandResponse.FsdnResponse>>(n.Content)
          Expect.equal r.Kind "fsdn" (sprintf "fsdn response, but was %A, from json %A" r n)
          Expect.contains r.Data.Functions "List.map" (sprintf "the List.map is a valid response, but was %A, from json %A" r n)
      })
      testCaseAsync "cleanup" (async {
        let! server = server
        do! server.Shutdown()
      })
  ]

let uriTests =
  let verifyUri (given: string) (expectedLocal: string) = test (sprintf "roundtrip '%s' -> '%s'" given expectedLocal) {
    let actual = Path.FileUriToLocalPath given
    Expect.equal actual expectedLocal (sprintf "LocalPath of '%s' should be '%s'" given expectedLocal)
  }

  let convertRawPathToUri (rawPath: string) (expectedPath: string) = test (sprintf "convert '%s' -> '%s'" rawPath expectedPath) {
    let createdFilePath = Path.FilePathToUri rawPath
    let createdUri = createdFilePath |> Uri |> string
    Expect.equal createdUri expectedPath (sprintf "converting raw path '%s' should generate a Uri with LocalPath '%s'" createdFilePath expectedPath)
  }

  let samples =
    [ "file:///c%3A/foo/bar/baz", "c:/foo/bar/baz"
      "file:///c%3A/foo/bar bar/baz", "c:/foo/bar bar/baz" // spaces, windows-root
      "file:///Users/bob jones/foo/bar", "/Users/bob jones/foo/bar" // spaces, unix-root
      "file:///Users/bobjones/foo/bar", "/Users/bobjones/foo/bar"
      "file:///c%3A/f%23/bar/baz", "c:/f#/bar/baz" // escaped chars, windows-root
      "file:///Users/carlyrae/oss/f%23test", "/Users/carlyrae/oss/f#test" // escaped chars, unix-root
      "file:///c%3A/carly rae/oss/f%23test", "c:/carly rae/oss/f#test" // spaces and escaped chars, windows-root
      "file:///Users/carly rae/oss/f%23test", "/Users/carly rae/oss/f#test" // spaces and escaped chars, unix-root
      "file:///d%3A/code/Saturn/src/Saturn/Utils.fs", "d:/code/Saturn/src/Saturn/Utils.fs"
    ]

  testList "Uri tests" [
    testList "roundtrip tests" (samples |> List.map (fun (uriForm, filePath) -> verifyUri uriForm filePath))
    testList "fileName to uri tests" (samples |> List.map (fun (uriForm, filePath) -> convertRawPathToUri filePath uriForm))
  ]

/// Tests for linter
let linterTests state =
  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "LinterTest")
      let! (server, events) = serverInitialize path {defaultConfigDto with Linter = Some true} state
      let path = Path.Combine(path, "Script.fsx")
      let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path}
      do! server.TextDocumentDidOpen tdop
      let! diags =
        events
        |> linterDiagnostics "Script.fsx"
        |> diagnosticsToResult
        |> Async.AwaitObservable
        |> AsyncResult.bimap (fun _ -> failtest "Should have errors") id
      return (server, path, diags)
    }
    |> Async.Cache

  let urlFor (code: string option) =
    Some
     { Href =
        Some (System.Uri $"http://fsprojects.github.io/FSharpLint/how-tos/rules/%s{code.Value}.html") }

  let expectedDiagnostics =
    [|
      {
        Range = { Start = { Line = 0; Character = 7}; End = {Line = 0; Character = 11}}
        Severity = Some DiagnosticSeverity.Information
        Code = Some "FL0042"
        Source = "F# Linter"
        Message = "Consider changing `test` to PascalCase."
        RelatedInformation = None
        Tags = None
        Data = None
        CodeDescription = None
      }, "Test"
      {
        Range = { Start = { Line = 1; Character = 16 }
                  End = { Line = 1; Character = 25 } }
        Severity = Some DiagnosticSeverity.Information
        Code = Some "FL0065"
        Source = "F# Linter"
        Message = "`not (a = b)` might be able to be refactored into `a <> b`."
        RelatedInformation = None
        Tags = None
        Data = None
        CodeDescription = None
      }, "a <> b"
      {
        Range = { Start = { Line = 2; Character = 16 }
                  End = { Line = 2; Character = 26 } }
        Severity = Some DiagnosticSeverity.Information
        Code = Some "FL0065"
        Source = "F# Linter"
        Message = "`not (a <> b)` might be able to be refactored into `a = b`."
        RelatedInformation = None
        Tags = None
        Data = None
        CodeDescription = None
      }, "a = b"
      {
        Range = { Start = { Line = 3; Character = 12 }
                  End = { Line = 3; Character = 22 } }
        Severity = Some DiagnosticSeverity.Information
        Code = Some "FL0065"
        Source = "F# Linter"
        Message = "`fun x -> x` might be able to be refactored into `id`."
        RelatedInformation = None
        Tags = None
        Data = None
        CodeDescription = None
      }, "id"
      {
        Range = { Start = { Line = 4; Character = 12 }
                  End = { Line = 4; Character = 20 } }
        Severity = Some DiagnosticSeverity.Information
        Code = Some "FL0065"
        Source = "F# Linter"
        Message = "`not true` might be able to be refactored into `false`."
        RelatedInformation = None
        Tags = None
        Data = None
        CodeDescription = None
      }, "false"
      {
        Range = { Start = { Line = 5; Character = 12 }
                  End = { Line = 5; Character = 21 } }
        Severity = Some DiagnosticSeverity.Information
        Code = Some "FL0065"
        Source = "F# Linter"
        Message = "`not false` might be able to be refactored into `true`."
        RelatedInformation = None
        Tags = None
        Data = None
        CodeDescription = None
      }, "true"
      {
        Range = { Start = { Line = 7; Character = 14 }
                  End = { Line = 7; Character = 21 } }
        Severity = Some DiagnosticSeverity.Information
        Code = Some "FL0065"
        Source = "F# Linter"
        Message = "`a <> true` might be able to be refactored into `not a`."
        RelatedInformation = None
        Tags = None
        Data = None
        CodeDescription = None
      }, "not a"
      {
        Range = { Start = { Line = 8; Character = 14 }
                  End = { Line = 8; Character = 20 } }
        Severity = Some DiagnosticSeverity.Information
        Code = Some "FL0065"
        Source = "F# Linter"
        Message = "`x = null` might be able to be refactored into `isNull x`."
        RelatedInformation = None
        Tags = None
        Data = None
        CodeDescription = None
      }, "isNull a"
      {
        Range = { Start = { Line = 9; Character = 14 }
                  End = { Line = 9; Character = 37 } }
        Severity = Some DiagnosticSeverity.Information
        Code = Some "FL0065"
        Source = "F# Linter"
        Message = "`List.head (List.sort x)` might be able to be refactored into `List.min x`."
        RelatedInformation = None
        Tags = None
        Data = None
        CodeDescription = None
      }, "List.min a"
    |]
    |> Array.map (fun (diag, text) -> { diag with CodeDescription = urlFor diag.Code; Data = Some (box [{ Range = diag.Range; NewText = text  }]) })

  testSequenced <| testList "Linter Test" [
    yield testCaseAsync "Linter Diagnostics" (async {
      let! (_, _, diags) = server
      Seq.zip diags expectedDiagnostics
      |> Seq.iteri (fun idx (actual, expected) ->
        Expect.equal actual expected $"%d{idx} - Linter messages and codefixes match"
      )
    })

    yield testCaseAsync "cleanup" (async {
      let! server, _, _ = server
      do! server.Shutdown()
    })

  ]

let formattingTests state =
  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "Formatting")
      let! (server, events) = serverInitialize path defaultConfigDto state
      do! dotnetToolRestore path // need to restore CLI tools in order to use fantomas
      return server, events, path
    }
    |> Async.Cache

  /// normalize line endings in the tests to ensure OS-independent values/limit variability
  let normalizeLineEndings (s: string) = s.Replace("\r\n", "\n")

  let editForWholeFile sourceFile expectedFile =
    let sourceLines = File.ReadAllLines sourceFile
    let start = { Line = 0; Character = 0 }
    let ``end`` = { Line = sourceLines.Length - 1; Character = sourceLines.[sourceLines.Length - 1].Length }
    let expectedText = File.ReadAllText expectedFile
    { Range = { Start = start; End = ``end`` }; NewText = normalizeLineEndings expectedText }

  let verifyFormatting scenario document =
    testCaseAsync $"{scenario}-{document}" (async {
    let! (server, events, rootPath) = server
    let sourceFile = Path.Combine(rootPath, sprintf "%s.input.fsx" document)
    let expectedFile = Path.Combine(rootPath, sprintf "%s.expected.fsx" document)
    let expectedTextEdit = editForWholeFile sourceFile expectedFile
    do! server.TextDocumentDidOpen { TextDocument = loadDocument sourceFile }
    match! waitForParseResultsForFile (Path.GetFileName sourceFile) events with
    | Ok () ->
      match! server.TextDocumentFormatting { TextDocument = { Uri = Path.FilePathToUri sourceFile }
                                             Options = FormattingOptions(TabSize = 4, InsertSpaces = true) } with
      | Ok (Some [|edit|]) ->
        let normalized = { edit with NewText = normalizeLineEndings edit.NewText }
        Expect.equal normalized expectedTextEdit "should replace the entire file range with the expected content"
      | Ok other ->
        failwithf "Invalid formatting result: %A" other
      | Result.Error e ->
        failwithf "Error while formatting %s: %A" sourceFile e
    | Core.Result.Error errors ->
      failwithf "Errors while parsing script %s: %A" sourceFile errors
  })

  testSequenced <| testList "fantomas integration" [
    testList "tests" [
      verifyFormatting "can replace entire content of file when formatting whole document" "endCharacter"
    ]
    testCaseAsync "cleanup" (async {
      let! server, _, _ = server
      do! server.Shutdown()
    })
  ]

// let fakeInteropTests toolsPath =
//   let serverStart = lazy (
//     let folderPath = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "FakeInterop")
//     let (server, events) = serverInitialize folderPath defaultConfigDto toolsPath
//     let buildScript = "build.fsx"
//     do waitForWorkspaceFinishedParsing events
//     server, events, folderPath, buildScript
//   )
//   let serverTest f () = f serverStart.Value

//   testList "fake integration" [
//     testCase "can typecheck a fake script including uses of paket-delivered types" (serverTest (fun (server, events, rootPath, scriptName) ->
//         do server.TextDocumentDidOpen { TextDocument = loadDocument (Path.Combine(rootPath, scriptName)) } |> Async.RunSynchronously
//         match waitForParseResultsForFile scriptName events with
//         | Ok () ->
//           () // all good, no parsing/checking errors
//         | Core.Result.Error errors ->
//           failwithf "Errors while parsing script %s: %A" (Path.Combine(rootPath, scriptName)) errors
//         ))
//   ]

let analyzerTests state =
  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "Analyzers")
      // because the analyzer is a project this project has a reference, the analyzer can be
      // found in alongside this project, so we can use the directory this project is in
      let analyzerPath = System.IO.Path.GetDirectoryName (System.Reflection.Assembly.GetExecutingAssembly().Location)
      let analyzerEnabledConfig =
        { defaultConfigDto with
            EnableAnalyzers = Some true
            AnalyzersPath = Some [| analyzerPath |] }

      let! (server, events) = serverInitialize path analyzerEnabledConfig state
      let scriptPath = Path.Combine(path, "Script.fsx")
      do! Async.Sleep (TimeSpan.FromSeconds 5.)
      do! waitForWorkspaceFinishedParsing events
      do! server.TextDocumentDidOpen { TextDocument = loadDocument scriptPath }
      return server, events, path, scriptPath
    }
    |> Async.Cache

  testSequenced <|
    testList "analyzer integration" [
      testList "tests" [
        testCaseAsync "can run analyzer on file" (async {
          let! (server, events, rootPath, testFilePath) = server
          let! diagnostics = analyzerDiagnostics (System.IO.Path.GetFileName testFilePath) events |> Async.AwaitObservable
          let expected =
            [|{ Range = { Start = { Line = 3
                                    Character = 13 }
                          End = { Line = 3
                                  Character = 31 } }
                Severity = Some DiagnosticSeverity.Warning
                Code = Some "OV001"
                Source = "F# Analyzers (Option.Value analyzer)"
                Message = "Option.Value shouldn't be used"
                RelatedInformation = None
                Tags = None
                Data = None
                CodeDescription = None }|]
          Expect.equal diagnostics expected "Expected a single analyzer warning about options"
        })
      ]
      testCaseAsync "cleanup" (async {
        let! server, _, _, _ = server
        do! server.Shutdown()
      })
    ]

let signatureTests state =
  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "Signature")
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

  let verifySignature line (characterStart, characterEnd) expectedSignature =
    let verifyAt character = async {
      let! server, scriptPath = server
      let pos: TextDocumentPositionParams = {
        TextDocument =  { Uri = Path.FilePathToUri scriptPath }
        Position = { Line = line; Character = character }
      }
      match! server.FSharpSignature pos with
      | Ok { Content = content } ->
        let r = JsonSerializer.readJson<CommandResponse.ResponseMsg<string>>(content)
        Expect.equal r.Kind "typesig" "Should have a kind of 'typesig'"
        Expect.equal r.Data expectedSignature (sprintf "Should have a signature of '%s' at character %d" expectedSignature character)
      | Result.Error errors ->
        failtestf "Error while getting signature: %A" errors
    }

    testCaseAsync (sprintf "fsharp/signature for line %d characters [%d, %d] should be '%s'" line characterStart characterEnd expectedSignature) (
      [ for c in characterStart .. characterEnd -> verifyAt c ]
      |> Async.Sequential
      |> Async.map (fun _ -> ())
    )

  testSequenced <|
    testList "signature evaluation" [
      testList "tests" [
        verifySignature 0 (4, 16) "val arrayOfTuples: (int * int)[]"
        verifySignature 1 (4, 15) "val listOfTuples: (int * int) list"
        verifySignature 2 (4, 15) "val someFunction: a: 'a -> unit"
      ]
      testCaseAsync "cleanup" (async {
        let! server, _ = server
        do! server.Shutdown()
      })
  ]

