module FsAutoComplete.Tests.ExtensionsTests

open System
open Expecto
open System.IO
open LanguageServerProtocol
open LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open Helpers


let fsdnTest =
  let serverStart = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "FsdnTest")
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



///Tests for linter
let linterTests =
  let serverStart = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "LinterTest")
    let (server, event) = serverInitialize path {defaultConfigDto with Linter = Some true}

    let m = new System.Threading.ManualResetEvent(false)
    let bag = event |> waitForParsedScript m


    let projectPath = Path.Combine(path, "LinterTest.fsproj")
    parseProject projectPath server |> Async.RunSynchronously
    let path = Path.Combine(path, "Script.fs")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path}
    do server.TextDocumentDidOpen tdop |> Async.RunSynchronously

    m.WaitOne() |> ignore

    (server, path, bag)
  )
  let serverTest f () =
    let (server, path, bag) = serverStart.Value
    f server path bag

  testSequenced <| ptestList "Linter Test" [
    testCase "Linter Diagnostics" (serverTest (fun server path bag ->
      let (b,v) = bag.TryPeek()
      if b then
        let firstDiag = {
          Range = { Start = { Line = 0; Character = 7}; End = {Line = 0; Character = 11}}
          Severity = Some DiagnosticSeverity.Information
          Code = Some "FL0042"
          Source = "F# Linter"
          Message = "Consider changing `test` to PascalCase."
          RelatedInformation = None
          Tags = None}
        let secondDiag = {
          Range = { Start = { Line = 1; Character = 16 }
                    End = { Line = 1; Character = 25 } }
          Severity = Some DiagnosticSeverity.Information
          Code = Some "FL0065"
          Source = "F# Linter"
          Message = "`not (a = b)` might be able to be refactored into `a <> b`."
          RelatedInformation = None
          Tags = None
        }
        let thirdDiag =
          { Range = { Start = { Line = 2; Character = 16 }
                      End = { Line = 2; Character = 26 } }
            Severity = Some DiagnosticSeverity.Information
            Code = Some "FL0065"
            Source = "F# Linter"
            Message = "`not (a <> b)` might be able to be refactored into `a = b`."
            RelatedInformation = None
            Tags = None }

        let fourthDiag =
          { Range = { Start = { Line = 3; Character = 12 }
                      End = { Line = 3; Character = 22 } }
            Severity = Some DiagnosticSeverity.Information
            Code = Some "FL0065"
            Source = "F# Linter"
            Message = "`fun x -> x` might be able to be refactored into `id`."
            RelatedInformation = None
            Tags = None }
        let fifthDiag =
          { Range = { Start = { Line = 4; Character = 12 }
                      End = { Line = 4; Character = 20 } }
            Severity = Some DiagnosticSeverity.Information
            Code = Some "FL0065"
            Source = "F# Linter"
            Message = "`not true` might be able to be refactored into `false`."
            RelatedInformation = None
            Tags = None }
        let sixthDiag =
          { Range = { Start = { Line = 5; Character = 12 }
                      End = { Line = 5; Character = 21 } }
            Severity = Some DiagnosticSeverity.Information
            Code = Some "FL0065"
            Source = "F# Linter"
            Message = "`not false` might be able to be refactored into `true`."
            RelatedInformation = None
            Tags = None }
        let seventhDiag =
          { Range = { Start = { Line = 7; Character = 14 }
                      End = { Line = 7; Character = 21 } }
            Severity = Some DiagnosticSeverity.Information
            Code = Some "FL0065"
            Source = "F# Linter"
            Message = "`a <> true` might be able to be refactored into `not a`."
            RelatedInformation = None
            Tags = None }
        let eigthDiag =
          { Range = { Start = { Line = 8; Character = 14 }
                      End = { Line = 8; Character = 20 } }
            Severity = Some DiagnosticSeverity.Information
            Code = Some "FL0065"
            Source = "F# Linter"
            Message = "`x = null` might be able to be refactored into `isNull x`."
            RelatedInformation = None
            Tags = None }
        let ninthDiag =
          { Range = { Start = { Line = 9; Character = 14 }
                      End = { Line = 9; Character = 37 } }
            Severity = Some DiagnosticSeverity.Information
            Code = Some "FL0065"
            Source = "F# Linter"
            Message = "`List.head (List.sort x)` might be able to be refactored into `List.min x`."
            RelatedInformation = None
            Tags = None }

        let diagnostics = [|firstDiag; secondDiag; thirdDiag; fourthDiag; fifthDiag; sixthDiag; seventhDiag; eigthDiag; ninthDiag|]
        Expect.equal v.Diagnostics diagnostics "Linter messages match"
      else failtest "No diagnostic message recived"
     ))
  ]


let formattingTests =
  let serverStart = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "Formatting")
    let (server, events) = serverInitialize path defaultConfigDto
    do waitForWorkspaceFinishedParsing events
    server, events, path
  )
  let serverTest f () = f serverStart.Value

  let editForWholeFile sourceFile expectedFile =
    let sourceLines = File.ReadAllLines sourceFile
    let start = { Line = 0; Character = 0 }
    let ``end`` = { Line = sourceLines.Length - 1; Character = sourceLines.[sourceLines.Length - 1].Length }
    let expectedText = File.ReadAllText expectedFile
    { Range = { Start = start; End = ``end`` }; NewText = expectedText }

  let verifyFormatting (server: Lsp.FsharpLspServer, events, rootPath) scenario =
    let sourceFile = Path.Combine(rootPath, sprintf "%s.input.fsx" scenario)
    let expectedFile = Path.Combine(rootPath, sprintf "%s.expected.fsx" scenario)
    let expectedTextEdit = editForWholeFile sourceFile expectedFile
    do server.TextDocumentDidOpen { TextDocument = loadDocument sourceFile } |> Async.RunSynchronously
    match waitForParseResultsForFile (Path.GetFileName sourceFile) events with
    | Ok () ->
      match server.TextDocumentFormatting { TextDocument = { Uri = filePathToUri sourceFile }
                                            Options = { TabSize = 4
                                                        InsertSpaces = true
                                                        AdditionalData = dict [] } } |> Async.RunSynchronously with
      | Ok (Some [|edit|]) ->
        Expect.equal edit expectedTextEdit "should replace the entire file range with the expected content"
      | Ok other ->
        failwithf "Invalid formatting result: %A" other
      | Result.Error e ->
        failwithf "Error while formatting %s: %A" sourceFile e
    | Core.Result.Error errors ->
      failwithf "Errors while parsing script %s: %A" sourceFile errors

  testList "fantomas integration" [
    testCase "can replace entire content of file when formatting whole document" (serverTest (fun state ->
      verifyFormatting state "endCharacter"
    ))
  ]

let useFile filePath (server: Lsp.FsharpLspServer) =
  do server.TextDocumentDidOpen { TextDocument = loadDocument filePath } |> Async.RunSynchronously
  { new IDisposable with
     member _.Dispose () =
      do server.TextDocumentDidClose { TextDocument = { Uri = filePathToUri filePath } } |> Async.RunSynchronously
    }

let fakeInteropTests =
  let serverStart = lazy (
    let folderPath = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "FakeInterop")
    let (server, events) = serverInitialize folderPath defaultConfigDto
    let buildScript = "build.fsx"
    do waitForWorkspaceFinishedParsing events
    server, events, folderPath, buildScript
  )
  let serverTest f () = f serverStart.Value

  testSequenced <| testList "fake integration" [
    testCase "can typecheck a fake script including uses of paket-delivered types" (serverTest (fun (server, events, rootPath, scriptName) ->
        let scriptFilePath = Path.Combine (rootPath, scriptName)
        use __ = useFile scriptFilePath server
        match waitForParseResultsForFile scriptName events with
        | Ok () ->
          () // all good, no parsing/checking errors
        | Core.Result.Error errors ->
          failwithf "Errors while parsing script %s: %A" (Path.Combine(rootPath, scriptName)) errors
        ))
    testCase "can determine fake targets list for a fake script" (serverTest (fun (server, events, rootPath, scriptName) ->
        let dotnetBinary =
          match System.Environment.OSVersion.Platform with
          | System.PlatformID.Win32NT
          | System.PlatformID.Win32S
          | System.PlatformID.WinCE
          | System.PlatformID.Win32Windows -> "dotnet.exe"
          | _ -> "dotnet"
        let dotnetPath = Path.Combine(server.Config.DotNetRoot, dotnetBinary)
        let expectedTargets = [|"Default"|]
        let scriptFilePath = Path.Combine(rootPath, scriptName)
        use __ = useFile scriptFilePath server
        match server.FakeTargets({ FileName = Path.Combine(rootPath, scriptName)
                                   FakeContext = { DotNetRuntime = dotnetPath } }) |> Async.RunSynchronously with
        | Ok { WarningsAndErrors = warnings; Targets = targets } ->
          match warnings with
          | [||] -> ()
          | warnings -> printfn "FAKE warnings: %A" warnings
          let targetNames = targets |> Array.map (fun t -> t.Name)
          Expect.equal targetNames expectedTargets "should have found all targets"
        | Core.Result.Error errors ->
          failwithf "Errors while getting targets outline for %s: %A" (Path.Combine(rootPath, scriptName)) errors
        ))
  ]

let analyzerTests =
  let serverStart = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "Analyzers")
    // because the analyzer is a project this project has a reference, the analyzer can ber
    // found in alongside this project, so we can use the directory this project is in
    let analyzerPath = System.IO.Path.GetDirectoryName (System.Reflection.Assembly.GetExecutingAssembly().Location)
    let analyzerEnabledConfig =
      { defaultConfigDto with
          EnableAnalyzers = Some true
          AnalyzersPath = Some [| analyzerPath |] }

    Helpers.runProcess (logDotnetRestore "RenameTest") path "dotnet" "restore"
    |> expectExitCodeZero

    let (server, events) = serverInitialize path analyzerEnabledConfig
    let scriptPath = Path.Combine(path, "Script.fs")
    do waitForWorkspaceFinishedParsing events
    do server.TextDocumentDidOpen { TextDocument = loadDocument scriptPath } |> Async.RunSynchronously
    server, events, path, scriptPath
  )

  let serverTest f () = f serverStart.Value

  testList "analyzer integration" [
    testCase "can run analyzer on file" (serverTest (fun (server, events, rootPath, testFilePath) ->
      do server.TextDocumentDidOpen { TextDocument = loadDocument testFilePath } |> Async.RunSynchronously
      // now wait for analyzer events for the file:

      let diagnostic = analyzerEvents (System.IO.Path.GetFileName testFilePath) events |> Async.AwaitEvent |> Async.RunSynchronously
      let expected =
        [|{ Range = { Start = { Line = 3
                                Character = 13 }
                      End = { Line = 3
                              Character = 31 } }
            Severity = Some DiagnosticSeverity.Warning
            Code = None
            Source = "F# Analyzers (Option.Value analyzer)"
            Message = "Option.Value shouldn't be used"
            RelatedInformation = None
            Tags = None }|]
      Expect.equal diagnostic.Diagnostics expected "Expected a single analyzer warning about options"
    ))
  ]



