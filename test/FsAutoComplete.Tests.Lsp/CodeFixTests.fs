module FsAutoComplete.Tests.CodeFixTests

open Expecto
open System.IO
open Helpers
open LanguageServerProtocol.Types
open FsAutoComplete.Utils

let abstractClassGenerationTests state =
  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "AbstractClassGeneration")
      let! (server, events) = serverInitialize path { defaultConfigDto with AbstractClassStubGeneration = Some true } state
      do! waitForWorkspaceFinishedParsing events
      let path = Path.Combine(path, "Script.fsx")
      let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path }
      do! server.TextDocumentDidOpen tdop
      let! diagnostics = waitForParseResultsForFile "Script.fsx" events |> AsyncResult.bimap (fun _ -> failtest "Should have had errors") (fun e -> e)
      return (server, path, diagnostics)
    }
    |> Async.Cache

  let canGenerateForLongIdent = testCaseAsync "can generate a derivative of a long ident - System.IO.Stream" (async {
    let! server, file, diagnostics = server
    let diagnostic = diagnostics |> Array.tryFind (fun d -> d.Code = Some "365" && d.Range.Start.Line = 0 ) |> Option.defaultWith (fun _ -> failtest "Should have gotten an error of type 365")
    let! response = server.TextDocumentCodeAction { CodeActionParams.TextDocument = { Uri = Path.FilePathToUri file }
                                                    Range = diagnostic.Range
                                                    Context = { Diagnostics = [| diagnostic |] } }
    match response with
    | Ok (Some (TextDocumentCodeActionResult.CodeActions [| { Title = "Generate abstract class members" } |] )) -> ()
    | Ok other -> failtestf $"Should have generated the rest of the base class, but instead generated %A{other}"
    | Error reason -> failtestf $"Should have succeeded, but failed with %A{reason}"
  })

  let canGenerateForIdent = testCaseAsync "can generate a derivative for a simple ident - Stream" (async {
    let! server, file, diagnostics = server
    let diagnostic = diagnostics |> Array.tryFind (fun d -> d.Code = Some "365" && d.Range.Start.Line = 5 ) |> Option.defaultWith (fun _ -> failtest "Should have gotten an error of type 365")
    let! response = server.TextDocumentCodeAction { CodeActionParams.TextDocument = { Uri = Path.FilePathToUri file }
                                                    Range = diagnostic.Range
                                                    Context = { Diagnostics = [| diagnostic |] } }
    match response with
    | Ok (Some (TextDocumentCodeActionResult.CodeActions [| { Title = "Generate abstract class members" } |] )) -> ()
    | Ok other -> failtestf $"Should have generated the rest of the base class, but instead generated %A{other}"
    | Error reason -> failtestf $"Should have succeeded, but failed with %A{reason}"
  })


  testList "abstract class generation" [
    canGenerateForLongIdent
    canGenerateForIdent
  ]

let generateMatchTests state =
  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "MatchCaseGeneration")
      let! (server, events) = serverInitialize path { defaultConfigDto with UnionCaseStubGeneration = Some true } state
      do! waitForWorkspaceFinishedParsing events
      let path = Path.Combine(path, "Script.fsx")
      let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path }
      do! server.TextDocumentDidOpen tdop
      let! diagnostics = waitForParseResultsForFile "Script.fsx" events |> AsyncResult.bimap (fun _ -> failtest "Should have had errors") (fun e -> e)
      return (server, path, diagnostics)
    }
    |> Async.Cache

  testList "generate match cases" [
    testCaseAsync "can generate match cases for a simple DU" (async {
      let! server, file, diagnostics = server
      let expectedDiagnostic = diagnostics.[0]
      Expect.equal expectedDiagnostic.Code (Some "25") "Should have a empty match warning"
      let! response = server.TextDocumentCodeAction { CodeActionParams.TextDocument = { Uri = Path.FilePathToUri file }
                                                      Range = expectedDiagnostic.Range
                                                      Context = { Diagnostics = [| expectedDiagnostic |] } }
      match response with
      | Ok (Some (TextDocumentCodeActionResult.CodeActions [| { Title = "Generate union pattern match cases" } |] )) -> ()
      | Ok other -> failtestf $"Should have generated the rest of match cases, but instead generated %A{other}"
      | Error reason -> failtestf $"Should have succeeded, but failed with %A{reason}"
    })
  ]

let missingFunKeywordTests state =
  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "MissingFunKeyword")
      let! (server, events) = serverInitialize path defaultConfigDto state
      do! waitForWorkspaceFinishedParsing events
      let path = Path.Combine(path, "Script.fsx")
      let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path }
      do! server.TextDocumentDidOpen tdop
      let! diagnostics = waitForParseResultsForFile "Script.fsx" events |> AsyncResult.bimap (fun _ -> failtest "Should have had errors") (fun e -> e)
      return (server, path, diagnostics)
    }
    |> Async.Cache

  testList "missing fun keyword" [
    testCaseAsync "can generate the fun keyword when error 10 is raised" (async {
      let! server, file, diagnostics = server
      let expectedDiagnostic = diagnostics.[0]
      Expect.equal expectedDiagnostic.Code (Some "10") "Should have a missing fun keyword error"
      let! response = server.TextDocumentCodeAction { CodeActionParams.TextDocument = { Uri = Path.FilePathToUri file }
                                                      Range = expectedDiagnostic.Range
                                                      Context = { Diagnostics = [| expectedDiagnostic |] } }
      match response with
      | Ok (Some (TextDocumentCodeActionResult.CodeActions [| { Title = "Add missing 'fun' keyword"
                                                                Kind = Some "quickfix"
                                                                Edit = { DocumentChanges = Some [| { Edits = [| { NewText = "fun " } |] } |] } } |] )) -> ()
      | Ok other -> failtestf $"Should have generated missing fun keyword, but instead generated %A{other}"
      | Error reason -> failtestf $"Should have succeeded, but failed with %A{reason}"
    })
  ]

let tests state = testList "codefix tests" [
  abstractClassGenerationTests state
  generateMatchTests state
  missingFunKeywordTests state
]
