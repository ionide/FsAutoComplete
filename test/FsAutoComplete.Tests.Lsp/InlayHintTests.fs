module FsAutoComplete.Tests.InlayHintTests

open Expecto
open System
open System.IO
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open Helpers
open FsToolkit.ErrorHandling

let tests state =
  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "InlayHints")
      let config = defaultConfigDto
      let! (server, events) = serverInitialize path config state
      let path = Path.Combine(path, "Script.fsx")
      let tdop: DidOpenTextDocumentParams = { TextDocument = loadDocument path }
      do! server.TextDocumentDidOpen tdop

      do!
        waitForParseResultsForFile "Script.fsx" events
        |> AsyncResult.bimap id (fun e -> failtest "should have not had check errors")

      return (server, path)
    }
    |> Async.Cache

  let expectedHintsForFile: FsAutoComplete.Lsp.LSPInlayHint [] =
    [| "string", (3, 17), Lsp.InlayHintKind.Type
       "FileInfo", (5, 9), Lsp.InlayHintKind.Type
       "string", (15, 5), Lsp.InlayHintKind.Type
       "fileName", (5, 21), Lsp.InlayHintKind.Parameter
       "format", (15, 16), Lsp.InlayHintKind.Parameter |]
    |> Array.map (fun (text, (line, char), kind) ->
      { Text =
          match kind with
          | Lsp.InlayHintKind.Type -> ": " + text
          | Lsp.InlayHintKind.Parameter -> text + " ="
        // this is for truncated text, which we do not currently hit in our tests
        // TODO: add tests to cover this case
        InsertText =
          match kind with
          | Lsp.InlayHintKind.Type -> Some (": " + text)
          | Lsp.InlayHintKind.Parameter -> None
        Pos = { Line = line; Character = char }
        Kind = kind })

  testList
    "Inlay Hints"
    [ testCaseAsync
        "Can return all inlay hints for a file"
        (async {

          let! (server, path) = server

          let wholeFileRange: Range =
            { Start = { Line = 0; Character = 0 }
              End =
                { Line = Int32.MaxValue
                  Character = Int32.MaxValue } }

          let! doc =
            server.FSharpInlayHints
              { TextDocument = { Uri = path }
                Range = wholeFileRange }

          match doc with
          | Result.Error err -> failtest $"Doc error: {err.Message}"
          | Result.Ok hints -> Expect.equal hints expectedHintsForFile "Can provide all of the hints"
        })

      testCaseAsync
        "Filters hints returned when in a range of the file"
        (async {

          let! (server, path) = server

          let partOfFileRange: Range =
            { Start = { Line = 5; Character = 0 }
              End =
                { Line = Int32.MaxValue
                  Character = Int32.MaxValue } }

          let inRange = Range.rangeContainsPos partOfFileRange

          let expectedHintsWithinRange =
            expectedHintsForFile
            |> Array.filter (fun h -> inRange h.Pos)

          let! doc =
            server.FSharpInlayHints
              { TextDocument = { Uri = path }
                Range = partOfFileRange }

          match doc with
          | Result.Error err -> failtest $"Doc error: {err.Message}"
          | Result.Ok hints ->
            Expect.isNonEmpty hints "Should have had some hints"
            Expect.equal hints expectedHintsWithinRange "Can provide all of the hints that exist in the range"
        })

      testCaseAsync
        "Returns no hints for nonexistent range"
        (async {

          let! (server, path) = server

          let partOfFileRange: Range =
            { Start =
                { Line = Int32.MaxValue - 1
                  Character = Int32.MaxValue - 1 }
              End =
                { Line = Int32.MaxValue
                  Character = Int32.MaxValue } }

          let inRange = Range.rangeContainsPos partOfFileRange

          let! doc =
            server.FSharpInlayHints
              { TextDocument = { Uri = path }
                Range = partOfFileRange }

          match doc with
          | Result.Error err -> failtest $"Doc error: {err.Message}"
          | Result.Ok hints -> Expect.isEmpty hints "Should have had no hints"
        }) ]
