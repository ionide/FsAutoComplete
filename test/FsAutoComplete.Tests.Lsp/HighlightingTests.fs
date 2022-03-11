module FsAutoComplete.Tests.Highlighting

open System.IO
open Expecto
open Helpers
open FsAutoComplete.LspHelpers
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete.Utils

let tests state =
  let testPath = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "HighlightingTest")
  let scriptPath = Path.Combine(testPath, "Script.fsx")

  let server =
    async {
      let! (server, event) = serverInitialize testPath defaultConfigDto state
      let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument scriptPath }

      do! server.TextDocumentDidOpen tdop
      match! waitForParseResultsForFile "Script.fsx" event with
      | Ok () -> return server
      | Error errors ->
        let errorStrings = errors |> Array.map (fun e -> e.DebuggerDisplay) |> String.concat "\n\t* "
        return failtestf "Errors while parsing highlighting script:\n\t* %s" errorStrings
    }
    |> Async.Cache

  let decodeHighlighting (data: uint32 []) =
    let zeroLine = [| 0u; 0u; 0u; 0u; 0u |]

    let lines =
      Array.append [| zeroLine |] (Array.chunkBySize 5 data)

    let structures =
      let mutable lastLine = 0
      let mutable lastCol = 0
      lines
      |> Array.map (fun current ->
        let startLine = lastLine + int current.[0]
        let startCol = if current.[0] = 0u then lastCol + int current.[1] else int current.[1]
        let endLine = int startLine // assuming no multiline for now
        let endCol = startCol + int current.[2]
        lastLine <- startLine
        lastCol <- startCol
        let tokenType = enum<ClassificationUtils.SemanticTokenTypes> (int current.[3])
        let tokenMods = enum<ClassificationUtils.SemanticTokenModifier> (int current.[4])
        let range =
          { Start = { Line = startLine; Character = startCol }
            End   = { Line = endLine; Character = endCol }}
        range, tokenType, tokenMods
      )

    structures

  let fullHighlights =
    async {
      let p : SemanticTokensParams = { TextDocument = { Uri = Path.FilePathToUri scriptPath } }
      let! server = server
      let! highlights = server.TextDocumentSemanticTokensFull p
      match highlights with
      | Ok (Some highlights) ->
        let decoded =
          highlights.Data
          |> decodeHighlighting
        // printfn "%A" decoded
        return decoded
      | Ok None ->
        return failtestf "Expected to get some highlighting"
      | Error e ->
        return failtestf "error of %A" e
    } |> Async.Cache

  let rangeContainsRange (parent: Range) (child: Position) =
    parent.Start.Line <= child.Line &&
    parent.Start.Character <= child.Character &&
    parent.End.Line >= child.Line &&
    parent.End.Character >= child.Character

  let tokenIsOfType ((line, char) as pos) testTokenType (highlights: (Range * ClassificationUtils.SemanticTokenTypes * ClassificationUtils.SemanticTokenModifier) [] Async) =
    testCaseAsync $"can find token of type {testTokenType} at %A{pos}" (async {
      let! highlights = highlights
      let pos = { Line = line; Character = char }
      Expect.exists
        highlights
        ((fun (r, token, _modifiers) ->
          rangeContainsRange r pos
          && token = testTokenType))
        $"Could not find a highlighting range that contained (%d{line},%d{char}) and type %A{testTokenType} in the token set %A{highlights}"
    })

  /// this tests the range endpoint by getting highlighting for a range then doing the normal highlighting test
  let tokenIsOfTypeInRange ((startLine, startChar), (endLine, endChar)) ((line, char)) testTokenType =
    testCaseAsync $"can find token of type {testTokenType} in a subrange from ({startLine}, {startChar})-({endLine}, {endChar})" (async {
      let! server = server
      let range: Range =
        { Start = { Line = startLine; Character = startChar}
          End = { Line = endLine; Character = endChar }}
      let pos = { Line = line; Character = char }
      match! server.TextDocumentSemanticTokensRange { Range = range; TextDocument =  { Uri = Path.FilePathToUri scriptPath } } with
      | Ok (Some highlights) ->
        let decoded = decodeHighlighting highlights.Data
        Expect.exists
          decoded
          (fun (r, token, _modifiers) ->
            rangeContainsRange r pos
            && token = testTokenType)
          "Could not find a highlighting range that contained the given position"
      | Ok None -> failtestf "Expected to get some highlighting"
      | Error e -> failtestf "error of %A" e
    })

  testSequenced <| testList "Document Highlighting Tests" [
    testList "tests" [
      tokenIsOfType (0, 29) ClassificationUtils.SemanticTokenTypes.TypeParameter fullHighlights // the `^a` type parameter in the SRTP constraint
      tokenIsOfType (0, 44) ClassificationUtils.SemanticTokenTypes.Member fullHighlights // the `PeePee` member in the SRTP constraint
      tokenIsOfType (3, 52) ClassificationUtils.SemanticTokenTypes.Type fullHighlights // the `string` type annotation in the PooPoo srtp member
      tokenIsOfType (6, 21) ClassificationUtils.SemanticTokenTypes.EnumMember fullHighlights // the `PeePee` AP application in the `yeet` function definition
      tokenIsOfType (9, 10) ClassificationUtils.SemanticTokenTypes.Type fullHighlights //the `SomeJson` type alias should be a type
      tokenIsOfType (15, 2) ClassificationUtils.SemanticTokenTypes.Namespace fullHighlights // tests that module coloration isn't overwritten by function coloration when a module function is used, so Foo in Foo.x should be module-colored
    ]
    testCaseAsync "cleanup" (async {
      let! server = server
      do! server.Shutdown()
    })
  ]
