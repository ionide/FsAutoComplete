module FsAutoComplete.Tests.Highlighting

open System.IO
open Expecto
open Helpers
open FsAutoComplete.LspHelpers
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete.Utils
open Helpers.Expecto.ShadowedTimeouts

let tests state =
  let testPath = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "HighlightingTest")
  let scriptPath = Path.Combine(testPath, "Script.fsx")
  let signaturePath = Path.Combine(testPath, "Signature.fsi")

  let server =
    async {
      let! (server, event) = serverInitialize testPath defaultConfigDto state

      for documentPath, fileName in [ scriptPath, "Script.fsx"; signaturePath, "Signature.fsi" ] do
        let tdop: DidOpenTextDocumentParams = { TextDocument = loadDocument documentPath }

        do! server.TextDocumentDidOpen tdop

        match! waitForParseResultsForFile fileName event with
        | Ok() -> ()
        | Error errors ->
          let errorStrings = errors |> Array.map (fun e -> string e) |> String.concat "\n\t* "

          return failtestf "Errors while parsing highlighting fixture %s:\n\t* %s" fileName errorStrings

      return server
    }
    |> Async.Cache

  let decodeHighlighting (data: uint32[]) =
    let zeroLine = [| 0u; 0u; 0u; 0u; 0u |]

    let lines = Array.append [| zeroLine |] (Array.chunkBySize 5 data)

    let structures =
      let mutable lastLine = 0
      let mutable lastCol = 0

      lines
      |> Array.map (fun current ->
        let startLine = lastLine + int current.[0]

        let startCol =
          if current.[0] = 0u then
            lastCol + int current.[1]
          else
            int current.[1]

        let endLine = int startLine // assuming no multiline for now
        let endCol = startCol + int current.[2]
        lastLine <- startLine
        lastCol <- startCol
        let tokenType = enum<ClassificationUtils.SemanticTokenTypes> (int current.[3])
        let tokenMods = enum<ClassificationUtils.SemanticTokenModifier> (int current.[4])

        let range =
          { Start =
              { Line = uint32 startLine
                Character = uint32 startCol }
            End =
              { Line = uint32 endLine
                Character = uint32 endCol } }

        range, tokenType, tokenMods)

    structures

  let getFullHighlights documentPath =
    async {
      let p: SemanticTokensParams =
        { TextDocument = { Uri = Path.FilePathToUri documentPath }
          WorkDoneToken = None
          PartialResultToken = None }

      let! server = server
      let! highlights = server.TextDocumentSemanticTokensFull p

      match highlights with
      | Ok(Some highlights) ->
        let decoded = highlights.Data |> decodeHighlighting
        // printfn "%A" decoded
        return decoded
      | Ok None -> return failtestf "Expected to get some highlighting"
      | Error e -> return failtestf "error of %A" e
    }

  let fullHighlights = getFullHighlights scriptPath |> Async.Cache

  let signatureHighlights = getFullHighlights signaturePath |> Async.Cache

  let rangeContainsRange (parent: Range) (child: Position) =
    parent.Start.Line <= child.Line
    && parent.Start.Character <= child.Character
    && parent.End.Line >= child.Line
    && parent.End.Character >= child.Character

  let tokenIsOfType
    ((line, char) as pos)
    testTokenType
    (highlights: (Range * ClassificationUtils.SemanticTokenTypes * ClassificationUtils.SemanticTokenModifier)[] Async)
    =
    testCaseAsync
      $"can find token of type {testTokenType} at %A{pos}"
      (async {
        let! highlights = highlights
        let pos = { Line = line; Character = char }

        Expect.exists
          highlights
          ((fun (r, token, _modifiers) -> rangeContainsRange r pos && token = testTokenType))
          $"Could not find a highlighting range that contained (%d{line},%d{char}) and type %A{testTokenType} in the token set %A{highlights}"
      })

  let tokenHasModifier
    ((line, char) as pos)
    testTokenType
    testModifier
    shouldHaveModifier
    (highlights: (Range * ClassificationUtils.SemanticTokenTypes * ClassificationUtils.SemanticTokenModifier)[] Async)
    =
    testCaseAsync
      $"token at %A{pos} has modifier %A{testModifier}: %b{shouldHaveModifier}"
      (async {
        let! highlights = highlights
        let pos = { Line = line; Character = char }

        let token =
          highlights
          |> Array.tryFind (fun (r, tokenType, _) -> rangeContainsRange r pos && tokenType = testTokenType)

        let _, _, modifiers =
          Expect.wantSome
            token
            $"Could not find a highlighting range that contained (%d{line},%d{char}) and type %A{testTokenType}"

        let hasModifier = (int modifiers &&& int testModifier) <> 0

        Expect.equal
          hasModifier
          shouldHaveModifier
          $"Expected modifier %A{testModifier} at (%d{line},%d{char}) to be %b{shouldHaveModifier}, but modifiers were %A{modifiers}"
      })

  /// this tests the range endpoint by getting highlighting for a range then doing the normal highlighting test
  let _tokenIsOfTypeInRange ((startLine, startChar), (endLine, endChar)) ((line, char)) testTokenType =
    testCaseAsync
      $"can find token of type {testTokenType} in a subrange from ({startLine}, {startChar})-({endLine}, {endChar})"
      (async {
        let! server = server

        let range: Range =
          { Start =
              { Line = startLine
                Character = startChar }
            End = { Line = endLine; Character = endChar } }

        let pos = { Line = line; Character = char }

        match!
          server.TextDocumentSemanticTokensRange
            { Range = range
              TextDocument = { Uri = Path.FilePathToUri scriptPath }
              WorkDoneToken = None
              PartialResultToken = None }
        with
        | Ok(Some highlights) ->
          let decoded = decodeHighlighting highlights.Data

          Expect.exists
            decoded
            (fun (r, token, _modifiers) -> rangeContainsRange r pos && token = testTokenType)
            "Could not find a highlighting range that contained the given position"
        | Ok None -> failtestf "Expected to get some highlighting"
        | Error e -> failtestf "error of %A" e
      })

  testList
    "Document Highlighting Tests"
    [ testList
        "tests"
        [ tokenIsOfType (0u, 29u) ClassificationUtils.SemanticTokenTypes.TypeParameter fullHighlights // the `^a` type parameter in the SRTP constraint
          tokenIsOfType (0u, 44u) ClassificationUtils.SemanticTokenTypes.Member fullHighlights // the `PeePee` member in the SRTP constraint
          tokenIsOfType (3u, 52u) ClassificationUtils.SemanticTokenTypes.Class fullHighlights // the `string` type annotation in the PooPoo srtp member
          tokenIsOfType (6u, 21u) ClassificationUtils.SemanticTokenTypes.EnumMember fullHighlights // the `PeePee` AP application in the `yeet` function definition
          tokenHasModifier
            (9u, 10u)
            ClassificationUtils.SemanticTokenTypes.Class
            ClassificationUtils.SemanticTokenModifier.Definition
            true
            fullHighlights // the `SomeJson` type alias definition should be marked as a definition
          tokenIsOfType (15u, 2u) ClassificationUtils.SemanticTokenTypes.Module fullHighlights // tests that module coloration isn't overwritten by function coloration when a module function is used, so Foo in Foo.x should be module-colored

          // Regression test for https://github.com/ionide/FsAutoComplete/issues/1407:
          // A file containing a multiline string literal must not produce any decoded token
          // whose end column is unreasonably far from its start column (which would indicate
          // a uint32 underflow from the old tokenLen = uint32(End.Character - Start.Character)
          // on a multiline range). If the fix is reverted, the decoded range for a multiline
          // string would have endCol = startCol + ~4294967290, and the editor would freeze.
          testCaseAsync
            "no uint32 underflow in decoded token ranges when file contains a multiline string"
            (async {
              let! highlights = fullHighlights

              let maxReasonableTokenLen = 1_000u

              for (r, _tokenType, _tokenMods) in highlights do
                let tokenLen = r.End.Character - r.Start.Character

                Expect.isLessThan
                  tokenLen
                  maxReasonableTokenLen
                  $"Token length {tokenLen} is unreasonably large (possible uint32 underflow from multiline range {r})"
            })

          // Regression test for https://github.com/ionide/FsAutoComplete/issues/1381:
          // The `null` keyword in nullable type annotations like `string | null` should
          // receive a Keyword semantic token (FCS does not provide one natively).
          tokenIsOfType (28u, 26u) ClassificationUtils.SemanticTokenTypes.Keyword fullHighlights // `null` in `let withNull (x: string | null) = ()`

          // Regression test for https://github.com/ionide/FsAutoComplete/issues/1359:
          // Function parameters should receive a Parameter semantic token, not a Variable token.
          tokenIsOfType (32u, 15u) ClassificationUtils.SemanticTokenTypes.Parameter fullHighlights // `param` in `let withParam (param: int) = param`

          // Function and member definitions should have the Definition modifier, while calls should not.
          tokenHasModifier
            (35u, 5u)
            ClassificationUtils.SemanticTokenTypes.Function
            ClassificationUtils.SemanticTokenModifier.Definition
            true
            fullHighlights // definition of `semanticDefinitionTarget`
          tokenHasModifier
            (36u, 28u)
            ClassificationUtils.SemanticTokenTypes.Function
            ClassificationUtils.SemanticTokenModifier.Definition
            false
            fullHighlights // call to `semanticDefinitionTarget`
          tokenHasModifier
            (39u, 13u)
            ClassificationUtils.SemanticTokenTypes.Method
            ClassificationUtils.SemanticTokenModifier.Definition
            true
            fullHighlights // definition of `SemanticMethodTarget`
          tokenHasModifier
            (41u, 55u)
            ClassificationUtils.SemanticTokenTypes.Method
            ClassificationUtils.SemanticTokenModifier.Definition
            false
            fullHighlights // call to `SemanticMethodTarget`
          tokenHasModifier
            (45u, 13u)
            ClassificationUtils.SemanticTokenTypes.Method
            ClassificationUtils.SemanticTokenModifier.Declaration
            true
            fullHighlights // declaration of `SemanticMethodDeclaration`
          tokenHasModifier
            (48u, 36u)
            ClassificationUtils.SemanticTokenTypes.Class
            ClassificationUtils.SemanticTokenModifier.Definition
            false
            fullHighlights // reference to `SomeJson`
          tokenHasModifier
            (50u, 8u)
            ClassificationUtils.SemanticTokenTypes.Class
            ClassificationUtils.SemanticTokenModifier.Definition
            false
            fullHighlights // type augmentation reference to `SemanticDefinitionType`

          // Signature-file values, members, and types should be declarations rather than definitions.
          tokenHasModifier
            (2u, 4u)
            ClassificationUtils.SemanticTokenTypes.Function
            ClassificationUtils.SemanticTokenModifier.Declaration
            true
            signatureHighlights // declaration of `semanticFunctionDeclaration`
          tokenHasModifier
            (5u, 5u)
            ClassificationUtils.SemanticTokenTypes.Class
            ClassificationUtils.SemanticTokenModifier.Declaration
            true
            signatureHighlights // declaration of `SemanticTypeDeclaration`
          tokenHasModifier
            (6u, 11u)
            ClassificationUtils.SemanticTokenTypes.Method
            ClassificationUtils.SemanticTokenModifier.Declaration
            true
            signatureHighlights // declaration of `SemanticMethodDeclaration`
          tokenHasModifier
            (2u, 4u)
            ClassificationUtils.SemanticTokenTypes.Function
            ClassificationUtils.SemanticTokenModifier.Definition
            false
            signatureHighlights // a signature value declaration is not a definition
          tokenHasModifier
            (5u, 5u)
            ClassificationUtils.SemanticTokenTypes.Class
            ClassificationUtils.SemanticTokenModifier.Definition
            false
            signatureHighlights ] ] // a signature type declaration is not a definition
