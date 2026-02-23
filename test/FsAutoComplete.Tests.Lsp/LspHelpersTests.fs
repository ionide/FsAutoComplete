module FsAutoComplete.Tests.LspHelpersTests

open Expecto
open FsAutoComplete.LspHelpers
open Ionide.LanguageServerProtocol.Types

/// Helpers to construct test data for encodeSemanticHighlightRanges
module private Helpers =

  let makeRange startLine startChar endLine endChar : Range =
    { Start =
        { Line = uint32 startLine
          Character = uint32 startChar }
      End =
        { Line = uint32 endLine
          Character = uint32 endChar } }

  let makeEntry
    (range: Range)
    ty
    : struct (Range * ClassificationUtils.SemanticTokenTypes * ClassificationUtils.SemanticTokenModifier list) =
    struct (range, ty, [])

/// Unit tests for encodeSemanticHighlightRanges, focused on the uint32 underflow
/// regression described in https://github.com/ionide/FsAutoComplete/issues/1407.
///
/// The LSP semantic-token encoding stores each token as five uint32 values:
///   [lineDelta, charDelta, tokenLen, tokenType, tokenModifiers]
/// Before the fix, a multiline range (End.Character < Start.Character) would
/// cause tokenLen to underflow (e.g. uint32(4 - 10) = 4294967290u), which
/// could freeze or crash editors such as Neovim.
let encodeSemanticHighlightRangesTests =
  testList
    "encodeSemanticHighlightRanges"
    [ testCase "empty input returns None"
      <| fun _ ->
        let result = encodeSemanticHighlightRanges [||]
        Expect.isNone result "Empty input should return None"

      testCase "single normal single-line range is encoded with correct length"
      <| fun _ ->
        // Range on line 3, columns 5–10 → length 5
        let range = Helpers.makeRange 3 5 3 10
        let entry = Helpers.makeEntry range ClassificationUtils.SemanticTokenTypes.Variable

        match encodeSemanticHighlightRanges [| entry |] with
        | None -> failtest "Expected Some but got None"
        | Some data ->
          Expect.equal data.Length 5 "Single token should produce 5 uint32 values"
          // lineDelta relative to file start (line 0) = 3
          Expect.equal data.[0] 3u "lineDelta should be 3"
          // charDelta = absolute column when line changes = 5
          Expect.equal data.[1] 5u "charDelta should be 5"
          // tokenLen = 10 - 5 = 5
          Expect.equal data.[2] 5u "tokenLen should be 5"

      testCase "multiline range (End.Line > Start.Line) produces tokenLen = 0, not a uint32 underflow"
      <| fun _ ->
        // Simulates a multiline string: starts at col 10 on line 2, ends at col 4 on line 3.
        // Before the fix: uint32(4u - 10u) would underflow to 4294967290u (~4 GB).
        let range = Helpers.makeRange 2 10 3 4
        let entry = Helpers.makeEntry range ClassificationUtils.SemanticTokenTypes.String

        match encodeSemanticHighlightRanges [| entry |] with
        | None -> failtest "Expected Some but got None"
        | Some data ->
          Expect.equal data.Length 5 "Single token should produce 5 uint32 values"
          Expect.equal data.[2] 0u "Multiline range must clamp tokenLen to 0, not underflow"
          // Belt-and-suspenders: confirm it is not the old underflowed value
          Expect.isLessThan data.[2] 1_000_000u "tokenLen must not be a huge underflowed value"

      testCase "single-line inverted range (End.Character < Start.Character) produces tokenLen = 0"
      <| fun _ ->
        // Degenerate range on the same line where end column < start column.
        let range = Helpers.makeRange 5 15 5 3
        let entry = Helpers.makeEntry range ClassificationUtils.SemanticTokenTypes.Variable

        match encodeSemanticHighlightRanges [| entry |] with
        | None -> failtest "Expected Some but got None"
        | Some data -> Expect.equal data.[2] 0u "Inverted single-line range must clamp tokenLen to 0"

      testCase "multiline range followed by valid single-line range: subsequent token is encoded correctly"
      <| fun _ ->
        // A multiline token (lines 2–3) should not corrupt the delta encoding of the
        // token that follows it on line 5.
        let multilineRange = Helpers.makeRange 2 10 3 4
        // Valid token: line 5, col 2–8, length 6
        let validRange = Helpers.makeRange 5 2 5 8

        let entries =
          [| Helpers.makeEntry multilineRange ClassificationUtils.SemanticTokenTypes.String
             Helpers.makeEntry validRange ClassificationUtils.SemanticTokenTypes.Variable |]

        match encodeSemanticHighlightRanges entries with
        | None -> failtest "Expected Some but got None"
        | Some data ->
          Expect.equal data.Length 10 "Two tokens should produce 10 uint32 values"

          // First token (multiline): tokenLen must be 0
          Expect.equal data.[2] 0u "Multiline token must have tokenLen = 0"

          // Second token (valid, line 5): lineDelta from line 2 = 3, char = 2 (absolute, new line), len = 6
          Expect.equal data.[5] 3u "lineDelta of second token should be 5 - 2 = 3"
          Expect.equal data.[6] 2u "charDelta of second token should be 2 (absolute on new line)"
          Expect.equal data.[7] 6u "tokenLen of second token should be 6"

      testCase "sequence of valid single-line tokens uses correct relative deltas"
      <| fun _ ->
        // Three tokens on different lines; verifies standard delta encoding is unaffected.
        // Token A: line 1, col 0–4 (len 4)
        // Token B: line 1, col 8–12 (len 4, same line → charDelta = 8-0 = 8)
        // Token C: line 3, col 2–7 (len 5, new line → charDelta = 2 absolute)
        let rangeA = Helpers.makeRange 1 0 1 4
        let rangeB = Helpers.makeRange 1 8 1 12
        let rangeC = Helpers.makeRange 3 2 3 7

        let entries =
          [| Helpers.makeEntry rangeA ClassificationUtils.SemanticTokenTypes.Variable
             Helpers.makeEntry rangeB ClassificationUtils.SemanticTokenTypes.Function
             Helpers.makeEntry rangeC ClassificationUtils.SemanticTokenTypes.Class |]

        match encodeSemanticHighlightRanges entries with
        | None -> failtest "Expected Some but got None"
        | Some data ->
          Expect.equal data.Length 15 "Three tokens should produce 15 uint32 values"

          // Token A: lineDelta=1 (from line 0), charDelta=0 (absolute on new line), len=4
          Expect.equal data.[0] 1u "Token A lineDelta = 1"
          Expect.equal data.[1] 0u "Token A charDelta = 0"
          Expect.equal data.[2] 4u "Token A tokenLen = 4"

          // Token B: same line as A → lineDelta=0, charDelta = 8-0 = 8, len=4
          Expect.equal data.[5] 0u "Token B lineDelta = 0 (same line)"
          Expect.equal data.[6] 8u "Token B charDelta = 8 (relative to prev col 0)"
          Expect.equal data.[7] 4u "Token B tokenLen = 4"

          // Token C: new line → lineDelta = 3-1 = 2, charDelta = 2 (absolute), len=5
          Expect.equal data.[10] 2u "Token C lineDelta = 2"
          Expect.equal data.[11] 2u "Token C charDelta = 2 (absolute on new line)"
          Expect.equal data.[12] 5u "Token C tokenLen = 5" ]

let allTests = testList "LspHelpers" [ encodeSemanticHighlightRangesTests ]
