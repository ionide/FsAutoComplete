module private FsAutoComplete.Tests.CodeFixTests.HelpersTests
// `src\FsAutoComplete\CodeFixes.fs` -> `FsAutoComplete.CodeFix`

open Expecto
open FsAutoComplete.CodeFix
open Navigation
open FSharp.Compiler.Text
open Utils.TextEdit
open Ionide.LanguageServerProtocol.Types

let private navigationTests =
  testList (nameof Navigation) [
    let extractTwoCursors text =
      let (text, poss) = Cursors.extract text
      let text = SourceText.ofString text
      (text, (poss[0], poss[1]))

    testList (nameof tryEndOfPrevLine) [
      testCase "can get end of prev line when not border line" <| fun _ ->
        let text = """let foo = 4
let bar = 5
let baz = 5$0
let $0x = 5
let y = 7
let z = 4"""
        let (text, (expected, current)) = text |> extractTwoCursors
        let actual = tryEndOfPrevLine text current.Line
        Expect.equal actual (Some expected) "Incorrect pos"

      testCase "can get end of prev line when last line" <| fun _ ->
        let text = """let foo = 4
let bar = 5
let baz = 5
let x = 5
let y = 7$0
let z$0 = 4"""
        let (text, (expected, current)) = text |> extractTwoCursors
        let actual = tryEndOfPrevLine text current.Line
        Expect.equal actual (Some expected) "Incorrect pos"

      testCase "cannot get end of prev line when first line" <| fun _ ->
        let text = """let $0foo$0 = 4
let bar = 5
let baz = 5
let x = 5
let y = 7
let z = 4"""
        let (text, (_, current)) = text |> extractTwoCursors
        let actual = tryEndOfPrevLine text current.Line
        Expect.isNone actual "No prev line in first line"

      testCase "cannot get end of prev line when single line" <| fun _ ->
        let text = SourceText.ofString "let foo = 4"
        let line = 0
        let actual = tryEndOfPrevLine text line
        Expect.isNone actual "No prev line in first line"
    ]
    testList (nameof tryStartOfNextLine) [
      // this would be WAY easier by just using `{ Line = current.Line + 1; Character = 0 }`...
      testCase "can get start of next line when not border line" <| fun _ ->
        let text = """let foo = 4
let bar = 5
let baz = 5
let $0x = 5
$0let y = 7
let z = 4"""
        let (text, (current, expected)) = text |> extractTwoCursors
        let actual = tryStartOfNextLine text current.Line
        Expect.equal actual (Some expected) "Incorrect pos"

      testCase "can get start of next line when first line" <| fun _ ->
        let text = """let $0foo = 4
$0let bar = 5
let baz = 5
let x = 5
let y = 7
let z = 4"""
        let (text, (current, expected)) = text |> extractTwoCursors
        let actual = tryStartOfNextLine text current.Line
        Expect.equal actual (Some expected) "Incorrect pos"

      testCase "cannot get start of next line when last line" <| fun _ ->
        let text = """let foo = 4
let bar = 5
let baz = 5
let x = 5
let y = 7
let $0z$0 = 4"""
        let (text, (current, _)) = text |> extractTwoCursors
        let actual = tryStartOfNextLine text current.Line
        Expect.isNone actual "No next line in last line"

      testCase "cannot get start of next line when single line" <| fun _ ->
        let text = SourceText.ofString "let foo = 4"
        let line = 0
        let actual = tryStartOfNextLine text line
        Expect.isNone actual "No next line in first line"
    ]
    testList (nameof rangeToDeleteFullLine) [
      testCase "can get all range for single line" <| fun _ ->
        let text = "$0let foo = 4$0"
        let (text, (start, fin)) = text |> extractTwoCursors
        let expected = { Start = start; End = fin }

        let line = fin.Line
        let actual = text |> rangeToDeleteFullLine line
        Expect.equal actual expected "Incorrect range"

      testCase "can get line range with leading linebreak in not border line" <| fun _ ->
        let text = """let foo = 4
let bar = 5
let baz = 5$0
let x = 5$0
let y = 7
let z = 4"""
        let (text, (start, fin)) = text |> extractTwoCursors
        let expected = { Start = start; End = fin }

        let line = fin.Line
        let actual = text |> rangeToDeleteFullLine line
        Expect.equal actual expected "Incorrect range"

      testCase "can get line range with leading linebreak in last line" <| fun _ ->
        let text = """let foo = 4
let bar = 5
let baz = 5
let x = 5
let y = 7$0
let z = 4$0"""
        let (text, (start, fin)) = text |> extractTwoCursors
        let expected = { Start = start; End = fin }

        let line = fin.Line
        let actual = text |> rangeToDeleteFullLine line
        Expect.equal actual expected "Incorrect range"

      testCase "can get line range with trailing linebreak in first line" <| fun _ ->
        let text = """$0let foo = 4
$0let bar = 5
let baz = 5
let x = 5
let y = 7
let z = 4"""
        let (text, (start, fin)) = text |> extractTwoCursors
        let expected = { Start = start; End = fin }

        let line = start.Line
        let actual = text |> rangeToDeleteFullLine line
        Expect.equal actual expected "Incorrect range"
        
      testCase "can get all range for single empty line" <| fun _ ->
        let text = SourceText.ofString ""
        let pos = { Line = 0; Character = 0 }
        let expected = { Start = pos; End = pos }

        let line = pos.Line
        let actual = text |> rangeToDeleteFullLine line
        Expect.equal actual expected "Incorrect range"
    ]
  ]

let tests = testList ($"{nameof(FsAutoComplete)}.{nameof(FsAutoComplete.CodeFix)}") [
  navigationTests
]
