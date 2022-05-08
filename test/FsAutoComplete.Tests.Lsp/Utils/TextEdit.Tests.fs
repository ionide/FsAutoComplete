module Utils.Tests.TextEdit
open Expecto
open Expecto.Logging
open Expecto.Logging.Message
open FsToolkit.ErrorHandling
open Ionide.LanguageServerProtocol.Types
open Utils.TextEdit
open Utils.Utils

let private logger = Expecto.Logging.Log.create (sprintf "%s.%s" (nameof Utils.Tests) (nameof Utils.TextEdit))
let inline private pos line column: Position = { Line = line; Character = column }
let inline private (!-) text = Text.trimTripleQuotation text

module private Cursor =
  open Expecto.Flip

  let private tryExtractIndexTests = testList (nameof Cursor.tryExtractIndex) [
    testList "no cursor" [
      let assertNoCursor =
        Cursor.tryExtractPosition
        >> Expect.isNone "should have found no cursor"
      testCase "empty string" <| fun _ ->
        let text = ""
        assertNoCursor text
      testCase "single line" <| fun _ ->
        let text = "Foo Bar Baz"
        assertNoCursor text
      testCase "two lines" <| fun _ ->
        let text = "Foo Bar Baz\nLorem ipsum dolor sit"
        assertNoCursor text
      testCase "multiple lines" <| fun _ ->
        let text = "Foo\nBar\nBaz\nLorem\nimpsum\ndolor\nsit"
        assertNoCursor text
      testCase "just spaces" <| fun _ ->
        let text = "       "
        assertNoCursor text
      testCase "just spaces and new lines" <| fun _ ->
        let text = "       \n   \n\n  \n\n\n\n   \n  \n  \n"
        assertNoCursor text
      testCase "triple quoted string without processing" <| fun _ ->
        let text = """
module Foo

let a = 42
let b = 
  a + 5
printfn "Result=%i" b
        """
        assertNoCursor text
      testCase "triple quoted string with processing (starting new line, no indentation)" <| fun _ ->
        let text = !- """
module Foo

let a = 42
let b = 
  a + 5
printfn "Result=%i" b
        """
        assertNoCursor text
      testCase "triple quoted string with processing (starting new line, indentation)" <| fun _ ->
        let text = !- """
  module Foo

  let a = 42
  let b = 
    a + 5
  printfn "Result=%i" b
        """
        assertNoCursor text
    ]
    testList "with cursor" [
      let assertAndGetIndex =
        Text.trimTripleQuotation
        >> Cursor.tryExtractIndex
        >> Option.defaultWith (fun _ -> failtest "No cursor found")
      let assertResultIs (idx: int, text: string) =
        assertAndGetIndex
        >> Expect.equal "should be correct cursor position and text" (idx, text)
      let assertCursorAt (idx: int) =
        assertAndGetIndex
        >> fst
        >> Expect.equal "should have found cursor at correct position" idx
      let assertTextIs (text: string) =
        assertAndGetIndex
        >> snd
        >> Expect.equal "should have correct text" text

      testList "in normal string" [
        testCase "in empty string" <| fun _ ->
          let text = "$0"
          let expected = 0
          text |> assertCursorAt expected
        testCase "start of single line" <| fun _ ->
          let text = "$0Foo bar baz"
          let expected = 0
          text |> assertCursorAt expected
        testCase "end of single word" <| fun _ ->
          let text = "foo$0"
          // Note: out of string range: cursor is AFTER last character
          let expected = 3
          text |> assertCursorAt expected
        testCase "end of single line" <| fun _ ->
          let text = "foo bar baz$0"
          let expected = 11
          text |> assertCursorAt expected
        testCase "removes cursor marker from single line" <| fun _ ->
          let text = "foo $0bar"
          let expected = "foo bar"
          text |> assertTextIs expected
      ]
      testList "in triple quoted string" [
        testCase "in empty string unindented" <| fun _ ->
          // technically incorrect: contains `\n`
          let text = """
$0
          """
          let expected = 0
          text |> assertCursorAt expected
        testCase "in empty string indented" <| fun _ ->
          // technically incorrect: contains `\n`
          let text = """
            $0
          """
          let expected = 0
          text |> assertCursorAt expected
        testCase "in F# code unindented" <| fun _ ->
          let text = """
module Foo

let $0a = 42
let b = 
  a + 5
printfn "Result=%i" b
          """
          let expected = 16
          text |> assertCursorAt expected
        testCase "in F# code indented" <| fun _ ->
          let text = """
            module Foo

            let $0a = 42
            let b = 
              a + 5
            printfn "Result=%i" b
          """
          let expected = 16
          text |> assertCursorAt expected
        testCase "removes cursor in F# code unindented" <| fun _ ->
          let text = """
module Foo

let $0a = 42
let b = 
  a + 5
printfn "Result=%i" b
          """
          // expected isn't trimmed in assertXXX -> do manually
          let expected = !- """
module Foo

let a = 42
let b = 
  a + 5
printfn "Result=%i" b
          """
          text |> assertTextIs expected
        testCase "removes cursor in F# code indented" <| fun _ ->
          let text = """
            module Foo

            let $0a = 42
            let b = 
              a + 5
            printfn "Result=%i" b
          """
          let expected = !- """
module Foo

let a = 42
let b = 
  a + 5
printfn "Result=%i" b
          """
          text |> assertTextIs expected
        testCase "finds and removes only first cursor" <| fun _ ->
          let text = """
            module Foo

            let $0a = 42
            let $0b = 
              a + 5
            printfn "Result=%i" b
          """
          let expected = !- """
module Foo

let a = 42
let $0b = 
  a + 5
printfn "Result=%i" b
          """
          text |> assertTextIs expected
      ]
    ]
  ]

  let private tryExtractPositionMarkedWithAnyOfTests = testList (nameof Cursor.tryExtractPositionMarkedWithAnyOf) [
    testCase "exact first of many cursors" <| fun _ ->
      let text = "let $Avalue$B = $C42"
      let actual = 
        text
        |> Cursor.tryExtractPositionMarkedWithAnyOf [|"$B"; "$C"; "$A"|]
      let expected = Some (("$A", pos 0 4), "let value$B = $C42")

      actual
      |> Expect.equal "should be correct marker" expected
  ]
  
  let private tryExtractPositionTests = testList (nameof Cursor.tryExtractPosition) [
    testList "no cursor" [
      let assertNoCursor =
        Cursor.tryExtractPosition
        >> Expect.isNone "should have found no cursor"
      testCase "empty string" <| fun _ ->
        let text = ""
        assertNoCursor text
      testCase "single line" <| fun _ ->
        let text = "Foo Bar Baz"
        assertNoCursor text
      testCase "two lines" <| fun _ ->
        let text = "Foo Bar Baz\nLorem ipsum dolor sit"
        assertNoCursor text
      testCase "multiple lines" <| fun _ ->
        let text = "Foo\nBar\nBaz\nLorem\nimpsum\ndolor\nsit"
        assertNoCursor text
      testCase "just spaces" <| fun _ ->
        let text = "       "
        assertNoCursor text
      testCase "just spaces and new lines" <| fun _ ->
        let text = "       \n   \n\n  \n\n\n\n   \n  \n  \n"
        assertNoCursor text
      testCase "triple quoted string without processing" <| fun _ ->
        let text = """
module Foo

let a = 42
let b = 
  a + 5
printfn "Result=%i" b
        """
        assertNoCursor text
      testCase "triple quoted string with processing (starting new line, no indentation)" <| fun _ ->
        let text = !- """
module Foo

let a = 42
let b = 
  a + 5
printfn "Result=%i" b
        """
        assertNoCursor text
      testCase "triple quoted string with processing (starting new line, indentation)" <| fun _ ->
        let text = !- """
  module Foo

  let a = 42
  let b = 
    a + 5
  printfn "Result=%i" b
        """
        assertNoCursor text
    ]

    testList "with cursor" [
      let assertAndGetCursor =
        Text.trimTripleQuotation
        >> Cursor.tryExtractPosition
        >> Option.defaultWith (fun _ -> failtest "No cursor found")
      let assertCursorAt (pos: Position) =
        assertAndGetCursor
        >> fst
        >> Expect.equal "should have found cursor at correct position" pos
      let assertTextIs (text: string) =
        assertAndGetCursor
        >> snd
        >> Expect.equal "should have correct text" text
      let assertResultIs (pos: Position, text: string) =
        assertAndGetCursor
        >> Expect.equal "should be correct cursor position and text" (pos, text)

      testList "in normal string" [
        testCase "in empty string" <| fun _ ->
          let text = "$0"
          let expected = pos 0 0
          text |> assertCursorAt expected
        testCase "start of single line" <| fun _ ->
          let text = "$0Foo bar baz"
          let expected = pos 0 0
          text |> assertCursorAt expected
        testCase "end of single word" <| fun _ ->
          let text = "foo$0"
          // Note: out of string range: cursor is AFTER last character
          let expected = pos 0 3
          text |> assertCursorAt expected
        testCase "end of single line" <| fun _ ->
          let text = "foo bar baz$0"
          let expected = pos 0 11
          text |> assertCursorAt expected
        testCase "removes cursor marker from single line" <| fun _ ->
          let text = "foo $0bar"
          let expected = "foo bar"
          text |> assertTextIs expected
      ]
      testList "in triple quoted string" [
        testCase "in empty string unindented" <| fun _ ->
          // technically incorrect: contains `\n`
          let text = """
$0
          """
          let expected = pos 0 0
          text |> assertCursorAt expected
        testCase "in empty string indented" <| fun _ ->
          // technically incorrect: contains `\n`
          let text = """
            $0
          """
          let expected = pos 0 0
          text |> assertCursorAt expected
        testCase "in F# code unindented" <| fun _ ->
          let text = """
module Foo

let $0a = 42
let b = 
  a + 5
printfn "Result=%i" b
          """
          let expected = pos 2 4  // 0-based, first line (with `"""`) is removed
          text |> assertCursorAt expected
        testCase "in F# code indented" <| fun _ ->
          let text = """
            module Foo

            let $0a = 42
            let b = 
              a + 5
            printfn "Result=%i" b
          """
          let expected = pos 2 4  // 0-based, first line (with `"""`) is removed, leading indentation removed
          text |> assertCursorAt expected
        testCase "removes cursor in F# code unindented" <| fun _ ->
          let text = """
module Foo

let $0a = 42
let b = 
  a + 5
printfn "Result=%i" b
          """
          // expected isn't trimmed in assertXXX -> do manually
          let expected = !- """
module Foo

let a = 42
let b = 
  a + 5
printfn "Result=%i" b
          """
          text |> assertTextIs expected
        testCase "removes cursor in F# code indented" <| fun _ ->
          let text = """
            module Foo

            let $0a = 42
            let b = 
              a + 5
            printfn "Result=%i" b
          """
          let expected = !- """
module Foo

let a = 42
let b = 
  a + 5
printfn "Result=%i" b
          """
          text |> assertTextIs expected
        testCase "finds and removes only first cursor" <| fun _ ->
          let text = """
            module Foo

            let $0a = 42
            let $0b = 
              a + 5
            printfn "Result=%i" b
          """
          let expected = !- """
module Foo

let a = 42
let $0b = 
  a + 5
printfn "Result=%i" b
          """
          text |> assertTextIs expected
      ]
    ]
  ]

  let tryExtractRangeTests = testList (nameof Cursor.tryExtractRange) [
    let assertAndGetRange =
      Text.trimTripleQuotation
      >> Cursor.tryExtractRange
      >> Option.defaultWith (fun _ -> failtest "No cursor found")
    let assertRangeIs (range: Range) =
      assertAndGetRange
      >> fst
      >> Expect.equal "should have found correct range" range
    let assertTextIs (text: string) =
      assertAndGetRange
      >> snd
      >> Expect.equal "should have correct text" text
    let assertResultIs (range: Range, text: string) =
      assertAndGetRange
      >> Expect.equal "should be correct range and text" (range, text)
    testCase "no cursor results in no range" <| fun _ ->
      let text = !- """
module Foo

let a = 42
let b = 
  a + 5
printfn "Result=%i" b
      """
      text
      |> Cursor.tryExtractRange
      |> Expect.isNone "should have found no cursor"
    testCase "can extract range in same line" <| fun _ ->
      let text = """
module Foo

let $0a =$0 42
let b = 
  a + 5
printfn "Result=%i" b
      """
      let expected = { Start = pos 2 4; End = pos 2 7 }
      text |> assertRangeIs expected
    testCase "can extract range over multiple lines" <| fun _ ->
      let text = """
module Foo

let $0a = 42
let b = 
  a + 5
printfn "$0Result=%i" b
      """
      let expected = { Start = pos 2 4; End = pos 5 9 }
      text |> assertRangeIs expected
    testCase "can extract position" <| fun _ ->
      let text = """
module Foo

let a =$0 42
let b = 
  a + 5
printfn "Result=%i" b
      """
      let expected = { Start = pos 2 7; End = pos 2 7 }
      text |> assertRangeIs expected
    testCase "removes cursor markers from line" <| fun _ ->
      let text = """
module Foo

let $0a = 42
let b = 
  a + 5
printfn "$0Result=%i" b
      """
      let expected = !- """
module Foo

let a = 42
let b = 
  a + 5
printfn "Result=%i" b
      """
      text |> assertTextIs expected
    testCase "finds and removes only first range and its two markers" <| fun _ ->
      let text = """
module $0Foo

let a = $042
let b = 
  a + $05
printfn "$0Result$0=%i$0" b$0
      """
      let expectedRange = { Start = pos 0 7; End = pos 2 8 }
      let expectedText = !- """
module Foo

let a = 42
let b = 
  a + $05
printfn "$0Result$0=%i$0" b$0
      """
      text |> assertResultIs (expectedRange, expectedText)
  ]

  let beforeIndexTests = testList (nameof Cursor.beforeIndex) [
    let assertBeforeIndex expected textWithCursor =
      let textWithCursor = Text.trimTripleQuotation textWithCursor
      let idx = textWithCursor.IndexOf Cursor.Marker
      Expect.isGreaterThanOrEqual "Text has no cursor" (idx, 0)
      let text = textWithCursor.Remove(idx, Cursor.Marker.Length)

      text 
      |> Cursor.beforeIndex idx
      |> Expect.equal "Should be correct position" expected

      
    testList "single line" [
      testCase "empty string" <| fun _ ->
        let text = ""
        let idx = 0
        let expected = pos 0 0
        
        text
        |> Cursor.beforeIndex idx
        |> Expect.equal "Position should be at start of string" expected
  
      testCase "empty string with cursor" <| fun _ ->
        let text = "$0"
        let expected = pos 0 0
        assertBeforeIndex expected text

      testCase "single line string - start" <| fun _ ->
        let text = "$0let foo = 42"
        let expected = pos 0 0
        assertBeforeIndex expected text
      testCase "single line string - middle" <| fun _ ->
        let text = "let foo $0= 42"
        let expected = pos 0 8
        assertBeforeIndex expected text
      testCase "single line string - end" <| fun _ ->
        let text = "let foo = 42$0"
        let expected = pos 0 12
        assertBeforeIndex expected text
    ]
    testList "multi line" [
      testCase "start of first line" <| fun _ ->
        let text = """
$0module Foo

let a = 42
let b =
  a + 5
printfn "Result=%i" b
        """
        let expected = pos 0 0
        assertBeforeIndex expected text
      testCase "middle of first line" <| fun _ ->
        let text = """
module $0Foo

let a = 42
let b =
  a + 5
printfn "Result=%i" b
        """
        let expected = pos 0 7
        assertBeforeIndex expected text
      testCase "end of first line" <| fun _ ->
        let text = """
module Foo$0

let a = 42
let b =
  a + 5
printfn "Result=%i" b
        """
        let expected = pos 0 10
        assertBeforeIndex expected text
      testCase "start of 4th line" <| fun _ ->
        let text = """
module Foo

let a = 42
$0let b =
  a + 5
printfn "Result=%i" b
        """
        let expected = pos 3 0
        assertBeforeIndex expected text
      testCase "middle of 4th line" <| fun _ ->
        let text = """
module Foo

let a = 42
let $0b =
  a + 5
printfn "Result=%i" b
        """
        let expected = pos 3 4
        assertBeforeIndex expected text
      testCase "end of 4th line" <| fun _ ->
        let text = """
module Foo

let a = 42
let b =$0
  a + 5
printfn "Result=%i" b
        """
        let expected = pos 3 7
        assertBeforeIndex expected text
      testCase "start of last line" <| fun _ ->
        let text = """
module Foo

let a = 42
let b =
  a + 5
$0printfn "Result=%i" b"""
        let expected = pos 5 0
        assertBeforeIndex expected text
      testCase "middle of last line" <| fun _ ->
        let text = """
module Foo

let a = 42
let b =
  a + 5
printfn "$0Result=%i" b"""
        let expected = pos 5 9
        assertBeforeIndex expected text
      testCase "end of last line" <| fun _ ->
        let text = """
module Foo

let a = 42
let b =
  a + 5
printfn "Result=%i" b$0"""
        let expected = pos 5 21
        assertBeforeIndex expected text
    ]
  ]

  let tryIndexOfTests = testList (nameof Cursor.tryIndexOf) [
    let assertAndGetTextAndCursor =
      Text.trimTripleQuotation
      >> Cursor.tryExtractPosition
      >> Option.defaultWith (fun _ -> failtest "should have found cursor")
    let indexOf =
      assertAndGetTextAndCursor
      >> fun (pos, text) -> Cursor.tryIndexOf pos text
    let assertAndGetIndexOf =
      indexOf
      >> function
         | Ok i -> i
         | Result.Error msg -> failtest $"should have found index. But was error: {msg}"
    let assertIndexOf expectedIndex =
      assertAndGetIndexOf
      >> Expect.equal "wrong index" expectedIndex
    let assertIndexOf expectedIndex textWithCursor =
      let (pos, text) = assertAndGetTextAndCursor textWithCursor
      match Cursor.tryIndexOf pos text with
      | Ok actualIndex -> 
        let idxInText = textWithCursor.IndexOf Cursor.Marker
        let errorMsg = $"wrong index. Cursor at Postion={{Line={pos.Line};Char={pos.Character}}} or Index={idxInText}"
        Expect.equal errorMsg expectedIndex actualIndex
      | Result.Error msg -> failtest $"should have found index. But was error: {msg}"
    let assertNoIndexAt pos =
      Text.trimTripleQuotation
      >> Cursor.tryIndexOf pos
      >> function
         | Ok i -> failtest $"Expected Error, but was OK with index {i}"
         | Result.Error _ -> ()

    testList "empty string" [
      testCase "inside" <| fun _ ->
        let text = "$0"
        let expected = 0
        text |> assertIndexOf expected
      testCase "out of char range in empty string" <| fun _ ->
        let text = ""
        let pos = pos 0 1
        text |> assertNoIndexAt pos
      testCase "out of line range in empty string" <| fun _ ->
        let text = ""
        let pos = pos 1 0
        text |> assertNoIndexAt pos
    ]

    testList "single line" [
      testCase "out of char range" <| fun _ ->
        let text = "foo bar baz"
        let pos = pos 0 (11 + 1)
        text |> assertNoIndexAt pos
      testCase "out of line range" <| fun _ ->
        let text = "foo bar baz"
        let pos = pos 1 0
        text |> assertNoIndexAt pos
      testCase "start" <| fun _ ->
        let text = "$0foo bar baz"
        let expected = 0
        text |> assertIndexOf expected
      testCase "middle" <| fun _ ->
        let text = "foo b$0ar baz"
        let expected = 5
        text |> assertIndexOf expected
      testCase "end" <| fun _ ->
        let text = "foo bar baz$0"
        let expected = 11
        text |> assertIndexOf expected
    ]

    testList "two lines" [
      testCase "start of 1st line" <| fun _ ->
        // chars: 11 + `\n` + 17
        let text = "$0foo bar baz\nlorem ipsum dolor"
        let expected = 0
        text |> assertIndexOf expected
      testCase "middle of 1st line" <| fun _ ->
        let text = "foo b$0ar baz\nlorem ipsum dolor"
        let expected = 5
        text |> assertIndexOf expected
      testCase "end of 1st line" <| fun _ ->
        let text = "foo bar baz$0\nlorem ipsum dolor"
        let expected = 10 (*1st line; 0-based*) + 1 (*\n*) // on `\n`; 10: Index is 0-based: string with length=11 -> max index = 10
        text |> assertIndexOf expected
      testCase "start of 2nd line" <| fun _ ->
        let text = "foo bar baz\n$0lorem ipsum dolor"
        let expected = 10 (*1st line; 0-based*) + 1 (*\n*) + 0 (*2nd line*) + 1 (*index after cursor*)
        text |> assertIndexOf expected
      testCase "middle of 2nd line" <| fun _ ->
        let text = "foo bar baz\nlorem ip$0sum dolor"
        let expected = 10 (*1st line; 0-based*) + 1 (*\n*) + 8 (*2nd line*) + 1 (*index after cursor*)
        text |> assertIndexOf expected
      testCase "end of 2nd line" <| fun _ ->
        let text = "foo bar baz\nlorem ipsum dolor$0"
        let expected = 10 (*1st line; 0-based*) + 1 (*\n*) + 17 (*2nd line*) + 1 (*index afrer cursor*)
        text |> assertIndexOf expected
      testCase "out of char range in 1st line" <| fun _ ->
        let text = "foo bar baz\nlorem ipsum dolor"
        let pos = pos 0 (11 + 1)
        text |> assertNoIndexAt pos
      testCase "out of char range in 2nd line" <| fun _ ->
        let text = "foo bar baz\nlorem ipsum dolor"
        let pos = pos 1 (17 + 1)
        text |> assertNoIndexAt pos
      testCase "out of line range" <| fun _ ->
        let text = "foo bar baz\nlorem ipsum dolor"
        let pos = pos 2 0
        text |> assertNoIndexAt pos
    ]
    
    testList "F# code" [
      testCase "start of text" <| fun _ -> 
        let text = """
$0module Foo

let a = 42
let b = 
  a + 5
printfn "Result=%i" b
        """
        text |> assertIndexOf 0
      testCase "end of text" <| fun _ -> 
        let text = """
module Foo

let a = 42
let b = 
  a + 5
printfn "Result=%i" b
$0"""
        text |> assertIndexOf 62
      testCase "middle of 1st line" <| fun _ -> 
        let text = """
module$0 Foo

let a = 42
let b = 
  a + 5
printfn "Result=%i" b
        """
        text |> assertIndexOf 6
      testCase "end of 1st line" <| fun _ -> 
        let text = """
module Foo$0

let a = 42
let b = 
  a + 5
printfn "Result=%i" b
        """
        text |> assertIndexOf 10
      testCase "start of 4th line" <| fun _ -> 
        let text = """
module Foo

let a = 42
$0let b = 
  a + 5
printfn "Result=%i" b
        """
        text |> assertIndexOf 23
      testCase "middle of 4th line" <| fun _ -> 
        let text = """
module Foo

let a = 42
let $0b = 
  a + 5
printfn "Result=%i" b
        """
        text |> assertIndexOf 27
      testCase "end of 4th line" <| fun _ -> 
        let text = """
module Foo

let a = 42
let b = $0
  a + 5
printfn "Result=%i" b
        """
        text |> assertIndexOf 31
    ]
  ]

  let identityTests = testList "identities" [
    // * idx |> beforeIndex |> tryIndexOf = idx
    // * pos |> tryIndexOf |> beforeIndex = pos
    // * tryExtractIndex >> beforeIndex = tryExtractPosition
    // * tryExtractPosition >> tryIndexOf = tryExtractIndex

    // assert: `lhs = rhs`
    let assertEquality lhs rhs textWithCursor =
      let textWithCursor = textWithCursor |> Text.trimTripleQuotation
      let lhs = textWithCursor |> lhs
      let rhs = textWithCursor |> rhs
      Expect.equal "Should hold: lhs = rhs (expected = actual)" lhs rhs
    let testEquality lhs rhs name textWithCursor =
      testCase name <| fun _ -> assertEquality lhs rhs textWithCursor
    let testEqualityForAllCursors lhs rhs textWithCursors =
      textWithCursors
      |> Text.trimTripleQuotation
      |> Cursors.iter
      |> List.mapi (fun i t -> testEquality lhs rhs $"Cursor {i}" t)

    /// assert: `value |> roundTrip = value` 
    let assertThereAndBackAgain value roundTrip textWithCursor =
      let textWithCursor = textWithCursor |> Text.trimTripleQuotation
      let (value,text) = textWithCursor |> value
      let roundTripped = (value, text) ||> roundTrip
      Expect.equal "Should hold: value |> roundTrip = value (expected |> roundTrip = actual)" value roundTripped
    let testThereAndBackAgain value roundTrip name textWithCursor =
      testCase name <| fun _ -> assertThereAndBackAgain value roundTrip textWithCursor
    let testThereAndBackAgainForAllCursors value roundTrip textWithCursors =
      textWithCursors
      |> Text.trimTripleQuotation
      |> Cursors.iter
      |> List.mapi (fun i -> testThereAndBackAgain value roundTrip $"Cursor {i}")

    testList "idx |> beforeIndex |> tryIndexOf = idx" [
      let value (textWithCursor: string) = 
        let idx = textWithCursor.IndexOf Cursor.Marker
        if idx < 0 then
          failtest "No cursor"
        let text = textWithCursor.Replace(Cursor.Marker, "")
        (idx, text)
      let roundTrip idx text =
        let pos = Cursor.beforeIndex idx text
        Cursor.tryIndexOf pos text
        |> Result.defaultWith (fun error -> failtest $"Error while IndexOf: {error}")
      testList "F# Code 1" (
        let text = """
$0module$0 Foo$0

$0let $0a = 42$0
let b =
  $0a $0+ 5$0
$0printfn "$0Result=%i" b$0
        """
        text |> testThereAndBackAgainForAllCursors value roundTrip
      )
    ]
    testList "pos |> tryIndexOf |> beforeIndex = pos" [
      let value (textWithCursor: string) = 
        textWithCursor
        |> Cursor.tryExtractPosition
        |> Option.defaultWith (fun _ -> failtest "No cursor")
      let roundTrip pos text =
        let idx = 
          text
          |> Cursor.tryIndexOf pos
          |> Result.defaultWith (fun error -> failtest $"Error while IndexOf: {error}")
        Cursor.beforeIndex idx text
      testList "F# Code 1" (
        let text = """
$0module$0 Foo$0

$0let $0a = 42$0
let b =
  $0a $0+ 5$0
$0printfn "$0Result=%i" b$0
        """
        text |> testThereAndBackAgainForAllCursors value roundTrip
      )
    ]
    testList "tryExtractIndex >> beforeIndex = tryExtractPosition" [
      let lhs = 
        Cursor.tryExtractIndex
        >> Option.defaultWith (fun _ -> failtest "No cursor")
        >> fun (idx, text) -> Cursor.beforeIndex idx text
      let rhs = 
        Cursor.tryExtractPosition
        >> Option.defaultWith (fun _ -> failtest "No cursor")
        >> fst
      testList "F# Code 1" (
        let text = """
$0module$0 Foo$0

$0let $0a = 42$0
let b =
  $0a $0+ 5$0
$0printfn "$0Result=%i" b$0
        """
        text |> testEqualityForAllCursors lhs rhs
      )
    ]
    testList "tryExtractPosition >> tryIndexOf = tryExtractIndex" [
      let lhs = 
        Cursor.tryExtractPosition
        >> Option.defaultWith (fun _ -> failtest "No cursor")
        >> fun (pos, text) -> Cursor.tryIndexOf pos text
        >> Result.defaultWith (fun error -> failtest $"No index: {error}")
      let rhs = 
        Cursor.tryExtractIndex
        >> Option.defaultWith (fun _ -> failtest "No cursor")
        >> fst
      testList "F# Code 1" (
        let text = """
$0module$0 Foo$0

$0let $0a = 42$0
let b =
  $0a $0+ 5$0
$0printfn "$0Result=%i" b$0
        """
        text |> testEqualityForAllCursors lhs rhs
      )
    ]
  ]

  let tests = testList (nameof Cursor) [
    tryExtractIndexTests
    tryExtractPositionMarkedWithAnyOfTests
    tryExtractPositionTests
    tryExtractRangeTests
    beforeIndexTests
    tryIndexOfTests
    identityTests
  ]

module private Cursors =
  open Expecto.Flip

  let private iterTests = testList (nameof Cursors.iter) [
    testCase "no cursor" <| fun _ ->
      let text = "foo bar baz"
      let expected = []
      text 
      |> Cursors.iter
      |> Expect.equal "should be empty because no cursors" expected
    testCase "one cursor" <| fun _ ->
      let text = "foo $0bar baz"
      let expected = [text]
      text 
      |> Cursors.iter
      |> Expect.equal "should have returned one strings with cursor" expected
    testCase "two cursors" <| fun _ ->
      let text = "foo $0bar baz$0"
      let expected = [
        "foo $0bar baz"
        "foo bar baz$0"
      ]
      text 
      |> Cursors.iter
      |> Expect.equal "should have returned two strings with cursor" expected
    testCase "three cursors" <| fun _ ->
      let text = "$0foo $0bar baz$0"
      let expected = [
        "$0foo bar baz"
        "foo $0bar baz"
        "foo bar baz$0"
      ]
      text 
      |> Cursors.iter
      |> Expect.equal "should have returned three strings with cursor" expected
    testCase "four cursors" <| fun _ ->
      let text = "$0foo $0ba$0r baz$0"
      let expected = [
        "$0foo bar baz"
        "foo $0bar baz"
        "foo ba$0r baz"
        "foo bar baz$0"
      ]
      text 
      |> Cursors.iter
      |> Expect.equal "should have returned three strings with cursor" expected
    testCase "cursors in triple quoted string" <| fun _ ->
      let text = !- """
module $0Foo

let a = 42
$0let b = 
  a + 5$0
printfn "Result=%i$0" b$0
      """
      let expected = [
        !- """
module $0Foo

let a = 42
let b = 
  a + 5
printfn "Result=%i" b
        """
        !- """
module Foo

let a = 42
$0let b = 
  a + 5
printfn "Result=%i" b
        """
        !- """
module Foo

let a = 42
let b = 
  a + 5$0
printfn "Result=%i" b
        """
        !- """
module Foo

let a = 42
let b = 
  a + 5
printfn "Result=%i$0" b
        """
        !- """
module Foo

let a = 42
let b = 
  a + 5
printfn "Result=%i" b$0
        """
      ]
      text 
      |> Cursors.iter
      |> Expect.equal "should have returned all strings with single cursor" expected
  ]

  let private extractWithTests = testList (nameof Cursors.extractWith) [
    testCase "can extract all cursors" <| fun _ ->
      let text = !- """
        let $Ff a b = a + b
        let $Vvalue = 42
        let $0res = $Ff $Vvalue 3
        ()
        """
      let actual =
        text
        |> Cursors.extractWith [|"$F"; "$V"; "$0" |]

      let expectedText = !- """
        let f a b = a + b
        let value = 42
        let res = f value 3
        ()
        """
      let expectedPoss = [
        ("$F", pos 0 4)
        ("$V", pos 1 4)
        ("$0", pos 2 4)
        ("$F", pos 2 10)
        ("$V", pos 2 12)
      ]
      let expected = (expectedText, expectedPoss)

      actual
      |> Expect.equal "markers should match" expected 
  ]
  
  let tests = testList (nameof Cursors) [
    iterTests
    extractWithTests
  ]


module private Text =
  open Expecto.Flip
  let inline private range start fin = { Start = start; End = fin}

  let private removeTests = testList (nameof Text.remove) [
    testList "start=end should remove nothing" [
      let assertNothingChanged textWithCursor =
        let (range, text) = 
          textWithCursor
          |> Text.trimTripleQuotation
          |> Cursor.tryExtractRange 
          |> Option.defaultWith (fun _ -> failtest $"no cursor found")
        let expected = Ok text
        text
        |> Text.remove range
        |> Expect.equal "shouldn't have changed input string" expected

      let just pos = range pos pos
      testCase "empty string" <| fun _ ->
        let text = ""
        let range = just <| pos 0 0
        let expected = ""
        text 
        |> Text.remove range
        |> Expect.equal "shouldn't have change input string" (Ok expected)
      testCase "empty string with two cursors" <| fun _ ->
        "$0$0"
        |> assertNothingChanged
      testList "single line string" [
        testCase "start" <| fun _ ->
          "$0foo bar baz"
          |> assertNothingChanged
        testCase "middle" <| fun _ ->
          "foo b$0ar baz"
          |> assertNothingChanged
        testCase "end" <| fun _ ->
          "foo bar baz$0"
          |> assertNothingChanged
        testCase "two cursors in middle" <| fun _ ->
          "foo $0$0bar baz"
          |> assertNothingChanged
      ]
      testList "two line string" [
        testCase "start" <| fun _ ->
          "$0foo bar\n baz"
          |> assertNothingChanged
        testCase "end 1st line" <| fun _ ->
          "foo bar$0\n baz"
          |> assertNothingChanged
        testCase "start 2nd line" <| fun _ ->
          "foo bar\n$0 baz"
          |> assertNothingChanged
        testCase "middle 2nd line" <| fun _ ->
          "foo bar\n ba$0z"
          |> assertNothingChanged
        testCase "end" <| fun _ ->
          "foo bar\n baz$0"
          |> assertNothingChanged
      ]
      testList "F# Code" (
        let text = """
$0module$0 Foo$0

$0let $0a = 42$0
let b =
  $0a $0+ 5$0
$0printfn "$0Result=%i" b$0
        """
        text
        |> Cursors.iter
        |> List.mapi (fun i t ->
            testCase $"Cursor {i}" <| fun _ ->
              t |> assertNothingChanged
        )
      )
    ]
  
    let assertRemoveRange range expected text =
      text
      |> Text.remove range
      |> Expect.equal "incorrect string after removing" (Ok expected)
    let assertAfterRemovingIs expected textWithRangeCursors =
      let (range, text) =
        textWithRangeCursors
        |> Text.trimTripleQuotation
        |> Cursor.tryExtractRange
        |> Option.defaultWith (fun _ -> failtest "No cursors")
      assertRemoveRange range expected text
    testList "remove inside single line" [
      testList "single line string" [
        testCase "remove everything" <| fun _ ->
          let text = "$0foo bar baz$0"
          let expected = ""
          text |> assertAfterRemovingIs expected
        testCase "remove start to end of first word" <| fun _ ->
          let text = "$0foo$0 bar baz"
          let expected = " bar baz"
          text |> assertAfterRemovingIs expected
        testCase "remove last word to end of string" <| fun _ ->
          let text = "foo bar $0baz$0"
          let expected = "foo bar "
          text |> assertAfterRemovingIs expected
        testCase "remove word in middle" <| fun _ ->
          let text = "foo $0bar$0 baz"
          let expected = "foo  baz"
          text |> assertAfterRemovingIs expected
        testCase "remove a lot in middle" <| fun _ ->
          let text = "f$0oo bar ba$0z"
          let expected = "fz"
          text |> assertAfterRemovingIs expected
      ]
      testList "three line string" [
        testCase "remove everything" <| fun _ ->
          let text = "$0foo bar\nbaz\nlorem ipsum$0"
          let expected = ""
          text |> assertAfterRemovingIs expected
        testCase "remove first line without line break" <| fun _ ->
          // let text = "$0foo bar$0\nbaz\nlorem ipsum"
          let expected = "\nbaz\nlorem ipsum"
          // text |> assertAfterRemovingIs expected
          let text = "foo bar\nbaz\nlorem ipsum"
          let range = range (pos 0 0) (pos 0 7)
          text |> assertRemoveRange range expected
        testCase "remove first line with line break" <| fun _ ->
          // strictly speaking this removes over two lines...
          // let text = "$0foo bar\n$0baz\nlorem ipsum"
          let expected = "baz\nlorem ipsum"
          // text |> assertAfterRemovingIs expected
          let text = "foo bar\nbaz\nlorem ipsum"
          let range = range (pos 0 0) (pos 1 0)
          text |> assertRemoveRange range expected
        testCase "remove 2nd line without line breaks" <| fun _ ->
          let text = "foo bar\n$0baz$0\nlorem ipsum"
          let expected = "foo bar\n\nlorem ipsum"
          text |> assertAfterRemovingIs expected
        testCase "remove 2nd line with line breaks" <| fun _ ->
          let text = "foo bar$0\nbaz\n$0lorem ipsum"
          let expected = "foo barlorem ipsum"
          text |> assertAfterRemovingIs expected
        testCase "remove 3rd line without line break" <| fun _ ->
          let text = "foo bar\nbaz\n$0lorem ipsum$0"
          let expected = "foo bar\nbaz\n"
          text |> assertAfterRemovingIs expected
        testCase "remove 3rd line with line break" <| fun _ ->
          let text = "foo bar\nbaz$0\nlorem ipsum$0"
          let expected = "foo bar\nbaz"
          text |> assertAfterRemovingIs expected
      ]
      testList "F# Code" [
        testCase "Remove empty line" <| fun _ ->
          let text = """
module Foo
$0
$0let a = 42
let b =
  a + 5
printfn "Result=%i" b
          """
          let expected = !- """
module Foo
let a = 42
let b =
  a + 5
printfn "Result=%i" b
          """
          text |> assertAfterRemovingIs expected
        testCase "remove word" <| fun _ ->
          let text = """
module Foo

let a = 42
let $0b$0 =
  a + 5
printfn "Result=%i" b
          """
          let expected = !- """
module Foo

let a = 42
let  =
  a + 5
printfn "Result=%i" b
          """
          text |> assertAfterRemovingIs expected
        testCase "remove end" <| fun _ ->
          let text = """
module Foo

let a = 42
let b =
  a + 5
printfn "$0Result=%i" b$0
          """
          let expected = !- """
module Foo

let a = 42
let b =
  a + 5
printfn "
          """
          text |> assertAfterRemovingIs expected
        testCase "remove start" <| fun _ ->
          let text = """
$0module $0Foo

let a = 42
let b =
  a + 5
printfn "Result=%i" b
          """
          let expected = !- """
Foo

let a = 42
let b =
  a + 5
printfn "Result=%i" b
          """
          text |> assertAfterRemovingIs expected
      ]
    ]
    testList "remove over multiple lines" [
      testList "F# Code" [
        testCase "remove everything" <| fun _ ->
          let text = """
$0module Foo

let a = 42
let b =
  a + 5
printfn "Result=%i" b$0"""
          let expected = ""
          text |> assertAfterRemovingIs expected
        testCase "remove everything except last line break" <| fun _ ->
          let text = """
$0module Foo

let a = 42
let b =
  a + 5
printfn "Result=%i" b$0
          """
          let expected = "\n"
          text |> assertAfterRemovingIs expected
        testCase "remove lines 3-5" <| fun _ ->
          let text = """
module Foo

$0let a = 42
let b =
  a + 5
$0printfn "Result=%i" b
          """
          let expected = !-"""
module Foo

printfn "Result=%i" b
          """
          text |> assertAfterRemovingIs expected
        testCase "remove lines from inside line 3 to inside line 5" <| fun _ ->
          let text = """
module Foo

let a = $042
let b =
  a + $05
printfn "Result=%i" b
          """
          let expected = !-"""
module Foo

let a = 5
printfn "Result=%i" b
          """
          text |> assertAfterRemovingIs expected
        testCase "remove start to inside line 4" <| fun _ ->
          let text = """
$0module Foo

let a = 42
let b $0=
  a + 5
printfn "Result=%i" b
          """
          let expected = !-"""
=
  a + 5
printfn "Result=%i" b
          """
          text |> assertAfterRemovingIs expected
        testCase "remove inside line 4 to end" <| fun _ ->
          let text = """
module Foo

let a = 42
let b $0=
  a + 5
printfn "Result=%i" b$0"""
          let expected = !-"""
module Foo

let a = 42
let b """
          text |> assertAfterRemovingIs expected
      ]
    ]
  ]

  let private insertTests = testList (nameof Text.insert) [
    testList "empty insert should insert nothing" [
      let assertNothingChanged textWithCursor =
        let (pos, text) =
          textWithCursor
          |> Text.trimTripleQuotation
          |> Cursor.tryExtractPosition
          |> Option.defaultWith (fun _ -> failtest $"no cursor found")
        let expected = Ok text
        text
        |> Text.insert pos ""
        |> Expect.equal "shouldn't have changed input string" expected

      testCase "into empty string" <| fun _ ->
        "$0"
        |> assertNothingChanged
      testList "into single line" [
        testCase "start" <| fun _ ->
          "$0foo bar baz"
          |> assertNothingChanged
        testCase "middle" <| fun _ ->
          "foo ba$0r baz"
          |> assertNothingChanged
        testCase "end" <| fun _ ->
          "foo bar baz$0"
          |> assertNothingChanged
      ]
      testList "into three lines" [
        testCase "start" <| fun _ ->
          "$0foo\nbar\nbaz"
          |> assertNothingChanged
        testCase "start 2nd line" <| fun _ ->
          "foo\n$0bar\nbaz"
          |> assertNothingChanged
        testCase "middle 2nd line" <| fun _ ->
          "foo\nba$0r\nbaz"
          |> assertNothingChanged
        testCase "end 2nd line" <| fun _ ->
          "foo\nbar$0\nbaz"
          |> assertNothingChanged
        testCase "end" <| fun _ ->
          "foo\nbar\nbaz$0"
          |> assertNothingChanged
      ]
      testList "into F# Code" (
        let text = """
$0module$0 Foo$0

$0let $0a = 42$0
let b =
  $0a $0+ 5$0
$0printfn "$0Result=%i" b$0
        """
        text
        |> Cursors.iter
        |> List.mapi (fun i t ->
          testCase $"Cursor {i}" <| fun _ ->
            t |> assertNothingChanged
        )
      )
    ]

    let assertAfterInsertingIs expected (textWithCursor, insert) =
      let (pos, text) =
        textWithCursor
        |> Text.trimTripleQuotation
        |> Cursor.tryExtractPosition
        |> Option.defaultWith (fun _ -> failtest "No cursor")
      text
      |> Text.insert pos insert
      |> Expect.equal "incorrect string after inserting" (Ok expected)
    testList "insert without linebreak" [
      testCase "into empty string" <| fun _ ->
        let text = "$0"
        let insert = "some text"
        let expected = insert
        (text,insert)
        |> assertAfterInsertingIs expected
      testList "into single line string" [
        testCase "start" <| fun _ ->
          let text = "$0foo bar baz"
          let insert = "some text"
          let expected = $"{insert}foo bar baz"
          (text,insert)
          |> assertAfterInsertingIs expected
        testCase "middle" <| fun _ ->
          let text = "foo b$0ar baz"
          let insert = "some text"
          let expected = "foo bsome textar baz"
          (text,insert)
          |> assertAfterInsertingIs expected
        testCase "end" <| fun _ ->
          let text = "foo bar baz$0"
          let insert = "some text"
          let expected = $"foo bar baz{insert}"
          (text,insert)
          |> assertAfterInsertingIs expected
      ]
      testList "into F# Code" [
        testCase "start of 4th line" <| fun _ ->
          let text = """
module Foo

let a = 42
$0let b =
  a + 5
printfn "Result=%i" b
          """
          let insert = "some text"
          let expected = !- """
module Foo

let a = 42
some textlet b =
  a + 5
printfn "Result=%i" b
          """
          (text, insert)
          |> assertAfterInsertingIs expected
        testCase "middle of 4th line" <| fun _ ->
          let text = """
module Foo

let a = 42
let b$0 =
  a + 5
printfn "Result=%i" b
          """
          let insert = "some text"
          let expected = !- """
module Foo

let a = 42
let bsome text =
  a + 5
printfn "Result=%i" b
          """
          (text, insert)
          |> assertAfterInsertingIs expected
        testCase "end of 4th line" <| fun _ ->
          let text = """
module Foo

let a = 42
let b =$0
  a + 5
printfn "Result=%i" b
          """
          let insert = "some text"
          let expected = !- """
module Foo

let a = 42
let b =some text
  a + 5
printfn "Result=%i" b
          """
          (text, insert)
          |> assertAfterInsertingIs expected
      ]
    ]

    testList "insert with line break" [
      testCase "into empty string" <| fun _ ->
        let text = "$0"
        let insert = "lorem\nipsum"
        let expected = insert
        (text, insert)
        |> assertAfterInsertingIs expected
      testList "into single line string" [
        testCase "start" <| fun _ ->
          let text = "$0foo bar baz"
          let insert = "lorem\nipsum"
          let expected = "lorem\nipsumfoo bar baz"
          (text, insert)
          |> assertAfterInsertingIs expected
        testCase "middle" <| fun _ ->
          let text = "foo b$0ar baz"
          let insert = "lorem\nipsum"
          let expected = "foo blorem\nipsumar baz"
          (text, insert)
          |> assertAfterInsertingIs expected
        testCase "end" <| fun _ ->
          let text = "foo bar baz$0"
          let insert = "lorem\nipsum"
          let expected = "foo bar bazlorem\nipsum"
          (text, insert)
          |> assertAfterInsertingIs expected
      ]
      testList "into F# Code" [
        testCase "start" <| fun _ ->
          let text = """
$0module Foo

let a = 42
let b =
  a + 5
printfn "Result=%i" b
          """
          let insert = "lorem\nipsum"
          let expected = !- """
lorem
ipsummodule Foo

let a = 42
let b =
  a + 5
printfn "Result=%i" b
          """
          (text, insert)
          |> assertAfterInsertingIs expected
        testCase "end" <| fun _ ->
          let text = """
module Foo

let a = 42
let b =
  a + 5
printfn "Result=%i" b$0"""
          let insert = "lorem\nipsum"
          let expected = !- """
module Foo

let a = 42
let b =
  a + 5
printfn "Result=%i" blorem
ipsum"""
          (text, insert)
          |> assertAfterInsertingIs expected
        testCase "end before line break" <| fun _ ->
          let text = """
module Foo

let a = 42
let b =
  a + 5
printfn "Result=%i" b$0
          """
          let insert = "lorem\nipsum"
          let expected = !- """
module Foo

let a = 42
let b =
  a + 5
printfn "Result=%i" blorem
ipsum
          """
          (text, insert)
          |> assertAfterInsertingIs expected
        testCase "start of 4th line" <| fun _ ->
          let text = """
module Foo

let a = 42
$0let b =
  a + 5
printfn "Result=%i" b
          """
          let insert = "lorem\nipsum"
          let expected = !- """
module Foo

let a = 42
lorem
ipsumlet b =
  a + 5
printfn "Result=%i" b
          """
          (text, insert)
          |> assertAfterInsertingIs expected
        testCase "middle of 4th line" <| fun _ ->
          let text = """
module Foo

let a = 42
let b$0 =
  a + 5
printfn "Result=%i" b
          """
          let insert = "lorem\nipsum"
          let expected = !- """
module Foo

let a = 42
let blorem
ipsum =
  a + 5
printfn "Result=%i" b
          """
          (text, insert)
          |> assertAfterInsertingIs expected
        testCase "end of 4th line" <| fun _ ->
          let text = """
module Foo

let a = 42
let b =$0
  a + 5
printfn "Result=%i" b
          """
          let insert = "lorem\nipsum"
          let expected = !- """
module Foo

let a = 42
let b =lorem
ipsum
  a + 5
printfn "Result=%i" b
          """
          (text, insert)
          |> assertAfterInsertingIs expected
      ]
    ]
  ]

  let private replaceTests = testList (nameof Text.replace) [
    testList "neither change nor insert" [
      let assertNothingChanged (textWithCursors, replacement) =
        let (range, text) =
          textWithCursors
          |> Text.trimTripleQuotation
          |> Cursor.tryExtractRange
          |> Option.defaultWith (fun _ -> failtest $"no cursor(s) found")
        let expected = Ok text
        text
        |> Text.replace range replacement
        |> Expect.equal "shouldn't have changed input string" expected
      testCase "insert empty string into empty string" <| fun _ ->
        let text = "$0"
        let replacement = ""
        (text, replacement)
        |> assertNothingChanged
      testCase "replace single line text with same text" <| fun _ ->
        let text = "$0foo bar baz$0"
        let replacement = "foo bar baz"
        (text, replacement)
        |> assertNothingChanged
      testCase "replace inside single line text with same text" <| fun _ ->
        let text = "foo$0 bar$0 baz"
        let replacement = " bar"
        (text, replacement)
        |> assertNothingChanged
      testCase "insert empty string into single line string" <| fun _ ->
        let text = "foo $0bar baz"
        let replacement = ""
        (text, replacement)
        |> assertNothingChanged
      testList "F# Code" [
        testCase "insert empty string" <| fun _ ->
          let text = """
module Foo

let a = 42
let b$0 =
  a + 5
printfn "Result=%i" b
        """
          let replacement = ""
          (text, replacement)
          |> assertNothingChanged
        testCase "replace everything with itself" <| fun _ ->
          let text = """
$0module Foo

let a = 42
let b =
  a + 5
printfn "Result=%i" b$0"""
          let replacement = !- """
module Foo

let a = 42
let b =
  a + 5
printfn "Result=%i" b"""
          (text, replacement)
          |> assertNothingChanged
        testCase "replace inside with same string" <| fun _ ->
          let text = """
module Foo

let a = 42
let $0b =
  a +$0 5
printfn "Result=%i" b
          """
          let replacement = "b =\n  a +"
          (text, replacement)
          |> assertNothingChanged
      ]
    ]
    let assertAfterChangingIs expected (textWithCursors, replacement) =
      let (range, text) =
        textWithCursors
        |> Text.trimTripleQuotation
        |> Cursor.tryExtractRange
        |> Option.defaultWith (fun _ -> failtest $"no cursor(s) found")
      let expected = Ok expected
      text
      |> Text.replace range replacement
      |> Expect.equal "unexpected change" expected

    testList "replace in F# Code" [
      testCase "delete everything" <| fun _ ->
        let text = """
$0module Foo

let a = 42
let b =
a + 5
printfn "Result=%i" b$0"""
        let replacement = ""
        let expected = ""
        (text, replacement)
        |> assertAfterChangingIs expected
      testCase "replace everything" <| fun _ ->
        let text = """
$0module Foo

let a = 42
let b =
a + 5
printfn "Result=%i" b$0"""
        let replacement = !- """
module Blub

42
|> (+) 5
|> printfn "Result = %i"
        """
        let expected = replacement
        (text, replacement)
        |> assertAfterChangingIs expected

      testCase "insert some lines" <| fun _ ->
        let text = """
module Foo

$0let a = 42
let b =
a + 5
printfn "Result=%i" b
        """
        let replacement = !- """
let pi = 3.14
let pi2 = pi * pi
        """
        let expected = !- """
module Foo

let pi = 3.14
let pi2 = pi * pi
let a = 42
let b =
a + 5
printfn "Result=%i" b
        """
        (text, replacement)
        |> assertAfterChangingIs expected
    ]
  ]

  let tests = testList (nameof Text) [
    removeTests
    insertTests
    replaceTests
  ]

module private TextEdit =

  /// FSAC might return TextEdits with NewLine matching the OS
  /// but tests here only handle `\n` not `\r`
  /// -> test `TextEdit.apply` replaces/removes `\r`
  let private eolTests = testList "EOL" [
    let testEOLs baseName textWithRange (newTextWithN: string) expected =
      testList baseName [
        let expected = !- expected
        for eol in ["\n"; "\r\n"; "\r"] do
          let eolStr = System.Text.RegularExpressions.Regex.Escape eol
          testCase $"with {eolStr}" <| fun _ ->
            let (range, text) = Cursor.assertExtractRange <| !- textWithRange
            let newText = newTextWithN.Replace("\n", eol)
            let edit: TextEdit = {
              NewText = newText
              Range = range
            }

            let actual =
              text
              |> TextEdit.apply edit
              |> Flip.Expect.wantOk "Apply should not fail"

            Expect.equal actual expected "Apply should produce correct text"
            Expect.isFalse (actual.Contains '\r') "Should not contain \\r"
      ]

    testEOLs 
      "can apply insert edit"
      """
let foo = 42$0
let bar = 2
      """
      "\nlet baz = 4"
      """
let foo = 42
let baz = 4
let bar = 2
      """
    testEOLs 
      "can apply delete edit"
      """
let foo = $042
let bar = $02
      """
      ""  // kinda pointless: no new line in insert -> no new lines to replace...
      """
let foo = 2
      """
    testEOLs 
      "can apply replace edit"
      """
let foo = $042
let bar =$0 2
      """
      "3\nlet a = 1\nlet baz ="
      """
let foo = 3
let a = 1
let baz = 2
      """
  ]
  let private applyTests = testList (nameof TextEdit.apply) [
    eolTests
  ]
  
  let private tryFindErrorTests = testList (nameof TextEdit.tryFindError) [
    testCase "valid delete edit should should be ok" <| fun _ ->
      {
        Range = { Start = pos 2 2; End = pos 3 3 }
        NewText = ""
      }
      |> TextEdit.tryFindError
      |> Flip.Expect.isNone "Valid delete should be ok"
    testCase "valid insert edit should should be ok" <| fun _ ->
      {
        Range = { Start = pos 2 3; End = pos 2 3 }
        NewText = "foo"
      }
      |> TextEdit.tryFindError
      |> Flip.Expect.isNone "Valid delete should be ok"
    testCase "valid replace edit should should be ok" <| fun _ ->
      {
        Range = { Start = pos 2 3; End = pos 4 9 }
        NewText = "foo"
      }
      |> TextEdit.tryFindError
      |> Flip.Expect.isNone "Valid delete should be ok"
    testCase "empty edit should fail" <| fun _ ->
      {
        Range = { Start = pos 2 4; End = pos 2 4 }
        NewText = ""
      }
      |> TextEdit.tryFindError
      |> Flip.Expect.isSome "Empty edit should fail"
    testCase "edit with End before Start should fail" <| fun _ ->
      {
        Range = { Start = pos 3 4; End = pos 2 2 }
        NewText = ""
      }
      |> TextEdit.tryFindError
      |> Flip.Expect.isSome "End before Start should fail"
  ]

  let tests = testList (nameof TextEdit) [
    applyTests
    tryFindErrorTests
  ]


module private TextEdits =
  let sortByRangeTests = testList (nameof TextEdits.sortByRange) [
    let test (edits: TextEdit list) =
      let sorted = edits |> TextEdits.sortByRange
      Expect.equal (sorted.Length) (edits.Length) "Sorted edits should have same length as input edits"

      // must hold for all in sorted:
      // * r <= succ(r)
      //   -> sorted
      // * r = succ(r) -> Index(edits, r) < Index(edits, succ(r))
      //   -> preserve order
      let unsorted = 
        sorted 
        |> List.pairwise
        |> List.filter (fun (r, succ) -> not <| Position.leq r.Range.Start succ.Range.Start)
      // isEmpty doesn't print list when failure...
      if not (unsorted |> List.isEmpty) then
        logger.error (
          eventX "Unsorted: {list}"
          >> setField "list" unsorted
        )
      Expect.isEmpty unsorted "All edits should be sorted"

      // Note: for this to work edits must be different (-> different NewText)
      let idxInEdits (edit: TextEdit) =
        edits |> List.findIndex ((=) edit)

      let unordered =
        sorted
        |> List.indexed
        |> List.pairwise
        |> List.filter (fun ((_, r), (_, succ)) -> r.Range.Start = succ.Range.Start)
        |> List.choose (fun ((i1, e1), (i2, e2)) -> 
            let iSrc1, iSrc2 = (idxInEdits e1, idxInEdits e2)
            assert(iSrc1 <> iSrc2)
            if iSrc1 < iSrc2 then
              None
            else
              {|
                Edits = (e1, e2)
                SourceIndicies = (iSrc1, iSrc2)
                SortedIndices = (i1, i2)
              |}
              |> Some
        )
      // isEmpty doesn't print list when failure...
      if not (unordered |> List.isEmpty) then
        logger.error (
          eventX "Unordered: {list}"
          >> setField "list" unordered
        )
      Expect.isEmpty unordered "Edits with same start should keep order"

    testCase "can sort distinct ranges" <| fun _ -> 
      [
        (1,5)
        (1,1)
        (3, 2)
        (8, 5)
        (5, 4)
        (5, 6)
        (4, 11)
        (1,7)
      ]
      |> List.mapi (fun i (l,c) ->
        // end doesn't really matter (no overlap allowed)
        let start = { Line = l; Character = c }
        {
          Range = { Start = start; End = start }
          NewText = $"{i}=({l},{c})"
        }
      )
      |> test

    testCase "can sort all same position ranges" <| fun _ ->
      List.replicate 10 (2,4)
      |> List.mapi (fun i (l,c) ->
        // end doesn't really matter (no overlap allowed)
        let start = { Line = l; Character = c }
        {
          Range = { Start = start; End = start }
          NewText = $"{i}=({l},{c})"
        }
      )
      |> test

    testCase "can sort mix of same and different positions" <| fun _ ->
      [
        (1,5)
        (1,1)
        (3, 2)
        (5, 4)
        (1,5)
        (8, 5)
        (5, 4)
        (5, 6)
        (4, 11)
        (4, 11)
        (1,7)
      ]
      |> List.mapi (fun i (l,c) ->
        // end doesn't really matter (no overlap allowed)
        let start = { Line = l; Character = c }
        {
          Range = { Start = start; End = start }
          NewText = $"{i}=({l},{c})"
        }
      )
      |> test
  ]

  let tryFindErrorTests = testList (nameof TextEdits.tryFindError) [
    testCase "valid single edit should succeed" <| fun _ ->
      [
        { NewText = "foo"; Range = { Start = pos 1 2; End = pos 1 5 } }
      ]
      |> TextEdits.tryFindError
      |> Flip.Expect.isNone "valid single edit should succeed"
    testCase "valid multiple edits should succeed" <| fun _ ->
      [
        { NewText = "foo"; Range = { Start = pos 1 2; End = pos 1 5 } }
        { NewText = "bar"; Range = { Start = pos 5 2; End = pos 5 2 } }
        { NewText = "baz"; Range = { Start = pos 2 2; End = pos 3 3 } }
      ]
      |> TextEdits.tryFindError
      |> Flip.Expect.isNone "valid multiple edits should succeed"
    testCase "no edit should fail" <| fun _ ->
      TextEdits.tryFindError []
      |> Flip.Expect.isSome "No edit should fail"
    let replace (start, fin) text : TextEdit = {
      NewText = text
      Range = { Start = start; End = fin }
    }
    let delete (start, fin) = replace (start, fin) ""
    let insert pos text = replace (pos, pos) text
    let empty pos = insert pos ""
    /// used to mark edits that aren't main parts of the test, but instead are just used as 'surrounding'
    /// -> `filler` used for tagging
    let inline filler v = v
    testCase "single empty edit should fail" <| fun _ ->
      TextEdits.tryFindError [empty (pos 2 3)]
      |> Flip.Expect.isSome "Empty edit should fail"
    testCase "multiple empty edits should fail" <| fun _ ->
      TextEdits.tryFindError [empty (pos 2 3); empty (pos 3 5); empty (pos 1 1)]
      |> Flip.Expect.isSome "Empty edit should fail"
    testCase "empty edit in list with valid edits should fail" <| fun _ ->
      [
        filler <| replace (pos 1 2, pos 1 5) "0"
        filler <| replace (pos 5 2, pos 5 2) "1"
        empty (pos 1 7)
        filler <| replace (pos 2 2, pos 3 3) "1"
      ]
      |> TextEdits.tryFindError
      |> Flip.Expect.isSome "Empty edit should fail"
    testCase "two overlapping edits (Back/Front) on one line should fail" <| fun _ ->
      [
        replace (pos 1 2, pos 1 5) "front overlap"
        replace (pos 1 3, pos 1 7) "back overlap"
      ]
      |> TextEdits.tryFindError
      |> Flip.Expect.isSome "Overlapping edits should fail"
    testCase "two overlapping edits (Front/Back) on one line should fail" <| fun _ ->
      [
        replace (pos 1 3, pos 1 7) "back overlap"
        replace (pos 1 2, pos 1 5) "front overlap"
      ]
      |> TextEdits.tryFindError
      |> Flip.Expect.isSome "Overlapping edits should fail"
    testCase "two overlapping edits (Back/Front) over multiple lines should fail" <| fun _ ->
      [
        replace (pos 1 2, pos 3 5) "front overlap"
        replace (pos 2 3, pos 5 7) "back overlap"
      ]
      |> TextEdits.tryFindError
      |> Flip.Expect.isSome "Overlapping edits should fail"
    testCase "two touching edits should succeed" <| fun _ ->
      // valid because: cursor is between characters
      //  -> replace prior to (3,5); replace after (3,5)
      //  -> do not interfere with each other
      [
        replace (pos 1 2, pos 3 5) "front"
        replace (pos 3 5, pos 5 7) "back"
      ]
      |> TextEdits.tryFindError
      |> Flip.Expect.isNone "Touching edits should succeed"
    testCase "two overlapping edits (Front/Back) over multiple lines should fail" <| fun _ ->
      [
        replace (pos 2 3, pos 5 7) "back overlap"
        replace (pos 1 2, pos 3 5) "front overlap"
      ]
      |> TextEdits.tryFindError
      |> Flip.Expect.isSome "Overlapping edits should fail"
    testCase "overlapping edits (Back/Front) in list with valid edits should fail" <| fun _ ->
      [
        filler <| replace (pos 1 1, pos 1 1) "0"
        filler <| replace (pos 17 8, pos 19 8) "1"
        replace (pos 1 2, pos 3 5) "front overlap"
        filler <| replace (pos 7 5, pos 8 9) "2"
        replace (pos 2 3, pos 5 7) "back overlap"
        filler <| replace (pos 11 1, pos 15 9) "3"
      ]
      |> TextEdits.tryFindError
      |> Flip.Expect.isSome "Overlapping edits should fail"
    testCase "replace inside another replace should fail" <| fun _ ->
      [
        replace (pos 2 3, pos 4 1) "inside"
        replace (pos 1 2, pos 5 7) "outside"
      ]
      |> TextEdits.tryFindError
      |> Flip.Expect.isSome "Inside edits should fail"
    testCase "replace with another replace inside should fail" <| fun _ ->
      [
        replace (pos 1 2, pos 5 7) "outside"
        replace (pos 2 3, pos 4 1) "inside"
      ]
      |> TextEdits.tryFindError
      |> Flip.Expect.isSome "Inside edits should fail"
    testCase "inserts with same position should succeed" <| fun _ ->
      [
        insert (pos 2 4) "insert 1"
        insert (pos 2 4) "insert 2"
      ]
      |> TextEdits.tryFindError
      |> Flip.Expect.isNone "Same position inserts should succeed"
    testCase "inserts with same position followed by replace starting at same position should succeed" <| fun _ ->
      [
        insert (pos 2 4) "insert 1"
        insert (pos 2 4) "insert 2"
        replace (pos 2 4, pos 4 7) "replace"
      ]
      |> TextEdits.tryFindError
      |> Flip.Expect.isNone "Same position inserts followed by replace should succeed"
    testCase "replace before insert on same position should fail" <| fun _ ->
      [
        replace (pos 2 4, pos 4 7) "replace"
        insert (pos 2 4) "a"
      ]
      |> TextEdits.tryFindError
      |> Flip.Expect.isSome "Replace before insert on same position should fail"
    testCase "inserts with same position followed by replace at same position intermingled with other valid edits should succeed" <| fun _ ->
      [
        filler <| replace (pos 6 7, pos 7 9) "0"
        insert (pos 2 4) "insert 1"
        filler <| replace (pos 1 4, pos 2 1) "1"
        filler <| replace (pos 11 17, pos 18 19) "2"
        insert (pos 2 4) "insert 2"
        filler <| replace (pos 6 1, pos 6 2) "3"
        replace (pos 2 4, pos 4 7) "replace"
        filler <| replace (pos 9 2, pos 9 7) "4"
      ]
      |> TextEdits.tryFindError
      |> Flip.Expect.isNone "Same position inserts followed by replace should succeed"
    testCase "replace before insert on same position intermingled with other valid edits should fail" <| fun _ ->
      [
        filler <| replace (pos 6 7, pos 7 9) "0"
        insert (pos 2 4) "insert 1"
        filler <| replace (pos 1 4, pos 2 1) "1"
        filler <| replace (pos 11 17, pos 18 19) "2"
        replace (pos 2 4, pos 4 7) "replace"
        filler <| replace (pos 6 1, pos 6 2) "3"
        insert (pos 2 4) "insert 2"
        filler <| replace (pos 9 2, pos 9 7) "4"
      ]
      |> TextEdits.tryFindError
      |> Flip.Expect.isSome "Replace before insert on same position should fail"
    testCase "two replaces in same position should fail" <| fun _ ->
      [
        replace (pos 2 4, pos 5 9) "replace 1"
        replace (pos 2 4, pos 4 7) "replace 2"
      ]
      |> TextEdits.tryFindError
      |> Flip.Expect.isSome "Two replaces in same position should fail"
    testCase "two replaces in same position intermingled with other valid edits should fail should fail" <| fun _ ->
      [
        filler <| replace (pos 6 7, pos 7 9) "0"
        replace (pos 2 4, pos 5 9) "replace 1"
        filler <| replace (pos 1 4, pos 2 1) "1"
        replace (pos 2 4, pos 4 7) "replace 2"
        filler <| replace (pos 6 1, pos 6 2) "2"
      ]
      |> TextEdits.tryFindError
      |> Flip.Expect.isSome "Two replaces in same position should fail"
  ]

  let applyTests = testList (nameof TextEdits.apply) [
    testList "single edit" [
      testCase "insert" <| fun _ ->
        let (range, text) = Cursor.assertExtractRange !- """
          let foo = 42$0
          let bar = 2
          """
        let edit: TextEdit = {
          NewText = "\nlet baz = 4"
          Range = range
        }
        let expected = !- """
          let foo = 42
          let baz = 4
          let bar = 2
          """
        let actual =
          text
          |> TextEdit.apply edit
          |> Flip.Expect.wantOk "Apply should not fail"
        Expect.equal actual expected "Apply should produce correct text"
      testCase "remove" <| fun _ ->
        let (range, text) = Cursor.assertExtractRange !- """
          let foo = $042
          let bar = $02
          """
        let edit: TextEdit = {
          NewText = ""
          Range = range
        }
        let expected = !- """
          let foo = 2
          """
        let actual =
          text
          |> TextEdit.apply edit
          |> Flip.Expect.wantOk "Apply should not fail"
        Expect.equal actual expected "Apply should produce correct text"
      testCase "replace" <| fun _ ->
        let (range, text) = Cursor.assertExtractRange !- """
          let foo = $042
          let bar$0 = 2
          """
        let edit: TextEdit = {
          NewText = "1\nlet baz"
          Range = range
        }
        let expected = !- """
          let foo = 1
          let baz = 2
          """
        let actual =
          text
          |> TextEdit.apply edit
          |> Flip.Expect.wantOk "Apply should not fail"
        Expect.equal actual expected "Apply should produce correct text"
    ]
  ]
  let tests = testList (nameof TextEdits) [
    sortByRangeTests
    tryFindErrorTests
    applyTests
  ]

let tests = testList (nameof TextEdit) [
  Cursor.tests
  Cursors.tests
  Text.tests
  TextEdit.tests
  TextEdits.tests
]
