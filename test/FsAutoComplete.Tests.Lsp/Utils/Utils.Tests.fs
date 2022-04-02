module Utils.Tests.Utils

open Utils.Utils
open Expecto

module private Range =
  open Ionide.LanguageServerProtocol.Types

  let inline pos line column : Position = { Line = line; Character = column}
  let inline range p1 p2 = { Start = p1; End = p2 }

  let touchesTests = testList (nameof Range.touches) [
    testCase "completely disjoint ranges don't touch" <| fun _ ->
      let r1 = { Start = pos 1 5; End = pos 3 7}
      let r2 = { Start = pos 5 3; End = pos 7 8}

      let touch = Range.touches r1 r2
      Expect.isFalse touch "Should not touch"
    testCase "range 1 inside range 2 don't touch" <| fun _ ->
      let r1 = { Start = pos 2 5; End = pos 3 7}
      let r2 = { Start = pos 1 3; End = pos 7 8}

      let touch = Range.touches r1 r2
      Expect.isFalse touch "Should not touch"
      let touch = Range.touches r2 r1
      Expect.isFalse touch "Should not touch"
    testCase "two same single positions touch" <| fun _ ->
      let r1 = { Start = pos 2 5; End = pos 2 5}
      let r2 = { Start = pos 2 5; End = pos 2 5}

      let touch = Range.touches r1 r2
      Expect.isTrue touch "Should touch"
    testCase "common End/Start touch" <| fun _ ->
      let r1 = { Start = pos 1 5; End = pos 2 5}
      let r2 = { Start = pos 2 5; End = pos 5 3}

      let touch = Range.touches r1 r2
      Expect.isTrue touch "Should touch"
      let touch = Range.touches r2 r1
      Expect.isTrue touch "Should touch"
    testCase "two same ranges don't touch" <| fun _ ->
      let r1 = { Start = pos 1 5; End = pos 2 5}
      let r2 = { Start = pos 1 5; End = pos 2 5}
      
      let touch = Range.touches r1 r2
      Expect.isFalse touch "Should not touch"
    testCase "strictly overlapping ranges don't touch" <| fun _ ->
      let r1 = { Start = pos 1 5; End = pos 3 7}
      let r2 = { Start = pos 2 3; End = pos 5 8}

      let touch = Range.touches r1 r2
      Expect.isFalse touch "Should not touch"
  ]
  let private overlapsStrictlyTests = testList (nameof Range.overlapsStrictly) [
    testCase "completely distinct ranges on different lines don't overlap" <| fun _ ->
      let r1  = { Start = pos 1 3; End = pos 2 7 }
      let r2 = { Start = pos 4 5; End = pos 7 8 }

      let overlap = Range.overlapsStrictly r1 r2
      Expect.isFalse overlap "Should not overlap"

    testCase "completely distinct ranges on same line don't overlap" <| fun _ ->
      let r1  = { Start = pos 3 3; End = pos 3 7 }
      let r2 = { Start = pos 3 8; End = pos 3 11 }

      let overlap = Range.overlapsStrictly r1 r2
      Expect.isFalse overlap "Should not overlap"

    testCase "ranges with same End/Start overlap" <| fun _ ->
      let r1  = { Start = pos 2 3; End = pos 3 7 }
      let r2 = { Start = pos 3 7; End = pos 4 11 }

      let overlap = Range.overlapsStrictly r1 r2
      Expect.isTrue overlap "Should overlap"

    testCase "ranges with same Start/End overlap" <| fun _ ->
      let r1 = { Start = pos 3 7; End = pos 4 11 }
      let r2  = { Start = pos 2 3; End = pos 3 7 }

      let overlap = Range.overlapsStrictly r2 r1
      Expect.isTrue overlap "Should overlap"

    testCase "position ranges on same position overlap" <| fun _ ->
      let r1 = { Start = pos 2 5; End = pos 2 5}
      let r2 = { Start = pos 2 5; End = pos 2 5}

      let overlap = Range.overlapsStrictly r1 r2
      Expect.isTrue overlap "Should overlap"

    testCase "same ranges overlap" <| fun _ ->
      let r1 = { Start = pos 2 5; End = pos 3 7}
      let r2 = { Start = pos 2 5; End = pos 3 7}

      let overlap = Range.overlapsStrictly r1 r2
      Expect.isTrue overlap "Should overlap"

    testCase "completely inside overlap" <| fun _ ->
      let r1 = { Start = pos 2 5; End = pos 5 7}
      let r2 = { Start = pos 3 1; End = pos 4 3}

      let overlap = Range.overlapsStrictly r1 r2
      Expect.isTrue overlap "Should overlap"
      let overlap = Range.overlapsStrictly r2 r1
      Expect.isTrue overlap "Should overlap"

    testCase "overlapping ranges overlap" <| fun _ ->
      let r1 = { Start = pos 2 5; End = pos 3 7}
      let r2 = { Start = pos 3 1; End = pos 5 6}

      let overlap = Range.overlapsStrictly r1 r2
      Expect.isTrue overlap "Should overlap"
      let overlap = Range.overlapsStrictly r2 r1
      Expect.isTrue overlap "Should overlap"
  ]

  let private overlapsLooselyTests = testList (nameof Range.overlapsLoosely) [
    testCase "completely distinct ranges on different lines don't overlap" <| fun _ ->
      let r1  = { Start = pos 1 3; End = pos 2 7 }
      let r2 = { Start = pos 4 5; End = pos 7 8 }

      let overlap = Range.overlapsLoosely r1 r2
      Expect.isFalse overlap "Should not overlap"

    testCase "completely distinct ranges on same line don't overlap" <| fun _ ->
      let r1  = { Start = pos 3 3; End = pos 3 7 }
      let r2 = { Start = pos 3 8; End = pos 3 11 }

      let overlap = Range.overlapsLoosely r1 r2
      Expect.isFalse overlap "Should not overlap"

    testCase "ranges with same End/Start don't overlap" <| fun _ ->
      let r1  = { Start = pos 2 3; End = pos 3 7 }
      let r2 = { Start = pos 3 7; End = pos 4 11 }

      let overlap = Range.overlapsLoosely r1 r2
      Expect.isFalse overlap "Should not overlap"

    testCase "ranges with same Start/End don't overlap" <| fun _ ->
      let r1 = { Start = pos 3 7; End = pos 4 11 }
      let r2  = { Start = pos 2 3; End = pos 3 7 }

      let overlap = Range.overlapsLoosely r2 r1
      Expect.isFalse overlap "Should not overlap"

    testCase "position ranges on same position don't overlap" <| fun _ ->
      let r1 = { Start = pos 2 5; End = pos 2 5}
      let r2 = { Start = pos 2 5; End = pos 2 5}

      let overlap = Range.overlapsLoosely r1 r2
      Expect.isFalse overlap "Should not overlap"

    testCase "same ranges overlap" <| fun _ ->
      let r1 = { Start = pos 2 5; End = pos 3 7}
      let r2 = { Start = pos 2 5; End = pos 3 7}

      let overlap = Range.overlapsLoosely r1 r2
      Expect.isTrue overlap "Should overlap"

    testCase "completely inside overlap" <| fun _ ->
      let r1 = { Start = pos 2 5; End = pos 5 7}
      let r2 = { Start = pos 3 1; End = pos 4 3}

      let overlap = Range.overlapsLoosely r1 r2
      Expect.isTrue overlap "Should overlap"
      let overlap = Range.overlapsLoosely r2 r1
      Expect.isTrue overlap "Should overlap"

    testCase "overlapping ranges overlap" <| fun _ ->
      let r1 = { Start = pos 2 5; End = pos 3 7}
      let r2 = { Start = pos 3 1; End = pos 5 6}

      let overlap = Range.overlapsLoosely r1 r2
      Expect.isTrue overlap "Should overlap"
      let overlap = Range.overlapsLoosely r2 r1
      Expect.isTrue overlap "Should overlap"
  ]

  let isDisjointStrictlyTests = testList (nameof Range.isDisjointStrictly) [
    testCase "completely distinct ranges on different lines are disjoint" <| fun _ ->
      let r1  = { Start = pos 1 3; End = pos 2 7 }
      let r2 = { Start = pos 4 5; End = pos 7 8 }

      let disjoint = Range.isDisjointStrictly r1 r2
      Expect.isTrue disjoint "Should be disjoint"

    testCase "completely distinct ranges on same line are disjoint" <| fun _ ->
      let r1  = { Start = pos 3 3; End = pos 3 7 }
      let r2 = { Start = pos 3 8; End = pos 3 11 }

      let disjoint = Range.isDisjointStrictly r1 r2
      Expect.isTrue disjoint "Should be disjoint"

    testCase "ranges with same End/Start aren't disjoint" <| fun _ ->
      let r1  = { Start = pos 2 3; End = pos 3 7 }
      let r2 = { Start = pos 3 7; End = pos 4 11 }

      let disjoint = Range.isDisjointStrictly r1 r2
      Expect.isFalse disjoint "Should not be disjoint"

    testCase "ranges with same Start/End aren't disjoint" <| fun _ ->
      let r1 = { Start = pos 3 7; End = pos 4 11 }
      let r2  = { Start = pos 2 3; End = pos 3 7 }

      let disjoint = Range.isDisjointStrictly r2 r1
      Expect.isFalse disjoint "Should not be disjoint"

    testCase "position ranges on same position aren't disjoint" <| fun _ ->
      let r1 = { Start = pos 2 5; End = pos 2 5}
      let r2 = { Start = pos 2 5; End = pos 2 5}

      let disjoint = Range.isDisjointStrictly r1 r2
      Expect.isFalse disjoint "Should not be disjoint"

    testCase "same ranges aren't disjoint" <| fun _ ->
      let r1 = { Start = pos 2 5; End = pos 3 7}
      let r2 = { Start = pos 2 5; End = pos 3 7}

      let disjoint = Range.isDisjointStrictly r1 r2
      Expect.isFalse disjoint "Should not be disjoint"

    testCase "completely inside aren't disjoint" <| fun _ ->
      let r1 = { Start = pos 2 5; End = pos 5 7}
      let r2 = { Start = pos 3 1; End = pos 4 3}

      let disjoint = Range.isDisjointStrictly r1 r2
      Expect.isFalse disjoint "Should not be disjoint"
      let disjoint = Range.isDisjointStrictly r2 r1
      Expect.isFalse disjoint "Should not be disjoint"

    testCase "overlapping ranges aren't disjoint" <| fun _ ->
      let r1 = { Start = pos 2 5; End = pos 3 7}
      let r2 = { Start = pos 3 1; End = pos 5 6}

      let disjoint = Range.isDisjointStrictly r1 r2
      Expect.isFalse disjoint "Should not be disjoint"
      let disjoint = Range.isDisjointStrictly r2 r1
      Expect.isFalse disjoint "Should not be disjoint"
  ]

  let isDisjointLooselyTests = testList (nameof Range.isDisjointLoosely) [
    testCase "completely distinct ranges on different lines are disjoint" <| fun _ ->
      let r1  = { Start = pos 1 3; End = pos 2 7 }
      let r2 = { Start = pos 4 5; End = pos 7 8 }

      let disjoint = Range.isDisjointLoosely r1 r2
      Expect.isTrue disjoint "Should be disjoint"

    testCase "completely distinct ranges on same line are disjoint" <| fun _ ->
      let r1  = { Start = pos 3 3; End = pos 3 7 }
      let r2 = { Start = pos 3 8; End = pos 3 11 }

      let disjoint = Range.isDisjointLoosely r1 r2
      Expect.isTrue disjoint "Should be disjoint"

    testCase "ranges with same End/Start aren't disjoint" <| fun _ ->
      let r1  = { Start = pos 2 3; End = pos 3 7 }
      let r2 = { Start = pos 3 7; End = pos 4 11 }

      let disjoint = Range.isDisjointLoosely r1 r2
      Expect.isTrue disjoint "Should be disjoint"

    testCase "ranges with same Start/End aren't disjoint" <| fun _ ->
      let r1 = { Start = pos 3 7; End = pos 4 11 }
      let r2  = { Start = pos 2 3; End = pos 3 7 }

      let disjoint = Range.isDisjointLoosely r2 r1
      Expect.isTrue disjoint "Should be disjoint"

    testCase "position ranges on same position are disjoint" <| fun _ ->
      let r1 = { Start = pos 2 5; End = pos 2 5}
      let r2 = { Start = pos 2 5; End = pos 2 5}

      let disjoint = Range.isDisjointLoosely r1 r2
      Expect.isTrue disjoint "Should be disjoint"

    testCase "same ranges aren't disjoint" <| fun _ ->
      let r1 = { Start = pos 2 5; End = pos 3 7}
      let r2 = { Start = pos 2 5; End = pos 3 7}

      let disjoint = Range.isDisjointLoosely r1 r2
      Expect.isFalse disjoint "Should not be disjoint"

    testCase "completely inside aren't disjoint" <| fun _ ->
      let r1 = { Start = pos 2 5; End = pos 5 7}
      let r2 = { Start = pos 3 1; End = pos 4 3}

      let disjoint = Range.isDisjointLoosely r1 r2
      Expect.isFalse disjoint "Should not be disjoint"
      let disjoint = Range.isDisjointLoosely r2 r1
      Expect.isFalse disjoint "Should not be disjoint"

    testCase "overlapping ranges aren't disjoint" <| fun _ ->
      let r1 = { Start = pos 2 5; End = pos 3 7}
      let r2 = { Start = pos 3 1; End = pos 5 6}

      let disjoint = Range.isDisjointLoosely r1 r2
      Expect.isFalse disjoint "Should not be disjoint"
      let disjoint = Range.isDisjointLoosely r2 r1
      Expect.isFalse disjoint "Should not be disjoint"
  ]

  let tests = testList (nameof Range) [
    touchesTests
    overlapsStrictlyTests
    overlapsLooselyTests
    isDisjointStrictlyTests
    isDisjointLooselyTests
  ]

module private Text =
  let private trimTripleQuotationTests = testList (nameof Text.trimTripleQuotation) [
    let check input expected =
      let actual = input |> Text.trimTripleQuotation
      Expect.equal actual expected "Invalid trimming"

    testList "normal string" [
      testCase "empty string" <| fun _ ->
        let text = ""
        let expected = text
        check text expected
      testCase "single line with text" <| fun _ ->
        let text = "foo bar"
        let expected = text
        check text expected
      testCase "multi lines with text without indentation" <| fun _ ->
        let text = "foo bar\nlorem ipsum\ndolor\nsit"
        let expected = text
        check text expected
      testCase "leading new line and no indentation" <| fun _ ->
        let text = "\nfoo bar\nlorem ipsum\ndolor\nsit"
        let expected = text.Substring 1
        check text expected
      testCase "single line with indentation" <| fun _ ->
        let text = "    foo bar"
        let expected = text.TrimStart()
        check text expected
      testCase "multi lines with all same indentation" <| fun _ ->
        let text = "    foo bar\n    lorem ipsum\n    dolor sit"
        let expected = "foo bar\nlorem ipsum\ndolor sit"
        check text expected
      testCase "multi lines with all different indentation" <| fun _ ->
        let text = "      foo bar\n    lorem ipsum\n        dolor sit"
        let expected = "  foo bar\nlorem ipsum\n    dolor sit"
        check text expected
      testCase "leading new line and multi line with all different indentation" <| fun _ ->
        let text = "\n      foo bar\n    lorem ipsum\n        dolor sit"
        let expected = "  foo bar\nlorem ipsum\n    dolor sit"
        check text expected
      testCase "multi lines with empty lines" <| fun _ ->
        let text = "    foo bar\n  \n    baz\n      lorem ipsum\n        \n\n    dolor sit\n  \n        ---"
        let expected = "foo bar\n\nbaz\n  lorem ipsum\n    \n\ndolor sit\n\n    ---"
        check text expected
      testCase "last whitespace line gets trimmed" <| fun _ ->
        let text = "foo bar\n    "
        let expected = "foo bar\n"
        check text expected
      testCase "whitespace in last line doesn't get trimmed if there are other chars" <| fun _ ->
        let text = "foo bar\nbaz    "
        let expected = text
        check text expected
      testCase "trim leading nl, indentation, trailing whitespace line" <| fun _ ->
        let text = "\n  foo bar\n\n    baz\n  lorem ipsum\n    dolor sit\n    "
        let expected = "foo bar\n\n  baz\nlorem ipsum\n  dolor sit\n"
        check text expected
    ]
    testList "triple quotation" [
      testCase "fsharp code written on beginning of line" <| fun _ ->
        let text = """
module Foo

let rec handle (a: int) =
if a = 15 then
  failwith "Oh no!"
else
  match a with
  | 42 -> printfn "42"
  | i when i < 42 ->
    handle (i+1)
  | _ ->
    // i > 42
    let a =
      a + 17
    handle a

let a = 42
if a < 12 then
printfn "Result=%A" a
else
handle a
        """
        let expected = """module Foo

let rec handle (a: int) =
if a = 15 then
  failwith "Oh no!"
else
  match a with
  | 42 -> printfn "42"
  | i when i < 42 ->
    handle (i+1)
  | _ ->
    // i > 42
    let a =
      a + 17
    handle a

let a = 42
if a < 12 then
printfn "Result=%A" a
else
handle a
""" // whitespace in last empty line is trimmed, but `\n` is kept
        check text expected
      testCase "fsharp code written with indention to match surrounding code" <| fun _ ->
        let text = """
          module Foo

          let rec handle (a: int) =
            if a = 15 then
              failwith "Oh no!"
            else
              match a with
              | 42 -> printfn "42"
              | i when i < 42 ->
                handle (i+1)
              | _ ->
                // i > 42
                let a =
                  a + 17
                handle a

          let a = 42
          if a < 12 then
            printfn "Result=%A" a
          else
            handle a
        """
        let expected = """module Foo

let rec handle (a: int) =
  if a = 15 then
    failwith "Oh no!"
  else
    match a with
    | 42 -> printfn "42"
    | i when i < 42 ->
      handle (i+1)
    | _ ->
      // i > 42
      let a =
        a + 17
      handle a

let a = 42
if a < 12 then
  printfn "Result=%A" a
else
  handle a
"""
        check text expected
      testCase "trimming already trimmed string doesn't change string" <| fun _ ->
        let text = """
          module Foo

          let rec handle (a: int) =
            if a = 15 then
              failwith "Oh no!"
            else
              match a with
              | 42 -> printfn "42"
              | i when i < 42 ->
                handle (i+1)
              | _ ->
                // i > 42
                let a =
                  a + 17
                handle a

          let a = 42
          if a < 12 then
            printfn "Result=%A" a
          else
            handle a
        """
        let once = text |> Text.trimTripleQuotation
        let twice = once |> Text.trimTripleQuotation
        Expect.equal twice once "trimming should not change a trimmed string"
      testCase "independent trimmings should trim same" <| fun _ ->
        let text = """
          module Foo

          let rec handle (a: int) =
            if a = 15 then
              failwith "Oh no!"
            else
              match a with
              | 42 -> printfn "42"
              | i when i < 42 ->
                handle (i+1)
              | _ ->
                // i > 42
                let a =
                  a + 17
                handle a

          let a = 42
          if a < 12 then
            printfn "Result=%A" a
          else
            handle a
        """
        let a = text |> Text.trimTripleQuotation
        let b = text |> Text.trimTripleQuotation
        Expect.equal b a "both trimmings should be same"
    ]
  ]
  let tests = testList (nameof Text) [
    trimTripleQuotationTests
  ]

let tests = testList (nameof Utils) [
  Range.tests
  Text.tests
]
