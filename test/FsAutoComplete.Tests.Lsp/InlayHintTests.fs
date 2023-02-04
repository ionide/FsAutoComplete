module FsAutoComplete.Tests.InlayHintTests

open Expecto
open System
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open Helpers
open FsToolkit.ErrorHandling
open Utils.ServerTests
open FsAutoComplete.Core
open FsAutoComplete.Lsp

module private FSharpInlayHints =
  open Utils.Server
  open Utils.Tests
  open Utils.Utils
  open Utils.TextEdit

  let private at (text, pos, kind, edits) : InlayHint =
    { Label =
        match kind with
        | InlayHintKind.Type -> InlayHintLabel.String(": " + InlayHints.truncated text)
        | InlayHintKind.Parameter -> InlayHintLabel.String(InlayHints.truncated text + " =")
        | _ -> failwith $"unknown hint kind %O{kind}"
      // this is for truncated text, which we do not currently hit in our tests
      TextEdits = edits |> Option.map List.toArray
      Position = pos
      Kind = Some kind
      Tooltip = None
      PaddingLeft =
        match kind with
        | InlayHintKind.Type -> Some true
        | _ -> None
      PaddingRight =
        match kind with
        | InlayHintKind.Parameter -> Some true
        | _ -> None
      Data = None }

  let private from (text, (line, char), kind) =
    at (text, { Line = line; Character = char }, kind, None)


  let private check' (server: CachedServer) (text: string) (range: Range) (expected: _ array) =
    async {
      let! (doc, diags) = server |> Server.createUntitledDocument text
      use doc = doc // ensure doc gets closed (disposed) after test

      match diags with
      | [||] -> ()
      | diags -> failtest $"Should not have had check errors, but instead had %A{diags}"

      let! actual = Document.inlayHintsAt range doc
      Expect.equal actual expected "Expected the given set of hints"
    }

  let check (server: CachedServer) (documentText: string) (expectedHints: _ list) =
    async {
      let (range, text) =
        documentText
        |> Text.trimTripleQuotation
        |> Cursor.assertExtractRange

      let expected = expectedHints |> List.map from |> Array.ofList
      do! check' server text range expected
    }

  let private extractCursorsInsideRange (text: string) =
    let (text, poss) =
      text
      |> Text.trimTripleQuotation
      |> Cursors.extract

    let range =
      { Start = poss |> List.head
        End = poss |> List.last }

    let poss =
      let count = poss |> List.length
      poss[1 .. (count - 2)]

    (text, range, poss)

  let checkRange (server: CachedServer) (documentText: string) (expectedHints: _ list) =
    async {
      let (text, range, poss) = documentText |> extractCursorsInsideRange

      Expect.equal
        (poss |> List.length)
        (expectedHints |> List.length)
        $"Expected Hints & position cursors to match, but there were {expectedHints |> List.length} expected hints and {poss |> List.length} position cursors"

      let expected =
        List.zip poss expectedHints
        |> List.map (fun (pos, (name, kind, edits)) -> at (name, pos, kind, edits))
        |> List.toArray

      do! check' server text range expected
    }

  let private param (name: string) = (name, InlayHintKind.Parameter, None)
  let private ty (name: string) edits = (name, InlayHintKind.Type, Some edits)

  let tests state =
    serverTestList "F# Inlay Hints" state defaultConfigDto None (fun server ->
      [ testList
          "type hint"
          [ testCaseAsync "can show type hint"
            <| checkRange
                 server
                 """
            $0let f beta$0 = beta + 1$0
            """
                 [ ty
                     "int"
                     [ { Range =
                           { Start = { Line = 0; Character = 6 }
                             End = { Line = 0; Character = 6 } }
                         NewText = "(" }
                       { Range =
                           { Start = { Line = 0; Character = 10 }
                             End = { Line = 0; Character = 10 } }
                         NewText = ": " }
                       { Range =
                           { Start = { Line = 0; Character = 10 }
                             End = { Line = 0; Character = 10 } }
                         NewText = "int" }
                       { Range =
                           { Start = { Line = 0; Character = 10 }
                             End = { Line = 0; Character = 10 } }
                         NewText = ")" } ] ] ]

        testList
          "parameter hint"
          [ testCaseAsync "can show param hint"
            <| checkRange
                 server
                 """
            let f beta = ()
            $0f $042$0
            """
                 [ param "beta" ] ] ])

module private LspInlayHints =
  open Utils.Server
  open Utils.Tests
  open Utils.Utils
  open Utils.TextEdit
  open Ionide.LanguageServerProtocol.Types

  let checkInRange
    (server: CachedServer)
    (text: string)
    (range: Range)
    (validateInlayHints: Document -> string -> InlayHint[] -> Async<unit>)
    =
    async {
      let! (doc, diags) = server |> Server.createUntitledDocument text
      use doc = doc
      Expect.hasLength diags 0 "Should not have had check errors"

      let! hints = doc |> Document.inlayHintsAt range
      let hints = hints |> Array.sortBy (fun h -> h.Position)
      do! validateInlayHints doc text hints
    }

  let private validateHint
    (doc: Document)
    (expectedBase: InlayHint)
    (textAfterEdits: string option)
    (text: string)
    (actual: InlayHint)
    =
    async {
      // Edits are checked by applying -> only check Edits None or Some
      let mkDummyEdits o = o |> Option.bind (fun _ -> Some [||])
      let ignoreData (hint: InlayHint) = { hint with Data = None }

      // we remove edits and tooltips because they are too hard atm.
      let actualWithoutEdits =
        { actual with TextEdits = mkDummyEdits actual.TextEdits; }
        |> ignoreData

      let expectedWithoutExpected =
        { expectedBase with TextEdits = mkDummyEdits textAfterEdits }

      Expect.equal
        actualWithoutEdits
        expectedWithoutExpected
        "Hint doesn't match expectations (Note: `TextEdits` are handled separately. Here just `None` or `Some`)"

      match actual.TextEdits, textAfterEdits with
      | Some edits, Some textAfterEdits ->
        let appliedText =
          text
          |> TextEdits.applyWithErrorCheck (edits |> List.ofArray)
          |> Flip.Expect.wantOk "TextEdits are erroneous"

        Expect.equal appliedText textAfterEdits "Text after applying TextEdits does not match expected"
      | _ -> ()
    }

  let rangeMarker = "$|"

  let checkAllInMarkedRange
    (server: CachedServer)
    (textWithCursors: string)
    (expected: (InlayHint * (string option)) list)
    =
    async {
      let (text, cursors) =
        textWithCursors
        |> Text.trimTripleQuotation
        |> Cursors.extractGroupedWith [| rangeMarker; Cursor.Marker |]

      let range =
        let poss =
          cursors
          |> Map.tryFind rangeMarker
          |> Flip.Expect.wantSome "There should be range markers"

        Expect.hasLength poss 2 "There should be two range markers"
        { Start = poss[0]; End = poss[1] }

      let cursors =
        cursors
        |> Map.tryFind Cursor.Marker
        |> Option.defaultValue []

      Expect.hasLength
        cursors
        (expected.Length)
        $"Number of Cursors & expected hints don't match ({cursors.Length} cursors, {expected.Length} expected hints)"

      let expected =
        List.zip expected cursors
        |> List.map (fun ((hint, textAfterEdits), cursor) ->
          let hint = { hint with Position = cursor }
          (hint, textAfterEdits))

      let validateHints doc (text: string) (hints: InlayHint[]) =
        async {
          Expect.hasLength hints expected.Length "Number of actual hints and expected hints don't match"

          for (actual, (expected, textAfterEdits)) in Seq.zip hints expected do
            do! validateHint doc expected textAfterEdits text actual
        }

      do! checkInRange server text range validateHints
    }

  let private fromCursor: Position = { Line = -1; Character = -1 }

  let private mkBasicHint (kind: InlayHintKind) (pos: Position) (label: string) : InlayHint =
    { Kind = Some kind
      Position = pos
      Label = InlayHintLabel.String label
      TextEdits = None
      Tooltip = None
      PaddingLeft =
        match kind with
        | InlayHintKind.Type -> Some true
        | _ -> None
      PaddingRight =
        match kind with
        | InlayHintKind.Parameter -> Some true
        | _ -> None
      Data = None }

  let paramHint (paramName: string) =
    let label = $"{paramName} ="
    let hint = mkBasicHint InlayHintKind.Parameter fromCursor label
    (hint, None)

  let typeHint (typeName: string) (expectedAfterEdits: string) =
    let label = $": {typeName}"
    let hint = mkBasicHint InlayHintKind.Type fromCursor label
    let expectedAfterEdits = expectedAfterEdits |> Text.trimTripleQuotation
    (hint, Some expectedAfterEdits)

  let truncated (hint: InlayHint, edits) =
    let label =
      match hint.Label with
      | InlayHintLabel.String label -> label
      | _ -> failtestf "invalid label: %A" hint.Label

    let (name, kind) =
      match hint.Kind with
      | Some InlayHintKind.Parameter ->
        let name = label.Substring(0, label.Length - 2)
        name, InlayHintKind.Parameter
      | Some InlayHintKind.Type ->
        let name = label.Substring(2)
        name, InlayHintKind.Type
      | _ -> failtestf "invalid kind: %A" hint.Kind

    let truncatedName = InlayHints.truncated name
    Expect.notEqual truncatedName name "Truncated name should be different from untruncated one"

    let hint =
      { hint with
          Label =
            match kind with
            | InlayHintKind.Parameter -> truncatedName + " ="
            | InlayHintKind.Type -> ": " + truncatedName
            | _ -> failwith "unreachable"
            |> InlayHintLabel.String
          Tooltip = InlayHintTooltip.String name |> Some }

    (hint, edits)

open LspInlayHints

let private paramHintTests state =
  serverTestList "param hints" state defaultConfigDto None (fun server ->
    [ testCaseAsync "can show param hint"
      <| checkAllInMarkedRange
           server
           """
        let f beta = ()
        $|f $042$|
        """
           [ paramHint "beta" ]
      testCaseAsync "can show all param hints"
      <| checkAllInMarkedRange
           server
           """
        let f alpha beta = ()
        $|f $042 $013
        f $01 $02$|
        """
           [ paramHint "alpha"
             paramHint "beta"
             paramHint "alpha"
             paramHint "beta" ]
      testCaseAsync "can get tooltip for truncated hint"
      <| checkAllInMarkedRange
           server
           """
        let f averylongnamenotjustlongbutextremelylongandjusttobesureevenlonger = ()
        $|f $042$|
        """
           [ truncated
             <| paramHint "averylongnamenotjustlongbutextremelylongandjusttobesureevenlonger" ]

      testCaseAsync "doesn't show hint for well-known parameter name"
      <| checkAllInMarkedRange
           server
           """
        $|sprintf "thing %s" "blah" |> ignore$|
        """
           []
      testCaseAsync "doesn't show hints for short parameter names"
      <| checkAllInMarkedRange
           server
           """
        let someFunction s = s
        let noHintForShortParameter = $|someFunction "hi"$|
        """
           []
      testCaseAsync "doesn't show hints for parameter names that match user text"
      <| checkAllInMarkedRange
           server
           """
        let anotherFunction (kind: string) = ()
        let kind = "hi"
        $|anotherFunction kind$|
        """
           []

      testCaseAsync "show: param & variable have different names"
      <| checkAllInMarkedRange
           server
           """
        let f beta = ()
        let alpha = 42

        $|f $0alpha$|
        """
           [ paramHint "beta" ]

      testCaseAsync "hide: param & variable have same name"
      <| checkAllInMarkedRange
           server
           """
        let f alpha = ()
        let alpha = 42

        $|f alpha$|
        """
           []
      testCaseAsync "hide: variable prefix of param"
      <| checkAllInMarkedRange
           server
           """
        let f rangeCoveringExpr = ()
        let range = 2

        $|f range$|
        """
           []
      testCaseAsync "hide: variable postfix of param"
      <| checkAllInMarkedRange
           server
           """
        let f exactRange = ()
        let range = 2

        $|f range$|
        """
           []
      testCaseAsync "show: variable infix of param"
      <| checkAllInMarkedRange
           server
           """
        let f exactRangeCoveringExpr = ()
        let range = 2

        $|f $0range$|
        """
           [ paramHint "exactRangeCoveringExpr" ]
      testCaseAsync "show: variable prefix of param, but no word boundary"
      <| checkAllInMarkedRange
           server
           """
        let f rangecover = ()
        let range = 2

        $|f $0range$|
        """
           [ paramHint "rangecover" ]
      testCaseAsync "show: variable postfix of param, but no word boundary"
      <| checkAllInMarkedRange
           server
           """
        let f exactrange = ()
        let range = 2

        $|f $0range$|
        """
           [ paramHint "exactrange" ]

      testCaseAsync "hide: arg is prefix of param with leading _"
      <| checkAllInMarkedRange
           server
           """
        let f _rangeCoveringExpr = ()
        let range = 2

        $|f range$|
        """
           []
      testCaseAsync "hide: arg is postfix of param with trailing '"
      <| checkAllInMarkedRange
           server
           """
        let f exactRange' = ()
        let range = 2

        $|f range$|
        """
           []
      testCaseAsync "hide: arg is prefix of param with trailing ' in arg"
      <| checkAllInMarkedRange
           server
           """
        let f rangeCoveringExpr = ()
        let range' = 2

        $|f range'$|
        """
           []

      testCaseAsync "hide: param prefix of arg"
      <| checkAllInMarkedRange
           server
           """
        let f range = ()
        let rangeCoveringExpr = 2

        $|f rangeCoveringExpr$|
        """
           []
      testCaseAsync "hide: param postfix of arg"
      <| checkAllInMarkedRange
           server
           """
        let f range = ()
        let exactRange = 2

        $|f exactRange$|
        """
           []

      testCaseAsync "hide: arg is field access with same name as param (upper case start)"
      <| checkAllInMarkedRange
           server
           """
        type Data = {
          Range: int
        }
        let f range = ()
        let data: Data = { Range = 2 }

        $|f data.Range$|
        """
           []
      testCaseAsync "hide: arg is field access with same name as param (lower case start)"
      <| checkAllInMarkedRange
           server
           """
        type Data = {
          range: int
        }
        let f range = ()
        let data: Data = { range = 2 }

        $|f data.range$|
        """
           []
      testCaseAsync "hide: arg is field access prefix of param (upper case start)"
      <| checkAllInMarkedRange
           server
           """
        type Data = {
          Range: int
        }
        let f rangeCoveringExpr = ()
        let data: Data = { Range = 2 }

        $|f data.Range$|
        """
           []
      testCaseAsync "hide: arg is field access, param is prefix of arg"
      <| checkAllInMarkedRange
           server
           """
        type Data = {
          RangeCoveringExpr: int
        }
        let f range = ()
        let data: Data = { RangeCoveringExpr = 2 }

        $|f data.RangeCoveringExpr$|
        """
           []

      testCaseAsync "hide: arg in parens same as param"
      <| checkAllInMarkedRange
           server
           """
        let f alpha = ()
        let alpha = 42

        $|f (alpha)$|
        """
           []
      testCaseAsync "hide: arg in parens and spaces same as param"
      <| checkAllInMarkedRange
           server
           """
        let f alpha = ()
        let alpha = 42

        $|f ( alpha )$|
        """
           []
      testCaseAsync "show: expr including param name in parens"
      <| checkAllInMarkedRange
           server
           """
        let f alpha = ()
        let alpha x = x + 3

        $|f $0(1 |> alpha)$|
        """
           [ paramHint "alpha" ]

      //ENHANCEMENT: detect some common expressions like:
      // * receiving end of pipe: `1 |> alpha`, `alpha <| 1`, `1 |> toAlpha`
      // * last function: `1.ToAlpha()`
      // * often used convert functions: `string alpha`, `alpha.ToString()`
      testCaseAsync "show: any expression"
      <| checkAllInMarkedRange
           server
           """
        let f (alpha, beta, gamma) = ()
        let alpha = 1
        let beta = 2
        let gamma = 2

        $|f ($0string alpha, $0beta.ToString(), $0gamma |> string)$|
        """
           [ paramHint "alpha"
             paramHint "beta"
             paramHint "gamma" ]

      testCaseAsync "hide: unary operator"
      <| checkAllInMarkedRange
           server
           """
        let (~+.) listWithNumbers = List.map ((+) 1) listWithNumbers
        let data = [1..5]

        $|+. data$|
        """
           []
      testCaseAsync "hide: binary operator"
      <| checkAllInMarkedRange
           server
           """
        let (+.) listWithNumbers numberToAdd = List.map ((+) numberToAdd) listWithNumbers
        let data = [1..5]

        $|data +. 5$|
        """
           []

      testCaseAsync "hide: func name ends with param name"
      <| checkAllInMarkedRange
           server
           """
        let validateRange range = ()
        let data = 42

        $|validateRange data$|
        """
           []

      testList
        "special names"
        [ testList
            "mapping"
            [ testCaseAsync "hide: for List"
              <| checkAllInMarkedRange
                   server
                   """
            $|[1..3] |> List.map id$|
            """
                   []
              testCaseAsync "hide: for Array"
              <| checkAllInMarkedRange
                   server
                   """
            $|[|1..3|] |> Array.map id$|
            """
                   []
              testCaseAsync "show: for custom function"
              <| checkAllInMarkedRange
                   server
                   """
            let doStuff mapping = ()
            $|doStuff $042$|
            """
                   [ paramHint "mapping" ] ]
          testList
            "in collections"
            [ testCaseAsync "hide: predicate"
              <| checkAllInMarkedRange
                   server
                   """
            $|[1..3] |> List.filter ((<) 2)$|
            """
                   []
              testCaseAsync "hide: chooser"
              <| checkAllInMarkedRange
                   server
                   """
            $|[1..3] |> List.tryPick Some$|
            """
                   []
              testCaseAsync "hide: value"
              <| checkAllInMarkedRange
                   server
                   """
            $|[1..3] |> List.contains 2$|
            """
                   []
              testCaseAsync "hide: projection"
              <| checkAllInMarkedRange
                   server
                   """
            $|[1..3] |> List.sumBy id$|
            """
                   []
              testCaseAsync "hide: action"
              <| checkAllInMarkedRange
                   server
                   """
            $|[1..3] |> List.iter (printfn "%i")$|
            """
                   []
              testCaseAsync "hide: folder & state"
              <| checkAllInMarkedRange
                   server
                   """
            $|[1..3] |> List.fold (+) 0$|
            """
                   []


              testCaseAsync "hide: list"
              <| checkAllInMarkedRange
                   server
                   """
            $|List.tryLast [1..3]$|
            """
                   []
              testCaseAsync "hide: array"
              <| checkAllInMarkedRange
                   server
                   """
            $|Array.tryLast [|1..3|]$|
            """
                   []
              testCaseAsync "hide: source"
              <| checkAllInMarkedRange
                   server
                   """
            $|Seq.tryLast [1..3]$|
            """
                   []
              testCaseAsync "hide: lists"
              <| checkAllInMarkedRange
                   server
                   """
            $|List.concat []$|
            """
                   []
              testCaseAsync "hide: arrays"
              <| checkAllInMarkedRange
                   server
                   """
            $|Array.concat [||]$|
            """
                   []
              testCaseAsync "hide: sources"
              <| checkAllInMarkedRange
                   server
                   """
            $|Seq.concat []$|
            """
                   [] ]
          testList
            "option"
            [ testCaseAsync "hide: for Option"
              <| checkAllInMarkedRange
                   server
                   """
            $|Option.count (Some 3)$|
            """
                   []
              testCaseAsync "show: for custom function"
              <| checkAllInMarkedRange
                   server
                   """
            let doStuff option = ()
            $|doStuff $042$|
            """
                   [ paramHint "option" ] ]
          testList
            "voption"
            [ testCaseAsync "hide: for ValueOption"
              <| checkAllInMarkedRange
                   server
                   """
            $|ValueOption.count (ValueSome 3)$|
            """
                   []
              testCaseAsync "show: for custom function"
              <| checkAllInMarkedRange
                   server
                   """
            let doStuff voption = ()
            $|doStuff $042$|
            """
                   [ paramHint "voption" ] ]
          testList
            "format"
            [ testCaseAsync "hide: in printfn"
              <| checkAllInMarkedRange
                   server
                   """
            $|printfn "foo"$|
            """
                   []
              testCaseAsync "hide: in sprintf"
              <| checkAllInMarkedRange
                   server
                   """
            $|sprintf "foo"$|
            """
                   []
              testCaseAsync "hide: in Core.Printf"
              <|
              // "normal" printf is in `Microsoft.FSharp.Core.ExtraTopLevelOperators`
              checkAllInMarkedRange
                server
                """
            $|Microsoft.FSharp.Core.Printf.printfn "foo"$|
            """
                []
              testCaseAsync "show: for custom function"
              <| checkAllInMarkedRange
                   server
                   """
            let doStuff format = ()
            $|doStuff $042$|
            """
                   [ paramHint "format" ] ] ]

      testList
        "tuple param"
        [
          // Cannot get param name for type tuple,
          // instead just unnamed params for each tuple element.
          // see dotnet/fsharp#10441
          ptestCaseAsync "can show param hint for tuple param without individual names"
          <| checkAllInMarkedRange
               server
               """
          let f (tupleParam: _*_) = ()
          $|f $0(1,2)$|
          """
               [ paramHint "tupleParam" ]
          ptestCaseAsync "can show param hint for tuple-var param without individual names"
          <| checkAllInMarkedRange
               server
               """
          let f (tupleParam: _*_) = ()
          let myTuple = (1,2)
          $|f $0myTuple$|
          """
               [ paramHint "tupleParam" ]
          testCaseAsync "can show param hint for generic param with tuple args"
          <|
          // Note: unlike above param isn't a tuple -> can get param name
          checkAllInMarkedRange
            server
            """
          let f (tupleParam: 'a) = ()
          $|f $0(1,2)$|
          """
            [ paramHint "tupleParam" ]
          testCaseAsync "can show param hint for generic param with tuple var"
          <| checkAllInMarkedRange
               server
               """
          let f (tupleParam: 'a) = ()
          let myTuple = (1,2)
          $|f $0myTuple$|
          """
               [ paramHint "tupleParam" ]
          testCaseAsync "can show param hint for struct tuple param without individual name"
          <|
          // Note: unlike normal tuple, FCS provides name for struct tuple
          checkAllInMarkedRange
            server
            """
          let f (tupleParam: struct (_*_)) = ()
          $|f $0(struct (1,2))$|
          """
            [ paramHint "tupleParam" ]
          testCaseAsync "can show param hint for struct tuple-var param without individual name"
          <| checkAllInMarkedRange
               server
               """
          let f (tupleParam: struct (_*_)) = ()
          let myTuple = struct (1,2)
          $|f $0myTuple$|
          """
               [ paramHint "tupleParam" ]


          testCaseAsync "can show param hints for tuple param with individual names"
          <| checkAllInMarkedRange
               server
               """
          let f (alpha, beta) = ()
          $|f ($01, $02)$|
          """
               [ paramHint "alpha"; paramHint "beta" ]
          testCaseAsync "can show param hint for tuple-var param with individual names"
          <| checkAllInMarkedRange
               server
               """
          let f (alpha, beta) = ()
          let myTuple = (1,2)
          $|f $0myTuple$|
          """
               [ paramHint "(alpha,beta)" ]

          testCaseAsync "lambda"
          <| checkAllInMarkedRange
               server
               """
          let f (lambdaParens, lambdaNoParens) = ()

          $|
          f ($0(fun (v: int) -> v + 1), $0fun (v: int) -> v + 1)
          $|
          """
               [ paramHint "lambdaParens"
                 paramHint "lambdaNoParens" ]
          testCaseAsync "lambda without types"
          <| checkAllInMarkedRange
               server
               """
          let f (lambdaParens, lambdaNoParens) = ()

          $|f ($0(fun v$0 -> v + 1), $0fun v$0 -> v + 1)$|
          """
               [ paramHint "lambdaParens"
                 // justTypeHint "int"
                 typeHint
                   "int"
                   """
              let f (lambdaParens, lambdaNoParens) = ()

              f ((fun (v: int) -> v + 1), fun v -> v + 1)
              """
                 paramHint "lambdaNoParens"
                 // justTypeHint "int"
                 typeHint
                   "int"
                   """
              let f (lambdaParens, lambdaNoParens) = ()

              f ((fun v -> v + 1), fun (v: int) -> v + 1)
              """ ]

          // ionide/ionide-vscode-fsharp#1714
          testCaseAsync "ionide/ionide-vscode-fsharp#1714"
          <| checkAllInMarkedRange
               server
               """
          let inlayHintsTest firstParam tupleParam lastParam = ()
          $|inlayHintsTest $0"firstParam" $0("t1", "t2") $0"lastParam"$|
          """
               [ paramHint "firstParam"
                 paramHint "tupleParam"
                 paramHint "lastParam" ]

          testList
            "can assign param name to correct input"
            [ testCaseAsync "mix"
              <| checkAllInMarkedRange
                   server
                   """
            let f alpha (beta,gamma) delta (epsilon,zeta) =
              let (d1, d2) = delta
              alpha + beta + gamma + d1 + d2 + epsilon + zeta
            let ef = (6,7)

            $|f $01 ($02,$03) (4,5) $0ef$|
            """
                   [ paramHint "alpha"
                     paramHint "beta"
                     paramHint "gamma"
                     // no delta: FCS doesn't provide name for tuple param (but instead unnamed for each tuple element)
                     paramHint "(epsilon,zeta)" ]
              testCaseAsync "all tuple"
              <| checkAllInMarkedRange
                   server
                   """
            let f
              (alpha, beta, gamma)
              (delta, epsilon)
              (zeta, eta, theta)
              (iota, kappa)
              (lambda, muValue, nuValue)
              =
              ()
            let deValue = (4,5)
            let tValue = 8
            $|
            f
              ($01, $02, $03)
              $0deValue
              ($06, $0"7", $0tValue)
              ($0Some 9, $0Ok 10)
              ($0(fun (v: int) -> v + 1), $012.0, $0fun (v: int) -> v + 1)
            $|
            """
                   [ paramHint "alpha"
                     paramHint "beta"
                     paramHint "gamma"
                     paramHint "(delta,epsilon)"
                     paramHint "zeta"
                     paramHint "eta"
                     paramHint "theta"
                     paramHint "iota"
                     paramHint "kappa"
                     paramHint "lambda"
                     paramHint "muValue"
                     paramHint "nuValue" ]
              testCaseAsync "alternate tuple & var"
              <| checkAllInMarkedRange
                   server
                   """
            let f (alpha, beta) gamma (delta, epsilon) zeta (eta, theta) iota (kappa, lambda) = ()

            let v = (1,1)

            $|
            f $0v $0(1,1) ($01,$01) $0v ($01,$01) $0(1,1) ($01, $01) = ()
            $|
            """
                   [ paramHint "(alpha,beta)"
                     paramHint "gamma"
                     paramHint "delta"
                     paramHint "epsilon"
                     paramHint "zeta"
                     paramHint "eta"
                     paramHint "theta"
                     paramHint "iota"
                     paramHint "kappa"
                     paramHint "lambda" ] ] ]

      testList
        "unit"
        [ testCaseAsync "doesn't show param hint for unnamed unit param"
          <| checkAllInMarkedRange
               server
               """
          let f () = ()

          $|f ()$|
          """
               []
          testCaseAsync "does show param hint for named unit param"
          <| checkAllInMarkedRange
               server
               """
          let f (myValue: unit) = ()

          $|f $0()$|
          """
               [ paramHint "myValue" ]

          testCaseAsync "does show param hint for unit arg"
          <| checkAllInMarkedRange
               server
               """
          let f myValue = ()

          $|f $0()$|
          """
               [ paramHint "myValue" ] ]

      testCaseAsync "doesn't show param hint for wildcard param"
      <| checkAllInMarkedRange
           server
           """
        let f _ = ()

        $|f 42$|
        """
           []

      testCaseAsync "Show param hint for type arg"
      <| checkAllInMarkedRange
           server
           """
        type Parser(jsonText: string) = member _.Parse(decoder: string -> string) = decoder jsonText
        $|let parse (decoder: string -> string) (str: string) =  Parser$0(str).Parse(decoder)$|
        """
           [ paramHint "jsonText"]

      testList
        "operator"
        [ testList
            "pipe"
            [ testCaseAsync "doesn't show param for func with args piped in"
              <| checkAllInMarkedRange
                   server
                   """
            let f tupleParam = ()
            $|
            f <| 2
            $|
            """
                   []
              testCaseAsync "doesn't show param for pipe, but for function args"
              <| checkAllInMarkedRange
                   server
                   """
            let f1 someArgs = someArgs
            let f2 someValue someArgs = someArgs |> List.map (fun v -> v + someValue)
            let f3 someFunction someArgs = someArgs |> List.iter someFunction

            $|
            [1..4]
            |> f1
            |> f2 $042
            |> f3 $0(printfn "number %i")
            $|
            """
                   [ paramHint "someValue"
                     paramHint "someFunction" ]
              testCaseAsync "doesn't show param for piped in, but for function args"
              <| checkAllInMarkedRange
                   server
                   """
            let f1 someArgs = someArgs
            let f2 someValue someArgs = someArgs |> List.map (fun v -> v + someValue)
            let f3 someFunction someArgs = someArgs |> List.iter someFunction

            $|
            f3 $0(printfn "number %i") <| (f2 $042 <| (f1 <| [1..4]))
            $|
            """
                   [ paramHint "someFunction"
                     paramHint "someValue" ] ]
          testCaseAsync "doesn't show param for unary operator"
          <| checkAllInMarkedRange
               server
               """
          let (~+) someValue = ()
          $|+42$|
          """
               []
          testCaseAsync "doesn't show param for binary operator"
          <| checkAllInMarkedRange
               server
               """
          let (+) someValue someOtherValue = ()
          $|42 + 13$|
          """
               [] ]

      ptestCaseAsync "can show param for method"
      <| checkAllInMarkedRange
           server
           """
        $|System.Environment.GetEnvironmentVariable $0"Blah"
        |> ignore$|
        """
           [ paramHint "variable" ]

      testCaseAsync "can show param for name in backticks"
      <| checkAllInMarkedRange
           server
           """
        let f ``foo bar`` = ``foo bar`` + 1
        $|f $042$|
        |> ignore
        """
           [ paramHint "``foo bar``" ] ])

let private typeHintTests state =
  serverTestList "type hints" state defaultConfigDto None (fun server ->
    [ testCaseAsync "can show type hint"
      <| checkAllInMarkedRange
           server
           """
        $|let f beta$0 = beta + 1$|
        """
           [ typeHint
               "int"
               """
            let f (beta: int) = beta + 1
            """ ]
      testCaseAsync "can show all type hints"
      <| checkAllInMarkedRange
           server
           """
        let fromString (v: string) = int v
        let fromFloat (v: float) = int v
        $|let f alpha$0 beta$0 gamma$0 $|= (fromFloat alpha) + beta + (fromString gamma) + 1
        """
           [ typeHint
               "float"
               """
            let fromString (v: string) = int v
            let fromFloat (v: float) = int v
            let f (alpha: float) beta gamma = (fromFloat alpha) + beta + (fromString gamma) + 1
            """
             typeHint
               "int"
               """
            let fromString (v: string) = int v
            let fromFloat (v: float) = int v
            let f alpha (beta: int) gamma = (fromFloat alpha) + beta + (fromString gamma) + 1
            """
             typeHint
               "string"
               """
            let fromString (v: string) = int v
            let fromFloat (v: float) = int v
            let f alpha beta (gamma: string) = (fromFloat alpha) + beta + (fromString gamma) + 1
            """ ]
      testCaseAsync "let-bound function parameter type hints"
      <| checkAllInMarkedRange
           server
           """
        $|let tryFindFile p$0 = p + "hi"$|
        """
           [ typeHint
               "string"
               """
            let tryFindFile (p: string) = p + "hi"
            """ ]
      testCaseAsync "value let binding type hint"
      <| checkAllInMarkedRange
           server
           """
        $|let s$0 = "hi"$|
        """
           [ typeHint
               "string"
               """
            let s: string = "hi"
            """ ]
      testCaseAsync "no type hint for an explicitly-typed binding"
      <| checkAllInMarkedRange
           server
           """
        $|let s: string = "hi"$|
        """
           []
      testCaseAsync "long type hint gets truncated"
      <| checkAllInMarkedRange
           server
           """
        $|let t$0 = Some (Some (Some (Some (Some (Some (Some (Some (Some (Some (Some (Some (Some (Some (Some ()))))))))))))))$|
        """
           [ truncated
             <| typeHint
                  "unit option option option option option option option option option option option option option option option"
                  """
            let t: unit option option option option option option option option option option option option option option option = Some (Some (Some (Some (Some (Some (Some (Some (Some (Some (Some (Some (Some (Some (Some ()))))))))))))))
            """ ]


      testCaseAsync "can show type for generic actual type"
      <| checkAllInMarkedRange
           server
           """
        open System.Collections.Generic
        $|let list$0 = List<int>()$|
        list.Add 2
        """
           [ typeHint
               "List<int>"
               """
            open System.Collections.Generic
            let list: List<int> = List<int>()
            list.Add 2
            """ ]
      ptestCaseAsync "can show type hint for nested inside generic actual type"
      <|
      // see dotnet/fsharp#13202
      checkAllInMarkedRange
        server
        """
        open System.Collections.Immutable
        $|let arr$0 = ImmutableArray.CreateBuilder()$|
        arr.Add 2
        """
        [
          //Currently: `ImmutableArray`1.Builder<int>`
          typeHint
            "ImmutableArray<int>.Builder"
            """
            open System.Collections.Immutable
            let arr: ImmutableArray<int>.Builder = ImmutableArray.CreateBuilder()
            arr.Add 2
            """ ] ])

let private mixedHintTests state =
  serverTestList "inlay hints" state defaultConfigDto None (fun server ->
    [ testCaseAsync "can show all hints"
      <| checkAllInMarkedRange
           server
           """
        $|open System
        let f alpha$0 beta$0 =
          let beta$0 = Int32.Parse beta
          let value$0 = alpha + beta + 2
          value * 2
        let res$0 = f $042 $0"13" + f $01 $0"2"$|
        """
           [ typeHint
               "int"
               """
            open System
            let f (alpha: int) beta =
              let beta = Int32.Parse beta
              let value = alpha + beta + 2
              value * 2
            let res = f 42 "13" + f 1 "2"
            """
             typeHint
               "string"
               """
            open System
            let f alpha (beta: string) =
              let beta = Int32.Parse beta
              let value = alpha + beta + 2
              value * 2
            let res = f 42 "13" + f 1 "2"
            """
             typeHint
               "int"
               """
            open System
            let f alpha beta =
              let beta: int = Int32.Parse beta
              let value = alpha + beta + 2
              value * 2
            let res = f 42 "13" + f 1 "2"
            """
             typeHint
               "int"
               """
            open System
            let f alpha beta =
              let beta = Int32.Parse beta
              let value: int = alpha + beta + 2
              value * 2
            let res = f 42 "13" + f 1 "2"
            """
             typeHint
               "int"
               """
            open System
            let f alpha beta =
              let beta = Int32.Parse beta
              let value = alpha + beta + 2
              value * 2
            let res: int = f 42 "13" + f 1 "2"
            """
             paramHint "alpha"
             paramHint "beta"
             paramHint "alpha"
             paramHint "beta" ] ])

let private inlayHintTests state =
  testList
    "LSP InlayHints"
    [ paramHintTests state
      typeHintTests state
      mixedHintTests state ]

module InlayHintAndExplicitType =
  open Utils.Server
  open Utils.Tests
  open Utils.Utils
  open Utils.TextEdit

  let tryGetInlayHintAt pos doc =
    async {
      let allRange =
        { Start = { Line = 0; Character = 0 }
          End = { Line = 1234; Character = 1234 } }

      let! hints = doc |> Document.inlayHintsAt allRange
      return hints |> Array.tryFind (fun h -> h.Position = pos)
    }

  let tryGetCodeFixAt pos doc =
    async {
      let range = { Start = pos; End = pos }
      let! codeFixes = doc |> Document.codeActionAt [||] range

      return
        match codeFixes with
        | None -> None
        | Some (CodeActions codeActions) ->
          codeActions
          |> Array.tryFind (fun ca -> ca.Title = CodeFix.AddExplicitTypeAnnotation.title)
        | Some _ -> None
    }

  let private checkInlayHintAndCodeFix
    (server: CachedServer)
    (textWithCursor: string)
    (validateInlayHint: (Document * string * Position) -> InlayHint option -> Async<unit>)
    (validateCodeFix: (Document * string * Position) -> CodeAction option -> Async<unit>)
    =
    async {
      // Cursors:
      // * $0: normal cursor
      // * $I: optional insert inlay hint here
      //  * when not specified: location of $0
      let text = textWithCursor |> Text.trimTripleQuotation
      let (cursor, text) = Cursor.assertExtractPosition text

      let (hintPos, text) =
        Cursor.tryExtractPositionMarkedWithAnyOf [| "$I" |] text
        |> Option.map (fun ((_, pos), text) -> (pos, text))
        |> Option.defaultValue (cursor, text)

      let! (doc, diags) = server |> Server.createUntitledDocument text
      use doc = doc
      Expect.hasLength diags 0 "Document should not have had errors"

      let! hint = doc |> tryGetInlayHintAt hintPos
      let! codeFix = doc |> tryGetCodeFixAt cursor

      do! validateInlayHint (doc, text, hintPos) hint
      do! validateCodeFix (doc, text, cursor) codeFix
    }

  type Expected =
    /// Edit for InlayHint as well as AddExplicitType CodeFix
    | Edit of textAfterEdits: string
    /// Edit for AddExplicitType CodeFix,
    /// but no InlayHint
    | JustCodeFix of textAfterEdits: string
    /// Just display of InlayHint, but no Edits or CodeFix
    ///
    /// Label must not contain leading `:` (& following space)
    | JustInlayHint of label: string
    /// Neither InlayHint nor CodeFix
    | Nothing

  let check (recheckAfterAppliedTextEdits: bool) (server: CachedServer) (textWithCursor: string) (expected: Expected) =
    let recheckAfterAppliedEdits
      (doc: Document)
      (cursorBeforeEdits: Position)
      (edits: TextEdit list)
      (textAfterEdits: string)
      =
      async {
        let! (doc, _) = Server.createUntitledDocument textAfterEdits (doc.Server |> Async.singleton)
        use doc = doc
        let pos = cursorBeforeEdits |> Cursor.afterEdits edits
        let! inlayHint = doc |> tryGetInlayHintAt pos
        Expect.isNone inlayHint "There shouldn't be a inlay hint after inserting inlay hint text edit"
        let! codeFix = doc |> tryGetCodeFixAt pos
        Expect.isNone codeFix "There shouldn't be a code fix after inserting code fix text edit"
      }

    let rec validateInlayHint (doc, text, pos) (inlayHint: InlayHint option) =
      async {
        match expected with
        | JustCodeFix _
        | Nothing -> Expect.isNone inlayHint "There should be no Inlay Hint"
        | JustInlayHint label ->
          let inlayHint = Expect.wantSome inlayHint "There should be a Inlay Hint"

          let actual =
            match inlayHint.Label with
            | InlayHintLabel.String lbl -> lbl
            | InlayHintLabel.Parts parts ->
              parts
              |> Array.map (fun part -> part.Value)
              |> String.concat ""

          let actual =
            let actual = actual.TrimStart()

            if actual.StartsWith ':' then
              actual.Substring(1).TrimStart()
            else
              actual

          Expect.equal actual label "Inlay Hint Label is incorrect"

          let edits = inlayHint.TextEdits |> Option.defaultValue [||]
          Expect.isEmpty edits "There should be no text edits"
        | Edit textAfterEdits ->
          let inlayHint = Expect.wantSome inlayHint "There should be a Inlay Hint"

          let edits =
            Expect.wantSome inlayHint.TextEdits "There should be TextEdits"
            |> List.ofArray

          let actual =
            text
            |> TextEdits.apply edits
            |> Flip.Expect.wantOk "TextEdits should succeed"

          let expected = textAfterEdits |> Text.trimTripleQuotation
          Expect.equal actual expected "Text after TextEdits is incorrect"

          if recheckAfterAppliedTextEdits then
            do! recheckAfterAppliedEdits doc pos edits actual
      }

    let validateCodeFix (doc: Document, text, pos) (codeFix: CodeAction option) =
      async {
        match expected with
        | JustInlayHint _
        | Nothing -> Expect.isNone codeFix "There should be no Code Fix"
        | JustCodeFix textAfterEdits
        | Edit textAfterEdits ->
          let codeFix = Expect.wantSome codeFix "There should be a Code Fix"

          let edits =
            Expect.wantSome codeFix.Edit "There should be TextEdits"
            |> WorkspaceEdit.tryExtractTextEditsInSingleFile (doc.VersionedTextDocumentIdentifier)
            |> Flip.Expect.wantOk "WorkspaceEdit should be valid"

          let actual =
            text
            |> TextEdits.apply edits
            |> Flip.Expect.wantOk "TextEdits should succeed"

          let expected = textAfterEdits |> Text.trimTripleQuotation
          Expect.equal actual expected "Text after TextEdits is incorrect"

          if recheckAfterAppliedTextEdits then
            do! recheckAfterAppliedEdits doc pos edits actual
      }

    checkInlayHintAndCodeFix server textWithCursor validateInlayHint validateCodeFix

open InlayHintAndExplicitType

/// Test Inlay Type Hints & Add Explicit Type Code Fix:
/// * At most locations Type Hint & Code Fix should be valid at same location and contain same TextEdit -> checked together
/// * Checked by applying TextEdits
/// * Additional test: After applying TextEdit (-> add type annotation), neither Type Hint nor Code Fix are available any more
///
/// vs. `explicitTypeInfoTests`:
/// * `explicitTypeInfoTests`:
///   * Does Type Annotation exists
///   * Is Type Annotation valid
///   * Are parens required
///   * Where to parens go
///   * low-level -> doesn't use LSP Server, but instead calls `tryGetExplicitTypeInfo` directly
/// * `inlayTypeHintAndAddExplicitTypeTests`
///   * Is (and should) Inlay Type Hint be displayed here
///   * Does "Add Explicit Type" Code Fix get triggered
///   * Produce both correct Text Edits
///   * Are both not triggered any more after Text Edit
///   * high-level: LSP Server with `textDocument/inlayHint` & `textDocument/codeAction` commands
///
/// vs. `typeHintTests`:
/// * `typeHintTests`:
///   * Tests all properties of InlayHint like label, location
///   * Checks all InlayHint in a certain range (including their absent)
/// * `inlayTypeHintAndAddExplicitTypeTests`
///   * InlayHint at single location
///   * Tests just TextEdits
///   * Additional checks "Add Explicit Type" Code Fix
///
///
/// ->
/// * `explicitTypeInfoTests`: test type annotation
/// * `inlayTypeHintAndAddExplicitTypeTests`: test type annotation edit and Inlay Hint existence (vs. "Add Explicit Type")
///   * Tests when inlay hints should not be displayed should go here
/// * `typeHintTests`: test data in InlayHint (like label)
let private inlayTypeHintAndAddExplicitTypeTests state =
  let check = check true
  let checkAll server pre post = check server pre (Edit post)

  serverTestList "LSP InlayHint (type) & AddExplicitType" state defaultConfigDto None (fun server ->
    [ testCaseAsync "can add type annotation"
      <| checkAll
           server
           """
        let value$0 = 42
        """
           """
        let value: int = 42
        """
      testCaseAsync "neither Type Hint nor Code Fix when type annotation already exists"
      <| check
           server
           """
        let value$0: int = 42
        """
           Nothing

      testList
        "hide type hint"
        [ testCaseAsync "CodeFix for func variable, but no type hint"
          <| check
               server
               """
          let f$0 = fun a -> a + 1
          """
               (JustCodeFix
                 """
            let f: int -> int = fun a -> a + 1
            """)

          //ENHANCEMENT: add cases when Inlay Type Hint should not trigger (like `let str = "..."`?)
          ] ])

let tests state =
  testList
    (nameof InlayHint)
    [ FSharpInlayHints.tests state
      inlayHintTests state
      inlayTypeHintAndAddExplicitTypeTests state ]


open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open Utils.TextEdit
open Utils.Utils
open FsAutoComplete.Core.InlayHints
open FSharp.UMX
open FsAutoComplete.LspHelpers
open Ionide.LanguageServerProtocol.Types

let explicitTypeInfoTests =
  let file = "test.fsx"
  let checker = lazy (FSharpChecker.Create())

  let getAst input =
    async {
      let checker = checker.Value
      // Get compiler options for the 'project' implied by a single script file
      let! projOptions, diagnostics = checker.GetProjectOptionsFromScript(file, input, assumeDotNetFramework = false)
      // Expect.isEmpty diagnostics "There should be no diagnostics"
      Expect.hasLength diagnostics 0 "There should be no diagnostics"

      let parsingOptions, errors =
        checker.GetParsingOptionsFromProjectOptions(projOptions)
      // Expect.isEmpty errors "There should be no errors"
      Expect.hasLength errors 0 "There should be no errors"

      // Run the first phase (untyped parsing) of the compiler
      let! parseFileResults = checker.ParseFile(file, input, parsingOptions)
      // Expect.isEmpty parseFileResults.Diagnostics "There should be no parse diagnostics"
      Expect.hasLength parseFileResults.Diagnostics 0 "There should be no parse diagnostics"

      return parseFileResults.ParseTree
    }

  let getExplicitTypeInfo (pos: Position) (text: string) =
    async {
      let text = NamedText(UMX.tag file, text)
      let! ast = getAst text

      let pos = protocolPosToPos pos

      let explTy = InlayHints.tryGetExplicitTypeInfo (text, ast) pos
      return explTy
    }

  let fromCursor = Position.pos0
  let fromCursors = Range.Zero

  let fromCursorAndInsert =
    Range.mkRange fromCursors.FileName (Position.mkPos 12345 12345) (Position.mkPos 12345 12345)

  let cursor = "$0"
  let (openParenCursor, closeParenCursor) = "$(", "$)"
  let insertCursor = "$I"
  let identCursor = "$|"

  let markers =
    [| cursor
       openParenCursor
       closeParenCursor
       insertCursor
       identCursor |]

  let wantsExactlyOne msg vs =
    Expect.hasLength vs 1 msg
    vs |> List.exactlyOne

  let extractCursor (marker: string) cursors =
    let pos =
      cursors
      |> List.filter (fst >> (=) marker)
      |> List.map snd
      |> wantsExactlyOne $"There should be exactly one cursor marker '{marker}'"

    let cursors = cursors |> List.filter (fst >> (<>) marker)
    (pos, cursors)

  let toFcsPos (pos, cursors) =
    let pos = protocolPosToPos pos
    (pos, cursors)

  /// Cursors:
  /// * $0: Cursor
  /// * $(: Open Paren
  /// * $): Close Paren
  /// * $I: Insert Pos
  /// * $|: Ident range
  let testExplicitType' (textWithCursors: string) (expected: ExplicitType option) =
    async {
      let (text, cursors) =
        textWithCursors
        |> Text.trimTripleQuotation
        |> Cursors.extractWith markers

      let (pos, cursors) = cursors |> extractCursor cursor

      let updateExpected cursors (expected: ExplicitType) =
        let expected, cursors =
          match expected with
          | ExplicitType.Debug _ -> expected, cursors
          | ExplicitType.Invalid -> ExplicitType.Invalid, cursors
          | ExplicitType.Exists -> ExplicitType.Exists, cursors
          | ExplicitType.Missing ({ Ident = ident
                                    InsertAt = insertAt
                                    Parens = parens } as data) ->
            let insertAt, cursors =
              if insertAt = fromCursor then
                cursors |> extractCursor insertCursor |> toFcsPos
              else
                insertAt, cursors

            let (parens, cursors) =
              let extractParensRange cursors =
                let (openParen, cursors) =
                  cursors
                  |> extractCursor openParenCursor
                  |> toFcsPos

                let (closeParen, cursors) =
                  cursors
                  |> extractCursor closeParenCursor
                  |> toFcsPos

                let range = Range.mkRange file openParen closeParen
                range, cursors

              match parens with
              | Parens.Exist range when range = fromCursors ->
                let range, cursors = extractParensRange cursors
                (Parens.Exist range), cursors
              | Parens.Optional range when range = fromCursors ->
                let range, cursors = extractParensRange cursors
                (Parens.Optional range), cursors
              | Parens.Required range when range = fromCursors ->
                let range, cursors = extractParensRange cursors
                (Parens.Required range), cursors
              | _ -> parens, cursors

            let ident, cursors =
              if ident = fromCursorAndInsert then
                let range = Range.mkRange file (protocolPosToPos pos) insertAt
                (range, cursors)
              elif ident = fromCursors then
                let range =
                  let poss =
                    cursors
                    |> List.filter (fst >> ((=) identCursor))
                    |> List.map snd

                  Expect.hasLength poss 2 "There should be exactly 2 cursors for ident"
                  let (start, fin) = (protocolPosToPos poss[0], protocolPosToPos poss[1])
                  Range.mkRange file start fin

                let cursors = cursors |> List.filter (fst >> ((<>) identCursor))
                (range, cursors)
              else
                (ident, cursors)

            let data =
              { data with
                  Ident = ident
                  InsertAt = insertAt
                  Parens = parens }

            let updated = ExplicitType.Missing data
            updated, cursors

        Expect.hasLength cursors 0 "There are unused cursors!"
        expected

      let expected = expected |> Option.map (updateExpected cursors)

      let! actual = getExplicitTypeInfo pos text
      Expect.equal actual expected "Incorrect Explicit Type Info"
    }

  let testExplicitType textWithCursor expected =
    testExplicitType' textWithCursor (Some expected)

  testSequenced
  <| testList
    "detect type and parens"
    [ testList
        "Expr"
        [ testList
            "For loop"
            [
              // for loop is special: no pattern, no simple pattern, just ident
              // -> no type allowed
              testCaseAsync "explicit type is invalid"
              <| testExplicitType
                   """
            for $0i = 1 to 5 do
              ()
            """
                   ExplicitType.Invalid ] ]
      testSequenced <|
      testList
        "Bindings"
        [ testList
            "simple let"
            [ testCaseAsync "let value = 1"
              <| testExplicitType
                   """
            let $($0value$I$) = 1
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Optional fromCursors
                       SpecialRules = [] })
              testCaseAsync "let (value) = 1"
              <| testExplicitType
                   """
            let ($($0value$I$)) = 1
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Exist fromCursors
                       SpecialRules = [] })
              testCaseAsync "let value: int = 1"
              <| testExplicitType
                   """
            let $0value: int = 1
            """
                   (ExplicitType.Exists)
              testCaseAsync "let (value: int) = 1"
              <| testExplicitType
                   """
            let ($0value: int) = 1
            """
                   (ExplicitType.Exists)
              testCaseAsync "let (value): int = 1"
              <| testExplicitType
                   """
            let ($0value): int = 1
            """
                   (ExplicitType.Exists)
              testCaseAsync "let [<Attr>] value = 1"
              <| testExplicitType
                   """
            type Attr() =
              inherit System.Attribute()
            let [<Attr>] $($0value$I$) = 1
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Optional fromCursors
                       SpecialRules = [] })
              // Attributes are not allowed inside parens: `let ([<Attr>] value) = ...` is invalid!
              testCaseAsync "let [<Attr>] (value) = 1"
              <| testExplicitType
                   """
            type Attr() =
              inherit System.Attribute()
            let [<Attr>] ($($0value$I$)) = 1
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Exist fromCursors
                       SpecialRules = [] })
              testCaseAsync "let [<Attr>] value: int = 1"
              <| testExplicitType
                   """
            type Attr() =
              inherit System.Attribute()
            let [<Attr>] $0value: int = 1
            """
                   (ExplicitType.Exists)
              testCaseAsync "let [<Attr>] (value: int) = 1"
              <| testExplicitType
                   """
            type Attr() =
              inherit System.Attribute()
            let [<Attr>] ($0value: int) = 1
            """
                   (ExplicitType.Exists)
              testCaseAsync "let private value = 1"
              <| testExplicitType
                   """
            let $(private $0value$I$) = 1
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Optional fromCursors
                       SpecialRules = [] })
              testCaseAsync "let private value: int = 1"
              <| testExplicitType
                   """
            let private $0value: int = 1
            """
                   (ExplicitType.Exists) ]
          testList
            "let with multiple vars"
            [ testCaseAsync "let value1, value2, value3 = (1,2,3)"
              <| testExplicitType
                   """
            let value1, $($0value2$I$), value3 = (1,2,3)
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Required fromCursors
                       SpecialRules = [] })
              testCaseAsync "let (value1, value2, value3) = (1,2,3)"
              <| testExplicitType
                   """
            let (value1, $($0value2$I$), value3) = (1,2,3)
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Optional fromCursors
                       SpecialRules = [] })
              testCaseAsync "let (value1, value2: int, value3) = (1,2,3)"
              <| testExplicitType
                   """
            let (value1, $0value: int, value3) = (1,2,3)
            """
                   (ExplicitType.Exists) ]

          testList
            "use"
            [ testCaseAsync "use value = ..."
              <| testExplicitType
                   """
            let d = { new System.IDisposable with
                member _.Dispose() = ()
            }
            let _ =
              use $($0value$I$) = d
              ()
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Optional fromCursors
                       SpecialRules = [] })
              testCaseAsync "use value: IDisposable = ..."
              <| testExplicitType
                   """
            open System
            let d = { new System.IDisposable with
                member _.Dispose() = ()
            }
            let _ =
              use $0value: IDisposable = d
              ()
            """
                   (ExplicitType.Exists) ]

          testList
            "let!"
            [ testCaseAsync "let! value = ..."
              <| testExplicitType
                   """
            async {
              let! $($0value$I$) = async { return 1 }
              ()
            } |> ignore
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Required fromCursors
                       SpecialRules = [] })
              testCaseAsync "let! (value: int) = ..."
              <| testExplicitType
                   """
            async {
              let! ($0value: int) = async { return 1 }
              ()
            } |> ignore
            """
                   (ExplicitType.Exists) ]

          testList
            "use!"
            [ testCaseAsync "use! value = ..."
              <| testExplicitType
                   """
            let d = { new System.IDisposable with
                member _.Dispose() = ()
            }
            async {
                use! $0value = async { return d }
                ()
            } |> ignore
            """
                   (ExplicitType.Invalid) ]

          testList
            "foreach loop"
            [ testCaseAsync "for value in [1..5]"
              <| testExplicitType
                   """
            for $($0value$I$) in [1..5] do
              ()
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Optional fromCursors
                       SpecialRules = [] })
              testCaseAsync "for value: int in [1..5]"
              <| testExplicitType
                   """
            for $0value: int in [1..5] do
              ()
            """
                   (ExplicitType.Exists) ] ]
      testSequenced <|
      testList
        "Patterns"
        [ testList
            "tuple"
            [ testCaseAsync "let (value,_) = (1,2)"
              <| testExplicitType
                   """
            let ($($0value$I$),_) = (1,2)
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Optional fromCursors
                       SpecialRules = [] })
              testCaseAsync "let value,_ = (1,2)"
              <| testExplicitType
                   """
            let $($0value$I$),_ = (1,2)
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Required fromCursors
                       SpecialRules = [] })
              testCaseAsync "let (value: int,_) = (1,2)"
              <| testExplicitType
                   """
            let ($0value: int,_) = (1,2)
            """
                   (ExplicitType.Exists)
              testCaseAsync "let (value: int),_ = (1,2)"
              <| testExplicitType
                   """
            let ($0value: int),_ = (1,2)
            """
                   (ExplicitType.Exists)
              //ENHANCEMENT: Distinguish between direct and parently/ancestorly typed?
              testCaseAsync "let (value,_): int*int = (1,2)"
              <| testExplicitType
                   """
            let ($($0value$I$),_): int*int = (1,2)
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Optional fromCursors
                       SpecialRules = [] })
              testCaseAsync "let value,_ : int*int = (1,2)"
              <| testExplicitType
                   """
            let $($0value$I$),_ : int*int = (1,2)
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Required fromCursors
                       SpecialRules = [] }) ]
          testList
            "struct"
            [ testCaseAsync "let struct (value,_) ="
              <| testExplicitType
                   """
            let struct ($($0value$I$),_) = struct (1,2)
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Optional fromCursors
                       SpecialRules = [] }) ]
          testList
            "Union"
            [ testCaseAsync "let U value = U 42"
              <| testExplicitType
                   """
            type U = U of int
            let U $($0value$I$) = U 42
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Required fromCursors
                       SpecialRules = [] })
              testCaseAsync "let U (value) = U 42"
              <| testExplicitType
                   """
            type U = U of int
            let U ($($0value$I$)) = U 42
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Exist fromCursors
                       SpecialRules = [] })
              testCaseAsync "let ActPat v = U 42"
              <| testExplicitType
                   """
            let (|ActPat|) v = ActPat v
            let ActPat $($0value$I$) = 42
            """
                   // For ActivePattern parens aren't actually required -- but cannot distinguish from Union Case which requires Parens (because type of union, not type of value)
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Required fromCursors
                       SpecialRules = [] })
              testCaseAsync "| U (Beta=value) ->"
              <| testExplicitType
                   """
            type U = U of Alpha:int * Beta: int* Gamma: int

            match U (1,2,3) with
            | U (Beta=$($0value$I$)) -> ()
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Optional fromCursors
                       SpecialRules = [] })
              testCaseAsync "| U (Beta=value: int) ->"
              <| testExplicitType
                   """
            type U = U of Alpha:int * Beta: int* Gamma: int

            match U (1,2,3) with
            | U (Beta=$0value: int) -> ()
            """
                   (ExplicitType.Exists) ]
          testList
            "record"
            [ testCaseAsync "let { Value1=value1 } ="
              <| testExplicitType
                   """
            type R = { Value1: int; Value2: int; Value3: int}
            let r = { Value1=1; Value2=2; Value3=3 }

            let { Value1=$($0value1$I$) } = r
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Optional fromCursors
                       SpecialRules = [] })
              testCaseAsync "let { Value1=value1: int } ="
              <| testExplicitType
                   """
            type R = { Value1: int; Value2: int; Value3: int}
            let r = { Value1=1; Value2=2; Value3=3 }

            let { Value1=$0value1: int } = r
            """
                   (ExplicitType.Exists)

              // No pattern matching for anon records
              ]

          testList
            "Optional"
            [
              // Parens must include `?` too
              // Note for Insert Explicit Type Annotation: must not include `option` -> `: int`, NOT `: int option`
              testCaseAsync "static member DoStuff ?value = ..."
              <| testExplicitType
                   """
            type A =
              static member DoStuff $(?$0value$I$) = value |> Option.map ((+)1)
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Required fromCursors
                       SpecialRules = [ RemoveOptionFromType ] })
              testCaseAsync "static member DoStuff (?value) = ..."
              <| testExplicitType
                   """
            type A =
              static member DoStuff ($(?$0value$I$)) = value |> Option.map ((+)1)
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Exist fromCursors
                       SpecialRules = [ RemoveOptionFromType ] })
              testCaseAsync "static member DoStuff (?value: int) = ..."
              <| testExplicitType
                   """
            type A =
              static member DoStuff ($0value: int) = value |> Option.map ((+)1)
            """
                   (ExplicitType.Exists)
              testCaseAsync "static member DoStuff (a, b, ?value) = ..."
              <| testExplicitType
                   """
            type A =
              static member DoStuff (a, b, $(?$0value$I$)) = value |> Option.map (fun v -> v + a + b)
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Optional fromCursors
                       SpecialRules = [ RemoveOptionFromType ] })
              testCaseAsync "static member DoStuff (a, b, ?value: int) = ..."
              <| testExplicitType
                   """
            type A =
              static member DoStuff (a, b, $0value: int) = value |> Option.map (fun v -> v + a + b)
            """
                   (ExplicitType.Exists) ]

          testList
            "nested"
            [ testCaseAsync "options & tuples in option"
              <| testExplicitType
                   """
            let v = Some (Some (1, (2,Some 3)))
            match v with
            | Some (Some (_, (_, Some $(?$0value$I$)))) -> ()
            | _ -> ()
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Required fromCursors
                       SpecialRules = [ RemoveOptionFromType ] })
              testCaseAsync "options & tuples in tuple"
              <| testExplicitType
                   """
            let v = Some (Some (1, (2,Some 3)))
            match v with
            | Some (Some (_, ($(?$0value$I$), Some _))) -> ()
            | _ -> ()
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Optional fromCursors
                       SpecialRules = [ RemoveOptionFromType ] }) ] ]
      testList
        "let function"
        [ testList
            "params"
            [ testCaseAsync "let f value = value + 1"
              <| testExplicitType
                   """
            let f $($0value$I$) = value + 1
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Required fromCursors
                       SpecialRules = [] })
              testCaseAsync "let f (value) = value + 1"
              <| testExplicitType
                   """
            let f ($($0value$I$)) = value + 1
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Exist fromCursors
                       SpecialRules = [] })
              testCaseAsync "let f (value: int) = value + 1"
              <| testExplicitType
                   """
            let f ($0value: int) = value + 1
            """
                   (ExplicitType.Exists)

              testCaseAsync "let f a value b = ..."
              <| testExplicitType
                   """
            let f a $($0value$I$) b = value + b + a + 1
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Required fromCursors
                       SpecialRules = [] })
              testCaseAsync "let f a (value: int) b = ..."
              <| testExplicitType
                   """
            let f a ($0value: int) b = value + a + b + 1
            """
                   (ExplicitType.Exists) ]
          testList
            "function"
            [
              // not (yet?) supported
              testCaseAsync "let f value = value + 1"
              <| testExplicitType'
                   """
            let $0f value = value + 1
            """
                   None ]

          testList
            "member"
            [ testCaseAsync "static member DoStuff value ="
              <| testExplicitType
                   """
            type A =
              static member DoStuff $($0value$I$) = value + 1
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Required fromCursors
                       SpecialRules = [] })
              testCaseAsync "static member DoStuff (value) ="
              <| testExplicitType
                   """
            type A =
              static member DoStuff ($($0value$I$)) = value + 1
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Exist fromCursors
                       SpecialRules = [] })
              testCaseAsync "static member DoStuff (value: int) ="
              <| testExplicitType
                   """
            type A =
              static member DoStuff ($0value: int) = value + 1
            """
                   (ExplicitType.Exists)
              testCaseAsync "static member DoStuff a value b ="
              <| testExplicitType
                   """
            type A =
              static member DoStuff a $($0value$I$) b = value + a + b + 1
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Required fromCursors
                       SpecialRules = [] })
              testCaseAsync "static member DoStuff(a, value, b) ="
              <| testExplicitType
                   """
            type A =
              static member DoStuff(a, $($0value$I$), b) = value + a + b + 1
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Optional fromCursors
                       SpecialRules = [] })

              testCaseAsync "member x.DoStuff(a, value, b) ="
              <| testExplicitType
                   """
            type A() =
              member x.DoStuff(a, $($0value$I$), b) = value + a + b + 1
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Optional fromCursors
                       SpecialRules = [] })
              testCaseAsync "doesn't handle this"
              <| testExplicitType'
                   """
            type A() =
              member $0x.DoStuff(a, value, b) = value + a + b + 1
            """
                   None
              // not (yet?) supported
              testCaseAsync "doesn't handle function"
              <| testExplicitType'
                   """
            type A() =
              member x.$0DoStuff(a, value, b) = value + a + b + 1
            """
                   None ]
          testList
            "secondary ctor"
            [ testCaseAsync "new (a, value) ="
              <| testExplicitType
                   """
            type A(a: int) =
              new (a, $($0value$I$)) = A(a+value)
              member _.DoStuff(v) = v + a + 1
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Optional fromCursors
                       SpecialRules = [] })

              ] ]
      testList
        "pattern match"
        [ testCaseAsync "| value ->"
          <| testExplicitType
               """
          match 4 with
          | $($0value$I$) -> ()
          """
               (ExplicitType.Missing
                 { Ident = fromCursorAndInsert
                   InsertAt = fromCursor
                   Parens = Parens.Required fromCursors
                   SpecialRules = [] })
          testCaseAsync "| Some value ->"
          <| testExplicitType
               """
          match 4 with
          | Some $($0value$I$) -> ()
          | _ -> ()
          """
               (ExplicitType.Missing
                 { Ident = fromCursorAndInsert
                   InsertAt = fromCursor
                   Parens = Parens.Required fromCursors
                   SpecialRules = [] })
          testCaseAsync "Choice1Of2 value | Choice2Of2 value ->"
          <| testExplicitType
               """
          match Choice1Of2 3 with
          | Choice1Of2 value | Choice2Of2 $($0value$I$) -> ()
          """
               (ExplicitType.Missing
                 { Ident = fromCursorAndInsert
                   InsertAt = fromCursor
                   Parens = Parens.Required fromCursors
                   SpecialRules = [] })
          testList
            "as"
            [
              // strange `as`:
              // * `let _ as value: int = ...` -> ok
              // * `| _ as value: int -> ...` -> error
              // * `static member F (_ as value: int) = ...` -> ok
              //
              // * `let value: int as _ = ...` -> error
              // * `| value: int as _ -> ...` -> ok
              // * `static member F (value: int as _) = ...` -> ok

              // ->
              // trailing type anno:
              // * in `let`: trailing type anno part of `let` binding, NOT pattern -> ok
              // * similar when with parens: `(pat: type)` with `pat=_ as _`
              // * in `case`: just pattern -> no trailing type annotation part of case definition
              //
              // type anno in first binding position: don't know
              // Probably eager type annotation matching of let binding? -> `as` is now in pos of parameter
              // Other Patterns require parens too:
              // * `let Some value = Some 42` -> function named `Some` with argument `value: 'a` returning `Some 42`
              // * `let (Some value) = Some 42` -> destructure of `Some 42` to `value: int = 42`

              testCaseAsync "let _ as value ="
              <| testExplicitType
                   """
            let _ as $($0value$I$) = 42
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Optional fromCursors
                       SpecialRules = [] })
              testCaseAsync "| _ as value ->"
              <| testExplicitType
                   """
            match 4 with
            | _ as $($0value$I$) -> ()
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Required fromCursors
                       SpecialRules = [] })
              testCaseAsync "static member F (_ as value) ="
              <| testExplicitType
                   """
            type A =
              static member F (_ as $($0value$I$)) = value + 1
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Optional fromCursors
                       SpecialRules = [] })

              testCaseAsync "let value as _ ="
              <| testExplicitType
                   """
            let $($0value$I$) as _ = 42
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Required fromCursors
                       SpecialRules = [] })
              testCaseAsync "| value as _ ->"
              <| testExplicitType
                   """
            match 4 with
            | $($0value$I$) as _ -> ()
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Optional fromCursors
                       SpecialRules = [] })
              testCaseAsync "static member F (value as _) ="
              <| testExplicitType
                   """
            type A =
              static member F ($($0value$I$) as _) = value + 1
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Optional fromCursors
                       SpecialRules = [] })

              testCaseAsync "let (_ as value) ="
              <| testExplicitType
                   """
            let (_ as $($0value$I$)) = 42
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Optional fromCursors
                       SpecialRules = [] })
              testCaseAsync "| (_ as value) ->"
              <| testExplicitType
                   """
            match 4 with
            | (_ as $($0value$I$)) -> ()
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Optional fromCursors
                       SpecialRules = [] })

              testCaseAsync "let (value as _) ="
              <| testExplicitType
                   """
            let ($($0value$I$) as _) = 42
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Optional fromCursors
                       SpecialRules = [] })
              testCaseAsync "| (value as _) ->"
              <| testExplicitType
                   """
            match 4 with
            | ($($0value$I$) as _) -> ()
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Optional fromCursors
                       SpecialRules = [] }) ]
          testCaseAsync "| (_, value) ->"
          <| testExplicitType
               """
          match (4,2) with
          | (_, $($0value$I$)) -> ()
          """
               (ExplicitType.Missing
                 { Ident = fromCursorAndInsert
                   InsertAt = fromCursor
                   Parens = Parens.Optional fromCursors
                   SpecialRules = [] })
          testCaseAsync "| [value] ->"
          <| testExplicitType
               """
          match [] with
          | [$($0value$I$)] -> ()
          | _ -> ()
          """
               (ExplicitType.Missing
                 { Ident = fromCursorAndInsert
                   InsertAt = fromCursor
                   Parens = Parens.Optional fromCursors
                   SpecialRules = [] })
          testCaseAsync "| [_; value; _] ->"
          <| testExplicitType
               """
          match [] with
          | [_; $($0value$I$); _] -> ()
          | _ -> ()
          """
               (ExplicitType.Missing
                 { Ident = fromCursorAndInsert
                   InsertAt = fromCursor
                   Parens = Parens.Optional fromCursors
                   SpecialRules = [] })

          testList
            "match!"
            [ testCaseAsync "| value ->"
              <| testExplicitType
                   """
          async {
              match async {return 2} with
              | $($0value$I$) -> ()
          }
          |> ignore
          """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Required fromCursors
                       SpecialRules = [] })

              ] ]
      testList
        "lambda"
        [ testCaseAsync "fun value ->"
          <| testExplicitType
               """
          let f = fun $($0value$I$) -> value + 1
          """
               (ExplicitType.Missing
                 { Ident = fromCursorAndInsert
                   InsertAt = fromCursor
                   Parens = Parens.Required fromCursors
                   SpecialRules = [] })
          testCaseAsync "fun a value b ->"
          <| testExplicitType
               """
          let f = fun a $($0value$I$) b -> value + a + b + 1
          """
               (ExplicitType.Missing
                 { Ident = fromCursorAndInsert
                   InsertAt = fromCursor
                   Parens = Parens.Required fromCursors
                   SpecialRules = [] })
          testCaseAsync "fun (a, value, b) ->"
          <| testExplicitType
               """
          let f = fun (a, $($0value$I$), b) -> value + a + b + 1
          """
               (ExplicitType.Missing
                 { Ident = fromCursorAndInsert
                   InsertAt = fromCursor
                   Parens = Parens.Optional fromCursors
                   SpecialRules = [] })
          testList
            "let f a = fun b -> a + b + 1"
            [ testCaseAsync "f"
              <| testExplicitType'
                   """
            let $0f a = fun b -> a + b + 1
            """
                   None
              testCaseAsync "a"
              <| testExplicitType
                   """
            let f $($0a$I$) = fun b -> a + b + 1
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Required fromCursors
                       SpecialRules = [] })
              testCaseAsync "b"
              <| testExplicitType
                   """
            let f a = fun $($0b$I$)-> a + b + 1
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Required fromCursors
                       SpecialRules = [] })

              testCaseAsync "f typed"
              <| testExplicitType'
                   """
            let $0f a : int -> int = fun b -> a + b + 1
            """
                   None
              testCaseAsync "a typed"
              <| testExplicitType
                   """
            let f ($0a: int) = fun b -> a + b + 1
            """
                   (ExplicitType.Exists)
              testCaseAsync "b typed"
              <| testExplicitType
                   """
            let f a = fun ($0b: int) -> a + b + 1
            """
                   (ExplicitType.Exists) ] ]
      testList
        "SimplePats"
        [
          // primary ctor args & lambda args
          testList
            "primary ctor"
            [ testCaseAsync "T(a)"
              <| testExplicitType
                   """
            type A($0a$I) =
              member _.F(b) = a + b + 1
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Forbidden
                       SpecialRules = [] })
              testCaseAsync "T(a: int)"
              <| testExplicitType
                   """
            type A($0a: int) =
              member _.F(b) = a + b + 1
            """
                   (ExplicitType.Exists)
              testCaseAsync "T(a, b, c, d)"
              <| testExplicitType
                   """
            type A(a, b, $0c$I, d) =
              member _.F(b) = a + b + 1
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Forbidden
                       SpecialRules = [] })
              testCaseAsync "T(a, b, c: int, d)"
              <| testExplicitType
                   """
            type A(a, b, $0c: int, d) =
              member _.F(b) = a + b + 1
            """
                   (ExplicitType.Exists)
              testCaseAsync "T([<Attr>]a)"
              <| testExplicitType
                   """
            type Attr() =
              inherit System.Attribute()
            type A([<Attr>]$0a$I) =
              member _.F(b) = a + b + 1
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Forbidden
                       SpecialRules = [] }) ] ]

      testList
        "detect existing annotation"
        [ testCaseAsync "let (value): int ="
          <| testExplicitType
               """
            let ($0value): int = 3
            """
               (ExplicitType.Exists)
          testCaseAsync "let ((value)): int ="
          <| testExplicitType
               """
            let (($0value)): int = 3
            """
               (ExplicitType.Exists) ]

      testList
        "trigger location"
        [ testList
            "let f p = p + 2"
            [ testCaseAsync "trigger for p binding"
              <| testExplicitType
                   """
            let f $($0p$I$) = p + 2
            """
                   (ExplicitType.Missing
                     { Ident = fromCursorAndInsert
                       InsertAt = fromCursor
                       Parens = Parens.Required fromCursors
                       SpecialRules = [] })
              testCaseAsync "doesn't trigger for f binding"
              <|
              // ENHANCEMENT: handle
              testExplicitType'
                """
            let $0f p = p + 2
            """
                None
              testCaseAsync "doesn't trigger for p usage"
              <| testExplicitType'
                   """
            let f p = $0p + 2
            """
                   None ]
          testCaseAsync "nested let"
          <| testExplicitType
               """
          let f a b =
            let res =
              let $($0t$I$) = a + b
              t + a
            res + 3
          """
               (ExplicitType.Missing
                 { Ident = fromCursorAndInsert
                   InsertAt = fromCursor
                   Parens = Parens.Optional fromCursors
                   SpecialRules = [] }) ] ]
