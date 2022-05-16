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

module private InlayHints =
  open Utils.Server
  open Utils.Tests
  open Utils.Utils
  open Utils.TextEdit

  let private at (text, pos, kind) : LSPInlayHint =
    { Text =
        match kind with
        | InlayHintKind.Type -> ": " + InlayHints.truncated text
        | InlayHintKind.Parameter -> InlayHints.truncated text + " ="
      // this is for truncated text, which we do not currently hit in our tests
      // TODO: add tests to cover this case
      InsertText =
        match kind with
        | InlayHintKind.Type -> Some(": " + text)
        | InlayHintKind.Parameter -> None
      Pos = pos
      Kind = kind }
  let private from (text, (line, char), kind) = 
    at (text, { Line=line; Character=char}, kind)


  let private check' 
    (server: CachedServer)
    (text: string)
    (range: Range)
    (expected: LSPInlayHint array)
    = async {
      let! (doc, diags) = server |> Server.createUntitledDocument text
      use doc = doc // ensure doc gets closed (disposed) after test

      match diags with
      | [||] -> ()
      | diags -> failtest $"Should not have had check errors, but instead had %A{diags}"

      let! actual = Document.fsharpInlayHintsAt range doc
      Expect.equal actual expected "Expected the given set of hints"
    }
  let check (server: CachedServer) (documentText: string) (expectedHints: _ list) = async {
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
      { Start = poss |> List.head; End = poss |> List.last }
    let poss =
      let count = poss |> List.length
      poss[1..(count-2)]

    (text, range, poss)
  
  let checkRange (server: CachedServer) (documentText: string) (expectedHints: _ list) = async {
    let (text, range, poss) = documentText |> extractCursorsInsideRange
    Expect.equal (poss |> List.length) (expectedHints |> List.length) $"Expected Hints & position cursors to match, but there were {expectedHints |> List.length} expected hints and {poss |> List.length} position cursors"
    let expected =
      List.zip poss expectedHints
      |> List.map (fun (pos, (name, kind)) -> at (name, pos, kind))
      |> List.toArray
    do! check' server text range expected
  }

let private param (name: string) = (name, InlayHintKind.Parameter)
let private ty (name: string) = (name, InlayHintKind.Type)

let private fsharpInlayHintsTests state =
  serverTestList (nameof Core.InlayHints) state defaultConfigDto None (fun server -> [
    testList "type hint" [
      testCaseAsync "let-bound function parameter type hints"
        <| InlayHints.check
            server
            """
      $0let tryFindFile p = p + "hi"$0
      """
            [ "string", (0, 17), InlayHintKind.Type ]

      testCaseAsync "value let binding type hint"
      <| InlayHints.check
           server
           """
      $0let f = "hi"$0
      """
           [ "string", (0, 5), InlayHintKind.Type ]

      testCaseAsync "no type hint for an explicitly-typed binding"
      <| InlayHints.check server """$0let s: string = "hi"$0""" []

      testCaseAsync "type hints are truncated to 30 characters"
      <| InlayHints.check
           server
           """
        $0let t = Some (Some (Some (Some (Some (Some (Some (Some (Some (Some (Some (Some (Some (Some (Some ()))))))))))))))$0
        """
           [ "unit option option option option option option option option option option option option option option option",
             (0, 5),
             InlayHintKind.Type ] 
    ]

    testList "parameter hint" [

      testCaseAsync "parameter names aren't yet implemented, will fail when we update FCS"
      <| InlayHints.check server """$0 System.Environment.GetEnvironmentVariable "Blah" |> ignore$0""" []

      testCaseAsync "doesn't show hint for well-known parameter names"
      <| InlayHints.check server """$0sprintf "thing %s" "blah" |> ignore$0""" []

      testCaseAsync "doesn't show hints for short parameter names"
      <| InlayHints.check
           server
           """
        let someFunction s = s
        let noHintForShortParameter = $0someFunction "hi"$0
        """
           []

      testCaseAsync "doesn't show hints for parameter names that match user text"
      <| InlayHints.check
           server
           """
        let anotherFunction (kind: string) = ()
        let kind = "hi"
        $0anotherFunction kind$0
        """
           []

      testCaseAsync "no hint for a function with a short parameter name"
      <| InlayHints.check
           server
           """
        // shows that no parameter name hint is shown for a function with a short parameter name
        let someFunction s = s
        let noHintForShortParameter = $0someFunction "hi"$0
        """
           []

      testCaseAsync "show: param & variable have different names" <|
        InlayHints.checkRange server
          """
          let f beta = ()
          let alpha = 42

          $0f $0alpha$0
          """
          [ param "beta" ]

      testCaseAsync "hide: param & variable have same name" <|
        InlayHints.checkRange server
          """
          let f alpha = ()
          let alpha = 42

          $0f alpha$0
          """
          [  ]
      testCaseAsync "hide: variable prefix of param" <|
        InlayHints.checkRange server
          """
          let f rangeCoveringExpr = ()
          let range = 2

          $0f range$0
          """
          [  ]
      testCaseAsync "hide: variable postfix of param" <|
        InlayHints.checkRange server
          """
          let f exactRange = ()
          let range = 2

          $0f range$0
          """
          [  ]
      testCaseAsync "show: variable infix of param" <|
        InlayHints.checkRange server
          """
          let f exactRangeCoveringExpr = ()
          let range = 2

          $0f $0range$0
          """
          [ param "exactRangeCoveringExpr" ]
      testCaseAsync "show: variable prefix of param, but no word boundary" <|
        InlayHints.checkRange server
          """
          let f rangecover = ()
          let range = 2

          $0f $0range$0
          """
          [ param "rangecover" ]
      testCaseAsync "show: variable postfix of param, but no word boundary" <|
        InlayHints.checkRange server
          """
          let f exactrange = ()
          let range = 2

          $0f $0range$0
          """
          [ param "exactrange" ]

      testCaseAsync "hide: arg is prefix of param with leading _" <|
        InlayHints.checkRange server
          """
          let f _rangeCoveringExpr = ()
          let range = 2

          $0f range$0
          """
          []
      testCaseAsync "hide: arg is postfix of param with trailing '" <|
        InlayHints.checkRange server
          """
          let f exactRange' = ()
          let range = 2

          $0f range$0
          """
          []
      testCaseAsync "hide: arg is prefix of param with trailing ' in arg" <|
        InlayHints.checkRange server
          """
          let f rangeCoveringExpr = ()
          let range' = 2

          $0f range'$0
          """
          []

      testCaseAsync "hide: param prefix of arg" <|
        InlayHints.checkRange server
          """
          let f range = ()
          let rangeCoveringExpr = 2

          $0f rangeCoveringExpr$0
          """
          []
      testCaseAsync "hide: param postfix of arg" <|
        InlayHints.checkRange server
          """
          let f range = ()
          let exactRange = 2

          $0f exactRange$0
          """
          []

      testCaseAsync "hide: arg is field access with same name as param (upper case start)" <|
        InlayHints.checkRange server
          """
          type Data = {
            Range: int
          }
          let f range = ()
          let data: Data = { Range = 2 }

          $0f data.Range$0
          """
          []
      testCaseAsync "hide: arg is field access with same name as param (lower case start)" <|
        InlayHints.checkRange server
          """
          type Data = {
            range: int
          }
          let f range = ()
          let data: Data = { range = 2 }

          $0f data.range$0
          """
          []
      testCaseAsync "hide: arg is field access prefix of param (upper case start)" <|
        InlayHints.checkRange server
          """
          type Data = {
            Range: int
          }
          let f rangeCoveringExpr = ()
          let data: Data = { Range = 2 }

          $0f data.Range$0
          """
          []
      testCaseAsync "hide: arg is field access, param is prefix of arg" <|
        InlayHints.checkRange server
          """
          type Data = {
            RangeCoveringExpr: int
          }
          let f range = ()
          let data: Data = { RangeCoveringExpr = 2 }

          $0f data.RangeCoveringExpr$0
          """
          []

      testCaseAsync "hide: arg in parens same as param" <|
        InlayHints.checkRange server
          """
          let f alpha = ()
          let alpha = 42

          $0f (alpha)$0
          """
          [  ]
      testCaseAsync "hide: arg in parens and spaces same as param" <|
        InlayHints.checkRange server
          """
          let f alpha = ()
          let alpha = 42

          $0f ( alpha )$0
          """
          [  ]
      testCaseAsync "show: expr including param name in parens" <|
        InlayHints.checkRange server
          """
          let f alpha = ()
          let alpha x = x + 3

          $0f $0(1 |> alpha)$0
          """
          [ param "alpha" ]
          
      //ENHANCEMENT: detect some common expressions like:
      // * receiving end of pipe: `1 |> alpha`, `alpha <| 1`, `1 |> toAlpha`
      // * last function: `1.ToAlpha()`
      // * often used convert functions: `string alpha`, `alpha.ToString()`
      testCaseAsync "show: any expression" <|
        InlayHints.checkRange server
          """
          let f (alpha, beta, gamma) = ()
          let alpha = 1
          let beta = 2
          let gamma = 2

          $0f ($0string alpha, $0beta.ToString(), $0gamma |> string)$0
          """
          [ param "alpha"; param "beta"; param "gamma" ]

      testCaseAsync "hide: unary operator" <|
        InlayHints.checkRange server
          """
          let (~+.) listWithNumbers = List.map ((+) 1) listWithNumbers
          let data = [1..5]

          $0+. data$0
          """
          []
      testCaseAsync "hide: binary operator" <|
        InlayHints.checkRange server
          """
          let (+.) listWithNumbers numberToAdd = List.map ((+) numberToAdd) listWithNumbers
          let data = [1..5]

          $0data +. 5$0
          """
          []

      testCaseAsync "hide: func name ends with param name" <|
        InlayHints.checkRange server
          """
          let validateRange range = ()
          let data = 42

          $0validateRange data$0
          """
          []

      testList "special names" [
        testList "mapping" [
          testCaseAsync "hide: for List" <|
            InlayHints.checkRange server
              """
              $0[1..3] |> List.map id$0
              """
              []
          testCaseAsync "hide: for Array" <|
            InlayHints.checkRange server
              """
              $0[|1..3|] |> Array.map id$0
              """
              []
          testCaseAsync "show: for custom function" <|
            InlayHints.checkRange server
              """
              let doStuff mapping = ()
              $0doStuff $042$0
              """
              [ param "mapping" ]
        ]
        testList "in collections" [
          testCaseAsync "hide: predicate" <|
            InlayHints.checkRange server
              """
              $0[1..3] |> List.filter ((<) 2)$0
              """
              []
          testCaseAsync "hide: chooser" <|
            InlayHints.checkRange server
              """
              $0[1..3] |> List.tryPick Some$0
              """
              []
          testCaseAsync "hide: value" <|
            InlayHints.checkRange server
              """
              $0[1..3] |> List.contains 2$0
              """
              []
          testCaseAsync "hide: projection" <|
            InlayHints.checkRange server
              """
              $0[1..3] |> List.sumBy id$0
              """
              []
          testCaseAsync "hide: action" <|
            InlayHints.checkRange server
              """
              $0[1..3] |> List.iter (printfn "%i")$0
              """
              []
          testCaseAsync "hide: folder & state" <|
            InlayHints.checkRange server
              """
              $0[1..3] |> List.fold (+) 0$0
              """
              []


          testCaseAsync "hide: list" <|
            InlayHints.checkRange server
              """
              $0List.tryLast [1..3]$0
              """
              []
          testCaseAsync "hide: array" <|
            InlayHints.checkRange server
              """
              $0Array.tryLast [|1..3|]$0
              """
              []
          testCaseAsync "hide: source" <|
            InlayHints.checkRange server
              """
              $0Seq.tryLast [1..3]$0
              """
              []
          testCaseAsync "hide: lists" <|
            InlayHints.checkRange server
              """
              $0List.concat []$0
              """
              []
          testCaseAsync "hide: arrays" <|
            InlayHints.checkRange server
              """
              $0Array.concat [||]$0
              """
              []
          testCaseAsync "hide: sources" <|
            InlayHints.checkRange server
              """
              $0Seq.concat []$0
              """
              []
        ]
        testList "option" [
          testCaseAsync "hide: for Option" <|
            InlayHints.checkRange server
              """
              $0Option.count (Some 3)$0
              """
              []
          testCaseAsync "show: for custom function" <|
            InlayHints.checkRange server
              """
              let doStuff option = ()
              $0doStuff $042$0
              """
              [ param "option" ]
        ]
        testList "voption" [
          testCaseAsync "hide: for ValueOption" <|
            InlayHints.checkRange server
              """
              $0ValueOption.count (ValueSome 3)$0
              """
              []
          testCaseAsync "show: for custom function" <|
            InlayHints.checkRange server
              """
              let doStuff voption = ()
              $0doStuff $042$0
              """
              [ param "voption" ]
        ]
        testList "format" [
          testCaseAsync "hide: in printfn" <|
            InlayHints.checkRange server
              """
              $0printfn "foo"$0
              """
              []
          testCaseAsync "hide: in sprintf" <|
            InlayHints.checkRange server
              """
              $0sprintf "foo"$0
              """
              []
          testCaseAsync "hide: in Core.Printf" <|
            // "normal" printf is in `Microsoft.FSharp.Core.ExtraTopLevelOperators`
            InlayHints.checkRange server
              """
              $0Microsoft.FSharp.Core.Printf.printfn "foo"$0
              """
              []
          testCaseAsync "show: for custom function" <|
            InlayHints.checkRange server
              """
              let doStuff format = ()
              $0doStuff $042$0
              """
              [ param "format" ]
        ]
      ]
    ]
  ])

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
    = async {
      let! (doc, diags) = server |> Server.createUntitledDocument text
      use doc = doc
      // Expect.isEmpty diags "Should not have had check errors"
      Expect.hasLength diags 0 "Should not have had check errors"

      let! hints = doc |> Document.inlayHintsAt range
      do! validateInlayHints doc text hints
    }

  let private validateHint
    (doc: Document)
    (expectedBase: InlayHint)
    (textAfterEdits: string option)
    (text: string)
    (actual: InlayHint)
    = async {
      // Edits are checked by applying -> only check Edits None or Some
      let mkDummyEdits o = o |> Option.bind (fun _ -> Some [||])
      let ignoreData (hint: InlayHint) = { hint with Data = None }

      let actualWithoutEdits = { actual with TextEdits = mkDummyEdits actual.TextEdits } |> ignoreData
      let expectedWithoutExpected = { expectedBase with TextEdits = mkDummyEdits textAfterEdits }

      Expect.equal actualWithoutEdits expectedWithoutExpected "Hint doesn't match expectations (Note: `TextEdits` are handled separately. Here just `None` or `Some`)"

      match actual.TextEdits, textAfterEdits with
      | Some edits, Some textAfterEdits ->
          let appliedText =
            text
            |> TextEdits.applyWithErrorCheck (edits |> List.ofArray)
            |> Flip.Expect.wantOk "TextEdits are erroneous"
          Expect.equal appliedText textAfterEdits "Text after applying TextEdits does not match expected"
      | _ -> ()

      //TODO: handle capabilities
      //TODO: en/disable?
      let toResolve = 
        { actual with
            Tooltip = None
            TextEdits = None
        }
      let! resolved = doc |> Document.resolveInlayHint toResolve
      Expect.equal resolved actual "`textDocument/inlayHint` and `inlayHint/resolve` should result in same InlayHint"

      //todo: compare with AddExplicitType?
    }

  let rangeMarker = "$|"

  let checkAllInMarkedRange
    (server: CachedServer)
    (textWithCursors: string)
    (expected: (InlayHint * (string option)) list)
    = async {
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
        |> Flip.Expect.wantSome "There should be range markers"
      Expect.hasLength cursors (expected.Length) $"Number of Cursors & expected hints don't match ({cursors.Length} cursors, {expected.Length} expected hints)"
      let expected =
        List.zip expected cursors
        |> List.map (fun ((hint, textAfterEdits), cursor) ->
            let hint = { hint with Position = cursor}
            (hint, textAfterEdits)
        )

      let validateHints doc (text: string) (hints: InlayHint[]) = async {
        Expect.hasLength hints expected.Length "Number of actual hints and expected hints don't match"

        for (actual, (expected, textAfterEdits)) in Seq.zip hints expected do
          do! validateHint doc expected textAfterEdits text actual
      }

      do! checkInRange server text range validateHints
    }

  let private fromCursor: Position = { Line = -1; Character = -1 }

  let private mkBasicHint
    (kind: InlayHintKind)
    (pos: Position)
    (label: string)
    : InlayHint
    =
    {
      Kind = Some kind
      Position = pos
      Label = InlayHintLabel.String label
      TextEdits = None
      Tooltip = None
      PaddingLeft = match kind with | InlayHintKind.Type -> Some true | _ -> None
      PaddingRight = match kind with | InlayHintKind.Parameter -> Some true | _ -> None
      Data = None
    }
  let paramHint
    (paramName: string)
    =
    let label = $"{paramName} ="
    let hint = mkBasicHint InlayHintKind.Parameter fromCursor label
    (hint, None)
  let typeHint
    (typeName: string)
    (expectedAfterEdits: string)
    =
    let label = $": {typeName}"
    let hint = mkBasicHint InlayHintKind.Type fromCursor label
    let expectedAfterEdits =
      expectedAfterEdits
      |> Text.trimTripleQuotation
    (hint, Some expectedAfterEdits)

open LspInlayHints
let private paramHintTests state =
  serverTestList "param hints" state defaultConfigDto None (fun server -> [
    testCaseAsync "can show param hint" <|
      checkAllInMarkedRange server
        """
        let f beta = ()
        $|f $042$|
        """
        [
          paramHint "beta"
        ]
  ])
let private typeHintTests state =
  serverTestList "type hints" state defaultConfigDto None (fun server -> [
    testCaseAsync "can show type hint" <|
      checkAllInMarkedRange server
        """
        $|let f beta$0 = beta + 1$|
        """
        [
          typeHint "int"
            """
            let f (beta: int) = beta + 1
            """
        ]
    testCaseAsync "can show type for generic actual type" <|
      checkAllInMarkedRange server
        """
        open System.Collections.Generic
        $|let list$0 = List<int>()$|
        list.Add 2
        """
        [
          typeHint "List<int>"
            """
            open System.Collections.Generic
            let list: List<int> = List<int>()
            list.Add 2
            """
        ]
    ptestCaseAsync "can show type hint for nested inside generic actual type" <|
      checkAllInMarkedRange server
        """
        open System.Collections.Immutable
        $|let arr$0 = ImmutableArray.CreateBuilder()$|
        arr.Add 2
        """
        [
          //Currently: `ImmutableArray`1.Builder<int>`
          typeHint "ImmutableArray<int>.Builder"
            """
            open System.Collections.Immutable
            let arr: ImmutableArray<int>.Builder = ImmutableArray.CreateBuilder()
            arr.Add 2
            """
        ]
  ])
let private mixedHintTests state =
  serverTestList "inlay hints" state defaultConfigDto None (fun server -> [
    testCaseAsync "can show all hints" <|
      checkAllInMarkedRange server
        """
        $|open System
        let f alpha$0 beta$0 = 
          let beta$0 = Int32.Parse beta
          let value$0 = alpha + beta + 2
          value * 2
        let res$0 = f $042 $0"13" + f $01 $0"2"$|
        """
        [
          typeHint "int"
            """
            open System
            let f (alpha: int) beta = 
              let beta = Int32.Parse beta
              let value = alpha + beta + 2
              value * 2
            let res = f 42 "13" + f 1 "2"
            """
          typeHint "string"
            """
            open System
            let f alpha (beta: string) = 
              let beta = Int32.Parse beta
              let value = alpha + beta + 2
              value * 2
            let res = f 42 "13" + f 1 "2"
            """
          typeHint "int"
            """
            open System
            let f alpha beta = 
              let beta: int = Int32.Parse beta
              let value = alpha + beta + 2
              value * 2
            let res = f 42 "13" + f 1 "2"
            """
          typeHint "int"
            """
            open System
            let f alpha beta = 
              let beta = Int32.Parse beta
              let value: int = alpha + beta + 2
              value * 2
            let res = f 42 "13" + f 1 "2"
            """
          typeHint "int"
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
          paramHint "beta"
        ]
  ])
let private inlayHintTests state =
  testList "LSP InlayHints" [
    paramHintTests state
    typeHintTests state
    mixedHintTests state
  ]

let tests state = 
  testList (nameof InlayHint) [
    fsharpInlayHintsTests state
    inlayHintTests state
  ]


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
  let checker = lazy ( FSharpChecker.Create() )
  let getAst input = async {
    let checker = checker.Value
    // Get compiler options for the 'project' implied by a single script file
    let! projOptions, diagnostics = 
      checker.GetProjectOptionsFromScript(file, input, assumeDotNetFramework=false)
    // Expect.isEmpty diagnostics "There should be no diagnostics"
    Expect.hasLength diagnostics 0 "There should be no diagnostics"

    let parsingOptions, errors = checker.GetParsingOptionsFromProjectOptions(projOptions)
    // Expect.isEmpty errors "There should be no errors"
    Expect.hasLength errors 0 "There should be no errors"

    // Run the first phase (untyped parsing) of the compiler
    let! parseFileResults = 
      checker.ParseFile(file, input, parsingOptions) 
    // Expect.isEmpty parseFileResults.Diagnostics "There should be no parse diagnostics"
    Expect.hasLength parseFileResults.Diagnostics 0 "There should be no parse diagnostics"

    return parseFileResults.ParseTree
  }

  let getExplicitTypeInfo (pos: Position) (text: string) = async {
    let text = NamedText(UMX.tag file, text)
    let! ast = getAst text

    let pos = protocolPosToPos pos
    
    let explTy = InlayHints.tryGetExplicitTypeInfo (text, ast) pos
    return explTy
  }

  let fromCursor = Position.pos0
  let fromCursors = Range.Zero
  let fromCursorAndInsert = Range.mkRange fromCursors.FileName (Position.mkPos 12345 12345) (Position.mkPos 12345 12345)

  let cursor = "$0"
  let (openParenCursor, closeParenCursor) = "$(", "$)"
  let insertCursor = "$I"
  let identCursor = "$|"
  let markers = [| cursor; openParenCursor; closeParenCursor; insertCursor; identCursor |]

  let wantsExactlyOne msg vs =
    Expect.hasLength vs 1 msg
    vs |> List.exactlyOne
  let extractCursor (marker: string) cursors =
    let pos = cursors |> List.filter (fst >> (=) marker) |> List.map snd |> wantsExactlyOne $"There should be exactly one cursor marker '{marker}'"
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
  let testExplicitType'
    (textWithCursors: string)
    (expected: ExplicitType option)
    = async {
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
          | ExplicitType.Missing ({ Ident=ident; InsertAt=insertAt; Parens=parens } as data) ->
              let insertAt, cursors =
                if insertAt = fromCursor then
                  cursors |> extractCursor insertCursor |> toFcsPos
                else
                  insertAt, cursors
              let (parens, cursors) =
                let extractParensRange cursors =
                    let (openParen, cursors) = cursors |> extractCursor openParenCursor |> toFcsPos
                    let (closeParen, cursors) = cursors |> extractCursor closeParenCursor |> toFcsPos
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
                    InsertAt=insertAt
                    Parens=parens
                }
              let updated = ExplicitType.Missing data
              updated, cursors
        
        Expect.hasLength cursors 0 "There are unused cursors!"
        expected
      let expected = expected |> Option.map (updateExpected cursors)

      let! actual = getExplicitTypeInfo pos text
      Expect.equal actual expected "Incorrect Explicit Type Info"
    }
  let testExplicitType
    textWithCursor
    expected
    =
    testExplicitType' textWithCursor (Some expected)

  testList "detect type and parens" [
    testList "Expr" [
      testList "For loop" [
        // for loop is special: no pattern, no simple pattern, just ident
        // -> no type allowed
        testCaseAsync "explicit type is invalid" <|
          testExplicitType
            """
            for $0i = 1 to 5 do
              ()
            """
            ExplicitType.Invalid
      ]
    ]
    testList "Bindings" [
      testList "simple let" [
        testCaseAsync "let value = 1" <|
          testExplicitType
            """
            let $($0value$I$) = 1
            """
            (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Optional fromCursors; SpecialRules = [] })
        testCaseAsync "let (value) = 1" <|
          testExplicitType
            """
            let ($($0value$I$)) = 1
            """
            (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Exist fromCursors; SpecialRules = [] })
        testCaseAsync "let value: int = 1" <|
          testExplicitType
            """
            let $0value: int = 1
            """
            (ExplicitType.Exists)
        testCaseAsync "let (value: int) = 1" <|
          testExplicitType
            """
            let ($0value: int) = 1
            """
            (ExplicitType.Exists)
        testCaseAsync "let (value): int = 1" <|
          testExplicitType
            """
            let ($0value): int = 1
            """
            (ExplicitType.Exists)
        testCaseAsync "let [<Attr>] value = 1" <|
          testExplicitType
            """
            type Attr() =
              inherit System.Attribute()
            let [<Attr>] $($0value$I$) = 1
            """
            (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Optional fromCursors; SpecialRules = [] })
        // Attributes are not allowed inside parens: `let ([<Attr>] value) = ...` is invalid!
        testCaseAsync "let [<Attr>] (value) = 1" <|
          testExplicitType
            """
            type Attr() =
              inherit System.Attribute()
            let [<Attr>] ($($0value$I$)) = 1
            """
            (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Exist fromCursors; SpecialRules = [] })
        testCaseAsync "let [<Attr>] value: int = 1" <|
          testExplicitType
            """
            type Attr() =
              inherit System.Attribute()
            let [<Attr>] $0value: int = 1
            """
            (ExplicitType.Exists)
        testCaseAsync "let [<Attr>] (value: int) = 1" <|
          testExplicitType
            """
            type Attr() =
              inherit System.Attribute()
            let [<Attr>] ($0value: int) = 1
            """
            (ExplicitType.Exists)
        testCaseAsync "let private value = 1" <|
          testExplicitType
            """
            let $(private $0value$I$) = 1
            """
            (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Optional fromCursors; SpecialRules = [] })
        testCaseAsync "let private value: int = 1" <|
          testExplicitType
            """
            let private $0value: int = 1
            """
            (ExplicitType.Exists) 
      ]
      testList "let with multiple vars" [
        testCaseAsync "let value1, value2, value3 = (1,2,3)" <|
          testExplicitType
            """
            let value1, $($0value2$I$), value3 = (1,2,3)
            """
            (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Required fromCursors; SpecialRules = [] })
        testCaseAsync "let (value1, value2, value3) = (1,2,3)" <|
          testExplicitType
            """
            let (value1, $($0value2$I$), value3) = (1,2,3)
            """
            (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Optional fromCursors; SpecialRules = [] })
        testCaseAsync "let (value1, value2: int, value3) = (1,2,3)" <|
          testExplicitType
            """
            let (value1, $0value: int, value3) = (1,2,3)
            """
            (ExplicitType.Exists) 
      ]

      testList "use" [
        testCaseAsync "use value = ..." <|
          testExplicitType
            """
            let d = { new System.IDisposable with
                member _.Dispose() = ()
            }
            let _ =
              use $($0value$I$) = d
              ()
            """
            (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Optional fromCursors; SpecialRules = [] })
        testCaseAsync "use value: IDisposable = ..." <|
          testExplicitType
            """
            open System
            let d = { new System.IDisposable with
                member _.Dispose() = ()
            }
            let _ =
              use $0value: IDisposable = d
              ()
            """
            (ExplicitType.Exists)
      ]
    
      testList "let!" [
        testCaseAsync "let! value = ..." <|
          testExplicitType
            """
            async {
              let! $($0value$I$) = async { return 1 }
              ()
            } |> ignore
            """
            (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Required fromCursors; SpecialRules = [] })
        testCaseAsync "let! (value: int) = ..." <|
          testExplicitType
            """
            async {
              let! ($0value: int) = async { return 1 }
              ()
            } |> ignore
            """
            (ExplicitType.Exists)
      ]

      testList "use!" [
        testCaseAsync "use! value = ..." <|
          testExplicitType
            """
            let d = { new System.IDisposable with
                member _.Dispose() = ()
            }
            async {
                use! $0value = async { return d }
                ()
            } |> ignore
            """
            (ExplicitType.Invalid)
      ]

      testList "foreach loop" [
        testCaseAsync "for value in [1..5]" <|
          testExplicitType
            """
            for $($0value$I$) in [1..5] do
              ()
            """
            (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Optional fromCursors; SpecialRules = [] })
        testCaseAsync "for value: int in [1..5]" <|
          testExplicitType
            """
            for $0value: int in [1..5] do
              ()
            """
            (ExplicitType.Exists)
      ]
    ]
    testList "Patterns" [
      testList "tuple" [
        testCaseAsync "let (value,_) = (1,2)" <|
          testExplicitType
            """
            let ($($0value$I$),_) = (1,2)
            """
            (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Optional fromCursors; SpecialRules = [] })
        testCaseAsync "let value,_ = (1,2)" <|
          testExplicitType
            """
            let $($0value$I$),_ = (1,2)
            """
            (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Required fromCursors; SpecialRules = [] })
        testCaseAsync "let (value: int,_) = (1,2)" <|
          testExplicitType
            """
            let ($0value: int,_) = (1,2)
            """
            (ExplicitType.Exists)
        testCaseAsync "let (value: int),_ = (1,2)" <|
          testExplicitType
            """
            let ($0value: int),_ = (1,2)
            """
            (ExplicitType.Exists)
        //TODO: Distinguish between direct and parently/ancestorly typed?
        testCaseAsync "let (value,_): int*int = (1,2)" <|
          testExplicitType
            """
            let ($($0value$I$),_): int*int = (1,2)
            """
            (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Optional fromCursors; SpecialRules = [] })
        testCaseAsync "let value,_ : int*int = (1,2)" <|
          testExplicitType
            """
            let $($0value$I$),_ : int*int = (1,2)
            """
            (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Required fromCursors; SpecialRules = [] })
      ]
      testList "struct" [
        testCaseAsync "let struct (value,_) =" <|
          testExplicitType
            """
            let struct ($($0value$I$),_) = struct (1,2)
            """
            (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Optional fromCursors; SpecialRules = [] })
      ]
      testList "Union" [
        testCaseAsync "let U value = U 42" <|
          testExplicitType
            """
            type U = U of int
            let U $($0value$I$) = U 42
            """
            (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Required fromCursors; SpecialRules = [] })
        testCaseAsync "let U (value) = U 42" <|
          testExplicitType
            """
            type U = U of int
            let U ($($0value$I$)) = U 42
            """
            (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Exist fromCursors; SpecialRules = [] })
        testCaseAsync "let ActPat v = U 42" <|
          testExplicitType
            """
            let (|ActPat|) v = ActPat v
            let ActPat $($0value$I$) = 42
            """
            // For ActivePattern parens aren't actually required -- but cannot distinguish from Union Case which requires Parens (because type of union, not type of value)
            (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Required fromCursors; SpecialRules = [] })
        testCaseAsync "| U (Beta=value) ->" <|
          testExplicitType
            """
            type U = U of Alpha:int * Beta: int* Gamma: int

            match U (1,2,3) with
            | U (Beta=$($0value$I$)) -> ()
            """
            (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Optional fromCursors; SpecialRules = [] })
        testCaseAsync "| U (Beta=value: int) ->" <|
          testExplicitType
            """
            type U = U of Alpha:int * Beta: int* Gamma: int

            match U (1,2,3) with
            | U (Beta=$0value: int) -> ()
            """
            (ExplicitType.Exists)
      ]
      testList "record" [
        testCaseAsync "let { Value1=value1 } =" <|
          testExplicitType
            """
            type R = { Value1: int; Value2: int; Value3: int}
            let r = { Value1=1; Value2=2; Value3=3 }

            let { Value1=$($0value1$I$) } = r
            """
            (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Optional fromCursors; SpecialRules = [] })
        testCaseAsync "let { Value1=value1: int } =" <|
          testExplicitType
            """
            type R = { Value1: int; Value2: int; Value3: int}
            let r = { Value1=1; Value2=2; Value3=3 }

            let { Value1=$0value1: int } = r
            """
            (ExplicitType.Exists)

        // No pattern matching for anon records
      ]

      testList "Optional" [
        // Parens must include `?` too
        // Note for Insert Explicit Type Annotation: must not include `option` -> `: int`, NOT `: int option`
        testCaseAsync "static member DoStuff ?value = ..." <|
          testExplicitType
            """
            type A =
              static member DoStuff $(?$0value$I$) = value |> Option.map ((+)1)
            """
            (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Required fromCursors; SpecialRules = [RemoveOptionFromType] })
        testCaseAsync "static member DoStuff (?value) = ..." <|
          testExplicitType
            """
            type A =
              static member DoStuff ($(?$0value$I$)) = value |> Option.map ((+)1)
            """
            (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Exist fromCursors; SpecialRules = [RemoveOptionFromType] })
        testCaseAsync "static member DoStuff (?value: int) = ..." <|
          testExplicitType
            """
            type A =
              static member DoStuff ($0value: int) = value |> Option.map ((+)1)
            """
            (ExplicitType.Exists)
        testCaseAsync "static member DoStuff (a, b, ?value) = ..." <|
          testExplicitType
            """
            type A =
              static member DoStuff (a, b, $(?$0value$I$)) = value |> Option.map (fun v -> v + a + b)
            """
            (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Optional fromCursors; SpecialRules = [RemoveOptionFromType] })
        testCaseAsync "static member DoStuff (a, b, ?value: int) = ..." <|
          testExplicitType
            """
            type A =
              static member DoStuff (a, b, $0value: int) = value |> Option.map (fun v -> v + a + b)
            """
            (ExplicitType.Exists)
      ]

      testList "nested" [
        testCaseAsync "options & tuples in option" <|
          testExplicitType
            """
            let v = Some (Some (1, (2,Some 3)))
            match v with
            | Some (Some (_, (_, Some $(?$0value$I$)))) -> ()
            | _ -> ()
            """
            (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Required fromCursors; SpecialRules = [RemoveOptionFromType] })
        testCaseAsync "options & tuples in tuple" <|
          testExplicitType
            """
            let v = Some (Some (1, (2,Some 3)))
            match v with
            | Some (Some (_, ($(?$0value$I$), Some _))) -> ()
            | _ -> ()
            """
            (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Optional fromCursors; SpecialRules = [RemoveOptionFromType] })

      ]
    ]
    testList "let function" [
      testList "params" [
        testCaseAsync "let f value = value + 1" <|
          testExplicitType
            """
            let f $($0value$I$) = value + 1
            """
            (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Required fromCursors; SpecialRules = [] })
        testCaseAsync "let f (value) = value + 1" <|
          testExplicitType
            """
            let f ($($0value$I$)) = value + 1
            """
            (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Exist fromCursors; SpecialRules = [] })
        testCaseAsync "let f (value: int) = value + 1" <|
          testExplicitType
            """
            let f ($0value: int) = value + 1
            """
            (ExplicitType.Exists)

        testCaseAsync "let f a value b = ..." <|
          testExplicitType
            """
            let f a $($0value$I$) b = value + b + a + 1
            """
            (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Required fromCursors; SpecialRules = [] })
        testCaseAsync "let f a (value: int) b = ..." <|
          testExplicitType
            """
            let f a ($0value: int) b = value + a + b + 1
            """
            (ExplicitType.Exists)
      ]
      testList "function" [
        // not (yet?) supported
        testCaseAsync "let f value = value + 1" <|
          testExplicitType'
            """
            let $0f value = value + 1
            """
            None
      ]

      testList "member" [
        testCaseAsync "static member DoStuff value =" <|
          testExplicitType
            """
            type A =
              static member DoStuff $($0value$I$) = value + 1
            """
            (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Required fromCursors; SpecialRules = [] })
        testCaseAsync "static member DoStuff (value) =" <|
          testExplicitType
            """
            type A =
              static member DoStuff ($($0value$I$)) = value + 1
            """
            (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Exist fromCursors; SpecialRules = [] })
        testCaseAsync "static member DoStuff (value: int) =" <|
          testExplicitType
            """
            type A =
              static member DoStuff ($0value: int) = value + 1
            """
            (ExplicitType.Exists)
        testCaseAsync "static member DoStuff a value b =" <|
          testExplicitType
            """
            type A =
              static member DoStuff a $($0value$I$) b = value + a + b + 1
            """
            (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Required fromCursors; SpecialRules = [] })
        testCaseAsync "static member DoStuff(a, value, b) =" <|
          testExplicitType
            """
            type A =
              static member DoStuff(a, $($0value$I$), b) = value + a + b + 1
            """
            (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Optional fromCursors; SpecialRules = [] })

        testCaseAsync "member x.DoStuff(a, value, b) =" <|
          testExplicitType
            """
            type A() =
              member x.DoStuff(a, $($0value$I$), b) = value + a + b + 1
            """
            (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Optional fromCursors; SpecialRules = [] })
        testCaseAsync "doesn't handle this" <|
          testExplicitType'
            """
            type A() =
              member $0x.DoStuff(a, value, b) = value + a + b + 1
            """
            None
        // not (yet?) supported
        testCaseAsync "doesn't handle function" <|
          testExplicitType'
            """
            type A() =
              member x.$0DoStuff(a, value, b) = value + a + b + 1
            """
            None
      ]
      testList "secondary ctor" [
        testCaseAsync "new (a, value) =" <|
          testExplicitType
            """
            type A(a: int) =
              new (a, $($0value$I$)) = A(a+value)
              member _.DoStuff(v) = v + a + 1
            """
            (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Optional fromCursors; SpecialRules = [] })
            
      ]
    ]
    testList "pattern match" [
      testCaseAsync "| value ->" <|
        testExplicitType
          """
          match 4 with
          | $($0value$I$) -> ()
          """
          (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Required fromCursors; SpecialRules = [] })
      testCaseAsync "| Some value ->" <|
        testExplicitType
          """
          match 4 with
          | Some $($0value$I$) -> ()
          | _ -> ()
          """
          (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Required fromCursors; SpecialRules = [] })
      testCaseAsync " Choice1Of2 value | Choice2Of2 value ->" <|
        testExplicitType
          """
          match Choice1Of2 3 with
          | Choice1Of2 value | Choice2Of2 $($0value$I$) -> ()
          """
          (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Required fromCursors; SpecialRules = [] })
      testCaseAsync "| _ as value ->" <|
        testExplicitType
          """
          match 4 with
          | _ as $($0value$I$) -> ()
          """
          (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Required fromCursors; SpecialRules = [] })
      testCaseAsync "| value as _ ->" <|
        testExplicitType
          """
          match 4 with
          | $($0value$I$) as _ -> ()
          """
          (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Optional fromCursors; SpecialRules = [] })
      testCaseAsync "| (_, value) ->" <|
        testExplicitType
          """
          match (4,2) with
          | (_, $($0value$I$)) -> ()
          """
          (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Optional fromCursors; SpecialRules = [] })
      testCaseAsync "| [value] ->" <|
        testExplicitType
          """
          match [] with
          | [$($0value$I$)] -> ()
          | _ -> ()
          """
          (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Optional fromCursors; SpecialRules = [] })
      testCaseAsync "| [_; value; _] ->" <|
        testExplicitType
          """
          match [] with
          | [_; $($0value$I$); _] -> ()
          | _ -> ()
          """
          (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Optional fromCursors; SpecialRules = [] })

      testList "match!" [
      testCaseAsync "| value ->" <|
        testExplicitType
          """
          async {
              match async {return 2} with
              | $($0value$I$) -> ()
          }
          |> ignore
          """
          (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Required fromCursors; SpecialRules = [] })

      ]
    ]
    testList "lambda" [
      testCaseAsync "fun value ->" <|
        testExplicitType
          """
          let f = fun $($0value$I$) -> value + 1
          """
          (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Required fromCursors; SpecialRules = [] })
      testCaseAsync "fun a value b ->" <|
        testExplicitType
          """
          let f = fun a $($0value$I$) b -> value + a + b + 1
          """
          (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Required fromCursors; SpecialRules = [] })
      testCaseAsync "fun (a, value, b) ->" <|
        testExplicitType
          """
          let f = fun (a, $($0value$I$), b) -> value + a + b + 1
          """
          (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Optional fromCursors; SpecialRules = [] })
      testList "let f a = fun b -> a + b + 1" [
        testCaseAsync "f" <|
          testExplicitType'
            """
            let $0f a = fun b -> a + b + 1
            """
            None
        testCaseAsync "a" <|
          testExplicitType
            """
            let f $($0a$I$) = fun b -> a + b + 1
            """
            (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Required fromCursors; SpecialRules = [] })
        testCaseAsync "b" <|
          testExplicitType
            """
            let f a = fun $($0b$I$)-> a + b + 1
            """
            (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Required fromCursors; SpecialRules = [] })

        testCaseAsync "f typed" <|
          testExplicitType'
            """
            let $0f a : int -> int = fun b -> a + b + 1
            """
            None
        testCaseAsync "a typed" <|
          testExplicitType
            """
            let f ($0a: int) = fun b -> a + b + 1
            """
            (ExplicitType.Exists)
        testCaseAsync "b typed" <|
          testExplicitType
            """
            let f a = fun ($0b: int) -> a + b + 1
            """
            (ExplicitType.Exists)
      ]
    ]
    testList "SimplePats" [
      // primary ctor args & lambda args
      // * primary ctor: no parens allowed
      // * lambda args: absolutely fucked up -- in fact so fucked up I use the f-word to describe how fucked up it is...
      //   TODO: remove `fuck`s
      //     TODO: replace with something stronger?
      //   -> special handling for `SynExpr.Lambda` and then `parsedData |> fst` (-> `SynPat` instead of `SynSimplePat`)

      testList "primary ctor" [
        testCaseAsync "T(a)" <|
          testExplicitType
            """
            type A($0a$I) =
              member _.F(b) = a + b + 1
            """
            (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Forbidden; SpecialRules = [] })
        testCaseAsync "T(a: int)" <|
          testExplicitType
            """
            type A($0a: int) =
              member _.F(b) = a + b + 1
            """
            (ExplicitType.Exists)
        testCaseAsync "T(a, b, c, d)" <|
          testExplicitType
            """
            type A(a, b, $0c$I, d) =
              member _.F(b) = a + b + 1
            """
            (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Forbidden; SpecialRules = [] })
        testCaseAsync "T(a, b, c: int, d)" <|
          testExplicitType
            """
            type A(a, b, $0c: int, d) =
              member _.F(b) = a + b + 1
            """
            (ExplicitType.Exists)
        testCaseAsync "T([<Attr>]a)" <|
          testExplicitType
            """
            type Attr() =
              inherit System.Attribute() 
            type A([<Attr>]$0a$I) =
              member _.F(b) = a + b + 1
            """
            (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Forbidden; SpecialRules = [] })
      ]
    ]

    testList "detect existing annotation" [
      testCaseAsync "let (value): int =" <|
          testExplicitType
            """
            let ($0value): int = 3
            """
            (ExplicitType.Exists)
      testCaseAsync "let ((value)): int =" <|
          testExplicitType
            """
            let (($0value)): int = 3
            """
            (ExplicitType.Exists)
    ]

    testList "trigger location" [
      testList "let f p = p + 2" [
        testCaseAsync "trigger for p binding" <|
          testExplicitType
            """
            let f $($0p$I$) = p + 2
            """
            (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Required fromCursors; SpecialRules = [] })
        testCaseAsync "doesn't trigger for f binding" <|
          // ENHANCEMENT: handle
          testExplicitType'
            """
            let $0f p = p + 2
            """
            None
        testCaseAsync "doesn't trigger for p usage" <|
          testExplicitType'
            """
            let f p = $0p + 2
            """
            None
      ]
      testCaseAsync "nested let" <|
        testExplicitType
          """
          let f a b =
            let res =
              let $($0t$I$) = a + b
              t + a
            res + 3
          """
          (ExplicitType.Missing { Ident=fromCursorAndInsert; InsertAt=fromCursor; Parens=Parens.Optional fromCursors; SpecialRules = [] })
    ]
  ]
