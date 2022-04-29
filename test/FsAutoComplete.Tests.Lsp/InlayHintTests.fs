module FsAutoComplete.Tests.InlayHintTests

open Expecto
open System
open System.IO
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open Helpers
open FsToolkit.ErrorHandling
open Utils.ServerTests
open Expecto.Logging.Global
open FsAutoComplete.Core
open FsAutoComplete.Lsp

module InlayHints =
  open Utils.Server
  open Utils.Tests
  open Utils.Utils
  open Utils.TextEdit
  open FSharpx.Control

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

      let! actual = Document.inlayHintsAt range doc
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

let param (name: string) = (name, InlayHintKind.Parameter)
let ty (name: string) = (name, InlayHintKind.Type)

let tests state =
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
          let f specialNumber = ()
          let special = 2

          $0f special$0
          """
          [  ]
      testCaseAsync "hide: variable postfix of param" <|
        InlayHints.checkRange server
          """
          let f specialNumber = ()
          let number = 2

          $0f number$0
          """
          [  ]
      //todo: or hide?
      testCaseAsync "show: variable infix of param" <|
        InlayHints.checkRange server
          """
          let f extraSpecialNumber = ()
          let special = 2

          $0f $0special$0
          """
          [ param "extraSpecialNumber" ]
      //todo: or hide?
      testCaseAsync "show: variable prefix of param, but no word boundary" <|
        InlayHints.checkRange server
          """
          let f specialnumber = ()
          let special = 2

          $0f $0special$0
          """
          [ param "specialnumber" ]
      //todo: or hide?
      testCaseAsync "show: variable postfix of param, but no word boundary" <|
        InlayHints.checkRange server
          """
          let f specialnumber = ()
          let number = 2

          $0f $0number$0
          """
          [ param "specialnumber" ]

      testCaseAsync "hide: arg is prefix of param with leading _" <|
        InlayHints.checkRange server
          """
          let f _specialNumber = ()
          let special = 2

          $0f special$0
          """
          []
      testCaseAsync "hide: arg is postfix of param with trailing '" <|
        InlayHints.checkRange server
          """
          let f specialNumber' = ()
          let number = 2

          $0f number$0
          """
          []
      testCaseAsync "hide: arg is prefix of param with trailing ' in arg" <|
        InlayHints.checkRange server
          """
          let f specialNumber = ()
          let special' = 2

          $0f special'$0
          """
          []

      testCaseAsync "hide: param prefix of arg" <|
        InlayHints.checkRange server
          """
          let f special = ()
          let specialNumber = 2

          $0f specialNumber$0
          """
          []
      testCaseAsync "hide: param postfix of arg" <|
        InlayHints.checkRange server
          """
          let f number = ()
          let specialNumber = 2

          $0f specialNumber$0
          """
          []

      testCaseAsync "hide: arg is field access with same name as param (upper case start)" <|
        InlayHints.checkRange server
          """
          type Data = {
            Number: int
          }
          let f number = ()
          let data: Data = { Number = 2 }

          $0f data.Number$0
          """
          []
      testCaseAsync "hide: arg is field access with same name as param (lower case start)" <|
        InlayHints.checkRange server
          """
          type Data = {
            number: int
          }
          let f number = ()
          let data: Data = { number = 2 }

          $0f data.number$0
          """
          []
      testCaseAsync "hide: arg is field access prefix of param (upper case start)" <|
        InlayHints.checkRange server
          """
          type Data = {
            Special: int
          }
          let f specialNumber = ()
          let data: Data = { Special = 2 }

          $0f data.Special$0
          """
          []
      testCaseAsync "hide: arg is field access, param is prefix of arg" <|
        InlayHints.checkRange server
          """
          type Data = {
            SpecialNumber: int
          }
          let f special = ()
          let data: Data = { SpecialNumber = 2 }

          $0f data.SpecialNumber$0
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
      //todo: or hide? based on: what is last? but then (`alpha <| 1`, `1 |> alpha 2`, etc?) -> too complex to detect
      testCaseAsync "show: expr including param name in parens" <|
        InlayHints.checkRange server
          """
          let f alpha = ()
          let alpha x = x + 3

          $0f $0(1 |> alpha)$0
          """
          [ param "alpha" ]
          
      //todo: inspect most left/right identifier? extract function name? look for left of `.`? use ast?
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

        
    ]
  ])
