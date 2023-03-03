module FsAutoComplete.Tests.SignatureHelp

open Expecto
open Helpers
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open Utils.Server
open Utils.Utils
open Utils.TextEdit
open Utils.ServerTests
open Helpers.Expecto.ShadowedTimeouts

type private TriggerType =
  | Manual
  | Char of char

let private testSignatureHelp' (server: CachedServer) text pos triggerType checkResp =
  async {
    let! (doc, diags) = server |> Server.createUntitledDocument text
    use doc = doc

    let sigHelpRequest: SignatureHelpParams =
      { TextDocument = doc.TextDocumentIdentifier
        Position = pos
        Context =
          Some
            { TriggerKind =
                match triggerType with
                | Manual -> SignatureHelpTriggerKind.Invoked
                | Char c -> SignatureHelpTriggerKind.TriggerCharacter
              TriggerCharacter =
                match triggerType with
                | Manual -> None
                | Char c -> Some c
              IsRetrigger = false
              ActiveSignatureHelp = None } }

    let! resp = doc.Server.Server.TextDocumentSignatureHelp sigHelpRequest

    checkResp resp
  }

let private wantSignatureHelp = Flip.Expect.wantOk "unexpected request error"

let private testSignatureHelp (server: CachedServer) textWithCursor triggerType checkResp =
  async {
    let (pos, text) =
      textWithCursor
      |> Text.trimTripleQuotation
      |> Cursor.assertExtractPosition

    let checkResp = wantSignatureHelp >> checkResp
    return! testSignatureHelp' server text pos triggerType checkResp
  }

let private functionApplicationEdgeCasesTests server =
  testList
    "function application edge cases"
    [ testCaseAsync "issue 742 - signature help on functions counts the prior parameters"
      <| testSignatureHelp server """
      id 12 $0// note: keep the trailing space after '12' on this line
      """ (Char ' ') (fun resp -> Expect.isNone resp "there should be no sighelp on this location")
      testCaseAsync "issue 744 - signature help suggests parameters other than the first"
      <| testSignatureHelp server """
      let f a b = ()

      f 1 $0 // preserve last space
      """ Manual (fun resp ->
        match resp with
        | Some sigHelp -> Expect.equal sigHelp.ActiveParameter (Some 1) "should have suggested the second parameter"
        | None -> failwithf "There should be sighelp for this position")
      ptestCaseAsync "issue 745 - signature help shows tuples in parens"
      <| testSignatureHelp server """
      let f (a, b) = ()

      f $0 // preserve last space
      """ Manual (fun resp ->
        match resp with
        | Some sigHelp ->
          Expect.equal sigHelp.ActiveSignature (Some 0) "should have suggested the first overload"

          Expect.equal
            sigHelp.Signatures.[0].Label
            "val f : (a:'a * b:'b) -> unit"
            "should highlight tuples with parens in signature help"
        | None -> failwithf "There should be sighelp for this position")
      testCaseAsync "issue 746 - signature help understands piping for parameter application"
      <| testSignatureHelp server """
      [1..10] |> List.map id $0// keep trailing space
      """ Manual (fun resp ->
        Expect.isNone
          resp
          "there should be no suggestions at this position, since we've provided all parameters to List.map")
      testCaseAsync "issue 747 - signature help is provided for the most inner function"
      <| testSignatureHelp server """
      let f a b c = ()

      let g a b = ()

      f (g $0)
      """ Manual (fun resp ->
        Expect.isSome resp "should have provided signature help"
        let resp = resp.Value
        let methodsig = resp.Signatures.[0]
        Expect.stringStarts methodsig.Label "val g: " "should have provided signatures for function g")
      testCaseAsync "issue 748 - signature help is provided for functions inside CEs"
      <| testSignatureHelp server """
      let _ =
          async {
              let x = sqrt $0// trigger here
              return ()
          }
      """ Manual (fun resp ->
        Expect.isSome resp "should have provided signature help"
        let resp = resp.Value
        let methodsig = resp.Signatures.[0]
        Expect.stringStarts methodsig.Label "val sqrt: " "should have provided signatures for sqrt")
      testCaseAsync "issue 750 - signature help should have tips when triggered on generic type applications"
      <| testSignatureHelp server """
open System

/// Tries to cast an object to a given type; throws with the given error message if it fails.
let tryConvert<'T> (descriptor: string) (value: obj) : 'T =
    try
        Convert.ChangeType(value, typeof<'T>) :?> 'T
    with ex ->
        let msg = sprintf "Unable to convert '%s' with value %A" descriptor value
        raise (new Exception(msg, ex))

// Tooltip does NOT show when the generic argument is present:
let result = (box 123) |> tryConvert<string> $0(* trigger at > char, no sigdata occurs *)

      """ Manual (fun resp ->
        Expect.isSome resp "should get sigdata when triggered on applications"
        let resp = resp.Value
        let signatures = resp.Signatures.[0]
        Expect.stringStarts signatures.Label "val tryConvert:" "should have given help for tryConvert") ]

let private overloadEdgeCasesTests server =
  testList
    "overload edge cases"
    [ testList
        "unattached parens"
        [ let text = "let ___ = new System.IO.MemoryStream (  )"

          for c in 37..39 do
            let pos = { Line = 0; Character = c }

            testCaseAsync $"Can get overloads at whitespace position {c - 37} of unattached parens"
            <| testSignatureHelp'
                 server
                 text
                 pos
                 Manual
                 (wantSignatureHelp
                  >> fun resp ->
                       Expect.isSome
                         resp
                         $"Should get some signature overloads at position %A{pos} on file Overloads.fsx"

                       Expect.isNonEmpty resp.Value.Signatures "Should have some overloads") ]
      testList
        "attached parens"
        [ let text = "let _____ = new System.IO.MemoryStream(42)"

          for c in 39..41 do
            let pos = { Line = 0; Character = c }

            testCaseAsync $"Can get overloads at whitespace position {c - 39} of attached parens"
            <| testSignatureHelp'
                 server
                 text
                 pos
                 Manual
                 (wantSignatureHelp
                  >> fun resp ->
                       Expect.isSome
                         resp
                         $"Should get some signature overloads at position %A{pos} on file Overloads.fsx"

                       Expect.isNonEmpty resp.Value.Signatures "Should have some overloads") ] ]

let issuesTests server =
  testList
    "issues"
    [ testCaseAsync "issue #950 - exception when in first column in first line"
      <| testSignatureHelp' server "Syste" { Line = 0; Character = 0 } Manual (fun r ->
        let err = Expect.wantError r "No signature help at first position"
        Expect.equal err.Message "Couldn't find previous non-whitespace char" "Should fail because no prev char")
      testCaseAsync "type names aren't backticked"
      <| testSignatureHelp server """
      let count (x: int) = x

      let _ = count $010
      """ TriggerType.Manual (fun sigs ->
        Expect.isSome sigs "Should have sigs for backtick check"
        let sigText = sigs.Value.Signatures[0].Label

        Expect.equal
          sigText
          "val count: x: int -> int"
          "Should have no backticks because signatures are only ever rendered in `code` form")
      testCaseAsync "issue #1040" // IndexOutOfRangeException
        <| testSignatureHelp server "().ToString(\n\n,$0\n)" Manual (fun sigs ->
          Expect.isSome sigs "Should have sigs"
        ) ]

let tests state =
  serverTestList "signature help" state defaultConfigDto None (fun server ->
    [ functionApplicationEdgeCasesTests server
      overloadEdgeCasesTests server
      testList
        "parameter position detect"
        [ testCaseAsync "Can suggest second parameter when on the second parameter"
          <| testSignatureHelp server """
          System.IO.Directory.EnumerateDirectories("/var", $0) // signature help triggered at the space after the comma should suggest overload 1, not overload 0
          """ Manual (fun resp ->
            Expect.isSome resp "should get sigdata when triggered on applications"
            Expect.equal (Some 1) resp.Value.ActiveSignature "should have suggested the second overload") ]

      issuesTests server ])
