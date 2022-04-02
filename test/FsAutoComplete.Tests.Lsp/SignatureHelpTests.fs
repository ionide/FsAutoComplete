module FsAutoComplete.Tests.SignatureHelp

open Expecto
open System.IO
open Helpers
open Ionide.LanguageServerProtocol.Types
open FsToolkit.ErrorHandling
open FsAutoComplete

type TriggerType =
  | Manual
  | Char of char

let private server state =
  async {
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "SignatureHelp")

    return! serverInitialize path defaultConfigDto state
  }

let coreTestSignatureHelp hide title file (line, char) triggerType checkResp server =

  (if hide then
     ptestCaseAsync
   else
     testCaseAsync)
    title
    (async {
      let! (server: Lsp.FSharpLspServer, event: ClientEvents) = server

      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "SignatureHelp")

      let path = Path.Combine(path, file)
      let tdop: DidOpenTextDocumentParams = { TextDocument = loadDocument path }
      do! server.TextDocumentDidOpen tdop

      do!
        waitForParseResultsForFile file event
        |> AsyncResult.foldResult id ignore

      let sigHelpRequest: SignatureHelpParams =
        { TextDocument = { Uri = Path.FilePathToUri path }
          Position = { Line = line; Character = char }
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

      let! resp =
        server.TextDocumentSignatureHelp sigHelpRequest
        |> AsyncResult.foldResult id (fun e -> failwithf "unexpected request error %A" e)

      checkResp resp
    })

let ptestSignatureHelp = coreTestSignatureHelp true
let testSignatureHelp = coreTestSignatureHelp false

let test742 =
  testSignatureHelp
    "issue 742 - signature help on functions counts the prior parameters"
    "742.fsx"
    (0, 6)
    (Char ' ')
    (fun resp -> Expect.isNone resp "there should be no sighelp on this location")

let test744 =
  testSignatureHelp
    "issue 744 - signature help suggests parameters other than the first"
    "744.fsx"
    (2, 4)
    Manual
    (fun resp ->
      match resp with
      | Some sigHelp -> Expect.equal sigHelp.ActiveParameter (Some 1) "should have suggested the second parameter"
      | None -> failwithf "There should be sighelp for this position")

let test745 =
  ptestSignatureHelp "issue 745 - signature help shows tuples in parens" "745.fsx" (2, 2) Manual (fun resp ->
    match resp with
    | Some sigHelp ->
      Expect.equal sigHelp.ActiveSignature (Some 0) "should have suggested the first overload"

      Expect.equal
        sigHelp.Signatures.[0].Label
        "val f : (a:'a * b:'b) -> unit"
        "should highlight tuples with parens in signature help"
    | None -> failwithf "There should be sighelp for this position")

let test746 =
  testSignatureHelp
    "issue 746 - signature help understands piping for parameter application"
    "746.fsx"
    (0, 24) (* the end of 'id' *)
    Manual
    (fun resp ->
      Expect.isNone
        resp
        "there should be no suggestions at this position, since we've provided all parameters to List.map")

let test747 =
  testSignatureHelp
    "issue 747 - signature help is provided for the most inner function"
    "747.fsx"
    (4, 5)
    Manual
    (fun resp ->
      Expect.isSome resp "should have provided signature help"
      let resp = resp.Value
      let methodsig = resp.Signatures.[0]
      Expect.stringStarts methodsig.Label "val g: " "should have provided signatures for function g")

let test748 =
  testSignatureHelp
    "issue 748 - signature help is provided for functions inside CEs"
    "748.fsx"
    (2, 21)
    Manual
    (fun resp ->
      Expect.isSome resp "should have provided signature help"
      let resp = resp.Value
      let methodsig = resp.Signatures.[0]
      Expect.stringStarts methodsig.Label "val sqrt: " "should have provided signatures for sqrt")

let test750 =
  testSignatureHelp
    "issue 750 - signature help should have tips when triggered on generic type applications"
    "750.fsx"
    (11, 45)
    Manual
    (fun resp ->
      Expect.isSome resp "should get sigdata when triggered on applications"
      let resp = resp.Value
      let signatures = resp.Signatures.[0]
      Expect.stringStarts signatures.Label "val tryConvert:" "should have given help for tryConvert")


let checkOverloadsAt pos name =
  testSignatureHelp name "Overloads.fsx" pos Manual (fun resp ->
    Expect.isSome resp $"Should get some signature overloads at position %A{pos} on file Overloads.fsx"
    Expect.isNonEmpty resp.Value.Signatures "Should have some overloads")

let tests state =
  let server = server state

  testList
    "signature help"
    [ testList
        "function application edge cases"
        ([ test742
           test744
           test745
           test746
           test747
           test748
           test750 ]
         |> List.map (fun f -> f server))
      testList
        "overload edge cases"
        [ for c in 37..39 do
            checkOverloadsAt (0, c) $"Can get overloads at whitespace position {c - 37} of unattached parens" server
          for c in 39..41 do
            checkOverloadsAt (1, c) $"Can get overloads at whitespace position {c - 39} of attached parens" server ]
      testList
        "parameter position detect"
        [ testSignatureHelp
            "Can suggest second parameter when on the second parameter"
            "ParameterPosition.fsx"
            (0, 49)
            Manual
            (fun resp ->
              Expect.isSome resp "should get sigdata when triggered on applications"
              Expect.equal (Some 1) resp.Value.ActiveSignature "should have suggested the second overload")
            server ] ]
