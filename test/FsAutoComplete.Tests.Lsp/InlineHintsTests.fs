module FsAutoComplete.Tests.InlineHints

open Expecto
open Helpers
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open Utils.Server
open Utils.Utils
open Utils.TextEdit
open Utils.ServerTests
open Helpers.Expecto.ShadowedTimeouts

let private testInlineHints (server: CachedServer) text checkResp =
  async {
    let range, text = text |> Text.trimTripleQuotation |> Cursor.assertExtractRange

    let! doc, _diags = server |> Server.createUntitledDocument text
    use doc = doc

    let inlineValueRequest: InlineValueParams =
      { Context = { FrameId = 0; StoppedLocation = range }
        Range = range
        TextDocument = doc.TextDocumentIdentifier
        WorkDoneToken = None }

    let! resp = doc.Server.Server.TextDocumentInlineValue inlineValueRequest
    checkResp resp
  }

let good =
  """
let test value =
  $0
  value
  |> String.replicate 3
  |> String.filter (fun c -> Char.IsAscii(c))
  |> String.length
  $0
value "foo"
"""

let tests state =
  serverTestList "inline hints" state defaultConfigDto None (fun server ->
    [ testCaseAsync
        "Gets inline hints for simple pipeline"
        (testInlineHints server good (fun resp ->
          Expect.isOk resp "should get inline hints for valid code fragment"
          let resp = resp |> Result.defaultWith (fun _ -> failwith "Unpossible!")
          Expect.isSome resp "should get inline hints for valid code fragment"
          let resp = resp |> Option.get
          Expect.hasLength resp 4 "THERE ARE FOUR LIGHTS!!"

          let hints =
            resp
            |> Array.choose (function
              | U3.C1 hint -> Some hint.Text
              | _ -> None)

          Expect.containsAll hints [ nameof string; nameof int ] "should be 3 strings and an int")) ])
