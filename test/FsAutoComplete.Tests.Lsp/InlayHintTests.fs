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

  let from (text, (line, char), kind) : LSPInlayHint =
    { Text =
        match kind with
        | InlayHintKind.Type -> ": " + text
        | InlayHintKind.Parameter -> text + " ="
      // this is for truncated text, which we do not currently hit in our tests
      // TODO: add tests to cover this case
      InsertText =
        match kind with
        | InlayHintKind.Type -> Some(": " + text)
        | InlayHintKind.Parameter -> None
      Pos = { Line = line; Character = char }
      Kind = kind }

  let check (server: Async<Server>) (documentText: string) (expectedHints: _ list) =
    async {
      let (range, text) =
        documentText
        |> Text.trimTripleQuotation
        |> Cursor.assertExtractRange

      let! (doc, diags) = server |> Server.createUntitledDocument text
      use doc = doc // ensure doc gets closed (disposed) after test

      match diags with
      | [||] -> ()
      | diags -> failtest $"Should not have had check errors, but instead had %A{diags}"

      let! actual = Document.inlayHintsAt range doc
      let expected = expectedHints |> List.map from |> Array.ofList
      Expect.equal actual expected "Expected the given set of hints"
    }

let tests state =
  serverTestList (nameof Core.InlayHints) state defaultConfigDto None (fun server ->
    [ testCaseAsync "let-bound function parameter type hints"
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

      testCaseAsync "parameter names aren't yet implemented, will fail when we update FCS"
      <| InlayHints.check server """$0 System.Environment.GetEnvironmentVariable "Blah" |> ignore$0""" []

      testCaseAsync "doesn't show hint for well-known parameter names"
      <| InlayHints.check
           server
           """$0sprintf "thing %s" "blah" |> ignore$0"""
           []

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

      testCaseAsync "no type hint for an explicitly-typed binding"
      <| InlayHints.check
           server
           """$0let s: string = "hi"$0"""
           []

      testCaseAsync "no hint for a function with a short parameter name" <|
        InlayHints.check server """
        // shows that no parameter name hint is shown for a function with a short parameter name
        let someFunction s = s
        let noHintForShortParameter = $0someFunction "hi"$0
        """ []
      ])
