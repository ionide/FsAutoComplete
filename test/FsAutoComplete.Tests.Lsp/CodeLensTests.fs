module FsAutoComplete.Tests.CodeLens

open Expecto
open FsToolkit.ErrorHandling
open Helpers
open Ionide.LanguageServerProtocol.Types
open Utils.Server
open Utils.ServerTests
open Utils.ServerTests
open Utils.TextEdit
open Utils.Utils
open Utils.CursorbasedTests
open Utils.Tests.TextEdit

module private CodeLens =
  let assertNoDiagnostics (ds: Diagnostic []) =
    match ds with
    | [||] -> Ok()
    | ds -> Error $"Expected no diagnostics, but got %A{ds}"

  let check server text checkLenses =
    asyncResult {
      let textRange, text =
        text
        |> Text.trimTripleQuotation
        |> Cursor.assertExtractRange

      let! (doc, diags) = Server.createUntitledDocument text server
      do! assertNoDiagnostics diags

      let p: CodeLensParams = { TextDocument = doc.TextDocumentIdentifier }

      let! lenses =
        doc.Server.Server.TextDocumentCodeLens p
        |> AsyncResult.mapError string

      let! resolved =
        Option.toList lenses
        |> Array.concat
        |> List.ofArray
        |> List.traverseAsyncResultA doc.Server.Server.CodeLensResolve
        |> AsyncResult.mapError string

      let lensesForRange =
        resolved
        |> List.filter (fun lens -> Range.overlapsStrictly textRange lens.Range)

      checkLenses lensesForRange
    }
    |> AsyncResult.foldResult id (fun e -> failtest $"{e}")


let tests state =
  serverTestList (nameof CodeLens) state defaultConfigDto None (fun server ->
    [ testCaseAsync
        "can show codelens for type annotation" <|
          CodeLens.check server
            """
            module X =
              $0let func x = x + 1$0
            """ (fun lenses ->
                Expect.hasLength lenses 2 "should have a type lens and a reference lens"
                let typeLens = lenses[0]
                Expect.equal typeLens.Command.Value.Title "int -> int" "first lens should be a type hint of int to int"
                Expect.isNone typeLens.Command.Value.Arguments "No data required for type lenses"
                Expect.equal typeLens.Command.Value.Command "" "No command for type lenses"
            )

      testCaseAsync
        "can show codelens for reference count" <|
          CodeLens.check server
            """
            module X =
              $0let func x = x + 1$0
            """ (fun lenses ->
                Expect.hasLength lenses 2 "should have a type lens and a reference lens"
                let referenceLens = lenses[1]
                Expect.equal referenceLens.Command.Value.Title "0 References" "second lens should show the references"
                Expect.isSome referenceLens.Command.Value.Arguments "Reference lenses should carry data"
                Expect.equal referenceLens.Command.Value.Command "fsharp.showReferences" "Reference lens should call command"

            )
    ]
    )

