module FsAutoComplete.Tests.CodeLens

open Expecto
open FsToolkit.ErrorHandling
open Helpers
open Ionide.LanguageServerProtocol.Types
open Utils.Server
open Utils.ServerTests
open Utils.TextEdit
open Utils.Utils
open Utils.CursorbasedTests
open Utils.Tests.TextEdit
open Newtonsoft.Json.Linq

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

      checkLenses (doc, lensesForRange)
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
            """ (fun (doc, lenses) ->
                Expect.hasLength lenses 2 "should have a type lens and a reference lens"
                let typeLens = lenses[0]
                Expect.equal typeLens.Command.Value.Title "int -> int" "first lens should be a type hint of int to int"
                Expect.isNone typeLens.Command.Value.Arguments "No data required for type lenses"
                Expect.equal typeLens.Command.Value.Command "" "No command for type lenses"
            )

      testCaseAsync
        "can show codelens for 0 reference count" <|
          CodeLens.check server
            """
            module X =
              $0let func x = x + 1$0
            """ (fun (doc, lenses) ->
                Expect.hasLength lenses 2 "should have a type lens and a reference lens"
                let referenceLens = lenses[1]
                let emptyCommand = Some { Title = "0 References"; Arguments = None; Command = "" }
                Expect.equal referenceLens.Command emptyCommand "There should be no command or args for zero references"
            )
      testCaseAsync
        "can show codelens for multi reference count" <|
          CodeLens.check server
            """
            module X =
              $0let func x = x + 1$0

              let doThing () = func 1
            """ (fun (doc, lenses) ->
                Expect.hasLength lenses 2 "should have a type lens and a reference lens"
                let referenceLens = lenses[1]
                Expect.isSome referenceLens.Command "There should be a command for multiple references"
                let referenceCommand = referenceLens.Command.Value
                Expect.equal referenceCommand.Title "1 References" "There should be a title for multiple references"
                Expect.equal referenceCommand.Command "fsharp.showReferences" "There should be a command for multiple references"
                Expect.isSome referenceCommand.Arguments "There should be arguments for multiple references"
                let args = referenceCommand.Arguments.Value
                Expect.equal args.Length 3 "There should be 2 args"
                let filePath, triggerPos, referenceRanges =
                  args[0].Value<string>(),
                  (args[1] :?> JObject).ToObject<Ionide.LanguageServerProtocol.Types.Position>(),
                  (args[2] :?> JArray) |> Seq.map (fun t -> (t:?>JObject).ToObject<Ionide.LanguageServerProtocol.Types.Location>()) |> Array.ofSeq
                Expect.equal filePath doc.Uri "File path should be the doc we're checking"
                Expect.equal triggerPos { Line = 1; Character = 8 } "Position should be 1:8"
                Expect.hasLength referenceRanges 1 "There should be 1 reference range for the `func` function"
                Expect.equal referenceRanges[0] { Uri = doc.Uri; Range = { Start = { Line = 3; Character = 19 }; End = { Line = 3; Character = 23 } } } "Reference range should be 0:0"
            )
      testCaseAsync "can show reference counts for 1-character identifier" <|
        CodeLens.check server
          """
          $0let f () = ""$0
          """ (fun (doc, lenses) ->
                Expect.hasLength lenses 2 "should have a type lens and a reference lens"
                let referenceLens = lenses[1]
                Expect.isSome referenceLens.Command "There should be a command for multiple references"
                let referenceCommand = referenceLens.Command.Value
                Expect.equal referenceCommand.Title "0 References" "There should be a title for multiple references"
                Expect.equal referenceCommand.Command "" "There should be no command for multiple references"
                Expect.isNone referenceCommand.Arguments "There should be arguments for multiple references"
          )
    ]
    )

