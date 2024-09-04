module FsAutoComplete.Tests.CodeLens

open FsAutoComplete.Utils
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
open Helpers.Expecto.ShadowedTimeouts
open System.IO

module private CodeLens =
  let examples = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "CodeLensProjectTests")

  module CodeLensPositionStaysAccurate =
    let dir = examples </> "CodeLens_position_stays_accurate"
    let project = dir </> "CodeLens_position_stays_accurate.fsproj"
    let programFile = dir </> "Program.fs"

  let assertNoDiagnostics (ds: Diagnostic[]) =
    match ds with
    | [||] -> Ok()
    | ds -> Error $"Expected no diagnostics, but got %A{ds}"

  let getLenses (doc: Document) =
    let p: CodeLensParams =
      { TextDocument = doc.TextDocumentIdentifier
        PartialResultToken = None
        WorkDoneToken = None }

    doc.Server.Server.TextDocumentCodeLens p
    |> AsyncResult.mapError string
    |> AsyncResult.map (fun r -> r |> Option.defaultValue [||])

  let getResolvedLenses (doc: Document) lenses =
    lenses
    |> List.ofArray
    |> List.traverseAsyncResultA doc.Server.Server.CodeLensResolve
    |> AsyncResult.mapError string

  let check server text (checkLenses: Document * CodeLens list * CodeLens array * CodeLens list -> Async<unit>) =
    asyncResult {
      let textRange, text = text |> Text.trimTripleQuotation |> Cursor.assertExtractRange

      let! (doc, diags) = Server.createUntitledDocument text server
      do! assertNoDiagnostics diags

      let! lenses = getLenses doc
      let! resolved = getResolvedLenses doc lenses

      let lensesForRange =
        resolved
        |> List.filter (fun lens -> Range.overlapsStrictly textRange lens.Range)

      do! checkLenses (doc, lensesForRange, lenses, resolved)
    }
    |> AsyncResult.foldResult id (fun e -> failtest $"{e}")


let projectBasedTests state =
  testList "ProjectBased" [
    serverTestList ("CodeLensPositionStaysAccurate") state defaultConfigDto (Some CodeLens.CodeLensPositionStaysAccurate.dir) (fun server -> [

      testCaseAsync "can show codelens after adding newlines to code"
      <| (asyncResult {
            let program = CodeLens.CodeLensPositionStaysAccurate.programFile
            let! (doc, _diags) = Server.openDocument program server

            let! unresolved = CodeLens.getLenses doc
            let! resolved = CodeLens.getResolvedLenses doc unresolved

            let references =
              resolved
              |> List.filter (fun lens -> lens.Command |>Option.exists (fun c -> c.Title.EndsWith "References"))
              |> List.sortBy (fun lens -> lens.Range.Start.Line)

            Expect.hasLength references 2 "should have a reference lens"

            let lens1 = references.[0]
            let lens1Range : Range = {
              Start = { Line = 1u; Character = 6u }
              End = { Line = 1u; Character = 20u }
            }

            Expect.equal lens1.Range lens1Range "Lens 1 should be at 1:6-1:20"

            let lens2 = references.[1]
            let lens2Range : Range = {
              Start = { Line = 3u; Character = 6u }
              End = { Line = 3u; Character = 25u }
            }

            Expect.equal lens2.Range lens2Range "Lens 2 should be at 3:6-3:25"

            do! doc.Server.Server.TextDocumentDidChange({
              TextDocument = doc.VersionedTextDocumentIdentifier
              ContentChanges = [| U2.C1 {
                Range = { Start = { Line = 2u; Character = 0u }; End = { Line = 2u; Character = 0u }; }
                RangeLength = None
                Text = "\n\n"
              } |]
            })

            let! nextLens = CodeLens.getLenses doc
            let! resolvedNextLens = CodeLens.getResolvedLenses doc nextLens

            let references =
              resolvedNextLens
              |> List.filter (fun lens -> lens.Command |>Option.exists (fun c -> c.Title.EndsWith "References"))
              |> List.sortBy (fun lens -> lens.Range.Start.Line)

            let lens1 = references.[0]
            let lens1Range : Range = {
              Start = { Line = 1u; Character = 6u }
              End = { Line = 1u; Character = 20u }
            }

            Expect.equal lens1.Range lens1Range "Lens 1 should be at 1:6-1:20"

            let lens2 = references.[1]
            let lens2Range : Range = {
              Start = { Line = 5u; Character = 6u }
              End = { Line = 5u; Character = 25u }
            }

            Expect.equal lens2.Range lens2Range "Lens 2 should be at 5:6-5:25"

            return ()
      }
      |> AsyncResult.foldResult id (fun e -> failtest $"{e}" ))

    ]
    )
  ]

let tests state =

  testList (nameof CodeLens) [
    projectBasedTests state
    serverTestList "scriptTests" state defaultConfigDto None (fun server ->
      [ testCaseAsync "can show codelens for type annotation"
        <| CodeLens.check server """
              module X =
                $0let func x = x + 1$0
              """ (fun (_doc, lenses, _unresolved, _resolved) -> async {
          Expect.hasLength lenses 2 "should have a type lens and a reference lens"
          let typeLens = lenses[0]
          Expect.equal typeLens.Command.Value.Title "int -> int" "first lens should be a type hint of int to int"
          Expect.isNone typeLens.Command.Value.Arguments "No data required for type lenses"
          Expect.equal typeLens.Command.Value.Command "" "No command for type lenses" })

        testCaseAsync "can show codelens for 0 reference count"
        <| CodeLens.check server """
              module X =
                $0let func x = x + 1$0
              """ (fun (_doc, lenses, _unresolved, _resolved) ->  async {
          Expect.hasLength lenses 2 "should have a type lens and a reference lens"
          let referenceLens = lenses[1]

          let emptyCommand =
            Some
              { Title = "0 References"
                Arguments = None
                Command = "" }

          Expect.equal referenceLens.Command emptyCommand "There should be no command or args for zero references" })
        testCaseAsync "can show codelens for multi reference count"
        <| CodeLens.check server """
              module X =
                $0let func x = x + 1$0

                let doThing () = func 1
              """ (fun (doc, lenses, _unresolved, _resolved) -> async {


          Expect.hasLength lenses 2 "should have a type lens and a reference lens"
          let referenceLens = lenses[1]
          Expect.isSome referenceLens.Command "There should be a command for multiple references"
          let referenceCommand = referenceLens.Command.Value
          Expect.equal referenceCommand.Title "1 References" "There should be a title for multiple references"

          Expect.equal
            referenceCommand.Command
            "fsharp.showReferences"
            "There should be a command for multiple references"

          Expect.isSome referenceCommand.Arguments "There should be arguments for multiple references"
          let args = referenceCommand.Arguments.Value
          Expect.equal args.Length 3 "There should be 2 args"

          let filePath, triggerPos, referenceRanges =
            args[0].Value<string>(),
            (args[1] :?> JObject).ToObject<Ionide.LanguageServerProtocol.Types.Position>(),
            (args[2] :?> JArray)
            |> Seq.map (fun t -> (t :?> JObject).ToObject<Ionide.LanguageServerProtocol.Types.Location>())
            |> Array.ofSeq

          Expect.equal filePath doc.Uri "File path should be the doc we're checking"
          Expect.equal triggerPos { Line = 1u; Character = 6u } "Position should be 1:6"
          Expect.hasLength referenceRanges 1 "There should be 1 reference range for the `func` function"

          Expect.equal
            referenceRanges[0]
            { Uri = doc.Uri
              Range =
                { Start = { Line = 3u; Character = 19u }
                  End = { Line = 3u; Character = 23u } } }
            "Reference range should be 0:0"})
        testCaseAsync "can show reference counts for 1-character identifier"
        <| CodeLens.check server """
            $0let f () = ""$0
            """ (fun (_doc, lenses, _unresolved, _resolved) ->  async {
          Expect.hasLength lenses 2 "should have a type lens and a reference lens"
          let referenceLens = lenses[1]
          Expect.isSome referenceLens.Command "There should be a command for multiple references"
          let referenceCommand = referenceLens.Command.Value
          Expect.equal referenceCommand.Title "0 References" "There should be a title for multiple references"
          Expect.equal referenceCommand.Command "" "There should be no command for multiple references"
          Expect.isNone referenceCommand.Arguments "There should be arguments for multiple references"}) ])

  ]
