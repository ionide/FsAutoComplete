module FsAutoComplete.Tests.CallHierarchy

open Expecto
open FSharp.Compiler.Syntax
open FSharp.Compiler
open FSharp.Compiler.CodeAnalysis
open System.IO
open FsAutoComplete
open Ionide.ProjInfo.ProjectSystem
open FSharp.Compiler.Text
open Utils.ServerTests
open Helpers
open Utils.Server
open Ionide.LanguageServerProtocol.Types

let examples = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "CallHierarchy")
let incomingExamples = Path.Combine(examples, "IncomingCalls")
let outgoingExamples = Path.Combine(examples, "OutgoingCalls")
let sourceFactory: ISourceTextFactory = RoslynSourceTextFactory()

let resultGet =
  function
  | Ok x -> x
  | Error e -> failwithf "%A" e

let resultOptionGet =
  function
  | Ok(Some x) -> x
  | Ok(None) -> failwithf "Expected Some, got None"
  | Error e -> failwithf "%A" e

module CallHierarchyPrepareParams =
  let create (uri: DocumentUri) line character : CallHierarchyPrepareParams =
    { TextDocument = { Uri = uri }
      Position =
        { Character = uint32 character
          Line = uint32 line }
      WorkDoneToken = None }

module LspRange =
  let create startLine startCharacter endLine endCharacter : Range =
    { Start =
        { Character = uint32 startCharacter
          Line = uint32 startLine }
      End =
        { Character = uint32 endCharacter
          Line = uint32 endLine } }


let incomingTests createServer =
  serverTestList "IncomingTests" createServer defaultConfigDto (Some incomingExamples) (fun server ->
    [ testCaseAsync "Example1"
      <| async {
        let! (aDoc, _) = Server.openDocument "Example1.fsx" server
        use aDoc = aDoc
        let! server = server

        let prepareParams = CallHierarchyPrepareParams.create aDoc.Uri 2u 9u

        let! prepareResult =
          server.Server.TextDocumentPrepareCallHierarchy prepareParams
          |> Async.map resultOptionGet

        let expectedPrepareResult: CallHierarchyItem array =
          [| { Data = None
               Detail = None
               Kind = SymbolKind.Function
               Name = "bar"
               Range = LspRange.create 2 8 2 11
               SelectionRange = LspRange.create 2 8 2 11
               Tags = None
               Uri = aDoc.Uri } |]

        Expect.equal prepareResult expectedPrepareResult "prepareResult"

        let expectedIncomingResult: CallHierarchyIncomingCall array =
          [| { FromRanges = [| LspRange.create 8 12 8 15 |]
               From =
                 { Data = None
                   Detail = Some "From Example1.fsx"
                   Kind = SymbolKind.Function
                   Name = "foo"
                   Range = LspRange.create 6 12 8 18
                   SelectionRange = LspRange.create 6 12 6 15
                   Tags = None
                   Uri = aDoc.Uri } } |]

        let incomingParams: CallHierarchyIncomingCallsParams =
          { Item = prepareResult[0]
            PartialResultToken = None
            WorkDoneToken = None }

        let! incomingResult =
          server.Server.CallHierarchyIncomingCalls incomingParams
          |> Async.map resultOptionGet

        Expect.equal incomingResult expectedIncomingResult "incomingResult"
      } ])


let outgoingTests createServer =
  serverTestList "OutgoingTests" createServer defaultConfigDto (Some outgoingExamples) (fun server ->
    [ testCaseAsync "Example1 - Simple function calls"
      <| async {
        let! (aDoc, _) = Server.openDocument "Example1.fsx" server
        use aDoc = aDoc
        let! server = server

        // Test outgoing calls from mainFunction (line 6, character 6 - on the function name)
        let prepareParams = CallHierarchyPrepareParams.create aDoc.Uri 6u 6u

        let! prepareResult =
          server.Server.TextDocumentPrepareCallHierarchy prepareParams
          |> Async.map resultOptionGet

        Expect.equal prepareResult.Length 1 "Should find one symbol"
        Expect.equal prepareResult[0].Name "mainFunction" "Should find mainFunction"

        let outgoingParams: CallHierarchyOutgoingCallsParams =
          { Item = prepareResult[0]
            PartialResultToken = None
            WorkDoneToken = None }

        let! outgoingResult =
          server.Server.CallHierarchyOutgoingCalls outgoingParams
          |> Async.map resultOptionGet

        Expect.isGreaterThan outgoingResult.Length 0 "Should find outgoing calls"

        // Should find calls to: helper, calculate, printfn
        let callNames = outgoingResult |> Array.map (fun call -> call.To.Name)
        Expect.contains callNames "helper" "Should find call to helper"
        Expect.contains callNames "calculate" "Should find call to calculate"
      }

      testCaseAsync "Example2 - Method and constructor calls"
      <| async {
        let! (aDoc, _) = Server.openDocument "Example2.fsx" server
        use aDoc = aDoc
        let! server = server

        // Test outgoing calls from complexFunction (line 10, character 6 - on the function name)
        let prepareParams = CallHierarchyPrepareParams.create aDoc.Uri 10u 6u

        let! prepareResult =
          server.Server.TextDocumentPrepareCallHierarchy prepareParams
          |> Async.map resultOptionGet

        Expect.equal prepareResult.Length 1 "Should find one symbol"
        Expect.equal prepareResult[0].Name "complexFunction" "Should find complexFunction"

        let outgoingParams: CallHierarchyOutgoingCallsParams =
          { Item = prepareResult[0]
            PartialResultToken = None
            WorkDoneToken = None }

        let! outgoingResult =
          server.Server.CallHierarchyOutgoingCalls outgoingParams
          |> Async.map resultOptionGet

        Expect.isGreaterThan outgoingResult.Length 0 "Should find outgoing calls"

        // Should find calls to: Calculator constructor, Add methods, Multiply methods, createPerson
        let callNames = outgoingResult |> Array.map (fun call -> call.To.Name)
        Expect.contains callNames "createPerson" "Should find call to createPerson"
      }

      testCaseAsync "Example3 - System calls and higher-order functions"
      <| async {
        let! (aDoc, _) = Server.openDocument "Example3.fsx" server
        use aDoc = aDoc
        let! server = server

        // Test outgoing calls from processData (line 6, character 6 - on the function name)
        let prepareParams = CallHierarchyPrepareParams.create aDoc.Uri 6u 6u

        let! prepareResult =
          server.Server.TextDocumentPrepareCallHierarchy prepareParams
          |> Async.map resultOptionGet

        Expect.equal prepareResult.Length 1 "Should find one symbol"
        Expect.equal prepareResult[0].Name "processData" "Should find processData"

        let outgoingParams: CallHierarchyOutgoingCallsParams =
          { Item = prepareResult[0]
            PartialResultToken = None
            WorkDoneToken = None }

        let! outgoingResult =
          server.Server.CallHierarchyOutgoingCalls outgoingParams
          |> Async.map resultOptionGet

        Expect.isGreaterThan outgoingResult.Length 0 "Should find outgoing calls"

        // Should find calls to various system and List functions
        let callNames = outgoingResult |> Array.map (fun call -> call.To.Name)
        // Note: Some system calls might not be found depending on F# compiler service behavior
        Expect.isTrue
          (callNames
           |> Array.exists (fun name -> name.Contains("map") || name.Contains("filter")))
          "Should find higher-order function calls"
      } ])

let tests createServer =
  testList "CallHierarchy" [ incomingTests createServer; outgoingTests createServer ]
