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


let multiLevelTests createServer =
  serverTestList "MultiLevelTests" createServer defaultConfigDto (Some incomingExamples) (fun server ->
    [ testCaseAsync "Example2 - Helper function with multiple callers"
      <| async {
        let! (aDoc, _) = Server.openDocument "Example2.fsx" server
        use aDoc = aDoc
        let! server = server

        // Test incoming calls to 'helper' function at line 4
        let prepareParams = CallHierarchyPrepareParams.create aDoc.Uri 4u 8u

        let! prepareResult =
          server.Server.TextDocumentPrepareCallHierarchy prepareParams
          |> Async.map resultOptionGet

        Expect.hasLength prepareResult 1 "Should find helper function"
        Expect.equal prepareResult[0].Name "helper" "Should be helper function"

        let incomingParams: CallHierarchyIncomingCallsParams =
          { Item = prepareResult[0]
            PartialResultToken = None
            WorkDoneToken = None }

        let! incomingResult =
          server.Server.CallHierarchyIncomingCalls incomingParams
          |> Async.map resultOptionGet

        // Should find calls from processData, directCall, and Nested.nestedHelper
        Expect.isGreaterThanOrEqual incomingResult.Length 2 "Should have multiple incoming calls to helper"
      }
      
      testCaseAsync "Example2 - ProcessData function call chain"
      <| async {
        let! (aDoc, _) = Server.openDocument "Example2.fsx" server
        use aDoc = aDoc
        let! server = server

        // Test incoming calls to 'processData' function
        let prepareParams = CallHierarchyPrepareParams.create aDoc.Uri 6u 8u

        let! prepareResult =
          server.Server.TextDocumentPrepareCallHierarchy prepareParams
          |> Async.map resultOptionGet

        Expect.hasLength prepareResult 1 "Should find processData function"
        Expect.equal prepareResult[0].Name "processData" "Should be processData function"

        let incomingParams: CallHierarchyIncomingCallsParams =
          { Item = prepareResult[0]
            PartialResultToken = None
            WorkDoneToken = None }

        let! incomingResult =
          server.Server.CallHierarchyIncomingCalls incomingParams
          |> Async.map resultOptionGet

        // Should find call from validateInput
        Expect.isGreaterThanOrEqual incomingResult.Length 1 "Should have incoming calls to processData"
      } ])

let recursiveTests createServer =
  serverTestList "RecursiveTests" createServer defaultConfigDto (Some incomingExamples) (fun server ->
    [ testCaseAsync "Recursive factorial function"
      <| async {
        let! (aDoc, _) = Server.openDocument "RecursiveExample.fsx" server
        use aDoc = aDoc
        let! server = server

        // Test incoming calls to recursive 'factorial' function
        let prepareParams = CallHierarchyPrepareParams.create aDoc.Uri 4u 12u

        let! prepareResult =
          server.Server.TextDocumentPrepareCallHierarchy prepareParams
          |> Async.map resultOptionGet

        Expect.hasLength prepareResult 1 "Should find factorial function"
        Expect.equal prepareResult[0].Name "factorial" "Should be factorial function"

        let incomingParams: CallHierarchyIncomingCallsParams =
          { Item = prepareResult[0]
            PartialResultToken = None
            WorkDoneToken = None }

        let! incomingResult =
          server.Server.CallHierarchyIncomingCalls incomingParams
          |> Async.map resultOptionGet

        // Should find recursive call to itself and call from testFactorial
        Expect.isGreaterThanOrEqual incomingResult.Length 1 "Should have incoming calls to factorial"
      }

      testCaseAsync "Mutual recursion - isEven function"
      <| async {
        let! (aDoc, _) = Server.openDocument "RecursiveExample.fsx" server
        use aDoc = aDoc
        let! server = server

        // Test incoming calls to 'isEven' function
        let prepareParams = CallHierarchyPrepareParams.create aDoc.Uri 9u 12u

        let! prepareResult =
          server.Server.TextDocumentPrepareCallHierarchy prepareParams
          |> Async.map resultOptionGet

        Expect.hasLength prepareResult 1 "Should find isEven function"
        Expect.equal prepareResult[0].Name "isEven" "Should be isEven function"

        let incomingParams: CallHierarchyIncomingCallsParams =
          { Item = prepareResult[0]
            PartialResultToken = None
            WorkDoneToken = None }

        let! incomingResult =
          server.Server.CallHierarchyIncomingCalls incomingParams
          |> Async.map resultOptionGet

        // Should find call from isOdd (mutual recursion) and testEven
        Expect.isGreaterThanOrEqual incomingResult.Length 1 "Should have incoming calls to isEven"
      } ])

let methodCallTests createServer =
  serverTestList "MethodCallTests" createServer defaultConfigDto (Some incomingExamples) (fun server ->
    [ testCaseAsync "Object method calls - createPerson function"
      <| async {
        let! (aDoc, _) = Server.openDocument "MethodCallExample.fsx" server
        use aDoc = aDoc
        let! server = server

        // Test incoming calls to 'createPerson' function
        let prepareParams = CallHierarchyPrepareParams.create aDoc.Uri 14u 8u

        let! prepareResult =
          server.Server.TextDocumentPrepareCallHierarchy prepareParams
          |> Async.map resultOptionGet

        Expect.hasLength prepareResult 1 "Should find createPerson function"
        Expect.equal prepareResult[0].Name "createPerson" "Should be createPerson function"

        let incomingParams: CallHierarchyIncomingCallsParams =
          { Item = prepareResult[0]
            PartialResultToken = None
            WorkDoneToken = None }

        let! incomingResult =
          server.Server.CallHierarchyIncomingCalls incomingParams
          |> Async.map resultOptionGet

        // Should find calls from person1 and person2 creation
        Expect.isGreaterThanOrEqual incomingResult.Length 1 "Should have incoming calls to createPerson"
      }

      testCaseAsync "Higher-order function - processPersons"
      <| async {
        let! (aDoc, _) = Server.openDocument "MethodCallExample.fsx" server
        use aDoc = aDoc
        let! server = server

        // Test incoming calls to 'processPersons' function
        let prepareParams = CallHierarchyPrepareParams.create aDoc.Uri 16u 8u

        let! prepareResult =
          server.Server.TextDocumentPrepareCallHierarchy prepareParams
          |> Async.map resultOptionGet

        Expect.hasLength prepareResult 1 "Should find processPersons function"
        Expect.equal prepareResult[0].Name "processPersons" "Should be processPersons function"

        let incomingParams: CallHierarchyIncomingCallsParams =
          { Item = prepareResult[0]
            PartialResultToken = None
            WorkDoneToken = None }

        let! incomingResult =
          server.Server.CallHierarchyIncomingCalls incomingParams
          |> Async.map resultOptionGet

        // Should find call from names assignment
        Expect.isGreaterThanOrEqual incomingResult.Length 1 "Should have incoming calls to processPersons"
      } ])

let edgeCaseTests createServer =
  serverTestList "EdgeCaseTests" createServer defaultConfigDto (Some incomingExamples) (fun server ->
    [ testCaseAsync "Function not called anywhere"
      <| async {
        let! (aDoc, _) = Server.openDocument "Example2.fsx" server
        use aDoc = aDoc
        let! server = server

        // Test a function that has no callers - should return empty array
        let prepareParams = CallHierarchyPrepareParams.create aDoc.Uri 18u 8u // mainFunction

        let! prepareResult =
          server.Server.TextDocumentPrepareCallHierarchy prepareParams
          |> Async.map resultOptionGet

        Expect.hasLength prepareResult 1 "Should find mainFunction"
        Expect.equal prepareResult[0].Name "mainFunction" "Should be mainFunction"

        let incomingParams: CallHierarchyIncomingCallsParams =
          { Item = prepareResult[0]
            PartialResultToken = None
            WorkDoneToken = None }

        let! incomingResult =
          server.Server.CallHierarchyIncomingCalls incomingParams
          |> Async.map resultOptionGet

        // mainFunction is not called anywhere, should have empty incoming calls
        Expect.equal incomingResult.Length 0 "Should have no incoming calls for unused function"
      }

      testCaseAsync "Invalid position should return None"
      <| async {
        let! (aDoc, _) = Server.openDocument "Example1.fsx" server
        use aDoc = aDoc
        let! server = server

        // Test invalid position (beyond file bounds)
        let prepareParams = CallHierarchyPrepareParams.create aDoc.Uri 999u 999u

        let! prepareResult = server.Server.TextDocumentPrepareCallHierarchy prepareParams

        match prepareResult with
        | Ok None -> () // Expected for invalid position
        | Ok (Some _) -> failtest "Should return None for invalid position"
        | Error e -> failtestf "Should not error for invalid position: %A" e
      } ])

let tests createServer = 
  testList "CallHierarchy" [ 
    incomingTests createServer
    multiLevelTests createServer
    recursiveTests createServer
    methodCallTests createServer
    edgeCaseTests createServer
  ]
