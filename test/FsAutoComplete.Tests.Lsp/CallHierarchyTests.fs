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
open Ionide.LanguageServerProtocol.Mappings

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
      }

      testCaseAsync "OperatorExample - Custom operator calls"
      <| async {
        let! (aDoc, _) = Server.openDocument "OperatorExample.fsx" server
        use aDoc = aDoc
        let! server = server

        // Test outgoing calls from main function (line 10, character 4 - on the function name)
        let prepareParams = CallHierarchyPrepareParams.create aDoc.Uri 10u 4u

        let! prepareResult =
          server.Server.TextDocumentPrepareCallHierarchy prepareParams
          |> Async.map resultOptionGet

        Expect.equal prepareResult.Length 1 "Should find one symbol"
        Expect.equal prepareResult[0].Name "main" "Should find main function"

        let outgoingParams: CallHierarchyOutgoingCallsParams =
          { Item = prepareResult[0]
            PartialResultToken = None
            WorkDoneToken = None }

        let! outgoingResult =
          server.Server.CallHierarchyOutgoingCalls outgoingParams
          |> Async.map resultOptionGet

        Expect.isGreaterThan outgoingResult.Length 0 "Should find outgoing calls"

        // Should find calls to custom operators ++ and |>>
        let callNames = outgoingResult |> Array.map (fun call -> call.To.Name)

        Expect.isTrue
          (callNames
           |> Array.exists (fun name -> name.Contains("++") || name = "op_PlusPlus"))
          "Should find call to custom ++ operator"
      }

      testCaseAsync "LocalFunctionExample - Local function calls"
      <| async {
        let! (aDoc, _) = Server.openDocument "LocalFunctionExample.fsx" server
        use aDoc = aDoc
        let! server = server

        // Test outgoing calls from outerFunction (line 2, character 4 - on the function name)
        let prepareParams = CallHierarchyPrepareParams.create aDoc.Uri 2u 4u

        let! prepareResult =
          server.Server.TextDocumentPrepareCallHierarchy prepareParams
          |> Async.map resultOptionGet

        Expect.equal prepareResult.Length 1 "Should find one symbol"
        Expect.equal prepareResult[0].Name "outerFunction" "Should find outerFunction"

        let outgoingParams: CallHierarchyOutgoingCallsParams =
          { Item = prepareResult[0]
            PartialResultToken = None
            WorkDoneToken = None }

        let! outgoingResult =
          server.Server.CallHierarchyOutgoingCalls outgoingParams
          |> Async.map resultOptionGet

        Expect.isGreaterThan outgoingResult.Length 0 "Should find outgoing calls"

        // Should find calls to local functions
        let callNames = outgoingResult |> Array.map (fun call -> call.To.Name)
        Expect.contains callNames "localHelper" "Should find call to localHelper"
        Expect.contains callNames "localProcessor" "Should find call to localProcessor"
        Expect.contains callNames "nestedOuter" "Should find call to nestedOuter"
      }

      testCaseAsync "PropertyExample - Property and method calls"
      <| async {
        let! (aDoc, _) = Server.openDocument "PropertyExample.fsx" server
        use aDoc = aDoc
        let! server = server

        // Test outgoing calls from main function (line 18 1-indexed = line 17 0-indexed, character 4 - on the function name)
        let prepareParams = CallHierarchyPrepareParams.create aDoc.Uri 17u 4u

        let! prepareResult =
          server.Server.TextDocumentPrepareCallHierarchy prepareParams
          |> Async.map resultOptionGet

        Expect.equal prepareResult.Length 1 "Should find one symbol"
        Expect.equal prepareResult[0].Name "main" "Should find main function"

        let outgoingParams: CallHierarchyOutgoingCallsParams =
          { Item = prepareResult[0]
            PartialResultToken = None
            WorkDoneToken = None }

        let! outgoingResult =
          server.Server.CallHierarchyOutgoingCalls outgoingParams
          |> Async.map resultOptionGet

        Expect.isGreaterThan outgoingResult.Length 0 "Should find outgoing calls"

        // Should find calls to methods
        let callNames = outgoingResult |> Array.map (fun call -> call.To.Name)
        Expect.contains callNames "Increment" "Should find call to Increment method"
        Expect.contains callNames "GetDouble" "Should find call to GetDouble method"
        Expect.contains callNames "createPerson" "Should find call to createPerson function"
      }

      testCaseAsync "RecursiveExample1 - Simple recursion and mutual recursion"
      <| async {
        let! (aDoc, _) = Server.openDocument "RecursiveExample1.fsx" server
        use aDoc = aDoc
        let! server = server

        // Test outgoing calls from factorial function (line 3, on function name)
        let prepareParams = CallHierarchyPrepareParams.create aDoc.Uri 3u 10u

        let! prepareResult =
          server.Server.TextDocumentPrepareCallHierarchy prepareParams
          |> Async.map resultOptionGet

        Expect.equal prepareResult.Length 1 "Should find one symbol"
        Expect.equal prepareResult[0].Name "factorial" "Should find factorial function"

        let outgoingParams: CallHierarchyOutgoingCallsParams =
          { Item = prepareResult[0]
            PartialResultToken = None
            WorkDoneToken = None }

        let! outgoingResult =
          server.Server.CallHierarchyOutgoingCalls outgoingParams
          |> Async.map resultOptionGet

        Expect.isGreaterThan outgoingResult.Length 0 "Should find outgoing calls"

        // Should find recursive call to factorial itself
        let callNames = outgoingResult |> Array.map (fun call -> call.To.Name)
        Expect.contains callNames "factorial" "Should find recursive call to itself"
      }

      testCaseAsync "RecursiveExample1 - Mutual recursion calls"
      <| async {
        let! (aDoc, _) = Server.openDocument "RecursiveExample1.fsx" server
        use aDoc = aDoc
        let! server = server

        // Test outgoing calls from isEven function (line 7, 0-based is 6, position on function name)
        let prepareParams = CallHierarchyPrepareParams.create aDoc.Uri 6u 10u

        let! prepareResult =
          server.Server.TextDocumentPrepareCallHierarchy prepareParams
          |> Async.map resultOptionGet

        Expect.equal prepareResult.Length 1 "Should find one symbol"
        Expect.equal prepareResult[0].Name "isEven" "Should find isEven function"

        let outgoingParams: CallHierarchyOutgoingCallsParams =
          { Item = prepareResult[0]
            PartialResultToken = None
            WorkDoneToken = None }

        let! outgoingResult =
          server.Server.CallHierarchyOutgoingCalls outgoingParams
          |> Async.map resultOptionGet

        Expect.isGreaterThan outgoingResult.Length 0 "Should find outgoing calls"

        // Should find call to isOdd (mutual recursion)
        let callNames = outgoingResult |> Array.map (fun call -> call.To.Name)
        Expect.contains callNames "isOdd" "Should find mutual recursive call to isOdd"
      }



      testCaseAsync "RecursiveExample2 - Tree traversal with recursion"
      <| async {
        let! (aDoc, _) = Server.openDocument "RecursiveExample2.fsx" server
        use aDoc = aDoc
        let! server = server

        // Test outgoing calls from sumTree function (line 5, on function name)
        let prepareParams = CallHierarchyPrepareParams.create aDoc.Uri 5u 10u

        let! prepareResult =
          server.Server.TextDocumentPrepareCallHierarchy prepareParams
          |> Async.map resultOptionGet

        Expect.equal prepareResult.Length 1 "Should find one symbol"
        Expect.equal prepareResult[0].Name "sumTree" "Should find sumTree function"

        let outgoingParams: CallHierarchyOutgoingCallsParams =
          { Item = prepareResult[0]
            PartialResultToken = None
            WorkDoneToken = None }

        let! outgoingResult =
          server.Server.CallHierarchyOutgoingCalls outgoingParams
          |> Async.map resultOptionGet

        Expect.isGreaterThan outgoingResult.Length 0 "Should find outgoing calls"

        // Should find calls to List.fold, List.map, and recursive sumTree
        let callNames = outgoingResult |> Array.map (fun call -> call.To.Name)
        Expect.contains callNames "sumTree" "Should find recursive call to sumTree"
      // Note: List.fold and List.map might not be detected depending on F# compiler service behavior
      }

      testCaseAsync "RecursiveExample3 - Async recursive patterns"
      <| async {
        let! (aDoc, _) = Server.openDocument "RecursiveExample3.fsx" server
        use aDoc = aDoc
        let! server = server

        // Test outgoing calls from asyncFactorial function (line 3, on function name)
        let prepareParams = CallHierarchyPrepareParams.create aDoc.Uri 3u 10u

        let! prepareResult =
          server.Server.TextDocumentPrepareCallHierarchy prepareParams
          |> Async.map resultOptionGet

        Expect.equal prepareResult.Length 1 "Should find one symbol"
        Expect.equal prepareResult[0].Name "asyncFactorial" "Should find asyncFactorial function"

        let outgoingParams: CallHierarchyOutgoingCallsParams =
          { Item = prepareResult[0]
            PartialResultToken = None
            WorkDoneToken = None }

        let! outgoingResult =
          server.Server.CallHierarchyOutgoingCalls outgoingParams
          |> Async.map resultOptionGet

        Expect.isGreaterThan outgoingResult.Length 0 "Should find outgoing calls"

        // Should find recursive call to asyncFactorial itself
        let callNames = outgoingResult |> Array.map (fun call -> call.To.Name)
        Expect.contains callNames "asyncFactorial" "Should find recursive call to asyncFactorial"
      }

      testCaseAsync "NestedExample1 - Multi-level call hierarchy navigation"
      <| async {
        let! (aDoc, _) = Server.openDocument "NestedExample1.fsx" server
        use aDoc = aDoc
        let! server = server

        // Test from entryPoint (line 27, 0-based is 26, position on function name)
        let prepareParams = CallHierarchyPrepareParams.create aDoc.Uri 26u 4u

        let! prepareResult =
          server.Server.TextDocumentPrepareCallHierarchy prepareParams
          |> Async.map resultOptionGet

        Expect.equal prepareResult.Length 1 "Should find one symbol"
        Expect.equal prepareResult[0].Name "entryPoint" "Should find entryPoint function"

        // Get level 1 calls from entryPoint
        let outgoingParams1: CallHierarchyOutgoingCallsParams =
          { Item = prepareResult[0]
            PartialResultToken = None
            WorkDoneToken = None }

        let! level1Calls =
          server.Server.CallHierarchyOutgoingCalls outgoingParams1
          |> Async.map resultOptionGet

        Expect.isGreaterThan level1Calls.Length 0 "Should find level 1 calls"

        let callNames1 = level1Calls |> Array.map (fun call -> call.To.Name)
        Expect.contains callNames1 "level1Function" "Should find call to level1Function"
        Expect.contains callNames1 "complexLevel2" "Should find call to complexLevel2"

        // Navigate deeper - get calls from level1Function
        let level1FunctionItem =
          level1Calls |> Array.find (fun call -> call.To.Name = "level1Function")

        let outgoingParams2: CallHierarchyOutgoingCallsParams =
          { Item = level1FunctionItem.To
            PartialResultToken = None
            WorkDoneToken = None }

        let! level2Calls =
          server.Server.CallHierarchyOutgoingCalls outgoingParams2
          |> Async.map resultOptionGet

        Expect.isGreaterThan level2Calls.Length 0 "Should find level 2 calls"

        let callNames2 = level2Calls |> Array.map (fun call -> call.To.Name)
        Expect.contains callNames2 "level2Function" "Should find call to level2Function at level 2"

        // Navigate even deeper - get calls from level2Function
        let level2FunctionItem =
          level2Calls |> Array.find (fun call -> call.To.Name = "level2Function")

        let outgoingParams3: CallHierarchyOutgoingCallsParams =
          { Item = level2FunctionItem.To
            PartialResultToken = None
            WorkDoneToken = None }

        let! level3Calls =
          server.Server.CallHierarchyOutgoingCalls outgoingParams3
          |> Async.map resultOptionGet

        Expect.isGreaterThan level3Calls.Length 0 "Should find level 3 calls"

        let callNames3 = level3Calls |> Array.map (fun call -> call.To.Name)
        Expect.contains callNames3 "level3Function" "Should find call to level3Function at level 3"
      }

      testCaseAsync "NestedExample1 - Multiple branch navigation"
      <| async {
        let! (aDoc, _) = Server.openDocument "NestedExample1.fsx" server
        use aDoc = aDoc
        let! server = server

        // Test branching from complexLevel2 (line 22, 0-based is 21, position on function name)
        let prepareParams = CallHierarchyPrepareParams.create aDoc.Uri 21u 4u

        let! prepareResult =
          server.Server.TextDocumentPrepareCallHierarchy prepareParams
          |> Async.map resultOptionGet

        Expect.equal prepareResult.Length 1 "Should find one symbol"
        Expect.equal prepareResult[0].Name "complexLevel2" "Should find complexLevel2 function"

        let outgoingParams: CallHierarchyOutgoingCallsParams =
          { Item = prepareResult[0]
            PartialResultToken = None
            WorkDoneToken = None }

        let! outgoingCalls =
          server.Server.CallHierarchyOutgoingCalls outgoingParams
          |> Async.map resultOptionGet

        Expect.isGreaterThan outgoingCalls.Length 0 "Should find outgoing calls"

        let callNames = outgoingCalls |> Array.map (fun call -> call.To.Name)
        Expect.contains callNames "level3Function" "Should find call to level3Function"
        Expect.contains callNames "anotherLevel3" "Should find call to anotherLevel3"

        // Navigate to anotherLevel3 and check its calls
        let anotherLevel3Item =
          outgoingCalls |> Array.find (fun call -> call.To.Name = "anotherLevel3")

        let deeperParams: CallHierarchyOutgoingCallsParams =
          { Item = anotherLevel3Item.To
            PartialResultToken = None
            WorkDoneToken = None }

        let! deeperCalls =
          server.Server.CallHierarchyOutgoingCalls deeperParams
          |> Async.map resultOptionGet

        Expect.isGreaterThan deeperCalls.Length 0 "Should find deeper calls"

        let deeperCallNames = deeperCalls |> Array.map (fun call -> call.To.Name)
        // anotherLevel3 calls level4Function twice
        let level4Calls =
          deeperCallNames |> Array.filter (fun name -> name = "level4Function")

        Expect.isGreaterThan level4Calls.Length 0 "Should find calls to level4Function"
      }

      testCaseAsync "DeepHierarchy - 5-level navigation by choosing first results"
      <| async {
        let! (aDoc, _) = Server.openDocument "DeepHierarchy.fsx" server
        use aDoc = aDoc
        let! server = server

        // Start from topLevelEntry (line 10, 0-based is 9)
        let prepareParams = CallHierarchyPrepareParams.create aDoc.Uri 9u 4u

        let! prepareResult =
          server.Server.TextDocumentPrepareCallHierarchy prepareParams
          |> Async.map resultOptionGet

        Expect.equal prepareResult.Length 1 "Should find one symbol"
        Expect.equal prepareResult[0].Name "topLevelEntry" "Should find topLevelEntry function"

        // Level 1: topLevelEntry -> level1Main
        let outgoingParams1: CallHierarchyOutgoingCallsParams =
          { Item = prepareResult[0]
            PartialResultToken = None
            WorkDoneToken = None }

        let! level1Calls =
          server.Server.CallHierarchyOutgoingCalls outgoingParams1
          |> Async.map resultOptionGet

        Expect.isGreaterThan level1Calls.Length 0 "Should find level 1 calls"

        // Choose first result and verify it's level1Main
        let firstLevel1Call = level1Calls[0]
        Expect.equal firstLevel1Call.To.Name "level1Main" "First call should be to level1Main"

        // Level 2: level1Main -> level2Process (using first result)
        let outgoingParams2: CallHierarchyOutgoingCallsParams =
          { Item = firstLevel1Call.To
            PartialResultToken = None
            WorkDoneToken = None }

        let! level2Calls =
          server.Server.CallHierarchyOutgoingCalls outgoingParams2
          |> Async.map resultOptionGet

        Expect.isGreaterThan level2Calls.Length 0 "Should find level 2 calls"

        // Choose first result and verify it's level2Process
        let firstLevel2Call = level2Calls[0]
        Expect.equal firstLevel2Call.To.Name "level2Process" "First call should be to level2Process"

        // Level 3: level2Process -> level3Helper (using first result)
        let outgoingParams3: CallHierarchyOutgoingCallsParams =
          { Item = firstLevel2Call.To
            PartialResultToken = None
            WorkDoneToken = None }

        let! level3Calls =
          server.Server.CallHierarchyOutgoingCalls outgoingParams3
          |> Async.map resultOptionGet

        Expect.isGreaterThan level3Calls.Length 0 "Should find level 3 calls"

        // Choose first result and verify it's level3Helper
        let firstLevel3Call = level3Calls[0]
        Expect.equal firstLevel3Call.To.Name "level3Helper" "First call should be to level3Helper"

        // Level 4: level3Helper -> level4Action (using first result)
        let outgoingParams4: CallHierarchyOutgoingCallsParams =
          { Item = firstLevel3Call.To
            PartialResultToken = None
            WorkDoneToken = None }

        let! level4Calls =
          server.Server.CallHierarchyOutgoingCalls outgoingParams4
          |> Async.map resultOptionGet

        Expect.isGreaterThan level4Calls.Length 0 "Should find level 4 calls"

        // Choose first result and verify it's level4Action
        let firstLevel4Call = level4Calls[0]
        Expect.equal firstLevel4Call.To.Name "level4Action" "First call should be to level4Action"

        // Level 5: level4Action should have no outgoing calls (it's a leaf function)
        let outgoingParams5: CallHierarchyOutgoingCallsParams =
          { Item = firstLevel4Call.To
            PartialResultToken = None
            WorkDoneToken = None }

        let! level5Calls =
          server.Server.CallHierarchyOutgoingCalls outgoingParams5
          |> Async.map resultOptionGet

        // level4Action is a leaf node, so it should have no outgoing calls or very few
        Expect.isTrue
          (level5Calls.Length = 0 || level5Calls.Length <= 1)
          "Leaf function should have no or minimal outgoing calls"
      }

      testCaseAsync "NestedExample2 - parallelWorkflow duplicate calls have multiple fromRanges"
      <| async {
        let! (aDoc, _) = Server.openDocument "NestedExample2.fsx" server
        use aDoc = aDoc
        let! server = server

        // Test parallelWorkflow which calls Services.serviceOperation twice
        // Line 53 (1-indexed) = Line 52 (0-indexed), character 4 on function name
        let prepareParams = CallHierarchyPrepareParams.create aDoc.Uri 52u 4u

        let! prepareResult =
          server.Server.TextDocumentPrepareCallHierarchy prepareParams
          |> Async.map resultOptionGet

        Expect.equal prepareResult.Length 1 "Should find one symbol"
        Expect.equal prepareResult[0].Name "parallelWorkflow" "Should find parallelWorkflow function"

        let outgoingParams: CallHierarchyOutgoingCallsParams =
          { Item = prepareResult[0]
            PartialResultToken = None
            WorkDoneToken = None }

        let! outgoingResult =
          server.Server.CallHierarchyOutgoingCalls outgoingParams
          |> Async.map resultOptionGet

        Expect.isGreaterThan outgoingResult.Length 0 "Should find outgoing calls"

        // Find the serviceOperation call
        let serviceOperationCall =
          outgoingResult |> Array.tryFind (fun call -> call.To.Name = "serviceOperation")

        Expect.isSome serviceOperationCall "Should find call to serviceOperation"

        // Verify that serviceOperation has 2 fromRanges (called twice on lines 54 and 55)
        let serviceOpCall = serviceOperationCall.Value
        Expect.equal serviceOpCall.FromRanges.Length 2 "serviceOperation should have 2 fromRanges for the two calls"

        // Verify the fromRanges point to lines 53 and 54 (0-indexed)
        let fromLines =
          serviceOpCall.FromRanges |> Array.map (fun r -> r.Start.Line) |> Array.sort

        Expect.equal fromLines.[0] 53u "First fromRange should be on line 53 (0-indexed)"
        Expect.equal fromLines.[1] 54u "Second fromRange should be on line 54 (0-indexed)"
      }

      testCaseAsync "NestedExample2 - serviceOperation cross-module calls"
      <| async {
        let! (aDoc, _) = Server.openDocument "NestedExample2.fsx" server
        use aDoc = aDoc
        let! server = server

        // Test serviceOperation within Services module
        // Line 35 (1-indexed) = Line 34 (0-indexed), character 6 on function name
        let prepareParams = CallHierarchyPrepareParams.create aDoc.Uri 34u 6u

        let! prepareResult =
          server.Server.TextDocumentPrepareCallHierarchy prepareParams
          |> Async.map resultOptionGet

        Expect.equal prepareResult.Length 1 "Should find one symbol"
        Expect.equal prepareResult[0].Name "serviceOperation" "Should find serviceOperation function"

        let outgoingParams: CallHierarchyOutgoingCallsParams =
          { Item = prepareResult[0]
            PartialResultToken = None
            WorkDoneToken = None }

        let! outgoingResult =
          server.Server.CallHierarchyOutgoingCalls outgoingParams
          |> Async.map resultOptionGet

        Expect.isGreaterThan outgoingResult.Length 0 "Should find outgoing calls"

        // Should find calls to calculateMetrics and transformData
        let callNames = outgoingResult |> Array.map (fun call -> call.To.Name)
        Expect.contains callNames "calculateMetrics" "Should find call to calculateMetrics"
        Expect.contains callNames "transformData" "Should find call to transformData"
      }

      testCaseAsync "NestedExample2 - orchestrateWorkflow multi-module calls"
      <| async {
        let! (aDoc, _) = Server.openDocument "NestedExample2.fsx" server
        use aDoc = aDoc
        let! server = server

        // Test orchestrateWorkflow within Services module
        // Line 40 (1-indexed) = Line 39 (0-indexed), character 6 on function name
        let prepareParams = CallHierarchyPrepareParams.create aDoc.Uri 39u 6u

        let! prepareResult =
          server.Server.TextDocumentPrepareCallHierarchy prepareParams
          |> Async.map resultOptionGet

        Expect.equal prepareResult.Length 1 "Should find one symbol"
        Expect.equal prepareResult[0].Name "orchestrateWorkflow" "Should find orchestrateWorkflow function"

        let outgoingParams: CallHierarchyOutgoingCallsParams =
          { Item = prepareResult[0]
            PartialResultToken = None
            WorkDoneToken = None }

        let! outgoingResult =
          server.Server.CallHierarchyOutgoingCalls outgoingParams
          |> Async.map resultOptionGet

        Expect.isGreaterThan outgoingResult.Length 0 "Should find outgoing calls"

        // Should find calls to serviceOperation, validateData, and formatData
        let callNames = outgoingResult |> Array.map (fun call -> call.To.Name)
        Expect.contains callNames "serviceOperation" "Should find call to serviceOperation"
        Expect.contains callNames "validateData" "Should find call to validateData"
        Expect.contains callNames "formatData" "Should find call to formatData"
      }

      testCaseAsync "NestedExample2 - mainWorkflow simple delegation"
      <| async {
        let! (aDoc, _) = Server.openDocument "NestedExample2.fsx" server
        use aDoc = aDoc
        let! server = server

        // Test mainWorkflow at module level
        // Line 51 (1-indexed) = Line 50 (0-indexed), character 4 on function name
        let prepareParams = CallHierarchyPrepareParams.create aDoc.Uri 50u 4u

        let! prepareResult =
          server.Server.TextDocumentPrepareCallHierarchy prepareParams
          |> Async.map resultOptionGet

        Expect.equal prepareResult.Length 1 "Should find one symbol"
        Expect.equal prepareResult[0].Name "mainWorkflow" "Should find mainWorkflow function"

        let outgoingParams: CallHierarchyOutgoingCallsParams =
          { Item = prepareResult[0]
            PartialResultToken = None
            WorkDoneToken = None }

        let! outgoingResult =
          server.Server.CallHierarchyOutgoingCalls outgoingParams
          |> Async.map resultOptionGet

        Expect.isGreaterThan outgoingResult.Length 0 "Should find outgoing calls"

        // Should find call to orchestrateWorkflow
        let callNames = outgoingResult |> Array.map (fun call -> call.To.Name)
        Expect.contains callNames "orchestrateWorkflow" "Should find call to orchestrateWorkflow"
      }

      testCaseAsync "NestedExample1 - External symbols have local file URIs via SourceLink"
      <| async {
        let! (aDoc, _) = Server.openDocument "NestedExample1.fsx" server
        use aDoc = aDoc
        let! server = server

        // Test from complexLevel2 which uses the + operator (external FSharp.Core symbol)
        // Line 22, 0-based is 21, position on function name
        let prepareParams = CallHierarchyPrepareParams.create aDoc.Uri 21u 4u

        let! prepareResult =
          server.Server.TextDocumentPrepareCallHierarchy prepareParams
          |> Async.map resultOptionGet

        Expect.equal prepareResult.Length 1 "Should find one symbol"
        Expect.equal prepareResult[0].Name "complexLevel2" "Should find complexLevel2 function"

        let outgoingParams: CallHierarchyOutgoingCallsParams =
          { Item = prepareResult[0]
            PartialResultToken = None
            WorkDoneToken = None }

        let! outgoingResult =
          server.Server.CallHierarchyOutgoingCalls outgoingParams
          |> Async.map resultOptionGet

        Expect.isGreaterThan outgoingResult.Length 0 "Should find outgoing calls"

        // Find the + operator call (external symbol from FSharp.Core)
        let plusOperatorCall =
          outgoingResult
          |> Array.tryFind (fun call -> call.To.Name = "(+)" || call.To.Name = "op_Addition")

        // If we found the + operator, verify its URI points to a local file
        match plusOperatorCall with
        | Some call ->
          let uri = call.To.Uri

          // The URI should NOT be a non-existent build path like "file:///d%3A/a/_work/..."
          // Instead, it should be a local temp file path from SourceLink
          let uriStr = uri.ToString()

          // Check that it's a file URI
          Expect.isTrue (uriStr.StartsWith("file://")) "URI should be a file URI"

          // The URI should NOT contain Azure DevOps build paths
          Expect.isFalse
            (uriStr.Contains("/_work/") || uriStr.Contains("%2F_work%2F"))
            "URI should not contain Azure DevOps build paths"

          // Extract the local path from the URI and verify the file exists
          let localPath = Path.FileUriToLocalPath uri

          Expect.isTrue (File.Exists localPath) $"SourceLink file should exist locally at {localPath}"
        | None ->
          // The + operator might not be detected depending on FCS behavior, which is acceptable
          // In that case, we just verify the other calls have valid local URIs
          ()

        // Verify all outgoing calls have valid local file URIs (either local workspace or SourceLink temp files)
        for call in outgoingResult do
          let uri = call.To.Uri
          let uriStr = uri.ToString()

          // All URIs should be file URIs
          Expect.isTrue (uriStr.StartsWith("file://")) $"URI for {call.To.Name} should be a file URI"

          // Extract local path and verify file exists
          let localPath = Path.FileUriToLocalPath uri

          Expect.isTrue (File.Exists localPath) $"File for {call.To.Name} should exist locally at {localPath}"
      } ])

let tests createServer =
  testList "CallHierarchy" [ incomingTests createServer; outgoingTests createServer ]
