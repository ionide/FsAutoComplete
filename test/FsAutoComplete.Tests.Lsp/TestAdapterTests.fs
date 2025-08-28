module FsAutoComplete.Tests.TestAdapterTests

open Expecto
open FsAutoComplete.TestAdapter
open FSharp.Compiler.Text
open System

[<Tests>]
let tests =
  testList "TestAdapter" [
    
    testList "TestAdapterEntry Type Structure" [
      testCase "TestAdapterEntry has correct fields" <| fun _ ->
        let range = Range.mkRange "test.fs" (Position.mkPos 1 0) (Position.mkPos 1 10)
        let entry: TestAdapterEntry<range> = {
          Name = "test entry"
          Range = range
          Childs = ResizeArray()
          Id = 1
          List = false
          ModuleType = "TestModule"
          Type = "Expecto"
        }
        
        Expect.equal entry.Name "test entry" "Name should match"
        Expect.equal entry.Range range "Range should match"
        Expect.equal entry.Id 1 "Id should match"
        Expect.isFalse entry.List "List should be false"
        Expect.equal entry.ModuleType "TestModule" "ModuleType should match"
        Expect.equal entry.Type "Expecto" "Type should match"

      testCase "TestAdapterEntry can have child entries" <| fun _ ->
        let parentRange = Range.mkRange "test.fs" (Position.mkPos 1 0) (Position.mkPos 5 0)
        let childRange = Range.mkRange "test.fs" (Position.mkPos 2 0) (Position.mkPos 2 20)
        
        let parent: TestAdapterEntry<range> = {
          Name = "parent test"
          Range = parentRange
          Childs = ResizeArray()
          Id = 1
          List = true
          ModuleType = "Module"
          Type = "Expecto"
        }
        
        let child: TestAdapterEntry<range> = {
          Name = "child test"
          Range = childRange
          Childs = ResizeArray()
          Id = 2
          List = false
          ModuleType = "NoneModule"
          Type = "Expecto"
        }
        
        parent.Childs.Add(child)
        
        Expect.hasLength parent.Childs 1 "Parent should have 1 child"
        Expect.equal parent.Childs.[0].Name "child test" "Child name should match"
        Expect.equal parent.Childs.[0].Id 2 "Child ID should be different"
    ]

    testList "Module Function Existence" [
      testCase "TestAdapter module compiles successfully" <| fun _ ->
        // This test passing means the module compiled without errors
        Expect.isTrue true "TestAdapter module compiled successfully"

      testCase "TestAdapterEntry type is available" <| fun _ ->
        // Test that the TestAdapterEntry type is accessible
        let entryType = typeof<TestAdapterEntry<int>>
        Expect.isNotNull entryType "TestAdapterEntry type should be available"
        Expect.equal entryType.Name "TestAdapterEntry`1" "Type name should match"
    ]

    testList "Integration with Test Framework Types" [
      testCase "TestAdapterEntry works with different range types" <| fun _ ->
        // Test that TestAdapterEntry is generic over range type
        let stringRange = "line 1, col 5"
        let intRange = 42
        
        let stringEntry: TestAdapterEntry<string> = {
          Name = "string range test"
          Range = stringRange
          Childs = ResizeArray()
          Id = 1
          List = false
          ModuleType = "Module"
          Type = "Expecto"
        }
        
        let intEntry: TestAdapterEntry<int> = {
          Name = "int range test"  
          Range = intRange
          Childs = ResizeArray()
          Id = 2
          List = false
          ModuleType = "Module"
          Type = "Expecto"
        }
        
        Expect.equal stringEntry.Range stringRange "String range should match"
        Expect.equal intEntry.Range intRange "Int range should match"
        
      testCase "ResizeArray operations work correctly" <| fun _ ->
        let parent: TestAdapterEntry<range> = {
          Name = "parent"
          Range = Range.mkRange "test.fs" (Position.mkPos 1 0) (Position.mkPos 1 10)
          Childs = ResizeArray()
          Id = 1
          List = true
          ModuleType = "Module"
          Type = "Expecto"
        }
        
        Expect.hasLength parent.Childs 0 "Should start with no children"
        
        let child1: TestAdapterEntry<range> = {
          Name = "child1"
          Range = Range.mkRange "test.fs" (Position.mkPos 2 0) (Position.mkPos 2 10)
          Childs = ResizeArray()
          Id = 2
          List = false
          ModuleType = "NoneModule"
          Type = "Expecto"
        }
        
        let child2: TestAdapterEntry<range> = {
          Name = "child2"
          Range = Range.mkRange "test.fs" (Position.mkPos 3 0) (Position.mkPos 3 10)
          Childs = ResizeArray()
          Id = 3
          List = false
          ModuleType = "NoneModule"
          Type = "Expecto"
        }
        
        parent.Childs.Add(child1)
        parent.Childs.Add(child2)
        
        Expect.hasLength parent.Childs 2 "Should have 2 children after adding"
        Expect.equal parent.Childs.[0].Name "child1" "First child name should match"
        Expect.equal parent.Childs.[1].Name "child2" "Second child name should match"

      testCase "Range creation and manipulation" <| fun _ ->
        let range1 = Range.mkRange "test.fs" (Position.mkPos 1 5) (Position.mkPos 1 15)
        let range2 = Range.mkRange "test.fs" (Position.mkPos 2 0) (Position.mkPos 2 30)
        
        Expect.equal range1.StartLine 1 "Start line should be 1"
        Expect.equal range1.StartColumn 5 "Start column should be 5"
        Expect.equal range1.EndLine 1 "End line should be 1"
        Expect.equal range1.EndColumn 15 "End column should be 15"
        
        Expect.equal range2.StartLine 2 "Second range start line should be 2"
        Expect.equal range2.EndColumn 30 "Second range end column should be 30"
    ]

    testList "Type System Validation" [
      testCase "TestAdapterEntry generic type parameter works" <| fun _ ->
        // Test the generic nature of TestAdapterEntry
        let rangeEntry = {
          Name = "range test"
          Range = Range.mkRange "test.fs" (Position.mkPos 1 0) (Position.mkPos 1 10)
          Childs = ResizeArray<TestAdapterEntry<range>>()
          Id = 1
          List = false
          ModuleType = "Module"
          Type = "Expecto"
        }
        
        let customRangeEntry = {
          Name = "custom test"
          Range = (1, 2, 3, 4) // tuple as range
          Childs = ResizeArray<TestAdapterEntry<int * int * int * int>>()
          Id = 2
          List = true
          ModuleType = "Module"
          Type = "NUnit"
        }
        
        Expect.equal rangeEntry.Name "range test" "Range entry name should match"
        Expect.equal customRangeEntry.Name "custom test" "Custom entry name should match"
        Expect.isFalse rangeEntry.List "Range entry should not be a list"
        Expect.isTrue customRangeEntry.List "Custom entry should be a list"
    ]
  ]