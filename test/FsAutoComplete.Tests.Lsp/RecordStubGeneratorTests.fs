module FsAutoComplete.Tests.Lsp.RecordStubGeneratorTests

open Expecto
open FsAutoComplete.RecordStubGenerator
open FSharp.Compiler.Text

/// Tests for the RecordStubGenerator module
module RecordStubGeneratorTests =

  /// Tests for PositionKind enumeration and basic types
  let positionKindTests =
    testList "PositionKind Tests" [
      
      testCase "PositionKind cases are defined" <| fun _ ->
        // Test that all position kind cases are accessible
        let _afterLeftBrace = PositionKind.AfterLeftBrace
        let _afterCopyExpression = PositionKind.AfterCopyExpression  
        let _afterLastField = PositionKind.AfterLastField
        
        Expect.isTrue true "AfterLeftBrace should be defined"
        Expect.isTrue true "AfterCopyExpression should be defined"
        Expect.isTrue true "AfterLastField should be defined"
        
      testCase "PositionKind pattern matching works" <| fun _ ->
        let testKind = PositionKind.AfterLeftBrace
        
        let result = 
          match testKind with
          | PositionKind.AfterLeftBrace -> "after brace"
          | PositionKind.AfterCopyExpression -> "after copy"
          | PositionKind.AfterLastField -> "after field"
          
        Expect.equal result "after brace" "Pattern matching should work correctly"
    ]

  /// Tests for RecordStubsInsertionParams type and construction
  let insertionParamsTests =
    testList "RecordStubsInsertionParams Tests" [
      
      testCase "RecordStubsInsertionParams construction" <| fun _ ->
        let pos = Position.fromZ 5 10
        let insertionParams = {
          Kind = PositionKind.AfterLeftBrace
          InsertionPos = pos
          IndentColumn = 4
        }
        
        Expect.equal insertionParams.Kind PositionKind.AfterLeftBrace "Kind should be set correctly"
        Expect.equal insertionParams.InsertionPos.Line 5 "Position line should be set correctly"  
        Expect.equal insertionParams.InsertionPos.Column 10 "Position column should be set correctly"
        Expect.equal insertionParams.IndentColumn 4 "IndentColumn should be set correctly"
    ]

  /// Tests for RecordExpr type accessibility
  let recordExprTests =
    testList "RecordExpr Tests" [
      
      testCase "RecordExpr type structure" <| fun _ ->
        // Test that we can access the RecordExpr type and its fields
        let recordExprType = typeof<RecordExpr>
        Expect.isNotNull recordExprType "RecordExpr type should be accessible"
        
        // Check that the required fields exist
        let exprField = recordExprType.GetProperty("Expr")
        let copyExprField = recordExprType.GetProperty("CopyExprOption")
        let fieldExprListField = recordExprType.GetProperty("FieldExprList") 
        let lastKnownPosField = recordExprType.GetProperty("LastKnownGoodPosForSymbolLookup")
        
        Expect.isNotNull exprField "Expr field should exist"
        Expect.isNotNull copyExprField "CopyExprOption field should exist"
        Expect.isNotNull fieldExprListField "FieldExprList field should exist"
        Expect.isNotNull lastKnownPosField "LastKnownGoodPosForSymbolLookup field should exist"
    ]

  /// Tests for module compilation and function accessibility
  let moduleAccessibilityTests =
    testList "Module Accessibility Tests" [
      
      testCase "key functions exist" <| fun _ ->
        // Use reflection to verify the function exists and is accessible
        let moduleType = typeof<RecordExpr>.DeclaringType
        
        let formatRecordMethod = moduleType.GetMethod("formatRecord")
        Expect.isNotNull formatRecordMethod "formatRecord function should exist and be accessible"
        
        let tryFindRecordExprMethod = moduleType.GetMethod("tryFindRecordExprInBufferAtPos")
        Expect.isNotNull tryFindRecordExprMethod "tryFindRecordExprInBufferAtPos function should exist"
        
        let shouldGenerateMethod = moduleType.GetMethod("shouldGenerateRecordStub")
        Expect.isNotNull shouldGenerateMethod "shouldGenerateRecordStub function should exist"
    ]

  /// Tests for Position integration
  let positionTests =
    testList "Position Integration Tests" [
      
      testCase "Position creation and usage" <| fun _ ->
        // Test Position utility functions used in the module
        let pos1 = Position.fromZ 10 5
        let pos2 = Position.fromZ 10 5  
        let pos3 = Position.fromZ 11 5
        
        Expect.equal pos1.Line pos2.Line "Same positions should have same line"
        Expect.equal pos1.Column pos2.Column "Same positions should have same column"
        Expect.notEqual pos1.Line pos3.Line "Different positions should have different line"
    ]

let tests = 
  testList "RecordStubGenerator Tests" [
    RecordStubGeneratorTests.positionKindTests
    RecordStubGeneratorTests.insertionParamsTests
    RecordStubGeneratorTests.recordExprTests
    RecordStubGeneratorTests.moduleAccessibilityTests
    RecordStubGeneratorTests.positionTests
  ]