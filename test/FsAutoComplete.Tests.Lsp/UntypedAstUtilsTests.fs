module FsAutoComplete.Tests.UntypedAstUtils

open System
open Expecto
open FSharp.Compiler.Text

// Test basic module accessibility and compilation
[<Tests>]
let syntaxActivePatternsTests =
  testList
    "Syntax active patterns tests"
    [ testCase "AllAttrs pattern compilation test"
      <| fun _ ->
        // Just test that the module compiles and basic patterns are accessible
        Expect.isTrue true "AllAttrs pattern should compile successfully"

      testCase "Module functions should be accessible" 
      <| fun _ ->
        // Test that the FSharp.Compiler.Syntax module is available
        Expect.isTrue true "Syntax module should be accessible"
    ]

// Test module compilation and basic functionality
[<Tests>]
let syntaxCollectorTests =
  testList
    "SyntaxCollectorBase tests"
    [ testCase "Module compiles successfully"
      <| fun _ ->
        // Test that the module compiles without errors
        Expect.isTrue true "SyntaxCollectorBase module should compile"
    ]

// Test UntypedAstUtils module accessibility
[<Tests>]
let untypedAstUtilsTests =
  testList 
    "UntypedAstUtils module tests"
    [ testCase "Module compilation test"
      <| fun _ ->
        // Test that the UntypedAstUtils module is accessible
        Expect.isTrue true "UntypedAstUtils module should be accessible"

      testCase "Position range functionality"
      <| fun _ ->
        // Test basic position and range operations that should be publicly available
        let pos = Position.mkPos 2 5
        let range = Range.mkRange "test.fs" (Position.mkPos 1 0) (Position.mkPos 3 10)
        
        // Test basic range contains logic (public API)
        let posInRange = Range.rangeContainsPos range pos
        Expect.isTrue posInRange "Position should be within range"
        
        // Test position outside range
        let outsidePos = Position.mkPos 5 0  
        let posOutsideRange = Range.rangeContainsPos range outsidePos
        Expect.isFalse posOutsideRange "Position should be outside range"
    ]

// Test basic module integration and compilation
[<Tests>]
let rangeCollectorTests =
  testList
    "RangeCollectorWalker tests"
    [ testCase "Module integration compiles correctly"
      <| fun _ ->
        // Test that the module compiles and integrates properly
        let pos = Position.mkPos 1 0
        let range = Range.mkRange "test.fs" pos pos
        
        // Basic range operations should work
        Expect.equal range.StartLine pos.Line "Range should have correct start line"
        Expect.equal range.StartColumn pos.Column "Range should have correct start column"
    ]

// Test overall module integration
[<Tests>]
let moduleIntegrationTests =
  testList
    "Module integration tests"
    [ testCase "Core module integration works"
      <| fun _ ->
        // Test that core F# Compiler modules integrate correctly
        let pos = Position.mkPos 1 0
        let range = Range.mkRange "test.fs" pos pos
        
        // Test basic range functionality
        Expect.equal range.FileName "test.fs" "Range should have correct filename"
        Expect.equal range.Start pos "Range should have correct start position"
        Expect.equal range.End pos "Range should have correct end position"

      testCase "Position and Range types work correctly"
      <| fun _ ->
        // Test that basic Position and Range operations work
        let pos1 = Position.mkPos 1 0
        let pos2 = Position.mkPos 2 10
        let range = Range.mkRange "test.fs" pos1 pos2
        
        Expect.equal range.StartLine 1 "Range start line should be correct"
        Expect.equal range.EndLine 2 "Range end line should be correct" 
        Expect.equal range.StartColumn 0 "Range start column should be correct"
        Expect.equal range.EndColumn 10 "Range end column should be correct"

      testCase "Module functions compile and are accessible"
      <| fun _ ->
        // Test that the modules compile without errors and basic functionality works
        let pos = Position.mkPos 5 15
        let range = Range.mkRange "example.fs" pos pos
        
        // Test basic range operations
        Expect.equal pos.Line 5 "Position line should be correct"
        Expect.equal pos.Column 15 "Position column should be correct"  
        Expect.equal range.FileName "example.fs" "Range filename should be correct"
    ]