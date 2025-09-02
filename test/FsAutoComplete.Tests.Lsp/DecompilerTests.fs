module FsAutoComplete.Tests.Lsp.DecompilerTests

open Expecto
open FsAutoComplete.Decompiler
open FSharp.Compiler.Text

/// Tests for the Decompiler module utility functions
module DecompilerTests =

  /// Tests for file name sanitization
  let fileNameSanitizationTests =
    testList "File Name Sanitization" [
      
      testCase "toSafeFileNameRegex compilation" <| fun _ ->
        // Test that the regex compiles and works on basic unsafe characters
        let regex = toSafeFileNameRegex
        let testInput = "test<>file"
        let result = regex.Replace(testInput, "_")
        Expect.equal result "test__file" "Should replace unsafe characters with underscores"
        
      testCase "handles multiple unsafe characters" <| fun _ ->
        let regex = toSafeFileNameRegex
        let testInput = "test|file:name?"
        let result = regex.Replace(testInput, "_")
        Expect.equal result "test_file_name_" "Should replace all unsafe characters"
        
      testCase "leaves safe characters unchanged" <| fun _ ->
        let regex = toSafeFileNameRegex
        let testInput = "SafeFileName123.cs"
        let result = regex.Replace(testInput, "_")
        Expect.equal result "SafeFileName123.cs" "Should leave safe characters unchanged"
    ]

  /// Tests for type accessibility and compilation
  let typeAccessibilityTests =
    testList "Type Accessibility Tests" [
      
      testCase "DecompileError type is accessible" <| fun _ ->
        let decompileErrorType = typeof<DecompileError>
        Expect.isNotNull decompileErrorType "DecompileError type should be accessible"
        
      testCase "ExternalContentPosition type is accessible" <| fun _ ->
        let externalContentPositionType = typeof<ExternalContentPosition>
        Expect.isNotNull externalContentPositionType "ExternalContentPosition type should be accessible"
        
      testCase "FindExternalDeclarationError type is accessible" <| fun _ ->
        let errorType = typeof<FindExternalDeclarationError>
        Expect.isNotNull errorType "FindExternalDeclarationError type should be accessible"
    ]

  /// Tests for ExternalContentPosition record structure
  let externalContentPositionTests =
    testList "ExternalContentPosition Tests" [
      
      testCase "ExternalContentPosition record construction" <| fun _ ->
        let pos = Position.fromZ 10 5
        let position: ExternalContentPosition = {
          File = "test.cs"
          Position = pos
        }
        
        Expect.equal position.File "test.cs" "Should set file correctly"
        Expect.equal position.Position.Line 10 "Should set position line correctly"
        Expect.equal position.Position.Column 5 "Should set position column correctly"
        
      testCase "ExternalContentPosition with empty file" <| fun _ ->
        let pos = Position.fromZ 0 0
        let position: ExternalContentPosition = {
          File = ""
          Position = pos
        }
        
        Expect.equal position.File "" "Should handle empty file name"
        Expect.equal position.Position.Line 0 "Should handle zero position"
    ]

  /// Tests for module compilation and function accessibility
  let moduleAccessibilityTests =
    testList "Module Accessibility Tests" [
      
      testCase "core functions exist" <| fun _ ->
        // Use reflection to verify the function exists and is accessible
        let decompilerModule = typeof<DecompileError>.DeclaringType
        
        let decompilerForFileMethod = decompilerModule.GetMethod("decompilerForFile")
        Expect.isNotNull decompilerForFileMethod "decompilerForFile function should exist"
        
        let decompileMethod = decompilerModule.GetMethod("decompile")
        Expect.isNotNull decompileMethod "decompile function should exist"
        
        let tryFindMethod = decompilerModule.GetMethod("tryFindExternalDeclaration")
        Expect.isNotNull tryFindMethod "tryFindExternalDeclaration function should exist"
    ]

let tests = 
  testList "Decompiler Tests" [
    DecompilerTests.fileNameSanitizationTests
    DecompilerTests.typeAccessibilityTests
    DecompilerTests.externalContentPositionTests
    DecompilerTests.moduleAccessibilityTests
  ]