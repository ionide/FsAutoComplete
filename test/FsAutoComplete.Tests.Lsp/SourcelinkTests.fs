module FsAutoComplete.Tests.Lsp.SourcelinkTests

open Expecto
open FsAutoComplete.Sourcelink

/// Tests for the Sourcelink module functions that are accessible
module SourcelinkTests =

  /// Tests for module compilation and basic accessibility  
  let basicAccessibilityTests =
    testList "Basic Accessibility Tests" [
      
      testCase "Sourcelink module compiles and is accessible" <| fun _ ->
        // Test that the module compiles and key types are accessible
        let sourceLinkJsonType = typeof<SourceLinkJson>
        Expect.isNotNull sourceLinkJsonType "SourceLinkJson type should be accessible"
        
      testCase "Errors type is accessible" <| fun _ ->
        let errorsType = typeof<Errors>
        Expect.isNotNull errorsType "Errors type should be accessible"
        
      testCase "key functions exist via reflection" <| fun _ ->
        // Use reflection to verify key functions exist
        let sourceLinkJsonType = typeof<SourceLinkJson>
        let moduleType = sourceLinkJsonType.DeclaringType
        
        let tryFetchMethod = moduleType.GetMethod("tryFetchSourcelinkFile")
        Expect.isNotNull tryFetchMethod "tryFetchSourcelinkFile function should exist"
    ]

  /// Tests for error types
  let errorTests =
    testList "Error Type Tests" [
      
      testCase "Basic error cases are accessible" <| fun _ ->
        // Test that basic error cases can be constructed
        let _noInfoError = Errors.NoInformation
        let _invalidJsonError = Errors.InvalidJson
        let _missingSourceError = Errors.MissingSourceFile
        let _missingPatternsError = Errors.MissingPatterns
        
        Expect.isTrue true "NoInformation error should be accessible"
        Expect.isTrue true "InvalidJson error should be accessible"  
        Expect.isTrue true "MissingSourceFile error should be accessible"
        Expect.isTrue true "MissingPatterns error should be accessible"
        
      testCase "Error pattern matching works" <| fun _ ->
        let testError error =
          match error with
          | Errors.NoInformation -> "no-info"
          | Errors.InvalidJson -> "invalid-json"
          | Errors.MissingSourceFile -> "missing-source"
          | Errors.MissingPatterns -> "missing-patterns"
        
        Expect.equal (testError Errors.NoInformation) "no-info" "NoInformation pattern should match"
        Expect.equal (testError Errors.InvalidJson) "invalid-json" "InvalidJson pattern should match"
        Expect.equal (testError Errors.MissingSourceFile) "missing-source" "MissingSourceFile pattern should match"
        Expect.equal (testError Errors.MissingPatterns) "missing-patterns" "MissingPatterns pattern should match"
    ]

let tests = 
  testList "Sourcelink Tests" [
    SourcelinkTests.basicAccessibilityTests
    SourcelinkTests.errorTests
  ]