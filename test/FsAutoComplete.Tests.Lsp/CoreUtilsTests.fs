module FsAutoComplete.Tests.Lsp.CoreUtilsTests

#nowarn "3391" // Implicit ReadOnlySpan conversions

open Expecto
open FsAutoComplete.Utils
open System.IO
open System

/// Tests for the core utility functions in FsAutoComplete.Utils module
module UtilsTests =
  
  /// Tests for file type detection functions
  let fileTypeTests =
    testList "File Type Detection" [
      
      testList "isAScript" [
        testCase "detects .fsx files as scripts" <| fun _ ->
          Expect.isTrue (isAScript "test.fsx") "Should detect .fsx as script"
          
        testCase "detects .fsscript files as scripts" <| fun _ ->
          Expect.isTrue (isAScript "test.fsscript") "Should detect .fsscript as script"
          
        testCase "detects .sketchfs files as scripts" <| fun _ ->
          Expect.isTrue (isAScript "test.sketchfs") "Should detect .sketchfs as script"
          
        testCase "does not detect .fs files as scripts" <| fun _ ->
          Expect.isFalse (isAScript "test.fs") "Should not detect .fs as script"
          
        testCase "handles full paths" <| fun _ ->
          Expect.isTrue (isAScript "/path/to/test.fsx") "Should detect .fsx in full path as script"
          
        testCase "handles Windows paths" <| fun _ ->
          Expect.isTrue (isAScript @"C:\path\to\test.fsx") "Should detect .fsx in Windows path as script"
      ]
      
      testList "isSignatureFile" [
        testCase "detects .fsi files" <| fun _ ->
          Expect.isTrue (isSignatureFile "test.fsi") "Should detect .fsi files"
          
        testCase "does not detect .fs files" <| fun _ ->
          Expect.isFalse (isSignatureFile "test.fs") "Should not detect .fs files"
          
        testCase "does not detect .fsx files" <| fun _ ->
          Expect.isFalse (isSignatureFile "test.fsx") "Should not detect .fsx files"
          
        testCase "handles full paths" <| fun _ ->
          Expect.isTrue (isSignatureFile "/path/to/test.fsi") "Should detect .fsi in full path"
      ]
      
      testList "isFsharpFile" [
        testCase "detects .fs files" <| fun _ ->
          Expect.isTrue (isFsharpFile "test.fs") "Should detect .fs files"
          
        testCase "does not detect .fsi files" <| fun _ ->
          Expect.isFalse (isFsharpFile "test.fsi") "Should not detect .fsi files"
          
        testCase "does not detect .fsx files" <| fun _ ->
          Expect.isFalse (isFsharpFile "test.fsx") "Should not detect .fsx files"
          
        testCase "handles full paths" <| fun _ ->
          Expect.isTrue (isFsharpFile "/path/to/test.fs") "Should detect .fs in full path"
      ]
      
      testList "isFileWithFSharp" [
        testCase "detects all F# file types" <| fun _ ->
          Expect.isTrue (isFileWithFSharp "test.fs") "Should detect .fs files"
          Expect.isTrue (isFileWithFSharp "test.fsi") "Should detect .fsi files"
          Expect.isTrue (isFileWithFSharp "test.fsx") "Should detect .fsx files"
          Expect.isTrue (isFileWithFSharp "test.fsscript") "Should detect .fsscript files"
          Expect.isTrue (isFileWithFSharp "test.sketchfs") "Should detect .sketchfs files"
          
        testCase "does not detect non-F# files" <| fun _ ->
          Expect.isFalse (isFileWithFSharp "test.cs") "Should not detect .cs files"
          Expect.isFalse (isFileWithFSharp "test.txt") "Should not detect .txt files"
          Expect.isFalse (isFileWithFSharp "test") "Should not detect files without extension"
      ]
    ]
  
  /// Tests for path manipulation functions  
  let pathTests =
    testList "Path Functions" [
      
      testList "combinePaths" [
        testCase "combines simple paths" <| fun _ ->
          let result = combinePaths "base" "file.fs"
          let expected = Path.Combine("base", "file.fs")
          Expect.equal result expected "Should combine paths correctly"
          
        testCase "trims leading slashes from second path" <| fun _ ->
          let result = combinePaths "base" "/file.fs"
          let expected = Path.Combine("base", "file.fs")
          Expect.equal result expected "Should trim leading slash"
          
        testCase "trims leading backslashes from second path" <| fun _ ->
          let result = combinePaths "base" "\\file.fs"
          let expected = Path.Combine("base", "file.fs")
          Expect.equal result expected "Should trim leading backslash"
          
        testCase "handles multiple leading separators" <| fun _ ->
          let result = combinePaths "base" "\\/file.fs"
          let expected = Path.Combine("base", "file.fs")
          Expect.equal result expected "Should trim multiple leading separators"
      ]
      
      testList "path operator </>" [
        testCase "works like combinePaths" <| fun _ ->
          let result = "base" </> "file.fs"
          let expected = combinePaths "base" "file.fs"
          Expect.equal result expected "Operator should work like combinePaths"
          
        testCase "handles complex paths" <| fun _ ->
          let result = "/usr/local" </> "/bin/tool"
          let expected = Path.Combine("/usr/local", "bin/tool")
          Expect.equal result expected "Should handle complex paths"
      ]
    ]
  
  /// Tests for string manipulation functions
  let stringTests =
    testList "String Functions" [
      
      testList "chooseByPrefix" [
        testCase "returns suffix when prefix matches" <| fun _ ->
          let result = chooseByPrefix "test" "testSuffix"
          Expect.equal result (Some "Suffix") "Should return suffix when prefix matches"
          
        testCase "returns None when prefix doesn't match" <| fun _ ->
          let result = chooseByPrefix "test" "differentPrefix"
          Expect.equal result None "Should return None when prefix doesn't match"
          
        testCase "handles exact match" <| fun _ ->
          let result = chooseByPrefix "test" "test"
          Expect.equal result (Some "") "Should return empty string for exact match"
          
        testCase "handles empty prefix" <| fun _ ->
          let result = chooseByPrefix "" "anyString"
          Expect.equal result (Some "anyString") "Should return full string for empty prefix"
          
        testCase "case sensitive matching" <| fun _ ->
          let result = chooseByPrefix "Test" "testSuffix"
          Expect.equal result None "Should be case sensitive"
      ]
      
      testList "chooseByPrefix2" [
        testCase "finds first matching prefix" <| fun _ ->
          let prefixes = ["abc"; "def"; "ghi"]
          let result = chooseByPrefix2 prefixes "defSuffix"
          Expect.equal result (Some "Suffix") "Should find matching prefix"
          
        testCase "returns None when no prefix matches" <| fun _ ->
          let prefixes = ["abc"; "def"; "ghi"]
          let result = chooseByPrefix2 prefixes "xyzSuffix"
          Expect.equal result None "Should return None when no prefix matches"
          
        testCase "returns first match when multiple prefixes match" <| fun _ ->
          let prefixes = ["a"; "ab"; "abc"]
          let result = chooseByPrefix2 prefixes "abcdefg"
          Expect.equal result (Some "bcdefg") "Should return first match"
      ]
      
      testList "splitByPrefix" [
        testCase "splits when prefix matches" <| fun _ ->
          let result = splitByPrefix "test" "testSuffix"
          Expect.equal result (Some ("test", "Suffix")) "Should split when prefix matches"
          
        testCase "returns None when prefix doesn't match" <| fun _ ->
          let result = splitByPrefix "test" "differentPrefix"
          Expect.equal result None "Should return None when prefix doesn't match"
          
        testCase "handles exact match" <| fun _ ->
          let result = splitByPrefix "test" "test"
          Expect.equal result (Some ("test", "")) "Should handle exact match"
          
        testCase "case sensitive matching" <| fun _ ->
          let result = splitByPrefix "Test" "testSuffix"
          Expect.equal result None "Should be case sensitive"
      ]
      
      testList "splitByPrefix2" [
        testCase "finds first matching prefix" <| fun _ ->
          let prefixes = ["abc"; "def"; "ghi"]
          let result = splitByPrefix2 prefixes "defSuffix"
          Expect.equal result (Some ("def", "Suffix")) "Should find matching prefix"
          
        testCase "returns None when no prefix matches" <| fun _ ->
          let prefixes = ["abc"; "def"; "ghi"]
          let result = splitByPrefix2 prefixes "xyzSuffix"
          Expect.equal result None "Should return None when no prefix matches"
      ]
    ]
  
  /// Tests for Map module extensions
  let mapTests =
    testList "Map Extensions" [
      
      testList "Map.merge" [
        testCase "merges two empty maps" <| fun _ ->
          let result = Map.merge Map.empty Map.empty
          Expect.equal result Map.empty "Should merge empty maps to empty map"
          
        testCase "merges when first map is empty" <| fun _ ->
          let first = Map.empty
          let second = Map.ofList [("a", 1); ("b", 2)]
          let result = Map.merge first second
          Expect.equal result second "Should return second map when first is empty"
          
        testCase "merges when second map is empty" <| fun _ ->
          let first = Map.ofList [("a", 1); ("b", 2)]
          let second = Map.empty
          let result = Map.merge first second
          Expect.equal result first "Should return first map when second is empty"
          
        testCase "second map values override first map values" <| fun _ ->
          let first = Map.ofList [("a", 1); ("b", 2)]
          let second = Map.ofList [("b", 3); ("c", 4)]
          let result = Map.merge first second
          let expected = Map.ofList [("a", 1); ("b", 3); ("c", 4)]
          Expect.equal result expected "Second map values should override"
      ]
      
      testList "Map.combineTakeFirst" [
        testCase "takes first value for duplicate keys" <| fun _ ->
          let first = Map.ofList [("a", 1); ("b", 2)]
          let second = Map.ofList [("b", 3); ("c", 4)]
          let result = Map.combineTakeFirst first second
          let expected = Map.ofList [("a", 1); ("b", 2); ("c", 4)]
          Expect.equal result expected "Should take first value for duplicate keys"
          
        testCase "combines when no duplicate keys" <| fun _ ->
          let first = Map.ofList [("a", 1); ("b", 2)]
          let second = Map.ofList [("c", 3); ("d", 4)]
          let result = Map.combineTakeFirst first second
          let expected = Map.ofList [("a", 1); ("b", 2); ("c", 3); ("d", 4)]
          Expect.equal result expected "Should combine when no duplicates"
      ]
      
      testList "Map.values" [
        testCase "extracts all values from map" <| fun _ ->
          let map = Map.ofList [("a", 1); ("b", 2); ("c", 3)]
          let result = Map.values map |> Seq.sort |> Seq.toList
          let expected = [1; 2; 3]
          Expect.equal result expected "Should extract all values"
          
        testCase "returns empty sequence for empty map" <| fun _ ->
          let result = Map.values Map.empty |> Seq.toList
          Expect.equal result [] "Should return empty sequence for empty map"
      ]
    ]
  
  /// Tests for active patterns
  let patternTests =
    testList "Active Patterns" [
      
      testList "StartsWith pattern" [
        testCase "matches when string starts with pattern" <| fun _ ->
          match "testString" with
          | StartsWith "test" s -> Expect.equal s "testString" "Should match and return full string"
          | _ -> failtest "Should have matched"
          
        testCase "doesn't match when string doesn't start with pattern" <| fun _ ->
          match "otherString" with
          | StartsWith "test" _ -> failtest "Should not have matched"
          | _ -> () // expected
          
        testCase "handles null strings" <| fun _ ->
          match null with
          | StartsWith "test" _ -> failtest "Should not match null"
          | _ -> () // expected
          
        testCase "handles empty pattern" <| fun _ ->
          match "anyString" with
          | StartsWith "" s -> Expect.equal s "anyString" "Should match with empty pattern"
          | _ -> failtest "Should have matched"
      ]
      
      testList "Contains pattern" [
        testCase "matches when string contains pattern" <| fun _ ->
          match "testStringValue" with
          | Contains "String" s -> Expect.equal s "testStringValue" "Should match and return full string"
          | _ -> failtest "Should have matched"
          
        testCase "doesn't match when string doesn't contain pattern" <| fun _ ->
          match "otherValue" with
          | Contains "String" _ -> failtest "Should not have matched"
          | _ -> () // expected
          
        testCase "handles null strings" <| fun _ ->
          match null with
          | Contains "test" _ -> failtest "Should not match null"
          | _ -> () // expected
      ]
    ]

  let allTests = testList "FsAutoComplete.Utils Tests" [
    fileTypeTests
    pathTests  
    stringTests
    mapTests
    patternTests
  ]
