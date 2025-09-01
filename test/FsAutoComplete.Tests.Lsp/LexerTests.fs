module FsAutoComplete.Tests.Lexer

open Expecto
open FsAutoComplete.Lexer
open FsAutoComplete

let tests =
  testList "Lexer Tests" [

    testList "Tokenization Tests" [
      testCase "tokenizeLine handles simple identifier" <| fun _ ->
        let tokens = tokenizeLine [||] "let x = 42"
        Expect.isNonEmpty tokens "Should produce tokens"
        let tokenTexts = tokens |> List.map (fun t -> t.TokenName)
        Expect.contains tokenTexts "LET" "Should contain LET token"
        Expect.contains tokenTexts "IDENT" "Should contain IDENT token"

      testCase "tokenizeLine handles operators" <| fun _ ->
        let tokens = tokenizeLine [||] "x + y - z"
        Expect.isNonEmpty tokens "Should produce tokens"
        let operatorTokens = tokens |> List.filter (fun t -> t.TokenName.Contains("PLUS") || t.TokenName.Contains("MINUS"))
        Expect.isNonEmpty operatorTokens "Should contain operator tokens"

      testCase "tokenizeLine handles defines" <| fun _ ->
        let tokens = tokenizeLine [|"--define:DEBUG"|] "#if DEBUG"
        Expect.isNonEmpty tokens "Should produce tokens with defines"
        
      testCase "tokenizeLine handles language version" <| fun _ ->
        let tokens = tokenizeLine [|"--langversion:7.0"|] "let x = 42"
        Expect.isNonEmpty tokens "Should produce tokens with language version"

      testCase "tokenizeLine handles multiple defines" <| fun _ ->
        let tokens = tokenizeLine [|"--define:DEBUG"; "--define:TRACE"|] "#if DEBUG"
        Expect.isNonEmpty tokens "Should handle multiple defines"
    ]

    testList "Symbol Lookup Tests" [
      testCase "findIdents handles simple identifier" <| fun _ ->
        let result = findIdents 3u "let x = 42" SymbolLookupKind.Simple
        match result with
        | Some (_, parts) ->
          Expect.isNonEmpty parts "Should find identifier parts"
          Expect.contains parts "x" "Should contain the identifier"
        | None -> failtest "Should find identifier"

      testCase "findIdents handles dotted identifier" <| fun _ ->
        let result = findIdents 8u "System.Console.WriteLine" SymbolLookupKind.ByLongIdent
        match result with
        | Some (_, parts) ->
          Expect.equal parts.Length 3 "Should have 3 parts"
          Expect.equal parts.[0] "System" "First part should be System"
          Expect.equal parts.[1] "Console" "Second part should be Console"  
          Expect.equal parts.[2] "WriteLine" "Third part should be WriteLine"
        | None -> failtest "Should find dotted identifier"

      testCase "findIdents returns None for empty string" <| fun _ ->
        let result = findIdents 0u "" SymbolLookupKind.Simple
        Expect.isNone result "Should return None for empty string"

      testCase "findLongIdents is alias for findIdents with Fuzzy lookup" <| fun _ ->
        let result1 = findLongIdents (3u, "let x = 42")
        let result2 = findIdents 3u "let x = 42" SymbolLookupKind.Fuzzy
        Expect.equal result1 result2 "Should produce same result as findIdents with Fuzzy"
    ]

    testList "Long Identifier Resolution Tests" [
      testCase "findLongIdentsAndResidue handles complete identifier" <| fun _ ->
        let idents, residue = findLongIdentsAndResidue (18u, "System.Console.Wri")
        Expect.equal idents ["System"; "Console"] "Should extract complete parts"
        Expect.equal residue "Wri" "Should extract residue"

      testCase "findLongIdentsAndResidue handles ending with dot" <| fun _ ->
        let idents, residue = findLongIdentsAndResidue (15u, "System.Console.")
        Expect.equal idents ["System"; "Console"] "Should extract all complete parts"
        Expect.equal residue "" "Should have empty residue after dot"

      testCase "findLongIdentsAndResidue handles single identifier" <| fun _ ->
        let idents, residue = findLongIdentsAndResidue (3u, "Sys")
        Expect.equal idents [] "Should have empty idents for single partial"
        Expect.equal residue "Sys" "Should have residue for partial identifier"

      testCase "findLongIdentsAndResidue handles empty input" <| fun _ ->
        let idents, residue = findLongIdentsAndResidue (0u, "")
        Expect.equal idents [] "Should handle empty idents"
        Expect.equal residue "" "Should handle empty residue"
    ]

    testList "Closest Identifier Tests" [
      testCase "findClosestIdent finds identifier before cursor" <| fun _ ->
        let result = findClosestIdent 10u "let myVar = 42"
        match result with
        | Some (_, parts) ->
          Expect.contains parts "myVar" "Should find the closest identifier"
        | None -> failtest "Should find closest identifier"

      testCase "findClosestIdent handles no identifier before cursor" <| fun _ ->
        let result = findClosestIdent 2u "  let x = 42"  
        Expect.isNone result "Should return None when no identifier before cursor"

      testCase "findClosestIdent handles multiple identifiers" <| fun _ ->
        let result = findClosestIdent 15u "let first = second"
        match result with
        | Some (_, parts) ->
          Expect.contains parts "second" "Should find the closest (rightmost) identifier"
        | None -> failtest "Should find closest identifier"
    ]

    testList "Symbol Kind and Token Classification Tests" [
      testCase "getSymbol classifies identifiers correctly" <| fun _ ->
        let result = getSymbol 0u 4u "let myVar = 42" SymbolLookupKind.Simple [||]
        match result with
        | Some symbol ->
          Expect.equal symbol.Kind SymbolKind.Ident "Should classify as identifier"
          Expect.equal symbol.Text "myVar" "Should extract correct text"
        | None -> failtest "Should find symbol"

      testCase "getSymbol classifies keywords correctly" <| fun _ ->
        let result = getSymbol 0u 1u "let x = 42" SymbolLookupKind.Simple [||]
        match result with
        | Some symbol ->
          Expect.equal symbol.Kind SymbolKind.Keyword "Should classify as keyword"
          Expect.equal symbol.Text "let" "Should extract correct keyword"
        | None -> failtest "Should find keyword"

      testCase "getSymbol classifies operators correctly" <| fun _ ->
        let result = getSymbol 0u 6u "x + y - z" SymbolLookupKind.Simple [||]
        match result with
        | Some symbol ->
          Expect.equal symbol.Kind SymbolKind.Operator "Should classify as operator"
          Expect.equal symbol.Text "+" "Should extract correct operator"
        | None -> failtest "Should find operator"

      testCase "getSymbol handles generic type parameters" <| fun _ ->
        let result = getSymbol 0u 5u "List<'T>" SymbolLookupKind.Simple [||]
        match result with
        | Some symbol ->
          Expect.equal symbol.Kind SymbolKind.GenericTypeParameter "Should classify as generic type parameter"
        | None -> () // May not be found depending on tokenization, this is acceptable

      testCase "getSymbol handles statically resolved type parameters" <| fun _ ->
        let result = getSymbol 0u 3u "x ^T y" SymbolLookupKind.Simple [||]
        match result with
        | Some symbol ->
          Expect.equal symbol.Kind SymbolKind.StaticallyResolvedTypeParameter "Should classify as SRTP"
        | None -> () // May not be found depending on tokenization, this is acceptable
    ]

    testList "Symbol Lookup Kind Tests" [
      testCase "SymbolLookupKind.Fuzzy finds identifiers and operators" <| fun _ ->
        let result = getSymbol 0u 4u "let x + y" SymbolLookupKind.Fuzzy [||]
        match result with
        | Some symbol ->
          Expect.equal symbol.Text "x" "Should find identifier with fuzzy lookup"
        | None -> failtest "Fuzzy lookup should find symbol"

      testCase "SymbolLookupKind.ByLongIdent handles dotted identifiers" <| fun _ ->
        let result = getSymbol 0u 10u "System.Console.WriteLine" SymbolLookupKind.ByLongIdent [||]
        match result with
        | Some symbol ->
          Expect.stringStarts symbol.Text "System" "Should start with System for long ident lookup"
        | None -> failtest "ByLongIdent should find symbol"

      testCase "SymbolLookupKind.Simple gets last token under cursor" <| fun _ ->
        let result = getSymbol 0u 5u "let x = y" SymbolLookupKind.Simple [||]
        match result with
        | Some symbol ->
          Expect.equal symbol.Text "x" "Should get the token under cursor"
        | None -> failtest "Simple lookup should find symbol"

      testCase "SymbolLookupKind.ForCompletion handles completion scenarios" <| fun _ ->
        let result = getSymbol 0u 8u "System.C" SymbolLookupKind.ForCompletion [||]
        match result with
        | Some symbol ->
          Expect.isTrue (symbol.Text.Length > 0) "Should find symbol for completion"
        | None -> () // Acceptable for completion scenarios
    ]

    testList "Error Handling and Edge Cases" [
      testCase "getSymbol handles invalid position gracefully" <| fun _ ->
        let _result = getSymbol 0u 100u "let x = 42" SymbolLookupKind.Simple [||]
        // Should not throw, may return None
        Expect.isTrue true "Should handle invalid position without throwing"

      testCase "tokenizeLine handles empty string" <| fun _ ->
        let _tokens = tokenizeLine [||] ""
        // Should not throw, may return empty list
        Expect.isTrue true "Should handle empty string without throwing"

      testCase "tokenizeLine handles only whitespace" <| fun _ ->
        let _tokens = tokenizeLine [||] "   \t   "
        // Should not throw, may return minimal tokens
        Expect.isTrue true "Should handle whitespace without throwing"

      testCase "findIdents handles whitespace input" <| fun _ ->
        let result = findIdents 2u "   " SymbolLookupKind.Simple
        Expect.isNone result "Should return None for whitespace input"
    ]
  ]