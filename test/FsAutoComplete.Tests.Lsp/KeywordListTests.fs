module FsAutoComplete.Tests.KeywordListTests

open Expecto
open FsAutoComplete.KeywordList
open Ionide.LanguageServerProtocol.Types
open FSharp.Compiler.EditorServices
open Helpers.Expecto.ShadowedTimeouts

let keywordListTests =
  testSequenced <| testList "KeywordList Tests" [
    
    testList "keywordDescriptions Tests" [
      testCase "keywordDescriptions contains common F# keywords"
      <| fun () ->
          let commonKeywords = ["let"; "if"; "then"; "else"; "fun"; "function"; "match"; "with"; "type"; "module"; "namespace"]
          
          for keyword in commonKeywords do
            Expect.isTrue (keywordDescriptions.ContainsKey keyword) $"Should contain keyword '{keyword}'"
            Expect.isNonEmpty keywordDescriptions[keyword] $"Description for '{keyword}' should not be empty"
      
      testCase "keywordDescriptions values are non-empty"
      <| fun () ->
          for kv in keywordDescriptions do
            Expect.isNonEmpty kv.Value $"Description for keyword '{kv.Key}' should not be empty"
    ]
    
    testList "keywordTooltips Tests" [
      testCase "keywordTooltips contains same keys as keywordDescriptions"
      <| fun () ->
          let descriptionKeys = keywordDescriptions.Keys |> Set.ofSeq
          let tooltipKeys = keywordTooltips.Keys |> Set.ofSeq
          
          Expect.equal tooltipKeys descriptionKeys "Tooltip keys should match description keys"
      
      testCase "keywordTooltips values are properly formatted ToolTipText"
      <| fun () ->
          for kv in keywordTooltips do
            match kv.Value with
            | ToolTipText elements ->
                Expect.isNonEmpty elements $"Tooltip for '{kv.Key}' should have at least one element"
      
      testCase "keywordTooltips contains common keywords with proper formatting"
      <| fun () ->
          let testKeywords = ["let"; "if"; "match"; "type"]
          
          for keyword in testKeywords do
            Expect.isTrue (keywordTooltips.ContainsKey keyword) $"Should have tooltip for '{keyword}'"
            
            match keywordTooltips[keyword] with
            | ToolTipText elements ->
                Expect.isNonEmpty elements $"Tooltip for '{keyword}' should have elements"
    ]
    
    testList "hashDirectives Tests" [
      testCase "hashDirectives contains common F# script directives"
      <| fun () ->
          let commonDirectives = ["r"; "load"; "I"; "if"; "else"; "endif"]
          
          for directive in commonDirectives do
            Expect.isTrue (hashDirectives.ContainsKey directive) $"Should contain directive '{directive}'"
            Expect.isNonEmpty hashDirectives[directive] $"Description for directive '{directive}' should not be empty"
      
      testCase "hashDirectives has meaningful descriptions"
      <| fun () ->
          Expect.stringContains hashDirectives["r"] "assembly" "r directive should mention assembly"
          Expect.stringContains hashDirectives["load"] "script" "load directive should mention script"
          Expect.stringContains hashDirectives["I"] "search path" "I directive should mention search path"
          Expect.stringContains hashDirectives["if"] "conditional" "if directive should mention conditional"
      
      testCase "all hashDirectives have non-empty descriptions"
      <| fun () ->
          for kv in hashDirectives do
            Expect.isNonEmpty kv.Value $"Description for directive '{kv.Key}' should not be empty"
            Expect.isTrue (kv.Value.Length > 10) $"Description for directive '{kv.Key}' should be meaningful (>10 chars)"
    ]
    
    testList "hashSymbolCompletionItems Tests" [
      testCase "hashSymbolCompletionItems has correct count"
      <| fun () ->
          let expectedCount = hashDirectives.Count
          Expect.equal hashSymbolCompletionItems.Length expectedCount "Should have completion item for each hash directive"
      
      testCase "hashSymbolCompletionItems have proper structure"
      <| fun () ->
          for item in hashSymbolCompletionItems do
            // Label should start with #
            Expect.isTrue (item.Label.StartsWith("#")) $"Label '{item.Label}' should start with #"
            
            // Kind should be Keyword
            Expect.equal item.Kind (Some CompletionItemKind.Keyword) $"Item '{item.Label}' should be Keyword kind"
            
            // Should have documentation
            Expect.isSome item.Documentation $"Item '{item.Label}' should have documentation"
            
            // InsertText should be the directive name without #
            Expect.isSome item.InsertText $"Item '{item.Label}' should have insert text"
            let directiveName = item.Label.Substring(1)
            Expect.equal item.InsertText (Some directiveName) $"Insert text should be '{directiveName}'"
      
      testCase "hashSymbolCompletionItems contain expected directives"
      <| fun () ->
          let expectedLabels = hashDirectives.Keys |> Seq.map (fun k -> "#" + k) |> Set.ofSeq
          let actualLabels = hashSymbolCompletionItems |> Array.map (fun item -> item.Label) |> Set.ofArray
          
          Expect.equal actualLabels expectedLabels "Should have completion items for all hash directives"
      
      testCase "hashSymbolCompletionItems have proper data field"
      <| fun () ->
          for item in hashSymbolCompletionItems do
            Expect.isSome item.Data $"Item '{item.Label}' should have data field"
            
            match item.Data with
            | Some (jvalue) when jvalue.Type = Newtonsoft.Json.Linq.JTokenType.String ->
                let dataValue = jvalue.Value<string>()
                Expect.equal dataValue item.Label $"Data should match label for '{item.Label}'"
            | _ ->
                failtest $"Data for '{item.Label}' should be a string JValue"
    ]
    
    testList "allKeywords Tests" [
      testCase "allKeywords is non-empty list"
      <| fun () ->
          Expect.isNonEmpty allKeywords "Should have at least some keywords"
          Expect.isTrue (allKeywords.Length > 20) "Should have a reasonable number of keywords (>20)"
      
      testCase "allKeywords contains common F# keywords"
      <| fun () ->
          let commonKeywords = ["let"; "if"; "then"; "else"; "fun"; "function"; "match"; "with"; "type"; "module"]
          
          for keyword in commonKeywords do
            Expect.contains allKeywords keyword $"Should contain keyword '{keyword}'"
      
      testCase "allKeywords matches keywordDescriptions keys"
      <| fun () ->
          let descriptionKeys = keywordDescriptions.Keys |> Seq.toList |> List.sort
          let allKeywordsSorted = allKeywords |> List.sort
          
          Expect.equal allKeywordsSorted descriptionKeys "allKeywords should match keywordDescriptions keys"
    ]
    
    testList "keywordCompletionItems Tests" [
      testCase "keywordCompletionItems has correct count"
      <| fun () ->
          let expectedCount = allKeywords.Length
          Expect.equal keywordCompletionItems.Length expectedCount "Should have completion item for each keyword"
      
      testCase "keywordCompletionItems have proper structure"
      <| fun () ->
          for item in keywordCompletionItems do
            // Kind should be Keyword
            Expect.equal item.Kind (Some CompletionItemKind.Keyword) $"Item '{item.Label}' should be Keyword kind"
            
            // Label should match InsertText and FilterText
            Expect.equal item.InsertText (Some item.Label) $"InsertText should match label for '{item.Label}'"
            Expect.equal item.FilterText (Some item.Label) $"FilterText should match label for '{item.Label}'"
            
            // Should have Data field
            Expect.isSome item.Data $"Item '{item.Label}' should have data field"
            
            // SortText should be present and follow pattern
            Expect.isSome item.SortText $"Item '{item.Label}' should have sort text"
            
            match item.SortText with
            | Some sortText -> Expect.isTrue (sortText.StartsWith("1000000")) $"Sort text for '{item.Label}' should start with '1000000'"
            | None -> failtest $"Sort text should be present for '{item.Label}'"
      
      testCase "keywordCompletionItems have proper data values"
      <| fun () ->
          for item in keywordCompletionItems do
            match item.Data with
            | Some jvalue when jvalue.Type = Newtonsoft.Json.Linq.JTokenType.String ->
                let dataValue = jvalue.Value<string>()
                Expect.equal dataValue item.Label $"Data should match label for '{item.Label}'"
            | _ ->
                failtest $"Data for '{item.Label}' should be a string JValue"
      
      testCase "keywordCompletionItems contain expected keywords"
      <| fun () ->
          let expectedLabels = allKeywords |> Set.ofList
          let actualLabels = keywordCompletionItems |> Array.map (fun item -> item.Label) |> Set.ofArray
          
          Expect.equal actualLabels expectedLabels "Should have completion items for all keywords"
      
      testCase "keywordCompletionItems have unique sort text"
      <| fun () ->
          let sortTexts = 
            keywordCompletionItems 
            |> Array.choose (fun item -> item.SortText)
            |> Array.toList
          
          let uniqueSortTexts = sortTexts |> List.distinct
          
          Expect.equal sortTexts.Length uniqueSortTexts.Length "All sort texts should be unique"
    ]
    
    testList "Integration Tests" [
      testCase "all collections are consistent"
      <| fun () ->
          // Verify that all the different collections are properly related
          let descriptionKeyCount = keywordDescriptions.Count
          let tooltipKeyCount = keywordTooltips.Count
          let allKeywordsCount = allKeywords.Length
          let completionItemsCount = keywordCompletionItems.Length
          
          Expect.equal tooltipKeyCount descriptionKeyCount "Tooltips and descriptions should have same count"
          Expect.equal allKeywordsCount descriptionKeyCount "allKeywords and descriptions should have same count"
          Expect.equal completionItemsCount descriptionKeyCount "Completion items and descriptions should have same count"
      
      testCase "hash directives and completion items are consistent"
      <| fun () ->
          let directiveCount = hashDirectives.Count
          let hashCompletionCount = hashSymbolCompletionItems.Length
          
          Expect.equal hashCompletionCount directiveCount "Hash completion items and directives should have same count"
    ]
  ]