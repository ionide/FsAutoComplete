namespace FsAutoComplete

open LanguageServerProtocol.Types
open FSharp.Compiler.SourceCodeServices

module KeywordList =

    let keywordDescriptions =
        FSharpKeywords.KeywordsWithDescription
        |> dict

    let keywordTooltips =
      keywordDescriptions
      |> Seq.map (fun kv ->
        let lines = kv.Value.Replace("\r\n", "\n").Split('\n')
        let allLines = Array.concat [| [|"<summary>"|]; lines; [| "</summary>" |] |]
        let tip = FSharpToolTipText [FSharpToolTipElement.Single(kv.Key, FSharpXmlDoc.Text (allLines, allLines))]
        kv.Key, tip)
      |> dict

    let hashDirectives =
        [ "r", "References an assembly"
          "load", "Reads a source file, compiles it, and runs it."
          "I", "Specifies an assembly search path in quotation marks."
          "light", "Enables or disables lightweight syntax, for compatibility with other versions of ML"
          "if", "Supports conditional compilation"
          "else", "Supports conditional compilation"
          "endif", "Supports conditional compilation"
          "nowarn", "Disables a compiler warning or warnings"
          "line", "Indicates the original source code line"]
        |> dict

    let hashSymbolCompletionItems =
        hashDirectives
        |> Seq.map (fun kv ->
            { CompletionItem.Create(kv.Key) with
                Kind = Some CompletionItemKind.Keyword
                InsertText = Some kv.Key
                FilterText = Some kv.Key
                SortText = Some kv.Key
                Documentation = Some (Documentation.String kv.Value)
                Label = "#" + kv.Key
            })
        |> Seq.toArray

    let allKeywords : string list =
        FSharpKeywords.KeywordsWithDescription
        |> List.map fst

    let keywordCompletionItems =
        allKeywords
        |> List.mapi (fun id k ->
            { CompletionItem.Create(k) with
                Kind = Some CompletionItemKind.Keyword
                InsertText = Some k
                SortText = Some (sprintf "1000000%d" id)
                FilterText = Some k
                Label = k })
        |> List.toArray
