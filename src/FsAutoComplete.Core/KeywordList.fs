namespace FsAutoComplete

open LanguageServerProtocol.Types
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Compiler.Symbols
open FSharp.Compiler.Tokenization

module KeywordList =

    let keywordDescriptions =
        FSharpKeywords.KeywordsWithDescription
        |> dict

    let keywordTooltips =
      keywordDescriptions
      |> Seq.map (fun (KeyValue(keyword, description)) ->
        let lines = description.Replace("\r\n", "\n").Split('\n')
        let allLines = Array.concat [| [|"<summary>"|]; lines; [| "</summary>" |] |]
        let data: ToolTipElementData =
          { MainDescription = [||]
            ParamName = None
            Remarks = None
            TypeMapping = []
            XmlDoc = FSharpXmlDoc.FromXmlText (XmlDoc(allLines, Range.range0))
          }
        let tip = ToolTipText [ ToolTipElement.Group [ data ] ]
        keyword, tip)
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
