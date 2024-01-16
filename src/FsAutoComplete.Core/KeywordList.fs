namespace FsAutoComplete

open Ionide.LanguageServerProtocol.Types
open FSharp.Compiler.Text
open FSharp.Compiler.Tokenization
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Symbols

// 44 is the 'This construct is deprecated' error - we've addressed these by moving to TextEdit for the completionItems here,
// but the helper function for the CompletionItem record has to init the field to None, so it's still being counted as used.
#nowarn "44"

module KeywordList =

  let keywordDescriptions = FSharpKeywords.KeywordsWithDescription |> dict

  let keywordTooltips =
    keywordDescriptions
    |> Seq.map (fun kv ->
      let lines = kv.Value.Replace("\r\n", "\n").Split('\n')

      let allLines = Array.concat [| [| "<summary>" |]; lines; [| "</summary>" |] |]

      let tip =
        ToolTipText
          [ ToolTipElement.Single(
              [| TaggedText.tagText kv.Key |],
              FSharpXmlDoc.FromXmlText(FSharp.Compiler.Xml.XmlDoc(allLines, Range.Zero))
            ) ]

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
      "line", "Indicates the original source code line" ]
    |> dict

  let private textEdit text pos : U2<TextEdit, _> =
    U2.First(
      { Range = { Start = pos; End = pos }
        NewText = text }
    )

  let hashSymbolCompletionItems pos =
    hashDirectives
    |> Seq.map (fun kv ->
      let label = "#" + kv.Key

      { CompletionItem.Create(kv.Key) with
          Data = Some(Newtonsoft.Json.Linq.JValue(label))
          Kind = Some CompletionItemKind.Keyword
          TextEdit = Some(textEdit kv.Value pos)
          FilterText = Some kv.Key
          SortText = Some kv.Key
          Documentation = Some(Documentation.String kv.Value)
          Label = label })
    |> Seq.toArray

  let allKeywords: string list =
    keywordDescriptions |> Seq.map ((|KeyValue|) >> fst) |> Seq.toList

  let keywordCompletionItems pos =
    allKeywords
    |> List.mapi (fun id k ->
      { CompletionItem.Create(k) with
          Data = Some(Newtonsoft.Json.Linq.JValue(k))
          Kind = Some CompletionItemKind.Keyword
          TextEdit = Some(textEdit k pos)
          SortText = Some(sprintf "1000000%d" id)
          FilterText = Some k
          Label = k })
    |> List.toArray
