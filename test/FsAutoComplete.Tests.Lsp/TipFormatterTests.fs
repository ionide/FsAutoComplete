/// Unit tests for FsAutoComplete.TipFormatter, specifically the <seealso> XML doc tag rendering
/// introduced / fixed in https://github.com/ionide/FsAutoComplete/pull/1463.
module FsAutoComplete.Tests.TipFormatterTests

open Expecto
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open FsAutoComplete

/// Build an FSharpXmlDoc from raw XML lines (the same approach used in KeywordList.fs).
let private makeXmlDoc (lines: string[]) = FSharpXmlDoc.FromXmlText(FSharp.Compiler.Xml.XmlDoc(lines, Range.Zero))

/// Call formatDocumentationFromXmlDoc and return the Success string, failing the test otherwise.
let private getDoc (xmlDoc: FSharpXmlDoc) =
  match TipFormatter.formatDocumentationFromXmlDoc xmlDoc with
  | TipFormatter.TipFormatterResult.Success s -> s
  | TipFormatter.TipFormatterResult.None -> failtest "Expected doc content but got None"
  | TipFormatter.TipFormatterResult.Error e -> failtest $"Expected doc content but got Error: {e}"

let seeAlsoTests =
  testList
    "seealso rendering"
    [ testCase "cref attribute renders member name as inline code"
      <| fun _ ->
        let xml =
          makeXmlDoc [| "<summary>Description</summary>"; """<seealso cref="T:Foo.Bar"/>""" |]

        let content = getDoc xml
        Expect.stringContains content "* `Foo.Bar`" "cref should render as backtick-quoted member name"

      testCase "href void element renders as auto-link"
      <| fun _ ->
        let xml =
          makeXmlDoc
            [| "<summary>Description</summary>"
               """<seealso href="https://example.com"/>""" |]

        let content = getDoc xml

        Expect.stringContains
          content
          "* [https://example.com](https://example.com)"
          "href void element should produce a Markdown link where both label and URL are the href value"

      testCase "href element with inner text renders as labelled link"
      <| fun _ ->
        let xml =
          makeXmlDoc
            [| "<summary>Description</summary>"
               """<seealso href="https://example.com">Click here</seealso>""" |]

        let content = getDoc xml

        Expect.stringContains
          content
          "* [Click here](https://example.com)"
          "href with inner text should use the inner text as the link label"

      testCase "langword attribute renders keyword as inline code"
      <| fun _ ->
        let xml =
          makeXmlDoc [| "<summary>Description</summary>"; """<seealso langword="null"/>""" |]

        let content = getDoc xml
        Expect.stringContains content "* `null`" "langword should render as backtick-quoted keyword"

      testCase "unrecognised attribute is silently skipped"
      <| fun _ ->
        let xml =
          makeXmlDoc [| "<summary>Description</summary>"; """<seealso unknown="foo"/>""" |]

        let content = getDoc xml

        Expect.isFalse
          (content.Contains "See also")
          "an unrecognised attribute should produce no See also section rather than garbled output"

      testCase "multiple seealso entries all appear in See also section"
      <| fun _ ->
        let xml =
          makeXmlDoc
            [| "<summary>Description</summary>"
               """<seealso cref="T:Foo.Bar"/>"""
               """<seealso href="https://example.com"/>"""
               """<seealso langword="null"/>""" |]

        let content = getDoc xml
        Expect.stringContains content "* `Foo.Bar`" "cref entry should be present"

        Expect.stringContains content "* [https://example.com](https://example.com)" "href entry should be present"

        Expect.stringContains content "* `null`" "langword entry should be present" ]

let allTests = testList "TipFormatter" [ seeAlsoTests ]
