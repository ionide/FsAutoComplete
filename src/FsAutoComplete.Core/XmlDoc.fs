module FsAutoComplete.XmlDoc

open System
open System.IO
open System.Text.RegularExpressions
open System.Xml
open System.Xml.Linq
open System.Collections.Generic
open FSharp.Compiler.EditorServices
open System.Text

let inline nl<'T> = Environment.NewLine

module private Format =

    let tagPattern (tagName : string) =
        sprintf """(?'void_element'<%s(?'void_attributes'\s+[^\/>]+)?\/>)|(?'non_void_element'<%s(?'non_void_attributes'\s+[^>]+)?>(?'non_void_innerText'(?:(?!<%s>)(?!<\/%s>)[\s\S])*)<\/%s\s*>)""" tagName tagName tagName tagName tagName

    type TagInfo =
        | VoidElement of attributes : Map<string, string>
        | NonVoidElement of innerText : string * attributes : Map<string, string>

    type FormatterInfo =
        {
            TagName : string
            Formatter : TagInfo -> string option
        }

    let private extractTextFromQuote (quotedText : string) =
        quotedText.Substring(1, quotedText.Length - 2)


    let extractMemberText (text : string) =
        let pattern = "(?'member_type'[a-z]{1}:)?(?'member_text'.*)"
        let m = Regex.Match(text, pattern, RegexOptions.IgnoreCase)

        if m.Groups.["member_text"].Success then
            m.Groups.["member_text"].Value
        else
            text

    let private getAttributes (attributes : Group) =
        if attributes.Success then
            let pattern = """(?'key'\S+)=(?'value''[^']*'|"[^"]*")"""
            Regex.Matches(attributes.Value, pattern, RegexOptions.IgnoreCase)
            |> Seq.cast<Match>
            |> Seq.map (fun m ->
                m.Groups.["key"].Value, extractTextFromQuote m.Groups.["value"].Value
            )
            |> Map.ofSeq
        else
            Map.empty

    let rec private applyFormatter (info : FormatterInfo) text =
        let pattern = tagPattern info.TagName
        match Regex.Match(text, pattern, RegexOptions.IgnoreCase) with
        | m when m.Success ->
            if m.Groups.["void_element"].Success then
                let attributes = getAttributes m.Groups.["void_attributes"]

                let replacement =
                    VoidElement attributes
                    |> info.Formatter

                match replacement with
                | Some replacement ->
                    text.Replace(m.Groups.["void_element"].Value, replacement)
                    // Re-apply the formatter, because perhaps there is more
                    // of the current tag to convert
                    |> applyFormatter info

                | None ->
                    // The formatter wasn't able to convert the tag
                    // Return as it is and don't re-apply the formatter
                    // otherwise it will create an infinity loop
                    text

            else if m.Groups.["non_void_element"].Success then
                let innerText = m.Groups.["non_void_innerText"].Value
                let attributes = getAttributes m.Groups.["non_void_attributes"]

                let replacement =
                    NonVoidElement (innerText, attributes)
                    |> info.Formatter

                match replacement with
                | Some replacement ->
                    // Re-apply the formatter, because perhaps there is more
                    // of the current tag to convert
                    text.Replace(m.Groups.["non_void_element"].Value, replacement)
                    |> applyFormatter info

                | None ->
                    // The formatter wasn't able to convert the tag
                    // Return as it is and don't re-apply the formatter
                    // otherwise it will create an infinity loop
                    text
            else
                // Should not happend but like that we are sure to handle all possible cases
                text
        | _ ->
            text

    let private codeBlock =
        {
            TagName = "code"
            Formatter =
                function
                | VoidElement _ ->
                    None

                | NonVoidElement (innerText, attributes) ->
                    let lang =
                        match Map.tryFind "lang" attributes with
                        | Some lang ->
                            lang

                        | None ->
                            "forceNoHighlight"

                    let formattedText =
                        if innerText.Contains(nl) then

                            if innerText.StartsWith(nl) then
                              $"```%s{lang}%s{innerText}{nl}```"

                            else
                                $"```%s{lang}{nl}%s{innerText}{nl}```"

                        else
                            sprintf "`%s`" innerText

                    Some formattedText

        }
        |> applyFormatter

    let private codeInline =
        {
            TagName = "c"
            Formatter  =
                function
                | VoidElement _ ->
                    None
                | NonVoidElement (innerText, _) ->
                    "`" + innerText + "`"
                    |> Some
        }
        |> applyFormatter

    let private anchor =
        {
            TagName = "a"
            Formatter =
                function
                | VoidElement _ ->
                    None

                | NonVoidElement (innerText, attributes) ->
                    let href =
                        match Map.tryFind "href" attributes with
                        | Some href ->
                            href

                        | None ->
                            ""

                    sprintf "[%s](%s)" innerText href
                    |> Some
        }
        |> applyFormatter

    let private paragraph =
        {
            TagName = "para"
            Formatter =
                function
                | VoidElement _ ->
                    None

                | NonVoidElement (innerText, _) ->
                    nl + innerText + nl
                    |> Some
        }
        |> applyFormatter

    let private block =
        {
            TagName = "block"
            Formatter  =
                function
                | VoidElement _ ->
                    None

                | NonVoidElement (innerText, _) ->
                    nl + innerText + nl
                    |> Some
        }
        |> applyFormatter

    let private see =
        let getCRef (attributes : Map<string, string>) = Map.tryFind "cref" attributes
        {
            TagName = "see"
            Formatter =
                function
                | VoidElement attributes ->
                    match getCRef attributes with
                    | Some cref ->
                        // TODO: Add config to generates command
                        "`" + extractMemberText cref + "`"
                        |> Some

                    | None ->
                        None

                | NonVoidElement (innerText, attributes) ->
                    if String.IsNullOrWhiteSpace innerText then
                        match getCRef attributes with
                        | Some cref ->
                            // TODO: Add config to generates command
                            "`" + extractMemberText cref + "`"
                            |> Some

                        | None ->
                            None
                    else
                        "`" + innerText + "`"
                        |> Some
        }
        |> applyFormatter

    let private xref =
        let getHRef (attributes : Map<string, string>) = Map.tryFind "href" attributes
        {
            TagName = "xref"
            Formatter =
                function
                | VoidElement attributes ->
                    match getHRef attributes with
                    | Some href ->
                        // TODO: Add config to generates command
                        "`" + extractMemberText href + "`"
                        |> Some

                    | None ->
                        None

                | NonVoidElement (innerText, attributes) ->
                    if String.IsNullOrWhiteSpace innerText then
                        match getHRef attributes with
                        | Some href ->
                            // TODO: Add config to generates command
                            "`" + extractMemberText href + "`"
                            |> Some

                        | None ->
                            None
                    else
                        "`" + innerText + "`"
                        |> Some
        }
        |> applyFormatter

    let private paramRef =
        let getName (attributes : Map<string, string>) = Map.tryFind "name" attributes

        {
            TagName = "paramref"
            Formatter =
                function
                | VoidElement attributes ->
                    match getName attributes with
                    | Some name ->
                        "`" + name + "`"
                        |> Some

                    | None ->
                        None

                | NonVoidElement (innerText, attributes) ->
                    if String.IsNullOrWhiteSpace innerText then
                        match getName attributes with
                        | Some name ->
                            // TODO: Add config to generates command
                            "`" + name + "`"
                            |> Some

                        | None ->
                            None
                    else
                        "`" + innerText + "`"
                        |> Some

        }
        |> applyFormatter

    let private typeParamRef =
        let getName (attributes : Map<string, string>) = Map.tryFind "name" attributes

        {
            TagName = "typeparamref"
            Formatter =
                function
                | VoidElement attributes ->
                    match getName attributes with
                    | Some name ->
                        "`" + name + "`"
                        |> Some

                    | None ->
                        None

                | NonVoidElement (innerText, attributes) ->
                    if String.IsNullOrWhiteSpace innerText then
                        match getName attributes with
                        | Some name ->
                            // TODO: Add config to generates command
                            "`" + name + "`"
                            |> Some

                        | None ->
                            None
                    else
                        "`" + innerText + "`"
                        |> Some
        }
        |> applyFormatter

    let private fixPortableClassLibrary (text : string) =
        text.Replace(
            "~/docs/standard/cross-platform/cross-platform-development-with-the-portable-class-library.md",
            "https://docs.microsoft.com/en-gb/dotnet/standard/cross-platform/cross-platform-development-with-the-portable-class-library"
        )

    /// <summary>Handle Microsoft 'or' formatting blocks</summary>
    /// <remarks>
    /// <para>We don't use the formatter API here because we are not handling a "real XML element"</para>
    /// <para>We don't use regex neither because I am not able to create one covering all the possible case</para>
    /// <para>
    /// There are 2 types of 'or' blocks:
    ///
    /// - Inlined: [...]  -or-  [...]  -or-  [...]
    /// - Blocked:
    /// [...]
    /// -or-
    /// [...]
    /// -or-
    /// [...]
    /// </para>
    /// <para>
    /// This function can convert both styles. If an 'or' block is encounter the whole section will always result in a multiline output
    /// </para>
    /// <para>
    /// If we pass any of the 2 previous example, it will generate the same Markdown string as a result (because they have the same number of 'or' section). The result will be:
    /// </para>
    /// <para>
    /// >    [...]
    ///
    /// *or*
    ///
    /// >    [...]
    ///
    /// *or*
    ///
    /// >    [...]
    /// </para>
    /// </remarks>
    let private handleMicrosoftOrList (text : string) =
        let splitResult = text.Split([|"-or-"|], StringSplitOptions.RemoveEmptyEntries)

        // If text doesn't contains any `-or-` then we just forward it
        if Seq.length splitResult = 1 then
            text
        else
            splitResult
            |> Seq.map (fun orText ->
                let orText = orText.Trim()
                let lastParagraphStartIndex = orText.LastIndexOf(nl)

                // We make the assumption that an 'or' section should always be defined on a single line
                // From testing against different 'or' block written by Microsoft it seems to be always the case
                // By doing this assumption this allow us to correctly handle comments like:
                //
                // <block>
                // Some text goes here
                // </block>
                // CaseA of the or section
                // -or-
                // CaseB of the or section
                // -or-
                // CaseC of the or section
                //
                // The original comments is for `System.Uri("")`
                // By making the assumption that an 'or' section is always single line this allows us the detact the "<block></block>" section

                // orText is on a single line, we just add quotation syntax
                if lastParagraphStartIndex = -1 then
                    sprintf ">    %s" orText

                // orText is on multiple lines
                // 1. We first extract the everything until the last line
                // 2. We extract on the last line
                // 3. We return the start of the section and the end of the section marked using quotation
                else
                    let startText = orText.Substring(0, lastParagraphStartIndex)
                    let endText =  orText.Substring(lastParagraphStartIndex)

                    $"%s{startText}{nl}>    %s{endText}"
            )
            // Force a new `-or-` paragraph between each orSection
            // In markdown a new paragraph is define by using 2 empty lines
            |> String.concat $"{nl}{nl}*or*{nl}{nl}"

    /// <summary>Remove all invalid 'or' block found</summary>
    /// <remarks>
    /// If an 'or' block is found between 2 elements then we remove it as we can't generate a valid markdown for it
    ///
    /// For example, <td> Some text -or- another text </td> cannot be converted into a multiline string
    /// and so we prefer to remove the 'or' block instead of having some weird markdown artefacts
    ///
    /// For now, we only consider text between <td></td> to be invalid
    /// We can add more in the future if needed, but I want to keep this as minimal as possible to avoid capturing false positive
    /// </remarks>
    let private removeInvalidOrBlock (text : string) =
        let invalidOrBlockPattern = """<td(\s+[^>])*>(?'or_text'(?:(?!<td)[\s\S])*-or-(?:(?!<\/td)[\s\S])*)<\/td(\s+[^>])*>"""

        Regex.Matches(text, invalidOrBlockPattern, RegexOptions.Multiline)
        |> Seq.cast<Match>
        |> Seq.fold (fun (state : string) (m : Match) ->
            let orText = m.Groups.["or_text"]

            if orText.Success then
                let replacement =
                    orText.Value.Replace("-or-", "or")

                state.Replace(orText.Value, replacement)
            else
                state
        ) text


    let private convertTable =
        {
            TagName = "table"
            Formatter =
                function
                | VoidElement _ ->
                    None

                | NonVoidElement (innerText, _) ->

                    let rowCount = Regex.Matches(innerText, "<th\s?>").Count
                    let convertedTable =
                        innerText
                            .Replace(nl, "")
                            .Replace(nl, "")
                            .Replace("<table>", "")
                            .Replace("</table>", "")
                            .Replace("<thead>", "")
                            .Replace("</thead>", (String.replicate rowCount "| --- "))
                            .Replace("<tbody>", nl)
                            .Replace("</tbody>", "")
                            .Replace("<tr>", "")
                            .Replace("</tr>", "|" + nl)
                            .Replace("<th>", "|")
                            .Replace("</th>", "")
                            .Replace("<td>", "|")
                            .Replace("</td>", "")

                    nl + nl + convertedTable + nl
                    |> Some

        }
        |> applyFormatter

    type private Term = string
    type private Definition = string

    type private ListStyle =
        | Bulleted
        | Numbered
        | Tablered

    /// ItemList allow a permissive representation of an Item.
    /// In theory, TermOnly should not exist but we added it so part of the documentation doesn't disappear
    /// TODO: Allow direct text support without <description> and <term> tags
    type private ItemList =
        /// A list where the items are just contains in a <description> element
        | DescriptionOnly of string
        /// A list where the items are just contains in a <term> element
        | TermOnly of string
        /// A list where the items are a term followed by a definition (ie in markdown: * <TERM> - <DEFINITION>)
        | Definitions of Term * Definition

    let private itemListToStringAsMarkdownList (prefix : string) (item : ItemList) =
        match item with
        | DescriptionOnly description ->
            prefix + " " + description
        | TermOnly term ->
            prefix + " " + "**" + term + "**"
        | Definitions (term, description) ->
            prefix + " " + "**" + term + "** - " + description

    let private list =
        let getType (attributes : Map<string, string>) = Map.tryFind "type" attributes

        let tryGetInnerTextOnNonVoidElement (text : string) (tagName : string) =
            match Regex.Match(text, tagPattern tagName, RegexOptions.IgnoreCase) with
            | m when m.Success ->
                if m.Groups.["non_void_element"].Success then
                    Some m.Groups.["non_void_innerText"].Value
                else
                    None
            | _ ->
                None

        let tryGetNonVoidElement (text : string) (tagName : string) =
            match Regex.Match(text, tagPattern tagName, RegexOptions.IgnoreCase) with
            | m when m.Success ->
                if m.Groups.["non_void_element"].Success then
                    Some (m.Groups.["non_void_element"].Value, m.Groups.["non_void_innerText"].Value)
                else
                    None
            | _ ->
                None

        let tryGetDescription (text : string) = tryGetInnerTextOnNonVoidElement text "description"

        let tryGetTerm (text : string) = tryGetInnerTextOnNonVoidElement text "term"

        let rec extractItemList (res : ItemList list) (text : string) =
            match Regex.Match(text, tagPattern "item", RegexOptions.IgnoreCase) with
            | m when m.Success ->
                let newText = text.Substring(m.Value.Length)
                if m.Groups.["non_void_element"].Success then
                    let innerText = m.Groups.["non_void_innerText"].Value
                    let description = tryGetDescription innerText
                    let term = tryGetTerm innerText

                    let currentItem : ItemList option =
                        match description, term with
                        | Some description, Some term ->
                            Definitions (term, description)
                            |> Some
                        | Some description, None ->
                            DescriptionOnly description
                            |> Some
                        | None, Some term ->
                            TermOnly term
                            |> Some
                        | None, None ->
                            None

                    match currentItem with
                    | Some currentItem ->
                        extractItemList (res @ [ currentItem ]) newText
                    | None ->
                        extractItemList res newText
                else
                    extractItemList res newText
            | _ ->
                res

        let rec extractColumnHeader (res : string list) (text : string) =
            match Regex.Match(text, tagPattern "listheader", RegexOptions.IgnoreCase) with
            | m when m.Success ->
                let newText = text.Substring(m.Value.Length)
                if m.Groups.["non_void_element"].Success then
                    let innerText = m.Groups.["non_void_innerText"].Value

                    let rec extractAllTerms (res : string list) (text : string) =
                        match tryGetNonVoidElement text "term" with
                        | Some (fullString, innerText) ->
                            let escapedRegex = Regex(Regex.Escape(fullString))
                            let newText = escapedRegex.Replace(text, "", 1)
                            extractAllTerms (res @ [ innerText ]) newText
                        | None ->
                            res

                    extractColumnHeader (extractAllTerms [] innerText) newText
                else
                    extractColumnHeader res newText
            | _ ->
                res


        let rec extractRowsForTable (res : (string list) list) (text : string) =
            match Regex.Match(text, tagPattern "item", RegexOptions.IgnoreCase) with
            | m when m.Success ->
                let newText = text.Substring(m.Value.Length)
                if m.Groups.["non_void_element"].Success then
                    let innerText = m.Groups.["non_void_innerText"].Value

                    let rec extractAllTerms (res : string list) (text : string) =
                        match tryGetNonVoidElement text "term" with
                        | Some (fullString, innerText) ->
                            let escapedRegex = Regex(Regex.Escape(fullString))
                            let newText = escapedRegex.Replace(text, "", 1)
                            extractAllTerms (res @ [ innerText ]) newText
                        | None ->
                            res

                    extractRowsForTable (res @ [extractAllTerms [] innerText]) newText
                else
                    extractRowsForTable res newText
            | _ ->
                res

        {
            TagName = "list"
            Formatter =
                function
                | VoidElement _ ->
                    None

                | NonVoidElement (innerText, attributes) ->
                    let listStyle =
                        match getType attributes with
                        | Some "bullet" -> Bulleted
                        | Some "number" -> Numbered
                        | Some "table" -> Tablered
                        | Some _ | None -> Bulleted

                    match listStyle with
                    | Bulleted ->
                        let items = extractItemList [] innerText

                        items
                        |> List.map (itemListToStringAsMarkdownList "*")
                        |> String.concat nl

                    | Numbered ->
                        let items = extractItemList [] innerText

                        items
                        |> List.map (itemListToStringAsMarkdownList "1.")
                        |> String.concat nl

                    | Tablered ->
                        let columnHeaders = extractColumnHeader [] innerText
                        let rows = extractRowsForTable [] innerText

                        let columnHeadersText =
                            columnHeaders
                            |> List.mapi (fun index header ->
                                if index = 0 then
                                    "| " + header
                                elif index = columnHeaders.Length - 1 then
                                    " | " + header + " |"
                                else
                                    " | " + header
                            )
                            |> String.concat ""

                        let seprator =
                            columnHeaders
                            |> List.mapi (fun index _ ->
                                if index = 0 then
                                    "| ---"
                                elif index = columnHeaders.Length - 1 then
                                    " | --- |"
                                else
                                    " | ---"
                            )
                            |> String.concat ""

                        let itemsText =
                            rows
                            |> List.map (fun columns ->
                                columns
                                |> List.mapi (fun index column ->
                                    if index = 0 then
                                        "| " + column
                                    elif index = columnHeaders.Length - 1 then
                                        " | " + column + " |"
                                    else
                                        " | " + column
                                )
                                |> String.concat ""
                            )
                            |> String.concat nl

                        nl
                        + columnHeadersText
                        + nl
                        + seprator
                        + nl
                        + itemsText
                    |> Some
        }
        |> applyFormatter

    /// <summary>
    /// Unescape XML special characters
    ///
    /// For example, this allows to print '>' in the tooltip instead of '&gt;'
    /// </summary>
    let private unescapeSpecialCharacters (text : string) =
        text.Replace("&lt;", "<")
            .Replace("&gt;", ">")
            .Replace("&quot;", "\"")
            .Replace("&apos;", "'")
            .Replace("&amp;", "&")

    let applyAll (text : string) =
        text
        // Remove invalid syntax first
        // It's easier to identify invalid patterns when no transformation has been done yet
        |> removeInvalidOrBlock
        // Start the transformation process
        |> paragraph
        |> block
        |> codeInline
        |> codeBlock
        |> see
        |> xref
        |> paramRef
        |> typeParamRef
        |> anchor
        |> list
        |> convertTable
        |> fixPortableClassLibrary
        |> handleMicrosoftOrList
        |> unescapeSpecialCharacters

module private Section =

    let inline addSection (name : string) (content : string) =
        if name <> "" then
            nl + nl
            + "**" + name + "**"
            + nl + nl + content
        else
            nl + nl + content

    let fromKeyValueList (name : string) (content : list<KeyValuePair<string, string>>) =
        if List.isEmpty content then
            ""
        else
            content
            |> Seq.map (fun kv ->
                let text =
                    if kv.Value.Contains(nl) then
                        kv.Value.Split('\n')
                        |> Seq.map (fun line ->
                            "> " + line.TrimStart()
                        )
                        |> String.concat nl
                        |> (+) nl // Start the quote block on a new line
                    else
                        kv.Value

                "* `" + kv.Key + "`" + ": " + text
            )
            |> String.concat nl
            |> addSection name

    let fromOption (name : string) (content : string option) =
        if content.IsNone then
            ""
        else
            addSection name content.Value

    let fromList (name : string) (content : string seq) =
        if Seq.isEmpty content then
            ""
        else
            addSection name (content |> String.concat nl)


// TODO: Improve this parser. Is there any other XmlDoc parser available?
type XmlDocMember(doc: XmlDocument, indentationSize : int, columnOffset : int) =
    /// References used to detect if we should remove meaningless spaces
    let tabsOffset = String.replicate (columnOffset + indentationSize) " "
    let readContentForTooltip (node: XmlNode) =
        match node with
        | null -> null
        | _ ->
            let content =
                // Normale the EOL
                // This make it easier to work with line splittig
                node.InnerXml.Replace("\r\n", nl)
                |> Format.applyAll

            content.Split('\n')
            |> Array.map(fun line ->
                if not (String.IsNullOrWhiteSpace line) && line.StartsWith(tabsOffset) then
                    line.Substring(columnOffset + indentationSize)
                else
                    line
            )
            |> String.concat nl

    let readChildren name (doc: XmlDocument) =
        doc.DocumentElement.GetElementsByTagName name
        |> Seq.cast<XmlNode>
        |> Seq.map (fun node -> Format.extractMemberText node.Attributes.[0].InnerText, node)
        |> Seq.toList

    let readRemarks (doc : XmlDocument) =
        doc.DocumentElement.GetElementsByTagName "remarks"
        |> Seq.cast<XmlNode>

    let rawSummary = doc.DocumentElement.ChildNodes.[0]
    let rawParameters = readChildren "param" doc
    let rawRemarks = readRemarks doc
    let rawExceptions = readChildren "exception" doc
    let rawTypeParams = readChildren "typeparam" doc
    let rawReturns =
        doc.DocumentElement.GetElementsByTagName "returns"
        |> Seq.cast<XmlNode>
        |> Seq.tryHead
    let rawExamples =
        doc.DocumentElement.GetElementsByTagName "example"
        |> Seq.cast<XmlNode>
    let readNamedContentAsKvPair (key, content) = KeyValuePair(key, readContentForTooltip content)

    let summary = readContentForTooltip rawSummary

    let parameters = rawParameters |> List.map readNamedContentAsKvPair
    let remarks = rawRemarks |> Seq.map readContentForTooltip
    let exceptions = rawExceptions |> List.map readNamedContentAsKvPair
    let typeParams = rawTypeParams |> List.map readNamedContentAsKvPair
    let examples = rawExamples |> Seq.map readContentForTooltip
    let returns = rawReturns |> Option.map readContentForTooltip
    let seeAlso =
        doc.DocumentElement.GetElementsByTagName "seealso"
        |> Seq.cast<XmlNode>
        |> Seq.map (fun node ->
            "* `" + Format.extractMemberText node.Attributes.[0].InnerText + "`"
        )

    override x.ToString() =
        summary + nl + nl +
        (parameters |> Seq.map (fun kv -> "`" + kv.Key + "`" + ": " + kv.Value) |> String.concat nl) +
        (if exceptions.Length = 0 then ""
         else nl + nl + "Exceptions:" + nl +
                (exceptions |> Seq.map (fun kv -> "\t" + "`" + kv.Key + "`" + ": " + kv.Value) |> String.concat nl))

    member __.ToSummaryOnlyString() =
        // If we where unable to process the doc comment, then just output it as it is
        // For example, this cover the keywords' tooltips
        if String.IsNullOrEmpty summary then
            doc.InnerText
        else
            "**Description**" + nl + nl
            + summary

    member __.ToFullEnhancedString() =
        let content =
            summary
            + Section.fromList "" remarks
            + Section.fromKeyValueList "Type parameters" typeParams
            + Section.fromKeyValueList "Parameters" parameters
            + Section.fromOption "Returns" returns
            + Section.fromKeyValueList "Exceptions" exceptions
            + Section.fromList "Examples" examples
            + Section.fromList "See also" seeAlso

        // If we where unable to process the doc comment, then just output it as it is
        // For example, this cover the keywords' tooltips
        if String.IsNullOrEmpty content then
            doc.InnerText
        else
            "**Description**" + nl + nl
            + content

    member __.ToDocumentationString() =
        "**Description**" + nl + nl
        + summary
        + Section.fromList "" remarks
        + Section.fromKeyValueList "Type parameters" typeParams
        + Section.fromKeyValueList "Parameters" parameters
        + Section.fromOption "Returns" returns
        + Section.fromKeyValueList "Exceptions" exceptions
        + Section.fromList "Examples" examples
        + Section.fromList "See also" seeAlso

    member __.RawLines () =
      let sb = StringBuilder()
      let settings = XmlWriterSettings(Indent = true, IndentChars = "  ", NewLineChars = "\r\n", NewLineHandling = NewLineHandling.Replace)
      use writer = XmlWriter.Create(sb, settings)
      doc.Save(writer)
      sb.ToString().Split(nl)

let rec private readXmlDoc (reader: XmlReader) (indentationSize : int) (acc: Map<string, XmlDocMember>) =
  let acc' =
    match reader.Read() with
    | false -> indentationSize, None
    // Assembly is the first node in the XML and is at least always indended by 1 "tab"
    // So we used it as a reference to detect the tabs sizes
    // This is needed because `netstandard.xml` use 2 spaces tabs
    // Where when building a C# classlib, the xml file use 4 spaces size for example
    | true when reader.Name = "assembly" && reader.NodeType = XmlNodeType.Element ->
        let xli : IXmlLineInfo = (box reader) :?> IXmlLineInfo
        // - 2 : allow us to detect the position before the < char
        xli.LinePosition - 2, Some acc
    | true when reader.Name = "member" && reader.NodeType = XmlNodeType.Element ->
      try
        // We detect the member LinePosition so we can calculate the meaningless spaces later
        let xli : IXmlLineInfo = (box reader) :?> IXmlLineInfo
        let key = reader.GetAttribute("name")
        use subReader = reader.ReadSubtree()
        let doc = XmlDocument()
        doc.Load(subReader)
        // - 3 : allow us to detect the last indentation position
        // This isn't intuitive but from my tests this is what works
        indentationSize, acc |> Map.add key (XmlDocMember(doc, indentationSize, xli.LinePosition - 3)) |> Some
      with
      | ex ->
        indentationSize, Some acc
    | _ -> indentationSize, Some acc

  match acc' with
  | _, None -> acc
  | indentationSize, Some acc' -> readXmlDoc reader indentationSize acc'

let private xmlDocCache = Collections.Concurrent.ConcurrentDictionary<string, Map<string, XmlDocMember>>()

let getXmlDoc (dllFile: string) =
    let xmlFile = Path.ChangeExtension(dllFile, ".xml")
    //Workaround for netstandard.dll
    let xmlFile =
      if xmlFile.Contains "packages" && xmlFile.Contains  "netstandard.library" && xmlFile.Contains "netstandard2.0" then
        Path.Combine(Path.GetDirectoryName(xmlFile), "netstandard.xml")
      else
        xmlFile
    if xmlDocCache.ContainsKey xmlFile then
      Some xmlDocCache.[xmlFile]
    else
      let rec exists filePath tryAgain =
        match File.Exists filePath, tryAgain with
        | true, _ -> Some filePath
        | false, false -> None
        | false, true ->
          // In Linux, we need to check for upper case extension separately
          let filePath = Path.ChangeExtension(filePath, Path.GetExtension(filePath).ToUpper())
          exists filePath false

      match exists xmlFile true with
      | None -> None
      | Some actualXmlFile ->
        // Prevent other threads from tying to add the same doc simultaneously
        xmlDocCache.AddOrUpdate(xmlFile, Map.empty, fun _ _ -> Map.empty) |> ignore
        try
          let cnt = File.ReadAllText actualXmlFile
          //Workaround for netstandard xmlDoc
          let cnt =
            if actualXmlFile.Contains "netstandard.xml" then
                let cnt = Regex.Replace(cnt,"""(<p .*?>)+(.*)(<\/?p>)*""", "$2")
                cnt.Replace("<p>", "").Replace("</p>", "").Replace("<br>", "")
            else
                cnt

          use stringReader = new StringReader(cnt)
          use reader = XmlReader.Create stringReader
          let xmlDoc = readXmlDoc reader 0 Map.empty
          xmlDocCache.AddOrUpdate(xmlFile, xmlDoc, fun _ _ -> xmlDoc) |> ignore
          Some xmlDoc
        with ex ->
          None  // TODO: Remove the empty map from cache to try again in the next request?
