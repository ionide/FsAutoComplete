// --------------------------------------------------------------------------------------
// (c) Tomas Petricek, http://tomasp.net/blog
// --------------------------------------------------------------------------------------
module FsAutoComplete.TipFormatter

open System
open System.IO
open System.Xml
open System.Text.RegularExpressions
open FSharp.Compiler.SourceCodeServices

let inline nl<'T> = Environment.NewLine

module private Section =

    let inline addSection (name : string) (content : string) =
        if name <> "" then
            nl + nl
            + "**" + name + "**"
            + nl + nl + content
        else
            nl + nl + content

    let fromMap (name : string) (content : Map<string, string>) =
        if content.Count = 0 then
            ""
        else
            content
            |> Seq.map (fun kv ->
                let text =
                    if kv.Value.Contains("\n") then
                        kv.Value.Split('\n')
                        |> Seq.map (fun line ->
                            "> " + line.TrimStart()
                        )
                        |> String.concat "\n"
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
                        if innerText.Contains("\n") then

                            if innerText.StartsWith("\n") then

                                sprintf "```%s%s\n```" lang innerText

                            else
                                sprintf "```%s\n%s\n```" lang innerText

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
                let lastParagraphStartIndex = orText.LastIndexOf("\n")

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

                    sprintf "%s\n>    %s" startText endText
            )
            // Force a new `-or-` paragraph between each orSection
            // In markdown a new paragraph is define by using 2 empty lines
            |> String.concat "\n\n*or*\n\n"

    /// <summary>Remove all invalid 'or' block found</summary>
    /// <remarks>
    /// If an 'or' block is found between 2 elements then we remove it as we can't generate a valid markdown for it
    ///
    /// For example, <td> Some text -or- another text </td> cannot be converted into a multine string
    /// and so we prefer to remove the 'or' block instead of having some weird markdown artefacts
    ///
    /// For now, we only consider text between <td></td> to be invalid
    /// We can add more in the future if needed, but I want to keep this as minimal as possible to avoid capturing false positive
    /// /<remarks>
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
                            .Replace("\n", "")
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
        |> convertTable
        |> fixPortableClassLibrary
        |> handleMicrosoftOrList

// TODO: Improve this parser. Is there any other XmlDoc parser available?
type private XmlDocMember(doc: XmlDocument, indentationSize : int, columnOffset : int) =
    /// References used to detect if we should remove meaningless spaces
    let tabsOffset = String.replicate (columnOffset + indentationSize) " "
    let readContentForTooltip (node: XmlNode) =
        match node with
        | null -> null
        | _ ->
            let content =
                // Normale the EOL
                // This make it easier to work with line splittig
                node.InnerXml.Replace("\r\n", "\n")
                |> Format.applyAll

            content.Split('\n')
            |> Array.map(fun line ->
                if not (String.IsNullOrWhiteSpace line) && line.StartsWith(tabsOffset) then
                    line.Substring(columnOffset + indentationSize)
                else
                    line
            )
            |> String.concat "\n"

    let readChildren name (doc: XmlDocument) =
        doc.DocumentElement.GetElementsByTagName name
        |> Seq.cast<XmlNode>
        |> Seq.map (fun node -> Format.extractMemberText node.Attributes.[0].InnerText, node)
        |> Map.ofSeq

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

    let summary = readContentForTooltip rawSummary
    let parameters = rawParameters |> Map.map (fun _ n -> readContentForTooltip n)
    let remarks = rawRemarks |> Seq.map readContentForTooltip
    let exceptions = rawExceptions |> Map.map (fun _ n -> readContentForTooltip n)
    let typeParams = rawTypeParams |> Map.map (fun _ n -> readContentForTooltip n)
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
        (if exceptions.Count = 0 then ""
         else nl + nl + "Exceptions:" + nl +
                (exceptions |> Seq.map (fun kv -> "\t" + "`" + kv.Key + "`" + ": " + kv.Value) |> String.concat nl))

    member __.ToEnhancedString() =
        "**Description**" + nl + nl
        + summary
        + Section.fromList "" remarks
        + Section.fromMap "Type parameters" typeParams
        + Section.fromMap "Parameters" parameters
        + Section.fromOption "Returns" returns
        + Section.fromMap "Exceptions" exceptions
        + Section.fromList "Examples" examples
        + Section.fromList "See also" seeAlso

    member __.ToDocumentationString() =
        "**Description**" + nl + nl
        + summary
        + Section.fromList "" remarks
        + Section.fromMap "Type parameters" typeParams
        + Section.fromMap "Parameters" parameters
        + Section.fromOption "Returns" returns
        + Section.fromMap "Exceptions" exceptions
        + Section.fromList "Examples" examples
        + Section.fromList "See also" seeAlso

let rec private readXmlDoc (reader: XmlReader) (indentationSize : int) (acc: Map<string,XmlDocMember>) =
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

let private getXmlDoc dllFile =
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

// --------------------------------------------------------------------------------------
// Formatting of tool-tip information displayed in F# IntelliSense
// --------------------------------------------------------------------------------------
let private buildFormatComment cmt (isEnhanced : bool) (typeDoc: string option) =
    match cmt with
    | FSharpXmlDoc.Text s ->
        try
            // We create a "fake" XML document in order to use the same parser for both libraries and user code
            let xml = sprintf "<fake>%s</fake>" s
            let doc = XmlDocument()
            doc.LoadXml(xml)

            // This try to mimic how we found the indentation size when working a real XML file
            let rec findIndentationSize (lines : string list) =
                match lines with
                | head::tail ->
                    let lesserThanIndex = head.IndexOf("<")
                    if lesserThanIndex <> - 1 then
                        lesserThanIndex
                    else
                        findIndentationSize tail
                | [] -> 0

            let indentationSize =
                s.Replace("\r\n", "\n").Split('\n')
                |> Array.toList
                |> findIndentationSize

            let xmlDoc = XmlDocMember(doc, indentationSize, 0)
            xmlDoc.ToEnhancedString()

        with
            | ex ->
                Debug.print "%A" ex
                sprintf "An error occured when parsing the doc comment, please check that your doc comment is valid.\n\nMore info can be found LSP output"

    | FSharpXmlDoc.XmlDocFileSignature(dllFile, memberName) ->
        match getXmlDoc dllFile with
        | Some doc when doc.ContainsKey memberName ->
            let typeDoc =
                match typeDoc with
                | Some s when doc.ContainsKey s ->
                    if isEnhanced then doc.[s].ToEnhancedString() else string doc.[s]
                | _ -> ""
            let typeDoc = typeDoc.Replace("**Description**", "**Type Description**")
            if isEnhanced then
                doc.[memberName].ToEnhancedString() + (if typeDoc <> "" then "\n\n" + typeDoc else "")
            else
                string doc.[memberName] + (if typeDoc <> "" then "\n\n" + typeDoc else "")
        | _ -> ""
    | _ -> ""

let private buildFormatDocumentation cmt =
    match cmt with
    | FSharpXmlDoc.Text s -> s
    | FSharpXmlDoc.XmlDocFileSignature(dllFile, memberName) ->
       match getXmlDoc dllFile with
       | Some doc when doc.ContainsKey memberName ->
            doc.[memberName].ToDocumentationString()
       | _ -> ""
    | _ -> ""


let private formatGenericParamInfo cmt =
  let m = Regex.Match(cmt, """(.*) is (.*)""")
  if m.Success then
    sprintf "* `%s` is `%s`" m.Groups.[1].Value m.Groups.[2].Value
  else
    cmt


let formatTip (FSharpToolTipText tips) : (string * string) list list =
    tips
    |> List.choose (function
        | FSharpToolTipElement.Group items ->
            let getRemarks (it : FSharpToolTipElementData<string>) = defaultArg (it.Remarks |> Option.map (fun n -> if String.IsNullOrWhiteSpace n then n else "\n\n" + n)) ""
            Some (items |> List.map (fun (it) ->  (it.MainDescription + getRemarks it, buildFormatComment it.XmlDoc false None)))
        | FSharpToolTipElement.CompositionError (error) -> Some [("<Note>", error)]
        | _ -> None)

let formatTipEnhanced (FSharpToolTipText tips) (signature : string) (footer : string) (typeDoc: string option) : (string * string * string) list list =
    tips
    |> List.choose (function
        | FSharpToolTipElement.Group items ->
            Some (items |> List.map (fun i ->
                let comment =
                    if i.TypeMapping.IsEmpty then
                      buildFormatComment i.XmlDoc true typeDoc
                    else
                      buildFormatComment i.XmlDoc true typeDoc
                      + "\n\n**Generic parameters**\n\n"
                      + (i.TypeMapping |> List.map formatGenericParamInfo |> String.concat "\n")
                Debug.print "%s" comment
                (signature, comment, footer)))
        | FSharpToolTipElement.CompositionError (error) -> Some [("<Note>", error, "")]
        | _ -> None)

let formatDocumentation (FSharpToolTipText tips) ((signature, (constructors, fields, functions, interfaces, attrs, ts)) : string * (string [] * string [] * string []* string[]* string[]* string[])) (footer : string) (cn: string) =
    tips
    |> List.choose (function
        | FSharpToolTipElement.Group items ->
            Some (items |> List.map (fun i ->
                let comment =
                    if i.TypeMapping.IsEmpty then
                      buildFormatComment i.XmlDoc true None
                    else
                      buildFormatComment i.XmlDoc true None
                      + "\n\n**Generic parameters**\n\n"
                      + (i.TypeMapping |> List.map formatGenericParamInfo |> String.concat "\n")

                (signature, constructors, fields, functions, interfaces, attrs, ts, comment, footer, cn)))
        | FSharpToolTipElement.CompositionError (error) -> Some [("<Note>", [||],[||], [||], [||], [||], [||], error, "", "")]
        | _ -> None)

let formatDocumentationFromXmlSig (xmlSig: string) (assembly: string) ((signature, (constructors, fields, functions, interfaces, attrs, ts)) : string * (string [] * string [] * string [] * string[]* string[]* string[])) (footer : string) (cn: string) =
    let xmlDoc =  FSharpXmlDoc.XmlDocFileSignature(assembly, xmlSig)
    let comment = buildFormatComment xmlDoc true None
    [[(signature, constructors, fields, functions, interfaces, attrs, ts, comment, footer, cn)]]

let extractSignature (FSharpToolTipText tips) =
    let getSignature (str: string) =
        let nlpos = str.IndexOfAny([|'\r';'\n'|])
        let firstLine =
            if nlpos > 0 then str.[0..nlpos-1]
            else str

        if firstLine.StartsWith("type ", StringComparison.Ordinal) then
            let index = firstLine.LastIndexOf("=", StringComparison.Ordinal)
            if index > 0 then firstLine.[0..index-1]
            else firstLine
        else firstLine

    let firstResult x =
        match x with
        | FSharpToolTipElement.Group gs -> List.tryPick (fun (t : FSharpToolTipElementData<string>) -> if not (String.IsNullOrWhiteSpace t.MainDescription) then Some t.MainDescription else None) gs
        | _ -> None

    tips
    |> Seq.tryPick firstResult
    |> Option.map getSignature
    |> Option.getOrElse ""
