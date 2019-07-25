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
            addSection name (content |> Seq.map (fun kv -> "* `" + kv.Key + "`" + ": " + kv.Value) |> String.concat nl)

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
        sprintf """<%s\s*(?'attributes'[^>]+)?>(?'innerText'(?:(?!<%s>)(?!<\/%s>)[\s\S])*)<\/%s\s*>""" tagName tagName tagName tagName

    type TagInfo =
        {
            InnerText : string
            Attributes : Map<string, string>
        }

    type FormatterInfo =
        {
            TagName : string
            Formatter : TagInfo -> string
        }


    let private extractTextFromQuote (quotedText : string) =
        quotedText.Substring(1, quotedText.Length - 2)

    let private tryGetAttributes (attributes : Group) =
        if attributes.Success then
            let pattern = """(?'key'\S+)=(?'value''[^']*'|"[^"]*")"""
            Regex.Matches(attributes.Value, pattern, RegexOptions.IgnoreCase)
            |> Seq.cast<Match>
            |> Seq.map (fun group ->
                group.Groups.["key"].Value, extractTextFromQuote group.Groups.["value"].Value
            )
            |> Map.ofSeq
        else
            Map.empty

    let extractInfo (groups : GroupCollection) =
        {
            InnerText = groups.["innerText"].Value
            Attributes = tryGetAttributes groups.["attributes"]
        }

    let rec private applyFormatter (info : FormatterInfo) text =
        let pattern = tagPattern info.TagName
        match Regex.Match(text, pattern, RegexOptions.IgnoreCase) with
        | m when m.Success ->
            let replacement =
                m.Groups
                |> extractInfo
                |> info.Formatter

            text.Replace(m.Groups.[0].Value, replacement)
            |> applyFormatter info
        | _ ->
            text


    let private codeBlock =
        {
            TagName = "code"
            Formatter =
                fun info ->
                    let lang =
                        match Map.tryFind "lang" info.Attributes with
                        | Some lang ->
                            lang

                        | None ->
                            "forceNoHighlight"

                    if info.InnerText.Contains("\n")
                        || info.InnerText.Contains("\r\n") then

                        if info.InnerText.StartsWith("\n")
                            || info.InnerText.StartsWith("\r\n") then

                            sprintf "```%s%s\n```" lang info.InnerText

                        else
                            sprintf "```%s\n%s\n```" lang info.InnerText

                    else
                        sprintf "`%s`" info.InnerText

        }
        |> applyFormatter

    let private codeInline =
        {
            TagName = "c"
            Formatter  =
                fun info ->
                    "`" + info.InnerText + "`"
        }
        |> applyFormatter

    let private anchor =
        {
            TagName = "a"
            Formatter =
                fun info ->
                    let href =
                        match Map.tryFind "href" info.Attributes with
                        | Some href ->
                            href

                        | None ->
                            ""

                    sprintf "[%s](%s)" info.InnerText href
        }
        |> applyFormatter

    let private paragraph =
        {
            TagName = "para"
            Formatter =
                fun info ->
                    nl + info.InnerText + nl
        }
        |> applyFormatter

    let private see =
        {
            TagName = "see"
            Formatter =
                fun info ->
                    match Map.tryFind "cref" info.Attributes with
                    | Some cref ->
                        // TODO: Add config to generates command
                        "`" + cref + "`"
                    | None ->
                        "`" + info.InnerText + "`"
        }
        |> applyFormatter

    let private paramRef =
        {
            TagName = "paramref"
            Formatter =
                fun info ->
                    match Map.tryFind "name" info.Attributes with
                    | Some name ->
                        "`" + name + "`"
                    | None ->
                        "`" + info.InnerText + "`"
        }
        |> applyFormatter

    let private typeParamRef =
        {
            TagName = "typeparamref"
            Formatter =
                fun info ->
                    match Map.tryFind "name" info.Attributes with
                    | Some name ->
                        "`" + name + "`"
                    | None ->
                        "`" + info.InnerText + "`"
        }
        |> applyFormatter

    let private partialApply (text : string) =
        text
        |> paragraph
        |> codeInline
        |> codeBlock
        |> see
        |> paramRef
        |> typeParamRef
        |> anchor

    let private convertTable =
        {
            TagName = "table"
            Formatter =
                fun info ->
                    let tableContent = info.InnerText
                    let rowCount = Regex.Matches(tableContent, "<th\s?>").Count
                    let convertedTable =
                        tableContent
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
                        // Make a pass to convert XML tags inside the table
                        |> partialApply

                    nl + nl + convertedTable + nl

        }
        |> applyFormatter

    let applyAll (text : string) =
        text
        |> partialApply
        |> convertTable

// TODO: Improve this parser. Is there any other XmlDoc parser available?
type private XmlDocMember(doc: XmlDocument, indentationSize : int, columnOffset : int) =
    /// References used to detect if we should remove meaningless spaces
    let tabsOffset = String.replicate (columnOffset + indentationSize) " "
    let readContentForTooltip (node: XmlNode) =
        match node with
        | null -> null
        | _ ->
            let content = Format.applyAll node.InnerXml

            content.Replace("\r\n", "\n").Split('\n')
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
        |> Seq.map (fun node -> node.Attributes.[0].InnerText.Replace("T:",""), node)
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
            "* `" + node.Attributes.[0].InnerText.Replace("T:","") + "`"
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
          Debug.print "File content:\n%s" cnt
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
        s
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
