﻿// --------------------------------------------------------------------------------------
// (c) Tomas Petricek, http://tomasp.net/blog
// --------------------------------------------------------------------------------------
module FsAutoComplete.TipFormatter

open System
open System.IO
open System.Xml
open System.Text.RegularExpressions
open Microsoft.FSharp.Compiler.SourceCodeServices

// TODO: Improve this parser. Is there any other XmlDoc parser available?
type private XmlDocMember(doc: XmlDocument) =
    let nl = Environment.NewLine
    let readContent (node: XmlNode) =
        match node with
        | null -> null
        | _ ->
            // Many definitions contain references like <paramref name="keyName" /> or <see cref="T:System.IO.IOException">
            // Replace them by the attribute content (keyName and System.IO.Exception in the samples above)
            // Put content in single quotes for possible formatting improvements on editor side.
            let c = Regex.Replace(node.InnerXml,"""<code.*?>(.*?)<\/code>""", "`$1`")
            let c = Regex.Replace(c,"""<\w+ \w+="(?:\w:){0,1}(.+?)">.*<\/\w+>""", "`$1`")
            let c = Regex.Replace(c,"""<\w+ \w+="(?:\w:){0,1}(.+?)" />""", "`$1`")
            let tableIndex = c.IndexOf("<table>")
            if tableIndex > 0 then
                let start = c.Substring(0, tableIndex)
                let table = c.Substring(tableIndex)
                let rows = Regex.Matches(c, "<th>").Count
                let table =
                    table.Replace(nl, "")
                        .Replace("\n", "")
                        .Replace("<table>", "")
                        .Replace("</table>", "")
                        .Replace("<thead>", "")
                        .Replace("</thead>", (String.replicate rows "| --- "))
                        .Replace("<tbody>", nl)
                        .Replace("</tbody>", "")
                        .Replace("<tr>", "")
                        .Replace("</tr>", "|" + nl)
                        .Replace("<th>", "|")
                        .Replace("</th>", "")
                        .Replace("<td>", "|")
                        .Replace("</td>", "")
                start + nl + nl + nl + nl + table
            else
                c

    let readChildren name (doc: XmlDocument) =
        doc.DocumentElement.GetElementsByTagName name
        |> Seq.cast<XmlNode>
        |> Seq.map (fun node -> node.Attributes.[0].InnerText.Replace("T:",""), readContent node)
        |> Map.ofSeq



    let summary = readContent doc.DocumentElement.ChildNodes.[0]
    let pars = readChildren "param" doc
    let exceptions = readChildren "exception" doc

    let returns =
        doc.DocumentElement.GetElementsByTagName "returns"
        |> Seq.cast<XmlNode>
        |> Seq.tryHead
        |> Option.map (fun node -> readContent node)

    override x.ToString() =
        summary + nl + nl +
        (pars |> Seq.map (fun kv -> "`" + kv.Key + "`" + ": " + kv.Value) |> String.concat nl) +
        (if exceptions.Count = 0 then ""
         else nl + nl + "Exceptions:" + nl +
                (exceptions |> Seq.map (fun kv -> "\t" + "`" + kv.Key + "`" + ": " + kv.Value) |> String.concat nl))

    member __.ToEnhancedString() =
        summary + nl + nl +
        (if pars.Count = 0 then ""
         else "**Parameters**" + nl +
                (pars |> Seq.map (fun kv -> "* `" + kv.Key + "`" + ": " + kv.Value) |> String.concat nl)) +
        (if exceptions.Count = 0 then ""
         else nl + nl + "**Exceptions**" + nl +
                (exceptions |> Seq.map (fun kv -> "* `" + kv.Key + "`" + ": " + kv.Value) |> String.concat nl)) +
        (if returns.IsNone then "" else nl + nl + "**Returns**" + nl + nl + returns.Value )


let rec private readXmlDoc (reader: XmlReader) (acc: Map<string,XmlDocMember>) =
  let acc' =
    match reader.Read() with
    | false -> None
    | true when reader.Name = "member" && reader.NodeType = XmlNodeType.Element ->
      try
        let key = reader.GetAttribute("name")
        use subReader = reader.ReadSubtree()
        let doc = XmlDocument()
        doc.Load(subReader)
        acc |> Map.add key (XmlDocMember doc) |> Some
      with
      | ex ->
        printfn "%s" ex.Message
        printfn "%s" ex.StackTrace
        Some acc
    | _ -> Some acc
  match acc' with
  | None -> acc
  | Some acc' -> readXmlDoc reader acc'

let private getXmlDoc =
  let xmlDocCache = Collections.Concurrent.ConcurrentDictionary<string, Map<string, XmlDocMember>>()
  fun dllFile ->
    let xmlFile = Path.ChangeExtension(dllFile, ".xml")
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
          let cnt =
            if actualXmlFile.Contains "netstandard.xml" then
                let cnt = Regex.Replace(cnt,"""(<p .*?>)+(.*)(<\/?p>)*""", "$2")
                cnt.Replace("<p>", "").Replace("</p>", "").Replace("<br>", "")
            else
                cnt
          use stringReader = new StringReader(cnt)
          use reader = XmlReader.Create stringReader
          let xmlDoc = readXmlDoc reader Map.empty
          xmlDocCache.AddOrUpdate(xmlFile, xmlDoc, fun _ _ -> xmlDoc) |> ignore
          Some xmlDoc
        with ex ->
          printfn "%s" ex.Message
          printfn "%s" ex.StackTrace
          None  // TODO: Remove the empty map from cache to try again in the next request?

// --------------------------------------------------------------------------------------
// Formatting of tool-tip information displayed in F# IntelliSense
// --------------------------------------------------------------------------------------
let private buildFormatComment cmt (isEnhanced : bool) =
    match cmt with
    | FSharpXmlDoc.Text s -> s
    | FSharpXmlDoc.XmlDocFileSignature(dllFile, memberName) ->
       match getXmlDoc dllFile with
       | Some doc when doc.ContainsKey memberName ->
            if isEnhanced then
                doc.[memberName].ToEnhancedString()
            else
                string doc.[memberName]
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
            Some (items |> List.map (fun (it) ->  (it.MainDescription + getRemarks it, buildFormatComment it.XmlDoc false)))
        | FSharpToolTipElement.CompositionError (error) -> Some [("<Note>", error)]
        | _ -> None)

let formatTipEnhanced (FSharpToolTipText tips) (signature : string) (footer : string) : (string * string * string) list list =
    tips
    |> List.choose (function
        | FSharpToolTipElement.Group items ->
            Some (items |> List.map (fun i ->
                let comment =
                    if i.TypeMapping.IsEmpty then
                      buildFormatComment i.XmlDoc true
                    else
                      buildFormatComment i.XmlDoc true
                      + "\n\n**Generic parameters**\n\n"
                      + (i.TypeMapping |> List.map formatGenericParamInfo |> String.concat "\n")

                (signature, comment, footer)))
        | FSharpToolTipElement.CompositionError (error) -> Some [("<Note>", error, "")]
        | _ -> None)

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
