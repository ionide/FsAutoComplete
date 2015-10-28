// --------------------------------------------------------------------------------------
// (c) Tomas Petricek, http://tomasp.net/blog
// --------------------------------------------------------------------------------------
module FsAutoComplete.TipFormatter

open System.IO
open System.Xml
open System.Text
open System.Text.RegularExpressions
open Microsoft.FSharp.Compiler.SourceCodeServices

// TODO: Improve this parser. Is there any other XmlDoc parser available?
type private XmlDocMember(doc: XmlDocument) =
  let nl = System.Environment.NewLine
  let readContent (node: XmlNode) =
    // Many definitions contain references like <paramref name="keyName" /> or <see cref="T:System.IO.IOException">
    // Replace them by the attribute content (keyName and System.IO.Exception in the samples above)
    Regex.Replace(node.InnerXml,"""<\w+ \w+="(?:\w:){0,1}(.+?)" />""", "$1")
  let readChildren name (doc: XmlDocument) =
    doc.DocumentElement.GetElementsByTagName name
    |> Seq.cast<XmlNode>
    |> Seq.map (fun node -> node.Attributes.[0].InnerText.Replace("T:",""), readContent node)
    |> Map.ofSeq
  member val Summary = readContent doc.DocumentElement.ChildNodes.[0]
  member val Params = readChildren "param" doc
  member val Exceptions = readChildren "exception" doc
  override x.ToString() =
    x.Summary + nl + nl +
    (x.Params |> Seq.map (fun kv -> kv.Key + ": " + kv.Value) |> String.concat nl) +
    (if x.Exceptions.Count = 0 then ""
     else nl + nl + "Exceptions:" + nl +
          (x.Exceptions |> Seq.map (fun kv -> "\t" + kv.Key + ": " + kv.Value) |> String.concat nl))

let rec private readXmlDoc (reader: XmlReader) (acc: Map<string,XmlDocMember>) =
  let acc' =
    match reader.Read() with
    | false -> None
    | true when reader.Name = "member" ->
      use subReader = reader.ReadSubtree()
      let doc = XmlDocument()
      doc.Load(subReader)
      acc |> Map.add (reader.GetAttribute("name")) (XmlDocMember doc) |> Some
    | _ -> Some acc
  match acc' with
  | None -> acc
  | Some acc' -> readXmlDoc reader acc'

let private getXmlDoc =
  let xmlDocCache = System.Collections.Concurrent.ConcurrentDictionary<string, Map<string, XmlDocMember>>()
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
        // Prevent racing conditions
        xmlDocCache.AddOrUpdate(xmlFile, Map.empty, fun _ _ -> Map.empty) |> ignore
        try
          use reader = XmlReader.Create actualXmlFile
          let xmlDoc = readXmlDoc reader Map.empty
          xmlDocCache.AddOrUpdate(xmlFile, xmlDoc, fun _ _ -> xmlDoc) |> ignore
          Some xmlDoc
        with _ ->
          None  // TODO: Remove the empty map from cache to try again in the next request?

// --------------------------------------------------------------------------------------
// Formatting of tool-tip information displayed in F# IntelliSense
// --------------------------------------------------------------------------------------
let private buildFormatComment cmt =
  match cmt with
  | FSharpXmlDoc.Text s -> s
  | FSharpXmlDoc.XmlDocFileSignature(dllFile, memberName) ->
    match getXmlDoc dllFile with
    | Some doc when doc.ContainsKey memberName -> string doc.[memberName]
    | _ -> ""
  | _ -> ""

let formatTip tip = 
  match tip with
  | FSharpToolTipText tips -> tips |> Seq.where (function
                                                 | FSharpToolTipElement.Single _ | FSharpToolTipElement.Group _ -> true
                                                 | _ -> false)
                                   |>  Seq.fold (fun acc t -> match t with
                                                              | FSharpToolTipElement.Single (it, comment) -> [(it, comment |> buildFormatComment)]::acc
                                                              | FSharpToolTipElement.Group (items) -> (items |> List.map (fun (it, comment) ->  (it, comment |> buildFormatComment) )) :: acc
                                                              | _ -> acc) []
