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
type private XmlDoc(doc: XmlDocument) =
  let nl = System.Environment.NewLine
  let readContent (node: XmlNode) =
    // Many definitions contains references like <paramref name="keyName" /> or <see cref="T:System.IO.IOException">
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

let private getXmlDoc dllFile =
  let exists extension =
    let path = Path.ChangeExtension(dllFile, extension)
    if File.Exists(path) then Some path else None
  match exists ".xml", exists ".XML" with
  | Some path, _
  | _, Some path -> Some path
  | None, None -> None

let rec private findXmlInfo name (reader: XmlReader) =
  match reader.Read() with
  | false -> ""
  | true when reader.GetAttribute("name") = name ->
    use subReader = reader.ReadSubtree()
    let doc = XmlDocument()
    doc.Load(subReader)
    XmlDoc doc |> string
  | _ -> findXmlInfo name reader
    

// --------------------------------------------------------------------------------------
// Formatting of tool-tip information displayed in F# IntelliSense
// --------------------------------------------------------------------------------------
let private buildFormatComment cmt =
  match cmt with
  | FSharpXmlDoc.Text s -> s
  | FSharpXmlDoc.XmlDocFileSignature(dllFile, memberName) ->
    match getXmlDoc dllFile with
    | Some xmlFile ->
      try
        use reader = XmlReader.Create xmlFile
        findXmlInfo memberName reader
      with _ -> ""
    | None -> ""
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
