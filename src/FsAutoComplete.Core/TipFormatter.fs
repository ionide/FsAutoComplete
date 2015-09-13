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
    (x.Params |> Seq.map (fun kv -> kv.Key + ": " + kv.Value) |> String.concat nl) + nl + nl +
    "Exceptions:" + nl +
    (x.Exceptions |> Seq.map (fun kv -> "\t" + kv.Key + ": " + kv.Value) |> String.concat nl)
    

// --------------------------------------------------------------------------------------
// Formatting of tool-tip information displayed in F# IntelliSense
// --------------------------------------------------------------------------------------
let private buildFormatComment cmt =
  match cmt with
  | FSharpXmlDoc.Text s -> s
  | FSharpXmlDoc.XmlDocFileSignature(dllFile, memberName) when
    File.Exists(Path.ChangeExtension(dllFile, ".xml")) ->
    let rec findComment name (reader: XmlReader) =
      match reader.Read() with
      | false -> ""
      | true when reader.GetAttribute("name") = name ->
        use subReader = reader.ReadSubtree()
        let doc = XmlDocument()
        doc.Load(subReader)
        XmlDoc doc |> string
      | _ -> findComment name reader
    try
      use reader = Path.ChangeExtension(dllFile, ".xml") |> XmlReader.Create
      findComment memberName reader
    with _ -> ""
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
