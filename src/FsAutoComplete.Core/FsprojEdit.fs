namespace FsAutoComplete

open System

module FsProjEditor =

  let moveFileUp (fsprojPath: string) (file: string) =
    let xdoc = System.Xml.XmlDocument()
    xdoc.Load fsprojPath

    let xpath =
      sprintf "//Compile[@Include='%s']/.." file

    let itemGroup = xdoc.SelectSingleNode(xpath)
    let childXPath = sprintf "//Compile[@Include='%s']" file
    let node = itemGroup.SelectSingleNode(childXPath)
    let upNode = node.PreviousSibling

    if isNull upNode then
      ()
    else
      itemGroup.RemoveChild node |> ignore
      itemGroup.InsertBefore(node, upNode) |> ignore
      xdoc.Save fsprojPath

  let moveFileDown (fsprojPath: string) (file: string) =
    let xdoc = System.Xml.XmlDocument()
    xdoc.Load fsprojPath

    let xpath =
      sprintf "//Compile[@Include='%s']/.." file

    let itemGroup = xdoc.SelectSingleNode(xpath)
    let childXPath = sprintf "//Compile[@Include='%s']" file
    let node = itemGroup.SelectSingleNode(childXPath)
    let downNode = node.NextSibling

    if isNull downNode then
      ()
    else
      itemGroup.RemoveChild node |> ignore
      itemGroup.InsertAfter(node, downNode) |> ignore
      xdoc.Save fsprojPath

  let addFileAbove (fsprojPath: string) (aboveFile: string) (newFileName: string) =
    let xdoc = System.Xml.XmlDocument()
    xdoc.Load fsprojPath

    let xpath =
      sprintf "//Compile[@Include='%s']/.." aboveFile

    let itemGroup = xdoc.SelectSingleNode(xpath)

    let childXPath =
      sprintf "//Compile[@Include='%s']" aboveFile

    let aboveNode = itemGroup.SelectSingleNode(childXPath)
    let node = aboveNode.Clone()
    let attr = node.Attributes.GetNamedItem "Include"
    attr.Value <- newFileName
    itemGroup.InsertBefore(node, aboveNode) |> ignore
    xdoc.Save fsprojPath

  let addFileBelow (fsprojPath: string) (belowFile: string) (newFileName: string) =
    let xdoc = System.Xml.XmlDocument()
    xdoc.Load fsprojPath

    let xpath =
      sprintf "//Compile[@Include='%s']/.." belowFile

    let itemGroup = xdoc.SelectSingleNode(xpath)

    let childXPath =
      sprintf "//Compile[@Include='%s']" belowFile

    let aboveNode = itemGroup.SelectSingleNode(childXPath)
    let node = aboveNode.Clone()
    let attr = node.Attributes.GetNamedItem "Include"
    attr.Value <- newFileName
    itemGroup.InsertAfter(node, aboveNode) |> ignore
    xdoc.Save fsprojPath

  let addFile (fsprojPath: string) (newFileName: string) =
    let xdoc = System.Xml.XmlDocument()
    xdoc.Load fsprojPath
    let itemGroup = xdoc.SelectSingleNode("//Compile/..")
    let x = itemGroup.FirstChild
    let node = x.Clone()
    let attr = node.Attributes.GetNamedItem "Include"
    attr.Value <- newFileName
    itemGroup.InsertBefore(node, x) |> ignore
    xdoc.Save fsprojPath
