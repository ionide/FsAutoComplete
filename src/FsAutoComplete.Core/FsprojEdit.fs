namespace FsAutoComplete

open System.IO

module FsProjEditor =

  let moveFileUp (fsprojPath: string) (file: string) =
    let xdoc = System.Xml.XmlDocument()
    xdoc.Load fsprojPath
    let xpath = sprintf "//Compile[@Include='%s']/.." file
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
    let xpath = sprintf "//Compile[@Include='%s']/.." file
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

  let private createNewCompileNode (doc: System.Xml.XmlDocument) (includePath: string) =
    let node = doc.CreateElement("Compile")
    node.SetAttribute("Include", includePath)
    node

  let addFileAbove (fsprojPath: string) (aboveFile: string) (newFileName: string) =
    let xdoc = System.Xml.XmlDocument()
    xdoc.Load fsprojPath
    let xpath = sprintf "//Compile[@Include='%s']/.." aboveFile
    let itemGroup = xdoc.SelectSingleNode(xpath)
    let childXPath = sprintf "//Compile[@Include='%s']" aboveFile
    let aboveNode = itemGroup.SelectSingleNode(childXPath)
    let node = createNewCompileNode xdoc newFileName
    itemGroup.InsertBefore(node, aboveNode) |> ignore
    xdoc.Save fsprojPath

  let addFileBelow (fsprojPath: string) (belowFile: string) (newFileName: string) =
    let xdoc = System.Xml.XmlDocument()
    xdoc.Load fsprojPath
    let xpath = sprintf "//Compile[@Include='%s']/.." belowFile
    let itemGroup = xdoc.SelectSingleNode(xpath)
    let childXPath = sprintf "//Compile[@Include='%s']" belowFile
    let aboveNode = itemGroup.SelectSingleNode(childXPath)
    let node = createNewCompileNode xdoc newFileName
    itemGroup.InsertAfter(node, aboveNode) |> ignore
    xdoc.Save fsprojPath

  let addFile (fsprojPath: string) (newFileName: string) =
    let xdoc = System.Xml.XmlDocument()
    xdoc.Load fsprojPath
    let newNode = createNewCompileNode xdoc newFileName

    let compileItemGroups =
      xdoc.SelectNodes("//Compile/.. | //None/.. | //EmbeddedResource/.. | //Content/..")

    let hasExistingCompileElement = compileItemGroups.Count > 0

    if hasExistingCompileElement then
      let firstCompileItemGroup =
        compileItemGroups |> Seq.cast<System.Xml.XmlNode> |> Seq.head

      let x = firstCompileItemGroup.FirstChild

      firstCompileItemGroup.InsertBefore(newNode, x) |> ignore
    else
      let itemGroup = xdoc.CreateElement("ItemGroup")
      itemGroup.AppendChild(newNode) |> ignore
      let projectNode = xdoc.SelectSingleNode("//Project")
      projectNode.AppendChild(itemGroup) |> ignore

    xdoc.Save fsprojPath

  let addExistingFile (fsprojPath: string) (existingFile: string) =
    let relativePath =
      Path.GetRelativePath(Path.GetDirectoryName fsprojPath, existingFile)

    addFile fsprojPath relativePath

  let removeFile (fsprojPath: string) (fileToRemove: string) =
    let xdoc = System.Xml.XmlDocument()
    xdoc.Load fsprojPath
    let sanitazedFileToRemove = fileToRemove.Replace("\\", "/")

    let nodeToRemoveOpt =
      // Take all the <Compile Include="..." /> nodes
      xdoc.SelectNodes("//Compile[@Include]")
      |> Seq.cast<System.Xml.XmlNode>
      // Find the node that match the file we want to remove
      // Note: We sanitaze the file name path because it can changes depending on the OS
      // and also on if the user used / or \ as the separator
      |> Seq.tryFind (fun node ->
        let sanitazedInclude = node.Attributes.["Include"].InnerText.Replace("\\", "/")
        sanitazedInclude = sanitazedFileToRemove)

    match nodeToRemoveOpt with
    | Some nodeToRemove ->
      nodeToRemove.ParentNode.RemoveChild(nodeToRemove) |> ignore
      xdoc.Save fsprojPath

    | None ->
      // Node not found, do nothing
      ()
