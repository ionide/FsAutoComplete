module FsAutoComplete.Sourcelink

open System.IO
open System.Reflection.Metadata
open System.Reflection.PortableExecutable
open System.Text.RegularExpressions
open Newtonsoft.Json
open FSharp.Data
open FsAutoComplete.Logging

let logger = LogProvider.getLoggerByName "Sourcelink"

let private sourceLinkGuid = System.Guid "CC110556-A091-4D38-9FEC-25AB9A351A6A"
let private embeddedSourceGuid = System.Guid "0E8A571B-6926-466E-B4AD-8AB04611F5FE"

let private toHex (bytes: byte[]) =
    System.BitConverter.ToString(bytes).Replace("-", "").ToLowerInvariant()

type SourceLinkJson =
 { documents: System.Collections.Generic.Dictionary<string,string> }

type private Document =
    { Name: string
      Hash: byte[]
      Language: System.Guid
      IsEmbedded: bool }

let private pdbForDll (dllPath: string) =
    Path.ChangeExtension(dllPath, ".pdb")

let private tryGetSourcesForPdb (pdbPath: string) =
    logger.info (Log.setMessage "Reading metadata information for PDB {pdbPath}" >> Log.addContextDestructured "pdbPath" pdbPath)
    match File.Exists pdbPath with
    | true ->
        let pdbData = File.OpenRead pdbPath
        MetadataReaderProvider.FromPortablePdbStream pdbData |> Some
    | false ->
        None

let private tryGetSourcesForDll (dllPath: string) =
    logger.info (Log.setMessage "Reading metadata information for DLL {dllPath}" >> Log.addContextDestructured "dllPath" dllPath)
    let file = File.OpenRead dllPath
    let embeddedReader = new PEReader(file)
    try
        if embeddedReader.HasMetadata
        then
            embeddedReader.ReadDebugDirectory()
            |> Seq.tryFind (fun e -> e.Type = DebugDirectoryEntryType.EmbeddedPortablePdb && e <> Unchecked.defaultof<DebugDirectoryEntry>)
            |> Option.map embeddedReader.ReadEmbeddedPortablePdbDebugDirectoryData
        else
            tryGetSourcesForPdb (pdbForDll dllPath)
    with
    | e ->
        logger.error (Log.setMessage "Reading metadata information for DLL {dllPath} failed" >> Log.addContextDestructured "dllPath" dllPath >> Log.addExn e)
        tryGetSourcesForPdb (pdbForDll dllPath)

let private tryGetSourcelinkJson (reader: MetadataReader) =
    let handle: EntityHandle = ModuleDefinitionHandle.op_Implicit EntityHandle.ModuleDefinition
    let definitions = reader.GetCustomDebugInformation(handle)
    definitions
    |> Seq.tryPick (fun header ->
        let info = reader.GetCustomDebugInformation(header)
        if reader.GetGuid(info.Kind) = sourceLinkGuid
        then Some (reader.GetBlobBytes info.Value)
        else None
    )
    |> Option.map (fun bytes ->
        let byteString = System.Text.Encoding.UTF8.GetString(bytes)
        logger.info (Log.setMessage "Read sourcelink json in as {json}" >> Log.addContextDestructured "json" byteString)
        let blob = JsonConvert.DeserializeObject<SourceLinkJson>(byteString)
        logger.info (Log.setMessage "Read sourcelink structure {blob}" >> Log.addContextDestructured "blog" blob)
        blob
    )

let private isEmbedded (reader: MetadataReader) (handle: DocumentHandle) =
    let entityHandle: EntityHandle = DocumentHandle.op_Implicit handle
    let headers = reader.GetCustomDebugInformation(entityHandle)
    headers
    |> Seq.exists (fun header ->
        let info = reader.GetCustomDebugInformation(header)
        reader.GetGuid(info.Kind) = embeddedSourceGuid
    )

let private documentsFromReader (reader: MetadataReader) =
    seq {
        for docHandle in reader.Documents do
            if not docHandle.IsNil
            then
                let doc = reader.GetDocument docHandle
                if not (doc.Name.IsNil || doc.Language.IsNil)
                then
                    yield { Name = reader.GetString(doc.Name)
                            Hash = reader.GetBlobBytes(doc.Hash)
                            Language = reader.GetGuid(doc.Language)
                            IsEmbedded = isEmbedded reader docHandle }
    }

let private tryGetUrlWithWildcard (pathPattern: string) (urlPattern: string) (document: Document) =
    let pattern = Regex.Escape(pathPattern).Replace(@"\*", "(.+)")
    let regex = Regex(pattern)
    match regex.Match(document.Name) with
    | m when not m.Success -> None
    | m ->
        let replacement = m.Groups.[1].Value.Replace(@"\", "/")
        (urlPattern.Replace("*", replacement), replacement, document)
        |> Some

let private tryGetUrlWithExactMatch (pathPattern: string) (urlPattern: string) (document: Document) =
    if pathPattern.Equals(document.Name, System.StringComparison.Ordinal) then Some (urlPattern, pathPattern, document) else None

let private tryGetUrlForDocument (json: SourceLinkJson) (document: Document) =
    logger.info (Log.setMessage "finding source for document {doc}" >> Log.addContextDestructured "doc" document)
    match json.documents with
    | null -> None
    | documents ->
        documents
        |> Seq.tryPick (fun (KeyValue(path, url)) ->
            if path.Contains("*")
            then
                tryGetUrlWithWildcard path url document
            else
                tryGetUrlWithExactMatch path url document
        )

let private downloadFileToTempDir (url: string) (repoPathFragment: string) (document: Document) =
    let tempFile = System.IO.Path.GetTempPath() </> toHex document.Hash </> repoPathFragment
    let tempDir = Path.GetDirectoryName tempFile
    Directory.CreateDirectory tempDir |> ignore

    async {
        use fileStream = File.OpenWrite tempFile
        logger.info (Log.setMessage "Getting file from {url} for document {repoPath}" >> Log.addContextDestructured "url" url >> Log.addContextDestructured "repoPath" repoPathFragment)
        let! response = Http.AsyncRequestStream(url, httpMethod = "GET")
        do! response.ResponseStream.CopyToAsync fileStream |> Async.AwaitTask
        return tempFile
    }

type Errors =
| NoInformation
| InvalidJson
| MissingPatterns

let tryFetchSourcelinkFile (dllPath: string) (targetFile: string) =  async {
    // FCS prepends the CWD to the root of the targetFile for some reason, so we strip it here
    logger.info (Log.setMessage "Reading from {dll} for source file {file}" >> Log.addContextDestructured "dll" dllPath >> Log.addContextDestructured "file" targetFile)
    let targetFile =
        if targetFile.StartsWith System.Environment.CurrentDirectory
        then targetFile.Replace(System.Environment.CurrentDirectory + "/", "")
        else targetFile
    logger.info (Log.setMessage "Target file is {file}" >> Log.addContextDestructured "file" targetFile)
    match tryGetSourcesForDll dllPath with
    | None -> return Error NoInformation
    | Some sourceReaderProvider ->
        use sourceReaderProvider = sourceReaderProvider
        let sourceReader = sourceReaderProvider.GetMetadataReader()
        match tryGetSourcelinkJson sourceReader with
        | None ->
            return Error InvalidJson
        | Some json ->
            let docs = documentsFromReader sourceReader
            logger.info (Log.setMessage "trying to find document {doc} in {sources}" >> Log.addContextDestructured "doc" targetFile >> Log.addContextDestructured "sources" docs)
            let doc =
                docs
                |> Seq.tryFind (fun d -> d.Name = targetFile)
                |> Option.bind (tryGetUrlForDocument json)
            match doc with
            | None ->
                return Error MissingPatterns
            | Some (url, fragment, document) ->
                let! tempFile = downloadFileToTempDir url fragment document
                return Ok tempFile
}
