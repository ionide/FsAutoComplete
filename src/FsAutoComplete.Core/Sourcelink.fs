module FsAutoComplete.Sourcelink

open System.IO
open System.Reflection.Metadata
open System.Reflection.PortableExecutable
open System.Text.RegularExpressions
open Newtonsoft.Json
open FsAutoComplete.Logging
open FSharp.UMX
open FsAutoComplete.Utils
open Ionide.ProjInfo.ProjectSystem

let logger = LogProvider.getLoggerByName "FsAutoComplete.Sourcelink"

let private sourceLinkGuid = System.Guid "CC110556-A091-4D38-9FEC-25AB9A351A6A"
let private embeddedSourceGuid = System.Guid "0E8A571B-6926-466E-B4AD-8AB04611F5FE"

let private httpClient = new System.Net.Http.HttpClient()

let private toHex (bytes: byte[]) =
    System.BitConverter.ToString(bytes).Replace("-", "").ToLowerInvariant()

/// left hand side of sourcelink document mapping, represents a static or partially-static repo root path
type [<Measure>] SourcelinkPattern

let normalizeRepoPath (repo: string<RepoPathSegment>): string<NormalizedRepoPathSegment> =
  let s = UMX.untag repo
  let s' = s.Replace(@"\", "/")
  UMX.tag<NormalizedRepoPathSegment> s'

/// Some sourcelink urls (even after normalization?) have a leading slash, which we don't want.
/// keeping the slash results in temporary file paths with two slashes being created, which
/// results in invalid paths being generated (.NET treats a // in the path as a re-root of the path).
/// The shortest solution is to strip the leading slash as part of normalization, which works on Windows,
/// but I'm unsure of the effect on Linux and MacOS.
let stripLeadingSlash (repo: string<NormalizedRepoPathSegment>) =
    (UMX.untag repo).TrimStart('/') |> UMX.tag<NormalizedRepoPathSegment>

type SourceLinkJson =
 { documents: System.Collections.Generic.Dictionary<string<SourcelinkPattern>, string<Url>> }

type private Document =
    { Name: string<RepoPathSegment>
      Hash: byte[]
      Language: System.Guid
      IsEmbedded: bool }

let private compareRepoPath (d: Document) targetFile =
  if Environment.isWindows  then
    let s = UMX.untag d.Name
    let s' = normalizePath s |> UMX.untag
    let s' = UMX.tag<NormalizedRepoPathSegment> s'
    s' = targetFile
  else
    let t = UMX.untag targetFile |> UMX.tag<RepoPathSegment>
    let t' = normalizeRepoPath t
    normalizeRepoPath d.Name = t'

let private pdbForDll (dllPath: string<LocalPath>) =
    UMX.tag<LocalPath> (Path.ChangeExtension(UMX.untag dllPath, ".pdb"))

let private tryGetSourcesForPdb (pdbPath: string<LocalPath>) =
    let pdbPath = UMX.untag pdbPath
    logger.info (Log.setMessage "Reading metadata information for PDB {pdbPath}" >> Log.addContextDestructured "pdbPath" pdbPath)
    match File.Exists pdbPath with
    | true ->
        let pdbData = File.OpenRead pdbPath
        MetadataReaderProvider.FromPortablePdbStream pdbData |> Some
    | false ->
        None

let private tryGetSourcesForDll (dllPath: string<LocalPath>) =
    logger.info (Log.setMessage "Reading metadata information for DLL {dllPath}" >> Log.addContextDestructured "dllPath" dllPath)
    let file = File.OpenRead (UMX.untag dllPath)
    let embeddedReader = new PEReader(file)
    let readFromPDB () = tryGetSourcesForPdb (pdbForDll dllPath)
    try
        if embeddedReader.HasMetadata
        then
            embeddedReader.ReadDebugDirectory()
            |> Seq.tryFind (fun e -> e.Type = DebugDirectoryEntryType.EmbeddedPortablePdb && e <> Unchecked.defaultof<DebugDirectoryEntry>)
            |> Option.map embeddedReader.ReadEmbeddedPortablePdbDebugDirectoryData
            |> Option.orElseWith readFromPDB
        else
            readFromPDB()
    with
    | e ->
        logger.error (Log.setMessage "Reading metadata information for DLL {dllPath} failed" >> Log.addContextDestructured "dllPath" dllPath >> Log.addExn e)
        readFromPDB()

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
        let blob = JsonConvert.DeserializeObject<SourceLinkJson>(byteString)
        logger.info (Log.setMessage "Read sourcelink structure {blob}" >> Log.addContextDestructured "blob" blob)
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
                    yield { Name = UMX.tag<RepoPathSegment> (reader.GetString(doc.Name))
                            Hash = reader.GetBlobBytes(doc.Hash)
                            Language = reader.GetGuid(doc.Language)
                            IsEmbedded = isEmbedded reader docHandle }
    }

let replace (url: string<Url>) (replacement: string<NormalizedRepoPathSegment>): string<Url> =
  UMX.tag<Url> ((UMX.untag url).Replace("*", UMX.untag replacement))

let private tryGetUrlWithWildcard (pathPattern: string<SourcelinkPattern>) (urlPattern: string<Url>) (document: Document) =
    let pattern = Regex.Escape(UMX.untag pathPattern).Replace(@"\*", "(.+)")
    // this regex matches the un-normalized repo paths, so we need to compare against the un-normalized paths here
    let regex = Regex(pattern)
    // patch up the slashes because the sourcelink json will have os-specific paths but we're working with normalized
    let replaced = document.Name
    match regex.Match(UMX.untag replaced) with
    | m when not m.Success ->
      logger.info (Log.setMessage "document {doc} did not match pattern {pattern}" >> Log.addContext "doc" document.Name >> Log.addContext "pattern" pattern)
      None
    | m ->
        let replacement = UMX.tag<RepoPathSegment> m.Groups.[1].Value |> normalizeRepoPath |> stripLeadingSlash
        logger.info (Log.setMessage "document {doc} did match pattern {pattern} with value {replacement}" >> Log.addContext "doc" document.Name >> Log.addContext "pattern" pattern >> Log.addContext "replacement" replacement)
        Some (replace urlPattern replacement, replacement, document)

let private tryGetUrlWithExactMatch (pathPattern: string<SourcelinkPattern>) (urlPattern: string<Url>) (document: Document) =
    if (UMX.untag pathPattern).Equals(UMX.untag document.Name, System.StringComparison.Ordinal)
    then Some (urlPattern, normalizeRepoPath (UMX.cast<SourcelinkPattern, RepoPathSegment> pathPattern), document) else None

let isWildcardPattern (p: string<SourcelinkPattern>) =
  (UMX.untag p).Contains("*")

let private tryGetUrlForDocument (json: SourceLinkJson) (document: Document) =
    logger.info (Log.setMessage "finding source for document {doc}" >> Log.addContextDestructured "doc" document.Name)
    match json.documents with
    | null -> None
    | documents ->
        documents
        |> Seq.tryPick (fun (KeyValue(path, url)) ->
            if isWildcardPattern path
            then
                tryGetUrlWithWildcard path url document
            else
                tryGetUrlWithExactMatch path url document
        )

let private downloadFileToTempDir (url: string<Url>) (repoPathFragment: string<NormalizedRepoPathSegment>) (document: Document): Async<string<LocalPath>> =
    let tempFile = Path.Combine(Path.GetTempPath(), toHex document.Hash, UMX.untag repoPathFragment)
    let tempDir = Path.GetDirectoryName tempFile
    Directory.CreateDirectory tempDir |> ignore

    async {
        logger.info (Log.setMessage "Getting file from {url} for document {repoPath}" >> Log.addContextDestructured "url" url >> Log.addContextDestructured "repoPath" repoPathFragment)
        let! response = httpClient.GetStreamAsync(UMX.untag url) |> Async.AwaitTask
        use fileStream = File.OpenWrite tempFile
        do! response.CopyToAsync fileStream |> Async.AwaitTask
        return UMX.tag<LocalPath> tempFile
    }

type Errors =
| NoInformation
| InvalidJson
| MissingPatterns
| MissingSourceFile

let tryFetchSourcelinkFile (dllPath: string<LocalPath>) (targetFile: string<NormalizedRepoPathSegment>) =  async {
    // FCS prepends the CWD to the root of the targetFile for some reason, so we strip it here
    logger.info (Log.setMessage "Reading from {dll} for source file {file}" >> Log.addContextDestructured "dll" dllPath >> Log.addContextDestructured "file" targetFile)
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
            let doc = docs |> Seq.tryFind (fun d -> compareRepoPath d targetFile)
            match doc with
            | None ->
                logger.warn (Log.setMessage "No sourcelinked source file matched {target}. Available documents were (normalized paths here): {docs}" >> Log.addContextDestructured "docs" (docs |> Seq.map (fun d -> normalizeRepoPath d.Name)) >> Log.addContextDestructured "target" targetFile)
                return Error MissingSourceFile
            | Some doc ->
                match tryGetUrlForDocument json doc with
                | Some (url, fragment, document) ->
                  let! tempFile = downloadFileToTempDir url fragment document
                  return Ok tempFile
                | None ->
                  logger.warn (Log.setMessage "Couldn't derive a url for the source file {target}. None of the following patterns matched: {patterns}"
                               >> Log.addContext "target" doc.Name
                               >> Log.addContext "patterns" (json.documents |> Seq.map (function (KeyValue(k, _)) -> k)))
                  return Error MissingPatterns
}
