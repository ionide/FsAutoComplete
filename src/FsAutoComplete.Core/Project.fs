namespace FsAutoComplete

open System
open System.IO
open FSharp.Compiler.SourceCodeServices
open Newtonsoft.Json

type ProjectCrackerCache = {
    Options : FSharpProjectOptions
    Files : string list
    OutFile : string option
    References : string list
    Log : Map<string, string>
    ExtraInfo: Dotnet.ProjInfo.Workspace.ExtraProjectInfoData
}

type private ProjectPersistentCacheMessage =
    | Save of lastWriteTime : DateTime * response : ProjectCrackerCache option
    | Load of lastWriteTime : DateTime * channel : AsyncReplyChannel<ProjectCrackerCache option>

type ProjectPersistentCache (projectFile: string) =
    let cachePath = (Path.GetDirectoryName projectFile) </> "obj" </> "fsac.cache"
    let settings = JsonSerializerSettings()
    do settings.MissingMemberHandling <- MissingMemberHandling.Error

    let agent = MailboxProcessor.Start <| fun mb ->
        let rec loop () = async {
            let! msg = mb.Receive()
            match msg with
            | Save (lwt, resp) ->
                try
                    let r = resp |> Option.map JsonConvert.SerializeObject
                    let resp' = defaultArg r ""
                    let ctn = [| lwt.ToString(); resp' |]
                    File.WriteAllLines(cachePath, ctn)
                with _ex ->
                    //TODO add trace
                    ()
                return! loop()
            | Load (lwt, channel) ->
                let resp =
                    try
                        if File.Exists cachePath then
                            let ctn = File.ReadAllLines(cachePath)
                            if lwt.ToString() = ctn.[0] then
                                let r = ctn.[1]
                                try
                                    let x = JsonConvert.DeserializeObject<ProjectCrackerCache> r
                                    if isNull (box x) then
                                        File.Delete cachePath //Remove cahce that can't be deserialized
                                        None
                                    else
                                        Some x
                                with
                                | _ ->
                                    File.Delete cachePath
                                    None
                            else
                                None
                        else
                            None
                    with _ex ->
                        //TODO add trace
                        None

                channel.Reply resp
                return! loop()
        }
        loop ()

    member __.SaveCache (lwt, resp) = agent.Post(Save(lwt,resp))
    member __.LoadCache (lwt) = agent.PostAndReply( fun ch -> Load(lwt, ch))

type private ProjectMessage =
    | Changed of DateTime
    | GetResponse of AsyncReplyChannel<ProjectCrackerCache option>
    | SetResponse of ProjectCrackerCache option

type Project (projectFile, onChange: ProjectFilePath -> unit) =
    let persistentCache = ProjectPersistentCache(projectFile)

    let fullPath = Path.GetFullPath projectFile
    let objFolder = (Path.GetDirectoryName projectFile) </> "obj" |> Path.GetFullPath

    let projectAssetsFile = objFolder </> "project.assets.json"
    let projectProps = "*.props"

    let agent = MailboxProcessor.Start <| fun mb ->
        let rec loop (lastWriteTime, response) = async {
            let! msg = mb.Receive()
            match msg with
            | Changed lwt when lwt <> lastWriteTime ->
                onChange projectFile
                return! loop (lwt, None)
            | Changed _ -> return! loop (lastWriteTime, response)
            | GetResponse r ->
                r.Reply response
                return! loop (lastWriteTime, response)
            | SetResponse r ->
                persistentCache.SaveCache(lastWriteTime, r)

                return! loop (lastWriteTime, r)
        }
        let projectTime =
            if File.Exists projectFile then
                File.GetLastWriteTimeUtc projectFile
            else
                DateTime.MinValue
            
        let projectAssetsTime = 
            if File.Exists projectAssetsFile then
                File.GetLastWriteTimeUtc projectAssetsFile
            else
                DateTime.MinValue

        let projectPropsTime =
            if Directory.Exists objFolder then
                let propsFiles = Directory.EnumerateFiles(objFolder,projectProps) |> Seq.toList
                match propsFiles with
                | [] -> DateTime.MinValue
                | _ -> propsFiles |> Seq.map File.GetLastWriteTimeUtc |> Seq.max
            else
                DateTime.MinValue


        let lwt = max (max projectTime projectAssetsTime) projectPropsTime

        let state = persistentCache.LoadCache lwt
        loop (lwt, state)

    ///File System Watcher for `fsproj` file
    let fsw =
        new FileSystemWatcher(
            Path = Path.GetDirectoryName fullPath,
            Filter = Path.GetFileName fullPath,
            NotifyFilter = NotifyFilters.LastWrite)

    do fsw.Changed.Add (fun _ -> agent.Post (Changed (File.GetLastWriteTimeUtc projectFile)))
    do fsw.EnableRaisingEvents <- true

    do
        if projectAssetsFile |> Path.GetDirectoryName |> Directory.Exists |> not then
            projectAssetsFile |> Path.GetDirectoryName |> Directory.CreateDirectory |> ignore

    ///File System Watcher for `obj` dir, at the moment only `project.assets.json` and `*.props`
    let afsw =
        new FileSystemWatcher(
            Path = Path.GetDirectoryName projectAssetsFile,
            Filter = Path.GetFileName projectAssetsFile)

    do afsw.Changed.Add (fun _ -> agent.Post (Changed (File.GetLastWriteTimeUtc projectAssetsFile)))
    do afsw.Created.Add (fun _ -> agent.Post (Changed (File.GetLastWriteTimeUtc projectAssetsFile)))
    do afsw.Deleted.Add (fun _ -> agent.Post (Changed (DateTime.UtcNow)))

    do afsw.EnableRaisingEvents <- true

    let propsfsw =
        new FileSystemWatcher(
            Path = objFolder,
            Filter = projectProps)

    do propsfsw.Changed.Add (fun x -> agent.Post (Changed (File.GetLastWriteTimeUtc x.FullPath)))
    do propsfsw.Created.Add (fun x -> agent.Post (Changed (File.GetLastWriteTimeUtc x.FullPath)))
    do propsfsw.Deleted.Add (fun _ -> agent.Post (Changed (DateTime.UtcNow)))

    do propsfsw.EnableRaisingEvents <- true    

    member __.Response with get() = agent.PostAndReply GetResponse
                        and set r = agent.Post (SetResponse r)

    member __.FileName = fullPath

    interface IDisposable with
        member __.Dispose() =
            propsfsw.Dispose()
            afsw.Dispose()
            fsw.Dispose()
