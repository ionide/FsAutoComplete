namespace FsAutoComplete

open System
open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices
open Newtonsoft.Json

type ProjectCrackerCache = {
    Options : FSharpProjectOptions
    Files : string list
    OutFile : string option
    References : string list
    Log : Map<string, string>
}

type private ProjectPersistentCacheMessage =
    | Save of lastWriteTime : DateTime * response : ProjectCrackerCache option
    | Load of lastWriteTime : DateTime * channel : AsyncReplyChannel<ProjectCrackerCache option>

type ProjectPersistentCache (projectFile) =
    let cachePath = (Path.GetDirectoryName projectFile) </> "obj" </> "fsac.cache"

    let agent = MailboxProcessor.Start <| fun mb ->
        let rec loop () = async {
            let! msg = mb.Receive()
            match msg with
            | Save (lwt, resp) ->
                let r = resp |> Option.map JsonConvert.SerializeObject
                let resp' = defaultArg r ""
                let ctn = [| lwt.ToString(); resp' |]
                File.WriteAllLines(cachePath, ctn)
                return! loop()
            | Load (lwt, channel) ->
                let resp =
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
    let projectAsset = (Path.GetDirectoryName projectFile) </> "obj" </> "project.assets.json"

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
        let lwt =
            if File.Exists projectAsset then
                let a = File.GetLastWriteTimeUtc projectAsset
                let p = File.GetLastWriteTimeUtc projectFile
                if a > p then a else p
            else
                File.GetLastWriteTimeUtc projectFile
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
        if projectAsset |> Path.GetDirectoryName |> Directory.Exists |> not then
            projectAsset |> Path.GetDirectoryName |> Directory.CreateDirectory |> ignore
    ///File System Watcher for `obj` dir, at the moment only `project.assets.json`
    let afsw =
        new FileSystemWatcher(
            Path = Path.GetDirectoryName projectAsset,
            Filter = Path.GetFileName projectAsset,
            NotifyFilter = NotifyFilters.LastWrite)

    do afsw.Changed.Add (fun _ -> agent.Post (Changed (File.GetLastWriteTimeUtc projectAsset)))
    do afsw.EnableRaisingEvents <- true

    member __.Response with get() = agent.PostAndReply GetResponse
                        and set r = agent.Post (SetResponse r)

    interface IDisposable with
        member __.Dispose() =
            afsw.Dispose()
            fsw.Dispose()
