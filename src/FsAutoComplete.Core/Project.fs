namespace FsAutoComplete

open System
open System.IO

type SerializedProjectResponse = string

type private ProjectMessage =
    | Changed of DateTime
    | GetResponse of AsyncReplyChannel<SerializedProjectResponse option>
    | SetResponse of SerializedProjectResponse option

type Project (projectFile, onChange: ProjectFilePath -> unit) =
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
                return! loop (lastWriteTime, r)
        }    
        loop (File.GetLastWriteTimeUtc projectFile, None)

    let fullPath = Path.GetFullPath projectFile

    let fsw = 
        new FileSystemWatcher(
            Path = Path.GetDirectoryName fullPath, 
            Filter = Path.GetFileName fullPath,
            NotifyFilter = NotifyFilters.LastWrite)

    do fsw.Changed.Add (fun _ -> agent.Post (Changed (File.GetLastWriteTime projectFile)))
    do fsw.EnableRaisingEvents <- true

    member __.Response with get() = agent.PostAndReply GetResponse
                        and set r = agent.Post (SetResponse r)

    interface IDisposable with
        member __.Dispose() = fsw.Dispose()
