module BackgroundServices

open FsAutoComplete
open LanguageServerProtocol
open System.IO
open FSharp.Compiler.SourceCodeServices
open Ionide.ProjInfo.ProjectSystem
open FsAutoComplete.Logging

let logger = LogProvider.getLoggerByName "Background Service"

type Msg = {Value: string}

type BackgroundFileCheckType =
| SourceFile of filePath: string
| ScriptFile of filePath: string * tfm: FSIRefs.TFM
with
    member x.FilePath =
        match x with
        | SourceFile(path)
        | ScriptFile(path, _) -> path


type UpdateFileParms = {
    File: BackgroundFileCheckType
    Content: string
    Version: int
}

type ProjectParms = {
    Options: FSharpProjectOptions
    File: string
}

type FileParms = {
    File: BackgroundFileCheckType
}

let p =
    let t = typeof<State>
    Path.GetDirectoryName t.Assembly.Location

let pid =
    System.Diagnostics.Process.GetCurrentProcess().Id.ToString()

type MessageType =
    | Diagnostics of Types.PublishDiagnosticsParams

let messageRecived = Event<MessageType>()

let client =

    let notificationsHandler =
        Map.empty
        |> Map.add "background/notify" (Client.notificationHandling (fun (msg: Msg) -> async {
            logger.info (Log.setMessage "Background service message {msg}" >> Log.addContextDestructured "msg" msg)
            return None
        } ))
        |> Map.add "background/diagnostics" (Client.notificationHandling (fun (msg: Types.PublishDiagnosticsParams) -> async {
            messageRecived.Trigger (Diagnostics msg)
            return None
        } ))

    Client.Client("dotnet", Path.Combine(p, "fsautocomplete.backgroundservices.dll") + " " + pid, notificationsHandler)

let start () =
    client.Start ()

let updateFile(file, content, version) =
    let msg: UpdateFileParms = { File = file; Content = content; Version = version }
    client.SendNotificatoin "background/update" msg

let updateProject(file, opts) =
    let msg = { File = file; Options = opts }
    client.SendNotificatoin "background/project" msg

let saveFile(file) =
    let msg: FileParms = { File = file }
    client.SendNotificatoin "background/save" msg
