module BackgroundServices

open FsAutoComplete
open FsAutoComplete.Logging
open LanguageServerProtocol
open System.IO
open FSharp.Compiler.SourceCodeServices
open ProjectSystem

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
    let logger = LogProvider.getLoggerByName "BackgroundService"

    let notificationsHandler =
        Map.empty
        |> Map.add "background/notify" (Client.notificationHandling (fun (msg: Msg) -> async {
            logger.debug (Log.setMessage msg.Value)
            return None
        } ))
        |> Map.add "background/diagnostics" (Client.notificationHandling (fun (msg: Types.PublishDiagnosticsParams) -> async {
            messageRecived.Trigger (Diagnostics msg)
            return None
        } ))

    #if DOTNET_SPAWN
    Client.Client("dotnet", Path.Combine(p, "fsautocomplete.backgroundservices.dll") + " " + pid, notificationsHandler)
    #else
    if Environment.runningOnMono then
        Client.Client("mono", Path.Combine(p, "fsautocomplete.backgroundservices.exe")+ " " + pid, notificationsHandler)
    else
        Client.Client(Path.Combine(p, "fsautocomplete.backgroundservices.exe"), pid, notificationsHandler)
    #endif

let start () =
    client.Start ()

let updateFile(file, content, version) =
    let msg: UpdateFileParms = { File = file; Content = content; Version = version }
    client.SendRequest "background/update" msg

let updateProject(file, opts) =
    let msg = { File = file; Options = opts }
    client.SendRequest "background/project" msg

let saveFile(file) =
    let msg: FileParms = { File = file }
    client.SendRequest "background/save" msg
