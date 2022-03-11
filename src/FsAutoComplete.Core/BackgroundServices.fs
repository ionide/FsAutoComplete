module BackgroundServices

open FsAutoComplete
open Ionide.LanguageServerProtocol
open System.IO
open FSharp.Compiler.CodeAnalysis
open Ionide.ProjInfo.ProjectSystem
open FsAutoComplete.Logging

let logger =
  LogProvider.getLoggerByName "Background Service"

type Msg = { Value: string }

type BackgroundFileCheckType =
  | SourceFile of filePath: string
  | ScriptFile of filePath: string * tfm: FSIRefs.TFM
  member x.FilePath =
    match x with
    | SourceFile (path)
    | ScriptFile (path, _) -> path


type UpdateFileParms =
  { File: BackgroundFileCheckType
    Content: string
    Version: int }

type ProjectParms =
  { Options: FSharpProjectOptions
    ReferencedProjects: string array
    File: string }

type FileParms = { File: BackgroundFileCheckType }

type InitParms = {
  Ready: bool
}

let p =
  let t = typeof<State>
  Path.GetDirectoryName t.Assembly.Location

let pid =
  System
    .Diagnostics
    .Process
    .GetCurrentProcess()
    .Id.ToString()

type MessageType = Diagnostics of Types.PublishDiagnosticsParams

type BackgroundService =
  abstract UpdateFile : BackgroundFileCheckType * string * int -> unit
  abstract UpdateProject : string * FSharpProjectOptions -> unit
  abstract SaveFile : BackgroundFileCheckType -> unit
  abstract InitWorkspace : unit -> unit
  abstract MessageReceived : IEvent<MessageType>
  abstract Start : workspaceDir: string -> unit
  abstract GetSymbols: string -> Async<option<SymbolCache.SymbolUseRange array>>
  abstract GetImplementation: string -> Async<option<SymbolCache.SymbolUseRange array>>

type ActualBackgroundService() =
  let messageRecieved = Event<MessageType>()

  let client =

    let notificationsHandler =
      Map.empty
      |> Map.add
           "background/notify"
           (Client.notificationHandling
             (fun (msg: Msg) ->
               async {
                 logger.info (
                   Log.setMessage "Background service message {msg}"
                   >> Log.addContextDestructured "msg" msg
                 )

                 return None
               }))
      |> Map.add
           "background/diagnostics"
           (Client.notificationHandling
             (fun (msg: Types.PublishDiagnosticsParams) ->
               async {
                 messageRecieved.Trigger(Diagnostics msg)
                 return None
               }))

    Client.Client(
      "dotnet",
      "--roll-forward LatestMajor"
      + " "
      + Path.Combine(p, "fsautocomplete.backgroundservices.dll")
      + " "
      + pid,
      notificationsHandler
    )

  interface BackgroundService with
    member x.Start (workspaceDir) =
      logger.info (Log.setMessage "Starting background service")
      SymbolCache.initCache workspaceDir
      client.Start()

    member x.UpdateFile(file, content, version) =
      let msg : UpdateFileParms =
        { File = file
          Content = content
          Version = version }

      client.SendNotification "background/update" msg

    member x.UpdateProject(file, opts) =
      let optsToSend = {opts with ReferencedProjects = [||] }
      let refs =
        opts.ReferencedProjects |> Array.map (fun x -> x.OutputFile)
      let msg = { File = file; Options = optsToSend; ReferencedProjects = refs }
      client.SendNotification "background/project" msg

    member x.SaveFile(file) =
      let msg : FileParms = { File = file }
      client.SendNotification "background/save" msg

    member x.InitWorkspace () =
      client.SendNotification "background/init" {Ready = true}

    member x.MessageReceived = messageRecieved.Publish

    member x.GetSymbols symbolName =
      SymbolCache.getSymbols symbolName

    member x.GetImplementation symbolName =
      SymbolCache.getImplementation symbolName

type MockBackgroundService() =
  let m = Event<_>()

  interface BackgroundService with
    member x.Start _ = ()

    member x.UpdateFile(file, content, version) = ()

    member x.UpdateProject(file, opts) = ()

    member x.SaveFile(file) = ()

    member x.InitWorkspace () = ()

    member x.MessageReceived = m.Publish

    member x.GetSymbols _ = async { return None }

    member x.GetImplementation _ = async { return None }
