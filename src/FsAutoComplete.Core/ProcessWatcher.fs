namespace FsAutoComplete

open System.Diagnostics
open FsAutoComplete.Logging

module ProcessWatcher =

  let private logger = LogProvider.getLoggerByName "ProcessWatcher"

  type private OnExitMessage = Watch of Process * (Process -> unit)

  let watch (proc: Process) onExitCallback =

    proc.EnableRaisingEvents <- true

    proc.Exited
    |> Event.add (fun _ -> onExitCallback proc)

  let zombieCheckWithHostPID quit pid =
    try
      let hostProcess = Process.GetProcessById(pid)
      watch hostProcess (fun _ -> quit ())
    with
    | e ->
      logger.error (
        Log.setMessage "Host process {pid} not found"
        >> Log.addContextDestructured "pid" pid
        >> Log.addExn e
      )

      quit ()
