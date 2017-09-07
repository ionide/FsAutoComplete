// --------------------------------------------------------------------------------------
// (c) Robin Neatherway
// --------------------------------------------------------------------------------------
namespace FsAutoComplete

module Debug =

  let mutable verbose = false
  let mutable categories : Set<string> option = None

  let mutable output = stdout

  [<Sealed>]
  type Format<'T> private () =
    static let rec mkKn (ty: System.Type) =
      if Reflection.FSharpType.IsFunction(ty) then
          let _, ran = Reflection.FSharpType.GetFunctionElements(ty)
          let f = mkKn ran
          Reflection.FSharpValue.MakeFunction(ty, fun _ -> f)
      else
          box ()

    static let instance : 'T =
      unbox (mkKn typeof<'T>)
    static member Instance = instance

  let inline print (fmt: Printf.TextWriterFormat<'a>) : 'a =
    if verbose then
      fprintfn output fmt
    else
      Format<_>.Instance

  let inline printc cat fmt =
    if verbose && (match categories with
                    | None -> true
                    | Some c -> Set.contains cat c) then
      fprintf  output "[%s] " cat
      fprintfn output fmt
    else
      Format<_>.Instance

  let private startTime = System.DateTime.Now

  let printTiming (fmt: Printf.TextWriterFormat<'a>) : 'a =
    if verbose then
      fprintf  output "%f: " (System.DateTime.Now - startTime).TotalMilliseconds
      fprintfn output fmt
    else
      Format<_>.Instance

  let waitForDebugger () =
    while not(System.Diagnostics.Debugger.IsAttached) do
      System.Threading.Thread.Sleep(100)

  let zombieCheckWithHostPID quit pid =
    try
      let hostProcess = System.Diagnostics.Process.GetProcessById(pid)
      ProcessWatcher.watch hostProcess (fun _ -> quit ())
    with
    | e ->
      printfn "Host process ID %i not found: %s" pid e.Message
      // If the process dies before we get here then request shutdown
      // immediately
      quit ()

  let inline flush () =
    if verbose then
      (output).Flush()
