namespace FsAutoComplete

open System.IO

type Result<'a> =
  | Success of 'a
  | Failure of string

module Utils =
  
  let isAScript fileName =
      let ext = Path.GetExtension fileName
      [".fsx";".fsscript";".sketchfs"] |> List.exists ((=) ext)

  let runningOnMono = 
    try System.Type.GetType("Mono.Runtime") <> null
    with _ -> false

module Option =
  let getOrElse defaultValue option =
    match option with
    | None -> defaultValue
    | Some x -> x
