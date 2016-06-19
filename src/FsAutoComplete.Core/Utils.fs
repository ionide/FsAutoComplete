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

  let normalizePath (file : string) = 
    if file.EndsWith ".fs" then 
        let p = Path.GetFullPath file
        (p.Chars 0).ToString().ToLower() + p.Substring(1)
    else file
    
  let inline combinePaths path1 (path2 : string) = Path.Combine(path1, path2.TrimStart [| '\\'; '/' |])

  let inline (@@) path1 path2 = combinePaths path1 path2

module Option =
  let getOrElse defaultValue option =
    match option with
    | None -> defaultValue
    | Some x -> x
