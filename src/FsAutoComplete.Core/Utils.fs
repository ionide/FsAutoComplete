namespace FsAutoComplete

open System.IO

type Result<'a> =
  | Success of 'a
  | Failure of string

type Pos = 
    { Line: int
      Col: int }

type Serializer = obj -> string

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

  let inline (</>) path1 path2 = combinePaths path1 path2

module Option =
  let getOrElse defaultValue option =
    match option with
    | None -> defaultValue
    | Some x -> x

module Async =
    /// Transforms an Async value using the specified function.
    [<CompiledName("Map")>]
    let map (mapping : 'a -> 'b) (value : Async<'a>) : Async<'b> =
        async {
            // Get the input value.
            let! x = value
            // Apply the mapping function and return the result.
            return mapping x
        }

    // Transforms an Async value using the specified Async function.
    [<CompiledName("Bind")>]
    let bind (binding : 'a -> Async<'b>) (value : Async<'a>) : Async<'b> =
        async {
            // Get the input value.
            let! x = value
            // Apply the binding function and return the result.
            return! binding x
        }

module List =
    let inline singleton x = [x]