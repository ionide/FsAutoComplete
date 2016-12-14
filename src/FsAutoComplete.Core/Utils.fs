[<AutoOpen>]
module FsAutoComplete.Utils

open System.IO
open System.Collections.Concurrent
open System.Diagnostics
open System

type Result<'a> =
  | Success of 'a
  | Failure of string

type Pos =
    { Line: int
      Col: int }

type Serializer = obj -> string
type ProjectFilePath = string
type SourceFilePath = string
type LineStr = string

let isAScript fileName =
    let ext = Path.GetExtension fileName
    [".fsx";".fsscript";".sketchfs"] |> List.exists ((=) ext)

let runningOnMono =
  try not << isNull <| Type.GetType "Mono.Runtime"
  with _ -> false

let normalizePath (file : string) =
  if file.EndsWith ".fs" then
      let p = Path.GetFullPath file
      (p.Chars 0).ToString().ToLower() + p.Substring(1)
  else file

let inline combinePaths path1 (path2 : string) = Path.Combine(path1, path2.TrimStart [| '\\'; '/' |])

let inline (</>) path1 path2 = combinePaths path1 path2

let private sepChar = Path.DirectorySeparatorChar

let normalizeDirSeparators (path: string) =
  match sepChar with
  | '\\' -> path.Replace('/', '\\')
  | '/' -> path.Replace('\\', '/')
  | _ -> path

[<RequireQualifiedAccess>]
module Option =
  let getOrElse defaultValue option =
    match option with
    | None -> defaultValue
    | Some x -> x

  let orElse other option =
    match option with
    | None -> other()
    | Some x -> Some x

  let getOrElseFun defaultValue option =
    match option with
    | None -> defaultValue()
    | Some x -> x

  let inline orTry f =
    function
    | Some x -> Some x
    | None -> f()

[<RequireQualifiedAccess>]
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

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Array =
    let inline private checkNonNull argName arg =
        match box arg with
        | null -> nullArg argName
        | _ -> ()

    /// Fold over the array passing the index and element at that index to a folding function
    let foldi (folder: 'State -> int -> 'T -> 'State) (state: 'State) (array: 'T []) =
        checkNonNull "array" array
        if array.Length = 0 then state else
        let folder = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt folder
        let mutable state:'State = state
        let len = array.Length
        for i = 0 to len - 1 do
            state <- folder.Invoke (state, i, array.[i])
        state

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module String =
    let (|StartsWith|_|) pattern value =
        if String.IsNullOrWhiteSpace value then
            None
        elif value.StartsWith pattern then
            Some()
        else None



type ConcurrentDictionary<'key, 'value> with
    member x.TryFind key =
        match x.TryGetValue key with
        | true, value -> Some value
        | _ -> None

    member x.ToSeq() =
        x |> Seq.map (fun (KeyValue(k, v)) -> k, v)

type Path with
    static member GetFullPathSafe path =
        try Path.GetFullPath path
        with _ -> path

    static member GetFileNameSafe path =
        try Path.GetFileName path
        with _ -> path



let inline debug msg = Printf.kprintf Debug.WriteLine msg
let inline fail msg = Printf.kprintf Debug.Fail msg