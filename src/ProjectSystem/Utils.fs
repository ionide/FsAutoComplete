namespace ProjectSystem

open System.IO

[<AutoOpen>]
module internal Utils =
  let inline combinePaths path1 (path2 : string) = Path.Combine(path1, path2.TrimStart [| '\\'; '/' |])

  let inline (</>) path1 path2 = combinePaths path1 path2

  let normalizeDirSeparators (path: string) =
    match Path.DirectorySeparatorChar with
    | '\\' -> path.Replace('/', '\\')
    | '/' -> path.Replace('\\', '/')
    | _ -> path
