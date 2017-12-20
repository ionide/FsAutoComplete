[<AutoOpen>]
module FsAutoComplete.Utils

open System.IO
open System.Collections.Concurrent
open System.Diagnostics
open System

#if NETSTANDARD2_0
// F# 4.1 contains the Result type alredy
#else
type Result<'a,'b> =
  | Ok of 'a
  | Error of 'b

module Result =
  let map mapping result = match result with Error e -> Error e | Ok x -> Ok (mapping x)
  let mapError mapping result = match result with Error e -> Error (mapping e) | Ok x -> Ok x
  let bind binder result = match result with Error e -> Error e | Ok x -> binder x
#endif

type ResultOrString<'a> = Result<'a, string>


type Document =
    { FullName : string
      LineCount : int
      GetText : unit -> string
      GetLineText0 : int -> string
      GetLineText1 : int -> string}


type Serializer = obj -> string
type ProjectFilePath = string
type SourceFilePath = string
type FilePath = string
type LineStr = string

let isAScript fileName =
    let ext = Path.GetExtension fileName
    [".fsx";".fsscript";".sketchfs"] |> List.exists ((=) ext)

let runningOnMono =
  try not << isNull <| Type.GetType "Mono.Runtime"
  with _ -> false

let normalizePath (file : string) =
  if file.EndsWith ".fs" || file.EndsWith ".fsi" then
      let p = Path.GetFullPath file
      (p.Chars 0).ToString().ToLower() + p.Substring(1)
  else file

let inline combinePaths path1 (path2 : string) = Path.Combine(path1, path2.TrimStart [| '\\'; '/' |])

let inline (</>) path1 path2 = combinePaths path1 path2

let normalizeDirSeparators (path: string) =
  match Path.DirectorySeparatorChar with
  | '\\' -> path.Replace('/', '\\')
  | '/' -> path.Replace('\\', '/')
  | _ -> path

[<RequireQualifiedAccess>]
module Option =

  let inline attempt (f: unit -> 'T) = try Some <| f() with _ -> None

  let getOrElse defaultValue option =
    match option with
    | None -> defaultValue
    | Some x -> x

  /// Gets the option if Some x, otherwise the supplied default value.
  let inline orElse v option =
    match option with
    | Some x -> Some x
    | None -> v

  let inline fill f o =
    match o with
    | Some v -> v
    | _      -> f



  let orElseFun other option =
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

  /// Some(Some x) -> Some x | None -> None
  let inline flatten x =
    match x with
    | Some x -> x
    | None -> None


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

    let StartCatchCancellation(work, cancellationToken) =
        Async.FromContinuations(fun (cont, econt, _) ->
          // When the child is cancelled, report OperationCancelled
          // as an ordinary exception to "error continuation" rather
          // than using "cancellation continuation"
          let ccont e = econt e
          // Start the workflow using a provided cancellation token
          Async.StartWithContinuations( work, cont, econt, ccont, cancellationToken=cancellationToken) )

[<Sealed>]
type AsyncMaybeBuilder () =
    [<DebuggerStepThrough>]
    member __.Return value : Async<'T option> = Some value |> async.Return

    [<DebuggerStepThrough>]
    member __.ReturnFrom value : Async<'T option> = value

    [<DebuggerStepThrough>]
    member __.ReturnFrom (value: 'T option) : Async<'T option> = async.Return value

    [<DebuggerStepThrough>]
    member __.Zero () : Async<unit option> =
        Some () |> async.Return

    [<DebuggerStepThrough>]
    member __.Delay (f : unit -> Async<'T option>) : Async<'T option> = f ()

    [<DebuggerStepThrough>]
    member __.Combine (r1, r2 : Async<'T option>) : Async<'T option> =
        async {
            let! r1' = r1
            match r1' with
            | None -> return None
            | Some () -> return! r2
        }

    [<DebuggerStepThrough>]
    member __.Bind (value: Async<'T option>, f : 'T -> Async<'U option>) : Async<'U option> =
        async {
            let! value' = value
            match value' with
            | None -> return None
            | Some result -> return! f result
        }

    [<DebuggerStepThrough>]
    member __.Bind (value: 'T option, f : 'T -> Async<'U option>) : Async<'U option> =
        async {
            match value with
            | None -> return None
            | Some result -> return! f result
        }

    [<DebuggerStepThrough>]
    member __.Using (resource : ('T :> IDisposable), body : _ -> Async<_ option>) : Async<_ option> =
        try body resource
        finally
            if not << isNull <| resource then resource.Dispose ()

    [<DebuggerStepThrough>]
    member x.While (guard, body : Async<_ option>) : Async<_ option> =
        if guard () then
            x.Bind (body, (fun () -> x.While (guard, body)))
        else
            x.Zero ()

    [<DebuggerStepThrough>]
    member x.For (sequence : seq<_>, body : 'T -> Async<unit option>) : Async<_ option> =
        x.Using (sequence.GetEnumerator (), fun enum ->
            x.While (enum.MoveNext, x.Delay (fun () -> body enum.Current)))

    [<DebuggerStepThrough>]
    member inline __.TryWith (computation : Async<'T option>, catchHandler : exn -> Async<'T option>) : Async<'T option> =
            async.TryWith (computation, catchHandler)

    [<DebuggerStepThrough>]
    member inline __.TryFinally (computation : Async<'T option>, compensation : unit -> unit) : Async<'T option> =
            async.TryFinally (computation, compensation)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module AsyncMaybe =
    let inline liftAsync (async : Async<'T>) : Async<_ option> =
        async |> Async.map Some


[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Array =
    let inline private checkNonNull argName arg =
        match box arg with
        | null -> nullArg argName
        | _ -> ()

    /// Optimized arrays equality. ~100x faster than `array1 = array2` on strings.
    /// ~2x faster for floats
    /// ~0.8x slower for ints
    let inline areEqual (xs: 'T []) (ys: 'T []) =
        match xs, ys with
        | null, null -> true
        | [||], [||] -> true
        | null, _ | _, null -> false
        | _ when xs.Length <> ys.Length -> false
        | _ ->
            let mutable break' = false
            let mutable i = 0
            let mutable result = true
            while i < xs.Length && not break' do
                if xs.[i] <> ys.[i] then
                    break' <- true
                    result <- false
                i <- i + 1
            result


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

    /// Returns all heads of a given array.
    /// For [|1;2;3|] it returns [|[|1; 2; 3|]; [|1; 2|]; [|1|]|]
    let heads (array: 'T []) =
        checkNonNull "array" array
        let res = Array.zeroCreate<'T[]> array.Length
        for i = array.Length - 1 downto 0 do
            res.[i] <- array.[0..i]
        res

    /// check if subArray is found in the wholeArray starting
    /// at the provided index
    let inline isSubArray (subArray: 'T []) (wholeArray:'T []) index =
        if isNull subArray || isNull wholeArray then false
        elif subArray.Length = 0 then true
        elif subArray.Length > wholeArray.Length then false
        elif subArray.Length = wholeArray.Length then areEqual subArray wholeArray else
        let rec loop subidx idx =
            if subidx = subArray.Length then true
            elif subArray.[subidx] = wholeArray.[idx] then loop (subidx+1) (idx+1)
            else false
        loop 0 index

    /// Returns true if one array has another as its subset from index 0.
    let startsWith (prefix: _ []) (whole: _ []) =
        isSubArray prefix whole 0

    /// Returns true if one array has trailing elements equal to another's.
    let endsWith (suffix: _ []) (whole: _ []) =
        isSubArray suffix whole (whole.Length-suffix.Length)

    /// Returns a new array with an element replaced with a given value.
    let replace index value (array: _ []) =
        checkNonNull "array" array
        if index >= array.Length then raise (IndexOutOfRangeException "index")
        let res = Array.copy array
        res.[index] <- value
        res

    /// pass an array byref to reverse it in place
    let revInPlace (array: 'T []) =
        checkNonNull "array" array
        if areEqual array [||] then () else
        let arrlen, revlen = array.Length-1, array.Length/2 - 1
        for idx in 0 .. revlen do
            let t1 = array.[idx]
            let t2 = array.[arrlen-idx]
            array.[idx] <- t2
            array.[arrlen-idx] <- t1

module List =

    ///Returns the greatest of all elements in the list that is less than the threshold
    let maxUnderThreshold nmax =
        List.maxBy(fun n -> if n > nmax then 0 else n)




[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module String =
    let inline toCharArray (str:string) = str.ToCharArray()

    let lowerCaseFirstChar (str: string) =
        if String.IsNullOrEmpty str
         || Char.IsLower(str, 0) then str else
        let strArr = toCharArray str
        match Array.tryHead strArr with
        | None -> str
        | Some c  ->
            strArr.[0] <- Char.ToLower c
            String (strArr)


    let extractTrailingIndex (str: string) =
        match str with
        | null -> null, None
        | _ ->
            let charr = str.ToCharArray()
            Array.revInPlace charr
            let digits = Array.takeWhile Char.IsDigit charr
            Array.revInPlace digits
            String digits
            |> function
               | "" -> str, None
               | index -> str.Substring (0, str.Length - index.Length), Some (int index)


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

type Path with
    static member GetFullPathSafe path =
        try Path.GetFullPath path
        with _ -> path

    static member GetFileNameSafe path =
        try Path.GetFileName path
        with _ -> path



let inline debug msg = Printf.kprintf Debug.WriteLine msg
let inline fail msg = Printf.kprintf Debug.Fail msg
let asyncMaybe = AsyncMaybeBuilder()


let chooseByPrefix prefix (s: string) =
    if s.StartsWith(prefix) then Some (s.Substring(prefix.Length))
    else None

let chooseByPrefix2 prefixes (s: string) =
    prefixes
    |> List.tryPick (fun prefix -> chooseByPrefix prefix s)

let splitByPrefix prefix (s: string) =
    if s.StartsWith(prefix) then Some (prefix, s.Substring(prefix.Length))
    else None

let splitByPrefix2 prefixes (s: string) =
    prefixes
    |> List.tryPick (fun prefix -> splitByPrefix prefix s)


let runProcess (log: string -> unit) (workingDir: string) (exePath: string) (args: string) =
    let psi = System.Diagnostics.ProcessStartInfo()
    psi.FileName <- exePath
    psi.WorkingDirectory <- workingDir
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    psi.Arguments <- args
    psi.CreateNoWindow <- true
    psi.UseShellExecute <- false

    use p = new System.Diagnostics.Process()
    p.StartInfo <- psi

    p.OutputDataReceived.Add(fun ea -> log (ea.Data))

    p.ErrorDataReceived.Add(fun ea -> log (ea.Data))

    // printfn "running: %s %s" psi.FileName psi.Arguments

    p.Start() |> ignore
    p.BeginOutputReadLine()
    p.BeginErrorReadLine()
    p.WaitForExit()

    let exitCode = p.ExitCode

    exitCode, (workingDir, exePath, args)
