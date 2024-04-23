// Learn more about F# at http://fsharp.org

open System

[<EntryPoint>]
let main argv =
    let isOk, integer = System.Int32.TryParse nope
    printfn "Hello World from F#!"
    0 // return an integer exit code
