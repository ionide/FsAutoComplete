// Learn more about F# at http://fsharp.org

open System

[<EntryPoint>]
let main argv =
    l1.Say.hello "F#!"
    l1.Say.jsonstuff()
    0 // return an integer exit code
