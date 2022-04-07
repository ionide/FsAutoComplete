module FsAutoComplete.Program

open System.CommandLine.Parsing

[<EntryPoint>]
let entry args =
    let results = Parser.parser.Invoke args
    results
