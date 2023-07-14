module FsAutoComplete.Program

open System.CommandLine.Parsing

[<EntryPoint>]
let entry args =
  let results = Parser.parser.Invoke args
  // macos seems to stall if we don't actually call exit
  // https://github.com/fsharp/FsAutoComplete/issues/1117
  exit results
