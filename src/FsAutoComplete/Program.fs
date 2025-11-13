module FsAutoComplete.Program

[<EntryPoint>]
let entry args =
  let results = Parser.invoke args
  // macos seems to stall if we don't actually call exit
  // https://github.com/fsharp/FsAutoComplete/issues/1117
  exit results
