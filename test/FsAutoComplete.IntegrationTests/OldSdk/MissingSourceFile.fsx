#load "../TestHelpers.fsx"
open TestHelpers
open System.IO
open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let outputJson = "missingfs.json"
File.Delete outputJson

let doIt () =
  let p = new FsAutoCompleteWrapper()

  p.project "sample4/l1/Test1.fsproj"
  p.parse "sample4/l1/Module1.fs"

  p.send "quit\n"
  p.finalOutput ()
  |> writeNormalizedOutput outputJson

doIt ()
