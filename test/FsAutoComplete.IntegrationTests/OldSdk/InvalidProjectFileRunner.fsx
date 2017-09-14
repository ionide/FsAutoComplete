#load "../TestHelpers.fsx"
open TestHelpers
open System.IO
open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
File.Delete "invalidprojectfile.json"

let doIt () =
  let p = new FsAutoCompleteWrapper()

  p.project "sample3/l1/Test1.fsproj"
  p.parse "sample3/l1/Module1.fs"

  p.send "quit\n"
  p.finalOutput ()
  |> writeNormalizedOutput "invalidprojectfile.json"

doIt ()
