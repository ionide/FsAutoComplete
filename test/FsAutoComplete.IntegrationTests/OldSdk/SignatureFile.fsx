#load "../TestHelpers.fsx"
open TestHelpers
open System.IO
open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
File.Delete "signfile.json"

let doIt () =
  let p = new FsAutoCompleteWrapper()

  p.project "sample2/l1/Test1.fsproj"
  p.parse "sample2/l1/Module1.fsi"
  p.parse "sample2/l1/Module1.fs"

  p.quit()
  p.finalOutput ()
  |> writeNormalizedOutput "signfile.json"

doIt ()
