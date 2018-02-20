#load "../TestHelpers.fsx"
open TestHelpers
open System.IO
open System

(*
 * This test is a simple sanity check of a basic run of the program.
 * A few completions, files and script.
 *)

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
File.Delete "output.json"

let p = new FsAutoCompleteWrapper()

p.project "Test1.fsproj"
p.waitForLine()
Threading.Thread.Sleep 2000
File.WriteAllBytes("Test1.fsproj", File.ReadAllBytes "Test1.fsproj")
Threading.Thread.Sleep 2000
p.quit()
p.finalOutput ()
|> writeNormalizedOutput "output.json"

