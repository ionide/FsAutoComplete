#I ".."
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
p.parse "FileTwo.fs"
p.parse "Program.fs"
Threading.Thread.Sleep(6000)
p.methods "Program.fs" 4 36
p.methods "Program.fs" 4 37
p.methods "Program.fs" 8 30
p.methods "Program.fs" 8 35
p.methods "Program.fs" 10 39
p.methods "Program.fs" 14 3
Threading.Thread.Sleep(1000)
p.send "quit\n"
p.finalOutput ()
|> writeNormalizedOutput "output.json"
