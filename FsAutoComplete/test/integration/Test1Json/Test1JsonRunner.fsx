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
p.parse "Script.fsx"
p.parse "Program.fs"
Threading.Thread.Sleep(8000)
p.completion "Script.fsx" 6 16
p.completion "Program.fs" 8 20
p.completion "Program.fs" 4 23
p.completion "Program.fs" 6 14
p.completion "Program.fs" 10 20
p.tooltip "FileTwo.fs" 9 7
p.tooltip "Program.fs" 6 16
p.tooltip "Program.fs" 4 9
p.tooltip "Script.fsx" 4 10
p.finddeclaration "Program.fs" 8 23
p.finddeclaration "Script.fsx" 6 16
p.declarations "Program.fs"
p.declarations "FileTwo.fs"
p.declarations "Script.fsx"
Threading.Thread.Sleep(1000)
p.send "quit\n"
p.finalOutput ()
|> writeNormalizedOutput "output.json"

