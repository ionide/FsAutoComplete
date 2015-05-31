#load "../TestHelpers.fsx"
open TestHelpers
open System.IO
open System

(*
 * This test is a simple sanity check of a basic run of the program.
 * A few completions, files and script.
 *)

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
File.Delete "output.txt"

let p = new FSharpAutoCompleteWrapper()

p.project "FindDecl.fsproj"
p.parse "FileTwo.fs"
p.parse "Script.fsx"
p.parse "Program.fs"
p.finddeclaration "Program.fs" 6 16
p.finddeclaration "Program.fs" 8 20
p.finddeclaration "Program.fs" 14 26
p.finddeclaration "Program.fs" 10 20
p.finddeclaration "Script.fsx" 6 17
p.send "quit\n"
p.finalOutput ()
|> writeNormalizedOutput "output.txt"
