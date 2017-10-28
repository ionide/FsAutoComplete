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
p.methods "Program.fs" "let testval = FileTwo.NewObjectType()" 9 36
p.methods "Program.fs" "let testval = FileTwo.NewObjectType()" 9 37
p.methods "Program.fs" "let val3 = testval.Terrific(val2, 'c')" 13 30
p.methods "Program.fs" "let val3 = testval.Terrific(val2, 'c')" 13 35
p.methods "Program.fs" "let val4 = MyDateTime.Parse(\\\"hello\\\")" 15 34
p.methods "Program.fs" "          \\\"hello\\\"" 19 3
p.send "quit\n"
p.finalOutput ()
|> writeNormalizedOutput "output.json"
