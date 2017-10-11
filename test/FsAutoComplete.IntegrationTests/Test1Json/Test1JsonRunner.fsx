#load "../TestHelpers.fsx"
open TestHelpers
open System.IO
open System

(*
 * This test is a simple sanity check of a basic run of the program.
 * A few completions, files and script.
 *)

let outputJson = outputJsonForRuntime "output.json"

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
File.Delete outputJson

let p = new FsAutoCompleteWrapper()


p.project "Test1.fsproj"
p.parse "FileTwo.fs"
p.parse "Script.fsx"
p.parse "Program.fs"
p.completion "Script.fsx" "let val99 = XA.funky 21" 6 16
p.completionFilter "Program.fs" "let val3 = testval.Terrific val2" 8 21 "StartsWith"
p.completion "Program.fs" "let testval = FileTwo.NewObjectType()" 4 23
p.completion "Program.fs" "let val2 = X.func 2" 6 14
p.completion "Program.fs" "let val4 : FileTwo.NewObjectType = testval" 10 20
p.tooltip "FileTwo.fs" "let add x y = x + y" 9 7
p.tooltip "Program.fs" "let val2 = X.func 2" 6 16
p.tooltip "Program.fs" "let testval = FileTwo.NewObjectType()" 4 9
p.tooltip "Script.fsx" "  let funky x = x + 1" 4 10
p.finddeclaration "Program.fs" "let val3 = testval.Terrific val2" 8 23
p.finddeclaration "Script.fsx" "let val99 = XA.funky 21" 6 16
p.declarations "Program.fs"
p.declarations "FileTwo.fs"
p.declarations "Script.fsx"
p.send "quit\n"
p.finalOutput ()
|> writeNormalizedOutput outputJson

