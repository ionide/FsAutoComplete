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

p.send "outputmode json\n"
p.project "SymbolUse.fsproj"
p.parse "FileTwo.fs"
p.parse "Script.fsx"
p.parse "Program.fs"
Threading.Thread.Sleep(3000)
p.symboluse "Program.fs" 4 5 //testval
p.symboluse "Program.fs" 14 12 //testval shadowed
p.symboluse "Program.fs" 12 4 //miss
p.symboluse "Program.fs" 12 5 //shadowed start
p.symboluse "Program.fs" 12 13 //shadowed end

Threading.Thread.Sleep(1000)
p.send "quit\n"
p.finalOutput ()
|> writeNormalizedOutput "output.json"

