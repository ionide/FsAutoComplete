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

let p = new FsAutoCompleteWrapper()

p.parse "Script.fsx"
p.completion "Script.fsx" 6 17
p.completion "Script.fsx" 6 16
p.tooltip "Script.fsx" 6 14
p.tooltip "Script.fsx" 6 15
p.tooltip "Script.fsx" 6 16
p.tooltip "Script.fsx" 6 17
p.finddeclaration "Script.fsx" 6 14
p.finddeclaration "Script.fsx" 6 15
p.finddeclaration "Script.fsx" 6 16
p.finddeclaration "Script.fsx" 6 17

p.send "quit\n"
p.finalOutput ()
|> writeNormalizedOutput "output.txt"

