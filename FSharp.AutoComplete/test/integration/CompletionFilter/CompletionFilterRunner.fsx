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

p.parse "Script.fsx"
p.completion "Script.fsx" 2 7
p.completionFilter "Script.fsx" 2 7 "StartsWith"
p.completionFilter "Script.fsx" 2 7 "Contains"
p.send "quit\n"
p.finalOutput ()
|> writeNormalizedOutput "output.txt"
