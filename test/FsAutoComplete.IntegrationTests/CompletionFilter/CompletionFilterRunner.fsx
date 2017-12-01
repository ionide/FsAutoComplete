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

p.parse "Script.fsx"

p.completion "Script.fsx" "X.T" 8 4
p.completionFilter "Script.fsx" "X.T" 8 4 "StartsWith"
p.completionFilter "Script.fsx" "X.T" 8 4 "Contains"
p.quit()
p.finalOutput ()
|> writeNormalizedOutput "output.json"
