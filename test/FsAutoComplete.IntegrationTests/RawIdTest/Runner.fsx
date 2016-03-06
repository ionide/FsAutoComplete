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

p.parse "Test-Module.fsx"
p.parse "Test-Class.fsx"
p.completion "Test-Module.fsx" 9 3
p.completion "Test-Module.fsx" 11 13
p.completion "Test-Module.fsx" 13 14
p.completion "Test-Class.fsx" 9 3
p.completion "Test-Class.fsx" 11 13
p.completion "Test-Class.fsx" 13 14
p.send "quit\n"
p.finalOutput ()
|> writeNormalizedOutput "output.json"

