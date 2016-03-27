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
p.completion "Test-Module.fsx" "X." 9 3
p.completion "Test-Module.fsx" "X.``Column T" 11 13
p.completion "Test-Module.fsx" "X.``Another`C" 13 14
p.completion "Test-Class.fsx" "Y." 9 3
p.completion "Test-Class.fsx" "Y.``Column T" 11 13
p.completion "Test-Class.fsx" "Y.``Another`C" 13 14
p.send "quit\n"
p.finalOutput ()
|> writeNormalizedOutput "output.json"

