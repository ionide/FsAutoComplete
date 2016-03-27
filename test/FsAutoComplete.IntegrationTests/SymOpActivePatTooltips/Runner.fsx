#load "../TestHelpers.fsx"
open TestHelpers
open System.IO
open System

(*
 * This test runs through tooltips for symbolic operators
 *
 *)

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
File.Delete "output.json"

let p = new FsAutoCompleteWrapper()

p.parse "Script.fsx"
p.tooltip "Script.fsx" "let val99 = 1 |<>| 1 |..| 1" 6 14
p.tooltip "Script.fsx" "let val99 = 1 |<>| 1 |..| 1" 6 15
p.tooltip "Script.fsx" "let val99 = 1 |<>| 1 |..| 1" 6 16
p.tooltip "Script.fsx" "let val99 = 1 |<>| 1 |..| 1" 6 17
p.tooltip "Script.fsx" "let val99 = 1 |<>| 1 |..| 1" 6 18
p.tooltip "Script.fsx" "let val99 = 1 |<>| 1 |..| 1" 6 19
p.tooltip "Script.fsx" "let val99 = 1 |<>| 1 |..| 1" 6 20
p.tooltip "Script.fsx" "let val99 = 1 |<>| 1 |..| 1" 6 21
p.tooltip "Script.fsx" "let val99 = 1 |<>| 1 |..| 1" 6 22
p.tooltip "Script.fsx" "let val99 = 1 |<>| 1 |..| 1" 6 23
p.tooltip "Script.fsx" "let val99 = 1 |<>| 1 |..| 1" 6 24
p.tooltip "Script.fsx" "let val99 = 1 |<>| 1 |..| 1" 6 25
p.tooltip "Script.fsx" "let val99 = 1 |<>| 1 |..| 1" 6 26
p.tooltip "Script.fsx" "let val99 = 1 |<>| 1 |..| 1" 6 27
p.tooltip "Script.fsx" "let (|Zero|Succ|) n = if n = 0 then Zero else Succ(n-1)" 8 7
p.tooltip "Script.fsx" "let (|Zero|Succ|) n = if n = 0 then Zero else Succ(n-1)" 8 11
p.tooltip "Script.fsx" "let (|Zero|Succ|) n = if n = 0 then Zero else Succ(n-1)" 8 16
p.send "quit\n"
p.finalOutput ()
|> writeNormalizedOutput "output.json"

