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
p.tooltip "Script.fsx" "  let funky x = x + 1" 4 10
p.tooltip "Script.fsx" "module CommandResponse =" 8 19
p.tooltip "Script.fsx" "  type ResponseMsg<'T> =" 10 14
p.tooltip "Script.fsx" "  let funky x = x + 1" 4 10
p.tooltip "Script.fsx" "let funct (x : CommandResponse.ResponseMsg<_>) = ()" 19 6
p.tooltip "Script.fsx" "let x = System.DateTime(123L)" 21 21
p.tooltip "Script.fsx" "type Abcd () =" 24 8
p.tooltip "Script.fsx" "type Desf =" 27 8
p.tooltip "Script.fsx" "  member __.b = 234" 28 5
p.tooltip "Script.fsx" "type Qwer =" 30 8
p.tooltip "Script.fsx" "type Gjk =" 35 8
p.tooltip "Script.fsx" "type Gjk =" 35 2
p.tooltip "Script.fsx" "let (>=>) a b = a + b" 40 7
p.tooltip "Script.fsx" "let (|Even|Odd|) input = if input % 2 = 0 then Even else Odd" 42 7
p.tooltip "Script.fsx" "   | Even -> 1" 46 8






p.send "quit\n"
p.finalOutput ()
|> writeNormalizedOutput "output.json"

