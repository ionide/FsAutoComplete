#load "../TestHelpers.fsx"
open TestHelpers
open System.IO
open System

#I "../../../../packages/Fantomas/lib/"
#r "FantomasLib.dll"

open Fantomas.FormatConfig

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
File.Delete "output.json"

let p = new FsAutoCompleteWrapper()

p.project "Format.fsproj"

printfn "loaded project"

// this file has no changes when the default fantomas config is applied
p.parse "okbydefault.fs"
printfn "parsed okbydefault"

p.format "okbydefault.fs" None
printfn "formatted okbydefault"

// this file has a single offset indentation change when the dfault fantomas config is applied
p.parse "defaultoffset.fs"
printfn "parsed defaultoffset"
p.format "defaultoffset.fs" None
printfn "formatted defaultoffset"

// this file has a single indentation change when an indentation of 5 is specified, proving that we can override flags from the command line
p.parse "offset5.fs"
printfn "parsed offset5"
p.format "offset5.fs" (Some ({FormatConfig.Default with IndentSpaceNum = 5}))
printfn "formatted offset5"

p.send "quit\n"
p.finalOutput ()
|> writeNormalizedOutput "output.json"
