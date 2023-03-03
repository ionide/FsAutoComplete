module B.MyModule3

let _ = List.map
//>          ^^^ List.map
let _ = List.map id
//>          ^^^ List.map
let _ = [1;2] |> List.map id |> List.sum
//>                   ^^^ List.map

let _ = B.WorkingModule.doStuff ()
//>                     ^^^^^^^ public function
open B.WorkingModule
let _ = doStuff ()
//>     ^^^^^^^ public function

let _ = internalValue + 42
//>     ^^^^^^^^^^^^^ internal value
let _ = 2 * internalValue + 42
//>         ^^^^^^^^^^^^^ internal value