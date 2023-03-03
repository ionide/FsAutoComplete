// all locations for "find all references" are in here
module B.WorkingModule

// Only used here in module
// -> [B/WorkingModule]
let private localValue = 42
//>         xxxxxxxxxx private value
let _ = localValue + 42
//>     ^^^^^^^^^^ private value
let private _fJustInHere v = localValue + v
//>                          ^^^^^^^^^^ private value
let _ = ignore localValue
//>            ^^^^^^^^^^ private value

// internal -> only here and following modules in current project
// -> [B/WorkingModule; B/MyModule3]
let internal internalValue = 42
//>          xxxxxxxxxxxxx internal value
let _ = internalValue + 42
//>     ^^^^^^^^^^^^^ internal value
let _ = 2 * internalValue + 42
//>         ^^^^^^^^^^^^^ internal value

// external, but only used here
// -> [B/WorkingModule]
let _ = Seq.map
//>         ^^^ Seq.map
let _ = Seq.map id
//>         ^^^ Seq.map
let _ = [1;2] |> Seq.map id |> Seq.sum
//>                  ^^^ Seq.map

// external used all over solution (and script files)
// -> [A/MyModule1; B/MyModule1; B/WorkingModule; B/MyModule3; C/MyModule1; MyScript]
let _ = List.map
//>          ^^^ List.map
let _ = List.map id
//>          ^^^ List.map
let _ = [1;2] |> List.map id |> List.sum
//>                   ^^^ List.map

// function defined here and used in following files
// -> [B/WorkingModule; B/MyModule3; C/MyModule1]
let doStuff () = ()
//> xxxxxxx public function
let _ = doStuff ()
//>     ^^^^^^^ public function

// function defined here and only used here
// -> [B/WorkingModule]
// Note: not private -> public, but nowhere else used
let doStuffJustHere () = ()
//> xxxxxxxxxxxxxxx public function, only used here
let _ = doStuffJustHere ()
//>     ^^^^^^^^^^^^^^^ public function, only used here

let _ = A.MyModule1.value
//>                 ^^^^^ function from different project
let _ = A.MyModule1.``value``
//>                 ^^^^^^^^^ function from different project

let _ = B.MyModule1.value
//>                 ^^^^^ function from same project
let _ = B.MyModule1.``value``
//>                 ^^^^^^^^^ function from same project
