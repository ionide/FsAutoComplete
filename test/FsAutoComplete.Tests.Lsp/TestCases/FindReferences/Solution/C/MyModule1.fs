module C.MyModule1

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

// Using active patterns from B.WorkingModule (cross-project)

// Total active pattern usage (qualified)
let _ = B.WorkingModule.(|Even|Odd|) 200
let classifyInC n =
    match n with
    | B.WorkingModule.Even -> "even"
    | B.WorkingModule.Odd -> "odd"

// Total active pattern usage (open)
let _ = (|Even|Odd|) 300
let classifyInCOpen n =
    match n with
    | Even -> "even"
    | Odd -> "odd"

// Partial active pattern usage (qualified) - cross-file
// NOTE: No markers here - see B/WorkingModule.fs for explanation of FCS limitations
let _ = B.WorkingModule.(|ParseInt|_|) "777"
let parseInCQualified input =
    match input with
    | B.WorkingModule.ParseInt n -> Some n
    | _ -> None

// Partial active pattern usage (open) - cross-file
let _ = (|ParseInt|_|) "888"
let parseInCOpen input =
    match input with
    | ParseInt n -> Some n
    | _ -> None
