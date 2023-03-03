module A.MyModule1

let _ = List.map
//>          ^^^ List.map
let _ = List.map id
//>          ^^^ List.map
let _ = [1;2] |> List.map id |> List.sum
//>                   ^^^ List.map


let value = "hello from A"
//> xxxxx function from different project
let _ = value
//>     ^^^^^ function from different project
let _ = ``value``
//>     ^^^^^^^^^ function from different project
