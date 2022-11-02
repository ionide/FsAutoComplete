module B.MyModule1

let _ = List.map
//>          ^^^ List.map
let _ = List.map id
//>          ^^^ List.map
let _ = [1;2] |> List.map id |> List.sum
//>                   ^^^ List.map

let value = "hello from B.MyModule1"
//> xxxxx function from same project
let _ = value
//>     ^^^^^ function from same project
let _ = ``value``
//>     ^^^^^^^^^ function from same project

