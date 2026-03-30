module test

let test1 a b = not (a = b)
let test2 a b = not (a <> b)
let test3 = fun a -> a
let test4 = not true
let test5 = not false
let test6 = List.fold (+) 0
let test7 a = a <> true
let test8 a = a = null
let test9 a = List.head (List.sort a)

//test regression for https://github.com/fsharp/FsAutoComplete/issues/205
let x = [ 1 ]
let y = x @ [ 2; 3 ]
