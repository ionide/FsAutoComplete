module Project1B
type A = B of xxx: int * yyy : int
let b = B(xxx=1, yyy=2)
let x = 
    match b with
    // does not find usage here
    | B (xxx = a; yyy = b) -> ()
