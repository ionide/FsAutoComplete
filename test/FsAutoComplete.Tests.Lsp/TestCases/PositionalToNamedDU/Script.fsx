type A = A of a: int * b: bool

let (A(a, b)) = A(1, true)

match A(1, true) with
| A(a, b) -> ()

match A(1, true) with
| (A(a, b)) -> ()

type ThirdFieldWasJustAdded = ThirdFieldWasJustAdded of a: int * b: bool * c: char

let (ThirdFieldWasJustAdded(a, b)) = ThirdFieldWasJustAdded(1, true, 'c')
