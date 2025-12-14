module OperatorExample

// Custom operator definition
let (++) x y = x + y + 1

// Another custom operator
let (|>>) x f = f x |> f

let double x = x * 2

let main () =
  // Using custom operators - these should be detected as outgoing calls
  let a = 1 ++ 2
  let b = 3 ++ 4
  let c = a ++ b

  // Piping with custom operator
  let d = 5 |>> double

  c + d

ignore main
