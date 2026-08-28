module SignatureTests

type Color =
  | Red
  | Green

let private helper x = x + 1

let add x y = helper x + y

let addTwice x = add x x
