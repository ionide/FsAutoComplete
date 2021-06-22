(* unused self reference *)
type MyClass() =
  member this.DoAThing() = ()


(*
  replace usused binding with _
  prefix _ to unused binding
*)
let six = 6

(*
  replace usused function parameter with _
  prefix _ to unused function parameter
*)
let add one two three = one + two
