(*
 This file demonstrates the use of F# 5 language features.
*)

open System

let add x y =
    if x < 0 then raise (ArgumentOutOfRangeException(nameof(x)))
    x + y

open type System.Math

let minimal = Min(1.0, 2.0)
