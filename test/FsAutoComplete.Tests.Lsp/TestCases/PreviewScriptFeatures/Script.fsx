(*
 This file demonstrates the use of F# 4.7 preview language features, 
 which must be opted-in to.  Parsing/typechecking will fail if preview
 feature are not enabled.
*)

open System

let add x y = 
    if x < 0 then raise (ArgumentOutOfRangeException(nameof(x)))
    x + y

open System.Math

let minimal = Min(1.0, 2.0)