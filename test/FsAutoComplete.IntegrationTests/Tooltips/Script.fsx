

module XA =
  let funky x = x + 1

let val99 = XA.funky 21

module CommandResponse =

  type ResponseMsg<'T> =
    {
      Kind: string
      Data: 'T
    }

    member x.f y = 1

///some random xml docs
let funct (x : CommandResponse.ResponseMsg<_>) = ()

let x = System.DateTime(123L)

///Sample type
type Abcd () =
  let mutable _c = 123
  member __.asdfg = 123
  member val bnm = 1 with get, set
  member __.cvbnm
    with get () = _c
    and set (value) = _c <- value
  member __.Add(a,b) = a + b
  member __.Add(a) = a + 1
  member __.Add() = 1 + 1

type Desf =
  abstract member b : int

type Qwer =
  | A of string
  | B of int * float
  | C

type Gjk =
  | Q = 1
  | W = 2
  | E = 3

let (>=>) a b = a + b

let (|Even|Odd|) input = if input % 2 = 0 then Even else Odd

let TestNumber input =
   match input with
   | Even -> 1
   | Odd -> 0

let tasd = System.String.IsNullOrEmpty
let cbfdg = Array.append

let _  = [1..3] |> List.map ((+) 1)