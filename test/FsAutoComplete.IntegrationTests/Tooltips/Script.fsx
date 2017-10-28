

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
  member __.a = 123

type Desf =
  member __.b = 234

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