let inline (|PeePee|) x = (^a : (member PeePee: string) x)
let inline peepee (PeePee pp) = pp

let inline (|PooPoo|) x = (^a : (member PooPoo: string) x)
let inline poopoo (PooPoo pp) = pp

let inline yeet ((PeePee pp & PooPoo pp') as toilet) =
    peepee toilet = pp && poopoo toilet = pp'

type SomeJson = obj

module Foo =
  let x y z = ()

let myFoo x y =
  Foo.x x y
