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

// Multiline string: exercises the fix for uint32 underflow in semantic token encoding.
// Before the fix, FCS would return a multiline SemanticClassificationItem for this token,
// causing tokenLen = uint32(End.Character - Start.Character) to underflow for multiline ranges.
let multilineString =
  """this is a
multiline string literal"""

let afterMultiline = 42

// Regression test for https://github.com/ionide/FsAutoComplete/issues/1381:
// `null` in a nullable type annotation like `string | null` should receive a Keyword token.
let withNull (x: string | null) = ()
