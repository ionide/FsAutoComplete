let arrayOfTuples = [| 1, 2 |]
let listOfTuples = [ 1, 2 ]
let listOfStructTuples = [ struct(1, 2) ]
let floatThatShouldHaveGenericReportedInTooltip = 1.
sprintf "asd"

/// <summary>
/// My super summary
/// </summary>
/// <param name="c">foo</param>
/// <param name="b">bar</param>
/// <param name="a">baz</param>
/// <returns></returns>
let someFunction (a) (b) (c) = ()
let nestedTuples = (1, ((2, 3), 4))
let nestedStructTuples = 1, struct(2, 3)

type [<Measure>]s
type [<Measure>]m
let distance = 5.<m>
let time = 1.<s>
let speed = distance/time

// The purpose of this function is to test rendering of function parameters that are themselves functions.
// In the before times, `f` would be rendered as `int -> unit` on a single line.
// After the fix, we want `f` to be rendered as `(int -> unit)` on a single line, to emphasize the single-parameter nature of the function parameter
let funcWithFunParam (f: int->unit) (i: int) = ()

/// <summary>does a thing</summary>
/// <param name="f">the inputs</param>
let funcWithTupleParam (f: (int * int)) = snd f, fst f

let funcWithStructTupleParam (f: struct(int * int)) = match f with struct(x, y) -> struct(y, x)


type X() =
    member x.Foo(stuff: (int * int * int)) = match stuff with a, b, c -> a + b + c
    member x.Bar(a,b,c) = a + b + c

let (.>>) x y = x + y

let (^) x y = x + y

let inline add x y = x + y

let result = add 5 5
