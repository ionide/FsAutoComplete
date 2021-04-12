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
