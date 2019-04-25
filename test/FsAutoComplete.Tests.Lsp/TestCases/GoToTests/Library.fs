module GoToTests

let z = Definitions.sample_value

let x = Definitions.value_with_type

let v = {
    new Definitions.IInterface with
        member __.C(a) = 123
        member __.D(a) = 123
}

type Abc () =
    interface Definitions.IInterface with
        member __.C(a) = 123
        member __.D(a) = 123