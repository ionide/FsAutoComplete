module Foo =

    let bar () =
        printfn "lol"

    let bazz () =
        let foo () =
            printfn "lol"
            bar ()
        foo ()

