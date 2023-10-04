module Example1 =

    let bar () =
        printfn "lol"

    let bazz () =
        let foo () =
            printfn "lol"
            bar ()
        foo ()

    ignore bazz
