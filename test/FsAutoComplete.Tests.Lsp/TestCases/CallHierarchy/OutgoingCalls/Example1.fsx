module OutgoingExample1 =

    let helper () =
        printfn "helper function"

    let calculate x y =
        x + y

    let mainFunction () =
        helper ()        // Call to helper function - line 9
        let result = calculate 5 10  // Call to calculate function - line 10
        printfn "Result: %d" result  // Call to printfn - line 11
        result

    let anotherFunction () =
        mainFunction ()  // Call to mainFunction

    ignore anotherFunction