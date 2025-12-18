namespace ActivePatternProject

/// Main program that uses patterns from all modules
module Program =
    open Patterns
    open Module1
    open Module2

    // Direct usage of patterns
    let demo1 () =
        // Even|Odd usage
        let result = 
            match 42 with
            | Even -> "forty-two is even"
            | Odd -> "forty-two is odd"
        printfn "%s" result

    let demo2 () =
        // ParseInt usage
        match "123" with
        | ParseInt n -> printfn "Parsed: %d" n
        | _ -> printfn "Failed to parse"

    let demo3 () =
        // Using patterns as functions
        let evenOddResult = (|Even|Odd|) 100
        let parseResult = (|ParseInt|_|) "456"
        printfn "EvenOdd: %A, Parse: %A" evenOddResult parseResult

    let demo4 () =
        // Cross-module usage
        let m1Result = classifyNumber 10
        let m2Result = classifyWithQualified 20
        printfn "Module1: %s, Module2: %s" m1Result m2Result

    let demo5 () =
        // Positive|Negative|Zero usage
        let values = [-5; 0; 5]
        for v in values do
            match v with
            | Positive -> printfn "%d is positive" v
            | Negative -> printfn "%d is negative" v
            | Zero -> printfn "%d is zero" v

    let demo6 () =
        // ParseFloat usage
        match "3.14" with
        | ParseFloat f -> printfn "Float: %f" f
        | _ -> printfn "Not a float"

    let demo7 () =
        // DivisibleBy usage
        for n in 1..15 do
            match n with
            | DivisibleBy 3 q -> printfn "%d / 3 = %d" n q
            | _ -> ()

    // ============================================
    // STRUCT PARTIAL ACTIVE PATTERNS demos
    // ============================================

    let demoStruct1 () =
        // ParseIntStruct usage
        match "789" with
        | ParseIntStruct n -> printfn "Parsed (struct): %d" n
        | _ -> printfn "Failed to parse"

    let demoStruct2 () =
        // ParseFloatStruct usage
        match "2.718" with
        | ParseFloatStruct f -> printfn "Float (struct): %f" f
        | _ -> printfn "Not a float"

    let demoStruct3 () =
        // Using struct patterns as functions
        let parseIntResult = (|ParseIntStruct|_|) "321"
        let parseFloatResult = (|ParseFloatStruct|_|) "1.618"
        printfn "ParseInt (struct): %A, ParseFloat (struct): %A" parseIntResult parseFloatResult

    let demoStruct4 () =
        // NonEmptyStruct usage
        match "hello" with
        | NonEmptyStruct s -> printfn "Non-empty (struct): %s" s
        | _ -> printfn "Empty string"

    let demoStruct5 () =
        // DivisibleByStruct usage
        for n in 1..15 do
            match n with
            | DivisibleByStruct 5 q -> printfn "%d / 5 = %d (struct)" n q
            | _ -> ()

    [<EntryPoint>]
    let main _ =
        demo1 ()
        demo2 ()
        demo3 ()
        demo4 ()
        demo5 ()
        demo6 ()
        demo7 ()
        demoStruct1 ()
        demoStruct2 ()
        demoStruct3 ()
        demoStruct4 ()
        demoStruct5 ()
        0
