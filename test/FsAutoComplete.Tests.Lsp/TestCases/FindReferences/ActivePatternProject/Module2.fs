namespace ActivePatternProject

/// Second module - uses patterns with qualified access
module Module2 =

    // Using patterns with fully qualified names
    let classifyWithQualified n =
        match n with
        | Patterns.Even -> "even"
        | Patterns.Odd -> "odd"

    // Using partial pattern with qualified name
    let parseWithQualified input =
        match input with
        | Patterns.ParseInt n -> Some n
        | _ -> None

    // Using the pattern as a function with qualified name
    let parseIntQualified input = Patterns.(|ParseInt|_|) input
    let evenOddQualified n = Patterns.(|Even|Odd|) n

    // Using Regex pattern
    let matchEmail input =
        match input with
        | Patterns.Regex @"[\w.]+@[\w.]+" email -> Some email
        | _ -> None

    // Using NonEmpty pattern
    let validateInput input =
        match input with
        | Patterns.NonEmpty s -> Ok s
        | _ -> Error "Input cannot be empty"

    // Complex example with multiple patterns
    let processInput input =
        match input with
        | Patterns.NonEmpty s ->
            match s with
            | Patterns.ParseInt n ->
                match n with
                | Patterns.Even -> "parsed even number"
                | Patterns.Odd -> "parsed odd number"
            | Patterns.ParseFloat f -> sprintf "parsed float: %f" f
            | _ -> "non-numeric string"
        | _ -> "empty input"

    // Using DivisibleBy with different parameters
    let checkDivisibility n =
        match n with
        | Patterns.DivisibleBy 2 _ -> "divisible by 2"
        | Patterns.DivisibleBy 3 _ -> "divisible by 3"
        | Patterns.DivisibleBy 5 _ -> "divisible by 5"
        | _ -> "not divisible by 2, 3, or 5"

    // ============================================
    // STRUCT PARTIAL ACTIVE PATTERNS (qualified access)
    // ============================================

    // Using struct partial pattern with qualified name
    let parseWithQualifiedStruct input =
        match input with
        | Patterns.ParseIntStruct n -> ValueSome n
        | _ -> ValueNone

    // Using struct pattern as a function with qualified name
    let parseIntStructQualified input = Patterns.(|ParseIntStruct|_|) input
    let parseFloatStructQualified input = Patterns.(|ParseFloatStruct|_|) input

    // Complex example with struct patterns
    let processInputStruct input =
        match input with
        | Patterns.NonEmptyStruct s ->
            match s with
            | Patterns.ParseIntStruct n ->
                match n with
                | Patterns.Even -> "parsed even number (struct)"
                | Patterns.Odd -> "parsed odd number (struct)"
            | Patterns.ParseFloatStruct f -> sprintf "parsed float (struct): %f" f
            | _ -> "non-numeric string"
        | _ -> "empty input"

    // Using struct DivisibleBy with different parameters
    let checkDivisibilityStruct n =
        match n with
        | Patterns.DivisibleByStruct 2 _ -> "divisible by 2 (struct)"
        | Patterns.DivisibleByStruct 3 _ -> "divisible by 3 (struct)"
        | Patterns.DivisibleByStruct 5 _ -> "divisible by 5 (struct)"
        | _ -> "not divisible by 2, 3, or 5"

    // ============================================
    // INLINE GENERIC ACTIVE PATTERNS (qualified access)
    // ============================================

    // Using IsOneOfChoice as a function with qualified access
    let checkPrefixQualified input =
        Patterns.(|IsOneOfChoice|_|) (Patterns.(|StrStartsWith|_|), ["hello"; "hi"]) input
//>               ^^^^^^^^^^^^^^^^^ IsOneOfChoice

    // Using StrStartsWithOneOf with qualified access
    let checkGreetingQualified input =
        match input with
        | Patterns.StrStartsWithOneOf ["hello"; "hi"; "hey"] -> "greeting"
        | _ -> "not a greeting"

    // Using StrStartsWith with qualified access
    let startsWithHelloQualified input =
        match input with
        | Patterns.StrStartsWith "hello" -> true
        | _ -> false
