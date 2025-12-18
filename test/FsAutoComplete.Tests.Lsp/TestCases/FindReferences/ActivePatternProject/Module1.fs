namespace ActivePatternProject

/// First module that uses patterns from Patterns module
module Module1 =
    open Patterns

    // Using total active pattern Even|Odd
    let classifyNumber n =
        match n with
        | Even -> "even"
        | Odd -> "odd"

    // Using total active pattern as function
    let getEvenOdd n = (|Even|Odd|) n

    // Using partial active pattern ParseInt
    let tryParseNumber input =
        match input with
        | ParseInt n -> Some n
        | _ -> None

    // Using partial active pattern as function
    let parseIntDirect input = (|ParseInt|_|) input

    // Using ParseFloat partial active pattern
    let tryParseFloat input =
        match input with
        | ParseFloat f -> Some f
        | _ -> None

    // Using ParseFloat as function
    let parseFloatDirect input = (|ParseFloat|_|) input

    // Using parameterized active pattern
    let isDivisibleBy3 n =
        match n with
        | DivisibleBy 3 result -> Some result
        | _ -> None

    // Using multiple patterns in one match
    let analyzeNumber n =
        match n with
        | Even & Positive -> "even positive"
        | Even & Negative -> "even negative"
        | Odd & Positive -> "odd positive"
        | Odd & Negative -> "odd negative"
        | Zero -> "zero"

    // Using Positive|Negative|Zero pattern
    let getSign n =
        match n with
        | Positive -> 1
        | Negative -> -1
        | Zero -> 0

    // ============================================
    // STRUCT PARTIAL ACTIVE PATTERNS
    // ============================================

    // Using struct partial active pattern ParseIntStruct
    let tryParseNumberStruct input =
        match input with
        | ParseIntStruct n -> ValueSome n
        | _ -> ValueNone

    // Using struct partial active pattern as function
    let parseIntStructDirect input = (|ParseIntStruct|_|) input

    // Using ParseFloatStruct partial active pattern
    let tryParseFloatStruct input =
        match input with
        | ParseFloatStruct f -> ValueSome f
        | _ -> ValueNone

    // Using ParseFloatStruct as function
    let parseFloatStructDirect input = (|ParseFloatStruct|_|) input

    // Using NonEmptyStruct partial active pattern
    let validateInputStruct input =
        match input with
        | NonEmptyStruct s -> ValueSome s
        | _ -> ValueNone

    // Using struct parameterized active pattern
    let isDivisibleBy3Struct n =
        match n with
        | DivisibleByStruct 3 result -> ValueSome result
        | _ -> ValueNone
