namespace ActivePatternProject

module Seq =
    let inline tryPickV chooser (source: seq<'T>) =
        use e = source.GetEnumerator()
        let mutable res = ValueNone
        while (ValueOption.isNone res && e.MoveNext()) do
            res <- chooser e.Current
        res

/// Module containing various active pattern definitions
module Patterns =

    // ============================================
    // TOTAL/FULL ACTIVE PATTERNS
    // ============================================

    /// Total active pattern for even/odd classification
    let (|Even|Odd|) value =
//>       ^^^^ Even
        if value % 2 = 0 then Even else Odd

    /// Total active pattern for sign classification
    let (|Positive|Negative|Zero|) value =
        if value > 0 then Positive
        elif value < 0 then Negative
        else Zero

    // ============================================
    // PARTIAL ACTIVE PATTERNS
    // ============================================

    /// Partial active pattern for parsing integers
    let (|ParseInt|_|) (input: string) =
//>       ^^^^^^^^ ParseInt
        match System.Int32.TryParse input with
        | true, v -> Some v
        | false, _ -> None

    /// Partial active pattern for parsing floats
    let (|ParseFloat|_|) (input: string) =
        match System.Double.TryParse input with
        | true, v -> Some v
        | false, _ -> None

    /// Partial active pattern for non-empty strings
    let (|NonEmpty|_|) (input: string) =
        if System.String.IsNullOrWhiteSpace input then None
        else Some input

    // ============================================
    // PARAMETERIZED ACTIVE PATTERNS
    // ============================================

    /// Parameterized active pattern for divisibility
    let (|DivisibleBy|_|) divisor value =
//>       ^^^^^^^^^^^ DivisibleBy
        if value % divisor = 0 then Some(value / divisor)
        else None

    /// Parameterized active pattern for regex matching
    let (|Regex|_|) pattern input =
        let m = System.Text.RegularExpressions.Regex.Match(input, pattern)
        if m.Success then Some m.Value
        else None

    // ============================================
    // STRUCT PARTIAL ACTIVE PATTERNS (F# 7+)
    // These use ValueOption for better performance (no heap allocation)
    // ============================================

    /// Struct partial active pattern for parsing integers
    [<return: Struct>]
    let (|ParseIntStruct|_|) (input: string) =
//>       ^^^^^^^^^^^^^^ ParseIntStruct
        match System.Int32.TryParse input with
        | true, v -> ValueSome v
        | false, _ -> ValueNone

    /// Struct partial active pattern for parsing floats
    [<return: Struct>]
    let (|ParseFloatStruct|_|) (input: string) =
        match System.Double.TryParse input with
        | true, v -> ValueSome v
        | false, _ -> ValueNone

    /// Struct partial active pattern for non-empty strings
    [<return: Struct>]
    let (|NonEmptyStruct|_|) (input: string) =
        if System.String.IsNullOrWhiteSpace input then ValueNone
        else ValueSome input

    /// Struct parameterized active pattern for divisibility
    [<return: Struct>]
    let inline (|DivisibleByStruct|_|) divisor value =
        if value % divisor = 0 then ValueSome(value / divisor)
        else ValueNone


    [<return: Struct>]
    let inline (|IsOneOfChoice|_|) (chooser: 'a -> 'b -> 'c voption, values : 'a seq) (item : 'b) =
//>             ^^^^^^^^^^^^^^^ IsOneOfChoice
        values |> Seq.tryPickV (fun x -> chooser x item)

    [<return: Struct>]
    let inline (|StrStartsWith|_|) (value : string) (item : string) =
        if item.StartsWith value then ValueSome ()
        else ValueNone

    [<return: Struct>]
    let inline (|StrStartsWithOneOf|_|) (values : string seq) (item : string) =
//>              ^^^^^^^^^^^^^^^^^^ StrStartsWithOneOf
        (|IsOneOfChoice|_|) ((|StrStartsWith|_|), values) item
//>      ^^^^^^^^^^^^^^^ IsOneOfChoice
