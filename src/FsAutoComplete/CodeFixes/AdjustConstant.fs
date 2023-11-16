module FsAutoComplete.CodeFix.AdjustConstant

open System
open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.Compiler.Syntax
open System.Runtime.CompilerServices
open FSharp.Compiler.Text.Range
open Microsoft.FSharp.Core.LanguagePrimitives

/// If `true`: enable `debugFix`es which show parsed `XXXConstant`s.
///
/// Note: As constant, because F# doesn't have `#define`
[<Literal>]
let private DEBUG = false

let inline private unreachable () = invalidOp "unreachable"

/// Returns `SynConst` and its range at passed `pos`
///
/// Note:
/// When `SynConst.Measure`:
/// * returns contained constant when `pos` inside contained constant
/// * otherwise `SynConst.Measure` when on other parts of `Measure` constant (`<km>`)
///
/// Note:
/// Might be erroneous Constant -> containing `value` is then default (`0`).
/// Check by comparing returned range with existing Diagnostics.
let private tryFindConstant ast pos =
  let rec findConst range constant =
    match constant with
    | SynConst.Measure(constant, constantRange, _) when rangeContainsPos constantRange pos ->
      findConst constantRange constant
    | _ -> (range, constant)

  SyntaxTraversal.Traverse(
    pos,
    ast,
    { new SyntaxVisitorBase<_>() with
        member _.VisitExpr(_, _, defaultTraverse, expr) =
          match expr with
          //                                     without: matches when `pos` in comment after constant
          | SynExpr.Const(constant, range) when rangeContainsPos range pos -> findConst range constant |> Some
          | _ -> defaultTraverse expr

        member _.VisitEnumDefn(_, cases, _) =
          cases
          |> List.tryPick (fun (SynEnumCase(valueExpr = expr)) ->
            let rec tryFindConst (expr: SynExpr) =
              match expr with
              | SynExpr.Const(constant, range) when rangeContainsPos range pos -> findConst range constant |> Some
              | SynExpr.Paren(expr = expr) -> tryFindConst expr
              | SynExpr.App(funcExpr = funcExpr) when rangeContainsPos funcExpr.Range pos -> tryFindConst funcExpr
              | SynExpr.App(argExpr = argExpr) when rangeContainsPos argExpr.Range pos -> tryFindConst argExpr
              | _ -> None

            tryFindConst expr)

        member _.VisitPat(_, defaultTraverse, synPat) =
          match synPat with
          | SynPat.Const(constant, range) when rangeContainsPos range pos -> findConst range constant |> Some
          | _ -> defaultTraverse synPat }
  )

/// Computes the absolute of `n`
///
/// Unlike `abs` or `Math.Abs` this here handles `MinValue` and does not throw `OverflowException`.
type private Int =
  static member inline abs(n: sbyte) : byte = if n >= 0y then byte n else byte (0y - n)

  static member inline abs(n: int16) : uint16 = if n >= 0s then uint16 n else uint16 (0s - n)

  static member inline abs(n: int32) : uint32 = if n >= 0l then uint32 n else uint32 (0l - n)

  static member inline abs(n: int64) : uint64 =
    if n >= 0L then
      uint64 n
    else
      // unchecked subtraction -> wrapping/modular negation
      // -> Negates all numbers -- with the exception of `Int64.MinValue`:
      //        `0L - Int64.MinValue = Int64.MinValue`
      //        BUT: converting `Int64.MinValue` to `UInt64` produces correct absolute of `Int64.MinValue`
      uint64 (0L - n)

  static member inline abs(n: nativeint) : unativeint = if n >= 0n then unativeint n else unativeint (0n - n)

type private Offset = int

/// Range inside a **single** line inside a source text.
///
/// Invariant: `Start.Line = End.Line` (-> `Range.inSingleLine`)
type private RangeInLine = Range

module private Range =
  let inline inSingleLine (range: Range) = range.Start.Line = range.End.Line

type private Range with

  member inline range.Length = range.End.Character - range.Start.Character

  member inline range.SpanIn(text: ReadOnlySpan<char>) =
    assert (Range.inSingleLine range)
    text.Slice(range.Start.Character, range.Length)

/// Range inside some element list. Range is specified via Offsets inside that list.
/// In Practice: Range inside `RangeInLine`
///
/// Similar to `System.Range` -- except it doesn't support indexing from the end.
/// -> Some operations are easier to use (`Length` because it doesn't require length of container)
///
/// Unlike `LSP.Range`: just Offsets, not Positions (Line & Character)
[<IsReadOnly; Struct>]
[<StructuredFormatDisplay("{DisplayText}")>]
type private ORange =
  { Start: Offset
    End: Offset }

  member r.DisplayText = r.ToString()
  override r.ToString() = $"{r.Start}..{r.End}"

  member inline r.Length = r.End - r.Start
  member inline r.IsEmpty = r.Start = r.End

  member inline r.ToRangeFrom(pos: Position) : Range =
    { Start =
        { Line = pos.Line
          Character = pos.Character + r.Start }
      End =
        { Line = pos.Line
          Character = pos.Character + r.End } }

  member inline r.ToRangeInside(range: Range) : Range =
    assert (Range.inSingleLine range)
    assert (r.Length <= range.Length)
    r.ToRangeFrom(range.Start)

  member inline r.ShiftBy(d: Offset) = { Start = r.Start + d; End = r.End + d }
  /// Note: doesn't care about `Line`, only `Character`
  member inline private r.ShiftToStartOf(pos: Position) : ORange = r.ShiftBy(pos.Character)

  member inline private r.ShiftInside(range: Range) : ORange =
    assert (Range.inSingleLine range)
    assert (r.Length <= range.Length)
    r.ShiftToStartOf(range.Start)

  member inline r.SpanIn(str: String) = str.AsSpan(r.Start, r.Length)
  member inline r.SpanIn(s: ReadOnlySpan<_>) = s.Slice(r.Start, r.Length)

  member inline r.SpanIn(parent: Range, s: ReadOnlySpan<_>) = r.ShiftInside(parent).SpanIn(s)

  member inline r.SpanIn(parent: Range, s: String) = r.ShiftInside(parent).SpanIn(s)

  member inline r.EmptyAtStart = { Start = r.Start; End = r.Start }
  member inline r.EmptyAtEnd = { Start = r.End; End = r.End }

  /// Assumes: `range` is inside single line
  static member inline CoverAllOf(range: Range) =
    assert (Range.inSingleLine range)
    { Start = 0; End = range.Length }

  static member inline CoverAllOf(text: ReadOnlySpan<_>) = { Start = 0; End = text.Length }

module private ORange =
  /// Returns range that contains `range1` as well as `range2` with their extrema as border.
  ///
  /// Note: if there's a gap between `range1` and `range2` that gap is included in output range:
  ///       `union (1..3) (7..9) = 1..9`
  let inline union (range1: ORange) (range2: ORange) =
    { Start = min range1.Start range2.Start
      End = max range1.End range2.End }

  /// Split `range` after `length` counting from the front.
  ///
  /// Example:
  /// ```fsharp
  /// let range = { Start = 0; End = 10 }
  /// let (left, right) = range |> ORange.splitFront 4
  /// assert(left = { Start = 0; End = 4 })
  /// assert(right = { Start = 4; End = 10 })
  /// ```
  ///
  /// Note: Tuple instead of `ValueTuple` (`struct`) for better inlining.
  ///       Check when used: Tuple should not actually be created!
  let inline splitFront length (range: ORange) =
    ({ range with
        End = range.Start + length },
     { range with
         Start = range.Start + length })

  /// Split `range` after `length` counting from the back.
  ///
  /// Example:
  /// ```fsharp
  /// let range = { Start = 0; End = 10 }
  /// let (left, right) = range |> ORange.splitAfter 4
  /// assert(left = { Start = 0; End = 6 })
  /// assert(right = { Start = 6; End = 10 })
  /// ```
  let inline splitBack length (range: ORange) =
    ({ range with End = range.End - length },
     { range with
         Start = range.End - length })

  /// Adjusts `Start` by `+ dStart`
  let inline adjustStart dStart (range: ORange) =
    { range with
        Start = range.Start + dStart }

  /// Adjusts `End` by `- dEnd`
  let inline adjustEnd dEnd (range: ORange) = { range with End = range.End - dEnd }

  /// Adjusts `Start` by `+ dStart` and `End` by `- dEnd`
  let inline adjust (dStart, dEnd) (range: ORange) =
    { Start = range.Start + dStart
      End = range.End - dEnd }

[<Extension>]
type private Extensions() =
  /// Returns `-1` if no matching element
  [<Extension>]
  static member inline TryFindIndex(span: ReadOnlySpan<_>, [<InlineIfLambda>] f) =
    let mutable idx = -1
    let mutable i = 0

    while idx < 0 && i < span.Length do
      if f (span[i]) then idx <- i else i <- i + 1

    idx

  [<Extension>]
  static member inline Count(span: ReadOnlySpan<_>, [<InlineIfLambda>] f) =
    let mutable count = 0

    for c in span do
      if f c then
        count <- count + 1

    count

module private Parse =
  /// Note: LHS does not include position with `f(char) = true`, but instead is first on RHS
  let inline until (text: ReadOnlySpan<char>, range: ORange, [<InlineIfLambda>] f) =
    let text = range.SpanIn text
    let i = text.TryFindIndex(f)

    if i < 0 then
      range, range.EmptyAtEnd
    else
      range |> ORange.splitFront i

  let inline while' (text: ReadOnlySpan<char>, range: ORange, [<InlineIfLambda>] f) =
    until (text, range, (fun c -> not (f c)))

  let inline if' (text: ReadOnlySpan<char>, range: ORange, [<InlineIfLambda>] f) =
    let text = range.SpanIn text

    if text.IsEmpty then range.EmptyAtStart, range
    elif f text[0] then range |> ORange.splitFront 1
    else range.EmptyAtStart, range

/// Helper functions to splat tuples. With inlining: prevent tuple creation
module private Tuple =
  let inline splatR value (a, b) = (value, a, b)
  let inline splatL (a, b) value = (a, b, value)

module private Char =
  let inline isDigitOrUnderscore c = Char.IsDigit c || c = '_'

  let inline isHexDigitOrUnderscore c = isDigitOrUnderscore c || ('a' <= c && c <= 'f') || ('A' <= c && c <= 'F')

  let inline isSingleQuote c = c = '\''

[<RequireQualifiedAccess>]
type CharFormat =
  /// `ç`
  | Char
  /// `\231`
  | Decimal
  /// `\xE7`
  | Hexadecimal
  /// `\u00E7`
  | Utf16Hexadecimal
  /// `\U000000E7`
  | Utf32Hexadecimal

type private CharConstant =
  {
    Range: Range

    Value: char
    Format: CharFormat
    Constant: SynConst
    ValueRange: ORange

    /// `B` suffix
    /// Only when Byte
    SuffixRange: ORange
  }

  member c.IsByte = not c.SuffixRange.IsEmpty

module private CharConstant =
  let inline isAsciiByte (text: ReadOnlySpan<char>) = text.EndsWith "'B"

  /// `'a'`, `'\n'`, `'\231'`, `'\xE7'`, `'\u00E7'`, `'\U000000E7'`
  ///
  /// Can have `B` suffix (-> byte, otherwise normal char)
  let parse (lineStr: ReadOnlySpan<char>, constRange: RangeInLine, constant: SynConst, value: char) =
    let text = constRange.SpanIn(lineStr)
    let range = ORange.CoverAllOf constRange

    assert (text[0] = '\'')

    let suffixLength =
      if text.EndsWith "B" then
        assert (text.EndsWith "'B")
        1
      else
        assert (text.EndsWith "'")
        0

    let valueRange = ORange.adjust (1, 1 + suffixLength) range
    let suffixRange = ORange.adjustStart (-suffixLength) range.EmptyAtEnd

    let format =
      let valueStr = valueRange.SpanIn(text)

      if valueStr.Length > 2 && valueStr[0] = '\\' then
        match valueStr[1] with
        | 'x' -> CharFormat.Hexadecimal
        | 'u' -> CharFormat.Utf16Hexadecimal
        | 'U' -> CharFormat.Utf32Hexadecimal
        | c when Char.IsDigit c -> CharFormat.Decimal
        | _ -> CharFormat.Char
      else
        CharFormat.Char

    { Range = constRange
      Value = value
      Format = format
      Constant = constant
      ValueRange = valueRange
      SuffixRange = suffixRange }

type private Sign =
  | Negative
  | Positive

module private Sign =
  /// Returns `Positive` in case of no sign
  let inline parse (text: ReadOnlySpan<char>, range: ORange) =
    let text = range.SpanIn text

    if text.IsEmpty then
      Positive, range.EmptyAtStart, range
    elif text[0] = '-' then
      Tuple.splatR Negative (range |> ORange.splitFront 1)
    elif text[0] = '+' then
      Tuple.splatR Positive (range |> ORange.splitFront 1)
    else
      Positive, range.EmptyAtStart, range

[<RequireQualifiedAccess>]
type Base =
  /// No prefix
  | Decimal
  /// `0x`
  | Hexadecimal
  /// `0o`
  | Octal
  /// `0b`
  | Binary

module private Base =
  /// Returns `Decimal` in case of no base
  let inline parse (text: ReadOnlySpan<char>, range: ORange) =
    let text = range.SpanIn(text)

    if text.Length > 2 && text[0] = '0' then
      match text[1] with
      | 'x'
      | 'X' -> Tuple.splatR Base.Hexadecimal (range |> ORange.splitFront 2)
      | 'o'
      | 'O' -> Tuple.splatR Base.Octal (range |> ORange.splitFront 2)
      | 'b'
      | 'B' -> Tuple.splatR Base.Binary (range |> ORange.splitFront 2)
      | _ -> Base.Decimal, range.EmptyAtStart, range
    else
      Base.Decimal, range.EmptyAtStart, range

/// Int Constant (without ASCII byte form)
/// or Float Constant in Hex/Oct/Bin form
/// or `UserNum` Constant (`bigint`) (always Dec form)
///
/// * optional sign: `+` `-`
/// * optional base: `0` + [`x` `X` `o` `O` `b` `B`]
/// * required digits
///   * optional underscores inside
/// * optional suffix
type private IntConstant =
  { Range: Range

    Sign: Sign
    SignRange: ORange

    Base: Base
    BaseRange: ORange

    Constant: SynConst
    ValueRange: ORange

    SuffixRange: ORange }

module private IntConstant =
  /// Note: Does not handle ASCII byte. Check with `CharConstant.isAsciiByte` and then parse with `CharConstant.parse`
  let parse (lineStr: ReadOnlySpan<char>, constRange: RangeInLine, constant: SynConst) =
    let text = constRange.SpanIn(lineStr)
    assert (not (CharConstant.isAsciiByte text))

    let range = ORange.CoverAllOf(text)
    let sign, signRange, range = Sign.parse (text, range)
    let base', baseRange, range = Base.parse (text, range)

    let valueRange, suffixRange =
      Parse.while' (text, range, Char.isHexDigitOrUnderscore)

    { Range = constRange
      Sign = sign
      SignRange = signRange
      Base = base'
      BaseRange = baseRange
      Constant = constant
      ValueRange = valueRange
      SuffixRange = suffixRange }

[<RequireQualifiedAccess>]
type private FloatValue =
  | Float of float
  | Float32 of float32
  | Decimal of decimal

  static member from(f: float) = FloatValue.Float f
  static member from(f: float32) = FloatValue.Float32 f
  static member from(d: decimal) = FloatValue.Decimal d

/// Float Constant (without Hex/Oct/Bin form -- just Decimal & Scientific)
///
/// Includes `float32`, `float`, `decimal`
type private FloatConstant =
  {
    Range: Range

    /// Note: Leading sign, not exponent sign
    Sign: Sign
    SignRange: ORange

    Constant: SynConst
    Value: FloatValue
    /// Part before decimal separator (`.`)
    ///
    /// Note: Cannot be empty
    IntRange: ORange
    /// Part after decimal separator (`.`)
    ///
    /// Note: empty when no decimal
    DecimalRange: ORange
    /// Exponent Part without `e` or sign
    ///
    /// Note: empty when no exponent
    ExponentRange: ORange

    SuffixRange: ORange
  }

  member c.IsScientific = not c.ExponentRange.IsEmpty
  member c.ValueRange = ORange.union c.IntRange c.ExponentRange

module private FloatConstant =
  let inline isIntFloat (text: ReadOnlySpan<char>) = text.EndsWith "lf" || text.EndsWith "LF"

  /// Note: Does not handle Hex/Oct/Bin form (`lf` or `LF` suffix). Check with `FloatConstant.isIntFloat` and then parse with `IntConstant.parse`
  let parse (lineStr: ReadOnlySpan<char>, constRange: RangeInLine, constant: SynConst, value: FloatValue) =
    let text = constRange.SpanIn(lineStr)
    assert (not (isIntFloat text))

    let range = ORange.CoverAllOf(text)
    let sign, signRange, range = Sign.parse (text, range)
    let intRange, range = Parse.while' (text, range, Char.isDigitOrUnderscore)

    let decimalRange, range =
      let sepRange, range = Parse.if' (text, range, (fun c -> c = '.'))

      if sepRange.IsEmpty then
        range.EmptyAtStart, range
      else
        Parse.while' (text, range, Char.isDigitOrUnderscore)

    let exponentRange, suffixRange =
      let eRange, range = Parse.if' (text, range, (fun c -> c = 'e' || c = 'E'))

      if eRange.IsEmpty then
        range.EmptyAtStart, range
      else
        let _, _, range = Sign.parse (text, range)
        Parse.while' (text, range, Char.isDigitOrUnderscore)

    { Range = constRange
      Sign = sign
      SignRange = signRange
      Constant = constant
      Value = value
      IntRange = intRange
      DecimalRange = decimalRange
      ExponentRange = exponentRange
      SuffixRange = suffixRange }

// Titles in extra modules (instead with their corresponding fix)
// to exposed titles to Unit Tests while keeping fixes private.
module Title =
  let removeDigitSeparators = "Remove group separators"
  let replaceWith = sprintf "Replace with `%s`"

  module Int =
    module Convert =
      let toDecimal = "Convert to decimal"
      let toHexadecimal = "Convert to hexadecimal"
      let toOctal = "Convert to octal"
      let toBinary = "Convert to binary"

      module SpecialCase =
        /// `0b1111_1101y = -3y = -0b0000_0011y`
        let extractMinusFromNegativeConstant = "Extract `-` (constant is negative)"
        /// `-0b0000_0011y = -3y = 0b1111_1101y`
        let integrateExplicitMinus = "Integrate `-` into constant"

        /// `-0b1111_1101y = -(-3y) = 3y = 0b0000_0011y`
        let useImplicitPlusInPositiveConstantWithMinusSign =
          "Use implicit `+` (constant is positive)"

        /// `-0b1000_0000y = -(-128y) = -128y = 0b1000_0000y`
        /// -> Negative values have one more value than positive ones! -> `-MinValue = MinValue`
        let removeExplicitMinusWithMinValue = "Remove adverse `-` (`-MinValue = MinValue`)"

    module Separate =
      let decimal3 = "Separate thousands (3)"
      let hexadecimal4 = "Separate words (4)"
      let hexadecimal2 = "Separate bytes (2)"
      let octal3 = "Separate digit groups (3)"
      let binary4 = "Separate nibbles (4)"
      let binary8 = "Separate bytes (8)"

  module Float =
    module Separate =
      let all3 = "Separate digit groups (3)"

  module Char =
    module Convert =
      let toChar = sprintf "Convert to `%s`"
      let toDecimal = sprintf "Convert to `%s`"
      let toHexadecimal = sprintf "Convert to `%s`"
      let toUtf16Hexadecimal = sprintf "Convert to `%s`"
      let toUtf32Hexadecimal = sprintf "Convert to `%s`"

let inline private mkFix doc title edits =
  { Title = title
    File = doc
    Edits = edits
    Kind = FixKind.Refactor
    SourceDiagnostic = None }


module private DigitGroup =
  let removeFix (doc: TextDocumentIdentifier) (lineStr: String) (constantRange: Range) (localRange: ORange) =
    let text = localRange.SpanIn(constantRange, lineStr)

    if text.Contains '_' then
      let replacement = text.ToString().Replace("_", "")

      mkFix
        doc
        Title.removeDigitSeparators
        [| { Range = localRange.ToRangeInside constantRange
             NewText = replacement } |]
      |> List.singleton
    else
      []

  type Direction =
    /// thousands -> left of `.`
    | RightToLeft
    /// thousandth -> right of `.`
    | LeftToRight

  let addSeparator (n: String) (groupSize: int) (dir: Direction) =
    let mutable res = n.ToString()

    match dir with
    | RightToLeft ->
      // counting in reverse (from last to first)
      // starting at `1` and not `0`: never insert in last position
      for i in 1 .. (n.Length - 1) do
        if i % groupSize = 0 then
          res <- res.Insert(n.Length - i, "_")
    | LeftToRight ->
      // grouping from first to last
      // but insert must happen last to first (because insert at index)
      for i = (n.Length - 1) downto 1 do
        if i % groupSize = 0 then
          res <- res.Insert(i, "_")

    res

module private Format =
  module Char =
    /// Returns `None` for "invisible" chars (`Char.IsControl`)
    /// -- with the exception of some chars that can be represented via escape sequence
    ///
    /// See: [F# Reference](https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/strings#remarks)
    let tryAsChar (c: char) =
      match c with
      | '\a' -> Some "\\a"
      | '\b' -> Some "\\b"
      | '\f' -> Some "\\f"
      | '\n' -> Some "\\n"
      | '\r' -> Some "\\r"
      | '\t' -> Some "\\t"
      | '\v' -> Some "\\v"
      | '\\' -> Some "\\"
      // Note: double quotation marks can be escaped -- but don't have to be.
      //       We're emitting unescaped quotations: `'"'` and not `'\"'`
      | '\"' -> Some "\""
      | '\'' -> Some "\\\'"
      | _ when Char.IsControl c -> None
      | c -> Some(string c)

    let inline asChar (c: char) = tryAsChar c |> Option.defaultValue (string c)

    let inline asDecimal (c: char) = $"\\%03i{uint16 c}"
    let inline asHexadecimal (c: char) = $"\\x%02X{uint16 c}"
    let inline asUtf16Hexadecimal (c: char) = $"\\u%04X{uint16 c}"
    let inline asUtf32Hexadecimal (c: char) = $"\\U%08X{uint c}"

  module Int =
    let inline asDecimalUnsigned n = $"%u{n}"
    let inline asDecimalSigned n = $"%i{n}"
    /// Unsigned: no explicit `-` sign,
    ///           but sign gets directly encoding in hex representation (1st bit)
    let inline asHexadecimalUnsigned n = $"0x%X{n}"

    /// Signed: explicit `-` sign when negative and sign bit `0`
    ///         -> when negative: `-abs(n)`
    let inline asHexadecimalSigned (n, abs) =
      if n >= GenericZero then
        asHexadecimalUnsigned n
      else
        let absValue = abs n
        $"-0x%X{absValue}"

    let inline asOctalUnsigned n = $"0o%o{n}"

    let inline asOctalSigned (n, abs) =
      if n >= GenericZero then
        asHexadecimalUnsigned n
      else
        let absValue = abs n
        $"-0o%o{absValue}"

    let inline asBinaryUnsigned n = $"0b%B{n}"

    let inline asBinarySigned (n, abs) =
      if n >= GenericZero then
        asBinaryUnsigned n
      else
        let absValue = abs n
        $"-0b%B{absValue}"

module private CommonFixes =
  open FSharp.Compiler.Symbols

  /// Adding a sign might lead to invalid code:
  /// ```fsharp
  /// let value = 5y+0b1010_0101y
  ///
  /// // => Convert to decimal
  ///
  /// // without space:
  /// let value = 5y+-91y
  /// //            ^^
  /// //            The type 'sbyte' does not support the operator '+-'
  ///
  /// // with space:
  /// let value = 5y+ -91y
  /// ```
  ///
  /// -> Prepend space if leading sign in `replacement` and operator char immediately in front (in `lineStr`)
  let prependSpaceIfNecessary (range: Range) (lineStr: string) (replacement: string) =
    if
      (replacement.StartsWith("-", StringComparison.Ordinal)
       || replacement.StartsWith("+", StringComparison.Ordinal))
      && range.Start.Character > 0
      && "!$%&*+-./<=>?@^|~".Contains(lineStr[range.Start.Character - 1])
    then
      " " + replacement
    else
      replacement

  /// Returns:
  /// * `None`: unhandled `SynConst`
  /// * `Some`:
  ///   * Simple Name of Constant Type: `SynConst.Double _` -> `Double`
  ///   * `FSharpType` matching `constant` type
  ///     * Note: `None` if cannot find corresponding Entity/Type. Most likely an error inside this function!
  let tryGetFSharpType (parseAndCheck: ParseAndCheckResults) (constant: SynConst) =
    option {
      //Enhancement: cache? Must be by project. How to detect changes?

      let! name =
        match constant with
        | SynConst.Bool _ -> Some <| nameof (System.Boolean)
        | SynConst.Char _ -> Some <| nameof (System.Char)
        | SynConst.Byte _ -> Some <| nameof (System.Byte)
        | SynConst.SByte _ -> Some <| nameof (System.SByte)
        | SynConst.Int16 _ -> Some <| nameof (System.Int16)
        | SynConst.UInt16 _ -> Some <| nameof (System.UInt16)
        | SynConst.Int32 _ -> Some <| nameof (System.Int32)
        | SynConst.UInt32 _ -> Some <| nameof (System.UInt32)
        | SynConst.Int64 _ -> Some <| nameof (System.Int64)
        | SynConst.UInt64 _ -> Some <| nameof (System.UInt64)
        | SynConst.IntPtr _ -> Some <| nameof (System.IntPtr)
        | SynConst.UIntPtr _ -> Some <| nameof (System.UIntPtr)
        | SynConst.Single _ -> Some <| nameof (System.Single)
        | SynConst.Double _ -> Some <| nameof (System.Double)
        | SynConst.Decimal _ -> Some <| nameof (System.Decimal)
        | _ -> None

      let isSystemAssembly (assembly: FSharpAssembly) =
        match assembly.SimpleName with
        // dotnet core
        | "System.Runtime"
        // .net framework
        | "mscorlib"
        // .net standard
        | "netstandard" -> true
        | _ -> false

      let assemblies =
        parseAndCheck.GetCheckResults.ProjectContext.GetReferencedAssemblies()

      let ty =
        assemblies
        |> Seq.filter (isSystemAssembly)
        |> Seq.tryPick (fun system -> system.Contents.FindEntityByPath [ "System"; name ])
        |> Option.map (fun ent -> ent.AsType())

      // Note: `ty` should never be `None`: we're only looking up standard dotnet types -- which should always be available.
      //       But `isSystemAssembly` might not handle all possible assemblies with default types -> keep it safe and return `option`

      return (name, ty)
    }

  /// Fix that replaces `constantRange` with `propertyName` on type of `constant`.
  ///
  /// Example:
  /// `constant = SynConst.Double _` and `fieldName = "MinValue"`
  /// -> replaces `constantRange` with `Double.MinValue`
  ///
  /// Tries to detect if leading `System.` is necessary (`System` is not `open`).
  /// If cannot detect: Puts `System.` in front
  let replaceWithNamedConstantFix
    doc
    (pos: FcsPos)
    (parseAndCheck: ParseAndCheckResults)
    (constant: SynConst)
    (constantRange: Range)
    (fieldName: string)
    (mkTitle: string -> string)
    =
    option {
      let! (tyName, ty) = tryGetFSharpType parseAndCheck constant

      let propCall =
        ty
        |> Option.bind (fun ty ->
          parseAndCheck.GetCheckResults.GetDisplayContextForPos pos
          |> Option.map (fun displayContext -> $"{ty.Format displayContext}.{fieldName}"))
        |> Option.defaultWith (fun _ -> $"System.{tyName}.{fieldName}")

      let title = mkTitle $"{tyName}.{fieldName}"

      let edits =
        [| { Range = constantRange
             NewText = propCall } |]

      return mkFix doc title edits |> List.singleton
    }
    |> Option.defaultValue []


  /// Replaces float with `infinity` etc.
  let replaceFloatWithNameFix
    doc
    (pos: FcsPos)
    (lineStr: String)
    (parseAndCheck: ParseAndCheckResults)
    (constant: SynConst)
    (constantRange: Range)
    (constantValue: FloatValue)
    =
    let mkFix value =
      let title = Title.replaceWith value
      let replacement = prependSpaceIfNecessary constantRange lineStr value

      let edits =
        [| { Range = constantRange
             NewText = replacement } |]

      mkFix doc title edits |> List.singleton

    match constantValue with
    | FloatValue.Float value ->
      if Double.IsPositiveInfinity value then
        mkFix "infinity"
      elif Double.IsNegativeInfinity value then
        mkFix "-infinity"
      elif Double.IsNaN value then
        mkFix "nan"
      elif value = System.Double.MaxValue then
        replaceWithNamedConstantFix
          doc
          pos
          parseAndCheck
          constant
          constantRange
          (nameof (Double.MaxValue))
          Title.replaceWith
      elif value = System.Double.MinValue then
        replaceWithNamedConstantFix
          doc
          pos
          parseAndCheck
          constant
          constantRange
          (nameof (Double.MinValue))
          Title.replaceWith
      elif value = System.Double.Epsilon then
        replaceWithNamedConstantFix
          doc
          pos
          parseAndCheck
          constant
          constantRange
          (nameof (Double.Epsilon))
          Title.replaceWith
      else
        []
    | FloatValue.Float32 value ->
      if Single.IsPositiveInfinity value then
        mkFix "infinityf"
      elif Single.IsNegativeInfinity value then
        mkFix "-infinityf"
      elif Single.IsNaN value then
        mkFix "nanf"
      elif value = System.Single.MaxValue then
        replaceWithNamedConstantFix
          doc
          pos
          parseAndCheck
          constant
          constantRange
          (nameof (Single.MaxValue))
          Title.replaceWith
      elif value = System.Single.MinValue then
        replaceWithNamedConstantFix
          doc
          pos
          parseAndCheck
          constant
          constantRange
          (nameof (Single.MinValue))
          Title.replaceWith
      elif value = System.Single.Epsilon then
        replaceWithNamedConstantFix
          doc
          pos
          parseAndCheck
          constant
          constantRange
          (nameof (Single.Epsilon))
          Title.replaceWith
      else
        []
    | FloatValue.Decimal value ->
      if value = System.Decimal.MaxValue then
        replaceWithNamedConstantFix
          doc
          pos
          parseAndCheck
          constant
          constantRange
          (nameof (Decimal.MaxValue))
          Title.replaceWith
      elif value = System.Decimal.MinValue then
        replaceWithNamedConstantFix
          doc
          pos
          parseAndCheck
          constant
          constantRange
          (nameof (Decimal.MinValue))
          Title.replaceWith
      else
        []

module private CharFix =
  let private debugFix doc (lineStr: String) (constant: CharConstant) =
    let data =
      let full = constant.Range.SpanIn(lineStr).ToString()
      let value = constant.ValueRange.SpanIn(full).ToString()
      let suffix = constant.SuffixRange.SpanIn(full).ToString()

      let c =
        constant.Value
        |> Format.Char.tryAsChar
        |> Option.defaultWith (fun _ -> Format.Char.asUtf16Hexadecimal constant.Value)

      $"%A{value} (%A{constant.Format}, %A{c}) %A{suffix} (%A{full}, %A{constant})"

    mkFix doc data [||]

  let convertToOtherFormatFixes doc (constant: CharConstant) =
    [ let mkFix' title replacement =
        let edits =
          [| { Range = constant.ValueRange.ToRangeInside constant.Range
               NewText = replacement } |]

        mkFix doc title edits

      if constant.Format <> CharFormat.Char then
        match Format.Char.tryAsChar constant.Value with
        | None -> () // Don't convert to "invisible" char
        | Some value -> mkFix' (Title.Char.Convert.toChar value) value
      // `\x` & `\U` currently not supported for byte char
      // TODO: allow byte once support was added
      if constant.Format <> CharFormat.Decimal && int constant.Value <= 255 then
        let value = Format.Char.asDecimal constant.Value
        mkFix' (Title.Char.Convert.toDecimal value) value

      if
        not constant.IsByte
        && constant.Format <> CharFormat.Hexadecimal
        && int constant.Value <= 0xFF
      then
        let value = Format.Char.asHexadecimal constant.Value
        mkFix' (Title.Char.Convert.toHexadecimal value) value

      if constant.Format <> CharFormat.Utf16Hexadecimal then
        let value = Format.Char.asUtf16Hexadecimal constant.Value
        mkFix' (Title.Char.Convert.toUtf16Hexadecimal value) value

      if not constant.IsByte && constant.Format <> CharFormat.Utf32Hexadecimal then
        let value = Format.Char.asUtf32Hexadecimal constant.Value
        mkFix' (Title.Char.Convert.toUtf32Hexadecimal value) value

      if constant.IsByte then
        // convert to int representation
        let mkFix' title replacement =
          let edits =
            [| { Range = constant.Range
                 NewText = replacement + "uy" } |]

          mkFix doc title edits

        let value = byte constant.Value
        mkFix' Title.Int.Convert.toDecimal (Format.Int.asDecimalUnsigned value)
        mkFix' Title.Int.Convert.toHexadecimal (Format.Int.asHexadecimalUnsigned value)
        mkFix' Title.Int.Convert.toOctal (Format.Int.asOctalUnsigned value)
        mkFix' Title.Int.Convert.toBinary (Format.Int.asBinaryUnsigned value) ]

  let all doc (lineStr: String) (error: bool) (constant: CharConstant) =
    [ if not error then
        yield! convertToOtherFormatFixes doc constant

      if DEBUG then
        debugFix doc lineStr constant ]

module private IntFix =
  let private debugFix doc (lineStr: String) (constant: IntConstant) =
    let data =
      let full = constant.Range.SpanIn(lineStr).ToString()

      let value = constant.ValueRange.SpanIn(full).ToString()
      let suffix = constant.SuffixRange.SpanIn(full).ToString()
      $"%A{constant.Sign} %A{constant.Base} %A{value} %A{suffix} (%A{constant.Constant}) (%A{full}, %A{constant})"

    mkFix doc data [||]

  let convertToOtherBaseFixes doc (lineStr: string) (constant: IntConstant) =
    let mkFixKeepExistingSign title replacement =
      let range = ORange.union constant.BaseRange constant.ValueRange

      let edits =
        [| { Range = range.ToRangeInside constant.Range
             NewText = replacement } |]

      mkFix doc title edits

    let mkFixReplaceExistingSign title (replacement: string) =
      let localRange = ORange.union constant.SignRange constant.ValueRange
      let range = localRange.ToRangeInside constant.Range
      let replacement = CommonFixes.prependSpaceIfNecessary range lineStr replacement
      let edits = [| { Range = range; NewText = replacement } |]
      mkFix doc title edits

    let inline mkIntFixes (value: 'int, abs: 'int -> 'uint, minValue: 'int) =
      [ if constant.Base = Base.Decimal then
          // easy case: no special cases: `-` is always explicit, value always matches explicit sign
          // -> just convert absolute value and keep existing sign

          // but obviously there are no easy cases...:
          // special case: MinValue: `-128y = -0b1000_000y = 0b1000_0000y`
          //    -> technical `-0b1000_0000y` is correct -- but misleading (`-` AND negative bit) -> remove `-`
          if value = minValue then
            mkFixReplaceExistingSign Title.Int.Convert.toHexadecimal (Format.Int.asHexadecimalUnsigned value)
            mkFixReplaceExistingSign Title.Int.Convert.toOctal (Format.Int.asOctalUnsigned value)
            mkFixReplaceExistingSign Title.Int.Convert.toBinary (Format.Int.asBinaryUnsigned value)
          else
            let absValue = abs value
            mkFixKeepExistingSign Title.Int.Convert.toHexadecimal (Format.Int.asHexadecimalUnsigned absValue)
            mkFixKeepExistingSign Title.Int.Convert.toOctal (Format.Int.asOctalUnsigned absValue)
            mkFixKeepExistingSign Title.Int.Convert.toBinary (Format.Int.asBinaryUnsigned absValue)

        elif value = GenericZero || (value > GenericZero && constant.Sign = Positive) then
          // easy case: implicit or explicit `+` sign matches value
          // -> just convert absolute value and keep existing sign
          // additional special case handled here: keep `-` for exactly `0`
          let absValue =
            assert (value >= GenericZero)
            value

          if
            (assert (constant.Base <> Base.Decimal)
             true)
          then
            mkFixKeepExistingSign Title.Int.Convert.toDecimal (Format.Int.asDecimalUnsigned absValue)

          if constant.Base <> Base.Hexadecimal then
            mkFixKeepExistingSign Title.Int.Convert.toHexadecimal (Format.Int.asHexadecimalUnsigned absValue)

          if constant.Base <> Base.Octal then
            mkFixKeepExistingSign Title.Int.Convert.toOctal (Format.Int.asOctalUnsigned absValue)

          if constant.Base <> Base.Binary then
            mkFixKeepExistingSign Title.Int.Convert.toBinary (Format.Int.asBinaryUnsigned absValue)

        elif value > GenericZero && constant.Sign = Negative then
          // explicit `-`, but value is Positive
          // -> first sign bit is set (-> negative) and then negated with explicit `-`
          // Example: `-0b1000_0001y = -(-127y) = 127y`
          //
          // Quick Fixes:
          // * Adjust number in same base to use implicit `+`
          // * Change to decimal while remove explicit `-` (Decimal MUST match sign)
          // * Change to other bases while keeping explicit `-` (-> keep bits intact)

          if true then // `if` for grouping. Gets removed by compiler.
            let title =
              Title.Int.Convert.SpecialCase.useImplicitPlusInPositiveConstantWithMinusSign

            let absValue =
              assert (value >= GenericZero)
              value

            let replacement =
              match constant.Base with
              | Base.Decimal -> unreachable ()
              | Base.Hexadecimal -> Format.Int.asHexadecimalUnsigned absValue
              | Base.Octal -> Format.Int.asOctalUnsigned absValue
              | Base.Binary -> Format.Int.asBinaryUnsigned absValue

            mkFixReplaceExistingSign title replacement

          if
            (assert (constant.Base <> Base.Decimal)
             true)
          then
            let absValue =
              assert (value >= GenericZero)
              value

            mkFixReplaceExistingSign Title.Int.Convert.toDecimal (Format.Int.asDecimalUnsigned absValue)

          // keep `-` sign -> value after base-prefix must be negative
          let negativeValue = -value

          if constant.Base <> Base.Hexadecimal then
            mkFixKeepExistingSign Title.Int.Convert.toHexadecimal (Format.Int.asHexadecimalUnsigned negativeValue)

          if constant.Base <> Base.Octal then
            mkFixKeepExistingSign Title.Int.Convert.toOctal (Format.Int.asOctalUnsigned negativeValue)

          if constant.Base <> Base.Binary then
            mkFixKeepExistingSign Title.Int.Convert.toBinary (Format.Int.asBinaryUnsigned negativeValue)

        elif value = minValue then
          // special case: `MinValue`: there's no corresponding `abs` in same type:
          //               There's no `128y` matching `MinValue = -128y`

          // Note: we already handled `0` above
          //       -> if we're here we KNOW `value` & `minValue` MUST be signed and cannot be unsigned!
          assert (minValue <> GenericZero)

          if constant.Sign = Negative then
            // `-0b1000_0000y = -(-128y) = `-128y`
            // Note: Because no `+128y` and not decimal, we KNOW sign is not necessary
            let title = Title.Int.Convert.SpecialCase.removeExplicitMinusWithMinValue

            mkFix
              doc
              title
              [| { Range = constant.SignRange.ToRangeInside constant.Range
                   NewText = "" } |]

          if
            (assert (constant.Base <> Base.Decimal)
             true)
          then
            mkFixReplaceExistingSign Title.Int.Convert.toDecimal (Format.Int.asDecimalSigned value)

          if constant.Base <> Base.Hexadecimal then
            mkFixKeepExistingSign Title.Int.Convert.toHexadecimal (Format.Int.asHexadecimalUnsigned value)

          if constant.Base <> Base.Octal then
            mkFixKeepExistingSign Title.Int.Convert.toOctal (Format.Int.asOctalUnsigned value)

          if constant.Base <> Base.Binary then
            mkFixKeepExistingSign Title.Int.Convert.toBinary (Format.Int.asBinaryUnsigned value)

        elif value < GenericZero && constant.Sign = Positive then
          if true then
            let title = Title.Int.Convert.SpecialCase.extractMinusFromNegativeConstant

            let replacement =
              match constant.Base with
              | Base.Decimal -> unreachable ()
              | Base.Hexadecimal -> Format.Int.asHexadecimalSigned (value, abs)
              | Base.Octal -> Format.Int.asOctalSigned (value, abs)
              | Base.Binary -> Format.Int.asBinarySigned (value, abs)

            mkFixReplaceExistingSign title replacement

          if
            (assert (constant.Base <> Base.Decimal)
             true)
          then
            mkFixReplaceExistingSign Title.Int.Convert.toDecimal (Format.Int.asDecimalSigned value)

          // keep bits intact -> don't add any `-`
          if constant.Base <> Base.Hexadecimal then
            mkFixKeepExistingSign Title.Int.Convert.toHexadecimal (Format.Int.asHexadecimalUnsigned value)

          if constant.Base <> Base.Octal then
            mkFixKeepExistingSign Title.Int.Convert.toOctal (Format.Int.asOctalUnsigned value)

          if constant.Base <> Base.Binary then
            mkFixKeepExistingSign Title.Int.Convert.toBinary (Format.Int.asBinaryUnsigned value)

        elif value < GenericZero then
          assert (constant.Sign = Negative)

          if true then
            let title = Title.Int.Convert.SpecialCase.integrateExplicitMinus

            let replacement =
              match constant.Base with
              | Base.Decimal -> unreachable ()
              | Base.Hexadecimal -> Format.Int.asHexadecimalUnsigned value
              | Base.Octal -> Format.Int.asOctalUnsigned value
              | Base.Binary -> Format.Int.asBinaryUnsigned value

            mkFixReplaceExistingSign title replacement

          // keep `-` intact
          let absValue = abs value

          if
            (assert (constant.Base <> Base.Decimal)
             true)
          then
            mkFixKeepExistingSign Title.Int.Convert.toDecimal (Format.Int.asDecimalUnsigned absValue)

          if constant.Base <> Base.Hexadecimal then
            mkFixKeepExistingSign Title.Int.Convert.toHexadecimal (Format.Int.asHexadecimalUnsigned absValue)

          if constant.Base <> Base.Octal then
            mkFixKeepExistingSign Title.Int.Convert.toOctal (Format.Int.asOctalUnsigned absValue)

          if constant.Base <> Base.Binary then
            mkFixKeepExistingSign Title.Int.Convert.toBinary (Format.Int.asBinaryUnsigned absValue)

        else
          // unreachable()
          () ]

    let inline mkUIntFixes (value: 'uint) =
      [ if constant.Base <> Base.Decimal then
          mkFixKeepExistingSign Title.Int.Convert.toDecimal (Format.Int.asDecimalUnsigned value)
        if constant.Base <> Base.Hexadecimal then
          mkFixKeepExistingSign Title.Int.Convert.toHexadecimal (Format.Int.asHexadecimalUnsigned value)
        if constant.Base <> Base.Octal then
          mkFixKeepExistingSign Title.Int.Convert.toOctal (Format.Int.asOctalUnsigned value)
        if constant.Base <> Base.Binary then
          mkFixKeepExistingSign Title.Int.Convert.toBinary (Format.Int.asBinaryUnsigned value) ]

    let mkByteFixes (value: byte) =
      [ yield! mkUIntFixes value

        // convert to char (`'a'B`)
        if value < 128uy then
          let inline asByteChar charValue = $"'{charValue}'B"

          let mkFix title replacement =
            let edits =
              [| { Range = constant.Range
                   NewText = replacement } |]

            mkFix doc title edits

          let byteChar = char value

          match Format.Char.tryAsChar byteChar with
          | None -> ()
          | Some value ->
            let value = value |> asByteChar
            mkFix (Title.Char.Convert.toChar value) value

          let value = Format.Char.asDecimal byteChar |> asByteChar
          mkFix (Title.Char.Convert.toDecimal value) value
          // Currently not supported by F#
          // let value = Format.Char.asHexadecimal byteChar |> asByteChar
          // mkFix (Title.Char.Convert.toHexadecimal value) value
          let value = Format.Char.asUtf16Hexadecimal byteChar |> asByteChar
          mkFix (Title.Char.Convert.toUtf16Hexadecimal value) value
        // Currently not supported by F#
        // let value = Format.Char.asUtf32Hexadecimal byteChar |> asByteChar
        // mkFix (Title.Char.Convert.toUtf32Hexadecimal value) value
        ]

    let inline mkFloatFixes (value: 'float, getBits: 'float -> 'uint) =
      [ assert (constant.Base <> Base.Decimal)

        // value without explicit sign
        let specified = if constant.Sign = Negative then -value else value

        if constant.Base <> Base.Hexadecimal then
          mkFixKeepExistingSign Title.Int.Convert.toHexadecimal (Format.Int.asHexadecimalUnsigned (getBits specified))

        if constant.Base <> Base.Octal then
          mkFixKeepExistingSign Title.Int.Convert.toOctal (Format.Int.asOctalUnsigned (getBits specified))

        if constant.Base <> Base.Binary then
          mkFixKeepExistingSign Title.Int.Convert.toBinary (Format.Int.asBinaryUnsigned (getBits specified))

        // `0b1...lf`
        if value < GenericZero && constant.Sign = Positive then
          let title = Title.Int.Convert.SpecialCase.extractMinusFromNegativeConstant
          let posValue = abs value
          assert (posValue >= GenericZero)

          let replacement =
            match constant.Base with
            | Base.Decimal -> unreachable ()
            | Base.Hexadecimal -> Format.Int.asHexadecimalUnsigned (getBits posValue)
            | Base.Octal -> Format.Int.asOctalUnsigned (getBits posValue)
            | Base.Binary -> Format.Int.asBinaryUnsigned (getBits posValue)

          mkFixReplaceExistingSign title ("-" + replacement)
        // `-0b0....lf`
        elif value < GenericZero && constant.Sign = Negative then
          let title = Title.Int.Convert.SpecialCase.integrateExplicitMinus

          let replacement =
            match constant.Base with
            | Base.Decimal -> unreachable ()
            | Base.Hexadecimal -> Format.Int.asHexadecimalUnsigned (getBits value)
            | Base.Octal -> Format.Int.asOctalUnsigned (getBits value)
            | Base.Binary -> Format.Int.asBinaryUnsigned (getBits value)

          mkFixReplaceExistingSign title replacement
        // `-0b1...lf`
        elif value > GenericZero && constant.Sign = Negative then
          let title =
            Title.Int.Convert.SpecialCase.useImplicitPlusInPositiveConstantWithMinusSign

          let replacement =
            match constant.Base with
            | Base.Decimal -> unreachable ()
            | Base.Hexadecimal -> Format.Int.asHexadecimalUnsigned (getBits value)
            | Base.Octal -> Format.Int.asOctalUnsigned (getBits value)
            | Base.Binary -> Format.Int.asBinaryUnsigned (getBits value)

          mkFixReplaceExistingSign title replacement ]

    match constant.Constant with
    | SynConst.SByte value -> mkIntFixes (value, Int.abs, SByte.MinValue)
    | SynConst.Byte value -> mkByteFixes value
    | SynConst.Int16 value -> mkIntFixes (value, Int.abs, Int16.MinValue)
    | SynConst.UInt16 value -> mkUIntFixes value
    | SynConst.Int32 value -> mkIntFixes (value, Int.abs, Int32.MinValue)
    | SynConst.UInt32 value -> mkUIntFixes value
    | SynConst.Int64 value -> mkIntFixes (value, Int.abs, Int64.MinValue)
    | SynConst.UInt64 value -> mkUIntFixes value
    | SynConst.IntPtr value -> mkIntFixes (value, Int.abs, Int64.MinValue)
    | SynConst.UIntPtr value -> mkUIntFixes value

    | SynConst.Single value -> mkFloatFixes (value, BitConverter.SingleToUInt32Bits)
    | SynConst.Double value -> mkFloatFixes (value, BitConverter.DoubleToUInt64Bits)

    | _ -> []


  let padBinaryWithZerosFixes doc (lineStr: String) (constant: IntConstant) =
    match constant.Base with
    | Base.Binary ->
      let bits =
        match constant.Constant with
        | SynConst.Byte _ -> 8
        | SynConst.SByte _ -> 8
        | SynConst.Int16 _ -> 16
        | SynConst.UInt16 _ -> 16
        | SynConst.Int32 _ -> 32
        | SynConst.UInt32 _ -> 32
        | SynConst.Int64 _ -> 64
        | SynConst.UInt64 _ -> 64
        | SynConst.IntPtr _ -> 64
        | SynConst.UIntPtr _ -> 64
        | _ -> -1

      if bits > 0 then
        let digits = constant.ValueRange.SpanIn(constant.Range, lineStr)
        let nDigits = digits.Count(fun c -> c <> '_')

        let padTo (length: int) =
          if nDigits < length && length <= bits then
            let toAdd = length - nDigits
            let zeros = String('0', toAdd)

            let edits =
              [| { Range = constant.ValueRange.EmptyAtStart.ToRangeInside(constant.Range)
                   NewText = zeros } |]

            mkFix doc $"Pad with `0`s to `{length}` bits" edits |> Some
          else
            None

        // pad to 4,8,16 bits
        [ 4; 8; 16 ] |> List.choose padTo
      else
        []
    | _ -> []

  /// Separates digit groups with `_`.
  let separateDigitGroupsFix doc (lineStr: String) (constant: IntConstant) =
    let n = constant.ValueRange.SpanIn(constant.Range, lineStr)

    if n.Contains '_' then
      // don't change existing groups
      []
    else
      let n = n.ToString()

      let tryMkFix title groupSize =
        if n.Length > groupSize then
          [| { Range = constant.ValueRange.ToRangeInside(constant.Range)
               NewText = DigitGroup.addSeparator n groupSize DigitGroup.RightToLeft } |]
          |> mkFix doc title
          |> List.singleton
        else
          List.empty

      match constant.Base with
      | Base.Decimal -> [ yield! tryMkFix Title.Int.Separate.decimal3 3 ]
      | Base.Hexadecimal ->
        [ yield! tryMkFix Title.Int.Separate.hexadecimal4 4
          yield! tryMkFix Title.Int.Separate.hexadecimal2 2 ]
      | Base.Octal -> [ yield! tryMkFix Title.Int.Separate.octal3 3 ]
      | Base.Binary ->
        [ yield! tryMkFix Title.Int.Separate.binary4 4
          yield! tryMkFix Title.Int.Separate.binary8 8 ]

  /// Removes or adds digit group separators (`_`)
  let digitGroupFixes doc (lineStr: String) (constant: IntConstant) =
    match DigitGroup.removeFix doc lineStr constant.Range constant.ValueRange with
    | [] -> separateDigitGroupsFix doc lineStr constant
    | fix -> fix

  let private replaceIntWithNameFix
    doc
    (pos: FcsPos)
    (lineStr: String)
    (parseAndCheck: ParseAndCheckResults)
    (constant: IntConstant)
    =
    // Cannot use following because `Min/MaxValue` are Fields, not Properties (`get_Min/MaxValue`)
    // let inline private isMax<'int when 'int : equality and 'int:(static member MaxValue: 'int)> value =
    //   value = 'int.MaxValue
    // let inline private isMin<'int when 'int : equality and 'int:(static member MinValue: 'int)> value =
    //   value = 'int.MinValue
    let inline replaceWithExtremum value minValue maxValue =
      if value = maxValue then
        CommonFixes.replaceWithNamedConstantFix
          doc
          pos
          parseAndCheck
          constant.Constant
          constant.Range
          "MaxValue"
          Title.replaceWith
      //                   don't replace uint `0`
      elif value = minValue && value <> GenericZero then
        CommonFixes.replaceWithNamedConstantFix
          doc
          pos
          parseAndCheck
          constant.Constant
          constant.Range
          "MinValue"
          Title.replaceWith
      else
        []

    match constant.Constant with
    | SynConst.SByte value -> replaceWithExtremum value SByte.MinValue SByte.MaxValue
    | SynConst.Byte value -> replaceWithExtremum value Byte.MinValue Byte.MaxValue
    | SynConst.Int16 value -> replaceWithExtremum value Int16.MinValue Int16.MaxValue
    | SynConst.UInt16 value -> replaceWithExtremum value UInt16.MinValue UInt16.MaxValue
    | SynConst.Int32 value -> replaceWithExtremum value Int32.MinValue Int32.MaxValue
    | SynConst.UInt32 value -> replaceWithExtremum value UInt32.MinValue UInt32.MaxValue
    | SynConst.Int64 value -> replaceWithExtremum value Int64.MinValue Int64.MaxValue
    | SynConst.UInt64 value -> replaceWithExtremum value UInt64.MinValue UInt64.MaxValue
    | SynConst.IntPtr value -> replaceWithExtremum value Int64.MinValue Int64.MaxValue
    | SynConst.UIntPtr value -> replaceWithExtremum value UInt64.MinValue UInt64.MaxValue

    | SynConst.Single value ->
      CommonFixes.replaceFloatWithNameFix
        doc
        pos
        lineStr
        parseAndCheck
        constant.Constant
        constant.Range
        (FloatValue.from value)
    | SynConst.Double value ->
      CommonFixes.replaceFloatWithNameFix
        doc
        pos
        lineStr
        parseAndCheck
        constant.Constant
        constant.Range
        (FloatValue.from value)
    | SynConst.Decimal value -> replaceWithExtremum value Decimal.MinValue Decimal.MaxValue

    | _ -> []

  let all
    doc
    (pos: FcsPos)
    (lineStr: String)
    (parseAndCheck: ParseAndCheckResults)
    (error: bool)
    (constant: IntConstant)
    =
    [ if not error then
        yield! convertToOtherBaseFixes doc lineStr constant
        yield! replaceIntWithNameFix doc pos lineStr parseAndCheck constant

      yield! digitGroupFixes doc lineStr constant
      yield! padBinaryWithZerosFixes doc lineStr constant

      if DEBUG then
        debugFix doc lineStr constant ]

module private FloatFix =
  let private debugFix doc (lineStr: String) (constant: FloatConstant) =
    let data =
      let full = constant.Range.SpanIn(lineStr).ToString()

      let intPart =
        if constant.IntRange.IsEmpty then
          "∅"
        else
          constant.IntRange.SpanIn(full).ToString()

      let decPart =
        if constant.DecimalRange.IsEmpty then
          "∅"
        else
          constant.DecimalRange.SpanIn(full).ToString()

      let expPart =
        if constant.ExponentRange.IsEmpty then
          "∅"
        else
          constant.ExponentRange.SpanIn(full).ToString()

      let suffix = constant.SuffixRange.SpanIn(full).ToString()

      let format = if constant.IsScientific then "scientific" else "decimal"

      $"%A{constant.Sign} %A{intPart}.%A{decPart}e%A{expPart}%A{suffix} (%s{format}) (%A{full}, %A{constant.Value})"

    mkFix doc data [||]

  /// Separates digit groups with `_`.
  let separateDigitGroupsFix doc (lineStr: String) (constant: FloatConstant) =
    let text = constant.Range.SpanIn(lineStr)

    if text.Contains '_' then
      []
    else
      let edits =
        [| if constant.IntRange.Length > 3 then
             let range = constant.IntRange.ToRangeInside constant.Range
             let n = range.SpanIn(lineStr).ToString()

             { Range = range
               NewText = DigitGroup.addSeparator n 3 DigitGroup.RightToLeft }
           if constant.DecimalRange.Length > 3 then
             let range = constant.DecimalRange.ToRangeInside constant.Range
             let n = range.SpanIn(lineStr).ToString()

             { Range = range
               NewText = DigitGroup.addSeparator n 3 DigitGroup.LeftToRight }
           if constant.ExponentRange.Length > 3 then
             let range = constant.ExponentRange.ToRangeInside constant.Range
             let n = range.SpanIn(lineStr).ToString()

             { Range = range
               NewText = DigitGroup.addSeparator n 3 DigitGroup.RightToLeft } |]

      match edits with
      | [||] -> []
      | _ -> mkFix doc Title.Float.Separate.all3 edits |> List.singleton

  /// Removes or adds digit group separators (`_`)
  let digitGroupFixes doc (lineStr: String) (constant: FloatConstant) =
    match DigitGroup.removeFix doc lineStr constant.Range constant.ValueRange with
    | [] -> separateDigitGroupsFix doc lineStr constant
    | fix -> fix

  let all
    doc
    (pos: FcsPos)
    (lineStr: String)
    (parseAndCheck: ParseAndCheckResults)
    (error: bool)
    (constant: FloatConstant)
    =
    [ if not error then
        // Note: `infinity` & co don't get parsed as `SynConst`, but instead as `Ident`
        //       -> `constant` is always actual float value, not named
        yield!
          CommonFixes.replaceFloatWithNameFix
            doc
            pos
            lineStr
            parseAndCheck
            constant.Constant
            constant.Range
            constant.Value

      yield! digitGroupFixes doc lineStr constant

      if DEBUG then
        debugFix doc lineStr constant ]


/// CodeFixes for number-based Constant to:
/// * Convert between bases & forms
/// * Add digit group separators
/// * Replace with name (like `infinity` or `TYPE.MinValue`)
/// * Integrate/Extract Minus (Hex/Oct/Bin -> sign bit vs. explicit `-` sign)
let fix (getParseResultsForFile: GetParseResultsForFile) : CodeFix =
  fun (codeActionParams) ->
    asyncResult {
      let filePath = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath
      let fcsPos = protocolPosToPos codeActionParams.Range.Start
      let! (parseAndCheck, lineStr, _sourceText) = getParseResultsForFile filePath fcsPos

      match tryFindConstant parseAndCheck.GetAST fcsPos with
      | None -> return []
      | Some(range, constant) ->
        let range = fcsRangeToLsp range
        // We don't want any "convert to other base" fix for faulty constant:
        //   With error `SynConst value` falls back to its default value.
        //   For example: `let v = 12345uy` -> `SynConst.Byte 0`
        // But we might allow "Separate digit groups" fix
        let error =
          codeActionParams.Context.Diagnostics
          |> Array.exists (fun diag ->
            diag.Severity = Some DiagnosticSeverity.Error
            &&
            // Note: Only care about error when const is error, not any outer error
            diag.Range = range)

        let doc: TextDocumentIdentifier = codeActionParams.TextDocument

        /// Note: does NOT handle `byte` in ASCII format -- in fact it doesn't even check.
        ///       -> match ASCII `byte` BEFORE this! (-> `CharConstant.isAsciiByte`)
        let (|IntConstant|_|) constant =
          match constant with
          | SynConst.Byte _
          | SynConst.SByte _
          | SynConst.Int16 _
          | SynConst.UInt16 _
          | SynConst.Int32 _
          | SynConst.UInt32 _
          | SynConst.Int64 _
          | SynConst.UInt64 _
          | SynConst.IntPtr _
          | SynConst.UIntPtr _ ->
            assert (not (CharConstant.isAsciiByte (range.SpanIn(lineStr))))
            IntConstant.parse (lineStr, range, constant) |> Some
          | _ -> None

        /// Note: does NOT handle Hex/Oct/Bin formats -- in fact it doesn't even check.
        ///       -> match Hex/Oct/Bin BEFORE this! (-> `FloatConstant.isIntFloat`)
        let (|FloatConstant|_|) constant =
          let parse value =
            assert (not (FloatConstant.isIntFloat (range.SpanIn(lineStr))))
            FloatConstant.parse (lineStr, range, constant, value) |> Some

          match constant with
          | SynConst.Single value -> FloatValue.from value |> parse
          | SynConst.Double value -> FloatValue.from value |> parse
          | SynConst.Decimal value -> FloatValue.from value |> parse
          | _ -> None

        return
          match constant with
          | SynConst.Char value ->
            let constant = CharConstant.parse (lineStr, range, constant, value)
            CharFix.all doc lineStr error constant
          | SynConst.Byte value when CharConstant.isAsciiByte (range.SpanIn(lineStr)) ->
            let constant = CharConstant.parse (lineStr, range, constant, char value)
            CharFix.all doc lineStr error constant
          | IntConstant constant -> IntFix.all doc fcsPos lineStr parseAndCheck error constant
          | SynConst.UserNum _ ->
            let constant = IntConstant.parse (lineStr, range, constant)
            IntFix.all doc fcsPos lineStr parseAndCheck error constant
          | SynConst.Single _
          | SynConst.Double _ when FloatConstant.isIntFloat (range.SpanIn(lineStr)) ->
            let constant = IntConstant.parse (lineStr, range, constant)
            IntFix.all doc fcsPos lineStr parseAndCheck error constant
          | FloatConstant constant -> FloatFix.all doc fcsPos lineStr parseAndCheck error constant
          | _ -> []
    }
