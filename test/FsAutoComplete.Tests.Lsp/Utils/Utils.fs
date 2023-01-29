module Utils.Utils

open System.Runtime.CompilerServices
open System.Runtime.InteropServices

type File =
  static member CurrentDir([<CallerFilePath;DefaultParameterValue(null: string); OptionalAttribute>] callerPath: string) = callerPath


module Expect =
  open FsAutoComplete.Utils
  open Expecto

  let failureMatching (m: AssertException -> bool) (f: Async<_>) = async {
    let failed = async {
      try
        do! f |> Async.map ignore
        return false
      with
      | :? AssertException as ex when m ex -> return true
      // keep other exceptions
    }

    let! failed = failed
    if not failed then
      failtestf "Expected AssertException, but was no exception"
  }
  /// passed Async `f` is expected to throw `Expecto.AssertException`
  /// -> Expecto Test in `f` is expected to fail
  ///
  /// ~ Basically fancy `Async` wrapper for `Expect.throwsT<Expecto.AssertException>`
  ///
  /// Note: `failwith` doesn't trigger success (throws `System.Exception`). Use `failtest` instead
  let failure f = failureMatching (fun _ -> true) f

module private Seq =
  let tryMin source =
    source
    |> Seq.fold (fun m e ->
      match m with
      | None -> Some e
      | Some m -> Some (min m e)
    ) None

module Position =
  open Ionide.LanguageServerProtocol.Types

  let inline assertPositive (pos: Position) =
    assert(pos.Line >= 0)
    assert(pos.Character >= 0)

  let inline eq p1 p2 =
    // p1.Line = p2.Line && p1.Character = p2.Character
    p1 = p2
  let inline gt p1 p2 =
    p1.Line > p2.Line || (p1.Line = p2.Line && p1.Character > p2.Character)
  let inline geq p1 p2 = eq p1 p2 || gt p1 p2
  let inline lt p1 p2 = gt p2 p1
  let inline leq p1 p2 = geq p2 p1

/// Note: Always assumes correct order inside Range: `Start <= End`
module Range =
  open Ionide.LanguageServerProtocol.Types

  // Strict: no touching
  // Loose: touching


  /// Range represents a single position (`Start = End`)
  let inline isPosition (range: Range) =
    range.Start = range.End

  /// Strict: `pos` on `Start` or `End` of `range` counts as containing
  ///
  /// ```text
  /// ----------------------------------->
  ///       ^^^^^^^^^^^^^^^^^ range
  ///   ^   ^       ^       ^        ^ false
  ///   |   |       |       ┕ true
  ///   |   |       ┕ true
  ///   |   ┕ true
  ///   ┕ false
  /// ```
  let inline containsStrictly (pos: Position) (range: Range) =
    // range.Start <= pos <= range.End
    Position.leq range.Start pos && Position.leq pos range.End

  /// Loose: `pos` on `Start` or `End` of `range` doesn't count as containing
  ///
  /// ```text
  /// ----------------------------------->
  ///       ^^^^^^^^^^^^^^^^^ range
  ///   ^   ^       ^       ^        ^ false
  ///   |   |       |       ┕ false
  ///   |   |       ┕ true
  ///   |   ┕ false
  ///   ┕ false
  /// ```
  let inline containsLoosely (pos: Position) (range: Range) =
    // range.Start < pos < range.End
    Position.leq range.Start pos && Position.leq pos range.End

  /// ```text
  /// ----------------------------------->
  ///       ^^^^^^^^^^^^^^^^^ range
  ///   ^   ^       ^       ^        ^ false
  ///   |   |       |       ┕ true
  ///   |   |       ┕ false
  ///   |   ┕ true
  ///   ┕ false
  /// ```
  let inline onBorder (pos: Position) (range: Range) =
    pos = range.Start || pos = range.End

  /// Share a Start/End or End/Start, but nothing else.
  ///
  /// ```text
  /// -------------------------->
  ///   ^^^^^^^
  ///      |  |   ^^^^^^^^ false
  ///      |  ^^^^^^^^ true
  ///      ^^^^^^^^ false
  ///   ^^^^^^ false
  ///   ^^^ false
  ///   ^ true
  /// ^^^ true
  /// ```
  let inline touches (range1: Range) (range2: Range) =
    range1.Start = range2.End || range1.End = range2.Start

  /// Strict: Just sharing a Start/End (touching) counts as overlap too
  ///
  /// ```text
  /// -------------------------->
  ///   ^^^^^^^
  ///      |  |   ^^^^^^^^ false
  ///      |  ^^^^^^^^ true
  ///      ^^^^^^^^ true
  ///   ^^^^^^^ true
  /// ```
  let overlapsStrictly (range1: Range) (range2: Range) =
    range1 |> containsStrictly range2.Start
    ||
    range1 |> containsStrictly range2.End
    ||
    range2 |> containsStrictly range1.Start
    ||
    range2 |> containsStrictly range1.End

  /// Loose: Touching doesn't count as overlapping.
  ///        Neither does both just position and same position
  ///
  /// ```text
  /// -------------------------->
  ///   ^^^^^^^
  ///   |  |  |   ^^^^^^^^ false
  ///   |  |  ^^^^^^^^ false
  ///   |  ^^^^^^^^ true
  ///   ^^^^^^^ true
  /// ```
  /// ```text
  /// -------------------------->
  ///       ^
  ///       |  ^ false
  ///       ^ false
  ///       ^^^^ false
  ///    ^^^^^^ true
  /// ```
  let overlapsLoosely (range1: Range) (range2: Range) =
    (range1 |> overlapsStrictly range2)
    &&
    not (range1 |> touches range2)

  /// Strict: Touching is not disjoint
  ///
  /// ```text
  /// -------------------------->
  ///   ^^^^^^^
  ///      |  |   ^^^^^^^^ true
  ///      |  ^^^^^^^^ false
  ///      |  ^ false
  ///      ^^^^^^^^ false
  /// ```
  let isDisjointStrictly (range1: Range) (range2: Range) =
    not <| overlapsStrictly range1 range2
  /// Loose: Touching is disjoint
  ///
  /// ```text
  /// -------------------------->
  ///   ^^^^^^^
  ///      |  |   ^^^^^^^^ true
  ///      |  ^^^^^^^^ true
  ///      |  ^ true
  ///      ^^^^^^^^ false
  /// ```
  let isDisjointLoosely (range1: Range) (range2: Range) =
    not <| overlapsLoosely range1 range2


module Text =
  open System

  let inline assertNoCarriageReturn (text: string) =
    if text.Contains '\r' then
      Expecto.Tests.failtest "Text contains `\\r` (either alone or as `\\r\\n`). But only `\\n` is supported"
  let removeCarriageReturn (text: string) =
    text.Replace("\r\n", "\n").Replace("\r", "\n")

  /// Note: only works with `\n`, but fails with `\r`!
  let lines (text: string) =
    assertNoCarriageReturn text
    text.Split '\n'

  /// remove leading `\n` from triple quoted string with text starting in next line
  let private trimLeadingNewLine (text: string) =
    if text.StartsWith '\n' then
      text.Substring 1
    else
      text
  /// remove trailing whitespace from last line, if last line is otherwise empty.
  /// Note: keeps the `\n`!
  /// Note: doesn't trim a single line with just whitespace -> requires at least one `\n`
  let private trimLastWhitespacesLine (text: string) =
    match text.LastIndexOf '\n' with
    | -1 -> text
    | i ->
        let tail = text.AsSpan(i+1)
        if not tail.IsEmpty && tail.IsWhiteSpace() then
          text.Substring(0, i+1)
        else
          text
  /// remove trailing last line, if last line is empty.
  /// Unlike `trimLastWhitespacesLine` this removes the trailing `\n` too
  /// Note: doesn't trim a single line with just whitespace -> requires at least one `\n`
  let private trimTrailingEmptyLine (text: string) =
    match text.LastIndexOf '\n' with
    | -1 ->
        text
    | i when text.AsSpan().Slice(i).IsWhiteSpace() ->  // `\n` is whitespace
        text.Substring(0, i)
    | _ -> text

  let getIndentation (line: string) =
    line.Length - line.AsSpan().TrimStart().Length
  let private detectIndentation (text: string) =
    text
    |> lines
    |> Seq.filter (not << String.IsNullOrWhiteSpace)
    |> Seq.map getIndentation
    |> Seq.tryMin
    |> Option.defaultValue 0

  let private trimIndentation (text: string) =
    match text |> detectIndentation with
    | 0 -> text
    | ind ->
        text
        |> lines
        |> Seq.map (fun line ->
          if line.Length <= ind then
            assert(line |> String.IsNullOrWhiteSpace)
            ""
          else
            line.Substring ind
        )
        |> String.concat "\n"

  /// Trim:
  /// * Leading `\n` from triple quotes string with text starting in next line
  /// * indentation (measured for non-empty lines)
  /// * Trailing whitespace in otherwise empty last line
  ///   Note: `\n` isn't removed
  ///
  /// Note: Asserts the passed text contains no `\r` (neither `\r` nor `\r\n`).
  ///       It doesn't replace `\r` with `\n` but instead fails!
  let trimTripleQuotation (text: string) =
    assertNoCarriageReturn text

    text
    |> trimLeadingNewLine
    |> trimIndentation
    |> trimLastWhitespacesLine
