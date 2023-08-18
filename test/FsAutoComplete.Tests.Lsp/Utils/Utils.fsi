module Utils.Utils

module Expect =
    open FsAutoComplete.Utils
    open Expecto

    val failureMatching: m: (AssertException -> bool) -> f: Async<'a> -> Async<unit>
    /// passed Async `f` is expected to throw `Expecto.AssertException`
    /// -> Expecto Test in `f` is expected to fail
    ///
    /// ~ Basically fancy `Async` wrapper for `Expect.throwsT<Expecto.AssertException>`
    ///
    /// Note: `failwith` doesn't trigger success (throws `System.Exception`). Use `failtest` instead
    val failure: f: Async<'a> -> Async<unit>

module private Seq =
    val tryMin: source: seq<'a> -> 'a option when 'a: comparison

module Position =
    open Ionide.LanguageServerProtocol.Types
    val inline assertPositive: pos: Position -> unit
    val inline eq: p1: 'a -> p2: 'a -> bool when 'a: equality
    val inline gt: p1: Position -> p2: Position -> bool
    val inline geq: p1: Position -> p2: Position -> bool
    val inline lt: p1: Position -> p2: Position -> bool
    val inline leq: p1: Position -> p2: Position -> bool

/// Note: Always assumes correct order inside Range: `Start <= End`
module Range =
    open Ionide.LanguageServerProtocol.Types
    /// Range represents a single position (`Start = End`)
    val inline isPosition: range: Range -> bool
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
    val inline containsStrictly: pos: Position -> range: Range -> bool
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
    val inline containsLoosely: pos: Position -> range: Range -> bool
    /// ```text
    /// ----------------------------------->
    ///       ^^^^^^^^^^^^^^^^^ range
    ///   ^   ^       ^       ^        ^ false
    ///   |   |       |       ┕ true
    ///   |   |       ┕ false
    ///   |   ┕ true
    ///   ┕ false
    /// ```
    val inline onBorder: pos: Position -> range: Range -> bool
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
    val inline touches: range1: Range -> range2: Range -> bool
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
    val overlapsStrictly: range1: Range -> range2: Range -> bool
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
    val overlapsLoosely: range1: Range -> range2: Range -> bool
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
    val isDisjointStrictly: range1: Range -> range2: Range -> bool
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
    val isDisjointLoosely: range1: Range -> range2: Range -> bool

module Text =
    open System
    val inline assertNoCarriageReturn: text: string -> unit
    val removeCarriageReturn: text: string -> string
    /// Note: only works with `\n`, but fails with `\r`!
    val lines: text: string -> string array
    val getIndentation: line: string -> int
    /// Trim:
    /// * Leading `\n` from triple quotes string with text starting in next line
    /// * indentation (measured for non-empty lines)
    /// * Trailing whitespace in otherwise empty last line
    ///   Note: `\n` isn't removed
    ///
    /// Note: Asserts the passed text contains no `\r` (neither `\r` nor `\r\n`).
    ///       It doesn't replace `\r` with `\n` but instead fails!
    val trimTripleQuotation: text: string -> string
