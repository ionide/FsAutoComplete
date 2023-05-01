/// Code from VisualFSharpPowerTools project: https://github.com/fsprojects/VisualFSharpPowerTools/blob/master/src/FSharp.Editing/Common/UntypedAstUtils.fs
module FsAutoComplete.UntypedAstUtils

open FSharp.Compiler.Syntax
open System.Collections.Generic
open FSharp.Compiler
open FSharp.Compiler.Text
open FSharp.Control.Reactive.Observable

type Range with

  member inline IsEmpty: bool

type internal ShortIdent = string
type internal Idents = ShortIdent[]
val internal longIdentToArray: longIdent: LongIdent -> Idents
/// An recursive pattern that collect all sequential expressions to avoid StackOverflowException
val (|Sequentials|_|): (SynExpr -> SynExpr list option)
val (|ConstructorPats|): (SynArgPats -> SynPat list)
/// matches if the range contains the position
val (|ContainsPos|_|): pos: pos -> range: range -> unit option
/// Active pattern that matches an ident on a given name by the ident's `idText`
val (|Ident|_|): ofName: string -> (SynExpr -> unit option)
/// matches if the range contains the position
val (|IdentContainsPos|_|): pos: pos -> ident: Ident -> unit option
/// A pattern that collects all attributes from a `SynAttributes` into a single flat list
val (|AllAttrs|): attrs: SynAttributes -> SynAttribute list
/// A pattern that collects all patterns from a `SynSimplePats` into a single flat list
val (|AllSimplePats|): pats: SynSimplePats -> SynSimplePat list
/// Gives all ranges for current position
val internal getRangesAtPosition: input: ParsedInput -> r: Position -> Range list

module Completion =
  [<RequireQualifiedAccess>]
  type Context =
    | StringLiteral
    | Unknown
    | SynType

  val atPos: pos: Position * ast: ParsedInput -> Context
