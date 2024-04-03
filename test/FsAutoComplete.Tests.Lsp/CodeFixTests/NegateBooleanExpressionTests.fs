module private FsAutoComplete.Tests.CodeFixTests.NegateBooleanExpressionTests

open Expecto
open Helpers
open Utils.ServerTests
open Utils.CursorbasedTests
open FsAutoComplete.CodeFix

let tests state =
  fserverTestList
    (nameof NegateBooleanExpression)
    state
    { defaultConfigDto with
        EnableAnalyzers = Some true }
    None
    (fun server ->
      [ let selectCodeFix = CodeFix.withTitle NegateBooleanExpression.title

        testCaseAsync "negate single identifier"
        <| CodeFix.check
          server
          "let a = false
let b = a$0"
          Diagnostics.acceptAll
          selectCodeFix
          "let a = false
let b = not (a)"

        testCaseAsync "negate boolean expression"
        <| CodeFix.check
          server
          "let a = false
let b = a $0|| false"
          Diagnostics.acceptAll
          selectCodeFix
          "let a = false
let b = not (a || false)"

        testCaseAsync "negate longdotident expression"
        <| CodeFix.check
          server
          "
module A =
  let a = false

let b = $0A.a"
          Diagnostics.acceptAll
          selectCodeFix
          "
module A =
  let a = false

let b = not (A.a)"

        testCaseAsync "negate record field"
        <| CodeFix.check
          server
          "
type X = { Y: bool }

let a = { Y = true }
let b = $0a.Y"
          Diagnostics.acceptAll
          selectCodeFix
          "
type X = { Y: bool }

let a = { Y = true }
let b = not (a.Y)"

        testCaseAsync "negate class property"
        <| CodeFix.check
          server
          "
type X() =
  member val Y : bool = false

let b = X().Y$0"
          Diagnostics.acceptAll
          selectCodeFix
          "
type X() =
  member val Y : bool = false

let b = not (X().Y)"

        testCaseAsync "negate class property, cursor at start"
        <| CodeFix.check
          server
          "
type X() =
  member val Y : bool = false

let b = $0X().Y"
          Diagnostics.acceptAll
          selectCodeFix
          "
type X() =
  member val Y : bool = false

let b = not (X().Y)"

        testCaseAsync "negate unit function call"
        <| CodeFix.check
          server
          "
let a () = false
let b = a$0 ()"
          Diagnostics.acceptAll
          selectCodeFix
          "
let a () = false
let b = not (a ())"

        testCaseAsync "negate unit function call, cursor at end"
        <| CodeFix.check
          server
          "
let a () = false
let b = a ()$0"
          Diagnostics.acceptAll
          selectCodeFix
          "
let a () = false
let b = not (a ())"

        testCaseAsync "negate non-unit function call, cursor at end"
        <| CodeFix.check
          server
          "
let a _ = false
let b = a 4$0"
          Diagnostics.acceptAll
          selectCodeFix
          "
let a _ = false
let b = not (a 4)"

        testCaseAsync "negate non-unit function call, cursor in middle"
        <| CodeFix.check
          server
          "
let a _ = false
let b = a $0 4"
          Diagnostics.acceptAll
          selectCodeFix
          "
let a _ = false
let b = not (a  4)"

        testCaseAsync "negate unit member invocation"
        <| CodeFix.check
          server
          "
type X() =
  member x.Y () : bool = false

let b = $0X().Y()"
          Diagnostics.acceptAll
          selectCodeFix
          "
type X() =
  member x.Y () : bool = false

let b = not (X().Y())"

        testCaseAsync "negate unit member invocation, cursor at end"
        <| CodeFix.check
          server
          "
type X() =
  member x.Y () : bool = false

let b = X().Y()$0"
          Diagnostics.acceptAll
          selectCodeFix
          "
type X() =
  member x.Y () : bool = false

let b = not (X().Y())"

        testCaseAsync "negate instance member invocation"
        <| CodeFix.check
          server
          "
open System.Collections.Generic

let foo () =
    let dict = dict []
    dict.TryAdd$0(\"foo\", \"bar\")
    ()"
          Diagnostics.acceptAll
          selectCodeFix
          "
open System.Collections.Generic

let foo () =
    let dict = dict []
    (not (dict.TryAdd(\"foo\", \"bar\")))
    ()"

        ])
