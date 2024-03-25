module private FsAutoComplete.Tests.CodeFixTests.IgnoreExpressionTests

open Expecto
open Helpers
open Utils.ServerTests
open Utils.CursorbasedTests
open FsAutoComplete.CodeFix

let tests state =
  serverTestList (nameof IgnoreExpression) state defaultConfigDto None (fun server ->
    [ let selectCodeFix = CodeFix.withTitle IgnoreExpression.title

      testCaseAsync "ignore constant"
      <| CodeFix.check
        server
        "
let a b =
    9$0
    null"
        Diagnostics.acceptAll
        selectCodeFix
        "
let a b =
    (9) |> ignore
    null"

      testCaseAsync "ignore infix application"
      <| CodeFix.check
        server
        "
let a b =
    0 / 9$0
    null"
        Diagnostics.acceptAll
        selectCodeFix
        "
let a b =
    (0 / 9) |> ignore
    null"

      testCaseAsync "ignore member invocation"
      <| CodeFix.check
        server
        "
open System.Collections.Generic

let foo () =
    let dict = dict []
    di$0ct.TryAdd(\"foo\", \"bar\")
    ()"
        Diagnostics.acceptAll
        selectCodeFix
        "
open System.Collections.Generic

let foo () =
    let dict = dict []
    (dict.TryAdd(\"foo\", \"bar\")) |> ignore
    ()"

      ])
