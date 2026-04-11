module private FsAutoComplete.Tests.CodeFixTests.GenerateAnonRecordStubTests

open Expecto
open Helpers
open Utils.ServerTests
open Utils.CursorbasedTests
open FsAutoComplete.CodeFix

let tests state =
  serverTestList (nameof GenerateAnonRecordStub) state defaultConfigDto None (fun server ->
    [ testCaseAsync "add one missing field to anonymous record"
      <| CodeFix.check
        server
        """let f (x: {| A: int; B: string |}) = x
let y = f {| A$0 = 1 |}"""
        Diagnostics.acceptAll
        (CodeFix.withTitle GenerateAnonRecordStub.title)
        """let f (x: {| A: int; B: string |}) = x
let y = f {| A = 1; B = failwith "Not Implemented" |}"""

      testCaseAsync "add multiple missing fields to anonymous record"
      <| CodeFix.check
        server
        """let f (x: {| A: int; B: string; C: bool |}) = x
let y = f {| A$0 = 1 |}"""
        Diagnostics.acceptAll
        (CodeFix.withTitle GenerateAnonRecordStub.title)
        """let f (x: {| A: int; B: string; C: bool |}) = x
let y = f {| A = 1; B = failwith "Not Implemented"; C = failwith "Not Implemented" |}"""

      testCaseAsync "add missing field to empty anonymous record"
      <| CodeFix.check
        server
        """let f (x: {| A: int |}) = x
let y = f {|$0|}"""
        Diagnostics.acceptAll
        (CodeFix.withTitle GenerateAnonRecordStub.title)
        """let f (x: {| A: int |}) = x
let y = f {| A = failwith "Not Implemented" |}"""

      testCaseAsync "add missing field in type-annotated let binding"
      <| CodeFix.check
        server
        """let y: {| A: int; B: string |} = {| A$0 = 1 |}"""
        Diagnostics.acceptAll
        (CodeFix.withTitle GenerateAnonRecordStub.title)
        """let y: {| A: int; B: string |} = {| A = 1; B = failwith "Not Implemented" |}"""

      testCaseAsync "add missing field when record has trailing spaces before closing bracket"
      <| CodeFix.check
        server
        """let f (x: {| A: int; B: string |}) = x
let y = f {| A$0 = 1   |}"""
        Diagnostics.acceptAll
        (CodeFix.withTitle GenerateAnonRecordStub.title)
        """let f (x: {| A: int; B: string |}) = x
let y = f {| A = 1; B = failwith "Not Implemented" |}"""

      testCaseAsync "add missing field when record is in match arm"
      <| CodeFix.check
        server
        """let f (x: {| A: int; B: string |}) = x
let y =
    match 1 with
    | _ -> f {| A$0 = 1 |}"""
        Diagnostics.acceptAll
        (CodeFix.withTitle GenerateAnonRecordStub.title)
        """let f (x: {| A: int; B: string |}) = x
let y =
    match 1 with
    | _ -> f {| A = 1; B = failwith "Not Implemented" |}""" ])
