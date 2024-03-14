module private FsAutoComplete.Tests.CodeFixTests.UpdateValueInSignatureFileTests

open System.IO
open Expecto
open Helpers
open Utils.ServerTests
open Utils.CursorbasedTests
open FsAutoComplete.CodeFix
open Utils.CursorbasedTests.CodeFix

let path =
  Path.Combine(__SOURCE_DIRECTORY__, @"../TestCases/CodeFixTests/RenameParamToMatchSignature/")

let tests state =
  serverTestList (nameof UpdateValueInSignatureFile) state defaultConfigDto (Some path) (fun server ->
    [ let selectCodeFix = CodeFix.withTitle UpdateValueInSignatureFile.title

      testCaseAsync "first unit test for UpdateValueInSignatureFile"
      <| checkCodeFixInImplementationAndVerifySignature
        server
        """
module A

val a: b:int -> int
"""
        """
module A

let a$0 (b:int) (c: string) = 0
"""
        (Diagnostics.expectCode "34")
        selectCodeFix
        """
module A

val a: b: int -> c: string -> int
""" ])
