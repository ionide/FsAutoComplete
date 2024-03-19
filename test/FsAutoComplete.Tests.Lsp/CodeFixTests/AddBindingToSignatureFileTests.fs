module private FsAutoComplete.Tests.CodeFixTests.AddBindingToSignatureFileTests

open System.IO
open Expecto
open Helpers
open Utils.ServerTests
open Utils.CursorbasedTests
open FsAutoComplete.CodeFix

let path =
  Path.Combine(__SOURCE_DIRECTORY__, @"../TestCases/CodeFixTests/RenameParamToMatchSignature/")

let tests state =
  serverTestList (nameof AddBindingToSignatureFile) state defaultConfigDto (Some path) (fun server ->
    let selectCodeFix = CodeFix.withTitle AddBindingToSignatureFile.title

    let test name sigBefore impl sigAfter =
      ftestCaseAsync
        name
        (CodeFix.checkCodeFixInImplementationAndVerifySignature
          server
          sigBefore
          impl
          Diagnostics.acceptAll
          selectCodeFix
          sigAfter)

    [

      test
        "Add simple function binding"
        """
module Foo
"""
        """
module Foo

let a$0 b = b - 1
"""
        """
module Foo

val a: b: int -> int
"""

      ])
