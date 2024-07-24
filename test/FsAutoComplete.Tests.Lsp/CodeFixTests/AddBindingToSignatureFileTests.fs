module private FsAutoComplete.Tests.CodeFixTests.AddBindingToSignatureFileTests

open System.IO
open Expecto
open Helpers
open Utils.ServerTests
open Utils.CursorbasedTests
open FsAutoComplete.CodeFix

let path =
  Path.Combine(Helpers.Paths.SourceDirectory(), @"../TestCases/CodeFixTests/RenameParamToMatchSignature/")

let tests state =
  serverTestList (nameof AddBindingToSignatureFile) state defaultConfigDto (Some path) (fun server ->
    let selectCodeFix = CodeFix.withTitle AddBindingToSignatureFile.title

    let test name sigBefore impl sigAfter =
      testCaseAsync
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

      test
        "Add value binding"
        """
      module Foo
      """
        """
      module Foo

      let$0 a = 'c'
      """
        """
      module Foo

      val a: char
      """

      test
        "Add function binding using display context of signature file"
        """
module Foo
"""
        """
module Foo

open System

let d$0 (v:DateTime) = v
"""
        """
module Foo

val d: v: System.DateTime -> System.DateTime
"""

      test
        "Add binding to nested module"
        """
namespace Foo

module Bar =
    val x: int
"""
        """
namespace Foo

module Bar =

    let x = 42
    let a$0 b = b - 1
"""
        """
namespace Foo

module Bar =
    val x: int

    val a: b: int -> int
"""

      test
        "Add binding to empty nested module"
        """
namespace Foo

module Bar = begin end
"""
        """
namespace Foo

module Bar =
    let a$0 b = b - 1
"""
        """
namespace Foo

module Bar =
    val a: b: int -> int
""" ])
