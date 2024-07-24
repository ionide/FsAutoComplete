module private FsAutoComplete.Tests.CodeFixTests.AddTypeAliasToSignatureFileTests

open System.IO
open Expecto
open Helpers
open Utils.ServerTests
open Utils.CursorbasedTests
open FsAutoComplete.CodeFix

let path =
  Path.Combine(Helpers.Paths.SourceDirectory(), @"../TestCases/CodeFixTests/RenameParamToMatchSignature/")

let tests state =
  serverTestList (nameof AddTypeAliasToSignatureFile) state defaultConfigDto (Some path) (fun server ->
    let selectCodeFix = CodeFix.withTitle AddTypeAliasToSignatureFile.title

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

    [ test
        "Sample use-case for AddTypeAliasToSignatureFile"
        """
module Foo

open System
"""
        """
module Foo

open System

type Bar = $0int
"""
        """
module Foo

open System

type Bar = int
"""

      test
        "place type alias above existing val"
        """
module Foo

open System

val a: int
"""
        """
module Foo

open System

let a = 8

type Bar$0 = string
"""
        """
module Foo

open System

type Bar = string

val a: int
"""

      test
        "place type alias above existing type"
        """
namespace Foo

[<Class>]
type A =
    new: unit -> A
"""
        """
namespace Foo

type A() = class end

type $0P = int -> int
"""
        """
namespace Foo

type P = int -> int

[<Class>]
type A =
    new: unit -> A
"""

      test
        "replace and with type keyword"
        """
namespace Foo

open System
"""
        """
namespace Foo

open System

type Foo = class end
and Bar$0 = string
"""
        """
namespace Foo

open System

type Bar = string
"""

      test
        "empty module"
        """
namespace Foo
"""
        """
namespace Foo

type $0Foo = int
"""
        """
namespace Foo

type Foo = int
"""

      test
        "empty nested module"
        """
namespace Foo

module Bar =
    begin end
"""
        """
namespace Foo

module Bar =
    type $0Foo = int
"""
        """
namespace Foo

module Bar =
    type Foo = int
"""

      test
        "non empty nested module"
        """
namespace Foo

module Bar =
    val a: int
"""
        """
namespace Foo

module Bar =
    let a = -9
    type $0Foo = int
"""
        """
namespace Foo

module Bar =
    type Foo = int

    val a: int
""" ])
