module private FsAutoComplete.Tests.CodeFixTests.UpdateTypeAbbreviationInSignatureFileTests

open System.IO
open Expecto
open Helpers
open Utils.ServerTests
open Utils.CursorbasedTests
open FsAutoComplete.CodeFix

let path =
  Path.Combine(__SOURCE_DIRECTORY__, @"../TestCases/CodeFixTests/RenameParamToMatchSignature/")

let tests state =
  serverTestList (nameof UpdateTypeAbbreviationInSignatureFile) state defaultConfigDto (Some path) (fun server ->
    let selectCodeFix = CodeFix.withTitle UpdateTypeAbbreviationInSignatureFile.title

    let test name sigBefore impl sigAfter =
      testCaseAsync
        name
        (CodeFix.checkCodeFixInImplementationAndVerifySignature
          server
          sigBefore
          impl
          (Diagnostics.expectCode "318")
          selectCodeFix
          sigAfter)

    [

      test
        "Update anonymous record in signature file"
        """
namespace Foo

type X = {| y: int |}
"""
        """
namespace Foo

type X$0 = {| y: int; z: string |}
"""
        """
namespace Foo

type X = {| y: int; z: string |}
"""

      test
        "Update function type in signature file"
        """
namespace Foo

type X = unit -> int
"""
        """
namespace Foo

type X$0 = int -> int
"""
        """
namespace Foo

type X = int -> int
"""

      ])
