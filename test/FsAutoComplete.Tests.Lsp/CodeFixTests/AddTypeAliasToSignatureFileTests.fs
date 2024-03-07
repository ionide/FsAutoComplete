module private FsAutoComplete.Tests.CodeFixTests.AddTypeAliasToSignatureFileTests

open System.IO
open Expecto
open Helpers
open Utils.ServerTests
open Utils.CursorbasedTests
open FsAutoComplete.CodeFix

let path =
    Path.Combine (__SOURCE_DIRECTORY__, @"../TestCases/CodeFixTests/RenameParamToMatchSignature/")

let tests state =
    serverTestList
        (nameof AddTypeAliasToSignatureFile)
        state
        defaultConfigDto
        (Some path)
        (fun server ->
            [
                let selectCodeFix = CodeFix.withTitle AddTypeAliasToSignatureFile.title

                ftestCaseAsync
                    "Sample use-case for AddTypeAliasToSignatureFile"
                    (CodeFix.checkCodeFixInImplementationAndVerifySignature
                        server
                        """
module Foo

open Foo
"""
                        """
module Foo

open Foo

type Bar = $0int
"""
                        Diagnostics.acceptAll
                        selectCodeFix
                        """
module Foo

open Foo

type Bar = int
""")
            ]
        )
