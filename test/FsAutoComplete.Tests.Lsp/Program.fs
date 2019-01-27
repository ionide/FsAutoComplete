module LspTest

open Expecto
open FsAutoComplete.Tests.Lsp

[<EntryPoint>]
let main args =
  runTestsWithArgs defaultConfig args tests