module LspTest

open Expecto
open FsAutoComplete.Tests.Lsp

[<EntryPoint>]
let main args =
  let res = runTestsInAssembly defaultConfig args
  // let _ = System.Console.ReadKey()
  res