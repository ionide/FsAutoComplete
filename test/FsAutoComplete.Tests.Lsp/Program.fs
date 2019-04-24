module LspTest

open Expecto
open FsAutoComplete.Tests.Lsp

[<EntryPoint>]
let main args =
  let res =
    tests
    |> List.sumBy (fun n ->
      let test = n ()
      runTests defaultConfig test
    )

  // let _ = System.Console.ReadKey()
  res