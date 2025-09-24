namespace FsAutoComplete.Tests.TestExplorer

open Expecto

module Program =
  [<EntryPoint>]
  let main argv = Tests.runTestsInAssemblyWithCLIArgs [] argv
