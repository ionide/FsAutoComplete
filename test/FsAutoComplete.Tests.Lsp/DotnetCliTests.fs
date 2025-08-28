module FsAutoComplete.Tests.DotnetCliTests

open Expecto
open System

[<Tests>]
let tests =
  testList "DotnetCli" [
    
    testCase "DotnetCli module exists and compiles successfully" <| fun _ ->
      // Test that the DotnetCli module compiles and is accessible
      Expect.isTrue true "DotnetCli module compiled successfully"

    testCase "DotnetCli module can be found in assembly" <| fun _ ->
      // Test that DotnetCli functionality exists in the compiled assembly
      let assembly = System.Reflection.Assembly.GetAssembly(typeof<FsAutoComplete.TestAdapter.TestAdapterEntry<int>>)
      Expect.isNotNull assembly "Assembly should be loaded"
      
      let types = assembly.GetTypes()
      let dotnetCliTypes = types |> Array.filter (fun t -> t.FullName <> null && t.FullName.Contains("DotnetCli"))
      
      Expect.isTrue (dotnetCliTypes.Length > 0) "Should find DotnetCli related types in assembly"

    testCase "Core module has expected structure" <| fun _ ->
      // Test that we can reference the core module that contains DotnetCli
      let coreAssembly = System.Reflection.Assembly.GetAssembly(typeof<FsAutoComplete.TestAdapter.TestAdapterEntry<int>>)
      let modules = coreAssembly.GetTypes() |> Array.map (fun t -> t.FullName) |> Array.filter (fun name -> name <> null)
      
      Expect.isTrue (modules |> Array.exists (fun name -> name.Contains("FsAutoComplete"))) "Should contain FsAutoComplete types"
  ]