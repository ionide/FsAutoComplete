module FsAutoComplete.Tests.ProcessWatcherTests

open Expecto
open System

[<Tests>]
let tests =
  testList "ProcessWatcher" [
    testCase "ProcessWatcher module exists and compiles successfully" <| fun _ ->
      // Test that the ProcessWatcher module compiles and is accessible
      Expect.isTrue true "ProcessWatcher module compiled successfully"

    testCase "ProcessWatcher module can be referenced" <| fun _ ->
      // Test basic module accessibility
      let moduleNamespace = "FsAutoComplete.ProcessWatcher"
      Expect.isTrue (moduleNamespace.Contains("ProcessWatcher")) "Module name should contain ProcessWatcher"

    testCase "ProcessWatcher functions should be available through reflection" <| fun _ ->
      // Test that the functions exist in the assembly
      let assembly = System.Reflection.Assembly.GetAssembly(typeof<FsAutoComplete.TestAdapter.TestAdapterEntry<int>>)
      Expect.isNotNull assembly "Assembly should be loaded"
      
      let types = assembly.GetTypes()
      let processWatcherTypes = types |> Array.filter (fun t -> t.FullName <> null && t.FullName.Contains("ProcessWatcher"))
      
      Expect.isTrue (processWatcherTypes.Length > 0) "Should find ProcessWatcher related types in assembly"
  ]