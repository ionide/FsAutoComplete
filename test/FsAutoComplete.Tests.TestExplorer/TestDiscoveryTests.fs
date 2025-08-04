module TestDiscoveryTests

open Expecto
open FsAutoComplete.TestServer
open Microsoft.VisualStudio.TestPlatform.ObjectModel;
open System.IO
  

let vstestPath = ResourceLocators.tryFindVsTest ()

[<Tests>]
let tests =
  testList "VSTestWrapper Test Discovery" [
    testCase "should return an empty list if given no projects" <| fun () ->
      let expected = []
      let actual = VSTestWrapper.discoverTests vstestPath ignore [] 
      Expect.equal actual expected ""
    
    testCase "should discover tests given a single xunit project" <| fun () -> 
      let expectedTestIds = ["Tests.My test"]
      
      let sources = [
        Path.Combine(ResourceLocators.sampleProjectsRootDir, "VSTest.XUnit.Tests/bin/Debug/net8.0/VSTest.XUnit.Tests.dll")
      ]

      let discovered = VSTestWrapper.discoverTests vstestPath ignore sources
      let actual = discovered |> List.map _.FullyQualifiedName

      Expect.equal actual expectedTestIds ""
  ]
