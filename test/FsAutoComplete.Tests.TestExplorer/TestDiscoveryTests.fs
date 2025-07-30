module TestDiscoveryTests

open Expecto
open FsAutoComplete.TestServer
open Microsoft.VisualStudio.TestPlatform.ObjectModel;
open System.IO


let tryFindVsTest () : string =
  let dotnetBinary =  
    Ionide.ProjInfo.Paths.dotnetRoot.Value
    |> Option.defaultWith (fun () -> failwith "Couldn't find dotnet root. The dotnet sdk must be installed to run these tests")

  let cwd = System.Environment.CurrentDirectory |> Some
  
  VSTestWrapper.tryFindVsTestFromDotnetRoot dotnetBinary.FullName cwd
  |> Result.defaultWith failwith
  |> _.FullName
  

let vstestPath = tryFindVsTest ()

[<Tests>]
let tests =
  testList "VSTestWrapper Test Discovery" [
    testCase "should return an empty list if given no projects" <| fun () ->
      let expected = []
      let actual = VSTestWrapper.discoverTests vstestPath ignore [] 
      Expect.equal actual expected ""
    
    testCase "should discover tests given a single xunit project" <| fun () -> 
      let expectedTestIds = ["Tests.My test"]
      
      let sourceDir = __SOURCE_DIRECTORY__
      let sources = [
        Path.Combine(sourceDir, "SampleTestProjects/VSTest.XUnit.Tests/bin/Debug/net8.0/VSTest.XUnit.Tests.dll")
      ]

      let discovered = VSTestWrapper.discoverTests vstestPath ignore sources
      let actual = discovered |> List.map _.FullyQualifiedName

      Expect.equal actual expectedTestIds ""
  ]
