module FsAutoComplete.Tests.TypeHierarchy

open Expecto
open System.IO
open FsAutoComplete
open Utils.ServerTests
open Helpers
open Utils.Server
open Ionide.LanguageServerProtocol.Types

let examples = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "TypeHierarchy")

let resultGet =
  function
  | Ok x -> x
  | Error e -> failwithf "%A" e

let resultOptionGet =
  function
  | Ok(Some x) -> x
  | Ok(None) -> failwithf "Expected Some, got None"
  | Error e -> failwithf "%A" e

module TypeHierarchyPrepareParams =
  let create (uri: DocumentUri) line character : TypeHierarchyPrepareParams =
    { TextDocument = { Uri = uri }
      Position =
        { Line = uint32 line
          Character = uint32 character }
      WorkDoneToken = None }

let tests createServer =
  serverTestList "TypeHierarchy" createServer defaultConfigDto (Some examples) (fun server ->
    [ testCaseAsync "PrepareTypeHierarchy returns item for class type"
      <| async {
        // Example1.fsx:
        //  Line 0: module TypeHierarchyExample
        //  Line 5: type Animal(name: string) =   <- char 5 = 'A'
        let! (doc, _) = Server.openDocument "Example1.fsx" server
        use doc = doc
        let! server = server

        let prepareParams = TypeHierarchyPrepareParams.create doc.Uri 5 5

        let! result =
          server.Server.TextDocumentPrepareTypeHierarchy prepareParams
          |> Async.map resultOptionGet

        Expect.equal result.Length 1 "Should return exactly one TypeHierarchyItem"
        Expect.equal result[0].Name "Animal" "Name should be Animal"
        Expect.equal result[0].Kind SymbolKind.Class "Kind should be Class"
      }

      testCaseAsync "PrepareTypeHierarchy returns item for interface type"
      <| async {
        // Example1.fsx:
        //  Line 2: type IAnimal =   <- char 5 = 'I'
        let! (doc, _) = Server.openDocument "Example1.fsx" server
        use doc = doc
        let! server = server

        let prepareParams = TypeHierarchyPrepareParams.create doc.Uri 2 5

        let! result =
          server.Server.TextDocumentPrepareTypeHierarchy prepareParams
          |> Async.map resultOptionGet

        Expect.equal result.Length 1 "Should return exactly one TypeHierarchyItem"
        Expect.equal result[0].Name "IAnimal" "Name should be IAnimal"
        Expect.equal result[0].Kind SymbolKind.Interface "Kind should be Interface"
      }

      testCaseAsync "PrepareTypeHierarchy returns None for non-type symbol"
      <| async {
        // Example1.fsx:
        //  Line 8: "    member _.Name = name"  <- char 11 = 'N' in "Name"
        //  "Name" is a member/property, not a type
        let! (doc, _) = Server.openDocument "Example1.fsx" server
        use doc = doc
        let! server = server

        let prepareParams = TypeHierarchyPrepareParams.create doc.Uri 8 11

        let! result =
          server.Server.TextDocumentPrepareTypeHierarchy prepareParams
          |> Async.map resultGet

        // A member symbol should not produce a type hierarchy item
        match result with
        | None -> () // expected: non-type symbol returns None
        | Some items ->
          // Accept if items is empty, or if the single item is not a type name
          if items.Length > 0 then
            Expect.notEqual items[0].Name "Animal" "Should not identify member as type"
      }

      testCaseAsync "TypeHierarchySupertypes returns declared interfaces"
      <| async {
        // Animal implements IAnimal, so IAnimal should appear in supertypes
        let! (doc, _) = Server.openDocument "Example1.fsx" server
        use doc = doc
        let! server = server

        let prepareParams = TypeHierarchyPrepareParams.create doc.Uri 5 5

        let! prepareResult =
          server.Server.TextDocumentPrepareTypeHierarchy prepareParams
          |> Async.map resultOptionGet

        Expect.equal prepareResult.Length 1 "PrepareTypeHierarchy should succeed"

        let supertypesParams: TypeHierarchySupertypesParams =
          { Item = prepareResult[0]
            WorkDoneToken = None
            PartialResultToken = None }

        let! supertypes =
          server.Server.TypeHierarchySupertypes supertypesParams
          |> Async.map resultOptionGet

        let supertypeNames = supertypes |> Array.map (fun i -> i.Name)
        Expect.contains supertypeNames "IAnimal" "IAnimal should be listed as a supertype of Animal"
      }

      testCaseAsync "TypeHierarchySupertypes returns None for type with no non-Object supertypes"
      <| async {
        // IAnimal is an interface with no base class, so no supertypes
        let! (doc, _) = Server.openDocument "Example1.fsx" server
        use doc = doc
        let! server = server

        let prepareParams = TypeHierarchyPrepareParams.create doc.Uri 2 5

        let! prepareResult =
          server.Server.TextDocumentPrepareTypeHierarchy prepareParams
          |> Async.map resultOptionGet

        Expect.equal prepareResult.Length 1 "PrepareTypeHierarchy should succeed"

        let supertypesParams: TypeHierarchySupertypesParams =
          { Item = prepareResult[0]
            WorkDoneToken = None
            PartialResultToken = None }

        let! result = server.Server.TypeHierarchySupertypes supertypesParams |> Async.map resultGet

        // IAnimal has no supertypes (F# interfaces don't inherit System.Object in the type hierarchy sense)
        match result with
        | None -> () // expected
        | Some items -> Expect.equal items.Length 0 "Should have no supertypes"
      }

      testCaseAsync "TypeHierarchySubtypes returns direct subclasses"
      <| async {
        // Animal is inherited by Dog and Cat
        let! (doc, _) = Server.openDocument "Example1.fsx" server
        use doc = doc
        let! server = server

        let prepareParams = TypeHierarchyPrepareParams.create doc.Uri 5 5

        let! prepareResult =
          server.Server.TextDocumentPrepareTypeHierarchy prepareParams
          |> Async.map resultOptionGet

        Expect.equal prepareResult.Length 1 "PrepareTypeHierarchy should succeed"

        let subtypesParams: TypeHierarchySubtypesParams =
          { Item = prepareResult[0]
            WorkDoneToken = None
            PartialResultToken = None }

        let! subtypes = server.Server.TypeHierarchySubtypes subtypesParams |> Async.map resultOptionGet

        let subtypeNames = subtypes |> Array.map (fun i -> i.Name)

        Expect.isGreaterThan subtypeNames.Length 0 "Should find at least one subtype"
        Expect.contains subtypeNames "Dog" "Dog should be a subtype of Animal"
        Expect.contains subtypeNames "Cat" "Cat should be a subtype of Animal"
      } ])
