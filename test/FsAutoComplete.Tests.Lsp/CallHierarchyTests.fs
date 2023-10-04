module FsAutoComplete.Tests.CallHierarchy
open Expecto
open FSharp.Compiler.Syntax
open FSharp.Compiler
open FSharp.Compiler.CodeAnalysis
open System.IO
open FsAutoComplete
open Ionide.ProjInfo.ProjectSystem
open FSharp.Compiler.Text
open Utils.ServerTests
open Helpers
open Utils.Server
open Ionide.LanguageServerProtocol.Types

let examples =  Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "CallHierarchy")
let sourceFactory : ISourceTextFactory = RoslynSourceTextFactory()
let resultGet = function | Ok x -> x | Error e -> failwithf "%A" e
let resultOptionGet =
  function
  | Ok (Some x) -> x
  | Ok (None) -> failwithf "Expected Some, got None"
  | Error e -> failwithf "%A" e

module CallHierarchyPrepareParams =
  let create (uri : DocumentUri) (line : int) (character : int) : CallHierarchyPrepareParams =
    {
      TextDocument = { Uri = uri }
      Position = { Character = character; Line = line }
    }

module LspRange =
  let create (startLine : int) (startCharacter : int) (endLine : int) (endCharacter : int) : Range =
    {
      Start = { Character = startCharacter; Line = startLine }
      End = { Character = endCharacter; Line = endLine }
    }


let incomingTests createServer =
  serverTestList "IncomingTests" createServer defaultConfigDto (Some examples) (fun server -> [
      testCaseAsync "Example1" <| async {
        let! (aDoc, _) = Server.openDocument "Example1.fsx" server
        use aDoc = aDoc
        let! server = server

        let prepareParams = CallHierarchyPrepareParams.create aDoc.Uri 2 9
        let! prepareResult = server.Server.TextDocumentPrepareCallHierarchy prepareParams |> Async.map resultOptionGet

        let expectedPrepareResult : HierarchyItem array = [|
          {
            Data = None
            Detail = None
            Kind = SymbolKind.Function
            Name = "bar"
            Range =  LspRange.create 2 8 2 11
            SelectionRange = LspRange.create 2 8 2 11
            Tags = None
            Uri = aDoc.Uri
          }
        |]

        Expect.equal prepareResult expectedPrepareResult "prepareResult"

        let expectedIncomingResult : CallHierarchyIncomingCall array =
          [|
            {
              FromRanges =   [|
                LspRange.create 8 12 8 15
              |]
              From = {
                Data = None
                Detail = Some "From Example1.fsx"
                Kind = SymbolKind.Function
                Name = "foo"
                Range = LspRange.create 6 12 8 18
                SelectionRange = LspRange.create 6 12 6 15
                Tags = None
                Uri = aDoc.Uri
              }
            }
          |]

        let incomingParams : CallHierarchyIncomingCallsParams = {
          Item = prepareResult[0]
        }
        let! incomingResult = server.Server.CallHierarchyIncomingCalls incomingParams |> Async.map resultOptionGet

        Expect.equal incomingResult expectedIncomingResult "incomingResult"
      }
    ])


let tests createServer =
  ftestList "CallHierarchy" [
    incomingTests createServer
  ]
