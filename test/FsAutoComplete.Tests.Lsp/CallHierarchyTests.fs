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

let examples =
  Path.Combine(Helpers.Paths.SourceDirectory(), "TestCases", "CallHierarchy")

let incomingExamples = Path.Combine(examples, "IncomingCalls")
let sourceFactory: ISourceTextFactory = RoslynSourceTextFactory()

let resultGet =
  function
  | Ok x -> x
  | Error e -> failwithf "%A" e

let resultOptionGet =
  function
  | Ok(Some x) -> x
  | Ok(None) -> failwithf "Expected Some, got None"
  | Error e -> failwithf "%A" e

module CallHierarchyPrepareParams =
  let create (uri: DocumentUri) line character : CallHierarchyPrepareParams =
    { TextDocument = { Uri = uri }
      Position =
        { Character = uint32 character
          Line = uint32 line }
      WorkDoneToken = None }

module LspRange =
  let create startLine startCharacter endLine endCharacter : Range =
    { Start =
        { Character = uint32 startCharacter
          Line = uint32 startLine }
      End =
        { Character = uint32 endCharacter
          Line = uint32 endLine } }


let incomingTests createServer =
  serverTestList "IncomingTests" createServer defaultConfigDto (Some incomingExamples) (fun server ->
    [ testCaseAsync "Example1"
      <| async {
        let! (aDoc, _) = Server.openDocument "Example1.fsx" server
        use aDoc = aDoc
        let! server = server

        let prepareParams = CallHierarchyPrepareParams.create aDoc.Uri 2u 9u

        let! prepareResult =
          server.Server.TextDocumentPrepareCallHierarchy prepareParams
          |> Async.map resultOptionGet

        let expectedPrepareResult: CallHierarchyItem array =
          [| { Data = None
               Detail = None
               Kind = SymbolKind.Function
               Name = "bar"
               Range = LspRange.create 2 8 2 11
               SelectionRange = LspRange.create 2 8 2 11
               Tags = None
               Uri = aDoc.Uri } |]

        Expect.equal prepareResult expectedPrepareResult "prepareResult"

        let expectedIncomingResult: CallHierarchyIncomingCall array =
          [| { FromRanges = [| LspRange.create 8 12 8 15 |]
               From =
                 { Data = None
                   Detail = Some "From Example1.fsx"
                   Kind = SymbolKind.Function
                   Name = "foo"
                   Range = LspRange.create 6 12 8 18
                   SelectionRange = LspRange.create 6 12 6 15
                   Tags = None
                   Uri = aDoc.Uri } } |]

        let incomingParams: CallHierarchyIncomingCallsParams =
          { Item = prepareResult[0]
            PartialResultToken = None
            WorkDoneToken = None }

        let! incomingResult =
          server.Server.CallHierarchyIncomingCalls incomingParams
          |> Async.map resultOptionGet

        Expect.equal incomingResult expectedIncomingResult "incomingResult"
      } ])


let tests createServer = testList "CallHierarchy" [ incomingTests createServer ]
