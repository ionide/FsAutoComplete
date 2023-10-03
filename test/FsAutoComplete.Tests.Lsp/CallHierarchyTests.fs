module FsAutoComplete.Tests.CallHierarchy
open Expecto
open FSharp.Compiler.Syntax
open FSharp.Compiler
open FSharp.Compiler.CodeAnalysis
open System.IO
open FsAutoComplete
open Ionide.ProjInfo.ProjectSystem
open FSharp.Compiler.Text

let examples =  Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "CallHierarchy")
let sourceFactory : ISourceTextFactory = RoslynSourceTextFactory()
let resultGet = function | Ok x -> x | Error e -> failwith e


let tests =
  testList "CallHierarchy" [
    testCaseAsync "test" <| async {
      let checker = FSharpCompilerServiceChecker(false, 100L, true)
      let example1Name = Path.Combine(examples, "example1.fsx")
      let exampleNameNormalized = Utils.normalizePath  example1Name
      let! example1File = File.ReadAllTextAsync(example1Name) |> Async.AwaitTask
      let source = sourceFactory.Create(exampleNameNormalized, example1File)
      let! opts = checker.GetProjectOptionsFromScript(exampleNameNormalized, source, FSIRefs.TFM.NetCore)

      let! checkResultsR = checker.ParseAndCheckFileInProject(exampleNameNormalized, 0, source, opts, false)
      let checkResults = resultGet checkResultsR
      let lol = checkResults.GetParseResults.TryRangeOfNameOfNearestOuterBindingContainingPos (Position.mkPos 9 14)
      let tree = checkResults.GetAST
      let visitor =
          { new SyntaxVisitorBase<_>() with
              override this.VisitBinding(path, defaultTraverse, synBinding) =
                  defaultTraverse synBinding
              // override this.VisitExpr(path, traverseSynExpr, defaultTraverse, synExpr) =

              //     defaultTraverse synExpr
        }

      let result =
          SyntaxTraversal.Traverse(Position.pos0, tree, visitor) //
      printfn "%A" result
    }
  ]
