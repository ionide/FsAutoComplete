namespace Benchmarks
open System
open BenchmarkDotNet
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open System.Security.Cryptography




module EntryPoint =

  [<EntryPoint>]
  let main argv =
    let summary = BenchmarkRunner.Run<SourceText_LineChanges_Benchmarks>();
    0
