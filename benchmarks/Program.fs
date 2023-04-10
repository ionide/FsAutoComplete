namespace Benchmarks
open System
open BenchmarkDotNet
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open System.Security.Cryptography

// [<MemoryDiagnoser>]
// type HelloBenchmark () =

//   let n = 10000
//   let data : byte array = Array.zeroCreate n
//   do Random().NextBytes(data)

//   let md5 = MD5.Create()
//   let sha256 = SHA256.Create()


//   [<Benchmark>]
//   member _.MD5 () =
//     md5.ComputeHash(data)

//   [<Benchmark>]
//   member _.Sha256 () =
//     sha256.ComputeHash(data)



module EntryPoint =

  [<EntryPoint>]
  let main argv =
    let summary = BenchmarkRunner.Run<SourceText_LineChanges_Benchmarks>();
    // let summary = BenchmarkRunner.Run<SourceText_TextChanges_Benchmarks>();
    0
