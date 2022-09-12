#r "nuget: FSharp.Data.Adaptive"
open System
open FSharp.Data.Adaptive
open System.Collections.Concurrent

// module ASet =
//   let mapAtoAMap mapper src =
//     src |> ASet.mapToAMap mapper |> AMap.mapA (fun _ v -> v)


// let adaptive_add =
//   let inputs : cset<int*int> = cset []
//   let calculation =
//     inputs
//     |> ASet.mapAtoAMap(fun (x,y) ->
//       aval {
//         return add x y
//       }
//     )
//   fun x y ->
//     transact(fun () ->
//       let added = inputs.Add(x,y)
//       printfn $"added {x} {y} : {added}"
//     )
//     calculation
//     |> AMap.find((x,y))

// adaptive_add 3 4 |> AVal.force
// adaptive_add 3 5 |> AVal.force


// let adaptive_calculate runner =
//   let inputs : cset<int*int> = cset []
//   let calculation =
//     inputs
//     |> ASet.mapAtoAMap(fun (x,y) ->
//       aval {
//         let! runner = runner
//         return runner x y
//       }
//     )
//   fun x y ->
//     transact(fun () ->
//       let added = inputs.Add(x,y)
//       printfn $"added {x} {y} : {added}"
//     )
//     calculation
//     |> AMap.filter(fun key _ -> key = (x,y))
//     |> AMap.toAVal

// let runner = cval add

// let currentCalculator = adaptive_calculate runner

// currentCalculator 3 4 |> AVal.force
// currentCalculator 3 5 |> AVal.force
// currentCalculator 3 6 |> AVal.force

// transact(fun () -> runner.Value <- add)
// transact(fun () -> runner.Value <- sub)



let add x y =
  printfn $"calculating {x} + {y}"
  x + y


let sub x y =
  printfn $"calculating {x} - {y}"
  x - y


let adaptive_calculate2 (runner : aval<int -> int -> int>) =
  let inputs = ConcurrentDictionary<int*int, _>()
  fun x y ->
    let factory (x,y) =
      aval {
          let! runner = runner
          return runner x y
        }
    inputs.GetOrAdd((x,y), valueFactory= factory)

let runner2 = cval add

let currentCalculator2 = adaptive_calculate2 runner2

currentCalculator2 3 4 |> AVal.force
currentCalculator2 3 5 |> AVal.force
currentCalculator2 3 6 |> AVal.force

transact(fun () -> runner2.Value <- add)
transact(fun () -> runner2.Value <- sub)


Type Config = {
  Port : int
}
