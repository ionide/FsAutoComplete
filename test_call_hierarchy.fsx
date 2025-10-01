// Test file to check CallHierarchy results
module TestCallHierarchy

let helper x = x + 1

type AwaitAwaitable() =
  member this.AwaitAwaiter() =
    printfn "AwaitAwaiter called"
    async { return () }

  member this.GetAwaiter() =
    printfn "GetAwaiter called"
    async { return () }

let testFunction () =
  let awaitable = AwaitAwaitable()
  awaitable.AwaitAwaiter() |> ignore
  awaitable.GetAwaiter() |> ignore
  helper 5 |> ignore
