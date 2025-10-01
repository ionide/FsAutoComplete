// Simple integration test for CallHierarchy OutgoingCalls
// This file can be used to manually test the implementation

let helper () = printfn "Helper called"
let compute x y = x + y

let testFunction () =
  helper () // Should be detected as outgoing call
  let result = compute 5 10 // Should be detected as outgoing call
  printfn "Result: %d" result // Should be detected as outgoing call
  result

// Position to test: line 7 (testFunction definition)
// Expected outgoing calls: helper, compute, printfn
