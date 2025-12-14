module RecursiveExample3 =

  // Async recursive patterns
  let rec asyncFactorial n =
    async {
      if n <= 1 then
        return 1
      else
        let! prev = asyncFactorial (n - 1)
        return n * prev
    }

  // Continuation-passing style
  let rec fibWithContinuation n cont =
    if n <= 1 then
      cont n
    else
      fibWithContinuation (n - 1) (fun fib1 -> fibWithContinuation (n - 2) (fun fib2 -> cont (fib1 + fib2)))

  // Higher-order function chains
  let rec processWithCallbacks input =
    let step1 = transformStep1 input
    let step2 = transformStep2 step1
    let step3 = transformStep3 step2
    finalizeResult step3

  and transformStep1 x = x * 2
  and transformStep2 x = x + 5
  and transformStep3 x = x - 1
  and finalizeResult x = x.ToString()

  // Nested function calls with recursion
  let rec processNestedData data depth =
    if depth <= 0 then
      data
    else
      let processed = processItem data
      let nested = processNestedData processed (depth - 1)
      combineResults data nested

  and processItem item = item + 1
  and combineResults original processed = original + processed

  // Complex call chain with async
  let rec orchestrateWorkflow input =
    async {
      let! step1Result = performStep1 input
      let! step2Result = performStep2 step1Result
      let! finalResult = performFinalStep step2Result
      return finalResult
    }

  and performStep1 x =
    async {
      let! intermediate = asyncFactorial x
      return intermediate * 2
    }

  and performStep2 x =
    async {
      let processed = processNestedData x 3
      return processed
    }

  and performFinalStep x =
    async {
      let result = fibWithContinuation x (fun result -> result)
      return result
    }

