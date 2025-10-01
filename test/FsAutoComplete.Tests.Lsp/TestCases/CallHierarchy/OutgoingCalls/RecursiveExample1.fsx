module RecursiveExample1 =

  // Simple recursive function
  let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1)

  // Mutual recursion
  let rec isEven n = if n = 0 then true else isOdd (n - 1)

  and isOdd n = if n = 0 then false else isEven (n - 1)

  // Function that calls recursive functions
  let calculateValues x =
    let fact = factorial x
    let even = isEven x
    let odd = isOdd x
    (fact, even, odd)

  // Higher-order function with recursive behavior
  let rec processNumbers numbers =
    match numbers with
    | [] -> []
    | head :: tail ->
      let processedHead = factorial head
      processedHead :: processNumbers tail

  // Chain of function calls
  let rec firstLevel x = secondLevel (x + 1)

  and secondLevel x = thirdLevel (x * 2)

  and thirdLevel x = factorial x
