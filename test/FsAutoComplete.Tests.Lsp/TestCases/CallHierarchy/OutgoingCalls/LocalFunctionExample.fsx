module LocalFunctionExample

let outerFunction x =
  // Local function definition
  let localHelper y = y * 2

  // Another local function that calls the first
  let localProcessor z =
    let doubled = localHelper z
    doubled + 1

  // Nested local function
  let nestedOuter a =
    let nestedInner b = b + 10
    nestedInner a + localHelper a

  // Using local functions - these should be detected as outgoing calls
  let result1 = localHelper x
  let result2 = localProcessor x
  let result3 = nestedOuter x

  result1 + result2 + result3

ignore outerFunction
