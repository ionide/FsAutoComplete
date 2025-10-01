module OutgoingExample2 =

  type Person = { Name: string; Age: int }

  let createPerson name age = { Name = name; Age = age }

  type Calculator() =
    member _.Add(x, y) = x + y
    member _.Multiply(x, y) = x * y

  let complexFunction () =
    // Constructor call
    let calc = Calculator()

    // Method calls
    let sum = calc.Add(5, 10)
    let product = calc.Multiply(sum, 2)

    // Function call with record creation
    let person = createPerson "John" 30

    // Nested function calls
    let result = calc.Add(calc.Multiply(2, 3), 4)

    result

  ignore complexFunction
