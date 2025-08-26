// Object methods and property access patterns
module MethodCallExample =
    
    type Person = {
        Name: string
        Age: int
    }
    
    type Calculator() =
        member this.Add x y = x + y
        member this.Multiply x y = x * y
        static member StaticAdd x y = x + y
    
    let createPerson name age = { Name = name; Age = age }
    
    let processPersons persons =
        persons |> List.map (fun p -> p.Name)
    
    let calculateValues calculator =
        let sum = calculator.Add 5 10
        let product = calculator.Multiply 3 4
        sum + product
    
    // Usage scenarios
    let person1 = createPerson "Alice" 25
    let person2 = createPerson "Bob" 30
    let allPersons = [person1; person2]
    let names = processPersons allPersons
    
    let calc = Calculator()
    let result = calculateValues calc
    let staticResult = Calculator.StaticAdd 1 2