module PropertyExample

type Counter() =
  let mutable count = 0

  member _.Value
    with get () = count
    and set v = count <- v

  member _.Increment() = count <- count + 1

  member _.GetDouble() = count * 2

type Person = { mutable Name: string; mutable Age: int }

let createPerson name age = { Name = name; Age = age }

let main () =
  // Constructor call
  let counter = Counter()

  // Property getter
  let initialValue = counter.Value

  // Property setter
  counter.Value <- 10

  // Method calls
  counter.Increment()
  let doubled = counter.GetDouble()

  // Record creation and property access
  let person = createPerson "John" 30
  let personName = person.Name
  person.Age <- 31

  initialValue + doubled + person.Age

ignore main
