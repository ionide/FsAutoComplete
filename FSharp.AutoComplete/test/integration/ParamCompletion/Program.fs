module X =
  let func x = x + 1

let testval = FileTwo.NewObjectType()

let val2 = X.func(2)

let val3 = testval.Terrific(val2, 'c')

let val4 = System.DateTime.Parse("hello")

let v5 = System.DateTime.Parse(

          "hello"

   )


[<EntryPoint>]
let main args =
    printfn "Hello %d" val2
    0
