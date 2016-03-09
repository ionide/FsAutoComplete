module X =
  let func x = x + 1

type MyDateTime() =
    static member Parse(s: string) = ()
    static member Parse(s: string, y: int) = ()
    static member Parse(s: string, z: MyDateTime) = ()

let testval = FileTwo.NewObjectType()

let val2 = X.func(2)

let val3 = testval.Terrific(val2, 'c')

let val4 = MyDateTime.Parse("hello")

let v5 = MyDateTime.Parse(

          "hello"

   )


[<EntryPoint>]
let main args =
    printfn "Hello %d" val2
    0
