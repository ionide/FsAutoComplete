module X =
  let func x = x + 1

let testval = FileTwo.NewObjectType()

let val2 = X.func 2

let val3 = testval.Terrific val2

let val4 : FileTwo.NewObjectType = testval

let val5 = ClassLib.ExternalType.SimpleMethod()

let val6 = ClassLib.ExternalType.SimpleMethod("a", "b")

let mutable lst : System.Collections.Generic.IList<string> = null

let val7 = ClassLib.ExternalType.RefParameter(ref lst)

let val8 = ClassLib.ExternalType.Generic(System.Collections.Generic.Dictionary<string, string>())

[<EntryPoint>]
let main args =
    printfn "Hello %d" val2
    0
