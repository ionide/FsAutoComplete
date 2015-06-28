module X =
  let func x = x + 1

let testval = FileTwo.NewObjectType()

let val2 = X.func 2

let val3 = testval.Terrific val2

let val4 : FileTwo.NewObjectType = testval

let shadowed = 
    // This shadowed var shouldn't show up when looking for testval above and vice-versa
    let testval = 123
    testval + 1

[<EntryPoint>]
let main args =
    printfn "Hello %d" val2
    0
