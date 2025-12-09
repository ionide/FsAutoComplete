open ActivePatterns

let test1 =
  match "42" with
  | ParseInt i -> printfn $"Got int: {i}"
//  ^^^^^^^^ Usage in test1
  | _ -> printfn "Not an int"

let test2 x =
  async {
    match x with
    | ParseInt i -> return i
//    ^^^^^^^^ Usage in test2 async
    | _ -> return 0
  }

[<EntryPoint>]
let main argv =
  test1
  0
