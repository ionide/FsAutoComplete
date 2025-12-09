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

let findFeatNumber branch =
    match branch with
    | ParseRegex @"(?i)(FEAT-\d+)-.*" [num] -> num
//    ^^^^^^^^^^ Usage of parameterized partial active pattern
    | _ -> branch

let testStructPattern input =
    match input with
    | ParseIntStruct i -> printfn $"Got struct int: {i}"
//    ^^^^^^^^^^^^^^ Usage of Struct partial active pattern
    | _ -> printfn "Not an int"

[<EntryPoint>]
let main argv =
  test1
  0
