let mySum xs acc =
    match xs with
    | [] -> acc
    | _ :: tail ->
        mySum tail (acc + 1)

