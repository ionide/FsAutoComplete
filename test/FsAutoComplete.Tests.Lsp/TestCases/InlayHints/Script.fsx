open System.IO

// shows function let-binding type annotations
let tryFindFile p =
    // shows value let-binding type annotations
    let f = FileInfo p // constructor parameter name annotations

    if f.Exists then Some f else None

// when method parameter name annotations are enabled, this will cause a break in our tests
System.Environment.GetEnvironmentVariable "Blah"
|> ignore

// shows that sprintf-like functions with only one 'parameter' (in this case the format)
// are don't throw anymore
let s = sprintf "thing %s" "blah"
