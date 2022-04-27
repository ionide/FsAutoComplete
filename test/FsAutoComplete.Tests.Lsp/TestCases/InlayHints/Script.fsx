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
// are don't throw anymore. we _should_ show a parameter name hint for uses of this function
let doThing (someName: string) = ()
doThing "blah"

// shows that functions that use well-known parameter names like 'format' don't show hints
let s = sprintf "thing %s" "blah"

// shows that explicit type bindings don't results in a type hint
let s': string = ""

// shows that no parameter name hint is shown for a function with a short parameter name
let someFunction s = s
let noHintForShortParameter = someFunction "hi"

// shows that no hints are shown with a parameter name that matches the argument text
let anotherFunction (kind: string) = ()
let kind = "hi"
anotherFunction kind
