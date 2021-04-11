open System

/// Tries to cast an object to a given type; throws with the given error message if it fails.
let tryConvert<'T> (descriptor: string) (value: obj) : 'T =
    try
        Convert.ChangeType(value, typeof<'T>) :?> 'T
    with ex ->
        let msg = sprintf "Unable to convert '%s' with value %A" descriptor value
        raise (new Exception(msg, ex))

// Tooltip does NOT show when the generic argument is present:
let result = (box 123) |> tryConvert<string> (* trigger at > char, no sigdata occurs *)
