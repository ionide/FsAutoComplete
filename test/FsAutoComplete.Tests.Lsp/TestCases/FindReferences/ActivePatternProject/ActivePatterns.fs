module ActivePatterns

open System.Text.RegularExpressions

let (|ParseInt|_|) (str: string) =
//>  ^^^^^^^^^^^^^ Partial Active Pattern definition
  let success, i = System.Int32.TryParse str
  if success then Some i else None

let (|ParseRegex|_|) regex str =
//>  ^^^^^^^^^^^^^ Partial Active Pattern with parameters
    let m = Regex(regex).Match(str)
    if m.Success then
        Some(List.tail [ for x in m.Groups -> x.Value ])
    else
        None

/// Partial active pattern with [<return: Struct>] attribute returning ValueOption
[<return: Struct>]
let (|ParseIntStruct|_|) (str: string) =
//>  ^^^^^^^^^^^^^^^^^^^ Partial Active Pattern with Struct attribute
    let success, i = System.Int32.TryParse str
    if success then ValueSome i else ValueNone
