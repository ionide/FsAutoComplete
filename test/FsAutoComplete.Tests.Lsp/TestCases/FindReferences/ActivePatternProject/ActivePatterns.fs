module ActivePatterns

let (|ParseInt|_|) (str: string) =
//>  ^^^^^^^^^^^^^ Partial Active Pattern definition
  let success, i = System.Int32.TryParse str
  if success then Some i else None
