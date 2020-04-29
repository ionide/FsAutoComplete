#r "nuget: Newtonsoft.Json"
open Newtonsoft.Json

let x = [1..10]
let z = JsonConvert.SerializeObject x