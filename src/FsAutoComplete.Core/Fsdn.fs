module FsAutoComplete.Fsdn

open FSharp.Data
open FSharp.Data.JsonExtensions

let query (querystr: string) =

  let exclusionList =
    [ "System.Net.Http"
      "System.Web"
      "System.Xml"
      "System.Xml.Linq"
      "Argu"
      "ExtCore"
      "Fake"
      "FParsec"
      "FSharp.Collections.ParallelSeq"
      "FSharp.Compiler.Service"
      "FSharp.Control.AsyncSeq"
      "FSharp.Control.Reactive"
      "FSharp.Data"
      "FSharp.ViewModule.Core"
      "FsPickler"
      "FsUnit"
      "Newtonsoft.Json"
      "Suave"
      "System.Reactive" ]

  let queryString =
    [ "query", querystr
      "exclusion", (exclusionList |> String.concat "+")
      "respect_name_difference", "enabled"
      "greedy_matching", "enabled"
      "ignore_parameter_style", "enabled"
      "ignore_case", "enabled"
      "substring", "enabled"
      "swap_order", "enabled"
      "complement", "enabled"
      "language", "fsharp"
      "single_letter_as_variable", "enabled"
      "limit", "50" ]
    |> List.map (fun (k, v) -> k, System.Uri.EscapeDataString(v))
    |> List.map (fun (k, v) -> sprintf "%s=%s" k v)
    |> String.concat "&"

  // the FSharp.Data Http.RequestString doesnt escape correctly the query args if these are passed in the `query` function argument
  // for example the `+` is not escaped as `%2B`
  // as workaround all query args are passed in the `url` function argument
  let req =
    Http.RequestString(
      "https://fsdn.azurewebsites.net/api/search?"
      + queryString
    )

  let results = JsonValue.Parse(req)

  let values = results?values.AsArray()

  let createResult (v: JsonValue) =
    let info2 = v?api?name
    //return a list of strings
    let infonamespace = info2?``namespace``.AsString()
    let infoclass = info2?class_name.AsString()
    let infomethod = info2?id.AsString()

    let removeGenerics (s: string) =
      if not (s.Contains("<")) then
        s
      else
        s.Substring(0, s.IndexOf('<'))

    let finalresp =
      infoclass + "." + (removeGenerics infomethod)

    finalresp

  values |> Array.map createResult |> Array.toList
