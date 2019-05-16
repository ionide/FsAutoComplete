module FsAutoComplete.Fsdn

open FSharp.Data
open FSharp.Data.JsonExtensions

let query (querystr:string) = 
    let queryString =
        [ "query=" + querystr
          "exclusion="
          "respect_name_difference=enabled"
          "greedy_matching=disabled"
          "ignore_parameter_style=enabled"
          "ignore_case=enabled"
          "substring=enabled"
          "swap_order=enabled"
          "complement=enabled"
          "language=fsharp"
          "single_letter_as_variable=enabled"
          "limit=50"
        ]
        |> String.concat "&"

    let req = Http.RequestString( "https://fsdn.azurewebsites.net/api/search?" + queryString )
    let results = JsonValue.Parse(req)

    let values = results?values.AsArray()
    
    let createResult (v: JsonValue) =
        let info2 = v?api?name
        //return a list of strings
        let infonamespace = info2?``namespace``.AsString()
        let infoclass = info2?class_name.AsString()
        let infomethod = info2?id.AsString()
        let finalresp = infoclass + "." + infomethod
        finalresp 

    values
    |> Array.map createResult
    |> Array.toList

