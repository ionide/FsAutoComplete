module FsAutoComplete.Fsdn

open Newtonsoft.Json


let private httpClient = new System.Net.Http.HttpClient()

type Response =
  {| values:
       {| api:
            {| name:
                 {| ``namespace``: string
                    class_name: string
                    id: string |} |} |}[] |}

let query (queryStr: string) =
  async {

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
      [ "query", queryStr
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

    let! req =
      httpClient.GetStringAsync("https://fsdn.azurewebsites.net/api/search?" + queryString)
      |> Async.AwaitTask

    let results = JsonConvert.DeserializeObject<Response> req


    return
      results.values
      |> Array.map (fun v ->

        let info2 = v.api.name
        //return a list of strings
        let infoNamespace = info2.``namespace``
        let infoClass = info2.class_name
        let infoMethod = info2.id

        let removeGenerics (s: string) =
          if not (s.Contains("<")) then
            s
          else
            s.Substring(0, s.IndexOf('<'))

        let finalResponse = infoClass + "." + (removeGenerics infoMethod)
        finalResponse)
      |> Array.toList
  }
