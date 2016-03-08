// --------------------------------------------------------------------------------------
// (c) Tomas Petricek, http://tomasp.net/blog
// --------------------------------------------------------------------------------------
module FsAutoComplete.TipFormatter

open System
open System.Text
open Microsoft.FSharp.Compiler.SourceCodeServices

// --------------------------------------------------------------------------------------
// Formatting of tool-tip information displayed in F# IntelliSense
// --------------------------------------------------------------------------------------
let private buildFormatComment cmt  =
  match cmt with
  | FSharpXmlDoc.Text s -> s
  // For 'XmlCommentSignature' we could get documentation from 'xml'
  // files, but I'm not sure whether these are available on Mono
  | _ -> ""

let formatTip (FSharpToolTipText tips) = 
    tips
    |> Seq.where (function
                    | FSharpToolTipElement.Single _ | FSharpToolTipElement.Group _ -> true
                    | _ -> false)
    |>  Seq.fold (fun acc t -> match t with
                                | FSharpToolTipElement.Single (it, comment) -> [(it, comment |> buildFormatComment)]::acc
                                | FSharpToolTipElement.Group (items) -> (items |> List.map (fun (it, comment) ->  (it, comment |> buildFormatComment) )) :: acc
                                | _ -> acc) []

let extractSignature (FSharpToolTipText tips) =
    let getSignature (str: string) =
        let nlpos = str.IndexOfAny([|'\r';'\n'|])
        let firstLine =
            if nlpos > 0 then str.[0..nlpos-1]
            else str

        if firstLine.StartsWith("type ", StringComparison.Ordinal) then
            let index = firstLine.LastIndexOf("=", StringComparison.Ordinal)
            if index > 0 then firstLine.[0..index-1]
            else firstLine
        else firstLine

    let firstResult x =
        match x with
        | FSharpToolTipElement.Single (t, _) when not (String.IsNullOrWhiteSpace t) -> Some t
        | FSharpToolTipElement.Group gs -> List.tryPick (fun (t, _) -> if not (String.IsNullOrWhiteSpace t) then Some t else None) gs
        | _ -> None

    tips
    |> Seq.tryPick firstResult
    |> Option.map getSignature
    |> Option.getOrElse ""
