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

let formatTip tip = 
  match tip with
  | FSharpToolTipText tips -> tips |> Seq.where (function
                                                 | FSharpToolTipElement.Single _ | FSharpToolTipElement.Group _ -> true
                                                 | _ -> false)
                                   |>  Seq.fold (fun acc t -> match t with
                                                              | FSharpToolTipElement.Single (it, comment) -> [(it, comment |> buildFormatComment)]::acc
                                                              | FSharpToolTipElement.Group (items) -> (items |> List.map (fun (it, comment) ->  (it, comment |> buildFormatComment) )) :: acc
                                                              | _ -> acc) []




let f x =
    match x with
    | FSharpToolTipElement.Single (t, _) when not (String.IsNullOrWhiteSpace t) -> Some t
    | FSharpToolTipElement.Group gs -> List.tryPick (fun (t, _) -> if not (String.IsNullOrWhiteSpace t) then Some t else None) gs
    | _ -> None

let format (str: string) =
    if str.StartsWith("type ", StringComparison.Ordinal) then
        let index = str.LastIndexOf("=", StringComparison.Ordinal)
        if index > 0 then
            str.[0..index-1]
        else str
    else str

let extractSignature tip =
  match tip with
  | FSharpToolTipText tips -> tips
                              |> Seq.tryPick f
                              |> Option.map format
                              |> Option.getOrElse ""
