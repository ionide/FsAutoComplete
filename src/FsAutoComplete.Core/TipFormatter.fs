// --------------------------------------------------------------------------------------
// (c) Tomas Petricek, http://tomasp.net/blog
// --------------------------------------------------------------------------------------
module FsAutoComplete.TipFormatter

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

let extractSignature tip =
  match tip with
  | FSharpToolTipText tips -> tips |> Seq.where (function
                                                 | FSharpToolTipElement.Single _ | FSharpToolTipElement.Group _ -> true
                                                 | _ -> false)
                                   |>  Seq.fold (fun acc t -> match t with
                                                              | FSharpToolTipElement.Single (it, _) -> [ it ]::acc
                                                              | FSharpToolTipElement.Group (items) -> (items |> List.map (fun (it, _) ->  it )) :: acc
                                                              | _ -> acc) []
