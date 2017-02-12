namespace FsAutoComplete

open Fantomas
open Fantomas.FormatConfig
open System.IO

module Formatter =

    exception InvalidCodeException of SourceFile:string
        with override x.ToString() = sprintf "File %s contains invalid F# code." x.SourceFile

    let formatSourceFile reorderOpen pageWidth source = async {
        let config = {FormatConfig.Default with PageWidth = pageWidth; ReorderOpenDeclaration = reorderOpen}
        let originalContent = File.ReadAllText source
        let formattedContent = CodeFormatter.FormatDocument(source, originalContent, config)
        if formattedContent <> originalContent then
            if not <| CodeFormatter.IsValidFSharpCode (source, formattedContent) then
                return source |> InvalidCodeException |> raise
            else
                return [formattedContent]
        else
            return []}
