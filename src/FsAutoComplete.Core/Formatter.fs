namespace FsAutoComplete

open Fantomas
open Fantomas.FormatConfig
open System.IO

module Formatter =

    exception InvalidCodeException of unit

    let formatSourceFile reorderOpen pageWidth source = async {
        let config = {FormatConfig.Default with PageWidth = pageWidth; ReorderOpenDeclaration = reorderOpen}
        let originalContent = File.ReadAllText source
        let formattedContent = CodeFormatter.FormatDocument(source, originalContent, config)
        if formattedContent <> originalContent then
            if not <| CodeFormatter.IsValidFSharpCode (source, formattedContent) then
                return raise <| InvalidCodeException()
            else
                return [formattedContent]
        else
            return []}
