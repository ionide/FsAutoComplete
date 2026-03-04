module FsAutoComplete.CodeFix.IntroduceMissingBinding

open FsAutoComplete.CodeFix.Types

val title: name: string -> string
val fix: getParseResultsForFile: GetParseResultsForFile -> getLineText: GetLineText -> CodeFix
