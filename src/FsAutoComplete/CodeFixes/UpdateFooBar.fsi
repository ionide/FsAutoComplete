module FsAutoComplete.CodeFix.UpdateFooBar

open FsAutoComplete.CodeFix.Types

val title: string
val fix: getParseResultsForFile: GetParseResultsForFile -> CodeFix
