module FsAutoComplete.CodeFix.AddMissingSeq

open FsAutoComplete.CodeFix.Types

val title: string
val fix: getParseResultsForFile: GetParseResultsForFile -> CodeFix
