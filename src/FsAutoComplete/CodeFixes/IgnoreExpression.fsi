module FsAutoComplete.CodeFix.IgnoreExpression

open FsAutoComplete.CodeFix.Types

val title: string
val fix: getParseResultsForFile: GetParseResultsForFile -> CodeFix
