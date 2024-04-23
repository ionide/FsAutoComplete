module FsAutoComplete.CodeFix.ExprTypeMismatch

open FsAutoComplete.CodeFix.Types

val title: string
val fix: getParseResultsForFile: GetParseResultsForFile -> CodeFix
