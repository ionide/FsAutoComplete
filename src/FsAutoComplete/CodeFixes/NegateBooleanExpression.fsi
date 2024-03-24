module FsAutoComplete.CodeFix.NegateBooleanExpression

open FsAutoComplete.CodeFix.Types

val title: string
val fix: getParseResultsForFile: GetParseResultsForFile -> CodeFix
