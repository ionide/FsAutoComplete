module FsAutoComplete.CodeFix.UpdateValueInSignatureFile

open FsAutoComplete.CodeFix.Types

val title: string
val fix: getParseResultsForFile: GetParseResultsForFile -> CodeFix
