module FsAutoComplete.CodeFix.AddTypeAliasToSignatureFile

open FsAutoComplete.CodeFix.Types

val title: string
val fix: getParseResultsForFile: GetParseResultsForFile -> CodeFix
