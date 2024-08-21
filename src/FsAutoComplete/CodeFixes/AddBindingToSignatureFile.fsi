module FsAutoComplete.CodeFix.AddBindingToSignatureFile

open FsAutoComplete.CodeFix.Types

val title: string
val fix: getProjectOptionsForFile: GetProjectOptionsForFile -> getParseResultsForFile: GetParseResultsForFile -> CodeFix
