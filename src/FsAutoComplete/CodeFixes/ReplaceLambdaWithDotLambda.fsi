module FsAutoComplete.CodeFix.ReplaceLambdaWithDotLambda

open FsAutoComplete.CodeFix.Types

val title: string
val fix: getLanguageVersion: GetLanguageVersion -> getParseResultsForFile: GetParseResultsForFile -> CodeFix
