module FsAutoComplete.CodeFix.ReplaceLambdaWithDotLambda

open FsAutoComplete.CodeFix.Types

val titleReplaceToDotLambda: string
val titleReplaceToLambda: string
val fix: getLanguageVersion: GetLanguageVersion -> getParseResultsForFile: GetParseResultsForFile -> CodeFix
