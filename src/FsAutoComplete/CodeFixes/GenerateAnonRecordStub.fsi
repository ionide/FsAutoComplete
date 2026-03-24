module FsAutoComplete.CodeFix.GenerateAnonRecordStub

open FsAutoComplete.CodeFix.Types

val title: string
val fix: getParseResultsForFile: GetParseResultsForFile -> CodeFix
