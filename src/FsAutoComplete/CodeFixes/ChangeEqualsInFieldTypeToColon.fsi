module FsAutoComplete.CodeFix.ChangeEqualsInFieldTypeToColon

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open FsAutoComplete

val title: string
/// a codefix that fixes a malformed record type annotation to use colon instead of equals
val fix: CodeFix
