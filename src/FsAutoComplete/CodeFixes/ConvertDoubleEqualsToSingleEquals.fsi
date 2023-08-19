module FsAutoComplete.CodeFix.ConvertDoubleEqualsToSingleEquals

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

val title: string

/// a codefix that corrects == equality to = equality
val fix:
  getRangeText: GetRangeText ->
    (Ionide.LanguageServerProtocol.Types.CodeActionParams -> Async<Result<Fix list, string>>)
