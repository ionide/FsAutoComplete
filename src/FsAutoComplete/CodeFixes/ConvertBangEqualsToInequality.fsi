/// fix to convert uses of != to <> to use the proper thing
module FsAutoComplete.CodeFix.ConvertBangEqualsToInequality

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

val title: string

val fix:
  getRangeText: GetRangeText ->
    (Ionide.LanguageServerProtocol.Types.CodeActionParams -> Async<Result<Fix list, string>>)
