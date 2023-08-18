/// <summary>A codefix that converts DU case matches from positional form to named form</summary>
/// <example id="sample transformation">
/// Given this type:
/// <code lang="fsharp">
/// type Person = Person of first: string * middle: string option * last: string
/// </code>
///
/// This codefix will take the following destructuring pattern:
/// <code lang="fsharp">
/// let (Person(f, m, l)) = person
/// </code>
/// and convert it to the following pattern:
/// <code lang="fsharp">
/// let (Person(first = f; middle = m; last = l)) = person
/// </code>
/// </example>
module FsAutoComplete.CodeFix.ConvertPositionalDUToNamed

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Navigation
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

val title: string

val fix:
  getParseResultsForFile: GetParseResultsForFile ->
  getRangeText: GetRangeText ->
    (CodeActionParams -> Async<Result<Fix list, string>>)
