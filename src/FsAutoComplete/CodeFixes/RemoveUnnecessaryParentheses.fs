module FsAutoComplete.CodeFix.RemoveUnnecessaryParentheses

open System
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete.CodeFix
open FsAutoComplete.CodeFix.Types
open FsToolkit.ErrorHandling
open FsAutoComplete
open FsAutoComplete.LspHelpers

let title = "Remove unnecessary parentheses"

[<AutoOpen>]
module private Patterns =
  let inline toPat f x = if f x then ValueSome() else ValueNone

  [<AutoOpen>]
  module Char =
    [<return: Struct>]
    let inline (|LetterOrDigit|_|) c = toPat Char.IsLetterOrDigit c

    [<return: Struct>]
    let inline (|Punctuation|_|) c = toPat Char.IsPunctuation c

    [<return: Struct>]
    let inline (|Symbol|_|) c = toPat Char.IsSymbol c

  [<AutoOpen>]
  module SourceText =
    /// E.g., something like:
    ///
    ///     let … = (␤
    ///     …
    ///     )
    [<return: Struct>]
    let (|TrailingOpen|_|) (range: FcsRange) (sourceText: IFSACSourceText) =
      match sourceText.GetLine range.Start with
      | Some line ->
        if
          line.AsSpan(0, range.Start.Column).LastIndexOfAnyExcept(' ', '(') >= 0
          && line.AsSpan(range.Start.Column).IndexOfAnyExcept('(', ' ') < 0
        then
          ValueSome TrailingOpen
        else
          ValueNone

      | None -> ValueNone

    /// Trim only spaces from the start if there is something else
    /// before the open paren on the same line (or else we could move
    /// the whole inner expression up a line); otherwise trim all whitespace
    /// from start and end.
    let (|Trim|) (range: FcsRange) (sourceText: IFSACSourceText) =
      match sourceText.GetLine range.Start with
      | Some line ->
        if line.AsSpan(0, range.Start.Column).LastIndexOfAnyExcept(' ', '(') >= 0 then
          fun (s: string) -> s.TrimEnd().TrimStart ' '
        else
          fun (s: string) -> s.Trim()

      | None -> id

    /// Returns the offsides diff if the given span contains an expression
    /// whose indentation would be made invalid if the open paren
    /// were removed (because the offside line would be shifted).
    [<return: Struct>]
    let (|OffsidesDiff|_|) (range: FcsRange) (sourceText: IFSACSourceText) =
      let startLineNo = range.StartLine
      let endLineNo = range.EndLine

      if startLineNo = endLineNo then
        ValueNone
      else
        let rec loop innerOffsides (pos: FcsPos) startCol =
          if pos.Line <= endLineNo then
            match sourceText.GetLine pos with
            | None -> ValueNone
            | Some line ->
              match line.AsSpan(startCol).IndexOfAnyExcept(' ', ')') with
              | -1 -> loop innerOffsides (pos.IncLine()) 0
              | i -> loop (i + startCol) (pos.IncLine()) 0
          else
            ValueSome(range.StartColumn - innerOffsides)

        loop range.StartColumn range.Start (range.StartColumn + 1)

    let (|ShiftLeft|NoShift|ShiftRight|) n =
      if n < 0 then ShiftLeft -n
      elif n = 0 then NoShift
      else ShiftRight n

/// A codefix that removes unnecessary parentheses from the source.
let fix (getFileLines: GetFileLines) : CodeFix =
  Run.ifDiagnosticByCode (Set.singleton "FSAC0004") (fun d codeActionParams ->
    asyncResult {
      let fileName = codeActionParams.TextDocument.GetFilePath() |> normalizePath
      let range = protocolRangeToRange (string fileName) d.Range

      let! sourceText = getFileLines fileName
      let! txt = sourceText.GetText range

      let firstChar = txt[0]
      let lastChar = txt[txt.Length - 1]

      match firstChar, lastChar with
      | '(', ')' ->
        let adjusted =
          match sourceText with
          | TrailingOpen range -> txt[1 .. txt.Length - 2].TrimEnd()

          | Trim range trim & OffsidesDiff range spaces ->
            match spaces with
            | NoShift -> trim txt[1 .. txt.Length - 2]
            | ShiftLeft spaces -> trim (txt[1 .. txt.Length - 2].Replace("\n" + String(' ', spaces), "\n"))
            | ShiftRight spaces -> trim (txt[1 .. txt.Length - 2].Replace("\n", "\n" + String(' ', spaces)))

          | _ -> txt[1 .. txt.Length - 2].Trim()

        let newText =
          let (|ShouldPutSpaceBefore|_|) (s: string) =
            // ……(……)
            // ↑↑ ↑
            (sourceText.TryGetChar(range.Start.IncColumn -1), sourceText.TryGetChar range.Start)
            ||> Option.map2 (fun twoBefore oneBefore ->
              match twoBefore, oneBefore, s[0] with
              | _, _, ('\n' | '\r') -> None
              | '[', '|', (Punctuation | LetterOrDigit) -> None
              | _, '[', '<' -> Some ShouldPutSpaceBefore
              | _, ('(' | '[' | '{'), _ -> None
              | _, '>', _ -> Some ShouldPutSpaceBefore
              | ' ', '=', _ -> Some ShouldPutSpaceBefore
              | _, '=', ('(' | '[' | '{') -> None
              | _, '=', (Punctuation | Symbol) -> Some ShouldPutSpaceBefore
              | _, LetterOrDigit, '(' -> None
              | _, (LetterOrDigit | '`'), _ -> Some ShouldPutSpaceBefore
              | _, (Punctuation | Symbol), (Punctuation | Symbol) -> Some ShouldPutSpaceBefore
              | _ -> None)
            |> Option.flatten

          let (|ShouldPutSpaceAfter|_|) (s: string) =
            // (……)…
            //   ↑ ↑
            sourceText.TryGetChar(range.End.IncColumn 1)
            |> Option.bind (fun endChar ->
              match s[s.Length - 1], endChar with
              | '>', ('|' | ']') -> Some ShouldPutSpaceAfter
              | _, (')' | ']' | '[' | '}' | '.' | ';' | ',' | '|') -> None
              | (Punctuation | Symbol), (Punctuation | Symbol | LetterOrDigit) -> Some ShouldPutSpaceAfter
              | LetterOrDigit, LetterOrDigit -> Some ShouldPutSpaceAfter
              | _ -> None)

          match adjusted with
          | ShouldPutSpaceBefore & ShouldPutSpaceAfter -> " " + adjusted + " "
          | ShouldPutSpaceBefore -> " " + adjusted
          | ShouldPutSpaceAfter -> adjusted + " "
          | adjusted -> adjusted

        return
          [ { Edits = [| { Range = d.Range; NewText = newText } |]
              File = codeActionParams.TextDocument
              Title = title
              SourceDiagnostic = Some d
              Kind = FixKind.Fix } ]

      | notParens ->
        System.Diagnostics.Debug.Fail $"%A{notParens} <> ('(', ')')"
        return []
    })
