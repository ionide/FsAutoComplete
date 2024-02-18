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
    let (|TrailingOpen|_|) (range: Range) (sourceText: IFSACSourceText) =
      let line = sourceText.Lines[range.Start.Line]

      if
        line.AsSpan(0, range.Start.Character).LastIndexOfAnyExcept(' ', '(') >= 0
        && line.AsSpan(range.Start.Character).IndexOfAnyExcept('(', ' ') < 0
      then
        ValueSome TrailingOpen
      else
        ValueNone

    /// Trim only spaces from the start if there is something else
    /// before the open paren on the same line (or else we could move
    /// the whole inner expression up a line); otherwise trim all whitespace
    // from start and end.
    let (|Trim|) (range: Range) (sourceText: IFSACSourceText) =
      let line = sourceText.Lines[range.Start.Line]

      if line.AsSpan(0, range.Start.Character).LastIndexOfAnyExcept(' ', '(') >= 0 then
        fun (s: string) -> s.TrimEnd().TrimStart ' '
      else
        fun (s: string) -> s.Trim()

    /// Returns the offsides diff if the given span contains an expression
    /// whose indentation would be made invalid if the open paren
    /// were removed (because the offside line would be shifted).
    [<return: Struct>]
    let (|OffsidesDiff|_|) (range: Range) (sourceText: IFSACSourceText) =
      let startLineNo = range.Start.Line
      let endLineNo = range.End.Line

      if startLineNo = endLineNo then
        ValueNone
      else
        let rec loop innerOffsides lineNo startCol =
          if lineNo <= endLineNo then
            let line = sourceText.Lines[lineNo]

            match line.AsSpan(startCol).IndexOfAnyExcept(' ', ')') with
            | -1 -> loop innerOffsides (lineNo + 1) 0
            | i -> loop (i + startCol) (lineNo + 1) 0
          else
            ValueSome(range.Start.Character - innerOffsides)

        loop range.Start.Character startLineNo (range.Start.Character + 1)

    let (|ShiftLeft|NoShift|ShiftRight|) n =
      if n < 0 then ShiftLeft -n
      elif n = 0 then NoShift
      else ShiftRight n

/// A codefix that removes unnecessary parentheses from the source.
let fix (getFileLines: GetFileLines) : CodeFix =
  Run.ifDiagnosticByCode (Set.singleton "FSAC0004") (fun d codeActionParams ->
    asyncResult {
      let fileName = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath

      let! sourceText = getFileLines fileName
      let! txt = sourceText.GetText(protocolRangeToRange (string fileName) d.Range)

      let firstChar = txt[0]
      let lastChar = txt[txt.Length - 1]

      match firstChar, lastChar with
      | '(', ')' ->
        let adjusted =
          match sourceText with
          | TrailingOpen d.Range -> txt[1 .. txt.Length - 2].TrimEnd()

          | Trim d.Range trim & OffsidesDiff d.Range spaces ->
            match spaces with
            | NoShift -> trim txt[1 .. txt.Length - 2]
            | ShiftLeft spaces -> trim (txt[1 .. txt.Length - 2].Replace("\n" + String(' ', spaces), "\n"))
            | ShiftRight spaces -> trim (txt[1 .. txt.Length - 2].Replace("\n", "\n" + String(' ', spaces)))

          | _ -> txt[1 .. txt.Length - 2].Trim()

        let newText =
          let (|ShouldPutSpaceBefore|_|) (s: string) =
            // "……(……)"
            //  ↑↑ ↑
            (sourceText.TryGetChar((protocolPosToPos d.Range.Start).IncColumn -1),
             sourceText.TryGetChar((protocolPosToPos d.Range.Start)))
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
            // "(……)…"
            //    ↑ ↑
            sourceText.TryGetChar((protocolPosToPos d.Range.End).IncColumn 1)
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
