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

[<NoEquality; NoComparison>]
type private InnerOffsides =
  /// We haven't found an inner construct yet.
  | NoneYet

  /// The start column of the first inner construct we find.
  /// This may not be on the same line as the open paren.
  | FirstLine of col: int

  /// The leftmost start column of an inner construct on a line
  /// following the first inner construct we found.
  /// We keep the first column of the first inner construct for comparison at the end.
  | FollowingLine of firstLine: int * followingLine: int

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
        /// Trim only spaces from the start if there is something else
        /// before the open paren on the same line (or else we could move
        /// the whole inner expression up a line); otherwise trim all whitespace
        /// from start and end.
        let (|Trim|) (sourceText: IFSACSourceText) =
          match sourceText.GetLine range.Start with
          | Some line ->
            if line.AsSpan(0, range.Start.Column).LastIndexOfAnyExcept(' ', '(') >= 0 then
              fun (s: string) -> s.TrimEnd().TrimStart ' '
            else
              fun (s: string) -> s.Trim()

          | None -> id

        let (|ShiftLeft|NoShift|ShiftRight|) (sourceText: IFSACSourceText) =
          let startLineNo = range.StartLine
          let endLineNo = range.EndLine

          if startLineNo = endLineNo then
            NoShift
          else
            let outerOffsides = range.StartColumn

            let rec loop innerOffsides lineNo startCol =
              if lineNo <= endLineNo then
                let line = sourceText.Lines[lineNo].ToString()

                match line.AsSpan(startCol).IndexOfAnyExcept(' ', ')') with
                | -1 -> loop innerOffsides (lineNo + 1) 0
                | i ->
                  match innerOffsides with
                  | NoneYet -> loop (FirstLine(i + startCol)) (lineNo + 1) 0
                  | FirstLine innerOffsides -> loop (FollowingLine(innerOffsides, i + startCol)) (lineNo + 1) 0
                  | FollowingLine(firstLine, innerOffsides) ->
                    loop (FollowingLine(firstLine, min innerOffsides (i + startCol))) (lineNo + 1) 0
              else
                innerOffsides

            match loop NoneYet startLineNo (range.StartColumn + 1) with
            | NoneYet -> NoShift
            | FirstLine innerOffsides when innerOffsides < outerOffsides -> ShiftRight(outerOffsides - innerOffsides)
            | FirstLine innerOffsides -> ShiftLeft(innerOffsides - outerOffsides)
            | FollowingLine(firstLine, followingLine) ->
              match firstLine - outerOffsides with
              | 0 -> NoShift
              | 1 when firstLine < followingLine -> NoShift
              | primaryOffset when primaryOffset < 0 -> ShiftRight -primaryOffset
              | primaryOffset -> ShiftLeft primaryOffset

        let adjusted =
          match sourceText with
          | TrailingOpen range -> txt[1 .. txt.Length - 2].TrimEnd()
          | Trim trim & NoShift -> trim txt[1 .. txt.Length - 2]
          | Trim trim & ShiftLeft spaces -> trim (txt[1 .. txt.Length - 2].Replace("\n" + String(' ', spaces), "\n"))
          | Trim trim & ShiftRight spaces -> trim (txt[1 .. txt.Length - 2].Replace("\n", "\n" + String(' ', spaces)))

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

      | _notParens -> return []
    })
