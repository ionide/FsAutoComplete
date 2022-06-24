module Utils.TextEdit
open Ionide.LanguageServerProtocol.Types
open Utils.Utils
open Expecto
open FsToolkit.ErrorHandling

/// Functions to extract Cursor or Range from a given string.
/// Cursor is marked in string with `$0` (`Cursor.Marker`)
/// 
/// Note: Only `\n` is supported. Neither `\r\n` nor `\r` produce correct results.
module Cursor =
  /// 0-based
  let inline private pos line column: Position = { Line = line; Character = column }

  /// Cursor Marker in text.
  /// Single marker: Position
  /// Two markers: Range
  let [<Literal>] Marker = "$0"

  (*
    Identities:
    * idx |> beforeIndex |> tryIndexOf = idx
    * pos |> tryIndexOf |> beforeIndex = pos
    * tryExtractIndex >> beforeIndex = tryExtractPosition
    * tryExtractPosition >> tryIndexOf = tryExtractIndex
    Note: Even though it's enough to only implement one side,
          here all functions are implemented independent of each other.
          Reason: For testing. One function wrong -> equality unit tests should fail
  *)

  /// Returns Cursor Position BEFORE index
  /// 
  /// Index might be `text.Length` (-> cursor AFTER last character). 
  /// All other out of text range indices throw exception.
  let beforeIndex (i: int) (text: string) : Position =
    assert(i >= 0)
    assert(i <= text.Length)

    let linesBefore =
      text.Substring(0, i)
      |> Text.lines
    // line & char are 0-based
    let line = linesBefore.Length - 1
    let char = linesBefore |> Array.last |> String.length
    pos line char

  /// Returns index of first `$0` (`Cursor.Marker`) and the updated input text without the cursor marker.
  /// 
  /// Note: Cursor Position is BEFORE index.  
  /// Note: Index might be `text.Length` (-> Cursor AFTER last char in text)
  let tryExtractIndex (text: string) =
    match text.IndexOf Marker with
    | -1 -> None
    | i ->
        (i, text.Remove(i, Marker.Length))
        |> Some
  /// `tryExtractIndex`, but fails when there's no cursor
  let assertExtractIndex =
    tryExtractIndex
    >> Option.defaultWith (fun _ -> failtest "No cursor")

  /// Extracts first cursor marked with any of `markers`. Remaining cursors aren't touched
  let tryExtractPositionMarkedWithAnyOf (markers: string[]) (text: string) =
    let tryFindAnyCursorInLine (line: string) =
      let markersInLine =
        markers
        |> Array.choose (fun marker ->
            match line.IndexOf marker with
            | -1 -> None
            | column -> Some (marker, column)
        )
      match markersInLine with
      | [||] -> None
      | _ ->
          let (marker, column) = markersInLine |> Array.minBy snd
          let line = line.Substring(0, column) + line.Substring(column + marker.Length)
          Some (marker, column, line)
    // Note: Input `lines` gets mutated to remove cursor
    let tryFindAnyCursor (lines: string[]) =
      lines
      |> Seq.mapi (fun i l -> (i,l))
      |> Seq.tryPick (fun (i,line) -> 
          tryFindAnyCursorInLine line 
          |> Option.map (fun (marker, c, line) -> (marker, pos i c, line))
         )
      |> function
          | None -> None
          | Some (marker, p,line) -> 
              lines.[p.Line] <- line
              Some ((marker, p), lines)
    
    let lines = text |> Text.lines
    match tryFindAnyCursor lines with
    | None -> None
    | Some ((marker, p), lines) ->
        let text = lines |> String.concat "\n"
        Some ((marker, p), text)

  /// Returns Position of first `$0` (`Cursor.Marker`) and the updated input text without the cursor marker.  
  /// Only the first `$0` is processed.
  /// 
  /// Note: Cursor Position is BETWEEN characters and might be outside of text range (cursor AFTER last character)
  let tryExtractPosition =
    tryExtractPositionMarkedWithAnyOf [| Marker |]
    >> Option.map (fun ((_, pos), line) -> (pos, line))
  /// `tryExtractPosition`, but fails when there's no cursor
  let assertExtractPosition =
    tryExtractPosition
    >> Option.defaultWith (fun _ -> failtest "No cursor")

  /// Returns Range between the first two `$0` (`Cursor.Marker`) and the updated text without the two cursor markers.
  /// 
  /// If there's only one cursor marker, the range covers exactly that position (`Start = End`)
  let tryExtractRange (text: string) =
    match tryExtractPosition text with
    | None -> None
    | Some (start, text) ->
        let (fin, text) = tryExtractPosition text |> Option.defaultValue (start, text)
        let range = { Start = start; End = fin }
        Some (range, text)
  /// `tryExtractRange`, but fails when there's no cursor.
  let assertExtractRange =
    tryExtractRange
    >> Option.defaultWith (fun _ -> failtest "No cursor(s)")

  /// Position is between characters, while index is on character.
  /// For Insert & Remove: character indices
  /// 
  /// Returned index is AFTER cursor:
  /// * `Column=0`: before first char; `Index=0`: on first char 
  /// * `Column=1`: after first char, before 2nd char; `Index=1`: on 2nd char
  /// * `Column=max`: after last char; `Index=max`: AFTER last char in line (-> `\n` or end of string)
  let tryIndexOf (pos: Position) (text: string) =
    Position.assertPositive pos

    let lines = text |> Text.lines

    // check in range
    if pos.Line >= lines.Length then
      $"Line {pos.Line} is out of text range. Text has {lines.Length} lines."
      |> Error
    elif pos.Character > lines.[pos.Line].Length then
      // `>`: character can be AFTER last char in string
      $"Character {pos.Character} is out of line range {pos.Line}. Line {pos.Line} has length of {lines[pos.Line].Length}."
      |> Error
    else
      let offsetToLine =
        lines
        |> Seq.take pos.Line  // `Line` is 0-based -> can be used as length
        |> Seq.sumBy (String.length >> (+) 1) // `+ 1`: `\n`

      offsetToLine + pos.Character
      |> Ok
  /// `tryIndexOf`, but fails when position is invalid
  let assertIndexOf pos =
    tryIndexOf pos
    >> Result.valueOr (failtestf "Invalid position: %s")

  /// Calculates cursors position after all edits are applied.
  /// 
  /// When cursor inside a changed area:
  /// * deleted: cursor moves to start of deletion:
  ///   ```fsharp
  ///   let foo = 42 $|+ $013 $|+ 123
  ///   ```
  ///   -> delete inside `$|`
  ///   ```fsharp
  ///   let foo = 42 $0+ 123
  ///   ```
  /// * inserted: cursor stays at start of insert
  ///   ```fsharp
  ///   let foo = 42 $0+ 123
  ///   ```
  ///   -> insert at cursor pos
  ///   ```fsharp
  ///   let foo = 42 $0+ 13 + 123
  ///   ```
  /// * changes: cursors moved to start of replacement
  ///   ```fsharp
  ///   let foo = 42 $|+ $013 $|+ 123
  ///   ```
  ///   -> replace inside `$|`
  ///   ```fsharp
  ///   let foo = 42 $0- 7 + 123
  ///   ```
  ///   -> like deletion
  ///   * Implementation detail:  
  ///     Replacement is considered: First delete (-> move cursor to front), then insert (-> cursor stays)
  ///
  /// Note: `edits` must be sorted by range!
  let afterEdits (edits: TextEdit list) (pos: Position) =
    edits
    |> List.filter (fun edit -> edit.Range.Start < pos)
    |> List.rev
    |> List.fold (fun pos edit ->
      // remove deleted range from pos
      let pos =
        if Range.isPosition edit.Range then
          // just insert
          pos
        elif edit.Range |> Range.containsLoosely pos then
          // pos inside edit -> fall to start of delete
          edit.Range.Start
        else
          // everything to delete is before cursor
          let (s,e) = edit.Range.Start, edit.Range.End
          // always <= 0 (nothing gets inserted here)
          let deltaLine = s.Line - e.Line
          let deltaChar =
            if e.Line < pos.Line then
              // doesn't touch line of pos
              0
            else
              - e.Character + s.Character
          { Line = pos.Line + deltaLine; Character = pos.Character + deltaChar }
        
      // add new text to pos
      let pos =
        if System.String.IsNullOrEmpty edit.NewText then
          // just delete
          pos
        elif pos <= edit.Range.Start then
          // insert is after pos -> doesn't change cursor
          // happens when cursor inside replacement -> cursor move to front of deletion
          pos
        else
          let lines =
            edit.NewText
            |> Text.removeCarriageReturn
            |> Text.lines
          let deltaLine = lines.Length - 1
          let deltaChar =
            if edit.Range.Start.Line = pos.Line then
              let lastLine = lines |> Array.last
              if lines.Length = 1 then
                // doesn't introduce new line
                lastLine.Length
              else
                // inserts new line
                - edit.Range.Start.Character + lastLine.Length
            else
              // doesn't touch line of pos
              0
          { Line = pos.Line + deltaLine; Character = pos.Character + deltaChar }

      pos
    ) pos

module Cursors =
  /// For each cursor (`$0`) in text: return text with just that one cursor
  /// 
  /// Note: doesn't trim input!
  let iter (textWithCursors: string) =
    let rec collect (textsWithSingleCursor) (textWithCursors: string) =
      match textWithCursors.IndexOf Cursor.Marker with
      | -1 -> textsWithSingleCursor |> List.rev
      | i ->
        let textWithSingleCursor =
          textWithCursors.Substring(0, i + Cursor.Marker.Length)
          +
          textWithCursors.Substring(i + Cursor.Marker.Length).Replace(Cursor.Marker, "")
        let textWithCursors = textWithCursors.Remove(i, Cursor.Marker.Length)
        collect (textWithSingleCursor :: textsWithSingleCursor) textWithCursors
    collect [] textWithCursors

  /// Returns all cursor (`$0`) positions and the text without any cursors.
  /// 
  /// Unlike `iter` this extracts positions instead of reducing to texts with one cursor
  let extract (textWithCursors: string) =
    let tps =
      textWithCursors
      |> iter
      |> List.map (Cursor.assertExtractPosition)
    let text = tps |> List.head |> snd
    let poss = tps |> List.map fst
    (text, poss)
    

  /// Like `extract`, but instead of just extracting Cursors marked with `Cursor.Marker` (`$0`),
  /// this here extract all specified markers.
  let extractWith (markers: string[]) (text: string) =
    let rec collect poss text =
      match Cursor.tryExtractPositionMarkedWithAnyOf markers text with
      | None -> (text,poss)
      | Some ((marker, pos), text) ->
          let poss = (marker, pos) :: poss
          collect poss text
    let (text, cursors) = collect [] text
    (text, cursors |> List.rev)
  /// Like `extractWith`, but additional groups cursor positions by marker
  let extractGroupedWith (markers: string[]) (text: string) =
    let (text, cursors) = extractWith markers text
    let cursors =
      cursors
      |> List.groupBy fst
      |> List.map (fun (marker, poss) -> (marker, poss |> List.map snd))
      |> Map.ofList
    (text, cursors)


module Text =

  let private indicesOf (range: Range) (text: string) =
    result {
      let! start = Cursor.tryIndexOf range.Start text
      if range.Start = range.End then
        return (start, start)
      else
        let! fin = Cursor.tryIndexOf range.End text
        return (start, fin)
    }
  let remove (range: Range) (text: string) =
    result {
      if range.Start = range.End then
        return text
      else
        let! (start, fin) = indicesOf range text
        // Including start, excluding fin (cursor is BEFORE char)
        return text.Remove(start, fin - start)
    }

  let insert (pos: Position) (insert: string) (text: string) =
    result {
      if insert = "" then
        return text
      else
        let! idx = Cursor.tryIndexOf pos text
        // insert BEFORE idx (cursor is BEFORE char)
        return text.Insert (idx, insert)
    }

  let replace (range: Range) (replacement: string) (text: string) =
    text
    |> remove range
    |> Result.bind (insert range.Start replacement)

module TextEdit =

  let apply (edit: TextEdit) =
    // `edit` is from FSAC LSP -> might contain `\r`. 
    // But only `\n` handled by `Text.lines` -> remove `\r` 
    let newText = edit.NewText |> Text.removeCarriageReturn
    Text.replace edit.Range newText


  let deletes (edit: TextEdit) = not <| Range.isPosition edit.Range
  let inserts (edit: TextEdit) = not <| System.String.IsNullOrEmpty edit.NewText
  let replaces (edit: TextEdit) = deletes edit && inserts edit

  let doesNothing (edit: TextEdit) =
    not (edit |> deletes)
    &&
    not (edit |> inserts)

  // **Note**:  
  // VS Code allows TextEdits, that might not be strictly valid according to LSP Specs [^1]:
  // * inserts into not existing line (text has 2 line, insert into line 5 is ok)
  // * inserts into line way after last character (line has 15 char, insert into column 1000 is ok)
  // * accepts `Range.End` < `Range.Start`
  // * empty text edits (neither inserts nor deletes text)
  //
  // LSP Specs are quite vague. So above might or might not be ok according to Specs.
  // But from FSAC perspective: Any case above most likely indicates an error in CodeFix implementation  
  // -> TextEdit must be STRICTLY correct and all of the cases above are considered erroneous!
  // 
  // [^1]: https://microsoft.github.io/language-server-protocol/specifications/specification-current/

  /// Checks passed `edit` for errors:
  /// * Positive Lines & Characters in Ranges
  ///   * Note: doesn't test if range is inside text! Just simple positive test.
  /// * Start Range must be before or equal End Range
  /// * Does something (-> must insert or delete (or both -> replace) something)
  ///   * Note: empty edit is technically valid, but in practice it's most likely an error
  let tryFindError (edit: TextEdit) =
    if edit.Range.Start.Line < 0 then
      Some "Expected positive Start.Line, but was negative"
    else if edit.Range.Start.Character < 0 then
      Some "Expected positive Start.Character, but was negative"
    else if edit.Range.End.Line < 0 then
      Some "Expected positive End.Line, but was negative"
    else if edit.Range.End.Character < 0 then
      Some "Expected positive End.Character, but was negative"
    else if edit.Range.Start > edit.Range.End then
      Some "Expected Range.Start <= Range.End, but was Start > End"
    else if edit |> doesNothing then
      Some "Expected change, but does nothing (neither delete nor insert)"
    else
      None
    
module TextEdits =

  /// Checks edits for:
  /// * There's at least one TextEdit
  /// * All TextEdits are valid (`TextEdit.tryFindError`)
  /// * Edits don't overlap
  /// * For same position: All inserted before at most one replace (or delete)
  /// 
  /// 
  /// [LSP Specification for `TextEdit[]`](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textEditArray)
  /// > Text edits ranges must never overlap, that means no part of the original document must be manipulated by more than one edit. 
  /// > However, it is possible that multiple edits have the same start position: multiple inserts, 
  /// > or any number of inserts followed by a single remove or replace edit. 
  /// > If multiple inserts have the same position, the order in the array defines the order 
  /// > in which the inserted strings appear in the resulting text.
  let tryFindError (edits: TextEdit list) =
    let rec tryFindOverlappingEditExample (edits: TextEdit list) =
      match edits with
      | [] | [_] -> None
      | edit :: edits ->
          match edits |> List.tryFind (fun e -> Range.overlapsLoosely edit.Range e.Range) with
          | Some overlapping ->
            Some (edit, overlapping)
          | None ->
            tryFindOverlappingEditExample edits
    let (|Overlapping|_|) = tryFindOverlappingEditExample
    let (|Invalids|_|) = 
      List.choose (fun edit -> edit |> TextEdit.tryFindError |> Option.map (fun err -> (edit, err)))
      >> function | [] -> None | errs -> Some errs
    let findSameStarts (edits: TextEdit list) =
      edits
      |> List.groupBy (fun e -> e.Range.Start)
      |> List.filter (fun (_, es) -> List.length es > 1)
      |> List.map snd
    /// For same position: all inserts must be before at most one Delete/Replace
    /// Note: doesn't check edits for same position
    let rec replaceNotLast (edits: TextEdit list) =
      match edits with
      | [] | [_] -> false
      | a::edits ->
          assert(List.length edits >= 1)
          (TextEdit.deletes a) || (replaceNotLast edits)
    let (|ReplaceNotLast|_|) =
      findSameStarts
      >> List.filter (replaceNotLast)
      >> function | [] -> None | ss -> Some ss

    match edits with
    // there must be edits
    | [] -> Some "Expected at least one TextEdit, but were none"
    // edits should be valid
    | Invalids errs ->
      sprintf 
        "Expected all TextEdits to be valid, but there was at least one erroneous Edit. Invalid Edits: %A"
        errs
      |> Some
    // No overlapping
    | Overlapping (edit1, edit2) ->
        Some $"Expected no overlaps, but at least two edits overlap: {edit1.Range} and {edit2.Range}"
    // For same position: all inserts must be before at most one Delete/Replace
    | ReplaceNotLast errs ->
      sprintf 
        "Expected Inserts before at most one Delete/Replace, but there was at least one Delete/Before in invalid position: Invalid Edits: %A"
        errs
      |> Some
    | _ -> None


  /// Sorts edits by range (`Start`).
  /// Order is preserved for edits with same `Start`.
  let sortByRange (edits: TextEdit list) =
    edits
    |> List.sortWith (fun e1 e2 ->
      match e1.Range.Start.Line.CompareTo(e2.Range.Start.Line) with
      | 0 ->
        e1.Range.Start.Character.CompareTo(e2.Range.Start.Character)
      | r -> r
    )

  /// Applies the passed edits from last to first (sorted by range)
  let apply edits text =
    let edits = edits |> sortByRange |> List.rev
    List.fold (fun text edit -> text |> Result.bind (TextEdit.apply edit)) (Ok text) edits

  /// `tryFindError` before `apply`
  let applyWithErrorCheck edits text =
    match tryFindError edits with
    | Some error -> Error error
    | None ->
        text
        |> apply edits

module WorkspaceEdit =
  /// Extract `TextEdit[]` from either `DocumentChanges` or `Changes`.
  /// All edits MUST be for passed `textDocument`.
  /// 
  /// Checks for errors:
  /// * Either `DocumentChanges` or `Changes`, but not both
  ///   * FsAutoComplete sends only `DocumentChanges`
  /// * All edits inside `textDocument`
  ///   * Version is only checked if: Version in `textDocument` and Version in `workspaceEdit.DocumentChanges.*`
  /// * Using `TextEdit.tryFindError`:
  ///   * At least one edit
  ///   * No empty edit
  ///   * No overlaps
  let tryExtractTextEditsInSingleFile (textDocument: VersionedTextDocumentIdentifier) (workspaceEdit: WorkspaceEdit) =

    let checkDocument (uri) (version) =
      if uri <> textDocument.Uri then
        Some $"Edit should be for document `{textDocument.Uri}`, but was for `{uri}`"
      else
        match textDocument.Version, version with
        // only compare `Version` when `textDocument` and `version` has a Version. Otherwise ignore
        | Some textDocVersion, Some version when textDocVersion <> version -> 
            Some $"Edit should be for document version `{textDocVersion}`, but version was `{version}`"
        | _ -> None

    match (workspaceEdit.DocumentChanges, workspaceEdit.Changes) with
    | None, None ->
        Error "Expected changes, but `DocumentChanges` and `Changes` were both `None`."
    | Some _, Some _ ->
        Error "Expected either `DocumentChanges` or `Changes`, but was both."
    | Some [||], None ->
        Error "Expected changes, but `DocumentChanges` was empty."
    | Some changes, None ->
        match changes |> Array.tryPick (fun c -> checkDocument c.TextDocument.Uri c.TextDocument.Version) with
        | Some error -> Error error
        | _ ->
          changes
          |> Seq.map (fun c -> c.Edits)
          |> Seq.collect id
          |> Seq.toList
          |> Ok
    | None, Some changes when changes.IsEmpty ->
        Error "Expected changes, but `Changes` was empty."
    | None, Some changes ->
        match changes |> Seq.tryPick (fun c -> checkDocument c.Key None) with
        | Some error -> Error error
        | _ ->
          changes.Values
          |> Seq.collect id
          |> Seq.toList
          |> Ok
    |> Result.bind (fun edits ->
        match TextEdits.tryFindError edits with
        | Some error -> Error error
        | None -> Ok edits
    )
