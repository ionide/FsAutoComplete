namespace Benchmarks

type ISourceText =

  abstract Item: index: int -> char with get

  abstract GetLineString: lineIndex: int -> string

  abstract GetLineCount: unit -> int

  abstract GetLastCharacterPosition: unit -> int * int

  abstract GetSubTextString: start: int * length: int -> string

  abstract SubTextEquals: target: string * startIndex: int -> bool

  abstract Length: int

  abstract ContentEquals: sourceText: ISourceText -> bool

  abstract CopyTo: sourceIndex: int * destination: char[] * destinationIndex: int * count: int -> unit

open System
open FSharp.Data.Adaptive
open Microsoft.CodeAnalysis.Text
open CommunityToolkit.HighPerformance.Buffers
type FileVersion = int

[<Struct>]
type TextChange = {
  Change : string
  Version : FileVersion
  LastTouched : DateTime
  Position : int * int
}

type AdaptiveSourceText(path: string, initialText: string) =
  let pool = StringPool.Shared
  let initSourceText = SourceText.From(initialText)
  let changes = clist<TextChange> []

  let sortedChanges =  changes |> AList.sortBy (fun t -> t.Version)

  let lastTouched = changes |> AList.map(fun x -> x.LastTouched) |> AList.tryMax

  let currentState =
        (initSourceText, sortedChanges)
        ||> AList.fold (fun state change ->
          // printfn "Applying change: %A" change
          let (start, length) = change.Position
          state.WithChanges(TextChange(TextSpan(start, length), change.Change))
        )

  let toIndexedList (lineCollection : TextLineCollection) : IndexList<string> =
    let mutable list = IndexList.Empty
    for line in lineCollection do

      list <- list.InsertAt (line.LineNumber, line.ToString())
    list

  let lines =
    currentState
    |> AVal.map(fun x -> toIndexedList x.Lines)
    |> AList.ofAVal


  let toIndexedList4 (lineCollection : TextLineCollection) : IndexList<_> =
    let mutable list = IndexList.Empty
    for line in lineCollection do

      list <- list.InsertAt (line.LineNumber, pool.GetOrAdd(line.ToString()))
    list

  let lines4 =
    currentState
    |> AVal.map(fun x -> toIndexedList4 x.Lines)
    |> AList.ofAVal


  let toIndexedList2 (lineCollection : TextLineCollection) : IndexList<TextLine> =
    let mutable list = IndexList.Empty
    for line in lineCollection do

      list <- list.InsertAt (line.LineNumber, line)
    list

  let lines2 =
    currentState
    |> AVal.map(fun x -> toIndexedList2 x.Lines)
    |> AList.ofAVal


  let lines3 =
    currentState
    |> AVal.map(fun x -> x.Lines)
    |> AList.ofAVal

  let text = currentState |> AVal.map(fun x -> x.ToString())

  member x.Path = path
  member x.Changes = changes

  member this.Text = text
  member this.CurrentStateForced = currentState |> AVal.force

  member this.Lines = lines
  member this.Lines2 = lines2
  member this.Lines3 = lines3

  member this.Lines4 = lines4

  override this.GetHashCode() =
    let sourceText = this.CurrentStateForced
    sourceText.GetHashCode()

  override this.Equals(obj) =
    match obj with
    | :? AdaptiveSourceText as other -> (this.CurrentStateForced).Equals(other.CurrentStateForced)
    | _ -> false

  interface ISourceText with
    member this.ContentEquals(sourceText: ISourceText) : bool =
      match sourceText with
      | :? AdaptiveSourceText as other -> (this.CurrentStateForced).ContentEquals(other.CurrentStateForced)
      | _ -> false

    member this.CopyTo(sourceIndex: int, destination: char array, destinationIndex: int, count: int) : unit =
      let source = this.CurrentStateForced
      source.CopyTo(sourceIndex, destination, destinationIndex, count)

    member this.GetLastCharacterPosition() : int * int =
      let sourceText = this.CurrentStateForced

      if sourceText.Lines.Count > 0 then
        (sourceText.Lines.Count, sourceText.Lines.[sourceText.Lines.Count - 1].Span.Length)
      else
        (0, 0)

    member this.GetLineCount() : int =
      let sourceText = this.CurrentStateForced
      sourceText.Lines.Count

    member this.GetLineString(lineIndex: int) : string =
      let sourceText = this.CurrentStateForced
      sourceText.Lines.[lineIndex].ToString()

    member this.GetSubTextString(start: int, length: int) : string =
      let sourceText = this.CurrentStateForced
      sourceText.GetSubText(TextSpan(start, length)).ToString()

    member this.Item
      with get (index: int): char =
        let sourceText = this.CurrentStateForced
        sourceText.[index]

    member this.Length: int =
      let sourceText = this.CurrentStateForced
      sourceText.Length

    member this.SubTextEquals(target: string, startIndex: int) : bool =
      let sourceText = this.CurrentStateForced

      if startIndex < 0 || startIndex >= sourceText.Length then
        invalidArg "startIndex" "Out of range."

      if String.IsNullOrEmpty(target) then
        invalidArg "target" "Is null or empty."

      let lastIndex = startIndex + target.Length

      if lastIndex <= startIndex || lastIndex >= sourceText.Length then
        invalidArg "target" "Too big."

      let mutable finished = false
      let mutable didEqual = true
      let mutable i = 0

      while not finished && i < target.Length do
        if target.[i] <> sourceText.[startIndex + i] then
          didEqual <- false
          finished <- true // bail out early
        else
          i <- i + 1

      didEqual


// open System.Runtime.CompilerServices
// open System.Text
// [<Struct; IsByRefLike>]
// type Position = {
//   Line : uint32
//   Character : uint32
// }

// [<Struct; IsByRefLike>]
// type Range = {
//   Start : Position
//   End : Position
// }


open LinkDotNet.StringBuilder

open FsAutoComplete.Adaptive
module Helpers =

  let initFile () =
    let file1 = AdaptiveSourceText("foo.txt", initialText = "Hello")

    do transact(fun () ->
      file1.Changes.Add({ Change = "World"; Version = 1; LastTouched = DateTime.Now; Position = (5, 0) }) |> ignore
    )
    file1

  let initFile2 () =
    let file2 = AdaptiveSourceText2("foo.txt", "Hello")
    file2.ModifyText({ Start = { Line = 0; Character = 5 }; End = { Line = 0; Character = 5 } }, "World")
    file2


  let addLine (file : AdaptiveSourceText) =
    transact(fun () ->
      file.Changes.Add({ Change = "\nLOL"; Version = 1; LastTouched = DateTime.Now; Position = (10, 0) }) |> ignore
    )

  let addLine2 (file : AdaptiveSourceText2) =
    file.ModifyText({ Start = { Line = 0; Character = 10 }; End = { Line = 0; Character = 10 } }, "\nLOL")

  let convertToTextSpan (sourceText : SourceText, range : Ionide.LanguageServerProtocol.Types.Range) =
    let start = sourceText.Lines.[max 0 (range.Start.Line)].Start + range.Start.Character
    let endPosition =
            sourceText.Lines.[min (range.End.Line) (sourceText.Lines.Count - 1)].Start
            + range.End.Character
    TextSpan(start, endPosition - start)

  let addToSourceText (sourceText : SourceText, range : Ionide.LanguageServerProtocol.Types.Range, text : string) =
    let textSpan = convertToTextSpan(sourceText, range)
    let newText = sourceText.WithChanges([| TextChange(textSpan, text) |])
    newText

open BenchmarkDotNet
open BenchmarkDotNet.Attributes
open Helpers
[<MemoryDiagnoser>]
type SourceText_LineChanges_Benchmarks ()=

  let n = 100




  [<Benchmark(Baseline = true)>]
  member _.Roslyn_Text_changeText () =
    let mutable file = SourceText.From("Hello")
    file <- addToSourceText(file, { Start = { Line = 0; Character = 5 }; End = { Line = 0; Character = 5 } }, "World")
    for i in 1..n do
      file <- addToSourceText(file, { Start = { Line = 0; Character = 10 }; End = { Line = 0; Character = 10 } }, "\nLOL")
    file.Lines |> Seq.toArray |> ignore

  [<Benchmark>]
  member _.Lines1_forceOnce () =
    let file1 = initFile ()
    for i in 1..n do
      addLine file1
    file1.Lines |> AList.force |> ignore


  [<Benchmark>]
  member _.Lines2_forceOnce () =
    let file1 = initFile ()
    for i in 1..n do
      addLine file1
    file1.Lines2 |> AList.force |> ignore

  [<Benchmark>]
  member _.Lines3_forceOnce () =
    let file1 = initFile ()
    for i in 1..n do
      addLine file1
    file1.Lines3 |> AList.force |> ignore


  [<Benchmark>]
  member _.Lines4_forceOnce () =
    let file1 = initFile ()
    for i in 1..n do
      addLine file1
    file1.Lines4 |> AList.force |> ignore


  [<Benchmark>]
  member _.File2_Lines_forceOnce () =
    let file1 = initFile2 ()
    for i in 1..n do
      addLine2 file1
    file1.Lines |> AList.force |> ignore


  [<Benchmark>]
  member _.Roslyn_Text_changeText_every10 () =
    let mutable file = SourceText.From("Hello")
    file <- addToSourceText(file, { Start = { Line = 0; Character = 5 }; End = { Line = 0; Character = 5 } }, "World")
    for i in 1..n do
      file <- addToSourceText(file, { Start = { Line = 0; Character = 10 }; End = { Line = 0; Character = 10 } }, "\nLOL")
      if i % 10 = 0 then file.Lines |> Seq.toArray |> ignore
    file.Lines |> Seq.toArray |> ignore

  [<Benchmark>]
  member _.Lines1_forceEvery10 () =
    let file1 = initFile ()
    for i in 1..n do
      addLine file1
      if i % 10 = 0 then file1.Lines3 |> AList.force |> ignore
    file1.Lines |> AList.force |> ignore


  [<Benchmark>]
  member _.Lines2_forceEvery10 () =
    let file1 = initFile ()
    for i in 1..n do
      addLine file1
      if i % 10 = 0 then file1.Lines3 |> AList.force |> ignore
    file1.Lines2 |> AList.force |> ignore

  [<Benchmark>]
  member _.Lines3_forceEvery10 () =
    let file1 = initFile ()
    for i in 1..n do
      addLine file1
      if i % 10 = 0 then file1.Lines3 |> AList.force |> ignore
    file1.Lines3 |> AList.force |> ignore

  [<Benchmark>]
  member _.Lines4_forceEvery10 () =
    let file1 = initFile ()
    for i in 1..n do
      addLine file1
      if i % 10 = 0 then file1.Lines3 |> AList.force |> ignore
    file1.Lines4 |> AList.force |> ignore


  [<Benchmark>]
  member _.File2_Lines_forceEvery10 () =
    let file1 = initFile2 ()
    for i in 1..n do
      addLine2 file1
      if i % 10 = 0 then file1.Lines |> AList.force |> ignore
    file1.Lines |> AList.force |> ignore



  [<Benchmark>]
  member _.Roslyn_Text_changeText_everyUpdate () =
    let mutable file = SourceText.From("Hello")
    file <- addToSourceText(file, { Start = { Line = 0; Character = 5 }; End = { Line = 0; Character = 5 } }, "World")
    for i in 1..n do
      file <- addToSourceText(file, { Start = { Line = 0; Character = 10 }; End = { Line = 0; Character = 10 } }, "\nLOL")
      file.Lines |> Seq.toArray |> ignore

  [<Benchmark>]
  member _.Lines1_forceEveryUpdate () =
    let file1 = initFile ()
    for i in 1..n do
      addLine file1
      file1.Lines |> AList.force |> ignore


  [<Benchmark>]
  member _.Lines2_forceEveryUpdate () =
    let file1 = initFile ()
    for i in 1..n do
      addLine file1
      file1.Lines2 |> AList.force |> ignore

  [<Benchmark>]
  member _.Lines3_forceEveryUpdate () =
    let file1 = initFile ()
    for i in 1..n do
      addLine file1
      file1.Lines3 |> AList.force |> ignore

  [<Benchmark>]
  member _.Lines4_forceEveryUpdate () =
    let file1 = initFile ()
    for i in 1..n do
      addLine file1
      file1.Lines4 |> AList.force |> ignore

  [<Benchmark>]
  member _.File2_Lines_forceEveryUpdate() =
    let file1 = initFile2 ()
    for i in 1..n do
      addLine2 file1
      file1.Lines |> AList.force |> ignore


[<MemoryDiagnoser>]
type SourceText_TextChanges_Benchmarks ()=

  let n = 100


  [<Benchmark>]
  member _.File1_Text_forceOnce () =
    let file1 = initFile ()
    for i in 1..n do
      addLine file1
    file1.Text |> AVal.force |> ignore


  [<Benchmark>]
  member _.File2_Text_forceOnce () =
    let file1 = initFile2 ()
    for i in 1..n do
      addLine2 file1
    file1.Text |> AVal.force |> ignore


  [<Benchmark(Baseline = true)>]
  member _.Roslyn_Text_changeText () =
    let mutable file = SourceText.From("Hello")
    file <- addToSourceText(file, { Start = { Line = 0; Character = 5 }; End = { Line = 0; Character = 5 } }, "World")
    for i in 1..n do
      file <- addToSourceText(file, { Start = { Line = 0; Character = 10 }; End = { Line = 0; Character = 10 } }, "\nLOL")
    file.ToString()




  [<Benchmark>]
  member _.File1_Lines1_forceEvery10 () =
    let file1 = initFile ()
    for i in 1..n do
      addLine file1
      if i % 10 = 0 then file1.Text |> AVal.force |> ignore
    file1.Text |> AVal.force |> ignore



  [<Benchmark>]
  member _.File2_Lines_forceEvery10 () =
    let file1 = initFile2 ()
    for i in 1..n do
      addLine2 file1
      if i % 10 = 0 then file1.Text |> AVal.force |> ignore
    file1.Text |> AVal.force |> ignore


  [<Benchmark>]
  member _.File1_Lines1_forceEveryUpdate () =
    let file1 = initFile ()
    for i in 1..n do
      addLine file1
      file1.Text |> AVal.force |> ignore


  [<Benchmark>]
  member _.File2_Lines_forceEveryUpdate() =
    let file1 = initFile2 ()
    for i in 1..n do
      addLine2 file1
      file1.Text |> AVal.force |> ignore
