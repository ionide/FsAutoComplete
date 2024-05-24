namespace Benchmarks

open System
open FSharp.Data.Adaptive
open Microsoft.CodeAnalysis.Text

type FileVersion = int


module Helpers =
  open FsAutoComplete.LspHelpers
  open FSharp.UMX
  open System.Collections.Generic

  let fileContents =
    IO.File.ReadAllText(
      @"C:\Users\jimmy\Repositories\public\TheAngryByrd\span-playground\Romeo and Juliet by William Shakespeare.txt"
    )



  let initRoslynSourceText () = SourceText.From(fileContents)


  let convertToTextSpan (sourceText: SourceText, range: Ionide.LanguageServerProtocol.Types.Range) =
    let start =
      uint32 sourceText.Lines.[int (max 0u range.Start.Line)].Start
      + range.Start.Character

    let endPosition =
      uint32 sourceText.Lines.[int32 (min range.End.Line (uint32 (sourceText.Lines.Count - 1)))].Start
      + range.End.Character

    TextSpan(int start, int endPosition - int start)

  let addToSourceText (sourceText: SourceText, range: Ionide.LanguageServerProtocol.Types.Range, text: string) =
    let textSpan = convertToTextSpan (sourceText, range)
    let newText = sourceText.WithChanges([| TextChange(textSpan, text) |])
    newText

  let addToSourceTextMany
    (sourceText: SourceText, spans: IEnumerable<Ionide.LanguageServerProtocol.Types.Range * string>)
    =
    let textSpans =
      spans
      |> Seq.map (fun (range, text) -> TextChange(convertToTextSpan (sourceText, range), text))
      |> Seq.toArray

    let newText = sourceText.WithChanges(textSpans)
    newText

open BenchmarkDotNet
open BenchmarkDotNet.Attributes
open Helpers
open BenchmarkDotNet.Jobs

[<MemoryDiagnoser>]
[<SimpleJob(RuntimeMoniker.Net70)>]
type SourceText_LineChanges_Benchmarks() =

  [<Params(1, 15, 50, 100, 1000)>]
  member val public N = 0 with get, set

  [<Benchmark>]
  member this.Roslyn_Text_changeText_everyUpdate() =
    let mutable file = initRoslynSourceText ()

    file <-
      addToSourceText (
        file,
        { Start = { Line = 0u; Character = 5u }
          End = { Line = 0u; Character = 5u } },
        "World"
      )

    for i in 1 .. this.N do
      file <-
        addToSourceText (
          file,
          { Start = { Line = 0u; Character = 10u }
            End = { Line = 0u; Character = 10u } },
          "\nLOL"
        )

      file.Lines |> Seq.toArray |> ignore
