namespace Benchmarks

open System
open FSharp.Data.Adaptive
open Microsoft.CodeAnalysis.Text
type FileVersion = int


module Helpers =
  open FsAutoComplete.LspHelpers
  open FSharp.UMX
  open System.Collections.Generic

  let fileContents = IO.File.ReadAllText(@"C:\Users\jimmy\Repositories\public\TheAngryByrd\span-playground\Romeo and Juliet by William Shakespeare.txt")

  let initNamedText () =
    FsAutoComplete.NamedText(UMX.tag "lol", fileContents)

  let initRoslynSourceText () =
    SourceText.From(fileContents)


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

  let addToSourceTextMany (sourceText : SourceText, spans : IEnumerable<Ionide.LanguageServerProtocol.Types.Range * string>) =
    let textSpans = spans |> Seq.map (fun (range, text) -> TextChange(convertToTextSpan(sourceText, range), text)) |> Seq.toArray
    let newText = sourceText.WithChanges(textSpans)
    newText

  let addToNamedText (namedText : FsAutoComplete.NamedText, range : Ionide.LanguageServerProtocol.Types.Range, text : string)  =
    let range = protocolRangeToRange (UMX.untag namedText.FileName) range
    match namedText.ModifyText(range, text) with | Ok x -> x | Error e -> failwith e

open BenchmarkDotNet
open BenchmarkDotNet.Attributes
open Helpers
open BenchmarkDotNet.Jobs
[<MemoryDiagnoser>]
[<SimpleJob(RuntimeMoniker.Net70)>]
type SourceText_LineChanges_Benchmarks ()=

  [<Params(1, 15, 50, 100, 1000)>]
  member val public N = 0 with get, set

  [<Benchmark(Baseline = true)>]
  member this.Named_Text_changeText_everyUpdate () =
    let mutable file = initNamedText ()
    file <- addToNamedText(file, { Start = { Line = 0; Character = 5 }; End = { Line = 0; Character = 5 } }, "World")
    for i in 1..this.N do
      file <- addToNamedText(file, { Start = { Line = 0; Character = 10 }; End = { Line = 0; Character = 10 } }, "\nLOL")
      file.Lines |> Seq.toArray |> ignore

  [<Benchmark>]
  member this.Roslyn_Text_changeText_everyUpdate () =
    let mutable file = initRoslynSourceText ()
    file <- addToSourceText(file, { Start = { Line = 0; Character = 5 }; End = { Line = 0; Character = 5 } }, "World")
    for i in 1..this.N do
      file <- addToSourceText(file, { Start = { Line = 0; Character = 10 }; End = { Line = 0; Character = 10 } }, "\nLOL")
      file.Lines |> Seq.toArray |> ignore
