open System
open System.Collections
open System.Collections.Generic
open System.Runtime.ExceptionServices

#r "nuget: Microsoft.Extensions.Logging"
open Microsoft.Extensions.Logging

[<AutoOpen>]
module private LibInternals =
  type ILL = InlineIfLambdaAttribute

[<AutoOpen>]
module Library =

  let inline ``panic!`` (message: string) : 'T =
    try
      raise (InvalidProgramException message)
    with x ->
      Environment.FailFast("Fatal error; program must exit!", x)
      Unchecked.defaultof<'T>


[<Interface>]
type IFault =
  abstract Message: string
  abstract Cause: IFault option


type Demotion<'X when 'X :> exn>(source: 'X, ?message: string) =
  member _.Source: 'X = source

  member val Message: string = defaultArg message source.Message

  interface IFault with
    member me.Message = me.Message
    member _.Cause = None


type DemotionDI<'X when 'X :> exn> internal (edi: ExceptionDispatchInfo, ?message: string) =
  inherit Demotion<'X>(edi.SourceException :?> 'X, ?message = message)
  member _.ExceptionDispatchInfo = edi
  member inline _.Rethrow() = edi.Throw()

type Faulty<'T when 'T: (member Message: string)> = 'T


[<RequireQualifiedAccess>]
module Fault =
  let inline demote (source: 'X :> exn) = Demotion<'X>(source)

  let inline capture (source: 'X :> exn) = ExceptionDispatchInfo.Capture(source) |> DemotionDI<'X>

  let inline demoteAggregate (source: AggregateException) =
    let exs = source.Flatten()

    if 1 = exs.InnerExceptions.Count then
      demote exs.InnerExceptions.[0]
    else
      demote source

  let inline derive<'T when Faulty<'T>> (faulty: 'T) : IFault =
    match box faulty with
    | :? IFault as fault -> fault
    | :? exn as source -> Demotion source
    | _ ->
      { new IFault with
          member _.Message = faulty.Message
          member _.Cause = None }

  let inline promote ([<ILL>] toExn: IFault -> 'X) (fault: IFault) : 'X :> exn = toExn fault

  let inline escalate ([<ILL>] toExn: IFault -> 'X) (fault: IFault) : 'X :> exn = fault |> promote toExn |> raise

  let inline repromote ([<ILL>] wrapper: exn -> exn) (fault: Demotion<'X>) = fault.Source |> wrapper |> raise


[<StructuralEquality; StructuralComparison>]
[<Struct>]
type Report<'Pass, 'Fail when 'Fail :> IFault> =
  | Pass of value: 'Pass
  | Fail of fault: 'Fail

  static member inline op_Implicit(report: Report<'Pass, 'Fail>) : Result<'Pass, 'Fail> =
    match report with
    | Pass(value: 'Pass) -> Ok value
    | Fail(error: 'Fail) -> Error error

  static member inline op_Implicit(result: Result<'Pass, 'Fail>) : Report<'Pass, 'Fail> =
    match result with
    | Ok(value: 'Pass) -> Pass value
    | Error(error: 'Fail) -> Fail error


[<AutoOpen>]
module Patterns =
  open System
  open System.Threading.Tasks

  [<return: Struct>]
  let inline (|FailAs|_|) (report: Report<'Pass, IFault>) : 'Fail voption when 'Fail :> IFault =
    match report with
    | Fail(:? 'Fail as fault) -> ValueSome fault
    | Fail _
    | Pass _ -> ValueNone

  [<return: Struct>]
  let inline (|DemotedAs|_|) (fault: IFault) : 'X voption when 'X :> exn =
    match fault with
    | :? Demotion<'X> as x -> ValueSome x.Source
    | :? Demotion<exn> as x when (x.Source :? 'X) -> ValueSome(downcast x.Source)
    | _ -> ValueNone

  [<return: Struct>]
  let inline (|Demoted|_|) (report: Report<'Pass, IFault>) =
    match report with
    | Fail(DemotedAs(demoted: 'X)) -> ValueSome demoted
    | _ -> ValueNone

  [<return: Struct>]
  let inline (|OperationCanceledException|_|) (report: Report<'Pass, IFault>) =
    match report with
    | Demoted(c: OperationCanceledException) -> ValueSome()
    | _ -> ValueNone

  [<return: Struct>]
  let inline (|TaskCanceledException|_|) (report: Report<'Pass, IFault>) =
    match report with
    | Demoted(c: TaskCanceledException) -> ValueSome()
    | _ -> ValueNone




[<RequireQualifiedAccess>]
module Report =
  let inline ofFault (fault: IFault) : Report<'Pass, IFault> = Fail fault

  let inline ofExn (fault: 'X) : Report<'Pass, Demotion<'X>> = fault |> Demotion<_> |> Fail

  let inline bind ([<ILL>] pass: 'Pass -> Report<'T, 'Fail>) (report: Report<'Pass, 'Fail>) : Report<'T, 'Fail> =
    match report with
    | Pass value -> pass value
    | Fail error -> Fail error

  let inline bindFail ([<ILL>] fail: 'Fail -> Report<'Pass, 'T>) (report: Report<'Pass, 'Fail>) : Report<'Pass, 'T> =
    match report with
    | Pass value -> Pass value
    | Fail error -> fail error

  let inline map ([<ILL>] pass: 'Pass -> 'T) (report: Report<'Pass, 'Fail>) : Report<'T, 'Fail> =
    report |> bind (pass >> Pass)

  let inline mapFail ([<ILL>] fail: 'Fail -> 'T) (report: Report<'Pass, 'Fail>) : Report<'Pass, 'T> =
    report |> bindFail (fail >> Fail)

  let inline generalize (report: Report<'Pass, 'Fail>) : Report<'Pass, IFault> =
    report |> mapFail (fun fault -> upcast fault)

  let inline iter ([<ILL>] pass: 'Pass -> unit) (report: Report<'Pass, 'Fail>) : unit =
    match report with
    | Pass value -> pass value
    | Fail _ -> ( (* noop *) )

  let inline iterFail ([<ILL>] fail: 'Fail -> unit) (report: Report<'Pass, 'Fail>) : unit =
    match report with
    | Pass _ -> ( (* noop *) )
    | Fail fault -> fail fault

  let inline isPass (report: Report<'Pass, 'Fail>) : bool =
    match report with
    | Pass _ -> true
    | Fail _ -> false

  let inline isFail (report: Report<'Pass, 'Fail>) : bool =
    match report with
    | Pass _ -> false
    | Fail _ -> true

  let inline toResult (report: Report<'Pass, 'Fail>) : Result<'Pass, 'Fail> = Report.op_Implicit report

  let inline ofResult (result: Result<'Pass, 'Fail>) : Report<'Pass, 'Fail> = Report.op_Implicit result

  let inline toOption (report: Report<'Pass, 'Fail>) : 'Pass option =
    match report with
    | Pass value -> Some value
    | Fail _ -> None

  let inline ofOption ([<ILL>] withFault: unit -> 'Fail) (option: 'Pass option) : Report<'Pass, 'Fail> =
    match option with
    | Some value -> Pass value
    | None -> Fail(withFault ())

  let inline toChoice (report: Report<'Pass, 'Fail>) : Choice<'Pass, 'Fail> =
    match report with
    | Pass value -> Choice1Of2 value
    | Fail fault -> Choice2Of2 fault

  let inline ofChoice (choice: Choice<'Pass, 'Fail>) : Report<'Pass, 'Fail> =
    match choice with
    | Choice1Of2 value -> Pass value
    | Choice2Of2 fault -> Fail fault

  let inline defaultValue (value: 'Pass) (report: Report<'Pass, 'Fail>) : 'Pass =
    match report with
    | Pass value' -> value'
    | Fail _ -> value

  let inline defaultWith ([<ILL>] withFault: 'Fail -> 'Pass) (report: Report<'Pass, 'Fail>) : 'Pass =
    match report with
    | Pass value -> value
    | Fail fault -> withFault fault


[<Sealed>]
type CompoundFault(faults: IFault seq, ?message: string, ?cause: IFault) =
  do (* .ctor *)
    if isNull faults then
      nullArg (nameof faults)
    elif Seq.length faults < 1 then
      invalidArg (nameof faults) "Must provide at least one fault."

  member val Faults: IFault seq = faults |> Seq.toArray |> Seq.readonly

  member val Message: string = defaultArg message "One or more errors occurred"

  interface IFault with
    member me.Message = me.Message
    member _.Cause = cause

  interface IEnumerable<IFault> with
    member me.GetEnumerator() = me.Faults.GetEnumerator()
    member me.GetEnumerator() = (me.Faults :> IEnumerable).GetEnumerator()


[<RequireQualifiedAccess>]
module Array =
  open Microsoft.FSharp.Core.CompilerServices

  let inline divide (items: Report<'Pass, 'Fail> array) : 'Pass array * 'Fail array =
    if isNull items then
      nullArg (nameof items)
    elif 0 = Array.length items then
      (Array.empty, Array.empty)
    else

      let mutable passing, failing = ArrayCollector<'Pass>(), ArrayCollector<_>()

      for item in items do
        match item with
        | Pass value -> passing.Add(value)
        | Fail fault -> failing.Add(fault)

      (passing.Close(), failing.Close())

  let inline accumulate ([<ILL>] project: 'T -> Report<'Pass, IFault>) (items: 'T array) : Report<'Pass array, CompoundFault> =
    if isNull items then
      nullArg (nameof items)
    elif 0 = Array.length items then
      Pass Array.empty
    else
      let mutable passing, failing = ArrayCollector<'Pass>(), ArrayCollector<_>()

      for item in items do
        match project item with
        | Pass value -> passing.Add(value)
        | Fail error -> failing.Add(error)

      let failing = failing.Close()

      if 0 < failing.Length then
        Fail(CompoundFault failing)
      else
        Pass(passing.Close())

  let inline traverse ([<ILL>] project: 'T -> Report<'Pass, IFault>) (items: 'T array) : Report<'Pass array, IFault> =
    if isNull items then
      nullArg (nameof items)
    elif 0 = Array.length items then
      Pass Array.empty
    else
      let mutable buffer = ArrayCollector<'Pass>()
      let mutable halted = ValueOption.None
      let enum = (items :> 'T seq).GetEnumerator()

      while ValueOption.isNone halted && enum.MoveNext() do
        match project enum.Current with
        | Pass value -> buffer.Add(value)
        | Fail error -> halted <- ValueSome error

      match halted with
      | ValueSome error -> Fail error
      | ValueNone -> Pass(buffer.Close())

  let inline sequence (reports: Report<'Pass, IFault> array) : Report<'Pass array, IFault> = reports |> traverse id


[<RequireQualifiedAccess>]
module List =
  open Microsoft.FSharp.Core.CompilerServices

  let inline divide (items: Report<'Pass, 'Fail> list) : 'Pass list * 'Fail list =
    match items with
    | [] -> (List.empty, List.empty)
    | items ->
      let mutable passing, failing = ListCollector<'Pass>(), ListCollector<_>()

      for item in items do
        match item with
        | Pass value -> passing.Add(value)
        | Fail fault -> failing.Add(fault)

      (passing.Close(), failing.Close())

  let inline accumulate ([<ILL>]project: 'T -> Report<'Pass, IFault>) (items: 'T list) : Report<'Pass list, CompoundFault> =
    match items with
    | [] -> Pass List.empty
    | items ->
      let mutable passing, failing = ListCollector<'Pass>(), ArrayCollector<_>()

      for item in items do
        match project item with
        | Pass value -> passing.Add(value)
        | Fail error -> failing.Add(error)

      let failing = failing.Close()

      if 0 < failing.Length then
        Fail(CompoundFault failing)
      else
        Pass(passing.Close())

  let inline traverse ([<ILL>] project: 'T -> Report<'Pass, IFault>) (items: 'T list) : Report<'Pass list, IFault> =
    match items with
    | [] -> Pass List.empty
    | items ->
      let mutable buffer = ListCollector<'Pass>()
      let mutable halted = ValueOption.None
      let enum = (items :> 'T seq).GetEnumerator()

      while ValueOption.isNone halted && enum.MoveNext() do
        match project enum.Current with
        | Pass value -> buffer.Add(value)
        | Fail error -> halted <- ValueSome error

      match halted with
      | ValueSome error -> Fail error
      | ValueNone -> Pass(buffer.Close())

  let inline sequence (reports: Report<'Pass, IFault> list) : Report<'Pass list, IFault> = reports |> traverse id


[<RequireQualifiedAccess>]
module Seq =
  let inline divide (items: Report<'Pass, 'Fail> seq) : 'Pass seq * 'Fail seq =
    let passing, failing = items |> Seq.toArray |> Array.divide
    (Seq.ofArray passing, Seq.ofArray failing)

  let inline accumulate ([<ILL>] project: 'T -> Report<'Pass, IFault>) (items: 'T seq) : Report<'Pass seq, CompoundFault> =
    items |> Seq.toArray |> Array.accumulate project |> Report.map Array.toSeq

  let inline traverse ([<ILL>] project: 'T -> Report<'Pass, IFault>) (items: 'T seq) : Report<'Pass seq, IFault> =
    items |> Seq.toArray |> Array.traverse project |> Report.map Array.toSeq

  let inline sequence (reports: Report<'Pass, IFault> seq) : Report<'Pass seq, IFault> = reports |> traverse id



type ReportBuilder() =

  member _.Return v = Pass v

  member inline _.ReturnFrom(v: Report<_, _>) = v

  member inline _.ReturnFrom(exn: exn) = Report.ofExn exn

  member inline _.Bind(report, [<ILL>] f) = Report.bind f report



// type AsyncReportBuilder() =

//   member _.Return v = async.Return(Pass v)

//   member inline _.ReturnFrom(v: Async<Report<_, _>>) = v

//   member inline _.Bind(report, [<ILL>] f) = async {
//     let! report = report
//     match report with
//     | Pass value -> return! f value
//     | Fail error -> return Fail error
//   }



// type ILogError =
//   abstract LogIt: Logger<_> -> unit


[<AutoOpen>]
module ReportBuilderExtensions =

  let report = ReportBuilder()

module Example =

  type WorkFailed =
    | Timeout
    | NetworkError
    | UnknownError

    interface IFault with
      member this.Cause: IFault option = None

      member this.Message: string =
        match this with
        | Timeout -> "The operation timed out."
        | NetworkError -> "A network error occurred."
        | UnknownError -> "An unknown error occurred."
    // interface ILogError with
    //   member this.LogIt logger =
    //     match this with
    //     | Timeout -> logger.LogError("Timeout")
    //     | NetworkError -> logger.LogError("NetworkError")
    //     | UnknownError -> logger.LogError("UnknownError")

  [<return: Struct>]
  let inline (|FailAsWorkFailed|_|) x =
    match x with
    | FailAs(fault: WorkFailed) -> ValueSome fault
    | _ -> ValueNone

  let doWork (magicFail: int) =
    report {
      if magicFail = 42 then
        return! Fail WorkFailed.Timeout
      else
        return "lol"
    }

  type OtherWorkFailed =
    | CatNeedsPetting
    | CatNeedsFed
    | CatInLap

    interface IFault with
      member this.Cause: IFault option = None

      member this.Message: string =
        match this with
        | CatNeedsPetting -> "The cat needs petting."
        | CatNeedsFed -> "The cat needs fed."
        | CatInLap -> "The cat is in your lap. You cannot move"

  let doWork2 magicFail =
    report {
      if magicFail = 1701 then
        return! Fail OtherWorkFailed.CatInLap
      else
        return "lmao"
    }

  let doSomethingStupid (divisor) =
    try
      1 / divisor |> Pass
    with e ->
      Report.ofExn e

  let doWorkAgain () =
    report {
      let! x = doWork 43 |> Report.generalize
      let! y = doWork2 1702 |> Report.generalize
      let! z = doSomethingStupid 0 |> Report.generalize
      return x, y, z
    }

  let handler () =
    match doWorkAgain () with
    | Pass(x) -> printfn "Success: %A" x
    | FailAsWorkFailed(fault) ->
      match fault with
      | WorkFailed.NetworkError -> printfn "Network Error"
      | WorkFailed.Timeout -> printfn "Timeout"
      | WorkFailed.UnknownError -> printfn "Unknown Error"
    | Demoted(f: System.DivideByZeroException) -> eprintfn "%A" f
    | Fail error -> printfn "Error: %s" error.Message

open Example

handler ()



let doException () =
  let doWork () = raise (new System.Exception("This is an exception"))

  let thrown () =
    printfn "---Doing Thrown ---"
    let mutable original = null
    let mutable dispatchInfo = null

    try
      try
        doWork ()
      with e ->
        original <- e
        dispatchInfo <- ExceptionDispatchInfo.Capture(e)
        raise e
    with e ->
      printfn "Original Exception: %A" original
      printfn "Thrown: %A" e

    try
      dispatchInfo.Throw()
    with e ->
      printfn "Dispatched Exception: %A" e

    printfn "---Done Thrown ---"

  thrown ()

  let doingReraise () =
    printfn "---Doing doingReraise ---"
    let mutable original = null
    let mutable dispatchInfo = null

    try
      try
        doWork ()
      with e ->
        original <- e
        dispatchInfo <- ExceptionDispatchInfo.Capture(e)
        reraise ()
    with e ->
      printfn "Original Exception: %A" original
      printfn "Thrown: %A" e

    try
      dispatchInfo.Throw()
    with e ->
      printfn "Dispatched Exception: %A" e

    printfn "---Done doingReraise ---"

  let doingInner () =
    printfn "---Doing doingInner ---"
    let mutable original = null
    // let mutable dispatchInfo = null
    try
      try
        doWork ()
      with e ->
        original <- e
        // dispatchInfo <- ExceptionDispatchInfo.Capture(e)
        reraise ()
    with e ->
      printfn "Original Exception: %A" original
      printfn "Thrown: %A" e

    try
      raise (new System.Exception("This is an outer exception", original))
    with e ->
      printfn "Dispatched Exception: %A" e

    printfn "---Done doingInner ---"

  thrown ()
  doingReraise ()
  doingInner ()

doException ()
