namespace FsAutoComplete.Adaptive

open System
open FSharp.Data.Adaptive
open FSharp.Data.Traceable
open System.Threading.Tasks
open IcedTasks
open System.Threading

[<AutoOpen>]
module AdaptiveExtensions =
  type ChangeableHashMap<'Key, 'Value> with

    /// <summary>
    /// Adds the given key and calls the adder function if no previous key exists.
    /// Otherwise calls updater with the current key/value and returns a new value to be set.
    /// Returns true when the map changed.
    /// </summary>
    member x.AddOrUpdate(key, adder, updater) =
      match x.TryGetValue key with
      | None -> x.Add(key, adder key)
      | Some v -> x.Add(key, updater key v)

    /// <summary>
    /// Adds the given key and calls the adder function if no previous key exists.
    /// Otherwise calls updater with the current key/value but does not override existing value in the map.
    /// This is useful when the 'Value is itself a changeable value like a cval, aset, amap which should be changed
    /// but the parent container doesn't need to know about those changes itself.
    /// </summary>
    member x.AddOrElse(key, adder, updater) =
      match x.TryGetValue key with
      | None -> x.Add(key, adder key) |> ignore
      | Some v -> updater key v

    member x.GetOrAdd(key, adder: 'Key -> 'Value) : 'Value =
      match x.TryGetValue key with
      | Some x -> x
      | None ->
        let v = adder key
        x.Add(key, v) |> ignore
        v


module Utils =
  let cheapEqual (a: 'T) (b: 'T) =
    ShallowEqualityComparer<'T>.Instance.Equals(a, b)

/// <summary>
/// Maps and calls dispose before mapping of new values. Useful for cleaning up callbacks like AddMarkingCallback for tracing purposes.
/// </summary>
type MapDisposableTupleVal<'T1, 'T2, 'Disposable when 'Disposable :> IDisposable>
  (mapping: 'T1 -> ('T2 * 'Disposable), input: aval<'T1>) =
  inherit AVal.AbstractVal<'T2>()

  let mutable cache: ValueOption<struct ('T1 * 'T2 * 'Disposable)> = ValueNone

  override x.Compute(token: AdaptiveToken) =
    let i = input.GetValue token

    match cache with
    | ValueSome(struct (a, b, _)) when Utils.cheapEqual a i -> b
    | ValueSome(struct (a, b, c)) ->
      (c :> IDisposable).Dispose()
      let (b, c) = mapping i
      cache <- ValueSome(struct (i, b, c))
      b
    | ValueNone ->
      let (b, c) = mapping i
      cache <- ValueSome(struct (i, b, c))
      b

module AVal =
  let mapOption f = AVal.map (Option.map f)

  /// <summary>
  /// Maps and calls dispose before mapping of new values. Useful for cleaning up callbacks like AddMarkingCallback for tracing purposes.
  /// </summary>
  let mapDisposableTuple mapper value =
    MapDisposableTupleVal(mapper, value) :> aval<_>

  /// <summary>
  /// Calls a mapping function which creates additional dependencies to be tracked.
  /// </summary>
  let mapWithAdditionalDependencies (mapping: 'a -> 'b * #seq<#IAdaptiveValue>) (value: aval<'a>) : aval<'b> =
    let mutable lastDeps = HashSet.empty

    { new AVal.AbstractVal<'b>() with
        member x.Compute(token: AdaptiveToken) =
          let input = value.GetValue token

          // re-evaluate the mapping based on the (possibly new input)
          let result, deps = mapping input

          // compute the change in the additional dependencies and adjust the graph accordingly
          let newDeps = HashSet.ofSeq deps

          for op in HashSet.computeDelta lastDeps newDeps do
            match op with
            | Add(_, d) ->
              // the new dependency needs to be evaluated with our token, s.t. we depend on it in the future
              d.GetValueUntyped token |> ignore
            | Rem(_, d) ->
              // we no longer need to depend on the old dependency so we can remove ourselves from its outputs
              lock d.Outputs (fun () -> d.Outputs.Remove x) |> ignore

          lastDeps <- newDeps

          result }
    :> aval<_>

  /// <summary>
  /// Creates observables from adaptive values
  /// </summary>
  module Observable =
    open System.Reactive.Linq

    /// <summary>
    /// Creates an observable with the given object and will be executed whenever the object gets marked out-of-date. Note that it does not trigger when the object is currently out-of-date.
    /// </summary>
    /// <param name="aval">The aval to get out-of-date information from.</param>
    let onOutOfDateWeak (aval: #aval<_>) =
      Observable.Create(fun (obs: IObserver<_>) -> aval.AddWeakMarkingCallback(fun _ -> obs.OnNext aval))


    /// <summary>Creates an observable on the aval that will be executed whenever the avals value changed.</summary>
    /// <param name="aval">The aval to get out-of-date information from.</param>
    let onValueChangedWeak (aval: #aval<_>) =
      Observable.Create(fun (obs: IObserver<_>) -> aval.AddCallback(obs.OnNext))

module ASet =
  /// Creates an amap with the keys from the set and the values given by mapping and
  /// adaptively applies the given mapping function to all elements and returns a new amap containing the results.
  let mapAtoAMap mapper src =
    src |> ASet.mapToAMap mapper |> AMap.mapA (fun _ v -> v)

module AMap =
  open FSharp.Data.Traceable

  /// A simple multi-map implementation.
  type internal MultiSetMap<'k, 'v> = HashMap<'k, HashSet<'v>>

  /// A simple multi-map implementation.
  module internal MultiSetMap =
    [<GeneralizableValue>]
    let empty<'k, 'v> : MultiSetMap<'k, 'v> = HashMap.empty

    let add (key: 'k) (value: 'v) (m: MultiSetMap<'k, 'v>) : MultiSetMap<'k, 'v> =
      m
      |> HashMap.alter key (fun old ->
        match old with
        | Some old -> Some(HashSet.add value old)
        | None -> Some(HashSet.single value))

    let remove (key: 'k) (value: 'v) (m: MultiSetMap<'k, 'v>) : bool * MultiSetMap<'k, 'v> =
      let wasLast = ref false

      let result =
        m
        |> HashMap.alter key (fun old ->
          match old with
          | None -> None
          | Some old ->
            let s = HashSet.remove value old

            if HashSet.isEmpty s then
              wasLast.Value <- true
              None
            else
              Some s)

      wasLast.Value, result

    let find (key: 'k) (m: MultiSetMap<'k, 'v>) =
      match HashMap.tryFind key m with
      | Some s -> s
      | None -> HashSet.empty


  /// Reader for batchRecalculate operations.
  [<Sealed>]
  type BatchRecalculateDirty<'k, 'a, 'b>(input: amap<'k, 'a>, mapping: HashMap<'k, 'a> -> HashMap<'k, aval<'b>>) =
    inherit AbstractReader<HashMapDelta<'k, 'b>>(HashMapDelta.empty)

    let reader = input.GetReader()
    do reader.Tag <- "input"
    let cacheLock = obj ()
    let mutable cache: HashMap<'k, aval<'b>> = HashMap.Empty
    let mutable targets = MultiSetMap.empty<aval<'b>, 'k>
    let mutable dirty = HashMap.empty<'k, aval<'b>>

    let consumeDirty () =
      lock cacheLock (fun () ->
        let d = dirty
        dirty <- HashMap.empty
        d)

    override x.InputChangedObject(t, o) =
#if FABLE_COMPILER
      if isNull o.Tag then
        let o = unbox<aval<'b>> o

        for i in MultiSetMap.find o targets do
          dirty <- HashMap.add i o dirty
#else
      match o with
      | :? aval<'b> as o ->
        lock cacheLock (fun () ->
          for i in MultiSetMap.find o targets do
            dirty <- HashMap.add i o dirty

        )
      | _ -> ()
#endif

    override x.Compute t =
      let mutable dirty = consumeDirty ()
      let old = reader.State
      let ops = reader.GetChanges t |> HashMapDelta.toHashMap

      let setOps, removeOps =
        ((HashMap.empty, HashMap.empty), ops)
        ||> HashMap.fold (fun (sets, rems) i op ->
          dirty <- HashMap.remove i dirty

          cache <-
            match HashMap.tryRemove i cache with
            | Some(o, remainingCache) ->
              let rem, rest = MultiSetMap.remove o i targets
              targets <- rest

              if rem then
                o.Outputs.Remove x |> ignore

              remainingCache
            | None -> cache

          match op with
          | Set v -> HashMap.add i v sets, rems
          | Remove -> sets, HashMap.add i Remove rems)


      let mutable changes = HashMap.empty

      let setOps =
        (setOps, dirty)
        ||> HashMap.fold (fun s k _ ->
          match HashMap.tryFind k old with
          | Some v -> HashMap.add k v s
          | None -> s)

      for i, k in mapping setOps do
        cache <- HashMap.add i k cache
        let v = k.GetValue t
        targets <- MultiSetMap.add k i targets
        changes <- HashMap.add i (Set v) changes

      HashMap.union removeOps changes |> HashMapDelta


  /// Adaptively looks up the given key in the map and flattens the value to be easily worked with. Note that this operation should not be used extensively since its resulting aval will be re-evaluated upon every change of the map.
  let tryFindAndFlatten (key: 'Key) (map: amap<'Key, aval<option<'Value>>>) =
    aval {
      match! AMap.tryFind key map with
      | Some x -> return! x
      | None -> return None
    }

  /// Adaptively looks up the given key in the map and binds the value to be easily worked with. Note that this operation should not be used extensively since its resulting aval will be re-evaluated upon every change of the map.
  let tryFindA (key: 'Key) (map: amap<'Key, #aval<'Value>>) =
    aval {
      match! AMap.tryFind key map with
      | Some v ->
        let! v2 = v
        return Some v2
      | None -> return None
    }

  /// Adaptively applies the given mapping function to all elements and returns a new amap containing the results.
  let mapAVal
    (mapper: 'Key -> 'InValue -> aval<'OutValue>)
    (map: #amap<'Key, #aval<'InValue>>)
    : amap<'Key, aval<'OutValue>> =
    map |> AMap.map (fun k v -> AVal.bind (mapper k) v)


  /// Adaptively applies the given mapping to all changes and reapplies mapping on dirty outputs
  let batchRecalcDirty (mapping: HashMap<'K, 'T1> -> HashMap<'K, aval<'T2>>) (map: amap<'K, 'T1>) =
    // if map.IsConstant then
    //     let map = force map |> mapping
    //     if map |> HashMap.forall (fun _ v -> v.IsConstant) then
    //         constant (fun () -> map |> HashMap.map (fun _ v -> AVal.force v))
    //     else
    //         // TODO better impl possible
    //         create (fun () -> BatchRecalculateDirty(ofHashMap map, id))
    // else
    AMap.ofReader (fun () -> BatchRecalculateDirty(map, mapping))

  let mapWithAdditionalDependencies
    (mapping: HashMap<'K, 'T1> -> HashMap<'K, 'T2 * #seq<#IAdaptiveValue>>)
    (map: amap<'K, 'T1>)
    =
    let mapping =
      mapping
      >> HashMap.map (fun _ v -> AVal.constant v |> AVal.mapWithAdditionalDependencies (id))

    batchRecalcDirty mapping map

/// <summary>
/// A task creator that caches the task and cancels it when no longer needed.
/// </summary>
/// <remarks>
/// Since the task can be references multiple times in the dependency graph, it is important to cancel it only after there are no more references to it.
/// </remarks>
type internal RefCountingTaskCreator<'a>(create: CancellationToken -> Task<'a>) =

  let mutable refCount = 0
  let mutable cache: option<Task<'a>> = None
  let mutable cancel: CancellationTokenSource = null

  /// <summary>Decrements the reference count and cancels the CancellationTokenSource if there are no more references.</summary>
  member private x.RemoveRef() =
    lock x (fun () ->
      if refCount = 1 then
        refCount <- 0
        cancel.Cancel()
        cancel.Dispose()
        cancel <- null
        cache <- None
      else
        refCount <- refCount - 1)

  /// <summary>Creates a new task based on the creation function from the constructor.  If a task has already been created, returns a cached version of the inflight task.</summary>
  member x.New() =
    lock x (fun () ->
      match cache with
      | Some cache ->
        refCount <- refCount + 1
        AdaptiveCancellableTask(x.RemoveRef, cache)
      | None ->
        cancel <- new CancellationTokenSource()
        let task = create cancel.Token
        cache <- Some task
        refCount <- refCount + 1
        AdaptiveCancellableTask(x.RemoveRef, task))

/// <summary>
/// Represents a task that can be cancelled.
/// </summary>
/// <remarks>
/// Upon cancellation, it will run the cancel function passed in and set cancellation for the task completion source.
/// </remarks>
and AdaptiveCancellableTask<'a>(cancel: unit -> unit, real: Task<'a>) =
  let cts = new CancellationTokenSource()

  let output =
    if real.IsCompleted then
      real
    else
      let tcs = new TaskCompletionSource<'a>()

      let s = cts.Token.Register(fun () -> tcs.TrySetCanceled() |> ignore)

      real.ContinueWith(fun (t: Task<'a>) ->
        s.Dispose()

        if t.IsFaulted then tcs.TrySetException(t.Exception)
        elif t.IsCanceled then tcs.TrySetCanceled()
        else tcs.TrySetResult(t.Result))
      |> ignore

      tcs.Task

  /// <summary>Will run the cancel function passed into the constructor and set the output Task to cancelled state.</summary>
  member x.Cancel() =
    cancel ()
    cts.Cancel()

  /// <summary>The output of the passed in task to the constructor.</summary>
  /// <returns></returns>
  member x.Task = output

type asyncaval<'a> =
  inherit IAdaptiveObject
  abstract GetValue: AdaptiveToken -> AdaptiveCancellableTask<'a>

module CancellableTask =
  /// <summary>Converts AdaptiveCancellableTask to a CancellableTask.</summary>
  let inline ofAdaptiveCancellableTask (ct: AdaptiveCancellableTask<_>) =
    fun (ctok: CancellationToken) ->
      task {
        use _ = ctok.Register(fun () -> ct.Cancel())
        return! ct.Task
      }

module Async =
  /// <summary>Converts AdaptiveCancellableTask to an Async.</summary>
  let inline ofAdaptiveCancellableTask (ct: AdaptiveCancellableTask<_>) =
    async {
      let! ctok = Async.CancellationToken
      use _ = ctok.Register(fun () -> ct.Cancel())
      return! ct.Task |> Async.AwaitTask
    }

[<AutoOpenAttribute>]
module Extensions =

  type IcedTasks.CancellableTasks.CancellableTaskBuilderBase with

    /// <summary>Allows implicit conversion of a AdaptiveCancellableTask to a CancellableTask in a cancellableTask CE.</summary>
    member inline x.Source(ct: AdaptiveCancellableTask<_>) =
      fun ctok -> (CancellableTask.ofAdaptiveCancellableTask ct ctok).GetAwaiter()


module AsyncAVal =

  /// <summary>
  /// Evaluates the given adaptive value and returns a Task containing the value.
  /// This should not be used inside the adaptive evaluation
  /// of other AdaptiveObjects since it does not track dependencies.
  /// </summary>
  /// <remarks>
  /// This follows Task semantics and is already running.
  /// </remarks>
  let force (value: asyncaval<_>) = value.GetValue(AdaptiveToken.Top)

  /// <summary>
  /// Evaluates the given adaptive value and returns an Async containing the value.
  /// This should not be used inside the adaptive evaluation
  /// of other AdaptiveObjects since it does not track dependencies.
  /// </summary>
  /// <remarks>
  /// This follows Async semantics and is not already running.
  /// </remarks>
  let forceAsync (value: asyncaval<_>) =
    async {
      let ct = value.GetValue(AdaptiveToken.Top)
      return! Async.ofAdaptiveCancellableTask ct
    }

  /// <summary>
  /// Evaluates the given adaptive value and returns a CancellableTask containing the value.
  /// This should not be used inside the adaptive evaluation
  /// of other AdaptiveObjects since it does not track dependencies.
  /// </summary>
  /// <remarks>
  /// This follows CancellableTask semantics and is not already running.
  /// </remarks>
  let forceCancellableTask (value: asyncaval<_>) =
    cancellableTask {
      let ct = value.GetValue(AdaptiveToken.Top)
      return! ct
    }

  /// A constant value that results in a Task.
  type ConstantVal<'a>(value: AdaptiveCancellableTask<'a>) =
    inherit ConstantObject()

    new(value: Task<'a>) = ConstantVal<'a>(AdaptiveCancellableTask(id, value))

    interface asyncaval<'a> with
      member x.GetValue _ = value

  /// <summary>
  /// Base class for standard Async Adaptive Values.
  /// </summary>
  [<AbstractClass>]
  type AbstractVal<'a>() =
    inherit AdaptiveObject()
    abstract Compute: AdaptiveToken -> AdaptiveCancellableTask<'a>

    member x.GetValue token = x.EvaluateAlways token x.Compute

    interface asyncaval<'a> with
      member x.GetValue t = x.GetValue t

  /// <summary>
  /// Creates a constant async adaptive value always holding the given value.
  /// </summary>
  let constant (value: 'a) =
    ConstantVal(Task.FromResult value) :> asyncaval<_>

  /// <summary>
  /// Creates a constant async adaptive value always holding the task.
  /// </summary>
  let ofTask (value: Task<'a>) = ConstantVal(value) :> asyncaval<_>

  let ofCancellableTask (value: CancellableTask<'a>) =
    ConstantVal(
      let cts = new CancellationTokenSource()

      let cancel () =
        cts.Cancel()
        cts.Dispose()

      let real =
        task {
          try
            return! value cts.Token
          finally
            cts.Dispose()
        }

      AdaptiveCancellableTask(cancel, real)
    )
    :> asyncaval<_>


  let ofAsync (value: Async<'a>) =
    ConstantVal(
      let cts = new CancellationTokenSource()

      let cancel () =
        cts.Cancel()
        cts.Dispose()

      let real =
        task {
          try
            return! Async.StartImmediateAsTask(value, cts.Token)
          finally
            cts.Dispose()
        }

      AdaptiveCancellableTask(cancel, real)
    )
    :> asyncaval<_>


  /// <summary>
  /// Creates an async adaptive value evaluation the given value.
  /// </summary>
  let ofAVal (value: aval<'a>) =
    if value.IsConstant then
      ConstantVal(Task.FromResult(AVal.force value)) :> asyncaval<_>
    else
      { new AbstractVal<'a>() with
          member x.Compute t =
            let real = Task.FromResult(value.GetValue t)
            AdaptiveCancellableTask(id, real) }
      :> asyncaval<_>


  /// <summary>
  /// Returns a new async adaptive value that adaptively applies the mapping function to the given
  /// adaptive inputs.
  /// </summary>
  let map (mapping: 'a -> CancellationToken -> Task<'b>) (input: asyncaval<'a>) =
    let mutable cache: option<RefCountingTaskCreator<'b>> = None

    { new AbstractVal<'b>() with
        member x.Compute t =
          if x.OutOfDate || Option.isNone cache then
            let ref =
              RefCountingTaskCreator(
                cancellableTask {
                  let! ct = CancellableTask.getCancellationToken ()
                  let it = input.GetValue t
                  let s = ct.Register(fun () -> it.Cancel())

                  try
                    let! i = it
                    return! mapping i
                  finally
                    s.Dispose()
                }
              )

            cache <- Some ref
            ref.New()
          else
            cache.Value.New() }
    :> asyncaval<_>


  /// <summary>
  /// Returns a new async adaptive value that adaptively applies the mapping function to the given
  /// adaptive inputs.
  /// </summary>
  let mapAsync (mapping: 'a -> Async<'b>) (input: asyncaval<'a>) =
    let mutable cache: option<RefCountingTaskCreator<'b>> = None

    { new AbstractVal<'b>() with
        member x.Compute t =
          if x.OutOfDate || Option.isNone cache then
            let ref =
              RefCountingTaskCreator(
                cancellableTask {
                  let! ct = CancellableTask.getCancellationToken ()
                  let it = input.GetValue t
                  let s = ct.Register(fun () -> it.Cancel())

                  try
                    let! i = it
                    return! mapping i
                  finally
                    s.Dispose()
                }
              )

            cache <- Some ref
            ref.New()
          else
            cache.Value.New() }
    :> asyncaval<_>


  /// <summary>
  /// Returns a new async adaptive value that adaptively applies the mapping function to the given
  /// adaptive inputs.
  /// </summary>
  let mapSync (mapping: 'a -> CancellationToken -> 'b) (input: asyncaval<'a>) =
    map (fun a ct -> Task.FromResult(mapping a ct)) input

  /// <summary>
  /// Returns a new async adaptive value that adaptively applies the mapping function to the given
  /// adaptive inputs.
  /// </summary>
  let map2 (mapping: 'a -> 'b -> CancellationToken -> Task<'c>) (ca: asyncaval<'a>) (cb: asyncaval<'b>) =
    let mutable cache: option<RefCountingTaskCreator<'c>> = None

    { new AbstractVal<'c>() with
        member x.Compute t =
          if x.OutOfDate || Option.isNone cache then
            let ref =
              RefCountingTaskCreator(
                cancellableTask {
                  let ta = ca.GetValue t
                  let tb = cb.GetValue t

                  let! ct = CancellableTask.getCancellationToken ()

                  let s =
                    ct.Register(fun () ->
                      ta.Cancel()
                      tb.Cancel())

                  try
                    let! va = ta
                    let! vb = tb
                    return! mapping va vb
                  finally
                    s.Dispose()
                }
              )

            cache <- Some ref
            ref.New()
          else
            cache.Value.New() }
    :> asyncaval<_>

  /// Returns a new async adaptive value that adaptively applies the mapping function to the given
  /// input and adaptively depends on the resulting adaptive value.
  /// The resulting adaptive value  will hold the latest value of the asyncaval<_> returned by mapping.
  let bind (mapping: 'a -> CancellationToken -> asyncaval<'b>) (value: asyncaval<'a>) =
    let mutable cache: option<_> = None
    let mutable innerCache: option<_> = None
    let mutable inputChanged = 0
    let inners: ref<HashSet<asyncaval<'b>>> = ref HashSet.empty

    { new AbstractVal<'b>() with

        override x.InputChangedObject(_, o) =
          if System.Object.ReferenceEquals(o, value) then
            inputChanged <- 1

            lock inners (fun () ->
              for i in inners.Value do
                i.Outputs.Remove x |> ignore

              inners.Value <- HashSet.empty)

        member x.Compute t =
          if x.OutOfDate then
            if Interlocked.Exchange(&inputChanged, 0) = 1 || Option.isNone cache then
              let outerTask =
                RefCountingTaskCreator(
                  cancellableTask {
                    let it = value.GetValue t
                    let! ct = CancellableTask.getCancellationToken ()
                    let s = ct.Register(fun () -> it.Cancel())

                    try
                      let! i = it
                      let inner = mapping i ct
                      return inner
                    finally
                      s.Dispose()
                  }
                )

              cache <- Some outerTask

            let outerTask = cache.Value

            let ref =
              RefCountingTaskCreator(
                cancellableTask {
                  let innerCellTask = outerTask.New()

                  let! ct = CancellableTask.getCancellationToken ()
                  let s = ct.Register(fun () -> innerCellTask.Cancel())

                  try
                    let! inner = innerCellTask
                    let innerTask = inner.GetValue t
                    lock inners (fun () -> inners.Value <- HashSet.add inner inners.Value)

                    let s2 =
                      ct.Register(fun () ->
                        innerTask.Cancel()
                        lock inners (fun () -> inners.Value <- HashSet.remove inner inners.Value)
                        inner.Outputs.Remove x |> ignore)

                    try
                      let! innerValue = innerTask
                      return innerValue
                    finally
                      s2.Dispose()
                  finally
                    s.Dispose()
                }
              )

            innerCache <- Some ref

            ref.New()
          else
            innerCache.Value.New()

    }
    :> asyncaval<_>


  /// Returns a new async adaptive value that adaptively applies the mapping function to the given
  /// optional adaptive inputs.
  let mapOption f (value: asyncaval<'a option>) : asyncaval<'b option> =
    mapSync (fun data ctok -> data |> Option.map (fun d -> f d ctok)) value

type AsyncAValBuilder() =

  member inline x.MergeSources(v1: asyncaval<'T1>, v2: asyncaval<'T2>) =
    (v1, v2)
    ||> AsyncAVal.map2 (fun a b ctok ->
      if ctok.IsCancellationRequested then
        Task.FromCanceled<_>(ctok)
      else
        Task.FromResult(a, b))


  // member inline x.MergeSources3(v1 : aval<'T1>, v2 : aval<'T2>, v3 : aval<'T3>) =
  //     AVal.map3 (fun a b c -> a,b,c) v1 v2 v3

  member inline x.BindReturn(value: asyncaval<'T1>, [<InlineIfLambda>] mapping: 'T1 -> CancellationToken -> Task<'T2>) =
    AsyncAVal.map mapping value

  member inline x.BindReturn(value: asyncaval<'T1>, [<InlineIfLambda>] mapping: 'T1 -> Async<'T2>) =
    AsyncAVal.mapAsync mapping value

  member inline x.BindReturn(value: asyncaval<'T1>, [<InlineIfLambda>] mapping: 'T1 -> Task<'T2>) =
    AsyncAVal.map (fun data _ -> mapping data) value

  member inline x.Bind(value: asyncaval<'T1>, [<InlineIfLambda>] mapping: 'T1 -> CancellationToken -> asyncaval<'T2>) =
    AsyncAVal.bind (mapping) value

  member inline x.Bind(value: asyncaval<'T1>, [<InlineIfLambda>] mapping: 'T1 -> asyncaval<'T2>) =
    AsyncAVal.bind (fun data _ -> mapping data) value

  member inline x.Return(value: 'T) = AsyncAVal.constant value

  member inline x.ReturnFrom(value: asyncaval<'T>) = value

  member inline x.Source(value: asyncaval<'T>) = value

[<AutoOpen>]
module AsyncAValBuilderExtensions =
  let asyncAVal = AsyncAValBuilder()

  type AsyncAValBuilder with

    member inline x.Source(value: aval<'T>) = AsyncAVal.ofAVal value
    member inline x.Source(value: Task<'T>) = AsyncAVal.ofTask value

    member inline x.BindReturn(value: asyncaval<'T1>, [<InlineIfLambda>] mapping: 'T1 -> CancellationToken -> 'T2) =
      AsyncAVal.mapSync (fun data ctok -> mapping data ctok) value

    member inline x.BindReturn(value: asyncaval<'T1>, [<InlineIfLambda>] mapping: 'T1 -> 'T2) =
      AsyncAVal.mapSync (fun data ctok -> mapping data) value

module AMapAsync =

  /// <summary>
  /// Adaptively maps over the given map lifting the value in the map to be an asyncaval.
  /// </summary>
  let mapAVal
    (mapper: 'Key -> 'InValue -> CancellationToken -> asyncaval<'OutValue>)
    (map: #amap<'Key, #aval<'InValue>>)
    : amap<'Key, asyncaval<'OutValue>> =
    map |> AMap.map (fun k v -> v |> AsyncAVal.ofAVal |> AsyncAVal.bind (mapper k))

  /// <summary>
  /// Adaptively maps over the given map.
  /// </summary>
  let mapAsyncAVal
    (mapper: 'Key -> 'InValue -> CancellationToken -> asyncaval<'OutValue>)
    (map: #amap<'Key, #asyncaval<'InValue>>)
    : amap<'Key, asyncaval<'OutValue>> =
    map |> AMap.map (fun k v -> v |> AsyncAVal.bind (mapper k))

  /// Adaptively looks up the given key in the map and binds the value to be easily worked with. Note that this operation should not be used extensively since its resulting aval will be re-evaluated upon every change of the map.
  let tryFindA (key: 'Key) (map: amap<'Key, #asyncaval<'Value>>) =
    asyncAVal {
      match! AMap.tryFind key map with
      | Some v ->
        let! v2 = v
        return Some v2
      | None -> return None
    }


  /// Adaptively looks up the given key in the map and flattens the value to be easily worked with. Note that this operation should not be used extensively since its resulting aval will be re-evaluated upon every change of the map.
  let tryFindAndFlatten (key: 'Key) (map: amap<'Key, asyncaval<option<'Value>>>) =
    asyncAVal {
      match! AMap.tryFind key map with
      | Some x -> return! x
      | None -> return None
    }
