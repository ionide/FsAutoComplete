namespace FsAutoComplete.Adaptive

open System
open FSharp.Data.Adaptive
open FSharp.Data.Traceable

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
  let mapWithAdditionalDependenies (mapping: 'a -> 'b * #seq<#IAdaptiveValue>) (value: aval<'a>) : aval<'b> =
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
    /// <returns>An observable</returns>
    let onWeakMarking (aval: #aval<_>) =
      Observable.Create(fun (obs: IObserver<unit>) -> aval.AddWeakMarkingCallback(obs.OnNext))

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


  /// Reader for batchRecalc operations.
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
            | Some(o, remaingCache) ->
              let rem, rest = MultiSetMap.remove o i targets
              targets <- rest

              if rem then
                o.Outputs.Remove x |> ignore

              remaingCache
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

  let mapWithAdditionalDependenies
    (mapping: HashMap<'K, 'T1> -> HashMap<'K, 'T2 * #seq<#IAdaptiveValue>>)
    (map: amap<'K, 'T1>)
    =
    let mapping =
      mapping
      >> HashMap.map (fun _ v -> AVal.constant v |> AVal.mapWithAdditionalDependenies (id))

    batchRecalcDirty mapping map
