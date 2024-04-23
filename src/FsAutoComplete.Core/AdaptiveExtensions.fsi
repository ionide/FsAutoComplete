namespace FsAutoComplete.Adaptive

open System.Threading

[<AutoOpen>]
module AdaptiveExtensions =

  type System.Threading.CancellationTokenSource with

    /// Communicates a request for cancellation. Ignores ObjectDisposedException
    member TryCancel: unit -> unit
    /// Releases all resources used by the current instance of the System.Threading.CancellationTokenSource class.
    member TryDispose: unit -> unit

  type FSharp.Data.Adaptive.ChangeableHashMap<'Key, 'Value> with

    /// <summary>
    /// Adds the given key and calls the adder function if no previous key exists.
    /// Otherwise calls updater with the current key/value and returns a new value to be set.
    /// Returns true when the map changed.
    /// </summary>
    member AddOrUpdate: key: 'Key * adder: ('Key -> 'Value) * updater: ('Key -> 'Value -> 'Value) -> bool

  type FSharp.Data.Adaptive.ChangeableHashMap<'Key, 'Value> with

    /// <summary>
    /// Adds the given key and calls the adder function if no previous key exists.
    /// Otherwise calls updater with the current key/value but does not override existing value in the map.
    /// This is useful when the 'Value is itself a changeable value like a cval, cset, cmap which should be changed
    /// but the parent container doesn't need to know about those changes itself.
    /// </summary>
    member AddOrElse: key: 'Key * adder: ('Key -> 'Value) * updater: ('Key -> 'Value -> unit) -> unit

module Utils =

  val cheapEqual: a: 'T -> b: 'T -> bool

/// <summary>
/// Maps and calls dispose before mapping of new values. Useful for cleaning up callbacks like AddMarkingCallback for tracing purposes.
/// </summary>
type MapDisposableTupleVal<'T1, 'T2, 'Disposable when 'Disposable :> System.IDisposable> =
  inherit FSharp.Data.Adaptive.AVal.AbstractVal<'T2>

  new:
    mapping: ('T1 -> 'T2 * 'Disposable) * input: FSharp.Data.Adaptive.aval<'T1> ->
      MapDisposableTupleVal<'T1, 'T2, 'Disposable>

  override Compute: token: FSharp.Data.Adaptive.AdaptiveToken -> 'T2

module AVal =

  val mapOption: f: ('a -> 'b) -> (FSharp.Data.Adaptive.aval<'a option> -> FSharp.Data.Adaptive.aval<'b option>)

  /// <summary>
  /// Maps and calls dispose before mapping of new values. Useful for cleaning up callbacks like AddMarkingCallback for tracing purposes.
  /// </summary>
  val mapDisposableTuple:
    mapper: ('a -> 'b * #System.IDisposable) -> value: FSharp.Data.Adaptive.aval<'a> -> FSharp.Data.Adaptive.aval<'b>

  /// <summary>
  /// Calls a mapping function which creates additional dependencies to be tracked.
  /// </summary>
  val mapWithAdditionalDependencies:
    mapping: ('a -> 'b * #seq<'b1>) -> value: FSharp.Data.Adaptive.aval<'a> -> FSharp.Data.Adaptive.aval<'b>
      when 'b1 :> FSharp.Data.Adaptive.IAdaptiveValue

  /// <summary>
  /// Creates observables from adaptive values
  /// </summary>
  module Observable =

    /// <summary>
    /// Creates an observable with the given object and will be executed whenever the object gets marked out-of-date. Note that it does not trigger when the object is currently out-of-date.
    /// </summary>
    /// <param name="aval">The aval to get out-of-date information from.</param>
    val onOutOfDateWeak: aval: 'a -> System.IObservable<'a> when 'a :> FSharp.Data.Adaptive.IAdaptiveObject

    /// <summary>Creates an observable on the aval that will be executed whenever the avals value changed.</summary>
    /// <param name="aval">The aval to get out-of-date information from.</param>
    val onValueChangedWeak: aval: #FSharp.Data.Adaptive.aval<'b> -> System.IObservable<'b>

module ASet =

  /// Creates an amap with the keys from the set and the values given by mapping and
  /// adaptively applies the given mapping function to all elements and returns a new amap containing the results.
  val mapAtoAMap:
    mapper: ('a -> FSharp.Data.Adaptive.aval<'b>) ->
    src: FSharp.Data.Adaptive.aset<'a> ->
      FSharp.Data.Adaptive.amap<'a, 'b>

module AMap =
  open FSharp.Data.Adaptive

  /// A simple multi-map implementation.
  type internal MultiSetMap<'k, 'v> = FSharp.Data.Adaptive.HashMap<'k, FSharp.Data.Adaptive.HashSet<'v>>

  /// A simple multi-map implementation.
  module internal MultiSetMap =

    [<GeneralizableValue>]
    val empty<'k, 'v> : MultiSetMap<'k, 'v>

    val add: key: 'k -> value: 'v -> m: MultiSetMap<'k, 'v> -> MultiSetMap<'k, 'v>

    val remove: key: 'k -> value: 'v -> m: MultiSetMap<'k, 'v> -> bool * MultiSetMap<'k, 'v>

    val find: key: 'k -> m: MultiSetMap<'k, 'v> -> FSharp.Data.Adaptive.HashSet<'v>

  /// Reader for batchRecalc operations.
  [<Sealed>]
  type BatchRecalculateDirty<'k, 'a, 'b> =
    inherit FSharp.Data.Traceable.AbstractReader<FSharp.Data.Adaptive.HashMapDelta<'k, 'b>>

    new:
      input: FSharp.Data.Adaptive.amap<'k, 'a> *
      mapping: (FSharp.Data.Adaptive.HashMap<'k, 'a> -> FSharp.Data.Adaptive.HashMap<'k, FSharp.Data.Adaptive.aval<'b>>) ->
        BatchRecalculateDirty<'k, 'a, 'b>

    override Compute: t: FSharp.Data.Adaptive.AdaptiveToken -> FSharp.Data.Adaptive.HashMapDelta<'k, 'b>

    override InputChangedObject: t: obj * o: FSharp.Data.Adaptive.IAdaptiveObject -> unit

  val tryFindR: reason: 'a -> key: 'Key -> map: amap<'Key, 'Value> -> aval<Result<'Value, 'a>>

  /// Adaptively looks up the given key in the map and flattens the value to be easily worked with. Note that this operation should not be used extensively since its resulting aval will be re-evaluated upon every change of the map.
  val tryFindAndFlatten:
    key: 'Key ->
    map: FSharp.Data.Adaptive.amap<'Key, FSharp.Data.Adaptive.aval<'Value option>> ->
      FSharp.Data.Adaptive.aval<'Value option>

  /// Adaptively looks up the given key in the map and binds the value to be easily worked with. Note that this operation should not be used extensively since its resulting aval will be re-evaluated upon every change of the map.
  val tryFindA:
    key: 'Key ->
    map: FSharp.Data.Adaptive.amap<'Key, #FSharp.Data.Adaptive.aval<'Value>> ->
      FSharp.Data.Adaptive.aval<'Value option>

  /// Adaptively applies the given mapping function to all elements and returns a new amap containing the results.
  val mapAVal:
    mapper: ('Key -> 'InValue -> FSharp.Data.Adaptive.aval<'OutValue>) ->
    map: #FSharp.Data.Adaptive.amap<'Key, 'b> ->
      FSharp.Data.Adaptive.amap<'Key, FSharp.Data.Adaptive.aval<'OutValue>>
      when 'b :> FSharp.Data.Adaptive.aval<'InValue>

  /// Adaptively applies the given mapping to all changes and reapplies mapping on dirty outputs
  val batchRecalcDirty:
    mapping: (FSharp.Data.Adaptive.HashMap<'K, 'T1> -> FSharp.Data.Adaptive.HashMap<'K, FSharp.Data.Adaptive.aval<'T2>>) ->
    map: FSharp.Data.Adaptive.amap<'K, 'T1> ->
      FSharp.Data.Adaptive.amap<'K, 'T2>

  val mapWithAdditionalDependencies:
    mapping: (FSharp.Data.Adaptive.HashMap<'K, 'T1> -> FSharp.Data.Adaptive.HashMap<'K, ('T2 * #seq<'b>)>) ->
    map: FSharp.Data.Adaptive.amap<'K, 'T1> ->
      FSharp.Data.Adaptive.amap<'K, 'T2>
      when 'b :> FSharp.Data.Adaptive.IAdaptiveValue

/// <summary>
/// A task creator that caches the task and cancels it when no longer needed.
/// </summary>
/// <remarks>
/// Since the task can be references multiple times in the dependency graph, it is important to cancel it only after there are no more references to it.
/// </remarks>
type internal RefCountingTaskCreator<'a> =

  new: create: (System.Threading.CancellationToken -> System.Threading.Tasks.Task<'a>) -> RefCountingTaskCreator<'a>

  /// <summary>Creates a new task based on the creation function from the constructor.  If a task has already been created, returns a cached version of the inflight task.</summary>
  member New: unit -> AdaptiveCancellableTask<'a>

  /// <summary>Decrements the reference count and cancels the CancellationTokenSource if there are no more references.</summary>
  member private RemoveRef: unit -> unit

/// <summary>
/// Represents a task that can be cancelled.
/// </summary>
/// <remarks>
/// Upon cancellation, it will run the cancel function passed in and set cancellation for the task completion source.
/// </remarks>
and [<Class>] AdaptiveCancellableTask<'a> =

  new: cancel: (unit -> unit) * real: System.Threading.Tasks.Task<'a> -> AdaptiveCancellableTask<'a>

  /// <summary>Will run the cancel function passed into the constructor and set the output Task to cancelled state.</summary>
  member Cancel: CancellationToken -> unit

  /// <summary>The output of the passed in task to the constructor.</summary>
  /// <returns></returns>
  member Task: System.Threading.Tasks.Task<'a>

type asyncaval<'a> =
  inherit FSharp.Data.Adaptive.IAdaptiveObject

  abstract GetValue: FSharp.Data.Adaptive.AdaptiveToken -> AdaptiveCancellableTask<'a>

module CancellableTask =

  /// <summary>Converts AdaptiveCancellableTask to a CancellableTask.</summary>
  val inline ofAdaptiveCancellableTask:
    ct: AdaptiveCancellableTask<'a> -> ctok: System.Threading.CancellationToken -> System.Threading.Tasks.Task<'a>

module Async =

  /// <summary>Converts AdaptiveCancellableTask to an Async.</summary>
  val inline ofAdaptiveCancellableTask: ct: AdaptiveCancellableTask<'a> -> Async<'a>

[<AutoOpen>]
module Extensions =
  type IcedTasks.CancellableTaskBase.CancellableTaskBuilderBase with

    /// <summary>Allows implicit conversion of a AdaptiveCancellableTask to a CancellableTask in a cancellableTask CE.</summary>
    member inline Source:
      ct: AdaptiveCancellableTask<'a> ->
        (System.Threading.CancellationToken -> System.Runtime.CompilerServices.TaskAwaiter<'a>)

module AsyncAVal =
  open System.Threading

  /// <summary>
  /// Evaluates the given adaptive value and returns a Task containing the value.
  /// This should not be used inside the adaptive evaluation
  /// of other AdaptiveObjects since it does not track dependencies.
  /// </summary>
  /// <remarks>
  /// This follows Task semantics and is already running.
  /// </remarks>
  val force: value: asyncaval<'a> -> AdaptiveCancellableTask<'a>

  /// <summary>
  /// Evaluates the given adaptive value and returns an Async containing the value.
  /// This should not be used inside the adaptive evaluation
  /// of other AdaptiveObjects since it does not track dependencies.
  /// </summary>
  /// <remarks>
  /// This follows Async semantics and is not already running.
  /// </remarks>
  val forceAsync: value: asyncaval<'a> -> Async<'a>

  /// <summary>
  /// Evaluates the given adaptive value and returns a CancellableTask containing the value.
  /// This should not be used inside the adaptive evaluation
  /// of other AdaptiveObjects since it does not track dependencies.
  /// </summary>
  /// <remarks>
  /// This follows CancellableTask semantics and is not already running.
  /// </remarks>
  val forceCancellableTask: value: asyncaval<'a> -> IcedTasks.CancellableTasks.CancellableTask<'a>

  /// A constant value that results in a Task.
  type ConstantVal<'a> =
    inherit FSharp.Data.Adaptive.ConstantObject
    interface asyncaval<'a>

    new: value: System.Threading.Tasks.Task<'a> -> ConstantVal<'a>

    new: value: AdaptiveCancellableTask<'a> -> ConstantVal<'a>

  /// <summary>
  /// Base class for standard Async Adaptive Values.
  /// </summary>
  [<AbstractClass>]
  type AbstractVal<'a> =
    inherit FSharp.Data.Adaptive.AdaptiveObject
    interface asyncaval<'a>

    new: unit -> AbstractVal<'a>

    abstract Compute: FSharp.Data.Adaptive.AdaptiveToken -> AdaptiveCancellableTask<'a>

    member GetValue: token: FSharp.Data.Adaptive.AdaptiveToken -> AdaptiveCancellableTask<'a>

  /// <summary>
  /// Creates a constant async adaptive value always holding the given value.
  /// </summary>
  val constant: value: 'a -> asyncaval<'a>

  /// <summary>
  /// Creates a constant async adaptive value always holding the task.
  /// </summary>
  val ofTask: value: System.Threading.Tasks.Task<'a> -> asyncaval<'a>

  val ofCancellableTask: value: IcedTasks.CancellableTasks.CancellableTask<'a> -> asyncaval<'a>
  val ofCancellableValueTask: value: IcedTasks.CancellableValueTasks.CancellableValueTask<'a> -> asyncaval<'a>

  val ofAsync: value: Async<'a> -> asyncaval<'a>

  /// <summary>
  /// Creates an async adaptive value evaluation the given value.
  /// </summary>
  val ofAVal: value: FSharp.Data.Adaptive.aval<'a> -> asyncaval<'a>

  /// <summary>
  /// Returns a new async adaptive value that adaptively applies the mapping function to the given
  /// adaptive inputs.
  /// </summary>
  val map:
    mapping: ('a -> System.Threading.CancellationToken -> System.Threading.Tasks.Task<'b>) ->
    input: asyncaval<'a> ->
      asyncaval<'b>

  /// <summary>
  /// Returns a new async adaptive value that adaptively applies the mapping function to the given
  /// adaptive inputs.
  /// </summary>
  val mapAsync: mapping: ('a -> Async<'b>) -> input: asyncaval<'a> -> asyncaval<'b>

  /// <summary>
  /// Returns a new async adaptive value that adaptively applies the mapping function to the given
  /// adaptive inputs.
  /// </summary>
  val mapSync: mapping: ('a -> System.Threading.CancellationToken -> 'b) -> input: asyncaval<'a> -> asyncaval<'b>

  /// <summary>
  /// Returns a new async adaptive value that adaptively applies the mapping function to the given
  /// adaptive inputs.
  /// </summary>
  val map2:
    mapping: ('a -> 'b -> System.Threading.CancellationToken -> System.Threading.Tasks.Task<'c>) ->
    ca: asyncaval<'a> ->
    cb: asyncaval<'b> ->
      asyncaval<'c>

  /// Returns a new async adaptive value that adaptively applies the mapping function to the given
  /// input and adaptively depends on the resulting adaptive value.
  /// The resulting adaptive value  will hold the latest value of the asyncaval<_> returned by mapping.
  val bind:
    mapping: ('a -> System.Threading.CancellationToken -> asyncaval<'b>) -> value: asyncaval<'a> -> asyncaval<'b>

  /// Returns a new async adaptive value that adaptively applies the mapping function to the given
  /// optional adaptive inputs.
  val mapOption:
    f: ('a -> System.Threading.CancellationToken -> 'b) -> value: asyncaval<'a option> -> asyncaval<'b option>

  val mapResult:
    f: ('a -> CancellationToken -> 'b) -> value: asyncaval<Result<'a, 'Error>> -> asyncaval<Result<'b, 'Error>>

type AsyncAValBuilder =

  new: unit -> AsyncAValBuilder

  member inline Bind: value: asyncaval<'T1> * mapping: ('T1 -> asyncaval<'T2>) -> asyncaval<'T2>

  member inline Bind:
    value: asyncaval<'T1> * mapping: ('T1 -> System.Threading.CancellationToken -> asyncaval<'T2>) -> asyncaval<'T2>

  member inline BindReturn: value: asyncaval<'T1> * mapping: ('T1 -> Async<'T2>) -> asyncaval<'T2>

  member inline BindReturn:
    value: asyncaval<'T1> * mapping: ('T1 -> System.Threading.CancellationToken -> System.Threading.Tasks.Task<'T2>) ->
      asyncaval<'T2>

  member inline MergeSources: v1: asyncaval<'T1> * v2: asyncaval<'T2> -> asyncaval<'T1 * 'T2>

  member inline Return: value: 'T -> asyncaval<'T>

  member inline ReturnFrom: value: asyncaval<'T> -> asyncaval<'T>

  member inline Source: value: asyncaval<'T> -> asyncaval<'T>

[<AutoOpen>]
module AsyncAValBuilderExtensions =
  open IcedTasks
  val asyncAVal: AsyncAValBuilder

  type AsyncAValBuilder with

    member inline Source: value: FSharp.Data.Adaptive.aval<'T> -> asyncaval<'T>
    member inline Source: value: System.Threading.Tasks.Task<'T> -> asyncaval<'T>
    member inline Source: value: Async<'T> -> asyncaval<'T>
    member inline Source: value: CancellableTask<'T> -> asyncaval<'T>
    member inline Source: value: CancellableValueTask<'T> -> asyncaval<'T>

    member inline BindReturn:
      value: asyncaval<'T1> * mapping: ('T1 -> System.Threading.CancellationToken -> 'T2) -> asyncaval<'T2>

    member inline BindReturn: value: asyncaval<'T1> * mapping: ('T1 -> 'T2) -> asyncaval<'T2>

module AMapAsync =
  open FSharp.Data.Adaptive

  /// <summary>
  /// Adaptively maps over the given map lifting the value in the map to be an asyncaval.
  /// </summary>
  val mapAVal:
    mapper: ('Key -> 'InValue -> System.Threading.CancellationToken -> asyncaval<'OutValue>) ->
    map: #FSharp.Data.Adaptive.amap<'Key, 'b> ->
      FSharp.Data.Adaptive.amap<'Key, asyncaval<'OutValue>>
      when 'b :> FSharp.Data.Adaptive.aval<'InValue>

  /// <summary>
  /// Adaptively maps over the given map.
  /// </summary>
  val mapAsyncAVal:
    mapper: ('Key -> 'InValue -> System.Threading.CancellationToken -> asyncaval<'OutValue>) ->
    map: #FSharp.Data.Adaptive.amap<'Key, 'b> ->
      FSharp.Data.Adaptive.amap<'Key, asyncaval<'OutValue>>
      when 'b :> asyncaval<'InValue>

  /// Adaptively looks up the given key in the map and binds the value to be easily worked with. Note that this operation should not be used extensively since its resulting aval will be re-evaluated upon every change of the map.
  val tryFindA: key: 'Key -> map: FSharp.Data.Adaptive.amap<'Key, #asyncaval<'Value>> -> asyncaval<'Value option>

  /// Adaptively looks up the given key in the map and flattens the value to be easily worked with. Note that this operation should not be used extensively since its resulting aval will be re-evaluated upon every change of the map.
  val tryFindAndFlatten:
    key: 'Key -> map: FSharp.Data.Adaptive.amap<'Key, asyncaval<'Value option>> -> asyncaval<'Value option>

  val tryFindAndFlattenR:
    reason: 'Error ->
    key: 'Key ->
    map: amap<'Key, asyncaval<Result<'Value, 'Error>>> ->
      asyncaval<Result<'Value, 'Error>>
