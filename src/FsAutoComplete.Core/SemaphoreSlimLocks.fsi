namespace FsAutoComplete

open System
open System.Threading.Tasks

// /// <summary>
// /// An awaitable wrapper around a task whose result is disposable. The wrapper is not disposable, so this prevents usage errors like "use _lock = myAsync()" when the appropriate usage should be "use! _lock = myAsync())".
// /// </summary>
// [<Struct>]
// [<NoEquality; NoComparison>]
// type AwaitableDisposable<'T when 'T :> IDisposable> =
//   new: t: Task<'T> -> AwaitableDisposable<'T>
//   member GetAwaiter: unit -> Runtime.CompilerServices.TaskAwaiter<'T>
//   member AsTask: unit -> Task<'T>
//   static member op_Implicit: source: AwaitableDisposable<'T> -> Task<'T>

[<AutoOpen>]
module SemaphoreSlimExtensions =
  open System.Threading

  type SemaphoreSlim with

    member LockTask: ?ct: CancellationToken -> Task<IDisposable>
    member LockAsync: unit -> Async<IDisposable>
