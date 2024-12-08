namespace FsAutoComplete

open System
open System.Threading.Tasks

/// <summary>
/// An awaitable wrapper around a task whose result is disposable. The wrapper is not disposable, so this prevents usage errors like "use _lock = myAsync()" when the appropriate usage should be "use! _lock = myAsync())".
/// </summary>
[<Struct>]
[<NoEquality; NoComparison>]
type AwaitableDisposable<'T when 'T :> IDisposable>(t: Task<'T>) =
  member x.GetAwaiter() = t.GetAwaiter()
  member x.AsTask() = t
  static member op_Implicit(source: AwaitableDisposable<'T>) = source.AsTask()

[<AutoOpen>]
module SemaphoreSlimExtensions =
  open System.Threading
  open IcedTasks
  // Based on https://gist.github.com/StephenCleary/7dd1c0fc2a6594ba0ed7fb7ad6b590d6
  // and https://gist.github.com/brendankowitz/5949970076952746a083054559377e56
  type SemaphoreSlim with

    member x.LockTask(?ct: CancellationToken) =
        task {
          let ct = defaultArg ct CancellationToken.None
          let t = x.WaitAsync(ct)

          do! t

          return
            { new IDisposable with
                member _.Dispose() =
                  // only release if the task completed successfully
                  // otherwise, we could be releasing a semaphore that was never acquired
                  if t.Status = TaskStatus.RanToCompletion then
                    x.Release() |> ignore }
        }

    member x.LockAsync() =
      asyncEx {
        let! ct = Async.CancellationToken
        return! x.LockTask ct
      }
