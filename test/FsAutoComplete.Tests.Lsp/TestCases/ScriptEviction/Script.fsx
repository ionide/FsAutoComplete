type DisposerThingy() =
  interface System.IDisposable with
    member x.Dispose() = ()

let foo = DisposerThingy()
