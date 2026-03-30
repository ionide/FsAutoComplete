module X =
  let func x = x + 1

type MyDateTime() =
  static member Parse(s: string) = ()
  static member Parse(s: string, y: int) = ()
  static member Parse(s: string, z: MyDateTime) = ()

type A =
  abstract member Id: string
  abstract member Other: A

let rec x =
  { new A with
      member this.Id = "x"
      member this.Other = y }

and y =
  { new A with
      member this.Id = "y"
      member this.Other = x }

let z = y.Other.Id

let u = System.DateTime

let i () =
  let b = 1
  u

type LocalDbContext() =

  [<DefaultValue>]
  val mutable private test: int
