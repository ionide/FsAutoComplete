module FileTwo

type Foo =
  | Bar
  | Qux

let addition x y = x + y

let add x y = x + y

type NewObjectType() =

  member x.Terrific (y : int, z : char) : int =  y
  member x.Terrific (y : int, z : System.DateTime) : int =  y
  member x.Terrific (y : Set<'a>, z : int) : Set<'a> =  y
  
