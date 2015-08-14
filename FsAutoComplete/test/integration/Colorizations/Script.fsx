open System

#nowarn "40"

let a f = async { let! x = f "hello"
                  return x }

type LazyList<'T> =
  | Nil
  | Cons of 'T * Lazy<LazyList<'T>>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LazyList =
  let ofSeq (s:seq<'T>) =
    let en = s.GetEnumerator()
    let rec take() =
      if en.MoveNext() then
        Cons(en.Current, lazy take())
      else
        en.Dispose()
        Nil
    take()

type Parser<'T> = P of (LazyList<char> -> ('T * LazyList<char>) list)


let result v = P(fun c -> [v, c])
let zero () = P(fun _ -> [])
let bind (P p) f = P(fun inp ->
  [ for (pr, inp') in p inp do
      let (P pars) = f pr
      yield! pars inp' ])
let plus (P p) (P q) = P (fun inp ->
  (p inp) @ (q inp) )

let (<|>) p1 p2 = plus p1 p2

type ParserBuilder() =
  member x.Bind(v, f) = bind v f
  member x.Zero() = zero()
  member x.Return(v) = result(v)
  member x.ReturnFrom(p) = p
  member x.Combine(a, b) = plus a b
  member x.Delay(f) = f()

let parser = new ParserBuilder()

let item = P(function | LazyList.Nil -> [] | LazyList.Cons(c, r) -> [c,r.Value])

let sat p = parser { 
  let! v = item 
  if (p v) then return v }

let char x = sat ((=) x)
let digit = sat Char.IsDigit    
let lower = sat Char.IsLower
let upper = sat Char.IsUpper
let letter = sat Char.IsLetter

let alphanum = parser { 
  return! letter
  return! digit }  

let string (str:string) = 
  let chars = str.ToCharArray() |> List.ofSeq
  let rec string' = function
    | [] -> result []
    | x::xs -> parser { 
        let! y = char x
        let! ys = string' xs 
        return y::ys }
  string' chars

#if NOTDEFINED
let a = 1
#endif

#if INTERACTIVE
let b = 2
#endif

type String = System.String

type MyDictionary = System.Collections.Generic.Dictionary<String,String>

type MyAttribute(text : string) =
    inherit System.Attribute()
    
    do printfn "MyAttribute created. Text: %s" text
    
    member this.Text = text

[<MyAttribute("Hello world")>]    
type MyClass() =
    member this.SomeProperty = "This is a property"
