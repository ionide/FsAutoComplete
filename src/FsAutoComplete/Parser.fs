// --------------------------------------------------------------------------------------
// Simple monadic parser generator that we use for parsing STDI/O protocol messages
// --------------------------------------------------------------------------------------

#nowarn "40" // recursive references checked at runtime
namespace FsAutoComplete

open System

// --------------------------------------------------------------------------------------
// Simple implementation of LazyList
// --------------------------------------------------------------------------------------

type private LazyList<'T> =
  | Nil
  | Cons of 'T * Lazy<LazyList<'T>>

module private LazyList =
  let ofSeq (s:seq<'T>) =
    let en = s.GetEnumerator()
    let rec take() =
      if en.MoveNext() then
        Cons(en.Current, lazy take())
      else
        en.Dispose()
        Nil
    take()


module Parser =

  /// Add some useful methods for creating strings from sequences
  type String with
    static member OfSeq chars = chars |> Seq.toArray |> String
    static member OfReversedSeq chars = chars |> Seq.toArray |> Array.rev |> String

  /// Parser is implemented using lazy list (so that we can use seq<_>)
  type Parser<'T> = private P of (LazyList<char> -> ('T * LazyList<char>) list)

  // Basic functions needed by the computation builder

  let result v = P (fun c -> [v, c])
  let zero () = P (fun _ -> [])

  let bind (P p) f = P (fun inp ->
    [ for (pr, inp') in p inp do
        let (P pars) = f pr
        yield! pars inp' ])

  let plus (P p) (P q) = P (fun inp ->
    (p inp) @ (q inp) )

  let (<|>) p1 p2 = plus p1 p2

  type ParserBuilder() =
    member __.Bind(v, f) = bind v f
    member __.Zero() = zero()
    member __.Return(v) = result(v)
    member __.ReturnFrom(p) = p
    member __.Combine(a, b) = plus a b
    member __.Delay(f) = f()

  let parser = new ParserBuilder()

  // --------------------------------------------------------------------------------------
  // Basic combinators for composing parsers

  let item = P(function | LazyList.Nil -> [] | LazyList.Cons(c, r) -> [c,r.Value])

  let sat p = parser {
    let! v = item
    if (p v) then return v }

  let char x = sat ((=) x)
  let digit = sat Char.IsDigit
  let letter = sat Char.IsLetter
  let whitespace = sat (Char.IsWhiteSpace)

  let rec word = parser {
    return []
    return! parser {
      let! x = letter
      let! xs = word
      return x::xs } }

  let string (str:string) =
    let chars = str.ToCharArray() |> List.ofSeq
    let rec string' = function
      | [] -> result []
      | x::xs -> parser {
          let! y = char x
          let! ys = string' xs
          return y::ys }
    string' chars

  let rec many p = parser {
    return! parser {
      let! it = p
      let! res = many p
      return it::res }
    return [] }

  let rec some p = parser {
    let! first = p
    let! rest = many p
    return first::rest }

  let rec map f p = parser {
    let! v = p
    return f v }

  let optional p = parser {
    return! parser { let! v = p in return Some v }
    return None }

  let apply (P p) (str:seq<char>) =
    let res = str |> LazyList.ofSeq |> p
    res |> List.map fst

  /// Create sequence that reads the string forwards
  let createForwardStringReader (str:string) from = seq {
    for i in (max 0 from) .. (str.Length - 1) do yield str.[i] }

  /// Returns first result returned by the parser
  let getFirst p s = apply p s |> List.head
