// --------------------------------------------------------------------------------------
// Simple monadic parser generator that we use in the IntelliSense
// --------------------------------------------------------------------------------------

#nowarn "40" // recursive references checked at runtime
namespace FsAutoComplete

open System
open Microsoft.FSharp.Compiler

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
  open System

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

// --------------------------------------------------------------------------------------
/// Parsing utilities for IntelliSense (e.g. parse identifier on the left-hand side
/// of the current cursor location etc.)
module Parsing =
  open Parser

  module Legacy =
    let inline isFirstOpChar ch =
        ch = '!' || ch = '%'|| ch = '&' || ch = '*'|| ch = '+'|| ch = '-'|| ch = '.'|| ch = '/'|| ch = '<'|| ch = '='|| ch = '>'|| ch = '@'|| ch = '^'|| ch = '|'|| ch = '~'
  
    let isOpChar ch = ch = '?' || isFirstOpChar ch

    let parseBackSymOpFragment = parser {
      // This is unfortunate, but otherwise cracking at $ in A.$B
      // causes the backward parse to return a symbol fragment.
      let! c  = sat (fun c -> c <> '.' && isOpChar c)
      let! cs = many (sat isOpChar)
      return String.OfReversedSeq (c::cs)
      }

    /// Parses F# short-identifier (i.e. not including '.'); also ignores active patterns
    // let parseIdent =
    //   parseActivePatternEnd <|> parseSymOpFragment <|> many fsharpIdentCharacter
    //    |> map String.ofSeq
    let fsharpIdentCharacter = sat PrettyNaming.IsIdentifierPartCharacter

    let rawIdChar = sat (fun c -> c <> '\n' && c <> '\t' && c <> '\r' && c <> '`')

    /// Parse F# short-identifier and reverse the resulting string
    let parseBackIdent =
      parser {
          let! x = optional (string "``")
          let! res = many (if x.IsSome then rawIdChar else fsharpIdentCharacter) |> map String.OfReversedSeq
          let! _ = optional (string "``")
          return res }

    /// Parse remainder of a long identifier before '.' (e.g. "Name.space.")
    /// (designed to look backwards - reverses the results after parsing)
    let rec parseBackLongIdentRest = parser {
      return! parser {
        let! _ = char '.'
        let! ident = parseBackIdent
        let! rest = parseBackLongIdentRest
        return ident::rest }
      return [] }

    let parseBackLongIdent = parser {
      return! parser {
        let! ident = parseBackSymOpFragment <|> parseBackIdent
        let! rest = parseBackLongIdentRest
        return ident::rest |> List.rev }
      return [] }

    let parseBackTriggerThenLongIdent = parser {
      let! _ = char '(' <|> char '<'
      let! _  = many whitespace
      return! parseBackLongIdent
      }

  /// Create sequence that reads the string backwards
  let createBackStringReader (str:string) from = seq {
    for i in (min from (str.Length - 1)) .. -1 .. 0 do yield str.[i] }

  /// Create sequence that reads the string forwards
  let createForwardStringReader (str:string) from = seq {
    for i in (max 0 from) .. (str.Length - 1) do yield str.[i] }

  /// Returns first result returned by the parser
  let getFirst p s = apply p s |> List.head
  let tryGetFirst p s = match apply p s with h::_ -> Some h | [] -> None

  let inline private tryGetLexerSymbolIslands sym =
      match sym.Text with 
      | "" -> None 
      | _ -> Some (sym.RightColumn, sym.Text.Split '.' |> Array.toList)

  // Parsing - find the identifier around the current location
  // (we look for full identifier in the backward direction, but only
  // for a short identifier forward - this means that when you hover
  // 'B' in 'A.B.C', you will get intellisense for 'A.B' module)
  let findIdents col lineStr lookupType =
      if lineStr = "" then None
      else
          Lexer.getSymbol 0 col lineStr lookupType [||]
          |> Option.bind tryGetLexerSymbolIslands

  let findLongIdents (col, lineStr) =
    findIdents col lineStr SymbolLookupKind.Fuzzy

  let findLongIdentsAndResidue (col, lineStr:string) =
      let lineStr = lineStr.Substring(0, col)

      match Lexer.getSymbol 0 col lineStr SymbolLookupKind.ByLongIdent [||] with
      | Some sym ->
          match sym.Text with
          | "" -> [], ""
          | text ->
              let res = text.Split '.' |> List.ofArray |> List.rev
              if lineStr.[col - 1] = '.' then res |> List.rev, ""
              else
                  match res with
                  | head :: tail -> tail |> List.rev, head
                  | [] -> [], ""
      | _ -> [], ""

  /// find the identifier prior to a '(' or ',' once the method tip trigger '(' shows
  let findLongIdentsAtGetMethodsTrigger (col, lineStr) =
    let lookBack = createBackStringReader lineStr col
    let backIdentOpt = tryGetFirst Legacy.parseBackTriggerThenLongIdent lookBack
    match backIdentOpt with
    | None -> None
    | Some backIdent ->

    let identIsland =
      match List.rev backIdent with
      | last::prev -> (last::prev |> List.rev)
      | [] -> []

    match identIsland with
    | [] | [ "" ] -> None
    | _ -> Some identIsland