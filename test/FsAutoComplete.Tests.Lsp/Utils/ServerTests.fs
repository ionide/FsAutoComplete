module Utils.ServerTests

open FsAutoComplete.Utils
open Helpers
open Utils.Server
open Expecto
open Ionide.LanguageServerProtocol.Types

/// TestList which creates (in `initialize`) and caches (if `cacheValue`) a value, and runs cleanup after all tests were run (in `cleanup`)
///
/// Note: TestCase for `cleanup` is called `cleanup`
///
/// Note: no value is created when there are no `tests`, and neither gets `cleanup` executed
///
/// Note: Result of `initialize` is only cached when `cacheValue` is `true`. But `cleanup` is called regardless.
///       Use `false` when `initialize` returns an already cached value, otherwise `false`.
///       Then in here `Async.Cache` is used to cache value.
let cleanableTestList
  runner
  (name: string)
  (initialize: Async<'a>)
  (cacheValue: bool)
  (cleanup: Async<'a> -> Async<unit>)
  (tests: Async<'a> -> Test list)
  =
  let value = if cacheValue then initialize |> Async.Cache else initialize
  let tests = tests value

  testSequenced
  <| runner
    name
    [ yield! tests

      if not (tests |> List.isEmpty) then
        testCaseAsync "cleanup" (cleanup value) ]

let private serverTestList' runner name createServer config path tests =
  // path must be "absolutely normalized". `..` (parent) isn't valid -> Uri in FSAC and uri in doc are otherwise different, which leads to infinte waiting or timeouts.
  let path = path |> Option.map (System.IO.Path.GetFullPath)

  let init = Server.create path config createServer
  let cleanup = Server.shutdown

  cleanableTestList runner name init false cleanup tests

/// ## Example
/// ```fsharp
/// let tests state = serverTestList "Simple Test" state defaultConfigDto None (fun server -> [
///   testCaseAsync "can get diagnostics" <| async {
///     let! (doc, diags) = server |> Server.createUntitledDocument "let foo = bar"
///     use doc = doc // ensure doc gets closed (disposed) after test
///
///     Expect.exists diags (fun d -> d.Message = "The value or constructor 'bar' is not defined.") "Should have `bar not defined` error"
///   }
/// ])
/// ```
let serverTestList = serverTestList' testList
let fserverTestList = serverTestList' ftestList
let pserverTestList = serverTestList' ptestList

let private documentTestList'
  runner
  name
  (server: CachedServer)
  (getDocument: CachedServer -> Async<Document * Diagnostic[]>)
  tests
  =
  let doc = server |> getDocument |> Async.Cache
  let init = doc
  let cleanup = Async.map fst >> Async.bind Document.close

  cleanableTestList runner name init false cleanup tests

/// Note: Not intended for changing document: always same (initial) diags
///
/// ## Example
/// ```fsharp
/// let tests state = serverTestList "Simple Server Test" state defaultConfigDto None (fun server -> [
///   let initialText = "let foo = bar"
///   let getDoc = Server.createUntitledDocument initialText
///   documentTestList "Simple Doc Test" server getDoc (fun doc -> [
///     testCaseAsync "doc has correct diagnostics" <| async {
///       let! (doc, diags) = doc
///       // Note: don't `use doc = doc` here (vs. in single case -> see `Example` in `documentTestList`):
///       //       `doc` should stay open and should not be closed/disposed!
///       Expect.exists diags (fun d -> d.Message = "The value or constructor 'bar' is not defined.") "Should have `bar not defined` error"
///     }
///   ])
/// ])
/// ```
let documentTestList = documentTestList' testList
let fdocumentTestList = documentTestList' ftestList
let pdocumentTestList = documentTestList' ptestList
