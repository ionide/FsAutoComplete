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
val cleanableTestList:
  runner: (string -> Test list -> Test) ->
  name: string ->
  initialize: Async<'a> ->
  cacheValue: bool ->
  cleanup: (Async<'a> -> Async<unit>) ->
  tests: (Async<'a> -> Test list) ->
    Test

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
val serverTestList:
  (string
    -> (unit -> FsAutoComplete.Lsp.IFSharpLspServer * System.IObservable<string * obj>)
    -> FsAutoComplete.LspHelpers.FSharpConfigDto
    -> option<string>
    -> (Async<Server> -> list<Test>)
    -> Test)

val fserverTestList:
  (string
    -> (unit -> FsAutoComplete.Lsp.IFSharpLspServer * System.IObservable<string * obj>)
    -> FsAutoComplete.LspHelpers.FSharpConfigDto
    -> option<string>
    -> (Async<Server> -> list<Test>)
    -> Test)

val pserverTestList:
  (string
    -> (unit -> FsAutoComplete.Lsp.IFSharpLspServer * System.IObservable<string * obj>)
    -> FsAutoComplete.LspHelpers.FSharpConfigDto
    -> option<string>
    -> (Async<Server> -> list<Test>)
    -> Test)


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
val documentTestList:
  (string
    -> CachedServer
    -> (CachedServer -> Async<Document * Diagnostic[]>)
    -> (Async<Document * Diagnostic[]> -> list<Test>)
    -> Test)

val fdocumentTestList:
  (string
    -> CachedServer
    -> (CachedServer -> Async<Document * Diagnostic[]>)
    -> (Async<Document * Diagnostic[]> -> list<Test>)
    -> Test)

val pdocumentTestList:
  (string
    -> CachedServer
    -> (CachedServer -> Async<Document * Diagnostic[]>)
    -> (Async<Document * Diagnostic[]> -> list<Test>)
    -> Test)
