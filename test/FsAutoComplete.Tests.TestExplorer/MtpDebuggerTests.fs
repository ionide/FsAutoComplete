module MtpDebuggerTests

open Expecto
open System.Collections.Generic
open System.Threading
open FsAutoComplete.TestServer

let private request name (parameters: (string * obj) list) =
  let handler = MtpWrapper.attachDebuggerHandler

  fun onAttach ->
    handler
      onAttach
      name
      (Some(Dictionary<string, obj>(dict parameters) :> IReadOnlyDictionary<string, obj>))
      CancellationToken.None
    |> Async.AwaitTask
    |> Async.RunSynchronously

let private attachRequest processId = request "client/attachDebugger" [ "processId", box processId ]

let private answer (result: IReadOnlyDictionary<string, obj> option) =
  result
  |> Option.bind (fun r ->
    match r.TryGetValue "success" with
    | true, attached -> Some attached
    | _ -> None)

[<Tests>]
let tests =
  testList
    "MtpWrapper.attachDebuggerHandler"
    [ testCase "the process the server names is the one debugged"
      <| fun _ ->
        let mutable debugged = None

        attachRequest 4242 (fun processId ->
          debugged <- Some processId
          true)
        |> ignore

        Expect.equal debugged (Some 4242) "the debugger attaches to the process the server named"

      testCase "an attached debugger is reported to the server"
      <| fun _ ->
        let actual = attachRequest 4242 (fun _ -> true)

        Expect.equal (answer actual) (Some(box true)) "the server is told the debugger attached"

      testCase "a debugger that did not attach is reported to the server"
      <| fun _ ->
        let actual = attachRequest 4242 (fun _ -> false)

        Expect.equal (answer actual) (Some(box false)) "the server is told to carry on undebugged"

      testCase "a request naming no process attaches nothing"
      <| fun _ ->
        let mutable attempted = false

        let actual =
          request "client/attachDebugger" [] (fun _ ->
            attempted <- true
            true)

        Expect.isFalse attempted "there is no process to attach to"
        Expect.equal (answer actual) (Some(box false)) "the server is told no debugger attached"

      testCase "another request is left unanswered"
      <| fun _ ->
        let mutable attempted = false

        let actual =
          request "telemetry/update" [ "processId", box 4242 ] (fun _ ->
            attempted <- true
            true)

        Expect.isFalse attempted "only an attach request attaches a debugger"
        Expect.isNone actual "this handler answers nothing else" ]
