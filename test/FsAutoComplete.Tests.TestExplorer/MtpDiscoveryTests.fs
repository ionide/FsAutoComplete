module MtpDiscoveryTests

open Expecto
open System.IO
open FsAutoComplete.TestServer

let private sampleApp =
  Path.Combine(ResourceLocators.sampleProjectsRootDir, "Mtp.XUnit/bin/Debug/net8.0/Mtp.XUnit.dll")

let private namesOf (discovered: MtpWrapper.DiscoveredNode list) =
  discovered
  |> List.map (fun (_, node) -> node.DisplayName |> Option.defaultValue node.Uid)

/// Collects the messages logged at error level.
let private errorsInto (errors: ResizeArray<string>) =
  function
  | MtpWrapper.TestDiscoveryUpdate.LogMessage(FsAutoComplete.TestingPlatform.Client.ClientLogLevel.Error, message) ->
    errors.Add message
  | _ -> ()

[<Tests>]
let tests =
  testList
    "MtpWrapper Test Discovery"
    [ testCaseAsync "discovers nothing from no applications"
      <| async {
        let! actual = MtpWrapper.discoverTestsAsync ignore []
        Expect.isEmpty actual "no application reports no tests"
      }

      testCaseAsync "discovers the tests of an xunit application"
      <| async {
        let! discovered = MtpWrapper.discoverTestsAsync ignore [ sampleApp ]

        let names =
          discovered
          |> List.map (fun (_, node) -> node.DisplayName |> Option.defaultValue node.Uid)
          |> List.sort

        Expect.contains names "Tests.My test" "the fact is discovered"
        Expect.contains names "Tests.My theory(n: 1)" "the first theory case is discovered"
        Expect.contains names "Tests.My theory(n: 2)" "the second theory case is discovered"
      }

      testCaseAsync "pairs every node with the application it came from"
      <| async {
        let! discovered = MtpWrapper.discoverTestsAsync ignore [ sampleApp ]

        Expect.all discovered (fun (source, _) -> source = sampleApp) "each node names its application"
      }

      testCaseAsync "reports progress as the server discovers"
      <| async {
        let seen = ResizeArray()

        let! discovered =
          MtpWrapper.discoverTestsAsync
            (function
            | MtpWrapper.TestDiscoveryUpdate.Progress nodes -> seen.AddRange nodes
            | MtpWrapper.TestDiscoveryUpdate.LogMessage _ -> ())
            [ sampleApp ]

        Expect.equal (List.ofSeq seen) discovered "progress reports the same nodes as the result"
      }

      testCaseAsync "an application that does not exist is reported and the others are still discovered"
      <| async {
        let errors = ResizeArray()

        let! discovered = MtpWrapper.discoverTestsAsync (errorsInto errors) [ ResourceLocators.missingApp; sampleApp ]

        Expect.contains (namesOf discovered) "Tests.My test" "the built application is still discovered"
        Expect.all discovered (fun (source, _) -> source = sampleApp) "the missing application reports no tests"

        Expect.exists
          errors
          (fun message -> message.Contains ResourceLocators.missingApp)
          $"an error names the missing application; errors: {List.ofSeq errors}"
      }

      testCaseAsync "an application that fails to start is reported and the others are still discovered"
      <| ResourceLocators.withBrokenApp (fun brokenApp ->
        async {
          let errors = ResizeArray()

          let! discovered = MtpWrapper.discoverTestsAsync (errorsInto errors) [ brokenApp; sampleApp ]

          Expect.contains (namesOf discovered) "Tests.My test" "the working application is still discovered"
          Expect.all discovered (fun (source, _) -> source = sampleApp) "the broken application reports no tests"

          Expect.exists
            errors
            (fun message -> message.Contains brokenApp)
            $"an error names the broken application; errors: {List.ofSeq errors}"
        }) ]
