module TestIdTests

open System
open System.Collections.Generic
open Expecto
open FsAutoComplete.TestServer
open FsAutoComplete.TestingPlatform.Client
open Microsoft.VisualStudio.TestPlatform.ObjectModel

let private project = "/repo/Tests.fsproj"
let private framework = "net8.0"

let private vsTestCase (id: Guid) =
  let testCase =
    TestCase("Tests.Adds", Uri "executor://xunit/VsTestRunner2/netcoreapp", "/repo/bin/Tests.dll")

  testCase.Id <- id
  testCase

let private mtpNode nodeType uid : TestNodeUpdate =
  { Uid = uid
    DisplayName = None
    NodeType = nodeType
    ExecutionState = None
    ParentUid = None
    Location = None
    MethodIdentifier = None
    Duration = None
    Error = None
    StandardOutput = None
    StandardError = None
    Raw = Dictionary<string, obj>() }

/// Values that a naive "|"-separated format would split or decode wrongly.
let private awkwardValues =
  [ "plain"
    "a|b"
    "||"
    "%"
    "%7C"
    "%25"
    "100%|done"
    "with space"
    "Tests+Nested.Test 1"
    "Tests.Adds(a: 1, b: \"x|y\")"
    "Ünïcødé ✓ 测试"
    "/home/a|b/p.fsproj"
    @"C:\x\p.fsproj" ]

let private parsed projectFilePath targetFramework target : Result<ParsedTestId, string> =
  Ok
    { ProjectFilePath = projectFilePath
      TargetFramework = targetFramework
      Target = target }

[<Tests>]
let escapeTests =
  testList
    "TestId.escape"
    [ testCase "a value without separators or escapes is unchanged"
      <| fun _ -> Expect.equal (TestId.escape "Tests.Adds(1)") "Tests.Adds(1)" "the id stays readable"

      testCase "the separator and the escape character are percent-encoded"
      <| fun _ -> Expect.equal (TestId.escape "a|b%c") "a%7Cb%25c" "only '|' and '%' are encoded"

      testCase "unescape reverses escape"
      <| fun _ ->
        for value in awkwardValues do
          Expect.equal (TestId.unescape (TestId.escape value)) (Ok value) $"round-trips {value}"

      testCase "an escape the server never writes is rejected"
      <| fun _ ->
        for value in [ "%41"; "%"; "abc%"; "%7"; "%7c" ] do
          Expect.isError (TestId.unescape value) $"{value} is not a server escape" ]

[<Tests>]
let roundTripTests =
  testList
    "TestId round-trip"
    [ testCase "a VSTest case id parses back to its project, framework and case id"
      <| fun _ ->
        let caseId = Guid.NewGuid()

        for value in awkwardValues do
          let actual = TestId.ofVsTestCase value value (vsTestCase caseId) |> TestId.tryParse

          Expect.equal actual (parsed value value (TestIdTarget.VsTestCase caseId)) $"round-trips {value}"

      testCase "a testing platform action id parses back to its uid"
      <| fun _ ->
        for value in awkwardValues do
          let actual =
            TestId.ofMtpNode value value (mtpNode (Some NodeType.Action) value)
            |> TestId.tryParse

          Expect.equal actual (parsed value value (TestIdTarget.MtpNode value)) $"round-trips {value}"

      testCase "a testing platform node of unreported type is a runnable node"
      <| fun _ ->
        let actual =
          TestId.ofMtpNode project framework (mtpNode None "a3f9") |> TestId.tryParse

        Expect.equal actual (parsed project framework (TestIdTarget.MtpNode "a3f9")) "only a group is not runnable"

      testCase "a testing platform group id is a group id"
      <| fun _ ->
        let actual =
          TestId.ofMtpNode project framework (mtpNode (Some NodeType.Group) "Tests")
          |> TestId.tryParse

        Expect.equal actual (parsed project framework (TestIdTarget.Group "Tests")) "a group holds tests"

      testCase "a group id parses back to its key"
      <| fun _ ->
        for value in awkwardValues do
          let actual = TestId.group value value value |> TestId.tryParse

          Expect.equal actual (parsed value value (TestIdTarget.Group value)) $"round-trips {value}"

      testCase "a VSTest case id does not depend on the display name"
      <| fun _ ->
        let caseId = Guid.NewGuid()
        let before = vsTestCase caseId
        let after = vsTestCase caseId
        after.DisplayName <- "Renamed at run time"

        Expect.equal
          (TestId.ofVsTestCase project framework after)
          (TestId.ofVsTestCase project framework before)
          "the run reproduces the discovery id" ]

[<Tests>]
let distinctnessTests =
  testList
    "TestId distinctness"
    [ testCase "the same key in two projects gives different ids"
      <| fun _ ->
        Expect.notEqual
          (TestId.group "/repo/A.fsproj" framework "Tests")
          (TestId.group "/repo/B.fsproj" framework "Tests")
          "the project scopes the id"

      testCase "the same key in two target frameworks gives different ids"
      <| fun _ ->
        let caseId = Guid.NewGuid()

        Expect.notEqual
          (TestId.ofVsTestCase project "net8.0" (vsTestCase caseId))
          (TestId.ofVsTestCase project "net9.0" (vsTestCase caseId))
          "the target framework scopes the id"

      testCase "a leaf and a group with the same key give different ids"
      <| fun _ ->
        Expect.notEqual
          (TestId.ofMtpNode project framework (mtpNode (Some NodeType.Action) "Tests.A"))
          (TestId.group project framework "Tests.A")
          "leaves and groups do not share a namespace"

      testCase "a separator inside a field cannot shift the fields"
      <| fun _ ->
        Expect.notEqual
          (TestId.group "/repo/a|net8.0" "b" "Tests")
          (TestId.group "/repo/a" "net8.0|b" "Tests")
          "each field is escaped" ]

[<Tests>]
let parseErrorTests =
  let caseId = "3f2504e0-4f89-11d3-9a0c-0305e82c3301"

  let rejects (id: string) because =
    testCase because
    <| fun _ -> Expect.isError (TestId.tryParse id) $"{id} is not a valid id"

  testList
    "TestId.tryParse errors"
    [ rejects "" "an empty id"
      rejects $"vs|/repo/p.fsproj|net8.0|{caseId}" "an id without the version prefix"
      rejects $"t2|vs|/repo/p.fsproj|net8.0|{caseId}" "an id of an unknown version"
      rejects $"T1|vs|/repo/p.fsproj|net8.0|{caseId}" "a prefix in the wrong case"
      rejects "t1|vs|/repo/p.fsproj|net8.0" "an id with four fields"
      rejects $"t1|vs|/repo/p.fsproj|net8.0|{caseId}|extra" "an id with six fields"
      rejects "t1|xx|/repo/p.fsproj|net8.0|Tests" "an unknown kind"
      rejects "t1|vs|/repo/p.fsproj|net8.0|Tests.Adds" "a VSTest key that is not a guid"
      rejects "t1|mtp||net8.0|a3f9" "an empty project"
      rejects "t1|mtp|/repo/p.fsproj|net8.0|" "an empty key"
      rejects "t1|grp|/repo/p%zz.fsproj|net8.0|Tests" "a malformed escape" ]

[<Tests>]
let formatTests =
  let caseId = Guid.Parse "3f2504e0-4f89-11d3-9a0c-0305e82c3301"

  let roundTrips description (id: string) =
    testCase description
    <| fun _ ->
      let reformatted = TestId.tryParse id |> Result.map TestId.format
      Expect.equal reformatted (Ok id) "format writes back the id that was parsed"

  testList
    "TestId.format"
    [ roundTrips "a VSTest id" (TestId.ofVsTestCase project framework (vsTestCase caseId))
      roundTrips "a testing platform id" (TestId.ofMtpNode project framework (mtpNode (Some NodeType.Action) "a|%b"))
      roundTrips "a grouping id" (TestId.group "/repo/a|b.fsproj" framework "Tests+Nested") ]

[<Tests>]
let selectionTests =
  let caseId = Guid.Parse "3f2504e0-4f89-11d3-9a0c-0305e82c3301"
  let vsTestId = TestId.ofVsTestCase project framework (vsTestCase caseId)

  testList
    "TestRunSelection.ofRequest"
    [ testCase "neither a filter nor ids runs everything"
      <| fun _ -> Expect.equal (TestRunSelection.ofRequest None None) (Ok TestRunSelection.All) "all tests"

      testCase "a filter alone selects by filter"
      <| fun _ ->
        Expect.equal
          (TestRunSelection.ofRequest (Some "FullyQualifiedName~A") None)
          (Ok(TestRunSelection.Filter "FullyQualifiedName~A"))
          "a filter run"

      testCase "ids alone select those tests"
      <| fun _ ->
        Expect.equal
          (TestRunSelection.ofRequest None (Some [| vsTestId |]))
          (Ok(
            TestRunSelection.Ids
              [ { ProjectFilePath = project
                  TargetFramework = framework
                  Target = TestIdTarget.VsTestCase caseId } ]
          ))
          "an id run"

      testCase "an empty id list selects nothing rather than everything"
      <| fun _ -> Expect.equal (TestRunSelection.ofRequest None (Some [||])) (Ok(TestRunSelection.Ids [])) "no tests"

      testCase "ids and a filter together are rejected"
      <| fun _ ->
        Expect.isError
          (TestRunSelection.ofRequest (Some "FullyQualifiedName~A") (Some [| vsTestId |]))
          "the two selections are alternatives"

      testCase "a malformed id is rejected"
      <| fun _ -> Expect.isError (TestRunSelection.ofRequest None (Some [| "Tests.Adds" |])) "not an id"

      testCase "a grouping id is rejected"
      <| fun _ ->
        Expect.isError
          (TestRunSelection.ofRequest None (Some [| TestId.group project framework "Tests" |]))
          "a client runs a grouping by its leaves" ]
