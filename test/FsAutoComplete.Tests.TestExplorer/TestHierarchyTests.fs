module TestHierarchyTests

open Expecto
open FsAutoComplete.TestServer

let private xunitExecutor = "executor://xunit/VsTestRunner2/netcoreapp"

let private vsTestCase fullName =
  Microsoft.VisualStudio.TestPlatform.ObjectModel.TestCase(
    fullName,
    System.Uri xunitExecutor,
    "/repo/bin/Tests.dll",
    DisplayName = fullName
  )

let private leafIn projectFilePath fullName = TestItem.ofVsTestCase projectFilePath "net8.0" (vsTestCase fullName)

let private leaf fullName = leafIn "/repo/Tests.fsproj" fullName

let private byFullName (items: TestItem list) = items |> List.map (fun i -> i.FullName) |> List.sort

[<Tests>]
let parameterisedCaseTests =
  let fullNameOf executorUri fullName displayName =
    TestItem.fullNameWithParameterisedCases executorUri fullName displayName

  testList
    "TestItem.fullNameWithParameterisedCases"
    [ testCase "xunit theory cases nest under the method"
      <| fun _ ->
        let actual =
          fullNameOf "executor://xunit/VsTestRunner2/netcoreapp" "Tests.Adds" "Tests.Adds(a: 1, b: 2)"

        Expect.equal actual "Tests.Adds.Adds(a: 1, b: 2)" "the case parameters distinguish the case"

      testCase "an xunit fact keeps its name"
      <| fun _ ->
        let actual =
          fullNameOf "executor://xunit/VsTestRunner2/netcoreapp" "Tests.Adds" "Tests.Adds"

        Expect.equal actual "Tests.Adds" "a fact has one case"

      testCase "mstest data rows nest under the method"
      <| fun _ ->
        let actual = fullNameOf "executor://mstestadapter/v2" "Tests.Adds" "Adds (1,2)"

        Expect.equal actual "Tests.Adds.Adds (1,2)" "the row data distinguishes the case"

      testCase "a framework without parameterised naming keeps the reported name"
      <| fun _ ->
        let actual = fullNameOf "executor://yolodev/expecto" "Tests.My test" "My test"

        Expect.equal actual "Tests.My test" "Expecto reports one name per test"

      testCase "xunit theory cases of one method stay distinct"
      <| fun _ ->
        let executorUri = "executor://xunit/VsTestRunner2/netcoreapp"

        let case parameters =
          let testCase =
            Microsoft.VisualStudio.TestPlatform.ObjectModel.TestCase(
              "Tests.Adds",
              System.Uri executorUri,
              "/repo/bin/Tests.dll"
            )

          testCase.DisplayName <- $"Tests.Adds{parameters}"
          // The adapter gives each case its own id; one derived from the shared name would not.
          testCase.Id <- System.Guid.NewGuid()
          TestItem.ofVsTestCase "/repo/Tests.fsproj" "net8.0" testCase

        let actual =
          TestHierarchy.withInferredGroupings [ case "(a: 1)"; case "(a: 2)" ]
          |> List.filter _.IsLeaf

        Expect.hasLength actual 2 "both theory cases survive" ]

[<Tests>]
let tests =
  testList
    "TestHierarchy"
    [ testCase "a leaf with an unsegmented name has no parent"
      <| fun _ ->
        let actual = TestHierarchy.withInferredGroupings [ leaf "MyTest" ]

        Expect.hasLength actual 1 "the leaf is the whole tree"
        Expect.isNone actual.Head.ParentId "an unsegmented name is rooted"
        Expect.isTrue actual.Head.IsLeaf "the node is runnable"

      testCase "leaves sharing a prefix gain one grouping node"
      <| fun _ ->
        let actual =
          TestHierarchy.withInferredGroupings [ leaf "Tests.First"; leaf "Tests.Second" ]

        Expect.equal (byFullName actual) [ "Tests"; "Tests.First"; "Tests.Second" ] "the shared prefix becomes a node"

      testCase "a grouping node is not runnable"
      <| fun _ ->
        let actual =
          TestHierarchy.withInferredGroupings [ leaf "Tests.First"; leaf "Tests.Second" ]

        let grouping = actual |> List.find (fun i -> i.FullName = "Tests")

        Expect.isFalse grouping.IsLeaf "a grouping node has no test to run"

      testCase "a leaf points at its grouping node"
      <| fun _ ->
        let actual =
          TestHierarchy.withInferredGroupings [ leaf "Tests.First"; leaf "Tests.Second" ]

        let grouping = actual |> List.find (fun i -> i.FullName = "Tests")
        let first = actual |> List.find (fun i -> i.FullName = "Tests.First")

        Expect.equal first.ParentId (Some grouping.Id) "the leaf's parent is the grouping node"

      testCase "nested type separators nest"
      <| fun _ ->
        let actual = TestHierarchy.withInferredGroupings [ leaf "Tests+Nested.Test 1" ]

        Expect.equal
          (byFullName actual)
          [ "Tests"; "Tests+Nested"; "Tests+Nested.Test 1" ]
          "'+' separates a nested type from its parent"

      testCase "identical names in different projects stay distinct"
      <| fun _ ->
        let other = leafIn "/repo/Other.fsproj" "Tests.First"

        let actual = TestHierarchy.withInferredGroupings [ leaf "Tests.First"; other ]

        let ids = actual |> List.map _.Id |> List.distinct

        Expect.hasLength ids 4 "two leaves and two groupings, none shared across projects"

      testCase "cases with one name but different ids stay distinct"
      <| fun _ ->
        let case () =
          let testCase = vsTestCase "Tests.Adds"
          testCase.Id <- System.Guid.NewGuid()
          TestItem.ofVsTestCase "/repo/Tests.fsproj" "net8.0" testCase

        let actual =
          TestHierarchy.withInferredGroupings [ case (); case () ] |> List.filter _.IsLeaf

        Expect.hasLength actual 2 "the adapter's case ids tell the cases apart"

      testCase "a test named like a grouping keeps both the test and the grouping"
      <| fun _ ->
        let actual =
          TestHierarchy.withInferredGroupings [ leaf "Tests.A"; leaf "Tests.A.B" ]

        let named fullName isLeaf =
          actual
          |> List.filter (fun i -> i.FullName = fullName && i.IsLeaf = isLeaf)
          |> List.exactlyOne

        let test = named "Tests.A" true
        let grouping = named "Tests.A" false
        let child = named "Tests.A.B" true

        Expect.notEqual test.Id grouping.Id "a test and a grouping never share an id"
        Expect.equal child.ParentId (Some grouping.Id) "the nested test hangs off the grouping"
        Expect.equal test.ParentId (named "Tests" false |> _.Id |> Some) "the test hangs off its own parent" ]
