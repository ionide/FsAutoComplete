module MtpMappingTests

open Expecto
open System.Collections.Generic
open FsAutoComplete.TestServer
open FsAutoComplete.TestingPlatform.Client

let private project = "/repo/Tests.fsproj"
let private framework = "net8.0"

let private node uid : TestNodeUpdate =
  {
    Uid = uid
    DisplayName = None
    NodeType = Some NodeType.Action
    ExecutionState = None
    ParentUid = None
    Location = None
    MethodIdentifier = None
    Duration = None
    Error = None
    StandardOutput = None
    StandardError = None
    Raw = Dictionary<string, obj>()
  }

let private map (n: TestNodeUpdate) = TestItem.ofMtpNode project framework n

/// A test reported with the method it runs, as MSTest, NUnit and TUnit report their short names.
let private identified ns typeName displayName =
  { node "a3f9" with
      DisplayName = Some displayName
      MethodIdentifier =
        Some
          {
            Namespace = ns
            TypeName = typeName
            MethodName = Some "Test1"
          }
  }

[<Tests>]
let mtpNodeTests =
  testList
    "TestItem.ofMtpNode"
    [
      testCase "the uid identifies the node within its project and framework"
      <| fun _ ->
        let actual = map (node "a3f9")

        Expect.equal
          (TestId.tryParse actual.Id)
          (Ok
            {
              ProjectFilePath = project
              TargetFramework = framework
              Target = TestIdTarget.MtpNode "a3f9"
            })
          "the id routes the uid to its project and framework"

      testCase "the display name is the full name"
      <| fun _ ->
        let actual =
          map
            { node "a3f9" with
                DisplayName = Some "Tests.Adds(a: 1)"
            }

        Expect.equal actual.FullName "Tests.Adds(a: 1)" "the name the server displays names the test"

      testCase "the full name falls back to the uid"
      <| fun _ ->
        let actual = map (node "a3f9")

        Expect.equal actual.FullName "a3f9" "the uid names the node when the server sends no display name"

      testCase "a short display name is qualified by the test's type"
      <| fun _ ->
        let actual = map (identified (Some "MyNamespace") "ClassA" "Test1")

        Expect.equal actual.FullName "MyNamespace.ClassA.Test1" "the namespace and type qualify the method name"

      testCase "a data row is qualified by the test's type"
      <| fun _ ->
        let actual = map (identified (Some "MyNamespace") "ClassA" "Test1 (1,2)")

        Expect.equal actual.FullName "MyNamespace.ClassA.Test1 (1,2)" "the row's data stays after the method name"

      testCase "a type in no namespace qualifies a short display name alone"
      <| fun _ ->
        let actual = map (identified None "Tests" "Adds")

        Expect.equal actual.FullName "Tests.Adds" "the type alone qualifies the method name"

      testCase "a display name already qualified by its type is the full name"
      <| fun _ ->
        let actual = map (identified None "Tests" "Tests.My theory(n: 2)")

        Expect.equal actual.FullName "Tests.My theory(n: 2)" "the name is not qualified twice"

      testCase "a display name already qualified by its namespace and type is the full name"
      <| fun _ ->
        let actual = map (identified (Some "MyNamespace") "Tests" "MyNamespace.Tests.Adds")

        Expect.equal actual.FullName "MyNamespace.Tests.Adds" "the name is not qualified twice"

      testCase "a short display name is still the label"
      <| fun _ ->
        let actual = map (identified (Some "MyNamespace") "ClassA" "Test1")

        Expect.equal actual.DisplayName "Test1" "the server names the node"

      testCase "tests of one name in two types have different full names"
      <| fun _ ->
        let names =
          [
            map (identified (Some "MyNamespace") "ClassA" "Test1")
            map (identified (Some "MyNamespace") "ClassB" "Test1")
          ]
          |> List.map _.FullName

        Expect.equal (List.distinct names |> List.length) 2 "each type's test has its own name"

      testCase "a parent uid is scoped the same way as the node's own id"
      <| fun _ ->
        let actual =
          map
            { node "a3f9" with
                ParentUid = Some "b1c2"
            }

        let parent =
          map
            { node "b1c2" with
                NodeType = Some NodeType.Group
            }

        Expect.equal actual.ParentId (Some parent.Id) "the parent id matches the parent's id"

      testCase "a node without a parent sits at the root"
      <| fun _ ->
        let actual = map (node "Tests")

        Expect.isNone actual.ParentId "a root node has no parent"

      testCase "an empty parent uid is no parent"
      <| fun _ ->
        let actual = map { node "a3f9" with ParentUid = Some "" }

        Expect.isNone actual.ParentId "an empty uid names no node"

      testCase "an action node is runnable"
      <| fun _ ->
        let actual =
          map
            { node "Tests.Adds" with
                NodeType = Some NodeType.Action
            }

        Expect.isTrue actual.IsLeaf "an action is a test"

      testCase "a group node is not runnable"
      <| fun _ ->
        let actual =
          map
            { node "Tests" with
                NodeType = Some NodeType.Group
            }

        Expect.isFalse actual.IsLeaf "a group holds tests"

      testCase "a node of unreported type is runnable"
      <| fun _ ->
        let actual =
          map
            { node "Tests.Adds" with
                NodeType = None
            }

        Expect.isTrue actual.IsLeaf "only a group is excluded from running"

      testCase "the display name falls back to the uid"
      <| fun _ ->
        let actual =
          map
            { node "Tests.Adds" with
                DisplayName = None
            }

        Expect.equal actual.DisplayName "Tests.Adds" "the uid names the node when the server sends no display name"

      testCase "a reported display name is kept"
      <| fun _ ->
        let actual =
          map
            { node "Tests.Adds" with
                DisplayName = Some "Adds two numbers"
            }

        Expect.equal actual.DisplayName "Adds two numbers" "the server names the node"

      testCase "a location becomes a code file and range"
      <| fun _ ->
        let actual =
          map
            { node "Tests.Adds" with
                Location =
                  Some
                    {
                      File = "/repo/Tests.fs"
                      LineStart = Some 12
                      LineEnd = Some 20
                    }
            }

        Expect.equal actual.CodeFilePath (Some "/repo/Tests.fs") "the file is carried over"

        Expect.equal
          actual.CodeLocationRange
          (Some { StartLine = 12; EndLine = 20 })
          "the reported lines bound the test"

      testCase "a location without an end line covers one line"
      <| fun _ ->
        let actual =
          map
            { node "Tests.Adds" with
                Location =
                  Some
                    {
                      File = "/repo/Tests.fs"
                      LineStart = Some 12
                      LineEnd = None
                    }
            }

        Expect.equal actual.CodeLocationRange (Some { StartLine = 12; EndLine = 12 }) "the start line bounds both ends"

      testCase "a location without lines gives a file and no range"
      <| fun _ ->
        let actual =
          map
            { node "Tests.Adds" with
                Location =
                  Some
                    {
                      File = "/repo/Tests.fs"
                      LineStart = None
                      LineEnd = None
                    }
            }

        Expect.equal actual.CodeFilePath (Some "/repo/Tests.fs") "the file is still known"
        Expect.isNone actual.CodeLocationRange "no line was reported"

      testCase "a node without a location has neither file nor range"
      <| fun _ ->
        let actual = map (node "Tests.Adds")

        Expect.isNone actual.CodeFilePath "no file was reported"
        Expect.isNone actual.CodeLocationRange "no range was reported"

      testCase "the node carries the project it was discovered in"
      <| fun _ ->
        let actual = map (node "Tests.Adds")

        Expect.equal actual.ProjectFilePath project "the project is carried over"
        Expect.equal actual.TargetFramework framework "the framework is carried over"

      testCase "the executor uri names no VSTest adapter"
      <| fun _ ->
        let actual = map (node "Tests.Adds")

        Expect.isNone
          (TestFrameworkId.tryOfExecutorUri actual.ExecutorUri)
          "name breakdown by adapter does not apply to a platform that reports its own tree"
    ]

[<Tests>]
let mtpNodesTests =
  let parentOf (parent: TestNodeUpdate) =
    let child =
      { node "a3f9" with
          ParentUid = Some parent.Uid
      }

    match TestHierarchy.ofMtpNodes project framework [ parent; child ] with
    | [ parent; child ] -> parent, child
    | items -> failwith $"Expected two items, got {List.length items}"

  testList
    "TestHierarchy.ofMtpNodes"
    [ // The node type is optional on the wire, and nothing stops a test from having children.
      for description, nodeType in
        [
          "a grouping", Some NodeType.Group
          "a test", Some NodeType.Action
          "a node of no reported type", None
        ] do
        testCase $"a child links to {description} parent by the parent's own id"
        <| fun _ ->
          let parent, child = parentOf { node "b1c2" with NodeType = nodeType }

          Expect.equal child.ParentId (Some parent.Id) "the parent id is the id the parent was given"

      testCase "a parent reported elsewhere is taken to be a grouping"
      <| fun _ ->
        let child =
          TestHierarchy.ofMtpNodes
            project
            framework
            [
              { node "a3f9" with
                  ParentUid = Some "b1c2"
              }
            ]
          |> List.exactlyOne

        Expect.equal
          child.ParentId
          (Some(TestId.group project framework "b1c2"))
          "only a grouping has children, unless the parent is seen to be otherwise"
    ]

[<Tests>]
let idScopeTests =
  let vsTestCase fullName =
    Microsoft.VisualStudio.TestPlatform.ObjectModel.TestCase(
      fullName,
      System.Uri "executor://xunit/VsTestRunner2/netcoreapp",
      "/repo/bin/Tests.dll",
      DisplayName = fullName
    )

  let distinctIds (ids: string list) = ids |> List.distinct |> List.length

  testList
    "TestItem ids"
    [
      testCase "one platform uid in two projects or frameworks names different tests"
      <| fun _ ->
        let ids =
          [
            TestItem.ofMtpNode project framework (node "a3f9")
            TestItem.ofMtpNode "/repo/Other.fsproj" framework (node "a3f9")
            TestItem.ofMtpNode project "net9.0" (node "a3f9")
          ]
          |> List.map _.Id

        Expect.equal (distinctIds ids) 3 "each project and framework has its own id"

      testCase "one VSTest name in two projects or frameworks names different tests"
      <| fun _ ->
        let ids =
          [
            TestItem.ofVsTestCase project framework (vsTestCase "Tests.My test")
            TestItem.ofVsTestCase "/repo/Other.fsproj" framework (vsTestCase "Tests.My test")
            TestItem.ofVsTestCase project "net9.0" (vsTestCase "Tests.My test")
          ]
          |> List.map _.Id

        Expect.equal (distinctIds ids) 3 "each project and framework has its own id"

      testCase "a group and a test with one uid have different ids"
      <| fun _ ->
        let ids =
          [
            map (node "Tests")
            map
              { node "Tests" with
                  NodeType = Some NodeType.Group
              }
          ]
          |> List.map _.Id

        Expect.equal (distinctIds ids) 2 "a group cannot be mistaken for a runnable test"

      testCase "a VSTest id routes the case to its project and framework"
      <| fun _ ->
        let testCase = vsTestCase "Tests.My test"
        let actual = TestItem.ofVsTestCase project framework testCase

        Expect.equal
          (TestId.tryParse actual.Id)
          (Ok
            {
              ProjectFilePath = project
              TargetFramework = framework
              Target = TestIdTarget.VsTestCase testCase.Id
            })
          "the id carries the adapter's case id"

      testCase "a VSTest result keeps the discovery id when its display name changes"
      <| fun _ ->
        let discovered = vsTestCase "Tests.Adds"
        let discoveryId = (TestItem.ofVsTestCase project framework discovered).Id

        let ran = vsTestCase "Tests.Adds"
        ran.Id <- discovered.Id
        ran.DisplayName <- "Tests.Adds(a: 1)"

        let result =
          TestResult.ofVsTestResult project framework (Microsoft.VisualStudio.TestPlatform.ObjectModel.TestResult ran)

        Expect.equal result.TestItem.Id discoveryId "the result lands on the test discovery reported"
    ]

[<Tests>]
let mtpHierarchyTests =
  let test uid ns typeName displayName =
    { node uid with
        DisplayName = Some displayName
        MethodIdentifier =
          Some
            {
              Namespace = ns
              TypeName = typeName
              MethodName = Some "Test1"
            }
    }

  let tree nodes = TestHierarchy.ofMtpNodes project framework nodes

  let parentNameOf (items: TestItem list) (item: TestItem) =
    items
    |> List.tryFind (fun p -> Some p.Id = item.ParentId)
    |> Option.map _.FullName

  let groupNames (items: TestItem list) = items |> List.filter (_.IsLeaf >> not) |> List.map _.FullName |> List.sort

  testList
    "TestHierarchy.ofMtpNodes"
    [
      testCase "a node hangs off the parent the platform reported"
      <| fun _ ->
        let actual =
          tree
            [
              { node "b1c2" with
                  NodeType = Some NodeType.Group
                  DisplayName = Some "Any.Name+At all"
              }
              { test "a3f9" None "Tests" "Adds" with
                  ParentUid = Some "b1c2"
              }
            ]

        let child = actual |> List.find _.IsLeaf

        Expect.equal (parentNameOf actual child) (Some "Any.Name+At all") "the reported parent is the parent"
        Expect.equal (List.length actual) 2 "a tree the platform linked itself gains no groupings"

      testCase "a test without a parent hangs off the type the platform names for it"
      <| fun _ ->
        let actual =
          tree
            [
              test "a3f9" (Some "MyNamespace") "ClassA" "Test1"
              test "b1c2" (Some "MyNamespace") "ClassA" "Test1 (1,2)"
              test "c7d8" (Some "MyNamespace") "ClassB" "Test1"
            ]

        Expect.equal
          (actual
           |> List.filter _.IsLeaf
           |> List.map (fun t -> t.DisplayName, parentNameOf actual t))
          [
            "Test1", Some "MyNamespace.ClassA"
            "Test1 (1,2)", Some "MyNamespace.ClassA"
            "Test1", Some "MyNamespace.ClassB"
          ]
          "each test hangs off its type"

        Expect.equal
          (groupNames actual)
          [ "MyNamespace"; "MyNamespace.ClassA"; "MyNamespace.ClassB" ]
          "the namespace and each type become groups"

      testCase "a test name is never split into groups"
      <| fun _ ->
        let actual =
          tree
            [
              test "a3f9" None "XUnitTests" "XUnitTests.Contains.separator+characters"
              test "b1c2" None "XUnitTests" "theory(x: 1.5)"
            ]

        Expect.equal (groupNames actual) [ "XUnitTests" ] "only the type is a group"

        Expect.equal
          (actual |> List.filter _.IsLeaf |> List.map (parentNameOf actual))
          [ Some "XUnitTests"; Some "XUnitTests" ]
          "the separators in a test's name do not nest it"

      testCase "a nested type is grouped under the type enclosing it"
      <| fun _ ->
        let actual = tree [ test "a3f9" None "Tests+Nested+Deeper" "Adds" ]
        let byName name = actual |> List.find (fun item -> item.FullName = name)

        Expect.equal
          (groupNames actual)
          [ "Tests"; "Tests+Nested"; "Tests+Nested+Deeper" ]
          "each enclosing type is a group"

        Expect.equal
          (parentNameOf actual (byName "Tests+Nested+Deeper"))
          (Some "Tests+Nested")
          "a type hangs off its encloser"

        Expect.equal (byName "Tests").ParentId None "the outermost type sits at the root"
        Expect.equal (byName "Tests+Nested").DisplayName "Nested" "a group is labelled by its own segment"

      testCase "a type group addresses no test"
      <| fun _ ->
        let group = tree [ test "a3f9" None "Tests" "Adds" ] |> List.find (_.IsLeaf >> not)

        Expect.equal
          (TestId.tryParse group.Id |> Result.map _.Target)
          (Ok(TestIdTarget.Group "Tests"))
          "a group is not a test a client can run"

      testCase "a test the platform neither parents nor types sits at the root"
      <| fun _ ->
        let actual =
          tree
            [
              { node "a3f9" with
                  DisplayName = Some "list.sublist.case"
              }
            ]

        Expect.equal (actual |> List.map _.ParentId) [ None ] "nothing is inferred from the name"

      testCase "a test reported while running hangs off the same type as when discovered"
      <| fun _ ->
        let reported = test "a3f9" (Some "MyNamespace") "ClassA" "Test1"
        let discovered = tree [ reported ] |> List.find _.IsLeaf

        Expect.equal (TestItem.ofMtpNode project framework reported) discovered "a run update matches discovery"
    ]

[<Tests>]
let mtpOutcomeTests =
  testList
    "TestOutcome.ofMtpExecutionState"
    [
      testCase "a passed test passed"
      <| fun _ ->
        Expect.equal (TestOutcome.ofMtpExecutionState (Some ExecutionState.Passed)) TestOutcome.Passed "the test passed"

      testCase "a failed test failed"
      <| fun _ ->
        Expect.equal (TestOutcome.ofMtpExecutionState (Some ExecutionState.Failed)) TestOutcome.Failed "the test failed"

      testCase "a skipped test was skipped"
      <| fun _ ->
        Expect.equal
          (TestOutcome.ofMtpExecutionState (Some ExecutionState.Skipped))
          TestOutcome.Skipped
          "the test was skipped"

      testCase "a test that errored failed"
      <| fun _ ->
        Expect.equal
          (TestOutcome.ofMtpExecutionState (Some ExecutionState.Error))
          TestOutcome.Failed
          "an error stops the test from passing"

      testCase "a test that timed out failed"
      <| fun _ ->
        Expect.equal
          (TestOutcome.ofMtpExecutionState (Some ExecutionState.TimedOut))
          TestOutcome.Failed
          "a test that never finished did not pass"

      testCase "a cancelled test has no outcome"
      <| fun _ ->
        Expect.equal
          (TestOutcome.ofMtpExecutionState (Some ExecutionState.Canceled))
          TestOutcome.None
          "a cancelled test was never judged"

      testCase "a test still running has no outcome"
      <| fun _ ->
        Expect.equal
          (TestOutcome.ofMtpExecutionState (Some ExecutionState.InProgress))
          TestOutcome.None
          "a running test has not finished"

      testCase "a discovered test has no outcome"
      <| fun _ ->
        Expect.equal
          (TestOutcome.ofMtpExecutionState (Some ExecutionState.Discovered))
          TestOutcome.None
          "a discovered test has not run"

      testCase "an unreported state has no outcome"
      <| fun _ -> Expect.equal (TestOutcome.ofMtpExecutionState None) TestOutcome.None "the server judged nothing"

      testCase "a state this client does not know has no outcome"
      <| fun _ ->
        Expect.equal
          (TestOutcome.ofMtpExecutionState (Some(ExecutionState.Other "quarantined")))
          TestOutcome.None
          "an unknown state is not read as a verdict"
    ]

[<Tests>]
let mtpResultTests =
  let result (n: TestNodeUpdate) = TestResult.ofMtpNode project framework n

  testList
    "TestResult.ofMtpNode"
    [
      testCase "the result names the test it belongs to"
      <| fun _ ->
        let passed =
          { node "a3f9" with
              DisplayName = Some "Tests.Adds"
              ExecutionState = Some ExecutionState.Passed
          }

        let actual = result passed

        Expect.equal
          actual.TestItem
          (TestItem.ofMtpNode project framework passed)
          "the result carries the node it reports on"

      testCase "the execution state becomes the outcome"
      <| fun _ ->
        let actual =
          result
            { node "a3f9" with
                ExecutionState = Some ExecutionState.Failed
            }

        Expect.equal actual.Outcome TestOutcome.Failed "the server's verdict is the outcome"

      testCase "a reported error becomes a message and stack trace"
      <| fun _ ->
        let actual =
          result
            { node "a3f9" with
                ExecutionState = Some ExecutionState.Failed
                Error =
                  Some
                    {
                      Message = Some "Assert.True() failure"
                      StackTrace = Some "at Tests.Adds()"
                    }
            }

        Expect.equal actual.ErrorMessage (Some "Assert.True() failure") "the failure is explained"
        Expect.equal actual.ErrorStackTrace (Some "at Tests.Adds()") "the failure is located"

      testCase "a passing test reports no error"
      <| fun _ ->
        let actual =
          result
            { node "a3f9" with
                ExecutionState = Some ExecutionState.Passed
            }

        Expect.isNone actual.ErrorMessage "nothing failed"
        Expect.isNone actual.ErrorStackTrace "nothing failed"

      testCase "the duration is carried over"
      <| fun _ ->
        let actual =
          result
            { node "a3f9" with
                ExecutionState = Some ExecutionState.Passed
                Duration = Some(System.TimeSpan.FromMilliseconds 250.0)
            }

        Expect.equal
          actual.Duration
          (System.TimeSpan.FromMilliseconds 250.0)
          "the test took the time the server measured"

      testCase "an unreported duration is no time at all"
      <| fun _ ->
        let actual =
          result
            { node "a3f9" with
                ExecutionState = Some ExecutionState.Passed
            }

        Expect.equal actual.Duration System.TimeSpan.Zero "no time was reported"

      testCase "standard output is the test's additional output"
      <| fun _ ->
        let actual =
          result
            { node "a3f9" with
                ExecutionState = Some ExecutionState.Passed
                StandardOutput = Some "hello"
            }

        Expect.equal actual.AdditionalOutput (Some "hello") "what the test printed is kept"

      testCase "standard error joins standard output"
      <| fun _ ->
        let actual =
          result
            { node "a3f9" with
                ExecutionState = Some ExecutionState.Failed
                StandardOutput = Some "hello"
                StandardError = Some "boom"
            }

        Expect.equal
          actual.AdditionalOutput
          (Some(sprintf "hello%sboom" System.Environment.NewLine))
          "both streams are shown to the reader"

      testCase "a test that printed nothing has no additional output"
      <| fun _ ->
        let actual =
          result
            { node "a3f9" with
                ExecutionState = Some ExecutionState.Passed
            }

        Expect.isNone actual.AdditionalOutput "the test printed nothing"
    ]

[<Tests>]
let mtpWireTests =
  let update (properties: (string * obj) list) =
    let wire = Dictionary<string, obj>()
    wire["uid"] <- "a3f9"

    for key, value in properties do
      wire[key] <- value

    Interop.toUpdate (fun _ _ -> ()) None wire |> Option.get

  testList
    "Interop.toUpdate"
    [
      testCase "the method identifier is read off the node"
      <| fun _ ->
        let actual =
          update
            [
              "location.namespace", box "MyNamespace"
              "location.type", box "ClassA"
              "location.method", box "Test1(System.Int32)"
              "location.method-arity", box 0
            ]

        Expect.equal
          actual.MethodIdentifier
          (Some
            {
              Namespace = Some "MyNamespace"
              TypeName = "ClassA"
              MethodName = Some "Test1(System.Int32)"
            })
          "the namespace, type and method are carried over"

      testCase "a type reported with its namespace has no namespace of its own"
      <| fun _ ->
        let actual = update [ "location.type", box "MyNamespace.ClassA" ]

        Expect.equal
          actual.MethodIdentifier
          (Some
            {
              Namespace = None
              TypeName = "MyNamespace.ClassA"
              MethodName = None
            })
          "the platform folds the namespace into the type from 2.0"

      testCase "a node without a type has no method identifier"
      <| fun _ ->
        let actual = update [ "location.method", box "Test1" ]

        Expect.isNone actual.MethodIdentifier "a method means nothing without its type"
    ]
