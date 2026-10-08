namespace FsAutoComplete.TestServer

open System

type TestFileRange = { StartLine: int; EndLine: int }

type TestItem =
  {
    /// Distinguishes this node from every other node reported by the server. Issued by the server
    /// and opaque to clients; see `TestId`.
    Id: string
    /// The `Id` of the node one level up, or `None` at the root of a project.
    ParentId: string option
    /// A runnable test. `false` marks a grouping node.
    IsLeaf: bool
    FullName: string
    DisplayName: string
    /// Identifies the test adapter that ran the tests
    /// Example: executor://xunit/VsTestRunner2/netcoreapp
    /// Used for determining the test library, which effects how tests names are broken down
    ExecutorUri: string
    ProjectFilePath: string
    TargetFramework: string
    CodeFilePath: string option
    CodeLocationRange: TestFileRange option
  }

[<RequireQualifiedAccess>]
type TestFrameworkId =
  | NUnit
  | MsTest
  | XUnit
  | Expecto

module TestFrameworkId =
  let tryOfExecutorUri (executorUri: string) =
    let startsWith (prefix: string) = executorUri.StartsWith(prefix, StringComparison.Ordinal)

    if startsWith "executor://nunit" then
      Some TestFrameworkId.NUnit
    elif startsWith "executor://mstest" then
      Some TestFrameworkId.MsTest
    elif startsWith "executor://xunit" then
      Some TestFrameworkId.XUnit
    elif startsWith "executor://yolodev" then
      Some TestFrameworkId.Expecto
    else
      None

/// What a test id addresses within its project and target framework.
[<RequireQualifiedAccess>]
type TestIdTarget =
  /// A VSTest case, keyed by the id its adapter assigned. Parameterised cases share a
  /// fully-qualified name but not this id.
  | VsTestCase of Guid
  /// A runnable Microsoft.Testing.Platform node, keyed by its uid.
  | MtpNode of uid: string
  /// A grouping node, which holds tests but cannot be run itself.
  | Group of key: string

type ParsedTestId =
  { ProjectFilePath: string
    TargetFramework: string
    Target: TestIdTarget }

/// Test ids are issued by the server and opaque to clients. An id names its own project,
/// target framework and platform so a run can be routed without a lookup, and is derived from
/// what the platform reports rather than from display names, so a run reproduces the id that
/// discovery issued.
///
/// Format: `t1|kind|project|framework|key`, each field escaped so that splitting on `|` always
/// yields five fields.
module TestId =
  [<Literal>]
  let private version = "t1"

  [<Literal>]
  let private vsTestKind = "vs"

  [<Literal>]
  let private mtpKind = "mtp"

  [<Literal>]
  let private groupKind = "grp"

  /// Percent-encodes the field separator and the escape character, and nothing else, so an id
  /// stays readable in logs.
  let escape (value: string) = value.Replace("%", "%25").Replace("|", "%7C")

  /// Reverses `escape`, rejecting any escape it does not produce.
  let unescape (value: string) : Result<string, string> =
    let decoded = Text.StringBuilder(value.Length)

    let rec go index =
      if index >= value.Length then
        Ok(decoded.ToString())
      elif value[index] <> '%' then
        decoded.Append(value[index]) |> ignore
        go (index + 1)
      elif String.CompareOrdinal(value, index, "%25", 0, 3) = 0 then
        decoded.Append('%') |> ignore
        go (index + 3)
      elif String.CompareOrdinal(value, index, "%7C", 0, 3) = 0 then
        decoded.Append('|') |> ignore
        go (index + 3)
      else
        Error $"Malformed escape at position {index}"

    go 0

  let private create kind (projFilePath: string) (targetFramework: string) (key: string) =
    String.Join("|", [| version; kind; escape projFilePath; escape targetFramework; escape key |])

  let ofVsTestCase
    (projFilePath: string)
    (targetFramework: string)
    (testCase: Microsoft.VisualStudio.TestPlatform.ObjectModel.TestCase)
    =
    create vsTestKind projFilePath targetFramework (testCase.Id.ToString("D"))

  /// The id of a grouping node, keyed by its name for VSTest or its uid for the testing platform.
  let group (projFilePath: string) (targetFramework: string) (key: string) =
    create groupKind projFilePath targetFramework key

  let ofMtpNode
    (projFilePath: string)
    (targetFramework: string)
    (node: FsAutoComplete.TestingPlatform.Client.TestNodeUpdate)
    =
    if node.NodeType = Some FsAutoComplete.TestingPlatform.Client.NodeType.Group then
      group projFilePath targetFramework node.Uid
    else
      create mtpKind projFilePath targetFramework node.Uid

  let tryParse (id: string) : Result<ParsedTestId, string> =
    let nonEmpty name (value: Result<string, string>) =
      value
      |> Result.bind (fun v ->
        if String.IsNullOrEmpty v then
          Error $"The {name} is empty"
        else
          Ok v)

    match id.Split('|') with
    | [| v; kind; projFilePath; targetFramework; key |] when v = version ->
      let target =
        match kind, unescape key |> nonEmpty "key" with
        | _, Error e -> Error e
        | k, Ok key when k = vsTestKind ->
          match Guid.TryParseExact(key, "D") with
          | true, caseId -> Ok(TestIdTarget.VsTestCase caseId)
          | false, _ -> Error $"The test case id '{key}' is not a guid"
        | k, Ok key when k = mtpKind -> Ok(TestIdTarget.MtpNode key)
        | k, Ok key when k = groupKind -> Ok(TestIdTarget.Group key)
        | k, Ok _ -> Error $"Unknown test id kind '{k}'"

      match unescape projFilePath |> nonEmpty "project", unescape targetFramework, target with
      | Ok projFilePath, Ok targetFramework, Ok target ->
        Ok
          { ProjectFilePath = projFilePath
            TargetFramework = targetFramework
            Target = target }
      | Error e, _, _
      | _, Error e, _
      | _, _, Error e -> Error $"Malformed test id '{id}': {e}"
    | [| v; _; _; _; _ |] -> Error $"Unsupported test id version '{v}' in '{id}'"
    | _ -> Error $"Malformed test id '{id}': expected five '|'-separated fields"

  /// Writes a parsed id back out as the id it was parsed from, so a message can name it.
  let format (id: ParsedTestId) =
    match id.Target with
    | TestIdTarget.VsTestCase caseId -> create vsTestKind id.ProjectFilePath id.TargetFramework (caseId.ToString("D"))
    | TestIdTarget.MtpNode uid -> create mtpKind id.ProjectFilePath id.TargetFramework uid
    | TestIdTarget.Group key -> group id.ProjectFilePath id.TargetFramework key

/// Which tests a run was asked for. Ids and a filter are alternatives: an id names one test on
/// either platform, while a filter is VSTest syntax.
[<RequireQualifiedAccess>]
type TestRunSelection =
  | All
  | Filter of testCaseFilter: string
  /// Runnable tests by the id discovery issued them. An empty list runs none.
  | Ids of ParsedTestId list

module TestRunSelection =
  /// Reads the selection of a run request, rejecting one the server cannot run as asked.
  let ofRequest (testCaseFilter: string option) (testIds: string array option) : Result<TestRunSelection, string> =
    let runnable (id: string) =
      TestId.tryParse id
      |> Result.bind (fun parsed ->
        match parsed.Target with
        | TestIdTarget.Group _ -> Error $"The test id '{id}' names a grouping; run the tests under it by their own ids"
        | TestIdTarget.VsTestCase _
        | TestIdTarget.MtpNode _ -> Ok parsed)

    match testCaseFilter, testIds with
    | Some _, Some _ -> Error "TestIds and TestCaseFilter are mutually exclusive"
    | Some filter, None -> Ok(TestRunSelection.Filter filter)
    | None, None -> Ok TestRunSelection.All
    | None, Some ids ->
      let rec parseAll parsed =
        function
        | [] -> Ok(TestRunSelection.Ids(List.rev parsed))
        | id :: rest ->
          match runnable id with
          | Ok id -> parseAll (id :: parsed) rest
          | Error e -> Error e

      parseAll [] (List.ofArray ids)

/// Why a test run did not complete. A request the server cannot honour is rejected before
/// anything is launched; a run that fails once started is a different kind of failure.
[<RequireQualifiedAccess>]
type TestRunError =
  | InvalidRequest of string
  | RunFailed of string

module TestItem =
  /// The name that identifies a single test case. xUnit and MSTest report every case of a
  /// parameterised test under one fully-qualified name and vary only the display name, so the
  /// case data is appended to keep the cases apart.
  let fullNameWithParameterisedCases (executorUri: string) (fullName: string) (displayName: string) =
    match TestFrameworkId.tryOfExecutorUri executorUri with
    | Some TestFrameworkId.MsTest ->
      if fullName.EndsWith(displayName, StringComparison.Ordinal) then
        fullName
      else
        $"{fullName}.{displayName}"
    | Some TestFrameworkId.XUnit ->
      // xUnit repeats the fully-qualified name inside the display name and appends the case
      // parameters to it rather than nesting them.
      if displayName <> fullName then
        let caseFragment =
          if displayName.StartsWith(fullName, StringComparison.Ordinal) then
            let methodName = fullName.Split('.') |> Array.last
            methodName + displayName.Substring(fullName.Length)
          else
            displayName

        $"{fullName}.{caseFragment}"
      else
        fullName
    | _ -> fullName

  let ofVsTestCase
    (projFilePath: string)
    (targetFramework: string)
    (testCase: Microsoft.VisualStudio.TestPlatform.ObjectModel.TestCase)
    : TestItem =
    let fullName =
      fullNameWithParameterisedCases (string testCase.ExecutorUri) testCase.FullyQualifiedName testCase.DisplayName

    { Id = TestId.ofVsTestCase projFilePath targetFramework testCase
      ParentId = None
      IsLeaf = true
      FullName = fullName
      DisplayName = testCase.DisplayName
      ExecutorUri = testCase.ExecutorUri |> string
      ProjectFilePath = projFilePath
      TargetFramework = targetFramework
      CodeFilePath = Some testCase.CodeFilePath
      CodeLocationRange =
        Some
          { StartLine = testCase.LineNumber
            EndLine = testCase.LineNumber } }

  /// Stands in for the VSTest adapter uri on a platform that reports its own tree, where no
  /// adapter-specific name breakdown applies.
  [<Literal>]
  let mtpExecutorUri = "microsoft.testing.platform"

  let private ofMtpNodeWithParent
    (parentIdOf: string -> string)
    (projFilePath: string)
    (targetFramework: string)
    (node: FsAutoComplete.TestingPlatform.Client.TestNodeUpdate)
    : TestItem =
    let range (location: FsAutoComplete.TestingPlatform.Client.SourceLocation) =
      location.LineStart
      |> Option.map (fun startLine ->
        { StartLine = startLine
          EndLine = location.LineEnd |> Option.defaultValue startLine })

    let isLeaf =
      node.NodeType <> Some FsAutoComplete.TestingPlatform.Client.NodeType.Group

    let displayName = node.DisplayName |> Option.defaultValue node.Uid

    // MSTest displays a test by its method name alone, where xUnit displays the name of its type
    // too, so a display name is qualified by the test's type unless it already is.
    let fullName =
      match node.MethodIdentifier with
      | Some identifier ->
        let typeName =
          match identifier.Namespace with
          | Some ns -> $"{ns}.{identifier.TypeName}"
          | None -> identifier.TypeName

        if displayName.StartsWith($"{typeName}.", StringComparison.Ordinal) then
          displayName
        else
          $"{typeName}.{displayName}"
      | None -> displayName

    { Id = TestId.ofMtpNode projFilePath targetFramework node
      ParentId =
        node.ParentUid
        |> Option.filter (String.IsNullOrEmpty >> not)
        |> Option.map parentIdOf
      IsLeaf = isLeaf
      FullName = fullName
      DisplayName = displayName
      ExecutorUri = mtpExecutorUri
      ProjectFilePath = projFilePath
      TargetFramework = targetFramework
      CodeFilePath = node.Location |> Option.map _.File
      CodeLocationRange = node.Location |> Option.bind range }

  /// Maps a node of a Microsoft.Testing.Platform test tree onto the shape the clients consume.
  /// The platform reports parent links and grouping nodes itself, so the result needs no pass
  /// through `TestHierarchy.withInferredGroupings`. The parent is not in sight, so it is taken to
  /// be a grouping; `ofMtpNodes` links a parent that is reported alongside by its own id.
  let ofMtpNode (projFilePath: string) (targetFramework: string) node =
    ofMtpNodeWithParent (TestId.group projFilePath targetFramework) projFilePath targetFramework node

  /// Maps nodes one application reported. The node type is optional and a test can have
  /// children, so each parent reported among the nodes is linked by the id it was given.
  let ofMtpNodes
    (projFilePath: string)
    (targetFramework: string)
    (nodes: FsAutoComplete.TestingPlatform.Client.TestNodeUpdate list)
    : TestItem list =
    let byUid = nodes |> List.map (fun node -> node.Uid, node) |> Map.ofList

    let parentIdOf uid =
      match byUid.TryFind uid with
      | Some parent -> TestId.ofMtpNode projFilePath targetFramework parent
      | None -> TestId.group projFilePath targetFramework uid

    nodes |> List.map (ofMtpNodeWithParent parentIdOf projFilePath targetFramework)

  let tryTestCaseToDTO
    (projectLookup: string -> Ionide.ProjInfo.Types.ProjectOptions option)
    (testCase: Microsoft.VisualStudio.TestPlatform.ObjectModel.TestCase)
    : TestItem option =
    match projectLookup testCase.Source with
    | None -> None // this should never happen. We pass VsTest the list of executables to test, so all the possible sources should be known to us
    | Some project -> ofVsTestCase project.ProjectFileName project.TargetFramework testCase |> Some

/// Builds the test tree for platforms that report runnable tests alone.
module TestHierarchy =
  open System.Text.RegularExpressions

  type private Segment =
    { Text: string
      SeparatorBefore: string }

  let private segmentRegex = Regex(@"([+\.]?)([^+\.]+)", RegexOptions.Compiled)

  let private splitSegments (fullName: string) =
    [ for m in segmentRegex.Matches(fullName) ->
        { Text = m.Groups[2].Value
          SeparatorBefore = m.Groups[1].Value } ]

  /// The ancestor names of a fully-qualified test name, outermost first.
  let private ancestorNames (fullName: string) =
    splitSegments fullName
    |> List.scan (fun path segment -> $"{path}{segment.SeparatorBefore}{segment.Text}") ""
    |> List.filter (fun name -> name <> "" && name <> fullName)

  let private groupingNode (template: TestItem) (fullName: string) =
    { template with
        Id = TestId.group template.ProjectFilePath template.TargetFramework fullName
        ParentId = None
        IsLeaf = false
        FullName = fullName
        DisplayName = (splitSegments fullName |> List.last).Text
        CodeFilePath = None
        CodeLocationRange = None }

  /// Returns the given tests plus a grouping node per name segment they share, each node
  /// linked to its parent. Tests and groupings have ids of different kinds, so a test named like
  /// a grouping sits beside it rather than replacing it.
  let withInferredGroupings (tests: TestItem list) : TestItem list =
    let parentOf (item: TestItem) =
      ancestorNames item.FullName
      |> List.tryLast
      |> Option.map (TestId.group item.ProjectFilePath item.TargetFramework)

    let groupings =
      tests
      |> List.collect (fun leaf -> ancestorNames leaf.FullName |> List.map (groupingNode leaf))

    tests @ groupings
    |> List.distinctBy _.Id
    |> List.map (fun node -> { node with ParentId = parentOf node })

  /// Returns the nodes as a linked tree, one project at a time. A project whose platform reports
  /// its own parent links is taken at its word; a project reported without any is grouped by the
  /// segments of its test names.
  let withHierarchy (nodes: TestItem list) : TestItem list =
    nodes
    |> List.groupBy (fun node -> node.ProjectFilePath, node.TargetFramework)
    |> List.collect (fun (_, projectNodes) ->
      if projectNodes |> List.exists (fun node -> node.ParentId.IsSome) then
        projectNodes
      else
        withInferredGroupings projectNodes)

[<RequireQualifiedAccess>]
type TestPlatformKind =
  | VSTest
  | Mtp

module TestProject =
  [<Literal>]
  let private isTestingPlatformApplication = "IsTestingPlatformApplication"

  /// MSBuild property names the workspace loader must retain for `classify` to read.
  let requiredCustomProperties = [ isTestingPlatformApplication ]

  let private hasVsTestPackages (project: Ionide.ProjInfo.Types.ProjectOptions) =
    let indicators = set [ "Microsoft.TestPlatform.TestHost"; "Microsoft.NET.Test.Sdk" ]

    project.PackageReferences
    |> List.exists (fun pr -> Set.contains pr.Name indicators)

  let private optsIntoTestingPlatform (project: Ionide.ProjInfo.Types.ProjectOptions) =
    project.CustomProperties
    |> List.exists (fun p ->
      p.Name = isTestingPlatformApplication
      && p.Value.Equals("true", StringComparison.OrdinalIgnoreCase))

  /// The platform that will run a project's tests, or `None` for a project carrying none.
  /// A project opts into Microsoft.Testing.Platform through an MSBuild property alone, so it
  /// need not reference the VSTest packages; every other test project runs under VSTest.
  let classify (project: Ionide.ProjInfo.Types.ProjectOptions) : TestPlatformKind option =
    if optsIntoTestingPlatform project then
      Some TestPlatformKind.Mtp
    elif hasVsTestPackages project then
      Some TestPlatformKind.VSTest
    else
      None

  /// The platform a project's tests will actually be run on. With Microsoft.Testing.Platform
  /// disabled, a project that opts into it falls back to VSTest where it references the VSTest
  /// packages, and is otherwise unreachable.
  let platformFor (mtpEnabled: bool) (project: Ionide.ProjInfo.Types.ProjectOptions) : TestPlatformKind option =
    match classify project with
    | Some TestPlatformKind.Mtp when not mtpEnabled ->
      if hasVsTestPackages project then
        Some TestPlatformKind.VSTest
      else
        None
    | platform -> platform

[<RequireQualifiedAccess>]
type TestOutcome =
  | Failed = 0
  | Passed = 1
  | Skipped = 2
  | None = 3
  | NotFound = 4

module TestOutcome =
  type VSTestOutcome = Microsoft.VisualStudio.TestPlatform.ObjectModel.TestOutcome

  let ofVSTestOutcome (vsTestOutcome: VSTestOutcome) =
    match vsTestOutcome with
    | VSTestOutcome.Passed -> TestOutcome.Passed
    | VSTestOutcome.Failed -> TestOutcome.Failed
    | VSTestOutcome.Skipped -> TestOutcome.Skipped
    | VSTestOutcome.NotFound -> TestOutcome.NotFound
    | VSTestOutcome.None -> TestOutcome.None
    | _ -> TestOutcome.None

  /// Reads the verdict out of the state a Microsoft.Testing.Platform node reports. A state that
  /// records progress rather than a verdict, and one this client does not know, leave the test
  /// unjudged.
  let ofMtpExecutionState (state: FsAutoComplete.TestingPlatform.Client.ExecutionState option) =
    match state with
    | Some FsAutoComplete.TestingPlatform.Client.ExecutionState.Passed -> TestOutcome.Passed
    | Some FsAutoComplete.TestingPlatform.Client.ExecutionState.Skipped -> TestOutcome.Skipped
    | Some FsAutoComplete.TestingPlatform.Client.ExecutionState.Failed
    | Some FsAutoComplete.TestingPlatform.Client.ExecutionState.Error
    | Some FsAutoComplete.TestingPlatform.Client.ExecutionState.TimedOut -> TestOutcome.Failed
    | _ -> TestOutcome.None

type TestResult =
  { TestItem: TestItem
    Outcome: TestOutcome
    ErrorMessage: string option
    ErrorStackTrace: string option
    AdditionalOutput: string option
    Duration: TimeSpan }

module TestResult =
  type VSTestResult = Microsoft.VisualStudio.TestPlatform.ObjectModel.TestResult

  let ofVsTestResult (projFilePath: string) (targetFramework: string) (vsTestResult: VSTestResult) : TestResult =
    let stringToOption (text: string) = if String.IsNullOrEmpty(text) then None else Some text

    { Outcome = TestOutcome.ofVSTestOutcome vsTestResult.Outcome
      ErrorMessage = vsTestResult.ErrorMessage |> stringToOption
      ErrorStackTrace = vsTestResult.ErrorStackTrace |> stringToOption
      AdditionalOutput =
        match vsTestResult.Messages |> Seq.toList with
        | [] -> None
        | messages -> messages |> List.map _.Text |> String.concat Environment.NewLine |> Some
      Duration = vsTestResult.Duration
      TestItem = TestItem.ofVsTestCase projFilePath targetFramework vsTestResult.TestCase }

  /// Reads the outcome of a run out of a Microsoft.Testing.Platform node. The platform reports a
  /// result as a further update to the node that was discovered, so the test it belongs to is the
  /// node itself.
  let ofMtpNode
    (projFilePath: string)
    (targetFramework: string)
    (node: FsAutoComplete.TestingPlatform.Client.TestNodeUpdate)
    : TestResult =
    let output =
      [ node.StandardOutput; node.StandardError ]
      |> List.choose id
      |> function
        | [] -> None
        | streams -> streams |> String.concat Environment.NewLine |> Some

    { Outcome = TestOutcome.ofMtpExecutionState node.ExecutionState
      ErrorMessage = node.Error |> Option.bind _.Message
      ErrorStackTrace = node.Error |> Option.bind _.StackTrace
      AdditionalOutput = output
      Duration = node.Duration |> Option.defaultValue TimeSpan.Zero
      TestItem = TestItem.ofMtpNode projFilePath targetFramework node }
