namespace FsAutoComplete.TestServer

open System

type TestFileRange = { StartLine: int; EndLine: int }

type TestItem =
  {
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

module TestItem =
  let ofVsTestCase
    (projFilePath: string)
    (targetFramework: string)
    (testCase: Microsoft.VisualStudio.TestPlatform.ObjectModel.TestCase)
    : TestItem =
    { FullName = testCase.FullyQualifiedName
      DisplayName = testCase.DisplayName
      ExecutorUri = testCase.ExecutorUri |> string
      ProjectFilePath = projFilePath
      TargetFramework = targetFramework
      CodeFilePath = Some testCase.CodeFilePath
      CodeLocationRange =
        Some
          { StartLine = testCase.LineNumber
            EndLine = testCase.LineNumber } }

  let tryTestCaseToDTO
    (projectLookup: string -> Ionide.ProjInfo.Types.ProjectOptions option)
    (testCase: Microsoft.VisualStudio.TestPlatform.ObjectModel.TestCase)
    : TestItem option =
    match projectLookup testCase.Source with
    | None -> None // this should never happen. We pass VsTest the list of executables to test, so all the possible sources should be known to us
    | Some project -> ofVsTestCase project.ProjectFileName project.TargetFramework testCase |> Some

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
