module FsAutoComplete.Tests.UnusedDeclarationsTests
open Expecto
open FsAutoComplete
open Helpers
open Utils.Server
open Utils.ServerTests
open Utils.TextEdit
open Utils.Utils
open Ionide.LanguageServerProtocol.Types
open System.IO

type private Expected =
  | Used
  | Unused
type private Doc =
  | Untitled
  | Existing of path:string
let private checkUsageAt
  server
  doc
  sourceWithoutCursor
  cursor
  expected
  = async {
    let source = sourceWithoutCursor |> Text.trimTripleQuotation
    let! (doc, diags) = 
      let getDoc =
        match doc with
        | Untitled -> Server.createUntitledDocument
        | Existing path -> Server.openDocumentWithText path
      server |> getDoc source
    use doc = doc

    let diagsAtCursor =
      diags
      |> Array.filter (fun d -> d.Range |> Range.containsStrictly cursor)

    let isUnused (diag: Diagnostic) =
      diag.Source = "FSAC"
      &&
      diag.Code = Some "FSAC0003"
      &&
      diag.Message = "This value is unused"
      &&
      diag.Tags |> Option.map (Array.contains DiagnosticTag.Unnecessary) |> Option.defaultValue false

    let diag = diagsAtCursor |> Array.filter isUnused
    match expected with
    | Unused -> 
        Expect.hasLength diag 1 "There should be exactly one unused value diagnostic at cursor position"
    | Used -> 
        Expect.hasLength diag 0 "There should be no unused value diagnostic at cursor position"
  }

let private checkUsage
  server
  doc
  sourceWithCursor
  expected
  = async {
    let (cursor, source) =
      sourceWithCursor 
      |> Text.trimTripleQuotation
      |> Cursor.assertExtractPosition
    do! checkUsageAt server doc source cursor expected
  }
  

let private scriptTests state =
  let config = {
    defaultConfigDto with
      UnusedDeclarationsAnalyzer = Some true
  }
  serverTestList "script" state config None (fun server -> [
    testCaseAsync "unused variable" <|
      checkUsage server Untitled
        """
        let $0alpha = 42
        """
        Unused
    testCaseAsync "used variable" <|
      checkUsage server Untitled
        """
        let $0alpha = 42
        let _ = alpha
        """
        Used
    testCaseAsync "unused tuple element variable" <|
      checkUsage server Untitled
        """
        let (alpha, $0beta) = (42, 42)
        let _ = alpha
        """
        Unused
    testCaseAsync "used tuple element variable" <|
      checkUsage server Untitled
        """
        let ($0alpha, beta) = (42, 42)
        let _ = alpha
        """
        Used
    testCaseAsync "unused this" <|
      checkUsage server Untitled
        """
        type T() =
          member $0this.F _ = ()
        """
        Unused
    testCaseAsync "used this" <|
      checkUsage server Untitled
        """
        type T() =
          member $0this.F _ = this.GetType() |> ignore
        """
        Used

    testCaseAsync "unused function variable" <|
      checkUsage server Untitled
        """
        let $0f _ = ()
        """
        Unused
    testCaseAsync "used function variable" <|
      checkUsage server Untitled
        """
        let $0f _ = ()
        f ()
        """
        Used
  ])

let private projectTests state =
  let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "UnusedDeclarations")
  let file = Existing "Library.fs"

  let config = {
    defaultConfigDto with
      UnusedDeclarationsAnalyzer = Some true
  }
  serverTestList "project" state config (Some path) (fun server -> [
    // Note: public variables aren't considered unused (-> can be used somewhere else)

    testCaseAsync "unused private variable" <|
      checkUsage server file
        """
        module Root

        let private $0alpha = 42
        """
        Unused
    testCaseAsync "unused public variable" <|
      checkUsage server file
        """
        module Root

        let $0alpha = 42
        """
        Used
    testCaseAsync "used private variable" <|
      checkUsage server file
        """
        // module Root

        let private $0alpha = 42

        let _ = alpha
        """
        Used
    testCaseAsync "unused private tuple element variable" <|
      // Note: `let private (alpha, beta) =` is incorrect!
      //   must be: `let (private alpha, private beta) =`
      checkUsage server file
        """
        module Root

        let (private alpha, private $0beta) = (42, 42)
        let _ = alpha
        """
        Unused
    testCaseAsync "used private tuple element variable" <|
      checkUsage server file
        """
        module Root

        let (private $0alpha, private beta) = (42, 42)
        let _ = alpha
        """
        Used
    testCaseAsync "unused public tuple element variable" <|
      checkUsage server file
        """
        module Root

        let (alpha, $0beta) = (42, 42)
        let _ = alpha
        """
        Used
    testCaseAsync "used public tuple element variable" <|
      checkUsage server file
        """
        module Root

        let ($0alpha, beta) = (42, 42)
        let _ = alpha
        """
        Used

    testCaseAsync "unused this" <|
      checkUsage server file
        """
        module Root

        type T() =
          member $0this.F _ = ()
        """
        Unused
    testCaseAsync "used this" <|
      checkUsage server file
        """
        module Root

        type T() =
          member $0this.F _ = this.GetType() |> ignore
        """
        Used

    
    testCaseAsync "unused private function variable" <|
      checkUsage server file
        """
        module Root

        let private $0f _ = ()
        """
        Unused
    testCaseAsync "used private function variable" <|
      checkUsage server file
        """
        module Root

        let private $0f _ = ()
        f ()
        """
        Used
    testCaseAsync "unused public function variable" <|
      checkUsage server file
        """
        module Root

        let $0f _ = ()
        """
        Used
    
    // https://github.com/fsharp/FsAutoComplete/issues/832
    testList "issue #832" [
      // `$P`: used (or public -> not marked unused)
      // `$U`: unused
      let source = """
module External =
    let $Pa = 123

    let private $Ub = 321

    let $Px _ = ()

    let private $Uy _ = ()

module private Internal =
    let $Ua = 123

    let private $Ub = 321

    let $Ux _ = ()

    let private $Uy _ = ()
      """

      let (source, cursors) =
        source
        |> Text.trimTripleQuotation
        |> Cursors.extractWith [| "$P"; "$U" |]

      for (marker, pos) in cursors do
        let expected = if marker = "$P" then Used else Unused
        let title = $"%A{expected} at %s{pos.DebuggerDisplay}"
        testCaseAsync title <|
          checkUsageAt server file
            source pos
            expected
    ]
  ])

let tests state =
  testList ("Unused Declarations") [
    // Note: difference between Script & Project:
    //   Public in Script can be unused, public in Project cannot
    scriptTests state
    projectTests state
  ]
