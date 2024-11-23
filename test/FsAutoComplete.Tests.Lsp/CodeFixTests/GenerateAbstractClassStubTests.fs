module private FsAutoComplete.Tests.CodeFixTests.GenerateAbstractClassStubTests

open Expecto
open Helpers
open Utils.ServerTests
open Utils.CursorbasedTests
open FsAutoComplete.CodeFix
open System.IO

let sourceDir = __SOURCE_DIRECTORY__
let expectedTestDir = Path.Join(sourceDir, "GenerateAbstractClassStubTests")

let tfm =
#if NET8_0
  "net80"
#else
  "net90"
#endif

let expectFile file = File.ReadAllText(Path.Join(expectedTestDir, file))
let tests state =
  let config = { defaultConfigDto with AbstractClassStubGeneration = Some true }
  serverTestList (nameof GenerateAbstractClassStub) state config None (fun server -> [
    let selectCodeFix = CodeFix.withTitle GenerateAbstractClassStub.title
    testCaseAsync "can generate a derivative of a long ident - System.IO.Stream" <|
      CodeFix.checkApplicable server
        """
        type My$0Stream() =
          inherit System.IO.Stream()
        """
        (Diagnostics.expectCode "365")
        selectCodeFix
    testCaseAsync "can generate a derivative for a simple ident - Stream" <|
      CodeFix.checkApplicable server
        """
        open System.IO
        type My$0Stream2() =
          inherit Stream()
        """
        (Diagnostics.expectCode "365")
        selectCodeFix
    testCaseAsync "can generate abstract class stub" <|

      CodeFix.check server
        """
        [<AbstractClass>]
        type Shape(x0: float, y0: float) =
          let mutable x, y = x0, y0

          abstract Name : string with get
          abstract Area : float with get

          member _.Move dx dy =
            x <- x + dx
            y <- y + dy

        type $0Square(x,y, sideLength) =
          inherit Shape(x,y)
        ()"""
        (Diagnostics.expectCode "365")
        selectCodeFix
        (expectFile $"can_generate_abstract_class_stub.{tfm}.expected")

    testCaseAsync "can generate abstract class stub with another member with attribute" <|
      CodeFix.check server
        """
        [<AbstractClass>]
        type Shape(x0: float, y0: float) =
          let mutable x, y = x0, y0

          abstract Name : string with get
          abstract Area : float with get

          member _.Move dx dy =
            x <- x + dx
            y <- y + dy

        type $0Square(x,y, sideLength) =
          inherit Shape(x,y)

          [<CompiledName("yo")>]
          member x.Foo() = 1
        ()"""
        (Diagnostics.expectCode "365")
        selectCodeFix
         (expectFile $"can_generate_abstract_class_stub_with_another_member_with_attribute.{tfm}.expected")

    testCaseAsync "can generate abstract class stub without trailing nl" <|
      CodeFix.check server
        """
        [<AbstractClass>]
        type Shape(x0: float, y0: float) =
          let mutable x, y = x0, y0

          abstract Name : string with get
          abstract Area : float with get

          member _.Move dx dy =
            x <- x + dx
            y <- y + dy

        type $0Square(x,y, sideLength) =
          inherit Shape(x,y)
        ()"""
        (Diagnostics.expectCode "365")
        selectCodeFix
        (expectFile $"can_generate_abstract_class_stub_without_trailing_nl.{tfm}.expected")

    testCaseAsync "inserts override in correct place" <|
      CodeFix.check server
        """
        [<AbstractClass>]
        type Shape(x0: float, y0: float) =
          let mutable x, y = x0, y0

          abstract Name : string with get
          abstract Area : float with get

          member _.Move dx dy =
            x <- x + dx
            y <- y + dy

        type $0Square(x,y, sideLength) =
          inherit Shape(x,y)
        let a = 0"""
        (Diagnostics.expectCode "365")
        selectCodeFix
        (expectFile $"inserts_override_in_correct_place.{tfm}.expected")

    testCaseAsync "can generate abstract class stub with existing override" <|
      CodeFix.check server
        """
        [<AbstractClass>]
        type Shape(x0: float, y0: float) =
          let mutable x, y = x0, y0

          abstract Name : string with get
          abstract Area : float with get

          member _.Move dx dy =
            x <- x + dx
            y <- y + dy

        type $0Square(x,y, sideLength) =
          inherit Shape(x,y)

          override this.Name = "Circle"
        ()"""
        (Diagnostics.expectCode "365")
        selectCodeFix
        (expectFile $"can_generate_abstract_class_stub_with_existing_override.{tfm}.expected")
  ])
