module private FsAutoComplete.Tests.CodeFixTests.ImplementInterfaceTests

open Expecto
open Helpers
open Utils.ServerTests
open Utils.CursorbasedTests
open FsAutoComplete.CodeFix


let tests state =
  let selectCodeFixWithTypeAnnotation = CodeFix.withTitle ImplementInterface.titleWithTypeAnnotation
  let selectCodeFixWithoutTypeAnnotation = CodeFix.withTitle ImplementInterface.titleWithoutTypeAnnotation
  let validateDiags = Diagnostics.expectCode "366"
  let testBoth server name beforeWithCursor expectedWithTypeAnnotation expectedWithoutTypeAnnotation =
      testList name [
        testCaseAsync "with type annotation" <|
          CodeFix.check server
            beforeWithCursor
            validateDiags
            selectCodeFixWithTypeAnnotation
            expectedWithTypeAnnotation
        testCaseAsync "without type annotation" <|
          CodeFix.check server
            beforeWithCursor
            validateDiags
            selectCodeFixWithoutTypeAnnotation
            expectedWithoutTypeAnnotation
      ]
  // Note: there's a space after each generated `=` when linebreak! (-> from FCS)
  testList (nameof ImplementInterface) [
    let config = {
      defaultConfigDto with
        IndentationSize = Some 2
        InterfaceStubGeneration = Some true
        InterfaceStubGenerationObjectIdentifier = Some "_"
        InterfaceStubGenerationMethodBody = Some "failwith \"-\""
    }

    serverTestList "with 2 indentation" state config None (fun server -> [
      let testBoth = testBoth server
      testList "in type" [
        testBoth "can implement single interface with single method with existing with"
          """
          type X() =
            interface System.$0IDisposable with
          """
          """
          type X() =
            interface System.IDisposable with
              member _.Dispose(): unit =
                failwith "-"
          """
          """
          type X() =
            interface System.IDisposable with
              member _.Dispose() = failwith "-"
          """
        testBoth "can implement single interface with single method without existing with"
          """
          type X() =
            interface System.$0IDisposable
          """
          """
          type X() =
            interface System.IDisposable with
              member _.Dispose(): unit =
                failwith "-"
          """
          """
          type X() =
            interface System.IDisposable with
              member _.Dispose() = failwith "-"
          """
        testBoth "can implement single interface with multiple methods (none already specified)"
          """
          type IPrinter =
            abstract member Print: format: string -> unit
            abstract member Indentation: int with get,set
            abstract member Disposed: bool

          type Printer() =
            interface $0IPrinter with
          """
          """
          type IPrinter =
            abstract member Print: format: string -> unit
            abstract member Indentation: int with get,set
            abstract member Disposed: bool

          type Printer() =
            interface IPrinter with
              member _.Disposed: bool =
                failwith "-"
              member _.Indentation
                with get (): int =
                  failwith "-"
                and set (v: int): unit =
                  failwith "-"
              member _.Print(format: string): unit =
                failwith "-"
          """
          """
          type IPrinter =
            abstract member Print: format: string -> unit
            abstract member Indentation: int with get,set
            abstract member Disposed: bool

          type Printer() =
            interface IPrinter with
              member _.Disposed = failwith "-"
              member _.Indentation
                with get (): int =
                  failwith "-"
                and set (v: int): unit =
                  failwith "-"
              member _.Print(format) = failwith "-"
          """
        testBoth "can implement setter when existing getter"
          """
          type IPrinter =
            abstract member Indentation: int with get,set

          type Printer() =
            interface $0IPrinter with
              member _.Indentation with get () = 42
          """
          """
          type IPrinter =
            abstract member Indentation: int with get,set

          type Printer() =
            interface IPrinter with
              member _.Indentation
                with set (v: int): unit =
                  failwith "-"
              member _.Indentation with get () = 42
          """
          """
          type IPrinter =
            abstract member Indentation: int with get,set

          type Printer() =
            interface IPrinter with
              member _.Indentation with get () = 42
              member _.Indentation
                with set (v: int): unit =
                failwith "-"
          """
        testBoth "can implement interface member without parameter name"
          """
          type I =
            abstract member DoStuff: int -> unit

          type T() =
            interface $0I with
          """
          """
          type I =
            abstract member DoStuff: int -> unit

          type T() =
            interface I with
              member _.DoStuff(arg1: int): unit =
                failwith "-"
          """
          """
          type I =
            abstract member DoStuff: int -> unit

          type T() =
            interface I with
              member _.DoStuff(arg1) = failwith "-"
          """
        testBoth "can implement when one member already implemented"
          """
          type I =
            abstract member DoStuff: value:int -> unit
            abstract member DoOtherStuff: value:int -> name:string -> string
            abstract member Value: int

          type T() =
            interface $0I with
              member _.DoOtherStuff value name = name
          """
          """
          type I =
            abstract member DoStuff: value:int -> unit
            abstract member DoOtherStuff: value:int -> name:string -> string
            abstract member Value: int

          type T() =
            interface I with
              member _.DoOtherStuff value name = name
              member _.DoStuff(value: int): unit =
                failwith "-"
              member _.Value: int =
                failwith "-"
          """
          """
          type I =
            abstract member DoStuff: value:int -> unit
            abstract member DoOtherStuff: value:int -> name:string -> string
            abstract member Value: int

          type T() =
            interface I with
              member _.DoOtherStuff value name = name
              member _.DoStuff(value) = failwith "-"
              member _.Value = failwith "-"
          """
        testBoth "can implement when two members already implemented"
          """
          type I =
            abstract member DoStuff: value:int -> unit
            abstract member DoOtherStuff: value:int -> name:string -> string
            abstract member Value: int

          type T() =
            interface $0I with
              member _.DoOtherStuff value name = name
              member _.Value: int = failwith "-"
          """
          """
          type I =
            abstract member DoStuff: value:int -> unit
            abstract member DoOtherStuff: value:int -> name:string -> string
            abstract member Value: int

          type T() =
            interface I with
              member _.DoOtherStuff value name = name
              member _.Value: int = failwith "-"
              member _.DoStuff(value: int): unit =
                failwith "-"
          """
          """
          type I =
            abstract member DoStuff: value:int -> unit
            abstract member DoOtherStuff: value:int -> name:string -> string
            abstract member Value: int

          type T() =
            interface I with
              member _.DoOtherStuff value name = name
              member _.Value: int = failwith "-"
              member _.DoStuff(value) = failwith "-"
          """
        testBoth "can implement interface with existing class members"
          """
          type T() =
            let v = 4
            member _.DoStuff value = v + value
            interface System.$0IDisposable with
          """
          """
          type T() =
            let v = 4
            member _.DoStuff value = v + value
            interface System.IDisposable with
              member _.Dispose(): unit =
                failwith "-"
          """
          """
          type T() =
            let v = 4
            member _.DoStuff value = v + value
            interface System.IDisposable with
              member _.Dispose() = failwith "-"
          """
        testBoth "can implement in record"
          """
          type A =
            {
              Value: int
            }
            interface System.$0IDisposable with
          """
          """
          type A =
            {
              Value: int
            }
            interface System.IDisposable with
              member _.Dispose(): unit =
                failwith "-"
          """
          """
          type A =
            {
              Value: int
            }
            interface System.IDisposable with
              member _.Dispose() = failwith "-"
          """
        testBoth "can implement in union"
          """
          type U =
            | A of int
            | B of int * string
            | C
            interface System.$0IDisposable with
          """
          """
          type U =
            | A of int
            | B of int * string
            | C
            interface System.IDisposable with
              member _.Dispose(): unit =
                failwith "-"
          """
          """
          type U =
            | A of int
            | B of int * string
            | C
            interface System.IDisposable with
              member _.Dispose() = failwith "-"
          """
      ]
      testList "in object expression" [
        testBoth "can implement single interface with single method with existing with and } in same line"
          """
          { new System.$0IDisposable with }
          """
          """
          { new System.IDisposable with
              member _.Dispose(): unit =
                failwith "-" }
          """
          """
          { new System.IDisposable with
              member _.Dispose() = failwith "-" }
          """
        testBoth "can implement single interface with single method without existing with and with } in same line"
          """
          { new System.$0IDisposable }
          """
          """
          { new System.IDisposable with
              member _.Dispose(): unit =
                failwith "-" }
          """
          """
          { new System.IDisposable with
              member _.Dispose() = failwith "-" }
          """
        testBoth "can implement single interface with single method without existing with and without } in same line"
          """
          { new System.$0IDisposable
          """
          """
          { new System.IDisposable with
              member _.Dispose(): unit =
                failwith "-" }
          """
          """
          { new System.IDisposable with
              member _.Dispose() = failwith "-" }
          """
        // Note: `{ new System.IDisposable with` doesn't raise `FS0366` -> no CodeFix

        testBoth "can implement single interface with multiple methods"
          """
          type I =
            abstract member DoStuff: value:int -> unit
            abstract member DoOtherStuff: value:int -> name:string -> string
            abstract member Value: int

          { new $0I with }
          """
          """
          type I =
            abstract member DoStuff: value:int -> unit
            abstract member DoOtherStuff: value:int -> name:string -> string
            abstract member Value: int

          { new I with
              member _.DoOtherStuff(value: int) (name: string): string =
                failwith "-"
              member _.DoStuff(value: int): unit =
                failwith "-"
              member _.Value: int =
                failwith "-" }
          """
          """
          type I =
            abstract member DoStuff: value:int -> unit
            abstract member DoOtherStuff: value:int -> name:string -> string
            abstract member Value: int

          { new I with
              member _.DoOtherStuff value name = failwith "-"
              member _.DoStuff(value) = failwith "-"
              member _.Value = failwith "-" }
          """
        testBoth "can implement interface with multiple methods with one method already implemented"
          """
          type I =
            abstract member DoStuff: value:int -> unit
            abstract member DoOtherStuff: value:int -> name:string -> string
            abstract member Value: int

          { new $0I with
              member this.DoStuff(value: int): unit =
                let v = value + 4
                printfn "Res=%i" v
          }
          """
          """
          type I =
            abstract member DoStuff: value:int -> unit
            abstract member DoOtherStuff: value:int -> name:string -> string
            abstract member Value: int

          { new I with
              member this.DoStuff(value: int): unit =
                let v = value + 4
                printfn "Res=%i" v
              member _.DoOtherStuff(value: int) (name: string): string =
                failwith "-"
              member _.Value: int =
                failwith "-"
          }
          """
          """
          type I =
            abstract member DoStuff: value:int -> unit
            abstract member DoOtherStuff: value:int -> name:string -> string
            abstract member Value: int

          { new I with
              member this.DoStuff(value: int): unit =
                let v = value + 4
                printfn "Res=%i" v
              member _.DoOtherStuff value name = failwith "-"
              member _.Value = failwith "-"
          }
          """
        testBoth "can trigger with { and } on different lines"
          """
          {
            new System.$0IDisposable with
          }
          """
          """
          {
            new System.IDisposable with
              member _.Dispose(): unit =
                failwith "-"
          }
          """
          """
          {
            new System.IDisposable with
              member _.Dispose() = failwith "-"
          }
          """
        testBoth "can implement sub interface"
          """
          { new System.IDisposable with
              member _.Dispose() = ()
            interface System.$0ICloneable with
          }
          """
          """
          { new System.IDisposable with
              member _.Dispose() = ()
            interface System.ICloneable with
              member _.Clone(): obj =
                failwith "-"
          }
          """
          """
          { new System.IDisposable with
              member _.Dispose() = ()
            interface System.ICloneable with
              member _.Clone() = failwith "-"
          }
          """
        testBoth "can trigger with cursor on {"
          """
          { new System.IDisposable with
              member _.Dispose() = ()
            interface System.ICloneable with
          }$0
          """
          """
          { new System.IDisposable with
              member _.Dispose() = ()
            interface System.ICloneable with
              member _.Clone(): obj =
                failwith "-"
          }
          """
          """
          { new System.IDisposable with
              member _.Dispose() = ()
            interface System.ICloneable with
              member _.Clone() = failwith "-"
          }
          """
      ]
      testList "cursor position" [
        testList "type" [
          // diagnostic range is just interface name
          // -> triggers only with cursor on interface name
          testCaseAsync "start of name" <|
            CodeFix.check server
              """
              type T() =
                interface $0System.IDisposable with
              """
              validateDiags
              selectCodeFixWithoutTypeAnnotation
              """
              type T() =
                interface System.IDisposable with
                  member _.Dispose() = failwith "-"
              """
          testCaseAsync "end of name" <|
            CodeFix.check server
              """
              type T() =
                interface System.IDisposable$0 with
              """
              validateDiags
              selectCodeFixWithoutTypeAnnotation
              """
              type T() =
                interface System.IDisposable with
                  member _.Dispose() = failwith "-"
              """
          testCaseAsync "middle of name" <|
            CodeFix.check server
              """
              type T() =
                interface System.IDisp$0osable with
              """
              validateDiags
              selectCodeFixWithoutTypeAnnotation
              """
              type T() =
                interface System.IDisposable with
                  member _.Dispose() = failwith "-"
              """
        ]
        testList "object expression" [
          // diagnostic range:
          // * main interface: over complete obj expr (start of `{` to end of `}`)
          // * sub interface: start of `interface` to end of `}`
          testList "main" [
            testList "all on same line" [
              testCaseAsync "{" <|
                CodeFix.check server
                  """
                  $0{ new System.IDisposable with }
                  """
                  validateDiags
                  selectCodeFixWithoutTypeAnnotation
                  """
                  { new System.IDisposable with
                      member _.Dispose() = failwith "-" }
                  """
              testCaseAsync "new" <|
                CodeFix.check server
                  """
                  { $0new System.IDisposable with }
                  """
                  validateDiags
                  selectCodeFixWithoutTypeAnnotation
                  """
                  { new System.IDisposable with
                      member _.Dispose() = failwith "-" }
                  """
              testCaseAsync "with" <|
                CodeFix.check server
                  """
                  { new System.IDisposable w$0ith }
                  """
                  validateDiags
                  selectCodeFixWithoutTypeAnnotation
                  """
                  { new System.IDisposable with
                      member _.Dispose() = failwith "-" }
                  """
              testCaseAsync "}" <|
                CodeFix.check server
                  """
                  { new System.IDisposable with $0}
                  """
                  validateDiags
                  selectCodeFixWithoutTypeAnnotation
                  """
                  { new System.IDisposable with
                      member _.Dispose() = failwith "-" }
                  """
            ]
            testList "on different lines" [
              testCaseAsync "{" <|
                CodeFix.check server
                  """
                  $0{
                    new System.IDisposable with
                  }
                  """
                  validateDiags
                  selectCodeFixWithoutTypeAnnotation
                  """
                  {
                    new System.IDisposable with
                      member _.Dispose() = failwith "-"
                  }
                  """
              testCaseAsync "new" <|
                CodeFix.check server
                  """
                  {
                    n$0ew System.IDisposable with
                  }
                  """
                  validateDiags
                  selectCodeFixWithoutTypeAnnotation
                  """
                  {
                    new System.IDisposable with
                      member _.Dispose() = failwith "-"
                  }
                  """
              testCaseAsync "interface name" <|
                CodeFix.check server
                  """
                  {
                    new System.IDis$0posable with
                  }
                  """
                  validateDiags
                  selectCodeFixWithoutTypeAnnotation
                  """
                  {
                    new System.IDisposable with
                      member _.Dispose() = failwith "-"
                  }
                  """
              testCaseAsync "with" <|
                CodeFix.check server
                  """
                  {
                    new System.IDisposable wi$0th
                  }
                  """
                  validateDiags
                  selectCodeFixWithoutTypeAnnotation
                  """
                  {
                    new System.IDisposable with
                      member _.Dispose() = failwith "-"
                  }
                  """
              testCaseAsync "}" <|
                CodeFix.check server
                  """
                  {
                    new System.IDisposable with
                  }$0
                  """
                  validateDiags
                  selectCodeFixWithoutTypeAnnotation
                  """
                  {
                    new System.IDisposable with
                      member _.Dispose() = failwith "-"
                  }
                  """
            ]
          ]
          testList "sub" [
            testCaseAsync "interface" <|
              CodeFix.check server
                """
                open System
                {
                  new IDisposable with
                    member _.Dispose() = failwith "-"
                  in$0terface ICloneable with
                }
                """
                validateDiags
                selectCodeFixWithoutTypeAnnotation
                """
                open System
                {
                  new IDisposable with
                    member _.Dispose() = failwith "-"
                  interface ICloneable with
                    member _.Clone() = failwith "-"
                }
                """
            testCaseAsync "interface name" <|
              CodeFix.check server
                """
                open System
                {
                  new IDisposable with
                    member _.Dispose() = failwith "-"
                  interface IClo$0neable with
                }
                """
                validateDiags
                selectCodeFixWithoutTypeAnnotation
                """
                open System
                {
                  new IDisposable with
                    member _.Dispose() = failwith "-"
                  interface ICloneable with
                    member _.Clone() = failwith "-"
                }
                """
            testCaseAsync "with" <|
              CodeFix.check server
                """
                open System
                {
                  new IDisposable with
                    member _.Dispose() = failwith "-"
                  interface ICloneable wi$0th
                }
                """
                validateDiags
                selectCodeFixWithoutTypeAnnotation
                """
                open System
                {
                  new IDisposable with
                    member _.Dispose() = failwith "-"
                  interface ICloneable with
                    member _.Clone() = failwith "-"
                }
                """
            testCaseAsync "}" <|
              CodeFix.check server
                """
                open System
                {
                  new IDisposable with
                    member _.Dispose() = failwith "-"
                  interface ICloneable with
                }$0
                """
                validateDiags
                selectCodeFixWithoutTypeAnnotation
                """
                open System
                {
                  new IDisposable with
                    member _.Dispose() = failwith "-"
                  interface ICloneable with
                    member _.Clone() = failwith "-"
                }
                """
          ]
        ]
      ]
      testList "strange existing formatting" [
        testList "type" [
          testCaseAsync "interface on prev line" <|
            CodeFix.check server
              """
              open System
              type A () =
                interface
                  $0IDisposable with
              """
              validateDiags
              selectCodeFixWithoutTypeAnnotation
              """
              open System
              type A () =
                interface
                  IDisposable with
                  member _.Dispose() = failwith "-"
              """
          testCaseAsync "with on next line" <|
            CodeFix.check server
              """
              open System
              type A () =
                interface $0IDisposable
                    with
              """
              validateDiags
              selectCodeFixWithoutTypeAnnotation
              """
              open System
              type A () =
                interface IDisposable
                    with
                  member _.Dispose() = failwith "-"
              """
          testCaseAsync "interface and with on extra lines" <|
            CodeFix.check server
              """
              open System
              type A () =
                interface
                  $0IDisposable
                    with
              """
              validateDiags
              selectCodeFixWithoutTypeAnnotation
              """
              open System
              type A () =
                interface
                  IDisposable
                    with
                  member _.Dispose() = failwith "-"
              """
          testCaseAsync "attribute" <|
            CodeFix.check server
              """
              open System
              type I =
                abstract member DoStuff: value:int -> unit
                abstract member DoOtherStuff: value:int -> string

              type A () =
                interface $0I with
                  [<Obsolete>]
                    member _.DoStuff value = ()
              """
              validateDiags
              selectCodeFixWithoutTypeAnnotation
              """
              open System
              type I =
                abstract member DoStuff: value:int -> unit
                abstract member DoOtherStuff: value:int -> string

              type A () =
                interface I with
                  [<Obsolete>]
                    member _.DoStuff value = ()
                  member _.DoOtherStuff(value) = failwith "-"
              """
          testCaseAsync "inline comment" <|
            CodeFix.check server
              """
              open System
              type I =
                abstract member DoStuff: value:int -> unit
                abstract member DoOtherStuff: value:int -> string

              type A () =
                interface $0I with
                  (*foo bar*)[<Obsolete>]
                (*baaaaaaaaaaaz*)member _.DoStuff value = ()
              """
              validateDiags
              selectCodeFixWithoutTypeAnnotation
              // new member must be at least aligned to Attribute
              """
              open System
              type I =
                abstract member DoStuff: value:int -> unit
                abstract member DoOtherStuff: value:int -> string

              type A () =
                interface I with
                  (*foo bar*)[<Obsolete>]
                (*baaaaaaaaaaaz*)member _.DoStuff value = ()
                             member _.DoOtherStuff(value) = failwith "-"
              """
        ]
        testList "obj expr" [
          testCaseAsync "with on next line" <|
            CodeFix.check server
              """
              open System
              {
                new IDis$0posable
                with
              }
              """
              validateDiags
              selectCodeFixWithoutTypeAnnotation
              """
              open System
              {
                new IDisposable
                with
                  member _.Dispose() = failwith "-"
              }
              """
          testCaseAsync "with 3 lines below with comments" <|
            CodeFix.check server
              """
              open System
              {
                new IDis$0posable
                // some
                // comment
                with
              }
              """
              validateDiags
              selectCodeFixWithoutTypeAnnotation
              """
              open System
              {
                new IDisposable
                // some
                // comment
                with
                  member _.Dispose() = failwith "-"
              }
              """
          testCaseAsync "new on prev line" <|
            CodeFix.check server
              """
              open System
              {
                new
                      IDis$0posable with
              }
              """
              validateDiags
              selectCodeFixWithoutTypeAnnotation
              """
              open System
              {
                new
                      IDisposable with
                  member _.Dispose() = failwith "-"
              }
              """
          testCaseAsync "new and with on extra lines" <|
            CodeFix.check server
              """
              open System
              {
                new
                  IDis$0posable
                with
              }
              """
              validateDiags
              selectCodeFixWithoutTypeAnnotation
              """
              open System
              {
                new
                  IDisposable
                with
                  member _.Dispose() = failwith "-"
              }
              """
        ]
      ]
    ])
    let config = {
      defaultConfigDto with
        IndentationSize = Some 6
        InterfaceStubGeneration = Some true
        InterfaceStubGenerationObjectIdentifier = Some "this"
        InterfaceStubGenerationMethodBody = Some "raise (System.NotImplementedException())"
    }
    serverTestList "with 6 indentation" state config None (fun server -> [
      let testBoth = testBoth server
      testBoth "uses indentation, object identifier & method body from config"
          """
          type X() =
                interface System.$0IDisposable with
          """
          """
          type X() =
                interface System.IDisposable with
                      member this.Dispose(): unit =
                            raise (System.NotImplementedException())
          """
          """
          type X() =
                interface System.IDisposable with
                      member this.Dispose() = raise (System.NotImplementedException())
          """
      ()
    ])
  ]
