module private FsAutoComplete.Tests.CodeFixTests.AddExplicitTypeAnnotationTests

open Expecto
open Helpers
open Utils.ServerTests
open Utils.CursorbasedTests
open FsAutoComplete.CodeFix

let tests state =
  serverTestList (nameof AddExplicitTypeAnnotation) state defaultConfigDto None (fun server -> [
    let selectCodeFix = CodeFix.withTitle AddExplicitTypeAnnotation.title
    testCaseAsync "can suggest explicit parameter for record-typed function parameters" <|
      CodeFix.check server
        """
        type Foo =
            { name: string }

        let name $0f =
            f.name
        """
        (Diagnostics.acceptAll)
        selectCodeFix
        """
        type Foo =
            { name: string }

        let name (f: Foo) =
            f.name
        """
    testCaseAsync "can add type for int param" <|
      CodeFix.check server
        """
        let f ($0x) = x + 1
        """
        (Diagnostics.acceptAll)
        selectCodeFix
        """
        let f (x: int) = x + 1
        """
    testCaseAsync "can add type for generic param" <|
      CodeFix.check server
        """
        let f ($0x) = ()
        """
        (Diagnostics.acceptAll)
        selectCodeFix
        """
        let f (x: 'a) = ()
        """
    testCaseAsync "doesn't trigger when existing type" <|
      CodeFix.checkNotApplicable server
        """
        let f ($0x: int) = ()
        """
        (Diagnostics.acceptAll)
        selectCodeFix
    testCaseAsync "can add type to tuple item" <|
      CodeFix.check server
        """
        let f (a, $0b, c) = a + b + c + 1
        """
        (Diagnostics.acceptAll)
        selectCodeFix
        """
        let f (a, b: int, c) = a + b + c + 1
        """
    testCaseAsync "doesn't trigger in tuple when existing type" <|
      CodeFix.checkNotApplicable server
        """
        let f (a, $0b: int, c) = a + b + c + 1
        """
        (Diagnostics.acceptAll)
        selectCodeFix
    testCaseAsync "can add type to 2nd of 3 param" <|
      CodeFix.check server
        """
        let f a $0b c = a + b + c + 1
        """
        (Diagnostics.acceptAll)
        selectCodeFix
        """
        let f a (b: int) c = a + b + c + 1
        """
    testCaseAsync "doesn't trigger on 2nd of 3 param when existing type" <|
      CodeFix.checkNotApplicable server
        """
        let f a ($0b: int) c = a + b + c + 1
        """
        (Diagnostics.acceptAll)
        selectCodeFix
    testCaseAsync "can add type to 2nd of 3 param when other params have types" <|
      CodeFix.check server
        """
        let f (a: int) $0b (c: int) = a + b + c + 1
        """
        (Diagnostics.acceptAll)
        selectCodeFix
        """
        let f (a: int) (b: int) (c: int) = a + b + c + 1
        """
    testCaseAsync "can add type to member param" <|
      CodeFix.check server
        """
        type A() =
          member _.F($0a) = a + 1
        """
        (Diagnostics.acceptAll)
        selectCodeFix
        """
        type A() =
          member _.F(a: int) = a + 1
        """
    testCaseAsync "doesn't trigger for member param when existing type" <|
      CodeFix.checkNotApplicable server
        """
        type A() =
          member _.F($0a: int) = a + 1
        """
        (Diagnostics.acceptAll)
        selectCodeFix
    testCaseAsync "can add type to ctor param" <|
      CodeFix.check server
        """
        type A($0a) =
          member _.F() = a + 1
        """
        (Diagnostics.acceptAll)
        selectCodeFix
        """
        type A(a: int) =
          member _.F() = a + 1
        """
    testCaseAsync "doesn't trigger for ctor param when existing type" <|
      CodeFix.checkNotApplicable server
        """
        type A($0a: int) =
          member _.F() = a + 1
        """
        (Diagnostics.acceptAll)
        selectCodeFix
    testCaseAsync "can add type to correct ctor param" <|
      CodeFix.check server
        """
        type A(str, $0n, b) =
          member _.FString() = sprintf "str=%s" str
          member _.FInt() = n + 1
          member _.FBool() = sprintf "b=%b" b
        """
        (Diagnostics.acceptAll)
        selectCodeFix
        """
        type A(str, n: int, b) =
          member _.FString() = sprintf "str=%s" str
          member _.FInt() = n + 1
          member _.FBool() = sprintf "b=%b" b
        """
    testCaseAsync "doesn't trigger for ctor param when existing type and multiple params" <|
      CodeFix.checkNotApplicable server
        """
        type A(str, $0n: int, b) =
          member _.FString() = sprintf "str=%s" str
          member _.FInt() = a + 1
          member _.FBool() = sprintf "b=%b" b
        """
        (Diagnostics.acceptAll)
        selectCodeFix
    testCaseAsync "can add type to secondary ctor param" <|
      CodeFix.check server
        """
        type A(a) =
          new($0a, b) = A(a+b)
          member _.F() = a + 1
        """
        (Diagnostics.acceptAll)
        selectCodeFix
        """
        type A(a) =
          new(a: int, b) = A(a+b)
          member _.F() = a + 1
        """
    testCaseAsync "can add type to function parameter" <|
      CodeFix.check server
        """
        let map $0f v = f (v+1) + 1
        """
        (Diagnostics.acceptAll)
        selectCodeFix
        """
        let map (f: int -> int) v = f (v+1) + 1
        """
    testCaseAsync "can add type to function member parameter" <|
      CodeFix.check server
        """
        type A(a) =
          member _.F(b, $0f) = f (a+b) + 1
        """
        (Diagnostics.acceptAll)
        selectCodeFix
        """
        type A(a) =
          member _.F(b, f: int -> int) = f (a+b) + 1
        """
    testCaseAsync "can add type to function value" <|
      CodeFix.check server
        """
        let $0f = fun a b -> a + b
        """
        (Diagnostics.acceptAll)
        selectCodeFix
        """
        let f: int -> int -> int = fun a b -> a + b
        """
    testCaseAsync "doesn't trigger for function value with existing type annotation" <|
      CodeFix.checkNotApplicable server
        """
        let $0f: int -> int -> int = fun a b -> a + b
        """
        (Diagnostics.acceptAll)
        selectCodeFix
    testCaseAsync "doesn't trigger for function parameter with existing type annotation" <|
      CodeFix.checkNotApplicable server
        """
        let map ($0f: int -> int) v = f (v+1) + 1
        """
        (Diagnostics.acceptAll)
        selectCodeFix

    testList "parens" [
      testCaseAsync "single param without parens -> add parens" <|
        CodeFix.check server
          """
          let f $0x = x + 1
          """
          (Diagnostics.acceptAll)
          selectCodeFix
          """
          let f (x: int) = x + 1
          """
      testCaseAsync "single param with parens -> keep parens" <|
        CodeFix.check server
          """
          let f ($0x) = x + 1
          """
          (Diagnostics.acceptAll)
          selectCodeFix
          """
          let f (x: int) = x + 1
          """
      testCaseAsync "multi params without parens -> add parens" <|
        CodeFix.check server
          """
          let f a $0x y = x + 1
          """
          (Diagnostics.acceptAll)
          selectCodeFix
          """
          let f a (x: int) y = x + 1
          """
      testCaseAsync "multi params with parens -> keep parens" <|
        CodeFix.check server
          """
          let f a ($0x) y = x + 1
          """
          (Diagnostics.acceptAll)
          selectCodeFix
          """
          let f a (x: int) y = x + 1
          """
      testList "tuple params without parens -> no parens" [
        testCaseAsync "start" <|
          CodeFix.check server
            """
            let f ($0x, y, z) = x + y + z + 1
            """
            (Diagnostics.acceptAll)
            selectCodeFix
            """
            let f (x: int, y, z) = x + y + z + 1
            """
        testCaseAsync "center" <|
          CodeFix.check server
            """
            let f (x, $0y, z) = x + y + z + 1
            """
            (Diagnostics.acceptAll)
            selectCodeFix
            """
            let f (x, y: int, z) = x + y + z + 1
            """
        testCaseAsync "end" <|
          CodeFix.check server
            """
            let f (x, y, $0z) = x + y + z + 1
            """
            (Diagnostics.acceptAll)
            selectCodeFix
            """
            let f (x, y, z: int) = x + y + z + 1
            """
      ]
      testList "tuple params with parens -> keep parens" [
        testCaseAsync "start" <|
          CodeFix.check server
            """
            let f (($0x), y, z) = x + y + z + 1
            """
            (Diagnostics.acceptAll)
            selectCodeFix
            """
            let f ((x: int), y, z) = x + y + z + 1
            """
        testCaseAsync "center" <|
          CodeFix.check server
            """
            let f (x, ($0y), z) = x + y + z + 1
            """
            (Diagnostics.acceptAll)
            selectCodeFix
            """
            let f (x, (y: int), z) = x + y + z + 1
            """
        testCaseAsync "end" <|
          CodeFix.check server
            """
            let f (x, y, ($0z)) = x + y + z + 1
            """
            (Diagnostics.acceptAll)
            selectCodeFix
            """
            let f (x, y, (z: int)) = x + y + z + 1
            """
      ]
      testList "tuple params without parens but spaces -> no parens" [
        testCaseAsync "start" <|
          CodeFix.check server
            """
            let f (  $0x   ,   y   ,   z   ) = x + y + z + 1
            """
            (Diagnostics.acceptAll)
            selectCodeFix
            """
            let f (  x: int   ,   y   ,   z   ) = x + y + z + 1
            """
        testCaseAsync "center" <|
          CodeFix.check server
            """
            let f (  x   ,   $0y   ,   z   ) = x + y + z + 1
            """
            (Diagnostics.acceptAll)
            selectCodeFix
            """
            let f (  x   ,   y: int   ,   z   ) = x + y + z + 1
            """
        testCaseAsync "end" <|
          CodeFix.check server
            """
            let f (  x   ,   y   ,   $0z   ) = x + y + z + 1
            """
            (Diagnostics.acceptAll)
            selectCodeFix
            """
            let f (  x   ,   y   ,   z: int   ) = x + y + z + 1
            """
      ]
      testList "long tuple params without parens but spaces -> no parens" [
        testCaseAsync "start" <|
          CodeFix.check server
            """
            let f (  xV$0alue   ,   yAnotherValue   ,   zFinalValue   ) = xValue + yAnotherValue + zFinalValue + 1
            """
            (Diagnostics.acceptAll)
            selectCodeFix
            """
            let f (  xValue: int   ,   yAnotherValue   ,   zFinalValue   ) = xValue + yAnotherValue + zFinalValue + 1
            """
        testCaseAsync "center" <|
          CodeFix.check server
            """
            let f (  xValue   ,   yAn$0otherValue   ,   zFinalValue   ) = xValue + yAnotherValue + zFinalValue + 1
            """
            (Diagnostics.acceptAll)
            selectCodeFix
            """
            let f (  xValue   ,   yAnotherValue: int   ,   zFinalValue   ) = xValue + yAnotherValue + zFinalValue + 1
            """
        testCaseAsync "end" <|
          CodeFix.check server
            """
            let f (  xValue   ,   yAnotherValue   ,   zFina$0lValue   ) = xValue + yAnotherValue + zFinalValue + 1
            """
            (Diagnostics.acceptAll)
            selectCodeFix
            """
            let f (  xValue   ,   yAnotherValue   ,   zFinalValue: int   ) = xValue + yAnotherValue + zFinalValue + 1
            """
      ]
      testCaseAsync "never add parens to primary ctor param" <|
        CodeFix.check server
          """
          type A (
            $0a
            ) =
            member _.F(b) = a + b
          """
          (Diagnostics.acceptAll)
          selectCodeFix
          """
          type A (
            a: int
            ) =
            member _.F(b) = a + b
          """

      testCaseAsync "emit type for optional parameter without option" <|
        CodeFix.check server
          """
          type A =
            static member F(?$0a) = a |> Option.map ((+) 1)
          """
          (Diagnostics.acceptAll)
          selectCodeFix
          """
          type A =
            static member F(?a: int) = a |> Option.map ((+) 1)
          """
      testCaseAsync "adds parens to optional parameter" <|
        CodeFix.check server
          """
          type A =
            static member F?$0a = a |> Option.map ((+) 1)
          """
          (Diagnostics.acceptAll)
          selectCodeFix
          """
          type A =
            static member F(?a: int) = a |> Option.map ((+) 1)
          """
      testCaseAsync "adds parens to ident in match case" <|
        CodeFix.check server
          """
          match 4 with
          | $0value -> ()
          """
          (Diagnostics.acceptAll)
          selectCodeFix
          """
          match 4 with
          | (value: int) -> ()
          """

      testCaseAsync "doesn't add parens to let" <|
        CodeFix.check server
          """
          let $0value = 42
          """
          (Diagnostics.acceptAll)
          selectCodeFix
          """
          let value: int = 42
          """
      testCaseAsync "adds parens to let!" <|
        CodeFix.check server
          """
          async {
            let! $0value = async { return 4 }
            ()
          } |> ignore
          """
          (Diagnostics.acceptAll)
          selectCodeFix
          """
          async {
            let! (value: int) = async { return 4 }
            ()
          } |> ignore
          """
      testCaseAsync "doesn't add parens to use" <|
        CodeFix.check server
          """
          open System
          let d = { new IDisposable with
              member _.Dispose () = ()
          }
          let _ =
              use $0value = d
              ()
          """
          (Diagnostics.acceptAll)
          selectCodeFix
          """
          open System
          let d = { new IDisposable with
              member _.Dispose () = ()
          }
          let _ =
              use value: IDisposable = d
              ()
          """
      testCaseAsync "doesn't trigger for use!" <|
        CodeFix.checkNotApplicable server
          """
          open System
          let d = { new IDisposable with
              member _.Dispose () = ()
          }
          async {
              use! $0value = async { return d }
              ()
          } |> ignore
          """
          (Diagnostics.acceptAll)
          selectCodeFix
    ]
  ])
