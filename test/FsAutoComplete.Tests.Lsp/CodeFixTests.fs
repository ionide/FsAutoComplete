module FsAutoComplete.Tests.CodeFixTests

open Expecto
open Helpers
open Utils.ServerTests
open Utils.CursorbasedTests
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete.CodeFix

module private Diagnostics =
  let expectCode code (diags: Diagnostic[]) =
    Expecto.Flip.Expect.exists 
      $"There should be a Diagnostic with code %s{code}"
      (fun (d: Diagnostic) -> d.Code = Some code)
      diags
  let acceptAll = ignore

  open FsAutoComplete.Logging
  let private logger = FsAutoComplete.Logging.LogProvider.getLoggerByName "CodeFixes.Diagnostics"
  /// Usage: `(Diagnostics.log >> Diagnostics.expectCode "XXX")`
  /// Logs as `info`
  let log (diags: Diagnostic[]) =
    logger.info (
      Log.setMessage "diags({count})={diags}"
      >> Log.addContext "count" diags.Length
      >> Log.addContextDestructured "diags" diags
    )
    diags

module CodeFix =
  open FsAutoComplete.Logging
  let private logger = FsAutoComplete.Logging.LogProvider.getLoggerByName "CodeFixes.CodeFix"
  /// Usage: `(CodeFix.log >> CodeFix.withTitle "XXX")`
  /// Logs as `info`
  let log (codeActions: CodeAction[]) =
    logger.info (
      Log.setMessage "codeActions({count})={codeActions}"
      >> Log.addContext "count" codeActions.Length
      >> Log.addContextDestructured "codeActions" codeActions
    )
    codeActions


let private addExplicitTypeToParameterTests state =
  serverTestList (nameof AddExplicitTypeToParameter) state defaultConfigDto None (fun server -> [
    testCaseAsync "can suggest explicit parameter for record-typed function parameters" <|
      CodeFix.check server
        """
        type Foo =
            { name: string }

        let name $0f =
            f.name
        """
        (Diagnostics.acceptAll)
        (CodeFix.withTitle AddExplicitTypeToParameter.title)
        """
        type Foo =
            { name: string }

        let name (f: Foo) =
            f.name
        """
  ])

let private addMissingFunKeywordTests state =
  serverTestList (nameof AddMissingFunKeyword) state defaultConfigDto None (fun server -> [
    testCaseAsync "can generate the fun keyword when error 10 is raised" <|
      CodeFix.check server
        """
        let doThing = x $0-> printfn "%s" x
        """
        (Diagnostics.expectCode "10")
        (CodeFix.ofKind "quickfix" >> CodeFix.withTitle AddMissingFunKeyword.title)
        """
        let doThing = fun x -> printfn "%s" x
        """
  ])

let private addMissingInstanceMemberTests state =
  serverTestList (nameof AddMissingInstanceMember) state defaultConfigDto None (fun server -> [
    testCaseAsync "can add this member prefix" <|
      CodeFix.check server
        """
        type C () =
          member $0Foo() = ()
        """
        (Diagnostics.expectCode "673")
        (CodeFix.ofKind "quickfix" >> CodeFix.withTitle AddMissingInstanceMember.title)
        """
        type C () =
          member x.Foo() = ()
        """
  ])

let private addMissingRecKeywordTests state =
  serverTestList (nameof AddMissingRecKeyword) state defaultConfigDto None (fun server -> [
    // `rec` in single function is handled in `MakeOuterBindingRecursive`
    testCaseAsync "can add rec to mutual recursive function" <|
      CodeFix.check server
        """
        $0let a x = x
        and b x = x
        """
        (Diagnostics.expectCode "576")
        (CodeFix.withTitle (AddMissingRecKeyword.title "a"))
        """
        let rec a x = x
        and b x = x
        """
  ])

let private addTypeToIndeterminateValueTests state =
  serverTestList (nameof AddTypeToIndeterminateValue) state defaultConfigDto None (fun server -> [
    let selectCodeFix = CodeFix.withTitle AddTypeToIndeterminateValue.title
    testCaseAsync "can add type annotation to error 72 ('Lookup on object of indeterminate type')" <|
      CodeFix.check server
        """
        let data = [
          {| Name = "foo"; Value = 42 |}
          {| Name = "bar"; Value = 13 |}
        ]
        let res = List.filter (fun d -> $0d.Value > 20) data
        """
        (Diagnostics.expectCode "72")
        selectCodeFix
        """
        let data = [
          {| Name = "foo"; Value = 42 |}
          {| Name = "bar"; Value = 13 |}
        ]
        let res = List.filter (fun (d: {| Name: string; Value: int |}) -> d.Value > 20) data
        """
    testCaseAsync "can add type annotation to error 3245 ('The input to a copy-and-update expression that creates an anonymous record must be either an anonymous record or a record')" <|
      CodeFix.check server
        """
        [1..5]
        |> List.fold
            (fun s i ->
              match i % 2 with
              | 0 -> {| $0s with Evens = s.Evens + 1 |}
              | _ -> s
            )
            {| Evens = 0 |}
        """
        (Diagnostics.expectCode "3245")
        selectCodeFix
        """
        [1..5]
        |> List.fold
            (fun (s: {| Evens: int |}) i ->
              match i % 2 with
              | 0 -> {| s with Evens = s.Evens + 1 |}
              | _ -> s
            )
            {| Evens = 0 |}
        """
  ])

let private changeEqualsInFieldTypeToColonTests state = 
  serverTestList (nameof ChangeEqualsInFieldTypeToColon) state defaultConfigDto None (fun server -> [
    let selectCodeFix = CodeFix.withTitle ChangeEqualsInFieldTypeToColon.title
    testCaseAsync "can change = to : in single line" <|
      CodeFix.check server
        """
        type A = { Name : string; Key $0= int }
        """
        (Diagnostics.expectCode "10") 
        selectCodeFix
        """
        type A = { Name : string; Key : int }
        """
    testCaseAsync "can change = to : in multi line" <|
      CodeFix.check server
        """
        type A = { 
          Name : string
          Key $0= int 
        }
        """
        (Diagnostics.expectCode "10") 
        selectCodeFix
        """
        type A = { 
          Name : string
          Key : int 
        }
        """
  ])

let private changeTypeOfNameToNameOfTests state =
  serverTestList (nameof ChangeTypeOfNameToNameOf) state defaultConfigDto None (fun server -> [
    testCaseAsync "can suggest fix" <|
      CodeFix.check server
        """
        let x = $0typeof<Async<string>>.Name
        """
        (Diagnostics.acceptAll)
        (CodeFix.ofKind "refactor" >> CodeFix.withTitle ChangeTypeOfNameToNameOf.title)
        """
        let x = nameof(Async<string>)
        """
  ])

let private convertCSharpLambdaToFSharpTests state =
  serverTestList (nameof ConvertCSharpLambdaToFSharpLambda) state defaultConfigDto None (fun server -> [
    let selectCodeFix = CodeFix.withTitle ConvertCSharpLambdaToFSharpLambda.title
    testCaseAsync "can convert csharp lambda in variable assignment with cursor on input" <|
      CodeFix.check server
        """
        let x = $0y => 1 + y
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        let x = fun y -> 1 + y
        """
    testCaseAsync "can convert csharp lambda in variable assignment with cursor on usage" <|
      CodeFix.check server
        """
        let x = y => 1 + $0y
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        let x = fun y -> 1 + y
        """
    //ENHANCEMENT: trigger on `=>`
    // testCaseAsync "can convert csharp lambda in variable assignment with cursor on =>" <|
    //   CodeFix.check server
    //     """
    //     let x = y $0=> 1 + y
    //     """
    //     Diagnostics.acceptAll
    //     selectReplaceCSharpLambdaWithFSharp
    //     """
    //     let x = fun y -> 1 + y
    //     """
    testCaseAsync "can convert csharp lambda in lambda with parens with cursor on input" <|
      CodeFix.check server
        """
        [1..10] |> List.map ($0x => 1 + x)
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        [1..10] |> List.map (fun x -> 1 + x)
        """
    testCaseAsync "can convert csharp lambda in lambda with parens with cursor on usage" <|
      CodeFix.check server
        """
        [1..10] |> List.map (x => 1 + $0x)
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        [1..10] |> List.map (fun x -> 1 + x)
        """
    testCaseAsync "keep multi-line lambda intact - cursor on input" <|
      CodeFix.check server
        """
        let x =
          $0y =>
            let a = 1 + y
            a
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        let x =
          fun y ->
            let a = 1 + y
            a
        """
    testCaseAsync "keep multi-line lambda intact - cursor on usage" <|
      CodeFix.check server
        """
        let x =
          y =>
            let a = 1 + $0y
            a
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        let x =
          fun y ->
            let a = 1 + y
            a
        """
  ])

let private convertPositionalDUToNamedTests state =
  serverTestList (nameof ConvertPositionalDUToNamed) state defaultConfigDto None (fun server -> [
    let selectCodeFix = CodeFix.withTitle ConvertPositionalDUToNamed.title
    testCaseAsync "in parenthesized let binding" <|
      CodeFix.check server
        """
        type A = A of a: int * b: bool

        let (A(a$0, b)) = A(1, true)
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        type A = A of a: int * b: bool

        let (A(a = a; b = b;)) = A(1, true)
        """
    testCaseAsync "in simple match" <|
      CodeFix.check server
        """
        type A = A of a: int * b: bool

        match A(1, true) with
        | A(a$0, b) -> ()
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        type A = A of a: int * b: bool

        match A(1, true) with
        | A(a = a; b = b;) -> ()
        """
    testCaseAsync "in parenthesized match" <|
      CodeFix.check server
        """
        type A = A of a: int * b: bool

        match A(1, true) with
        | (A(a$0, b)) -> ()
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        type A = A of a: int * b: bool

        match A(1, true) with
        | (A(a = a; b = b;)) -> ()
        """
    testCaseAsync "when there are new fields on the DU" <|
      //ENHANCEMENT: add space before wildcard case
      CodeFix.check server
        """
        type ThirdFieldWasJustAdded = ThirdFieldWasJustAdded of a: int * b: bool * c: char

        let (ThirdFieldWasJustAdded($0a, b)) = ThirdFieldWasJustAdded(1, true, 'c')
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        type ThirdFieldWasJustAdded = ThirdFieldWasJustAdded of a: int * b: bool * c: char

        let (ThirdFieldWasJustAdded(a = a; b = b;c = _;)) = ThirdFieldWasJustAdded(1, true, 'c')
        """
  ])

let private generateAbstractClassStubTests state = 
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
    ptestCaseAsync "can generate abstract class stub" <|
      // issue: Wants to insert text in line 13, column 12.
      //        But Line 13 (line with `"""`) is empty -> no column 12
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
        """
        (Diagnostics.expectCode "365")
        selectCodeFix
        """
        [<AbstractClass>]
        type Shape(x0: float, y0: float) =
          let mutable x, y = x0, y0

          abstract Name : string with get
          abstract Area : float with get

          member _.Move dx dy =
            x <- x + dx
            y <- y + dy
          
        type Square(x,y, sideLength) =
          inherit Shape(x,y)

          override this.Area: float = 
              failwith "Not Implemented"
          override this.Name: string = 
              failwith "Not Implemented"
        """
    ptestCaseAsync "can generate abstract class stub without trailing nl" <|
      // issue: Wants to insert text in line 13, column 12.
      //        But there's no line 13 (last line is line 12)
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
          inherit Shape(x,y)"""
        (Diagnostics.expectCode "365")
        selectCodeFix
        """
        [<AbstractClass>]
        type Shape(x0: float, y0: float) =
          let mutable x, y = x0, y0

          abstract Name : string with get
          abstract Area : float with get

          member _.Move dx dy =
            x <- x + dx
            y <- y + dy
          
        type Square(x,y, sideLength) =
          inherit Shape(x,y)

          override this.Area: float = 
              failwith "Not Implemented"
          override this.Name: string = 
              failwith "Not Implemented"
        """
    ptestCaseAsync "inserts override in correct place" <|
      // issue: inserts overrides after `let a = ...`, not before
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
        let a = 0
        """
        (Diagnostics.expectCode "365")
        selectCodeFix
        """
        [<AbstractClass>]
        type Shape(x0: float, y0: float) =
          let mutable x, y = x0, y0

          abstract Name : string with get
          abstract Area : float with get

          member _.Move dx dy =
            x <- x + dx
            y <- y + dy
          
        type Square(x,y, sideLength) =
          inherit Shape(x,y)

          override this.Area: float = 
              failwith "Not Implemented"
          override this.Name: string = 
              failwith "Not Implemented"
        let a = 0
        """
    ptestCaseAsync "can generate abstract class stub with existing override" <|
      // issue: Generates override for already existing member
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
        """
        (Diagnostics.expectCode "365")
        selectCodeFix
        """
        [<AbstractClass>]
        type Shape(x0: float, y0: float) =
          let mutable x, y = x0, y0

          abstract Name : string with get
          abstract Area : float with get

          member _.Move dx dy =
            x <- x + dx
            y <- y + dy
          
        type Square(x,y, sideLength) =
          inherit Shape(x,y)

          override this.Name = "Circle"

          override this.Area: float = 
              failwith "Not Implemented"
        """
  ])

let private generateRecordStubTests state =
  let config = 
    { defaultConfigDto with 
        RecordStubGeneration = Some true 
        RecordStubGenerationBody = Some "failwith \"---\""
    }
  serverTestList (nameof GenerateRecordStub) state config None (fun server -> [
    CodeFix.testAllPositions "can generate record stubs for every pos in the record as soon as one field is known"
      server
      """
      type R = { a: string; b: int }

      let a = $0{  $0a = $0"";$0  }$0
      """
      (Diagnostics.expectCode "764")
      (CodeFix.withTitle GenerateRecordStub.title)
      """
      type R = { a: string; b: int }

      let a = {  a = "";
                 b = failwith "---"  }
      """
  ])

let private generateUnionCasesTests state =
  let config = 
    { defaultConfigDto with 
        UnionCaseStubGeneration = Some true 
        UnionCaseStubGenerationBody = Some "failwith \"---\""
    }
  serverTestList (nameof GenerateUnionCases) state config None (fun server -> [
    let selectCodeFix = CodeFix.withTitle GenerateUnionCases.title
    testCaseAsync "can generate match cases for a simple DU" <|
      CodeFix.check server
        """
        type Letter = A | B | C

        let char = A

        match $0char with
        | A -> ()
        """
        (Diagnostics.expectCode "25")
        (CodeFix.withTitle GenerateUnionCases.title)
        """
        type Letter = A | B | C

        let char = A

        match char with
        | A -> ()
        | B -> failwith "---"
        | C -> failwith "---"
        """
  ])

let private makeOuterBindingRecursiveTests state =
  serverTestList (nameof MakeOuterBindingRecursive) state defaultConfigDto None (fun server -> [
    testCaseAsync "can make the outer binding recursive when self-referential" <|
      CodeFix.check server
        """
        let mySum xs acc =
            match xs with
            | [] -> acc
            | _ :: tail ->
                $0mySum tail (acc + 1)
        """
        (Diagnostics.expectCode "39")
        (CodeFix.ofKind "quickfix" >> CodeFix.withTitle MakeOuterBindingRecursive.title)
        """
        let rec mySum xs acc =
            match xs with
            | [] -> acc
            | _ :: tail ->
                mySum tail (acc + 1)
        """
  ])

let private negationToSubtractionTests state =
  serverTestList (nameof NegationToSubtraction) state defaultConfigDto None (fun server -> [
    testCaseAsync "converts negation to subtraction" <|
      CodeFix.check server
        """
        let getListWithoutFirstAndLastElement list =
          let l = List.length list
          list[ 1 .. $0l -1 ]
        """
        (Diagnostics.expectCode "3")
        (CodeFix.ofKind "quickfix" >> CodeFix.withTitle NegationToSubtraction.title)
        """
        let getListWithoutFirstAndLastElement list =
          let l = List.length list
          list[ 1 .. l - 1 ]
        """
  ])

let private removeUnusedBindingTests state =
  let config = { defaultConfigDto with FSIExtraParameters = Some [| "--warnon:1182" |] }
  serverTestList (nameof RemoveUnusedBinding) state config None (fun server -> [
    let selectRemoveUnusedBinding = CodeFix.withTitle RemoveUnusedBinding.titleBinding
    let selectRemoveUnusedParameter = CodeFix.withTitle RemoveUnusedBinding.titleParameter
    let validateDiags = Diagnostics.expectCode "1182"

    testCaseAsync "can remove unused single character function parameter" <|
      CodeFix.check server
        """
        let incr $0i x = 2
        """
        validateDiags
        selectRemoveUnusedParameter
        """
        let incr x = 2
        """
    testCaseAsync "can remove unused single character function parameter in parens" <|
      CodeFix.check server
        """
        let incr ($0i) x = 2
        """
        validateDiags
        selectRemoveUnusedParameter
        """
        let incr x = 2
        """
    testCaseAsync "can remove unused binding inside top level" <|
      //ENHANCEMENT: remove empty line
      CodeFix.check server
        """
        let container () =
          let $0incr x = 2
          ()
        """
        validateDiags
        selectRemoveUnusedBinding
        """
        let container () =
          
          ()
        """
  ])

let private unusedValueTests state =
  let config = { defaultConfigDto with UnusedDeclarationsAnalyzer = Some true }
  serverTestList (nameof UnusedValue) state config None (fun server -> [
    let selectReplace = CodeFix.ofKind "refactor" >> CodeFix.withTitle UnusedValue.titleReplace
    let selectPrefix = CodeFix.ofKind "refactor" >> CodeFix.withTitle UnusedValue.titlePrefix

    testCaseAsync "can replace unused self-reference" <|
      CodeFix.check server
        """
        type MyClass() =
          member $0this.DoAThing() = ()
        """
        (Diagnostics.acceptAll)
        selectReplace
        """
        type MyClass() =
          member _.DoAThing() = ()
        """
    testCaseAsync "can replace unused binding" <|
      CodeFix.check server
        """
        let $0six = 6
        """
        (Diagnostics.acceptAll)
        selectReplace
        """
        let _ = 6
        """
    testCaseAsync "can prefix unused binding" <|
      CodeFix.check server
        """
        let $0six = 6
        """
        (Diagnostics.acceptAll)
        selectPrefix
        """
        let _six = 6
        """
    testCaseAsync "can replace unused parameter" <|
      CodeFix.check server
        """
        let add one two $0three = one + two
        """
        (Diagnostics.acceptAll)
        selectReplace
        """
        let add one two _ = one + two
        """
    testCaseAsync "can prefix unused parameter" <|
      CodeFix.check server
        """
        let add one two $0three = one + two
        """
        (Diagnostics.log >> Diagnostics.acceptAll)
        (CodeFix.log >> selectPrefix)
        """
        let add one two _three = one + two
        """
  ])

let private useTripleQuotedInterpolationTests state =
  serverTestList (nameof UseTripleQuotedInterpolation) state defaultConfigDto None (fun server -> [
    testCaseAsync "converts erroring single-quoted interpolation to triple-quoted" <|
      CodeFix.check server
        """
        let a = $":^) {if true then $0"y" else "n"} d"
        """
        (Diagnostics.expectCode "3373")
        (CodeFix.ofKind "quickfix" >> CodeFix.withTitle UseTripleQuotedInterpolation.title)
        // cannot use triple quotes string here: ends with `"""` -> cannot use in string
        @"
        let a = $"""""":^) {if true then ""y"" else ""n""} d""""""
        "
  ])

let private useMutationWhenValueIsMutableTests state =
  serverTestList (nameof UseMutationWhenValueIsMutable) state defaultConfigDto None (fun server -> [
    let selectCodeFix = CodeFix.withTitle UseMutationWhenValueIsMutable.title
    testCaseAsync "can replace = with <- when cursor on =" <|
      CodeFix.check server
        """
        let _ =
          let mutable v = 42
          v $0= 5
          v
        """
        (Diagnostics.expectCode "20")
        selectCodeFix
        """
        let _ =
          let mutable v = 42
          v <- 5
          v
        """
    testCaseAsync "can replace = with <- when cursor on variable" <|
      CodeFix.check server
        """
        let _ =
          let mutable v = 42
          $0v = 5
          v
        """
        (Diagnostics.expectCode "20")
        selectCodeFix
        """
        let _ =
          let mutable v = 42
          v <- 5
          v
        """
    testCaseAsync "doesn't suggest fix when = is comparison" <|
      CodeFix.checkNotApplicable server
        """
        let _ =
          let mutable v = 42
          v $0= 5
        """
        Diagnostics.acceptAll
        selectCodeFix
    testCaseAsync "doesn't suggest fix when variable is not mutable" <|
      CodeFix.checkNotApplicable server
        """
        let _ =
          let v = 42
          v $0= 5
          v
        """
        Diagnostics.acceptAll
        selectCodeFix
  ])


let tests state = testList "CodeFix tests" [
  addExplicitTypeToParameterTests state
  addMissingFunKeywordTests state
  addMissingInstanceMemberTests state
  addMissingRecKeywordTests state
  addTypeToIndeterminateValueTests state
  changeEqualsInFieldTypeToColonTests state
  changeTypeOfNameToNameOfTests state
  convertCSharpLambdaToFSharpTests state
  convertPositionalDUToNamedTests state
  generateAbstractClassStubTests state
  generateRecordStubTests state
  generateUnionCasesTests state
  makeOuterBindingRecursiveTests state
  negationToSubtractionTests state
  removeUnusedBindingTests state
  unusedValueTests state
  useTripleQuotedInterpolationTests state
  useMutationWhenValueIsMutableTests state
]
